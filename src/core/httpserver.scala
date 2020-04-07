/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package antiphony

import quarantine._
import euphemism._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec
import HttpServerHelpers._

object HttpServerHelpers {
    def mapKeysToLowerCase[T](map: Map[String, T]): Map[String, T] = map.map {case (k,v) => (k.toLowerCase -> v)}
}

trait RequestHandler {
  def handle(implicit request: Request): Response[_]
}

case class Request(
  method: Method,
  contentType: String,
  length: Int,
  content: Array[Byte],
  query: String,
  ssl: Boolean,
  hostname: String,
  port: Int,
  path: String,
  httpHeaders: Map[String, String],
  parameters: Map[String, List[String]]) {

  def apply[T: ParamParser](param: Param[T]): Option[T] =
    implicitly[ParamParser[T]].parse(parameters.get(param.name).flatMap(_.headOption))

  override def toString: String = {
    s"""{
      | method: ${method},
      | contentType: ${contentType},
      | length: ${length},
      | content(max 10 bytes): ${content.take(10).toList},
      | query: ${query},
      | ssl: ${ssl},
      | hostname: ${hostname},
      | port: ${port}.
      | path: ${path},
      | httpHeaders: ${httpHeaders},
      | parameters: ${parameters}
      |}""".stripMargin
  }

  def splitPath = {
    val splitPath = path.split("/").to[List]
    if(splitPath.isEmpty) splitPath else splitPath.tail
  }
}

case class Param[T](name: String) {
  def apply()(implicit r: Request, paramParser: ParamParser[T]): Option[T] = r(this)
}

object ParamParser {
  implicit val int: ParamParser[Int] = s => util.Try(s.map(_.toInt)).toOption.flatten
  implicit val string: ParamParser[String] = identity
  implicit val boolean: ParamParser[Boolean] = v => Some(v.isDefined)
}

trait ParamParser[T] { def parse(value: Option[String]): Option[T] }

sealed abstract class Method(name: String)

case class Cookie(domain: String, name: String, value: String, path: String, expiry: Option[Long], ssl: Boolean)

case class Redirect(url: String)

trait ResponseWriter {
  def setStatus(status: Int)
  def appendBody(body: String)
  def setContentType(contentType: String)
  def addHeader(key: String, value: String)
  def sendRedirect(url: String)
  def close()
}


class Response[Type: Responder.ResponseResponder](
  val value: Type,
  val cookies: List[Cookie],
  val headers: Map[String, String],
  val status: Int
) {
  val responder = implicitly[Responder.ResponseResponder[Type]]

  def setValue[T: Responder](value: T): Response[T] = new Response(value, this.cookies, this.headers, this.status)
  def setCookies(cookies: List[Cookie]): Response[Type] = new Response(this.value, cookies, this.headers, this.status)
  def setHeaders(headers: List[(String, String)]): Response[Type] = new Response(this.value, this.cookies, mapKeysToLowerCase(headers.toMap), this.status)
  def mapValue[T: Responder.ResponseResponder](fn: Type => T): Response[T] = {
    new Response(fn(this.value), this.cookies, this.headers, this.status)
  }
  def setStatus(status: Int) = new Response(this.value, this.cookies, this.headers, status)
  def respond(writer: ResponseWriter): Unit = responder(writer, this)

  def addCookies(cookies: Cookie*): Response[Type] = {
    new Response(this.value, cookies.toList ++ this.cookies, this.headers, this.status)
  }
  
  def addHeaders(headers: (String, String)*): Response[Type] = {
    new Response(this.value, this.cookies, this.headers ++ mapKeysToLowerCase(headers.toMap), this.status)
  }
}


object Response {
  def apply[T: Responder.ResponseResponder](
    v: T
  ): Response[T] = new Response(v, Nil, Map(), 200)
}

object Responder {
  import ServerDomain._

  implicit def futureResponder[T: Responder](implicit responder: Responder[T], ec: ExecutionContext): Responder[Future[T]] = { 
    (writer, future) => for (value <- future) responder(writer, value)
  }

  type ResponseResponder[T] = Responder[Response[T]]
  type ServerResultResponder[T] = ResponseResponder[Result[T]]

  def prepareResponse(writer: ResponseWriter, response: Response[_]): Unit = {
    writer.setStatus(response.status)
    for(header <- response.headers) {
      writer.addHeader(header._1, header._2)
    }
  }

  case class ErrorObj(msg: String)
  implicit def resultResponder[T: Responder]: ServerResultResponder[T] = { (writer, response) =>
    prepareResponse(writer, response)
    response.value match {
      case Answer(v) => 
        val responder = implicitly[Responder[T]]
        responder(writer, v)
      case Error(error) => 
        val responder = implicitly[Responder[Json]]
        writer.setStatus(error.status)
        responder(writer, Json(ErrorObj(error.responseContent)))
      case Surprise(error) => 
        throw error
    }
  }

  implicit def simpleResponder[T <: Any : Responder]: ResponseResponder[T] = { (writer, response) =>
    prepareResponse(writer, response)
    val responder = implicitly[Responder[T]]
    responder(writer, response.value)
  }

  implicit val stringResponder: Responder[String] = { (writer, value) =>
    writer.setContentType("text/plain")
    writer.appendBody(value)
  }

  implicit val redirectResponder: Responder[Redirect] = { (writer, value) => 
    writer.sendRedirect(value.url)
  }

  implicit val jsonResponder: Responder[Json] = { (writer, value) =>
    writer.setContentType("application/json")
    writer.appendBody(value.toString)
  }

  class CustomDomain[D <: Domain[_], Type: Responder](val domain: D) {
    class ResultSupport(mitigator: domain.Mitigator[ServerDomain.type]) extends ResponseResponder[domain.Result[Type]] {
      override def apply(w: ResponseWriter, r: Response[domain.Result[Type]]): Unit = {
        val serverResponder = implicitly[ResponseResponder[ServerDomain.Result[Type]]]
        val mappedResponse = r.mapValue(
          _.adapt(ServerDomain, mitigator)
          .extenuate {
            case err: Throwable =>
              err.printStackTrace()
              InternalServerError("Unexpected internal error")
        })
        serverResponder(w, mappedResponse)
      }
    }
  }

  def domainResultResponders[T: Responder](domain: Domain[_])(mitigatorFunc: domain.ExceptionType => ServerDomain.ExceptionType) = {
    val customDomain = new CustomDomain[domain.type, T](domain)
    new customDomain.ResultSupport(
      domain.mitigate(ServerDomain)(mitigatorFunc)
    )
  }
}

trait Responder[T] { def apply(writer: ResponseWriter, response: T): Unit }

sealed abstract class ServerException(val status: Int, val msg: String, val responseContent: String) extends Exception(msg) with Product with Serializable
case class NotFoundError(content: String = "Page not found") extends ServerException(404, "Page not found", content)
case class InternalServerError(content: String = "Internal server error") extends ServerException(500, "Internal server error", content)
case class UnauthorizedError(content: String = "Unauthorized") extends ServerException(401, "Unauthorized", content)
case class ForbiddenError(content: String = "Unauthorized") extends ServerException(403, "Forbidden", content)
case class BadRequest(content: String = "Bad request") extends ServerException(400, "Bad request", content)
//todo allow other response types for errors

object ServerDomain extends Domain[ServerException]


object Method {
  
  def from(str: String): Method = str match {
    case "GET" => Get
    case "HEAD" => Head
    case "POST" => Post
    case "PUT" => Put
    case "DELETE" => Delete
    case "CONNECT" => Connect
    case "OPTIONS" => Options
    case "TRACE" => Trace
    case "PATCH" => Patch
  }
  
  final case object Get extends Method("GET")
  final case object Head extends Method("HEAD")
  final case object Post extends Method("POST")
  final case object Put extends Method("PUT")
  final case object Delete extends Method("DELETE")
  final case object Connect extends Method("CONNECT")
  final case object Options extends Method("OPTIONS")
  final case object Trace extends Method("TRACE")
  final case object Patch extends Method("PATCH")
}
