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
      method: ${method},
      contentType: ${contentType},
      """
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
  def appendBody(body: String)
  def setContentType(contentType: String)
  def addHeader(key: String, value: String)
  def sendRedirect(url: String)
  def close()
}

class Response[Type: Responder](
  val value: Type,
  val cookies: List[Cookie],
  val headers: Map[String, String],
) {
  val responder = implicitly[Responder[Type]]

  def setValue[T: Responder](value: T): Response[T] = new Response(value, this.cookies, this.headers)
  def setCookies(cookies: List[Cookie]): Response[Type] = new Response(this.value, cookies, this.headers)
  def setHeaders(headers: (String, String)*): Response[Type] = new Response(this.value, this.cookies, headers.toMap)
  def withoutBody: Response[Unit] = setValue(())

  def respond(writer: ResponseWriter): Unit = responder(writer, this)
}

object Response {
  def apply[T: Responder](
    v: T
  ): Response[T] = new Response(v, Nil, Map())
}

object Responder {

  implicit val baseResponder: Responder[Unit] = { (writer, response) =>
    for(header <- response.headers) {
      writer.addHeader(header._1, header._2)
    }
  }

  def withBaseResponder[T <: Any](responder: ValueResponder[T]): Responder[T] = { (writer, response) =>
    baseResponder(writer, response.withoutBody)
    responder(writer, response.value)
  }

  implicit val stringResponder: Responder[String] = withBaseResponder[String] { (writer, value) =>
    writer.setContentType("text/plain")
    writer.appendBody(value)
  }

  implicit val redirectResponder: Responder[Redirect] = withBaseResponder[Redirect] { (writer, value) => 
    writer.sendRedirect(value.url)
  }

  implicit val jsonResponder: Responder[Json] = withBaseResponder[Json] { (writer, value) =>
    writer.setContentType("application/json")
    writer.appendBody(value.toString)
  }
}

trait Responder[T] { def apply(writer: ResponseWriter, response: Response[T]): Unit }

trait ValueResponder[T] { def apply(writer: ResponseWriter, value: T): Unit }


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
