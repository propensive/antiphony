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

import javax.servlet._, http._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec

import java.net._
import java.io._

sealed abstract class HttpException(url: String, code: Int) extends Exception

case class NotFound(url: String) extends HttpException(url, 404)
case class NotAuthorized(url: String) extends HttpException(url, 401)
case class OtherException(url: String, code: Int) extends HttpException(url, code)

abstract class ServletWrapper() extends HttpServlet {
  
  def handle(implicit request: Request): Response
  
  override def doGet(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse): Unit =
      handle(Request(servletRequest))(servletResponse)
  
  override def doPost(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse): Unit =
      handle(Request(servletRequest))(servletResponse)
}

object Request {
  def apply(r: HttpServletRequest): Request = {
    val in = r.getInputStream
    val data = new ByteArrayOutputStream()
    val buf = new Array[Byte](65536)

    @tailrec
    def read(): Array[Byte] = {
      val bytes = in.read(buf, 0, buf.length)
      if(bytes < 0) data.toByteArray else {
        data.write(buf, 0, bytes)
        read()
      }
    }

    val content = read()

    Request(Method.from(r.getMethod),
      r.getContentType,
      r.getContentLength,
      content,
      r.getQueryString,
      r.isSecure,
      r.getServerName,
      r.getServerPort,
      r.getRequestURI,
      r.getHeaderNames.asScala.to[List].map { k => k -> r.getHeader(k) }.toMap,
      r.getParameterNames.asScala.to[List].map { k => k -> r.getParameterValues(k).to[List] }.toMap)
  }
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

object Response {
  def apply[T: Responder](v: T, headers: Map[String, String] = Map(), cookies: List[Cookie] = Nil): Response = new Response(headers) {
    type Type = T
    val value: Type = v
    val responder: Responder[Type] = implicitly[Responder[T]]
  }
}
abstract class Response(val headers: Map[String, String]) {
  type Type
  val value: Type
  val responder: Responder[Type]
  def apply(response: HttpServletResponse, headers: Map[String, String] = Map()): Unit = {
    appendHeaders(response, headers)
    responder.process(response, value)
  }
  def appendHeaders(response: HttpServletResponse, headers: Map[String, String]): Unit = {
    for(header <- headers) {
      response.addHeader(header._1, header._2)
    }
  }
}

object Responder {
  implicit val stringResponder: Responder[String] = { (r, v) =>
    r.setContentType("text/plain")
    r.getWriter().println(v)
  }
  
  implicit val redirectResponder: Responder[Redirect] = (r, v) => r.sendRedirect(v.url)

  implicit val jsonResponder: Responder[Json] = { (r, v) =>
    r.setContentType("application/json")
    r.getWriter().println(v.toString)
  }
}

trait Responder[T] { def process(response: HttpServletResponse, value: T): Unit }

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