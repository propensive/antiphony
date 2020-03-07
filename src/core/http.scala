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
  def apply[T: Responder](v: T, cookies: List[Cookie] = Nil): Response = new Response() {
    type Type = T
    val value: Type = v
    val responder: Responder[Type] = implicitly[Responder[T]]
  }
}

abstract class Response() {
  type Type
  val value: Type
  val responder: Responder[Type]
  def apply(response: HttpServletResponse): Unit = responder.process(response, value)
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

object Postable {
  implicit object string extends Postable[String]("text/plain") {
    def content(value: String): Array[Byte] = value.getBytes("UTF-8")
  }

  implicit object map extends Postable[Map[String, String]]("multipart/form-data") {
    def content(value: Map[String, String]): Array[Byte] =
      value.map { case (key, value) =>
        s"${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"
      }.mkString("&").getBytes("UTF-8")
  }

  implicit object json extends Postable[Json]("application/json") {
    def content(value: Json): Array[Byte] = value.toString.getBytes("UTF-8")
  }
}

abstract class Postable[T](val contentType: String) { def content(value: T): Array[Byte] }

case class HttpHeader(key: String, value: String)

object Http extends Domain[HttpException] {

  def post[T: Postable](url: String, content: T, headers: Set[HttpHeader]): Result[Array[Byte]] =
    request[T](url, content, "POST", headers)

  def get(url: String, headers: Set[HttpHeader]): Result[Array[Byte]] =
    request(url, Map[String, String](), "GET", headers)

  def request[T: Postable](url: String, content: T, method: String, headers: Set[HttpHeader]): Result[Array[Byte]] = {
    new URL(url).openConnection match {
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method)
        conn.setRequestProperty("Content-Type", implicitly[Postable[T]].contentType)
        conn.setRequestProperty("User-Agent", "Furore 1.0.0")
        headers.foreach { case HttpHeader(key, value) => conn.setRequestProperty(key, value) }
        
        if(method == "POST") {
          conn.setDoOutput(true)
          val out = conn.getOutputStream()
          out.write(implicitly[Postable[T]].content(content))
          out.close()
        }

        conn.getResponseCode match {
          case 200 =>
            val in = conn.getInputStream()
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
            Result(read())
          case 404 => Error(NotFound(url))
          case 401 => Error(NotAuthorized(url))
          case code => Error(OtherException(url, code))
        }
    }
  }
}
