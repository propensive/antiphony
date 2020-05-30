package antiphony

import quarantine._
import euphemism._
import gastronomy._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec
import java.io.ByteArrayOutputStream
import java.io.InputStream

trait RequestHandler { def handle(request: Request): Response[_] }

object Request {
  def stream(in: InputStream): Stream[Bytes] = {
    val buf: Array[Byte] = new Array(65536)

    def read(): Stream[Bytes] = {
      val bytes = in.read(buf, 0, buf.length)
      if(bytes < 0) Stream.empty[Bytes] else Bytes(buf.take(bytes)) #:: read()
    }

    read()
  }
}

case class Request(method: Method,
                   contentType: String,
                   length: Option[Int],
                   body: Stream[Bytes],
                   query: String,
                   ssl: Boolean,
                   hostname: String,
                   port: Int,
                   path: String,
                   httpHeaders: Map[String, String],
                   params: Map[String, String]) {

  def apply[T: ParamParser](param: Param[T]): Option[T] =
    implicitly[ParamParser[T]].parse(params.get(param.name))
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
  def sendBody(body: String): Unit
  def setContentType(contentType: String): Unit
  def addHeader(key: String, value: String): Unit
  def sendRedirect(url: String): Unit
  def setStatus(code: Int): Unit
}

case class HttpHeader(key: String, value: String)

case class Response[Type: Responder](value: Type, cookies: Vector[Cookie], headers: Vector[HttpHeader]) {
  private[this] def responder = implicitly[Responder[Type]]

  def +(cookie: Cookie): Response[Type] = copy(cookies = cookies :+ cookie)
  def +(header: HttpHeader): Response[Type] = copy(headers = headers :+ header)

  def respond(writer: ResponseWriter): Unit = responder(writer, this)
}

object Response { def apply[T: Responder](value: T): Response[T] = Response(value, Vector(), Vector()) }

object Responder {

  def writeHeaders(response: Response[_], writer: ResponseWriter): Unit =
    response.headers.foreach { header => writer.addHeader(header.key, header.value) }

  implicit val unit: Responder[Unit] = { (writer, response) =>
    writeHeaders(response, writer)
    writer.setContentType("text/plain")
  }

  implicit val string: Responder[String] = { (writer, response) =>
    writeHeaders(response, writer)
    writer.setContentType("text/plain")
    writer.sendBody(response.value)
  }

  implicit val redirect: Responder[Redirect] = { (writer, response) =>
    writeHeaders(response, writer)
    writer.sendRedirect(response.value.url)
  }

  implicit val json: Responder[Json] = { (writer, response) =>
    writeHeaders(response, writer)
    writer.setContentType("application/json")
    writer.sendBody(response.value.toString)
  }
}

trait Responder[T] { def apply(writer: ResponseWriter, response: Response[T]): Unit }

object Method {
  
  def unapply(str: String): Option[Method] = str match {
    case "GET"     => Some(Get)
    case "HEAD"    => Some(Head)
    case "POST"    => Some(Post)
    case "PUT"     => Some(Put)
    case "DELETE"  => Some(Delete)
    case "CONNECT" => Some(Connect)
    case "OPTIONS" => Some(Options)
    case "TRACE"   => Some(Trace)
    case "PATCH"   => Some(Patch)
    case _         => None
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
