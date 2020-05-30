package antiphony

import quarantine._

import java.net.InetSocketAddress
import Method._
 
import com.sun.net.httpserver.{HttpServer => JavaHttpServer, _}
import java.io.OutputStream
import java.net.URI
import java.util.concurrent._
import scala.collection.JavaConverters._
import java.nio.charset.StandardCharsets
import java.net._

object SimpleRequestHandler {
  lazy val executor = Executors.newFixedThreadPool(8)
}

object bindDomain extends Domain[BindError]

sealed trait BindError extends Exception
case class PortInUse(port: Int) extends BindError
case class PortForbidden(port: Int) extends BindError

class LiveHttpServer(stop: () => Unit) { def shutdown(): Unit = stop() }

case class HttpServer(handler: Request => Response[_]) extends RequestHandler {
  import bindDomain._

  def handle(request: Request): Response[_] = handler(request)

  def bind(port: Int): Result[LiveHttpServer] = try {
    val server = JavaHttpServer.create(new InetSocketAddress(port), 8)
    server.createContext("/", httpHandler)
    server.setExecutor(SimpleRequestHandler.executor)
    server.start()
    Answer(new LiveHttpServer(() => server.stop(1)))
  } catch {
    case e: BindException if e.getMessage == "Address already in use" => Error(PortInUse(port))
    case e: SocketException if e.getMessage == "Permission denied"    => Error(PortForbidden(port))
  }
  
  private def httpHandler = new HttpHandler() {
    def handle(exchange: HttpExchange): Unit = {
      
      val writer = new ResponseWriter {
        private var status: Int = 200
        def addHeader(key: String, value: String): Unit = exchange.getResponseHeaders.add(key, value)
        def setContentType(contentType: String): Unit = exchange.getResponseHeaders.add("Content-Type", contentType)
        
        def sendBody(body: String): Unit = {
          val bytes = body.getBytes(StandardCharsets.UTF_8)
          exchange.sendResponseHeaders(status, bytes.length)
          exchange.getResponseBody.write(bytes)
          exchange.getResponseBody.close()
        }
        
        def sendRedirect(url: String): Unit = {
          exchange.getResponseHeaders.add("Location", url)
          exchange.sendResponseHeaders(302, 0)
          exchange.getResponseBody.close()
        }

        def setStatus(code: Int) = status = code
      }

      try {
        val headers = exchange.getRequestHeaders().asScala.toMap.mapValues(_.asScala.to[List].head)
        val stream = Request.stream(exchange.getRequestBody)

        val method = Method.unapply(exchange.getRequestMethod).get
        val contentType = headers.getOrElse("Content-Type", "text/plain")
        val query = Option(exchange.getRequestURI.getRawQuery).getOrElse("")
        val ssl = exchange.getRequestURI.getScheme == "https"
        val hostname = exchange.getRemoteAddress.getHostName
        val port = exchange.getRequestURI.getPort
        val path = exchange.getRequestURI.getPath
        val size = headers.get("Length").map(_.toInt)

        val (body, paramString) = (method, contentType) match {
          case (Put | Post, "application/x-www-form-urlencoded") =>
            val bytes = stream.reduce(_ ++ _)
            val string = new String(bytes.to[Array], StandardCharsets.UTF_8)
            
            (Stream(bytes), string)

          case (Get | Head, _) =>
            (stream, query)
            
          case _ =>
            (stream, "&")
        }

        def decode(str: String): String = URLDecoder.decode(str, StandardCharsets.UTF_8.name)

        val params: Map[String, String] = paramString.split("&").to[List].flatMap(_.split("=", 2) match {
          case Array(key, value) => List((decode(key), decode(value)))
          case Array(key)        => List((decode(key) -> ""))
          case _                 => Nil
        }).toMap

        val request = Request(method, contentType, size, body, query, ssl, hostname, port, path, headers, params)
        
        handler(request).respond(writer)
      } catch {
        case e: Exception =>
          writer.setStatus(500)
          writer.setContentType("text/plain")
          writer.sendBody("Internal server error: "+e+" at "+e.getStackTrace.mkString("\n"))
      }
    }
  }
}