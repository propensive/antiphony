package antiphony

import java.net.InetSocketAddress
 
import com.sun.net.httpserver._
import java.io.OutputStream
import java.net.URI
import java.util.concurrent._
import scala.collection.JavaConverters._

object SimpleRequestHandler {
  lazy val executor = Executors.newFixedThreadPool(8)
}

abstract class SimpleRequestHandler(port: Int) extends RequestHandler { handler =>
  
  def handle(request: Request): Response[_]
  
  val httpServer = HttpServer.create(new InetSocketAddress(port), 8)
  
  val httpHandler = new HttpHandler() {
    def handle(exchange: HttpExchange): Unit = {
      val headers = exchange.getRequestHeaders().asScala.toMap.mapValues(_.asScala.to[List].head)
      val body = Request.slurp(exchange.getRequestBody)

      val writer = new ResponseWriter {
        private var status: Int = 200
        def addHeader(key: String, value: String): Unit = exchange.getResponseHeaders.add(key, value)
        def setContentType(contentType: String): Unit = exchange.getResponseHeaders.add("Content-Type", contentType)
        
        def sendBody(body: String): Unit = {
          val bytes = body.getBytes("UTF-8")
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
      
      handler.handle(Request(
        Method.unapply(exchange.getRequestMethod).get,
        headers.getOrElse("Content-Type", "text/plain"),
        body.length,
        body,
        exchange.getRequestURI.getRawQuery,
        exchange.getRequestURI.getScheme == "https",
        exchange.getRemoteAddress.getHostName,
        exchange.getRequestURI.getPort,
        exchange.getRequestURI.getPath,
        headers,
        Map() //exchange.getRequestURI().getQuery.split("=")
      )).respond(writer)
    }
  }

  httpServer.createContext("/", httpHandler)
  httpServer.setExecutor(SimpleRequestHandler.executor)
  httpServer.start()
}