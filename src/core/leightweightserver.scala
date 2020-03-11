package antiphony

import java.net.InetSocketAddress;
 
import com.sun.net.httpserver._
import java.io.OutputStream
import java.net.URI
import java.io.ByteArrayOutputStream
import scala.collection.JavaConverters._
import LeightweightServer._

object LeightweightServer {

  case class RequestQuery(query: String, parameters: Map[String, List[String]]) 

  def readBody(exchange: HttpExchange): Array[Byte] = {
    val is = exchange.getRequestBody()
    val output = new ByteArrayOutputStream();
    val buffer = Array.ofDim[Byte](2048)
    var len = is.read(buffer)
    while(len != 0) {
      output.write(buffer, 0, len)
      is.read(buffer)
    }
    output.toByteArray()
  }

  def readQuery(exchange: HttpExchange): RequestQuery = {
    val uri = exchange.getRequestURI()
    val query = uri.getQuery()
    val paramStrings = query.split("&")
    val params = paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
      val split = elem.split("=")
      val key = split(0)
      val value = split(1)
      val values = map.getOrElse(key, List.empty[String])
      map + (key -> (value::values))
    }
    RequestQuery(query, params)
  }

  def readHeaders(exchange: HttpExchange): Map[String, String] = {
    exchange.getRequestHeaders().asScala.mapValues(_.get(0)).toMap
  }

  def mapToRequest(exchange: HttpExchange): Request = {
    val body = readBody(exchange)
    val query = readQuery(exchange)
    val headers = readHeaders(exchange)
    Request(
      Method.from(exchange.getRequestMethod()),
      exchange.getRequestHeaders().getFirst("ContentType"),
      body.length,
      body,
      query.query,
      false,
      exchange.getLocalAddress().getHostName(),
      exchange.getLocalAddress().getPort(),
      exchange.getHttpContext().getPath(),
      headers,
      query.parameters
    )
  }

  case class LightweightServerResponseWriter(exchange: HttpExchange) extends ResponseWriter {

    def appendBody(body: String) = {
      exchange.getResponseBody().write(body.getBytes())
    }

    def setContentType(contentType: String) = {
      exchange.getRequestHeaders().add("ContentType", contentType)
    }

    def addHeader(key: String, value: String) = {
      exchange.getRequestHeaders().add(key, value)
    }

    def sendRedirect(url: String) = {
      ???
    }
  }

}

abstract class LeightweightServer(port: Int, context: String) extends RequestHandler {

    val httpServer = HttpServer.create(new InetSocketAddress(port), 0)

    val handler: HttpHandler = { (exchange: HttpExchange) =>
      val request = mapToRequest(exchange)
    }

    httpServer.createContext(context, handler)
    httpServer.setExecutor(null)
    httpServer.start()
    
}

