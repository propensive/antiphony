package antiphony

import java.net.InetSocketAddress;
 
import com.sun.net.httpserver._
import java.io.OutputStream
import java.net.URI

object LeightweightRequestHandler {
  val handler: HttpHandler = ???
}

abstract class LeightweightRequestHandler(port: Int, context: String) extends RequestHandler {
    val httpServer = HttpServer.create(new InetSocketAddress(port), 0);
    httpServer.createContext(context, ???)
}

