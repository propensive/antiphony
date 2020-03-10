
package antiphony

import quarantine._
import euphemism._

import javax.servlet._, http._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec

import java.net._
import java.io._

case class ServletResponseWriter(r: HttpServletResponse) extends ResponseWriter {

  def appendBody(body: String) = {
    r.getWriter().println(body)
  }

  def setContentType(contentType: String) = {
    r.setContentType(contentType)
  }

  def addHeader(key: String, value: String) = {
    r.addHeader(key, value)
  }

  def sendRedirect(url: String) = {
    r.sendRedirect(url)
  }
}

abstract class ServlerRequestHandler() extends HttpServlet with RequestHandler {

  override def doGet(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = {
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(mapRequest(servletRequest)).write(responseWriter)
  }

  override def doPost(
    servletRequest: HttpServletRequest,
    servletResponse: HttpServletResponse
  ): Unit = {
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(mapRequest(servletRequest)).write(responseWriter)
  }

  private def mapRequest(request: HttpServletRequest): Request = {
    val in = request.getInputStream
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

    Request(Method.from(request.getMethod),
      request.getContentType,
      request.getContentLength,
      content,
      request.getQueryString,
      request.isSecure,
      request.getServerName,
      request.getServerPort,
      request.getRequestURI,
      request.getHeaderNames.asScala.to[List].map { k => k -> request.getHeader(k) }.toMap,
      request.getParameterNames.asScala.to[List].map { k => k -> request.getParameterValues(k).to[List] }.toMap)
  }

}
