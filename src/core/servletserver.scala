package antiphony

import quarantine._
import euphemism._
import gastronomy._

import javax.servlet._, http._

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.annotation.tailrec

import java.net._
import java.io._

case class ServletResponseWriter(response: HttpServletResponse) extends ResponseWriter {
  def sendBody(body: String) = response.getWriter().println(body)
  def setContentType(contentType: String) = response.setContentType(contentType)
  def addHeader(key: String, value: String) = response.addHeader(key, value)
  def sendRedirect(url: String) = response.sendRedirect(url)
  def setStatus(code: Int): Unit = response.setStatus(code)
}

abstract class ServletWrapper() extends HttpServlet with RequestHandler {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

  override def doPost(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

  override def doPut(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

  override def doDelete(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)
  
  def handle(request: HttpServletRequest, response: HttpServletResponse): Unit = {
    val writer = ServletResponseWriter(response)
    handle(makeRequest(request)).respond(writer)
  }

  private def makeRequest(request: HttpServletRequest): Request = {

    val length = request.getContentLength

    Request(Method.unapply(request.getMethod).get,
      request.getContentType,
      if(length < 0) None else Some(length),
      Request.stream(request.getInputStream),
      request.getQueryString,
      request.isSecure,
      request.getServerName,
      request.getServerPort,
      request.getRequestURI,
      request.getHeaderNames.asScala.to[List].map { k => k -> request.getHeader(k) }.toMap,
      request.getParameterNames.asScala.to[List].map { k => k -> request.getParameterValues(k)(0) }.toMap)
  }
}
