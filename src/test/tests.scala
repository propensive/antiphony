package antiphony

import probably._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object Tests extends TestApp {

  override def tests(): Unit = {
    test("http get") {
      Http.get("http://example.com", Set.empty).map(new String(_).take(15)).to[Try]
    }.assert(_ == Success("<!doctype html>"))

    test("https get") {
      Http.get("https://example.com", Set.empty).map(new String(_).take(15)).to[Try]
    }.assert(_ == Success("<!doctype html>"))

    test("Pinata gateway get") {
      val hash = "QmRqSLwNGKe6m7e536SuCKiPKtgvqTysEgwQdZtciJ4Ks6"
      val url = s"https://gateway.pinata.cloud/ipfs/$hash"
      Http.get(url, Set.empty).map(new String(_).take(9)).to[Try]
    }.assert(_ == Success("version\t6"))

    test("Pinata node API get") {
      val hash = "QmRqSLwNGKe6m7e536SuCKiPKtgvqTysEgwQdZtciJ4Ks6"
      val url = s"https://gateway.pinata.cloud/api/v0/get?arg=$hash"
      Http.get(url, Set.empty).map(new String(_).take(46)).to[Try]
    }.assert(_ == Success("QmRqSLwNGKe6m7e536SuCKiPKtgvqTysEgwQdZtciJ4Ks6"))

  }
}