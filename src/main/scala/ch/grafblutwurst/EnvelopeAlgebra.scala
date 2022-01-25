package ch.grafblutwurst

package tech.rivero.edgeconnector.api

import cats.Functor
import endpoints4s.Tupler
import endpoints4s.algebra.Documentation
import endpoints4s.algebra.Endpoints

/** The big problem here is that they're non-compositional ;_; Given a trait of
  * EnvelopeAlgebra EA and another one EB Intuitively it should be able to
  * compose them using applicative/semigroupal
  * However we can't do it based on two instances since we can't ensure they extends "the same" endpoints4s.algebra.Requests and we'd have to provide its members (leaky abstraction)
  * And I can't figure out how to do it based only on a trait. I have thought about doing a ComposeEnvelopeAlgebra trait, but can't nail it down.
  * 
  * This is a simplified version of what we currently use as we specifically use it for frontend-tracing-id & authentication data. So we also transform the response to track auth failure.
  * However the auth failure could be a similar EnvelopeAlgebra based on coporduct rather than product
  */
trait EnvelopeAlgebra extends endpoints4s.algebra.Requests {

  type EnvelopeUrlP
  type EnvelopeHeaderP
  type EnvelopeBodyP

  case class Envelope[A](
      urlPart: EnvelopeUrlP,
      headerPart: EnvelopeHeaderP,
      bodyPart: EnvelopeBodyP,
      requestData: A
  )

  object Envelope {
    val functor: Functor[Envelope] = new Functor[Envelope] {
      def map[A, B](fa: Envelope[A])(f: A => B): Envelope[B] = fa.copy(requestData = f(fa.requestData))
    }
  }

  // used for easy handling of units once the types are overriden
  protected def leftUnit[A]: A => (Unit, A) = a => ((), a)
  protected def extract[A]: (Unit, A) => A = (_, a) => a

  final def envelopedRequestBase[
      UrlP,
      BodyP,
      HeadersP,
      UrlA,
      BodyA,
      HeadersA,
      UrlO,
      BodyO,
      HeadersO,
      UB,
      UBH,
      OUB,
      OUBH
  ](
      method: Method,
      url: Url[UrlP],
      entity: RequestEntity[BodyP],
      docs: Documentation = None,
      headers: RequestHeaders[HeadersP]
  )(
      xmapUrlF: UrlP => (EnvelopeUrlP, UrlO),
      xmapBodyF: BodyP => (EnvelopeBodyP, BodyO),
      xmapHeadersF: HeadersP => (EnvelopeHeaderP, HeadersO)
  )(
      xmapUrlG: (EnvelopeUrlP, UrlO) => UrlP,
      xmapBodyG: (EnvelopeBodyP, BodyO) => BodyP,
      xmapHeadersG: (EnvelopeHeaderP, HeadersO) => HeadersP
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, BodyP, UB],
      tuplerUBH: Tupler.Aux[UB, HeadersP, UBH],
      tuplerOUB: Tupler.Aux[UrlO, BodyO, OUB],
      tuplerOUBH: Tupler.Aux[OUB, HeadersO, OUBH]
  ): Request[Envelope[OUBH]] =
    request[UrlP, BodyP, HeadersP, UB, UBH](
      method,
      url,
      entity,
      docs,
      headers
    ).xmap[Envelope[OUBH]] { ubh =>
      val (ub, h) = tuplerUBH.unapply(ubh)
      val (u, b) = tuplerUB.unapply(ub)
      val (urlA, urlO) = xmapUrlF(u)
      val (bodyA, bodyO) = xmapBodyF(b)
      val (headersA, headersO) = xmapHeadersF(h)

      Envelope(
        urlA,
        headersA,
        bodyA,
        tuplerOUBH(tuplerOUB(urlO, bodyO), headersO)
      )
    } { case Envelope(urlA, headersA, bodyA, oubh) =>
      val (oub, headersO) = tuplerOUBH.unapply(oubh)
      val (urlO, bodyO) = tuplerOUB.unapply(oub)
      val urlP = xmapUrlG(urlA, urlO)
      val bodyP = xmapBodyG(bodyA, bodyO)
      val headersP = xmapHeadersG(headersA, headersO)
      tuplerUBH(tuplerUB(urlP, bodyP), headersP)
    }

  def envelopedRequest[UrlP, BodyP, HeadersP, UB, UBH](
      method: Method,
      url: Url[UrlP],
      entity: RequestEntity[BodyP],
      docs: Documentation,
      headers: RequestHeaders[HeadersP]
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, BodyP, UB],
      tuplerUBH: Tupler.Aux[UB, HeadersP, UBH]
  ): Request[Envelope[UBH]]

}
