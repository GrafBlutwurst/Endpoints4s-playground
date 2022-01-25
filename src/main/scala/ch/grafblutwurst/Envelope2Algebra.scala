package ch.grafblutwurst

import cats.Functor
import endpoints4s.Tupler
import endpoints4s.algebra.Documentation
import endpoints4s.algebra.Endpoints


//This is a slightly different and probably better encoding, leveraging map like structure of query strings as well 
//Unlike the other formulation it is however restricted to query strings
trait Envelope2Algebra extends endpoints4s.algebra.Requests {

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

  def envelopeRequestHeader: RequestHeaders[EnvelopeHeaderP]

  def envelopeRequestEntity[A](base: RequestEntity[A]): RequestEntity[(EnvelopeBodyP, A)]

  def envelopeQuerySegment: QueryString[EnvelopeUrlP]


  final def envelopedRequest[UrlP, BodyP, HeadersP, UB, UBH](
      method: Method,
      url: Url[UrlP],
      entity: RequestEntity[BodyP],
      docs: Documentation,
      headers: RequestHeaders[HeadersP]
  )(implicit
      tuplerUB: Tupler.Aux[UrlP, BodyP, UB],
      tuplerUBH: Tupler.Aux[UB, HeadersP, UBH]
  ): Request[Envelope[UBH]] = 
    request[
      UrlP, 
      (EnvelopeBodyP, BodyP), 
      (EnvelopeHeaderP, HeadersP),
      (UrlP, (EnvelopeBodyP, BodyP)),
      ((UrlP, (EnvelopeBodyP, BodyP)), (EnvelopeHeaderP, HeadersP)) 
    ](
      method,
      url,
      envelopeRequestEntity(entity),
      docs,
      envelopeRequestHeader ++ headers,
    ).addQueryString(envelopeQuerySegment)
      .xmap[Envelope[UBH]] {
        case ((url, (envelopeBody, body)),(envelopeHeader, headers),envelopeUrl) => 
          Envelope(
            envelopeUrl,
            envelopeHeader,
            envelopeBody,
            tuplerUBH(tuplerUB(url,body),headers)
          )
      } {
        case Envelope(envelopeUrl, envelopeHeader, envelopeBody, ubh) => 
          val (ub,h) = tuplerUBH.unapply(ubh)
          val (u,b) = tuplerUB.unapply(ub)
          ((u, (envelopeBody, b)), (envelopeHeader, h), envelopeUrl)
      }

  


}

