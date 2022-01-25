package ch.grafblutwurst

import ch.grafblutwurst.tech.rivero.edgeconnector.api.EnvelopeAlgebra
import java.util.UUID
import endpoints4s.Tupler
import endpoints4s.algebra.Documentation

trait TracingIdAndMetaAlgebra1 extends EnvelopeAlgebra {
  final override type EnvelopeUrlP = Unit 
  final override type EnvelopeHeaderP = UUID 
  final override type EnvelopeBodyP = TracingIdAndMetaAlgebra1.MetaDataFromBody 



  def tracingIdRequestHeader: RequestHeaders[UUID]

  def metaDataRequestEntity[A](base: RequestEntity[A]): RequestEntity[(EnvelopeBodyP, A)]

  override def envelopedRequest[UrlP, BodyP, HeadersP, UB, UBH](
    method: Method, 
    url: Url[UrlP], 
    entity: RequestEntity[BodyP], 
    docs: Documentation, 
    headers: RequestHeaders[HeadersP])(
    implicit 
      tuplerUB: Tupler.Aux[UrlP,BodyP,UB],
      tuplerUBH: Tupler.Aux[UB,HeadersP,UBH]
  ): Request[Envelope[UBH]] = envelopedRequestBase(
    method,
    url,
    metaDataRequestEntity(entity),
    docs, 
    tracingIdRequestHeader ++ headers
  )(
    xmapUrlF = leftUnit, 
    xmapBodyF = identity,
    xmapHeadersF = identity,
  )(
    xmapUrlG = extract, 
    xmapBodyG = Tuple2.apply,
    xmapHeadersG = Tuple2.apply,
  )


}

object TracingIdAndMetaAlgebra1 {
  final case class MetaDataFromBody(meta: String) extends AnyVal 
}

