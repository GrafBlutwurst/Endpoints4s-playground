package ch.grafblutwurst

import ch.grafblutwurst.tech.rivero.edgeconnector.api.EnvelopeAlgebra
import endpoints4s.Tupler
import endpoints4s.algebra.Documentation
import TracingIdAndMetaAlgebra2._

trait TracingIdAndMetaAlgebra2 extends Envelope2Algebra {
  final override type EnvelopeUrlP = TracingIdAndMetaAlgebra2.MetaDataFromUrl 
  final override type EnvelopeHeaderP = TracingIdAndMetaAlgebra2.TracingId 
  final override type EnvelopeBodyP = TracingIdAndMetaAlgebra2.MetaDataFromBody 


  final override def envelopeRequestHeader: RequestHeaders[EnvelopeHeaderP] = requestHeader("tracing-id").xmap(TracingId.apply)(_.tracingId)

  //This has to be overriden specifically aware of the underlying HTTP implementation since the way this composition works is not generalized/genralizable
  //Indeed i think it would be wrong to require a semigroupal for RequestEntity as that is not possible in general
  //
  //def envelopeRequestEntity[A](base: RequestEntity[A]): RequestEntity[(EnvelopeBodyP, A)]

  final override def envelopeQuerySegment: QueryString[EnvelopeUrlP] = qs[String]("urlmeta")(stringQueryString).xmap(MetaDataFromUrl.apply)(_.meta)

}

object TracingIdAndMetaAlgebra2 {
  final case class TracingId(tracingId: String) extends AnyVal 
  final case class MetaDataFromBody(meta: String) extends AnyVal 
  final case class MetaDataFromUrl(meta: String) extends AnyVal 
}


