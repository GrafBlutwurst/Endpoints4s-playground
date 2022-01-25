package ch.grafblutwurst

import cats.syntax.all._

trait Http4sTracingIdAndMetaAlgebra2
    extends endpoints4s.http4s.server.EndpointsWithCustomErrors
    with TracingIdAndMetaAlgebra2 {

  def innerEnvelopeRequestEntity: RequestEntity[EnvelopeBodyP]

  override final def envelopeRequestEntity[A](
      base: RequestEntity[A]
  ): RequestEntity[(EnvelopeBodyP, A)] =
    req =>
      for {
        innerEither <- innerEnvelopeRequestEntity(req)
        result <- innerEither.flatTraverse[Effect, (EnvelopeBodyP, A)] {
          envelopeBody =>
            base(req).map(_.map(baseBody => (envelopeBody, baseBody)))
        }
      } yield result

}
