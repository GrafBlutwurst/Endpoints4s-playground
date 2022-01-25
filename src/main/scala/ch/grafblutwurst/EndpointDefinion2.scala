package ch.grafblutwurst

import ch.grafblutwurst.tech.rivero.edgeconnector.api.EnvelopeAlgebra


trait EndpointDefinition2 extends endpoints4s.algebra.EndpointsWithCustomErrors with TracingIdAndMetaAlgebra2 {


  val testEndpoint:Endpoint[Envelope[Unit], Unit] = endpoint(
    envelopedRequest(
      method = Get,
      url = path / "test",
      entity = emptyRequest,
      docs = None,
      headers = emptyRequestHeaders
    ),
    ok(
      entity = emptyResponse
    )
  )

}
