package ch.grafblutwurst

import ch.grafblutwurst.tech.rivero.edgeconnector.api.EnvelopeAlgebra


trait EndpointDefinition1 extends endpoints4s.algebra.EndpointsWithCustomErrors with TracingIdAndMetaAlgebra1 {


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
