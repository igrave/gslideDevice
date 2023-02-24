batch_req <- function(state) {
  reqs <- do.call(Request, state$rdata$batch_requests)
  presentations.batchUpdate(
    presentationId = state$rdata$slides_id,
    BatchUpdatePresentationRequest = BatchUpdatePresentationRequest(requests = reqs)
  )
  state$rdata$batch_requests <- NULL
}

send_request <- function(request, state) {
  reqs <- Request(request)
  res <- presentations.batchUpdate(
    presentationId = state$rdata$slides_id,
    BatchUpdatePresentationRequest = BatchUpdatePresentationRequest(requests = reqs)
  )
  res
}
