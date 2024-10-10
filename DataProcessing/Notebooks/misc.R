#' make version
#' @param X chr base of version identifier e.g. v1
#' @param n num number of versions to create
#' @return chr version identifiers
make_version = function(X = "v1", n = 100){
  v = sprintf("%s.%0.3i", X, seq_len(n))
  return (v)
}
