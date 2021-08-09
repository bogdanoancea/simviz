#' Read string in WKT format and convert it into an sf object.
#'
#' Read the input WKT string, parse it, and convert it into an sf object.
#'
#' @param text character string of WKT
#'
#' @param character vector of unique ids to label geometries as in function
#' \code{rgeos::readWKT}. Length must match the number of subgeometries in the
#' WKT
#'
#' @param p4s Either a character string or an object of class \code{CRS} as in
#' the function \code{rgeos::readWKT}
#'
#' @rdname readWKT_as_sf
#'
#' @name readWKT_as_sf
#'
#' @examples
#' txt <- system.file("extdata", "map.wkt", package = "simviz")
#' readWKT_as_sf <- readWKT_as_sf(txt)
#'
#' @export
readWKT_as_sf <- function(text, id = NULL, p4s = NULL, ...){

  mc      <- match.call()
  mc[[1]] <- rgeos::readWKT
  wktPoly <- eval(mc, parent.frame())
  wkt.sf  <- sf::st_as_sf(wktPoly, ...)
  return(wkt.sf)
}
