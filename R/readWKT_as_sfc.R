#' Read string in WKT format and convert it into an sf geometry list column.
#'
#' Read the input WKT string, parse it, and convert it into an sf geometry list 
#' column.
#'
#' @param text Character string of WKT
#' 
#' @param crs integer or character; coordinate reference system for the geometry
#'  as in function \code{sf::st_as_sfc}
#' 
#' @param ... further arguments passed on internally to function 
#' \code{sf::st_as_sfc}
#' 
#' @param GeoJSON logical; if TRUE, try to read geometries from GeoJSON text 
#' strings geometry as in function \code{sf::st_as_sfc}: see \code{sf::st_crs()}
#'
#' @rdname readWKT_as_sfc
#'
#' @name readWKT_as_sfc
#'
#' @examples
#' txt <- readLines(system.file("extdata", "map.wkt", package = "simviz"))
#' map_polygon_sfc <- readWKT_as_sfc(txt)
#' class(map_polygon_sfc)
#'
#' @export
readWKT_as_sfc <- function(x, crs = NA_integer_, precision = 0, check_ring_dir = FALSE, dim){
  
  mc      <- match.call()
  mc[[1]] <- sf::st_as_sfc
  wkt.sfc <- eval(mc, parent.frame())
  return(wkt.sfc)
}
