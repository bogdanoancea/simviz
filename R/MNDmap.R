#' Map underlying polygon.
#'
#' Map the udnerlying polygon representing the geographical territory as an
#'  \code{sf} object.
#'
#' @param map_polygon \code{sf} object with the polygon representing the 
#' territory.
#'
#' @rdname MNDmap
#'
#' @name MNDmap
#'
#' @examples
#' txt <- readLines(system.file("extdata", "map.wkt", package = "simviz"))
#' map_polygon <- readWKT_as_sf(txt)
#' MNDmap(map_polygon)
#'
#' @import ggplot2
#'
#' @export
MNDmap <- function(map_polygon){
  
  p <- ggplot(map_polygon) + geom_sf()
  return(p)
  
}
