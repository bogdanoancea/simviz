#' Grid over the map.
#'
#' Plot the grid over the map (object from MNDmap function)
#'
#' @param MNDmap \code{ggplot} object with the polygon representing the 
#' territory from MNDmap function.
#' 
#' @param grid \code{stars} object with the grid over the territory.
#'
#' @rdname MNDmap_grid
#'
#' @name MNDmap_grid
#'
#' @examples
#' txt <- readLines(system.file("extdata/input_files", "map.wkt", package = "simviz"))
#' map_polygon <- readWKT_as_sf(txt)
#' MNDmap(map_polygon)
#'
#' @import ggplot2 stars
#'
#' @export
MNDmap_grid <- function(MNDmap, grid){
  
  p <- MNDmap + geom_stars(data = grid, fill = NA)
  return(p)
  
}
