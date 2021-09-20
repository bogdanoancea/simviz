#' Grid over the map.
#'
#' Plot the grid over the map (object from MNDmap function)
#'
#' @param MNDmap \code{ggplot} object with the polygon representing the 
#' territory from MNDmap function with or without the grid.
#' 
#' @param grid \code{stars} object with the grid over the territory.
#'
#' @rdname MNDmap_networkCells
#'
#' @name MNDmap_networkCells
#'
#' @examples
#' txt <- readLines(system.file("extdata/input_files", "map.wkt", package = "simviz"))
#' map_polygon <- readWKT_as_sf(txt)
#' MNDmap(map_polygon)
#'
#' @import ggplot2 stars
#'
#' @export
MNDmap_networkCells <- function(MNDmap, network_info, coverage){
  
  p <- MNDmap + 
    geom_sf(data = coverage, aes(fill = network_info$power), alpha = 0.1) +
    geom_sf(data = network)
  return(p)
  
}
