#' @title Visualization of simulator inputs and outputs: Territory information.
#'
#' @description Make the visualization of the objects obtained from the MNO data simulator 
#' after being managed by simutils package.
#'
#' @param simData.list a list with the objects from the simulator created 
#' by the \code{read_simData} function from simutils R package.
#' 
#' @param aggre_level numeric, until now 1 for regions and 2 for subregions.
#' 
#' @param aggre_name string with the name of the aggregation level.
#' 
#' @param plot_title string with the general title of the plot.
#'  
#' @return It returns an object of class \code{ggplot} with the graph.
#'
#' @rdname simPlot_territory
#'
#' @name simPlot_territory
#'
#' @examples
#' filename_map      <- c(xml= system.file("extdata/input_files", "map.xml", package = "simutils"),  xsd= '')
#'
#'filename_network  <- c(csv= system.file("extdata/output_files/antennas.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/antennas_dict.xml", package = "simutils"))
#'
#'filename_signal <- c(csv= system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simutils"),  
#'  xml= system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", package = "simutils"))
#'
#'filename_coverage <- c(csv= system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/AntennaCells_dict.xml", package = "simutils"))
#'
#'filename_grid <- c(csv= system.file("extdata/output_files/grid.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/grid_dict.xml", package = "simutils")) 
#'
#'filename_individ <- c(csv= system.file("extdata/output_files/persons_dash.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/persons_dash_dict.xml", package = "simutils"))   
#'
#'filenames <- list(map                = filename_map,  
#'                  network_parameters = filename_network,
#'                  signal             = filename_signal,
#'                  coverage_cells     = filename_coverage,
#'                  grid               = filename_grid,
#'                  individuals        = filename_individ)
#'
#'simData <- read_simData(filenames, crs = 2062)
#'
#'simPlot_territory(simData, 
#'                  aggre_level = 1, 
#'                  aggre_name = "Regions", 
#'                  plot_title = 'Territorial map for simulation')
#'
#'
#' @import simutils ggplot2 stars sf data.table dplyr
#'
#' @export
simPlot_territory <- function(simData.list, aggre_level, aggre_name, plot_title){

  if(is.null(simData.list$map)){
    stop("It is mandatory to have a map of the territory for the visualization.")
  }
  
  map <- simData$map
 
  
  if(aggre_level == 1){
    
    map <- map %>%
      group_by(Region_long) %>%
      summarize(geometry = st_union(geometry))
    map <- cbind(map, st_coordinates(st_centroid(map)))
    p <- ggplot() +
      geom_sf(data = map, aes(fill = Region_long), alpha = 0.2) +
      coord_sf() +
      geom_label(data = map, aes(X, Y, label = Region_long), size = 5, fontface = "bold") +
      scale_fill_discrete(name = aggre_name) +
      theme_bw() +
      labs(title = plot_title, x = '', y = '') +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
    
  }
  
  if(aggre_level == 2){
    
    map <- cbind(map, st_coordinates(st_centroid(map)))
    
    p <- ggplot() +
      geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) +
      coord_sf() +
      geom_label(data = map, aes(X, Y, label = Subregion_long), size = 5, fontface = "bold") +
      scale_fill_discrete(name = aggre_name) +
      theme_bw() +
      labs(title = plot_title, x = '', y = '') +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
    
  }

  return(p)
  
}
