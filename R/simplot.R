#' Visualization of simulator outputs.
#'
#' Make the visualization of the objects obtained from the MNO data simulator.
#'
#' @param simElements a list with the output objects from the simulator created 
#' by the \code{create_simElements} function.
#' 
#' @param control.map a list with the visualization parameters for the map object:
#' size, ...
#' 
#' @param control.grid a list with the visualization parameters for the grid object:
#' fill, ...
#' 
#' @param control.network a list with the visualization parameters for the network object.
#' 
#' @param control.coverage a list with the visualization parameters for the coverage object.
#' 
#' @param control.individuals a list with the visualization parameters for the individuals object.
#' 
#' @return It returns an object of class \code{ggplot} with the graph.
#'
#' @rdname simplot
#'
#' @name simplot
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
#'
#' @import simutils ggplot2 stars gganimate viridis data.table
#'
#' @export
simplot <- function(simElem.list, control.map = list(),
                   control.grid = list(),
                   control.network = list(),
                   control.coverage = list(),
                   control.individuals = list(animate = FALSE)){

  if(is.null(simElem.list$map)){
    stop("It is mandatory to have a map of the terrotory for the visualization.")
  }
  
  p <-  ggplot() +
    geom_sf(data = simElem.list$map, size = control.map$size) +
    labs(x = 'longitude', y = 'latitude') 
  
  if(!is.null(simElem.list$grid)){
    
    p <- p + geom_stars(data = simElem.list$grid, fill = control.grid$fill)
    
  }
  
  if(!is.null(simElem.list$coverage) & is.null(simElem.list$network)){
    
    p <- p + geom_sf(data = simElem.list$coverage, alpha = control.coverage$alpha)
                     
  }
  
  if(!is.null(simElem.list$coverage) & !is.null(simElem.list$network) & !is.null(control.coverage$fill)){
    
    if(control.coverage$fill == "power"){
    
      p <- p + geom_sf(data = simElem.list$coverage, 
                       aes(fill = simElem.list$network$power),
                       alpha = control.coverage$alpha) +
        labs(fill = "power (dBm)")
      
    }
    
  }
  
  if(!is.null(simElem.list$network)){
    
      p <- p +  geom_sf(data = simElem.list$network) +
      geom_sf_label(data = simElem.list$network, 
                    mapping = aes(label = get(control.network$label)), 
                    nudge_x = control.network$nudge_x, nudge_y = control.network$nudge_y)
      
  }
  
  if(!is.null(simElem.list$individuals)){
    
    if(is.null(control.individuals$t) & !control.individuals$animate){
      
      stop("A specific time must be set or animate = TRUE")
      
    }
    if(!is.null(control.individuals$t) & !control.individuals$animate){
      
      p <- p + geom_point(data = simElem.list$individuals[abs(t - control.individuals$t) < .Machine$double.eps], 
                          mapping = aes(x, y, color = factor(nDev))) +
        labs(color = "Num. Devices")
      
    }
    if(is.null(control.individuals$t) & control.individuals$animate){
      
      p <- p + geom_point(data = simElem.list$individuals, 
                          mapping = aes(x, y, color = factor(nDev))) +
        transition_states(states = t) +
        labs(color = "Num. Devices")
        
    }
    
  }
  
  p <- p + scale_fill_viridis() + theme_bw()
  
  
  return(p)
  
}
