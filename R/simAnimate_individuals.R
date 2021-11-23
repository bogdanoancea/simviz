#' @title Visualization of simulator inputs and outputs: Individuals information.
#'
#' @description Make the visualization of the objects obtained from the MNO data simulator 
#' after being managed by simutils package.
#'
#' @param simData.list a list with the objects from the simulator created 
#' by the \code{read_simData} function from simutils R package.
#' 
#' @param time numeric vector, the specific time to be plotted.
#' 
#' @param size numeric, parameter of geom_sf().
#' 
#' @param size_var string, the name of the variable to be used for the size of points.
#' 
#' @param size_name string, the title of legend for size_var.
#' 
#' @param plot_title string with the general title of the plot.
#' 
#' @return It returns an object of class \code{ggplot} with the graph.
#'
#' @rdname simAnimate_individuals
#'
#' @name simAnimate_individuals
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
#'
#' @import simutils ggplot2 stars sf ggrepel viridis data.table dplyr
#'
#' @export
simAnimate_individuals <- function(simData.list, 
                            time = 0,
                            individuals_subset, 
                            size = 1, size_var = NULL, size_name = NULL,
                            plot_title = ""){

  if(is.null(simData.list$map)){
    stop("It is mandatory to have a map of the territory for the visualization.")
  }
  if(is.null(simData.list$network)){
    stop("It is mandatory to have network information for the visualization.")
  }
  
  
  p <- ggplot() 
  
  map <- simData$map
  network <- simData$network
  network$t <- NULL
  
  individuals <- simData$individuals
  individuals <- individuals[
    individuals$t %in% time,][
      individuals$`Person ID` %in% individuals_subset,]
  individuals_subset <- individuals[!is.na(individuals$t),]
  
  
  p <- p + 
    geom_sf(data = map, fill = NA) 
  
  if(!is.null(size_var)){
    p <- p +  geom_sf(data = network, aes(size = get(size_var)))
  }
  if(is.null(size_var) & !is.null(size)){
    p <- p +  geom_sf(data = network, size = size)
  }
  
  p <- p +
    geom_sf(data = individuals_subset, aes(color = factor(nDev)), size = 3) +
    coord_sf() +
    theme_bw() +
    labs(title = plot_title, x = '', y = '', color = 'No. Devices') +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
          axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7)) +
    transition_states(states = t) +
    ease_aes('linear')
  

  return(p)
  
}
