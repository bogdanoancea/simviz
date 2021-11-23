#' @title Visualization of simulator inputs and outputs: Network information.
#'
#' @description Make the visualization of the objects obtained from the MNO data simulator 
#' after being managed by simutils package.
#'
#' @param simData.list a list with the objects from the simulator created 
#' by the \code{read_simData} function from simutils R package.
#' 
#' @param map.plot logical, TRUE if map must be plot (then, grid = FALSE). 
#' 
#' @param aggre_level numeric, until now 1 for regions and 2 for subregions.
#' 
#' @param aggre_name string with the name of the aggregation level.
#' 
#' @param coverage.plot logical, TRUE if coverage must be plot. 
#' 
#' @param grid.plot logical, TRUE if grid must be plot (then, map = FALSE). 
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
#' @rdname simplot_network
#'
#' @name simplot_network
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
#'simplot_network(simData, 
#'                map.plot = TRUE,
#'                aggre_level = 1, 
#'                aggre_name = "Regions", 
#'                coverage.plot = FALSE,
#'                size_var = "power", 
#'                size_name = "Power",
#'                grid.plot = FALSE,
#'                plot_title = 'Antenna positions')
#'
#'
#' @import simutils ggplot2 stars sf ggrepel viridis data.table dplyr
#'
#' @export
simplot_network <- function(simData.list, 
                            map.plot, aggre_level = NULL, aggre_name = NULL, 
                            coverage.plot, 
                            grid.plot,
                            size = 1, size_var = NULL, size_name = NULL,
                            plot_title = ""){

  if(map.plot & is.null(simData.list$map)){
    stop("It is mandatory to have a map of the territory for the visualization.")
  }
  if(is.null(simData.list$network)){
    stop("It is mandatory to have network information for the visualization.")
  }
  if(coverage.plot & is.null(simData.list$coverage)){
      stop("It is mandatory to have coverage information for the visualization.")
  }
  
  
  p <- ggplot() 
  
  if(map.plot)  map <- simData$map
  
  network <- simData$network
  network <- cbind(network, st_coordinates(network))
  
  if(coverage.plot){
    
    coverage <- simData$coverage
    
  }
  if(grid.plot){
    
    network <- cbind(network, cell_idx = seq_along(network$Antenna.ID))
    
    if(coverage.plot){
      
      coverage <- cbind(coverage, cell_idx = seq_along(coverage$AntennaId))
      
    }
    
    grid <- simData$grid
    names(attr(grid, 'dimensions'))[3] <- 'cell_idx'
    cell_labels <- network$Antenna.ID
    cell_names <- seq(along = cell_labels)
    names(cell_labels) <- cell_names
    
  }

  if(map.plot){
 
    if(aggre_level == 1){
      
      map <- map %>%
        group_by(Region_long) %>%
        summarize(geometry = st_union(geometry))
      map <- cbind(map, st_coordinates(st_centroid(map)))
      
      p <- p +
        geom_sf(data = map, aes(fill = Region_long), alpha = 0.2) 
      
      if(!is.null(size_var)){
        p <- p +  geom_sf(data = network, aes(size = get(size_var)))
      }
      if(is.null(size_var) & !is.null(size)){
        p <- p +  geom_sf(data = network, size = size)
      }

      
      if(coverage.plot){
        
        p <- p +
          geom_sf(data = coverage, fill = NA)
        
      }
      
       
      p <- p +
        geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
        scale_fill_discrete(name = aggre_name) +
        theme_bw() +
        labs(title = plot_title, x = '', y = '') +
        theme(plot.title = element_text(size = 16, hjust = 0.5))
      
      
      
    }
    
    if(aggre_level == 2){
      
      map <- cbind(map, st_coordinates(st_centroid(map)))
      
      p <- p +
        geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) 
      
      if(!is.null(size_var)){
        p <- p +  geom_sf(data = network, aes(size = get(size_var)))
      }
      if(is.null(size_var) & !is.null(size)){
        p <- p +  geom_sf(data = network, size = size)
      }

      
      if(coverage.plot){
        
        p <- p +
          geom_sf(data = coverage, fill = NA)
        
      }
      
      p <- p +
        geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
        scale_fill_discrete(name = aggre_name) +
        theme_bw() +
        labs(title = plot_title, x = '', y = '') +
        theme(plot.title = element_text(size = 16, hjust = 0.5))
      
    } 
  
  } #end if map
  
  if(!map.plot){
  
    if(grid.plot){
      
      p <- p +
        geom_stars(data = grid, interpolate = TRUE) 
      
      if(!is.null(size_var)){
        p <- p +  geom_sf(data = network, aes(size = get(size_var)))
      }
      if(is.null(size_var) & !is.null(size)){
        p <- p +  geom_sf(data = network, size = size)
      }
        
      if(coverage.plot){
        
        p <- p +
          geom_sf(data = coverage, fill = NA)
        
      }
        p <- p +
          coord_sf() +
        facet_wrap(~ cell_idx, labeller = labeller(cell_idx = cell_labels)) +
        scale_fill_gradient(low = "white", high = "black", na.value = 'white', name = 'RSS (dBm)') +
        theme_bw() +
        labs(title = plot_title, x = '', y = '', ) +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
              axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7))
      
      
    }
    
  } #end if !map
  
  
  if(!is.null(size_name)){
    
    p <- p +
      scale_size(name = size_name)
    
  }
  


  return(p)
  
}
