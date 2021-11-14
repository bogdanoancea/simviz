#' @examples 
#' filename_map      <- c(
#'  xml= system.file("extdata/input_files", "map.xml", package = "simviz"),
#'  xsd= '')
#'   
#' filename_network  <- c(
#'  csv= system.file("extdata/output_files/antennas.csv", package = "simviz"),
#'  xml= system.file("extdata/metadata/output_files/antennas_dict.xml", 
#'                    package = "simviz"))
#'                    
#' filename_signal <- c(
#'  csv= system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simviz"),
#'  xml= system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", 
#'                    package = "simviz"))
#'                  
#' filename_coverage <- c(
#'  csv= system.file("extdata/output_files", "AntennaCells_MNO1.csv", 
#'                   package = "simviz"),
#'  xml= system.file("extdata/metadata/output_files/AntennaCells_dict.xml", 
#'                   package = "simviz"))
#'                        
#' filename_grid <- c(
#'   csv= system.file("extdata/output_files/grid.csv", package = "simviz"),
#'   xml= system.file("extdata/metadata/output_files/grid_dict.xml", 
#'                    package = "simviz")) 
#' 
#' filename_individ <- c(
#'   csv= system.file("extdata/output_files/persons_dash.csv", package = "simviz"),
#'   xml= system.file("extdata/metadata/output_files/persons_dash_dict.xml", 
#'                    package = "simviz"))   
#'                        
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   signal             = filename_signal,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ)
#'   
#' simData <- simutils::read_simData(filenames, crs = 2062)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stars)
# map
# Subregions
map <- simData$map
map <- cbind(map, st_coordinates(st_centroid(map)))
ggplot() +
  geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) + 
  geom_label(data = map, aes(X, Y, label = Subregion_long), size = 5, fontface = "bold") +
  scale_fill_discrete(name = "Subregions") +
  theme_bw() +
  labs(title = 'Territorial map for simulation', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

# Regions
map <- simData$map
map <- map %>%
  group_by(Region_long) %>% 
  summarize(geometry = st_union(geometry))
map <- cbind(map, st_coordinates(st_centroid(map)))
ggplot() +
  geom_sf(data = map, aes(fill = Region_long), alpha = 0.2) + 
  geom_label(data = map, aes(X, Y, label = Region_long), size = 5, fontface = "bold") +
  scale_fill_discrete(name = "Regions") +
  theme_bw() +
  labs(title = 'Territorial map for simulation', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5))



# network on map
map <- simData$map
network <- simData$network
network <- cbind(network, st_coordinates(network))
ggplot() +
  geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) + 
  geom_sf(data = network) +
  geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
  scale_fill_discrete(name = "Subregions") +
  theme_bw() +
  labs(title = 'Antenna positions', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5))


# network on map with coverage cells
map <- simData$map
network <- simData$network
network <- cbind(network, st_coordinates(network))
coverage <- simData$coverage
ggplot() +
  geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) + 
  geom_sf(data = network) +
  geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
  geom_sf(data = coverage, fill = NA) +
  scale_fill_discrete(name = "Subregions") +
  theme_bw() +
  labs(title = 'Antenna positions', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5))


# network on map with grid
map <- simData$map
network <- simData$network
network <- cbind(network, st_coordinates(network))
grid <- simData$grid
ggplot() +
  #geom_sf(data = map, alpha = 0.2) + 
  #scale_fill_discrete(name = "Subregions") +
  geom_stars(data = grid[,,,1]) + 
  geom_sf(data = network[1,], size = 1) +
  #geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
  facet_wrap(~ `Antenna ID`) +
  theme_bw() +
  labs(title = 'RSS per Antenna', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7))
