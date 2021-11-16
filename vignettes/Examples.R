library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stars)
library(gganimate)

 filename_map      <- c(
  xml= system.file("extdata/input_files", "map.xml", package = "simviz"),
  xsd= '')
   
 filename_network  <- c(
  csv= system.file("extdata/output_files/antennas.csv", package = "simviz"),
  xml= system.file("extdata/metadata/output_files/antennas_dict.xml", 
                    package = "simviz"))
                    
 filename_signal <- c(
  csv= system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simviz"),
  xml= system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", 
                    package = "simviz"))
                  
 filename_coverage <- c(
  csv= system.file("extdata/output_files", "AntennaCells_MNO1.csv", 
                   package = "simviz"),
  xml= system.file("extdata/metadata/output_files/AntennaCells_dict.xml", 
                   package = "simviz"))
                        
 filename_grid <- c(
   csv= system.file("extdata/output_files/grid.csv", package = "simviz"),
   xml= system.file("extdata/metadata/output_files/grid_dict.xml", 
                    package = "simviz")) 
 
 filename_individ <- c(
   csv= system.file("extdata/output_files/persons_dash.csv", package = "simviz"),
   xml= system.file("extdata/metadata/output_files/persons_dash_dict.xml", 
                    package = "simviz"))   
                        
 filenames <- list(
   map                = filename_map,
   network_parameters = filename_network,
   signal             = filename_signal,
   coverage_cells     = filename_coverage,
   grid               = filename_grid,
   individuals        = filename_individ)
   
 simData <- simutils::read_simData(filenames, crs = 2062)
# map
# Subregions
map <- simData$map
map <- cbind(map, st_coordinates(st_centroid(map)))
ggplot() +
  geom_sf(data = map, aes(fill = Subregion_long), alpha = 0.2) +
  coord_sf() +
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
  coord_sf() +
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
  geom_sf(data = network, aes(size = power)) +
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
  geom_sf(data = network, aes(size = power)) +
  geom_text_repel(data = network, aes(x = X, y = Y, label = Antenna.ID), fontface = "bold") +
  geom_sf(data = coverage, fill = NA) +
  scale_fill_discrete(name = "Subregions") +
  theme_bw() +
  labs(title = 'Antenna coverage cells', x = '', y = '') +
  theme(plot.title = element_text(size = 16, hjust = 0.5))


# network with RSS grid
map <- simData$map
network <- simData$network
network <- cbind(network, st_coordinates(network))
network <- cbind(network, cell_idx = seq_along(network$Antenna.ID))
grid <- simData$grid
names(attr(grid, 'dimensions'))[3] <- 'cell_idx'
cell_labels <- network$Antenna.ID
cell_names <- seq(along = cell_labels)
names(cell_labels) <- cell_names
ggplot() +
  geom_stars(data = grid) +
  geom_sf(data = network, size = 1) +
  coord_sf() +
  facet_wrap(~ cell_idx, labeller = labeller(cell_idx = cell_labels)) +
  scale_fill_gradient(low = "white", high = "black", na.value = 'white', name = 'RSS (dBm)') +
  theme_bw() +
  labs(title = 'RSS per Antenna', x = '', y = '', ) +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7))

  
# individuals on map (initial time)
map <- simData$map
individuals <- simData$individuals
individuals_t0 <- individuals[individuals$t == 0,]
ggplot() +
  geom_sf(data = map, fill = NA) +
  geom_sf(data = network, size = 1) +
  geom_sf(data = individuals_t0, aes(color = factor(nDev))) +
  coord_sf() +
  theme_bw() +
  labs(title = 'Individuals at t=0', x = '', y = '', color = 'No. Devices') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7))


# individuals on map
map <- simData$map
network$t <- NULL
individuals <- simData$individuals
individuals <- individuals[
  individuals$t <= 20,][
  individuals$`Person ID` %in% c('778', '771'),]
ggplot() +
  geom_sf(data = map, fill = NA) +
  geom_sf(data = network, size = 1) +
  geom_sf(data = individuals, aes(color = factor(nDev), size = 3)) +
  coord_sf() +
  theme_bw() +
  labs(title = 'Individuals at t={closest_state}', x = '', y = '', color = 'No. Devices') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7)) +
  transition_states(states = t) +
  ease_aes('linear')

animate(
  anim ,
  fps = 4,
  nframes = 2 * length(unique(individuals$t)),
  renderer = gifski_renderer(file.path("C:", paste0("animate_example.gif")))
)

