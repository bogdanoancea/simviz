library(ggplot2)
library(viridis)
library(stars)
library(gganimate)
library(simutils)


filename_map      <- system.file("extdata/input_files", "map.wkt", package = "simviz")
filename_network  <- c(csv = system.file("extdata/output_files", "antennas.csv", package = "simviz"),
                        xml = system.file("extdata/metadata/output_files", "antennas_dict.xml", package = "simviz"))
                        
filename_coverage <- c(csv = system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simviz"),
                        xml = system.file("extdata/metadata/output_files", "AntennaCells_dict.xml", package = "simviz"))
                        
filename_grid     <- c(csv = system.file("extdata/output_files", "grid.csv", package = "simviz"),
                       xml = system.file("extdata/metadata/output_files", "grid_dict.xml", package = "simviz"))
 
filename_individ  <- c(csv = system.file("extdata/output_files", "persons.csv", package = "simviz"),
                        xml = system.file("extdata/metadata/output_files", "persons_dict.xml", package = "simviz"))   
                        
filenames <- list(
   map                = filename_map,
   network_parameters = filename_network,
   coverage_cells     = filename_coverage,
   grid               = filename_grid,
   individuals        = filename_individ)
   

simElem.list <- simutils::create_simElements(filenames, crs = 2062)



ggplot() +
  geom_sf(data = simElem.list$map, size = 1.5) +
  geom_stars(data = simElem.list$grid, fill = NA) +
  geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.1) +
  geom_sf(data = simElem.list$network) +
  geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
  geom_point(data = simElem.list$individuals[t==0], mapping = aes(x, y, color = factor(nDev))) +
  labs(fill = "power (dBm)") + 
  labs(color = "Num. Devices", x = 'longitude', y = 'latitude') +
  scale_fill_viridis() +
  theme_bw()


anim <- ggplot() +
          geom_sf(data = simElem.list$map, size = 1.5) +
          geom_stars(data = simElem.list$grid, fill = NA) +
          geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.1) +
          geom_sf(data = simElem.list$network) +
          geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
          geom_point(data = simElem.list$individuals, mapping = aes(x, y, color = factor(nDev))) +
          transition_states(states = t) +
          labs(fill = "power (dBm)", color = "Num. Devices", x = 'longitude', y = 'latitude') +
          scale_fill_viridis() +
          theme_bw()

times <- unique(simElem.list$individuals[, t])

# gganimate::animate(
#   anim ,
#   fps = 4,
#   nframes = 2 * length(times),
#   renderer = gifski_renderer(file.path("C:", paste0("animate_example.gif")))
# )

# map1 <- MNDmap(simElem.list$map)
# map2 <- MNDmap_grid(map1, simElem.list$grid)

