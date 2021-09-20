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
   

simElem.list <- create_simElements(filenames, crs = 2062)

library(ggplot2)
library(viridis)
library(stars)
library(gganimate)

# indiv.dt <- as.data.table(simElem.list$individuals)
# indiv.dt[, t := as.numeric(t)]
# indiv.dt[, x := as.numeric(x)]
# indiv.dt[, y := as.numeric(y)]
# indiv.dt[, nDev := fcase(
#   is.na(`Device 1`) & is.na(`Device 2`), 0L,
#   !is.na(`Device 1`) & is.na(`Device 2`), 1L,
#   is.na(`Device 1`) & !is.na(`Device 2`), 1L,
#   !is.na(`Device 1`) & !is.na(`Device 2`), 2L)]

ggplot() +
  geom_sf(data = simElem.list$map, size = 1.5) +
  geom_stars(data = simElem.list$grid, fill = NA) +
  geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.1) +
  geom_sf(data = simElem.list$network) +
  geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
  geom_point(data = indiv.dt[t==0], mapping = aes(x, y, color = factor(nDev))) +
  labs(fill = "power (dBm)", color = "Num. Devices", x = 'longitude', y = 'latitude') +
  scale_fill_viridis() +
  theme_bw()

# install.packages("gganimate")
# install.packages("transformr")

anim <- ggplot() +
          geom_sf(data = simElem.list$map, size = 1.5) +
          geom_stars(data = simElem.list$grid, fill = NA) +
          geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.1) +
          geom_sf(data = simElem.list$network) +
          geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
          geom_point(data = indiv.dt, mapping = aes(x, y, color = factor(nDev))) +
          transition_states(states = t) +
          labs(fill = "power (dBm)", color = "Num. Devices", x = 'longitude', y = 'latitude') +
          scale_fill_viridis() +
          theme_bw()

times <- unique(indiv.dt[, t])

gganimate::animate(
  anim ,
  fps = 4,
  nframes = 2 * length(times),
  renderer = gifski_renderer(file.path("C:", paste0("animate_example.gif")))
)

map1 <- MNDmap(simElem.list$map)
map2 <- MNDmap_grid(map1, simElem.list$grid)

