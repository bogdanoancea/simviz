filename_map      <- system.file("extdata", "map.wkt", package = "simviz")
filename_network  <- c(csv = system.file("extdata", "antennas.csv", package = "simviz"),
                       xsd = system.file("extdata", "antennas_dict.xsd", package = "simviz"),
                       BTS_coord_name_x = 'x',
                       BTS_coord_name_y = 'y')
filename_coverage <- c(csv = system.file("extdata", "AntennaCells_MNO1.csv", package = "simviz"),
                       xsd = system.file("extdata", "AntennaCells_MNO1_dict.xsd", package = "simviz"))
filename_grid     <- system.file("extdata", "grid.csv", package = "simviz")
filename_individ  <- c(csv = system.file("extdata", "persons.csv", package = "simviz"),
                       xsd = system.file("extdata", "persons_dict.xsd", package = "simviz"))   
filenames <- list(
   map = filename_map,
   network_parameters = filename_network,
   coverage_cells     = filename_coverage,
   grid               = filename_grid,
   individuals        = filename_individ)

simElem.list <- create_simElements(filenames, crs = 2062)
    

library(ggplot2)
library(viridis)
ggplot() +
  geom_sf(data = simElem.list$map) +
  geom_stars(data = simElem.list$grid, fill = NA) +
  geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.3) +
  geom_sf(data = simElem.list$network) +
  geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
  labs(fill = "power (dBm)") +
  scale_fill_viridis() +
  theme_bw()
