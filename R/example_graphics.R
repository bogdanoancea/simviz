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
ggplot() +
  geom_sf(data = simElem.list$map) +
  geom_stars(data = simElem.list$grid, fill = NA) +
  geom_sf(data = simElem.list$coverage, aes(fill = simElem.list$network$power), alpha = 0.3) +
  geom_sf(data = simElem.list$network) +
  geom_sf_label(data = simElem.list$network, mapping = aes(label = `Antenna ID`), nudge_x = 1, nudge_y = 1) +
  labs(fill = "power (dBm)") +
  scale_fill_viridis() +
  theme_bw()
