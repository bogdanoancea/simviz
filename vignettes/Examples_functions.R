library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stars)
library(gganimate)
library(stringr)
library(av) #av_renderer

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
simplot_territory(simData, 
                  aggre_level = 2, 
                  aggre_name = "Subregions", 
                  plot_title = 'Territorial map for simulation')

# Regions
simplot_territory(simData, 
                  aggre_level = 1, 
                  aggre_name = "Regions", 
                  plot_title = 'Territorial map for simulation')



# network on map
simplot_network(simData, 
                map.plot = TRUE,
                aggre_level = 1, 
                aggre_name = "Regions", 
                coverage.plot = FALSE,
                size_var = "power", size_name = "Power",
                grid.plot = FALSE,
                plot_title = 'Antenna positions')


# network on map with coverage cells
simplot_network(simData, 
                map.plot = TRUE,
                aggre_level = 2, 
                aggre_name = "Subregions", 
                coverage.plot = TRUE,
                size_var = "power", size_name = "Power",
                grid.plot = FALSE,
                plot_title = 'Antenna coverage cells')


# network on map with coverage cells (attenuation factor)
simplot_network(simData, 
                map.plot = TRUE,
                aggre_level = 2, 
                aggre_name = "Subregions", 
                plot_title = 'Antenna coverage cells',
                coverage.plot = TRUE,
                size_var = "attenuationfactor", size_name = "Attenuation Factor",
                grid.plot = FALSE)


# network with RSS grid
simplot_network(simData, 
                map.plot = FALSE,
                coverage.plot = TRUE,
                size = 1,
                grid.plot = TRUE,
                plot_title = 'RSS per Radio Cell')



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
time <- unique(simData$individuals$t)
time_subset <- sort(time[sample(x = c(1:600), 80)])
map <- simData$map
network <- simData$network
network$t <- NULL
individuals <- simData$individuals
individuals <- individuals[
  individuals$t %in% time_subset,][
  #individuals$`Person ID` %in% c('778', '771'),]
    individuals$`Person ID` %in% c('661', '378'),]
individuals_subset <- individuals[!is.na(individuals$t),]

anim <- ggplot() +
  geom_sf(data = map, fill = NA) +
  geom_sf(data = network, size = 1) +
  geom_sf(data = individuals_subset, aes(color = factor(nDev)), size = 3) +
  coord_sf() +
  theme_bw() +
  labs(title = 'Individuals at time = {closest_state}', x = '', y = '', color = 'No. Devices') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        axis.text.y = element_text(vjust = 0.5, hjust=1, size = 7)) +
  transition_states(states = t) +
  ease_aes('linear')

### SAVE ANIMATION AS GIF
animate(
  anim,
  fps = 4,
  nframes = 2 * length(unique(individuals_subset$t)),
  renderer = gifski_renderer(file.path("C:", paste0("animate_example.gif")))
)

### SAVE ANIMATION AS MPEG AND MP4 (compatible with PowerPoint)
# ffmpeg installed is needed:
# https://www.gyan.dev/ffmpeg/builds/
b <- animate(anim, renderer = ffmpeg_renderer(), width = 800, height = 450)
anim_save("C:/animate_example.mpeg", b)
av_video_convert(b, output = "C:/animate_example.mp4", verbose = TRUE)
# in command window: ffmpeg -i C:/animate_example.mpeg -c copy C:/animate_example.mp4


animate(
  anim,
  fps = 4,
  nframes = 2 * length(unique(individuals_subset$t)),
  renderer = av_renderer(file.path("C:", paste0("animate_example.mp4")))
)



### network parameters plot sin t, mno_ID...
network <- simData$network
network$t <- NULL
network$`Antenna ID` <- NULL
network$`MNO ID` <- NULL
network$mno_name <- NULL
plot(network)


