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
simPlot_territory(simData, 
                  aggre_level = 2, 
                  aggre_name = "Subregions", 
                  plot_title = 'Territorial map for simulation')

# Regions
simPlot_territory(simData, 
                  aggre_level = 1, 
                  aggre_name = "Regions", 
                  plot_title = 'Territorial map for simulation')



# network on map
simPlot_network(simData, 
                map.plot = TRUE,
                aggre_level = 1, 
                aggre_name = "Regions", 
                coverage.plot = FALSE,
                size_var = "power", size_name = "Power",
                grid.plot = FALSE,
                plot_title = 'Antenna positions')


# network on map with coverage cells
simPlot_network(simData, 
                map.plot = TRUE,
                aggre_level = 2, 
                aggre_name = "Subregions", 
                coverage.plot = TRUE,
                size_var = "power", size_name = "Power",
                grid.plot = FALSE,
                plot_title = 'Antenna coverage cells')


# network on map with coverage cells (attenuation factor)
simPlot_network(simData, 
                map.plot = TRUE,
                aggre_level = 2, 
                aggre_name = "Subregions", 
                plot_title = 'Antenna coverage cells',
                coverage.plot = TRUE,
                size_var = "attenuationfactor", size_name = "Attenuation Factor",
                grid.plot = FALSE)


# network with RSS grid
simPlot_network(simData, 
                map.plot = FALSE,
                coverage.plot = TRUE,
                size = 1,
                grid.plot = TRUE,
                plot_title = 'RSS per Radio Cell')



# individuals on map (initial time)
simPlot_individuals(simData, 
                time = 0,
                size = 1,
                plot_title = 'Individuals at t=0')



# individuals on map (animation)
time <- unique(simData$individuals$t)
time_subset <- sort(time[sample(x = c(1:600), 80)])
indiv_subset <- c('661', '378')

anim <- simAnimate_individuals(simData, 
                    time = time_subset,
                    individuals_subset = indiv_subset,
                    size = 1,
                    plot_title = 'Individuals at time = {closest_state}')



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




