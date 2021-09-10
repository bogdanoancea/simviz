#' Create list of parameters of the simulation.
#'
#' Create a named list of different elements for the simulation. It basically
#' contains the map (regions included), the network configuration (position of 
#' BTS, power, attenuation factor, etc.), the grid, the radio cells associated 
#' to each antenna, and the telecommunication signal measures (RSS, SDM, ...). 
#'
#' @param filenames Named list of filenames with each element of the simulation.
#'
#' @param id Character vector of unique ids to label geometries as in function
#' \code{rgeos::readWKT}. Length must match the number of subgeometries in the
#' WKT
#' 
#' @param crs integer or character; coordinate reference system for the geometry
#'  as in function \code{sf::st_as_sfc}
#' 
#' @param ... passed on to \code{sf::st_as_sf}, might include named arguments
#'  crs or precision
#'
#' @rdname create_simElements
#'
#' @name create_simElements
#' 
#' @import sf data.table stars
#' 
#' @importFrom readr read_delim
#' 
#' @importFrom tibble as_tibble 
#' 
#' @include readWKT_as_sf.R readWKT_as_sfc.R xsd_attrs2dt.R
#' 
#' @examples
#' filename_map      <- system.file("extdata/input_files", "map.wkt", package = "simviz")
#' filename_network  <- c(csv = system.file("extdata/output_files", "antennas.csv", package = "simviz"),
#'                        xml = system.file("extdata/metadata/output_files", "antennas_dict.xml", package = "simviz"))
#'                        
#' filename_coverage <- c(csv = system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simviz"),
#'                        xml = system.file("extdata/metadata/output_files", "AntennaCells_dict.xml", package = "simviz"))
#'                        
#' filename_grid     <- system.file("extdata/output_files", "grid.csv", package = "simviz")
#' 
#' filename_individ  <- c(csv = system.file("extdata/output_files", "persons.csv", package = "simviz"),
#'                        xml = system.file("extdata/metadata/output_files", "persons_dict.xml", package = "simviz"))   
#'                        
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ)
#'   
#' create_simElements(filenames, crs = 2062)
#'
#' @export
create_simElements <- function(filenames, id = NULL, crs = NA_integer_, ...){

  if (is.null(names(filenames))) {
    stop('[simviz::create_simElements] filenames must be a named list.\n')
  }
  
  if (!all(names(filenames) %in% c('map', 'network_parameters', 'coverage_cells', 'grid', 'individuals'))){
    stop("[simviz::create_simElements] The names of list filenames must be contained in c('map', 'network_parameters', 'coverage_cells', 'telco_measures', 'grid', 'individuals').\n")
  }
  
  
  # Read map file
  cat('[simviz::create_simElements] Reading and parsing wkt file for the map...')
  WKTtxt <- readLines(filenames$map)
  map_polygon <- readWKT_as_sf(WKTtxt, id = id)
  map_polygon <- st_set_crs(map_polygon, st_crs(crs))
  cat(' ok.\n')
  
  # Read network parameters
  cat('[simviz::create_simElements] Reading and parsing network parameters file...\n')
  netParam_names <- names(filenames$network_parameters)
  if (netParam_names[1] == 'csv') {
    network.dt <- fread(filenames$network_parameters['csv'], sep = ",", stringsAsFactors = FALSE)
    xmlParam.dt <- xml_attrs2dt(filenames$network_parameters['xml'], 'antennas')
    colnames_csv <- xmlParam.dt$name
    xmlParam.dt <- xmlParam.dt[name %in% names(network.dt)]
    classes_csv  <- xmlParam.dt$class
    names(classes_csv) <- xmlParam.dt$name
    classes_csv_num  <- classes_csv[which(classes_csv == 'numeric')]
    classes_csv_int  <- classes_csv[which(classes_csv == 'integer')]
    classes_csv_char <- classes_csv[which(classes_csv == 'character')]
    if ( length(classes_csv_num) > 0 ) {
      network.dt[, names(classes_csv_num) := lapply(.SD, as.numeric), .SDcols = names(classes_csv_num)]
    }
    if ( length(classes_csv_int) > 0 ) {
      network.dt[, names(classes_csv_int) := lapply(.SD, as.integer), .SDcols = names(classes_csv_int)]
    }
    if ( length(classes_csv_char) > 0 ) {
      network.dt[, names(classes_csv_char) := lapply(.SD, as.character), .SDcols = names(classes_csv_char)]
    }
    coords_name <- getCoordsNames(filenames$network_parameters['xml'], 'antennas')
    network.sf <- st_as_sf(network.dt, coords = coords_name, crs = crs)
  }
  cat(' ok.\n')
  
  # Read coverage file 
  cat('[simviz::create_simElements] Reading and parsing coverage cells file...')
  coverParam_names <- names(filenames$coverage_cells)
  if (coverParam_names[1] == 'csv') {
    coverage.dt <- fread(filenames$coverage_cells['csv'], sep = '\n', stringsAsFactors = FALSE)
    setnames(coverage.dt, 'V1')
    coverage_parsed.dt <- coverage.dt[, tstrsplit(V1, split = ',POLYGON')]
    xmlParam.dt <- xml_attrs2dt(filenames$coverage_cells['xml'], 'antennas')
    setnames(coverage_parsed.dt, xmlParam.dt$name)
    classes_csv  <- xmlParam.dt$class
    names(classes_csv) <- xmlParam.dt$name
    classes_csv_num  <- classes_csv[which(classes_csv == 'numeric')]
    classes_csv_int  <- classes_csv[which(classes_csv == 'integer')]
    classes_csv_char <- classes_csv[which(classes_csv == 'character')]
    if ( length(classes_csv_num) > 0 ) {
      coverage_parsed.dt[, names(classes_csv_num) := lapply(.SD, as.numeric), .SDcols = names(classes_csv_num)]
    }
    if ( length(classes_csv_int) > 0 ) {
      coverage_parsed.dt[, names(classes_csv_int) := lapply(.SD, as.integer), .SDcols = names(classes_csv_int)]
    }
    if ( length(classes_csv_char) > 0 ) {
      coverage_parsed.dt[, names(classes_csv_char) := lapply(.SD, as.character), .SDcols = names(classes_csv_char)]
    }
    coverage_parsed.dt[
      , (names(classes_csv)[2]) := paste0('POLYGON', get(names(classes_csv)[2]))]
    geometry.list <- lapply(coverage_parsed.dt[[names(classes_csv[2])]], readWKT_as_sfc, crs = crs)
    geometry.list <- lapply(geometry.list, st_intersection, map_polygon)
    geometry.sfc <- Reduce(c, geometry.list)
    coverage.sf <- st_sf(coverage_parsed.dt, geometry = geometry.sfc)

  }
  cat(' ok.\n')  
  
  # Read grid
  cat('[simviz::create_simElements] Reading grid file and creating stars object...')
  gridParam_names <- names(filenames$grid)
  if (gridParam_names[1] == 'csv') {
    grid.dt <- fread(filenames$grid['csv'], sep = ",", stringsAsFactors = FALSE)
    xmlParam.dt <- xml_attrs2dt(filenames$grid['xml'], 'grid')
    colnames_csv <- xmlParam.dt$name
    xmlParam.dt <- xmlParam.dt[name %in% names(grid.dt)]
    classes_csv  <- xmlParam.dt$class
    names(classes_csv) <- xmlParam.dt$name
    classes_csv_num  <- classes_csv[which(classes_csv == 'numeric')]
    classes_csv_int  <- classes_csv[which(classes_csv == 'integer')]
    classes_csv_char <- classes_csv[which(classes_csv == 'character')]
    if ( length(classes_csv_num) > 0 ) {
      grid.dt[, names(classes_csv_num) := lapply(.SD, as.numeric), .SDcols = names(classes_csv_num)]
    }
    if ( length(classes_csv_int) > 0 ) {
      grid.dt[, names(classes_csv_int) := lapply(.SD, as.integer), .SDcols = names(classes_csv_int)]
    }
    if ( length(classes_csv_char) > 0 ) {
      grid.dt[, names(classes_csv_char) := lapply(.SD, as.character), .SDcols = names(classes_csv_char)]
    }

    nx <- grid.dt[, getGridNoTilesX(filenames$grid['xml'], 'grid')]
    ny <- grid.dt[, getGridNoTilesY(filenames$grid['xml'], 'grid')]
    grid.stars <- st_as_stars(st_geometry(map_polygon), nx, ny )
    cat(' ok.\n')
    network.sf <- st_as_sf(network.dt, coords = coords_name, crs = crs)
  }
  
    
  # Read individuals
  individuals.dt <- fread(filenames$individuals[['csv']], sep = '\n', stringsAsFactors = FALSE)
  xmlParam.dt <- xml_attrs2dt(filenames$individuals['xml'], 'individuals')
  colnames_csv <- xmlParam.dt$name
  xmlParam.dt <- xmlParam.dt[name %in% names(individuals.dt)]
  classes_csv  <- xmlParam.dt$class
  names(classes_csv) <- xmlParam.dt$name
  classes_csv_num  <- classes_csv[which(classes_csv == 'numeric')]
  classes_csv_int  <- classes_csv[which(classes_csv == 'integer')]
  classes_csv_char <- classes_csv[which(classes_csv == 'character')]
  if ( length(classes_csv_num) > 0 ) {
    individuals.dt[, names(classes_csv_num) := lapply(.SD, as.numeric), .SDcols = names(classes_csv_num)]
  }
  if ( length(classes_csv_int) > 0 ) {
    individuals.dt[, names(classes_csv_int) := lapply(.SD, as.integer), .SDcols = names(classes_csv_int)]
  }
  if ( length(classes_csv_char) > 0 ) {
    individuals.dt[, names(classes_csv_char) := lapply(.SD, as.character), .SDcols = names(classes_csv_char)]
  }
  setnames(individuals.dt, 'V1')
  individuals_parsed.dt <- individuals.dt[, tstrsplit(V1, split = ',')]
  setnames(individuals_parsed.dt, colnames_csv)
  individuals.df <- as_tibble(individuals_parsed.dt)
  
  simElements <- list(
    map = map_polygon,
    network = network.sf,
    coverage = coverage.sf,
    grid = grid.stars,
    individuals = individuals.df
  )
  return(simElements)
  
  
}
