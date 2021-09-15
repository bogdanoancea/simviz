#' Create a data.table from a file metadata xml.
#'
#' @param xmlNode xml node of the input xml file
#'
#' @param dataset character vector of length 1 with the dataset name
#'
#' @rdname xml_attrs2dt
#'
#' @name xml_attrs2dt
#' 
#' @import sf xml2 data.table
#' 
#' @include readWKT_as_sf.R
#' 
#' @examples
#' xmlname <- system.file("extdata/metadata/output_files", "antennas_dict.xml", package = "simviz") 
#' xml_attrs2dt(xmlname, 'antennas')
#' 
#' xmlname <- system.file("extdata/metadata/input_files", "persons_dict.xml", package = "simviz") 
#' xml_attrs2dt(xsdname, 'individuals')
#' 
xmlTypes2RTypes <- function(xmlType) {
  rType <- ''
  if(xmlType == 'integer') {
    rType <- 'integer'
  } else if (xmlType == 'string') {
    rType <- 'character'
  } else if (xmlType == 'decimal') {
    rType <- 'numeric'
  } else if (xmlType == 'unsignedInt') {
    rType <- 'integer'
  }
  return (rType)
}

xml_attrs2dt <- function(xmlname, dataset){
  
  if (dataset == 'antennas' || dataset == 'antenna_cells' || dataset == 'grid' || dataset == 'individuals') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)[[1]]
    #get column names and column types
    antennas_colNames <- c()
    antennas_typeNames <- c()
    for(i in 1: length(ant)) {
      k <- 0
      l <- 0
      for(j in 1:length(ant[[i]])) {
        nodeName <- names(ant[[i]])[j]
        if(endsWith(nodeName, 'ColName')) {
          colName <- (ant[[i]])[[j]][[1]]
          antennas_colNames <- c(antennas_colNames, colName)
          k <- k + 1
        }
        if(endsWith(nodeName, 'value_type')) {
          typeName <- (ant[[i]])[[j]][[1]]
          antennas_typeNames <-c(antennas_typeNames, typeName)
          l <- l + 1
        }
      }
      if(k > l) {
        num <- k - l
        lastType <- antennas_typeNames[length(antennas_typeNames)]
        for(e in 1:num) {
          antennas_typeNames <-c(antennas_typeNames, typeName)
        }
      }
    }
    antennas_typeNames <- sapply(antennas_typeNames, xmlTypes2RTypes, USE.NAMES = FALSE)
    
    dt <- as.data.table(cbind(name = antennas_colNames, class = antennas_typeNames))
    return(dt[])
    
  }

  stop('[xml_attrs2dt] dataset not yet implemented.\n')
}

getCoordsNames <- function(xmlname, dataset) {
  if (dataset == 'antennas') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$antennas
    #get column names and column types
    antennas_coords_colNames <- c()

    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_antenna_coords') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'ColName')) {
            coordsColName <- (ant[[i]])[[j]][[1]]
            antennas_coords_colNames <- c(antennas_coords_colNames, coordsColName)
          }
        }
      }
    }
    return(antennas_coords_colNames)
  }
  
  if (dataset == 'individuals') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$persons
    #get column names and column types
    persons_coords_colNames <- c()
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_person_coords') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'ColName')) {
            coordsColName <- (ant[[i]])[[j]][[1]]
            persons_coords_colNames <- c(persons_coords_colNames, coordsColName)
          }
        }
      }
    }
    return(persons_coords_colNames)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}

getGridNoTilesX <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$grid
    #get column names and column types
    XColName <- ''
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_grid_tile_no') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'XColName')) {
            XColName <- (ant[[i]])[[j]][[1]]
          }
        }
      }
    }
    return(XColName)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}


getGridNoTilesY <- function(xmlname, dataset) {
  if (dataset == 'grid') {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)$grid
    #get column names and column types
    YColName <- ''
    
    for(i in 1: length(ant)) {
      if(names(ant)[i] == 'specs_grid_tile_no') {
        for(j in 1:length(ant[[i]])) {
          nodeName <- names(ant[[i]])[j]
          if(endsWith(nodeName, 'YColName')) {
            YColName <- (ant[[i]])[[j]][[1]]
          }
        }
      }
    }
    return(YColName)
  }
  stop('[getCoords] dataset not yet implemented.\n')
}
