#' Create a data.table from a file metadata xsd.
#'
#' @param xsdNode xml node of the input xsd file
#'
#' @param dataset character vector of length 1 with the dataset name
#'
#' @rdname xsd_attrs2dt
#'
#' @name xsd_attrs2dt
#' 
#' @import sf xml2 data.table
#' 
#' @include readWKT_as_sf.R
#' 
#' @examples
#' xsdname <- system.file("extdata", "antennas_dict.xsd", package = "simviz") 
#' xsd_attrs2dt(xsdname, 'antennas')
#' 
#' xsdname <- system.file("extdata", "persons_dict.xsd", package = "simviz") 
#' xsd_attrs2dt(xsdname, 'individuals')
#' 
xsd_node_attrs2dt <- function(xsdNode, dataset){
  
  if (dataset == 'antennas') {
    
    lngth <- xml_length(xsdNode)
    if (lngth == 0) {
      
      dt <- as.data.table(as.list(xml_attrs(xsdNode)))
      return(dt[])
      
    }
    if (lngth == 1) {
      
      element_name <- xml_attr(xsdNode, attr = 'name')
      child_node <- xml_child(xsdNode)
      child_name <- xml_name(child_node)
      if (child_name == 'simpleType') {
        
        if (xml_name(xml_child(child_node)) == 'restriction') {
          
         type <- xml_attrs(xml_child(child_node), 'base')
         dt <- data.table(name = element_name, type = type)
         return(dt[])
           
        }
        
        stop('[xml_attrs2dt] xsd simpleType element not coded yet.')
      }
      
      stop('[xml_attrs2dt] xsd structure not coded yet.')
      
      
    }
    if (lngth >= 2) { xsd_node_attrs2dt(xsdNode[[1]], dataset = dataset) }
  }
  
  
  if (dataset == 'individuals') {
    
    dt <- as.data.table(as.list(xml_attrs(xsdNode)))
    return(dt[])
    
  }
  
  stop('[xsd_node_attrs2dt] dataset not yet implemented.\n')
}


xsd_attrs2dt <- function(xsdname, dataset){
  
  if (dataset == 'antennas') {
    
    xml <- read_xml(xsdname)
    elements.xml <- xml_children(xml_find_all(xml, './/xs:all'))
    dt <- rbindlist(lapply(elements.xml, xsd_node_attrs2dt, dataset = dataset), fill = TRUE)
    types <- c('integer', 'numeric', 'character')
    names(types) <- c('xs:integer', 'xs:decimal', 'xs:string')
    dt <- dt[
      , class := types[type] ][
      , .(name, class)]
    return(dt[])
    
  }
  
  if (dataset == 'individuals') {
    
    xml <- read_xml(xsdname)
    elements.xml <- xml_children(xml_find_all(xml, './/xs:all'))
    dt <- rbindlist(lapply(elements.xml, function(vec){as.data.table(as.list(xml_attrs(vec)))}))
  return(dt)
    types <- c('integer', 'numeric', 'character')
    names(types) <- c('xs:integer', 'xs:decimal', 'xs:string')
    dt <- dt[
      , class := types[type] ][
        , .(name, class)]
    return(dt[])
    
  }
  
  stop('[xsd_attrs2dr] dataset not yet implemented.\n')
}