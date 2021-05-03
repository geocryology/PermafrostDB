# =============================================================================
#'
#' @title Write a kml plotting site locations 
#'
#' @description Exports site locations as a kml file. 
#'
#' @details Function needs to be extended to allow:
#'          --> filtering by location, etc. 
#'          --> display of summary information per site
#'          --> assigning shape and colour of icon based on attributes
#'
#' @param kmlfile Filename for kml to be produced.
#' 
#' @param location_name (optional) Character, one or more location names to 
#' convert. 
#' 
#' @param bounding_box (optional) numeric vector of the form
#'  (xmin, xmax, ymin, ymax) specifying longitude (x) and latitude (y) values
#'  to filter sites
#' 
#' @return No value is returned. 
#' 
#' @export
#' @examples
#' con <- dbpf_con()
#' dbpf_kml(con, kmlfile = "~/Desktop/permafrost.kml")
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_kml <- function(con, kmlfile, location_name, bounding_box) {
	require(sp)
	require(rgdal)
	require(plotKML)
  
	if (missing(con)){
	  con <- dbpf_con()
	}
  
	#get data and filter (if filtering parameters provided)
	loc <- dbpf_locations(con)
	
	if (!missing(location_name)){ # filter by location name
	  loc <- loc[loc$name %in% location_name,]
	}
	
	if (!missing(bounding_box)){ # filter by coordinates
	  loc <- loc[loc$lon > bounding_box[1] &
	               loc$lon < bounding_box[2] &
	               loc$lat > bounding_box[3] &
	               loc$lat < bounding_box[4],]
	}
	
	# make data to include
	df <- loc[,-which(names(loc) %in% c('id', 'record_observations'))]
	
	#make spatial points
	sp <- SpatialPointsDataFrame(subset(loc, select=c(lon,lat)), 
	                             subset(loc, select=c(-lon,-lat)))
	proj4string(sp) <- CRS("+init=epsg:4326")
	
	#write kml
	#TODO: give table: https://github.com/cran/plotKML/blob/master/R/layer.SpatialPoints.R
	# also see http://gsif.isric.org/doku.php?id=wiki:tutorial_plotkml 
	kml_open(kmlfile)   
	kml_layer.SpatialPoints(sp, 
	                        points_names = loc$name,
	                        balloon=T,  # show metadata in balloon
	                        #size=0.65,
	                        #shape="http://maps.google.com/mapfiles/kml/pal2/icon18.png",
	                        colour='black')
	kml_close(kmlfile)                        
	
	# feedback
	print('===> KML does not contain plots (ploygon locations), feature needs to be added to code.')  
}