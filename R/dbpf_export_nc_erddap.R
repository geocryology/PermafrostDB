# =============================================================================
#'
#' @title Download ground temperature time series to ERDDAP-optimized netcdf file
#'
#' @description Downloads one or more time series from the carleton permafrost
#' database and export as a netCDF file. The netCDF file is designed to
#' accommodate many stations with the same number of measurement depths (same
#' number of thermistors), but with different depth values.  It is possible
#' to group stations with different numbers of measurement depths but this is
#' at the cost of wasted space: any station with fewer measurement depths will
#' have missing values for any depth levels that it lacks.
#'
#' @details Requires that the PermafrostDB, reshape2 and ncdf4 packages be
#' installed. The file structure is based on H.5.1. "Multidimensional array
#' representations of time series profiles" with  Example H.16 used as a
#' template and a one-dimensional time coordinate variable.
#'
#' This structure is designed to accommodate stations with the same number of
#' measurement depths (levels) but with different depth values for each level.
#' For example one thermistor may measure temperature at 1.0, 2.0 and 3.0 m
#' and another may measure temperature at 0.5, 1.5 and 5.0 m. Both have three
#' measurement depths
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character, one location name corresponding to a location with temperature data.
#'
#' @param file_name character, path to output netCDF file (*.nc)
#'
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is to
#' be aggregated from the database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ncdf4)
#' library(PermafrostDB)
#' con <- dbpf_con()
#' dbpf_export_nc_chain_multi(con = con, location_name = c('NGO-DD-1005', 'NGO-DD-1006'),
#' file_name = "./thermistor_multi.nc")
#' dbDisconnect(con)
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_export_nc_erddap <- function(con, location_name, file_name, freq='daily'){

  if (!require(reshape2)){
    return("You must install package ncdf4")
  }
  if (!require(ncdf4)){
    return("You must install package ncdf4")
  }

  period <- switch(tolower(freq), 'daily'=24, 'hourly'=1)

  ## Get data from DB
  db_dat <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = period)
  loc <- dbpf_locations(con)
  # reshape and get values
  db_dat$height = abs(db_dat$height)
  db_dat <- db_dat[, c("loc_name", "height", "agg_avg", "time")]

  for (site in location_name){
    site_dat <- db_dat[db_dat$loc_name == site, ]

    if (length(file_name) == length(location_name)){
      file_name_i <- file_name[which(location_name == site)]
    } else {
      stop("location_name and file_name must have same length")
    }

    # get depth indices
    depths <- by(site_dat$height, site_dat$loc_name, unique, simplify=T)
    indx <- sapply(depths, length) # how many z levels for each station
    depths <- as.data.frame(do.call(rbind,lapply(depths, `length<-`, max(indx))))
    vals_depths <- as.matrix(t(depths))

    #convert depth value to level number
    Z <- by(site_dat$height, site_dat$loc_name, function(x) as.numeric(as.factor(x)))
    site_dat$height <- as.numeric(unlist(Z))

    m <- acast(site_dat,
               formula = height ~ time ~ loc_name,
               value.var = 'agg_avg',
               fun.aggregate = function(x) x[1])

    refdate <- as.POSIXct("1970-01-01 00:00:00", fmt="%Y-%m-%D %T", tz='UTC')

    vals_time <- as.POSIXct(dimnames(m)[[2]], fmt="%Y-%m-%D %T", tz='UTC')
    if (tolower(freq) == 'daily'){
      vals_time <- vals_time - refdate
      time_units <- "days since 1970-01-01 00:00:00"
    }else if(tolower(freq) == 'hourly'){
      vals_time <- as.numeric(vals_time) - as.numeric(refdate)
      time_units <- "seconds since 1970-01-01 00:00:00"
    }

    vals_tmp <- m
    vals_name <- dimnames(m)[[3]]

    n_depths <- dim(m)[1]
    n_ts <- dim(m)[2]
    n_stations <- dim(m)[3]

    ## Get coordinate data

    coords <- loc[match(site, loc$name),
                  c('name','lon', 'lat', 'elevation_in_metres')]

    ## Create .nc file
    nc <- createTimeSeriesProfileNCF(file = file_name_i,
                                     n_timestep = n_ts,
                                     n_stations = n_stations,
                                     n_levels = n_depths,
                                     close_file=F,
                                     time_units=time_units )

    ## Populate data
    ncvar_put(nc, varid='depth', vals=vals_depths)
    ncvar_put(nc, varid='time', vals = vals_time)
    ncvar_put(nc, varid="ground_temperature", vals = vals_tmp) #unique(site_dat$agg_avg))
    ncvar_put(nc, varid="platform_id", vals = vals_name)
    ncvar_put(nc, varid='latitude', vals = coords$lat)
    ncvar_put(nc, varid='longitude', vals = coords$lon)
    ncvar_put(nc, varid='elevation', vals = coords$elevation_in_metres)

    ncdf4::ncatt_put(nc, 0, "geospatial_bounds", paste0("POINT(", coords$lon, coords$lat, ")"))
    ncdf4::ncatt_put(nc, 0, "geospatial_bounds_crs", "EPSG:4326")
    ncdf4::ncatt_put(nc, 0, "platform_id", location_name)
    ncdf4::ncatt_put(nc, 0, "platform_name", location_name)
    ncdf4::ncatt_put(nc, "ground_temperature", "cell_methods", "time:mean")
    ncdf4::ncatt_put(nc, "depth", "positive", "down")
    ncdf4::ncatt_put(nc, "ground_temperature", "long_name", paste0(freq, " average of ground temperature"))

    ## Close file
    nc_close(nc)
  }

}

# =============================================================================
#'
#' @title Create empty netCDF file for (temperature) time series profile
#'
#' @description Creates a netCDF file with specified dimensions. The file has no
#' data within it, but must be added in afterwards. Thermistors should may have
#' different measurement depths but should have the same number of measurement
#' depths (levels)
#'
#' @details The file structure is based on H.5.1. "Multidimensional array of
#' time series profiles representation of time series"
#' (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#_multidimensional_array_representations_of_time_series_profiles)
#' * Depth values are not time-dependent, therefore the depth levels of a
#' station must not change between time periods
#'
#' @param file character, path to output netCDF file (*.nc)
#'
#' @param n_timestep integer, the number of unique timesteps for which there is
#'  temperature data
#'
#' @param n_depths integer, the largest number of depth measurements in any
#' profile
#'
#' @param n_stations  integer, how many sites are to be added to the file
#'
#' @param close_file logical, whether or not to close the connection to the file
#' after creation. Leaving the file open allows for the immediate addition of
#' data. Defaults to FALSE.
#'
#' @examples
#'  \dontrun{
#' library(ncdf4)
#' # create temperature data
#' t1 <- 3*sin(seq(1:792)*2*pi/365)+2
#' t2 <- sin(seq(1:792)*2*pi/365)
#' m <- matrix(c(t1,t2), nrow=2, byrow=TRUE)
#'
#' #create ncdf file
#' ncnew <- createMultiThermistorNCF("./thermistor_multi.nc", 2, 792, 1,  FALSE)
#'
#' # put some data in the ncdf file
#' ncvar_put(ncnew, 'soil_temperature', m)
#' ncvar_put(ncnew, 'platform_id', c('station1', 'station2'))
#'
#' #close the file
#' nc_close(ncnew)
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
createTimeSeriesProfileNCF <- function(file, n_stations, n_timestep, n_levels,
                                     close_file=F,
                                     time_units="days since 1970-01-01 00:00:00"
                                     ){
  missval = -999

  ## Create Dimensions
  dummyDimTime <- c(1:n_timestep)
  dimTime <- ncdim_def("time",
                       units=time_units, unlim = F, vals = dummyDimTime,
                       create_dimvar = T)

  dummyDimStation <- c(1:n_stations)
  dimStation <- ncdim_def("station",
                          unlim = F, vals = dummyDimStation, units = '',
                          create_dimvar = F)

  dummyDepth <- c(1:n_levels)
  dimDepth <- ncdim_def('depth',
                    unlim = F, vals = dummyDepth, units = '',
                    create_dimvar = F)

  dimNameStrlen <- ncdim_def("name_strlen",
                             unlim = F, vals = 1:25, units = '',
                             create_dimvar = F)

  ## Create Variables

  varLon <- ncvar_def(name = 'longitude', prec='float',
                      units = "degrees_E", missval = missval,
                      dim = list(dimStation),
                      longname = "Longitude")

  varLat <- ncvar_def(name = 'latitude', prec='float',
                      units = "degrees_N", missval = missval,
                      dim = list(dimStation),
                      longname = "Latitude")

  varElev <- ncvar_def(name= 'elevation', prec='float',
                       units = 'm', missval = missval,
                       dim=list(dimStation),
                       longname = "Station surface elevation")

  varStnName <- ncvar_def(name = 'platform_id', prec='char',
                          units = '',
                          dim = list(dimNameStrlen, dimStation), # strlen must be first (opposite of NCDF convention)
                          longname = "Borehole or monitoring station name")

  varDepth <- ncvar_def(name = 'depth', prec='float',
                        units = "m", missval = missval,
                        dim = list(dimDepth),  ## could possible remove time as var
                        longname = "Depth relative to ground surface")

  varTemp <- ncvar_def(name = 'ground_temperature', prec='float',
                       units = "degrees_C", missval = missval,
                       dim = list(dimDepth, dimTime, dimStation),
                       longname = "Ground temperature")


  varCRS <- ncvar_def(name = 'crs', prec='integer',
                      units = '', longname = "coordinate system",
                      dim = list())

  ## Create File
  ncnew <- nc_create(file, vars = list(varLon, varLat, varElev, varStnName,
                                       varDepth, varTemp, varCRS))

  ## Create Attributes from templates in '/PermafrostDB/extdata' or
  ##                                    '/PermafrostDB/inst/extdata' (unbuilt)
  f = 'extdata'
  p = 'PermafrostDB'
  nc_attributes_from_template(ncnew, system.file(f, 'depth.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'crs.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'latitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'elevation.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'longitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'platform_id.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'ground_temperature.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'time.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, '_global_CU_database.csv', package=p))

  ## Close File or Return
  if (close_file){
    nc_close(ncnew)
    return(file)
  }else{
    return(ncnew)
  }
}

