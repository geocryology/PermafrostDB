#' @title export borehole netcdf for ERDDAP
#' @param con database connection object provided by dbpf_con()
#' @param location character location name
#' @param file_name path to file (must not already exist)
#' @param sensors character vector of sensor labels to export
#' @param global_attrs global attributes to add to netcdf file
#' @importFrom ncdf4 nc_open ncdim_def nc_sync ncvar_add
#' @importFrom dplyr filter
#' @export
dbpf_export_nc_borehole_erddap <- function(con, location, file_name, sensors, global_attrs){
  # dbpf_export_nc_borehole_erddap(con, "NGO-DD-1010", "C:/tmp/1010.nc")
  if (file.exists(file_name)){
    file.remove(file_name)
  }
  
  if (missing(global_attrs)){
    global_attrs = list()
  }
  
  if (missing(sensors)){
    sensors <- c("ice_description", 
                 "ice_visual_perc", 
                 "geo_class_1", 
                 "geo_description",
              #   "soil_thermal_conductivity",
              #   "soil_electric_conductivity",
              #   "soil_heat_capacity_volumetric",
                 "soil_organic_thickness_m" ,
              #   "soil_moisture_volumetric",
                 "slope_angle",
                 "slope_aspect",
                 "soil_organic_thickness_m"
                 )
  }
  borehole_loc <- dbpf_locations(con, paste0("^",location,"$"))

  data <- dbpf_manual_obs_by_location(con, location, sensors)
  data[,c("from","to")] <- -data[,c("from","to")]  # Ensure depth positive
  surface_data <- data[data$from==0 & data$to == 0, ]
  data <- data[data$from!=0 | data$to != 0, ]
  
  ft <- intersect_profiles(data$from, data$to)
  
  #split into text and numeric
  txt <- data[data$type_of_measurement == 'text', -which(colnames(data) %in% c('numeric_value'))]
  colnames(txt)[colnames(txt) == "text_value"] <- "value"
  num <- data[data$type_of_measurement == 'numeric', -which(colnames(data)=='text_value')]
  colnames(num)[colnames(num) == "numeric_value"] <- "value"
  
  # Regrid into a separate dataframe for each data type
  txt_r <- by(txt, txt$label, function(d) regrid_profile(d, "from", "to", ft[,1], ft[,2]))
  num_r <- by(num, num$label, function(d) regrid_profile(d, "from", "to", ft[,1], ft[,2]))
  
  # create file
  make_profile_base(file_name, global_attrs)
  nc <- nc_open(file_name, write=TRUE)
  
  tryCatch({
    
    # Write basic data
    ncvar_put(nc, "latitude", borehole_loc$lat)
    ncvar_put(nc, "longitude", borehole_loc$lon)
    ncvar_put(nc, "profile", borehole_loc$name)
    ncvar_put(nc, nc$dim$depth_below_ground_surface, apply(ft, 1, mean), start=1, count=nrow(ft))
    nc_sync( nc )
    ncvar_put(nc, "top_of_interval", ft[,1])
    ncvar_put(nc, "bottom_of_interval", ft[,2])
    ncvar_put(nc, 'depth_bounds', ft)
    
    ncatt_put(nc, 0, "observation_depth_min", min(ft))
    ncatt_put(nc, 0, "observation_depth_max", max(ft))
    
    ### Numeric
    var_db_name <- "ice_visual_perc"
    if (var_db_name %in% names(num_r)) {
      varname <- "visible_ice"
      df <- num_r[[var_db_name]]
      new_nc_var <- ncvar_def(varname, df$unit_of_measurement[1], nc$dim$depth_below_ground_surface)
      nc <- ncvar_add(nc, new_nc_var)
      ncatt_put(nc, varname, "long_name", "Visible ice content")
      ncatt_put(nc, varname, "valid_min", 0)
      ncatt_put(nc, varname, "ioos_category", "Other")
      ncatt_put(nc, varname, "coordinates", "depth_below_ground_surface bottom_of_interval top_of_interval time latitude longitude platform_name profile")
      ncvar_put(nc, varname, df$value)
    }
    
    var_db_name <- "soil_moisture_volumetric"
    if (var_db_name %in% names(num_r)){
      varname <- "volumetric_moisture"
      df <- num_r[[var_db_name]]
      new_nc_var <- ncvar_def(varname, df$unit_of_measurement[1], nc$dim$depth_below_ground_surface)
      nc <- ncvar_add(nc, new_nc_var)
      ncatt_put(nc, varname, "long_name", "Volumetric moisture content")
      ncatt_put(nc, varname, "standard_name", "volume_fraction_of_condensed_water_in_soil")
      ncatt_put(nc, varname, "valid_min", 0)
      ncatt_put(nc, varname, "coordinates", "depth_below_ground_surface bottom_of_interval top_of_interval time latitude longitude platform_name profile")
      ncatt_put(nc, varname, "ioos_category", "Other")
      ncvar_put(nc, varname, df$value)
    }
    
    ### Text
    var_db_name <- "ice_description"
    if (var_db_name %in% names(txt_r)){
      varname <- "ice_class"
      df <- txt_r[[var_db_name]]
      new_nc_var <- ncvar_def(varname, "", nc$dim$depth_below_ground_surface)
      nc <- ncvar_add(nc, new_nc_var)
      ncatt_put(nc, varname, "coordinates", "depth_below_ground_surface bottom_of_interval top_of_interval time latitude longitude platform_name profile")
      ncatt_put(nc, varname, "ioos_category", "Other")
      ncatt_put(nc, varname, "long_name", "Ice classification")
      ncvar_put(nc, varname, df$value)
    }
    
    var_db_name <- "geo_description"
    if (var_db_name %in% names(txt_r)){
      varname <- "materials_description"
      df <- txt_r[[var_db_name]]
      new_nc_var <- ncvar_def(varname, "", nc$dim$depth_below_ground_surface)
      nc <- ncvar_add(nc, new_nc_var)
      ncatt_put(nc, varname, "coordinates", "depth_below_ground_surface bottom_of_interval top_of_interval time latitude longitude platform_name profile")
      ncatt_put(nc, varname, "ioos_category", "Other")
      ncatt_put(nc, varname, "long_name", "Description of materials in interval")
      ncvar_put(nc, varname, df$value)
    }
    
    var_db_name <- "geo_class_1"
    if (var_db_name %in% names(txt_r)){
      varname <- "materials"
      df <- txt_r[[var_db_name]]
      new_nc_var <- ncvar_def(varname, "", nc$dim$depth_below_ground_surface)
      nc <- ncvar_add(nc, new_nc_var)
      ncatt_put(nc, varname, "coordinates", "depth_below_ground_surface bottom_of_interval top_of_interval time latitude longitude platform_name profile")
      ncatt_put(nc, varname, "ioos_category", "Other")
      ncatt_put(nc, varname, "long_name", "Classification of materials in interval")
      ncvar_put(nc, varname, df$value)
    }

      ### Surface (metadata)
      var_db_name <- "soil_organic_thickness_m"
      if (var_db_name %in% surface_data$label){
        attname <- "organic_matter_thickness"
        ncatt_put(nc, 0, attname, (surface_data %>%
                    filter(surface_data$label==var_db_name))$numeric_data)
      }

      var_db_name <- "slope_angle"
      if (var_db_name %in% surface_data$label){
        attname <- "ground_slope_angle"
        ncatt_put(nc, 0, attname, (surface_data %>%
                    filter(surface_data$label==var_db_name))$numeric_data)
      }

      var_db_name <- "slope_aspect"
      if (var_db_name %in% surface_data$label){
        attname <- "ground_slope_direction"
        ncatt_put(nc, 0, attname, (surface_data %>%
                    filter(surface_data$label==var_db_name))$numeric_data)
      }

    
    
  }, finally={nc_close(nc)})
  
  
}

#' @title Create template netcdf file for profile borehole data
#' @importFrom ncdf4 ncdim_def ncvar_def nc_create ncatt_put nc_close
#' @param file path to file to create
#' @param global_attrs list of key-value pairs for global attributes
#'
make_profile_base <- function(file, global_attrs) {
  
  # Create dimensions
  dbgs <- ncdim_def('depth_below_ground_surface', units="m", unlim=TRUE, vals=numeric())
  nchar64 <- ncdim_def('nchar64', units="", create_dimvar = FALSE, vals=1:64)
  nbnd <- ncdim_def('nbnd', units="", create_dimvar = FALSE, vals=1:2)
  
  # Create coordinate variables
  Lat <- ncvar_def('latitude', "degrees_north", prec='double', dim=list())
  Lon <- ncvar_def('longitude', units='degrees_east', prec='double', dim=list())
  Profile <- ncvar_def('profile', units='', prec='char', dim=nchar64)
  Elev <- ncvar_def('surface_elevation', units='m',prec='double', dim=list())
  Time <- ncvar_def('time', units="hours since 1950-01-01T00:00:00Z", prec='double', dim=list())
  Name <- ncvar_def('platform_name', 'S1', dim=nchar64)
  #Z <- ncvar_def("depth_below_ground_surface", units="m", dim=dbgs)
  Zbnd <- ncvar_def( "depth_bounds", units='', dim=list(nbnd, dbgs), prec = "double")
  Ztop <- ncvar_def("top_of_interval", units="m", dim=dbgs, prec = "double")
  Zbot <- ncvar_def("bottom_of_interval", units="m", dim=dbgs, prec = "double")
  
  rootgrp = nc_create(file, vars=list(Lat,Lon,Profile,Elev,Time,Name,Zbnd,Ztop,Zbot))
  
  ncatt_put(rootgrp, Lat, 'standard_name', 'latitude')
  ncatt_put(rootgrp, Lat, 'standard_name_url', 'https://mmisw.org/ont/cf/parameter/latitude')
  ncatt_put(rootgrp, Lat, 'long_name', 'latitude')
  ncatt_put(rootgrp, Lat, 'axis', 'Y')
  ncatt_put(rootgrp, Lat, 'ioos_category', 'Location')
  
  ncatt_put(rootgrp, Lon, 'standard_name', 'longitude')
  ncatt_put(rootgrp, Lon, 'standard_name_url', 'https://mmisw.org/ont/cf/parameter/longitude')
  ncatt_put(rootgrp, Lon, 'long_name', 'longitude')
  ncatt_put(rootgrp, Lon, 'axis', 'X')
  ncatt_put(rootgrp, Lon, 'ioos_category', 'Location')
  
  ncatt_put(rootgrp, Profile, 'cf_role', 'profile_id')
  ncatt_put(rootgrp, Profile, '_Encoding', 'UTF-8')
  ncatt_put(rootgrp, Profile, 'long_name', 'Unique profile identifier')
  ncatt_put(rootgrp, Profile, 'ioos_category', 'Identifier')
  
  ncatt_put(rootgrp, Elev, 'long_name', 'Ground surface elevation')
  ncatt_put(rootgrp, Elev, 'standard_name', 'height_above_reference_ellipsoid')
  ncatt_put(rootgrp, Elev, 'ioos_category', 'Location')
  
  ncatt_put(rootgrp, Time, 'long_name', 'Date of measurement')
  ncatt_put(rootgrp, Time, 'standard_name_url', 'https://mmisw.org/ont/cf/parameter/time')
  ncatt_put(rootgrp, Time, 'standard_name', 'time')
  ncatt_put(rootgrp, Time, 'calendar', 'standard')
  ncatt_put(rootgrp, Time, 'axis', 'T')
  ncatt_put(rootgrp, Time, 'ioos_category', 'Time')
  
  ncatt_put(rootgrp, Name, 'standard_name', 'platform_name')
  ncatt_put(rootgrp, Name, 'long_name', 'Borehole or site name')
  ncatt_put(rootgrp, Name, 'ioos_category', 'Identifier')
  ncatt_put(rootgrp, Name, '_Encoding', 'UTF-8')
  
  # Create coordinate variables
  ncatt_put(rootgrp, 'depth_below_ground_surface', "standard_name", "depth")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "standard_name_url", "https://mmisw.org/ont/cf/parameter/depth")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "long_name", "Depth to midpoint of interval")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "axis", "Z")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "positive", "down")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "ioos_category", "Location")
  ncatt_put(rootgrp, 'depth_below_ground_surface', "bounds", "depth_bounds")
  
  ncatt_put(rootgrp, Zbnd, "long_name", "Depth below ground surface")
  ncatt_put(rootgrp, Zbnd, "ioos_category", "Location")
  
  ncatt_put(rootgrp, Ztop, "standard_name", "depth")
  ncatt_put(rootgrp, Ztop, "standard_name_url", "https://mmisw.org/ont/cf/parameter/depth")
  ncatt_put(rootgrp, Ztop, "long_name", "Depth to top of interval")
  ncatt_put(rootgrp, Ztop, "positive", "down")
  ncatt_put(rootgrp, Ztop, "ioos_category", "Location")
  
  ncatt_put(rootgrp, Zbot, "standard_name", "depth")
  ncatt_put(rootgrp, Zbot, "standard_name_url", "https://mmisw.org/ont/cf/parameter/depth")
  ncatt_put(rootgrp, Zbot, "long_name", "Depth to bottom of interval")
  ncatt_put(rootgrp, Zbot, "positive", "down")
  ncatt_put(rootgrp, Zbot, "ioos_category", "Location")
  
  # Set global attributes
  ncatt_put(rootgrp, 0, "featureType", "profile")
  ncatt_put(rootgrp, 0, "cdm_data_type", "profile")
  ncatt_put(rootgrp, 0, "cdm_altitude_proxy", "depth_below_ground_surface")
  ncatt_put(rootgrp, 0, "Conventions", "CF-1.6, ACDD-1.3")
  ncatt_put(rootgrp, 0, "cdm_profile_variables", "platform_name,time,latitude,longitude,surface_elevation,profile")
  ncatt_put(rootgrp, 0, "standard_name_vocabulary", "CF Standard Name Table v78")
  
  for (attr in names(global_attrs)){
    value = global_attrs[[attr]]
    ncatt_put(rootgrp, 0, attr, value)
  }
  
  # Close the NetCDF file
  nc_close(rootgrp)
  
  return(file)
  
}


#' @title From-To Union
#' @description Take the spatial union of multiple sets of depth intervals
#' @param from numeric vector of depths (positive down) corresponding to top of interval
#' @param to numeric vector of depths (positive down) corresponding to bottom of interval
#' @return numeric array with shape (n,2) of "Union"ed 
#'
intersect_profiles <- function(from, to){
  cutpoints <- sort(unique(c(from, to)))
  zero_length <- cbind(from, to)[from - to == 0,]
  zero_length <- unique(zero_length[order(zero_length[,1]), ])
  
  L <- length(cutpoints)
  nonzero_length <- cbind(cutpoints[1:L -1], cutpoints[2:L])
  
  intervals <- rbind(nonzero_length, zero_length)
  return(intervals[order(intervals[,2]),])
  
}


#' @title Regrid profile data
#' @param df dataframe with data and disjoint from-to intervals
#' @param from_col name of column with top of interval depth
#' @param to_col name of column with bottom of interval depth
#' @param new_from numeric vector of new top-of-interval depths
#' @param new_to numeric vector of new bottom-of-interval depths
regrid_profile <- function(df, from_col, to_col, new_from, new_to){
  
  sorted_input <- df[order(df[,to_col]),]
  fromto <- cbind(new_from, new_to)
  
  z <- apply(fromto, 1, function(x) regrid_interval(df=df, 
                                                    from_col=from_col,
                                                    to_col=to_col, 
                                                    fromto=x))
  result <- do.call('rbind', z)
  result$value[is.nan(result$value)] <- NA
  
  return(result)
}

#' @title Regrid dataframe 
#' @param df a 'tidy' data frame with disjoint from-to intervals
#' @param from_col see regrid_profile
#' @param to_col see regrid_profile
#' @param fromto length-2 depth interval over which to regrid data in df
regrid_interval <- function(df, from_col, to_col, fromto){
  
  if (length(unique(fromto)) == 1){
    output <- df[df[,from_col] == fromto[1] & df[,to_col] == fromto[1],]
  } else {
    output <- df[df[,from_col] <= fromto[1] & 
                   df[,to_col] >= fromto[2]  ,]
  }
  
  if (nrow(output) == 0){
    output[1,] <- NA
  }
  output[,from_col] <- fromto[1]
  output[,to_col] <- fromto[2]
  
  if (nrow(output) > 1){
    warning(paste0("\n(",df$label,") Multiple intersecting data records found for depth range [", fromto[1], ", ",fromto[2],"]", output[,c("from","to")]))
    output <- output[1,]
  }
  return(output)
}