# =============================================================================
#'
#' @title Download database time series in CSV ASCII format
#'
#' @description Downloads one or more time series from the carleton permafrost database
#' and exports them as a CSV.  The formatting of the output file is fully
#'  customizable.
#'
#' @details Requires the PermafrostDB package.
#'
#' @param con Database connection object, as returned by dbpf_con
#'
#' @param location_name Character, one or more location names to convert.
#'
#' @param output_directory (optional) character, path to output directory.
#' If missing, the
#' function will return the table as an R object instead of writing it to disk.
#'
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is to
#' be aggregated from the database.
#'
#' @param unit_of_measurement character, used to identify the desired measurement
#' type in the database.  Common values are 'C' for temperature or '\%RH'
#' for relative humidity.  Defaults to 'C'
#'
#' @param measurement_name character, description of what is being measured.
#' Used in file naming.  Examples incldue 'Ground_Temperature' or
#' 'Relative_Humidity'. Defaults to 'Ground_Temperature'
#'
#' @param date_header character, string to be used for the header of the date/time
#' column.  Defaults to 'Date'.
#'
#' @param date_format character, date format string for database. Defaults to
#' '\%Y/\%m/\%d \%H:\%M:\%S'.
#'
#' @param depth_header_units Character string, one of ('m', 'cm', 'mm'). Desired
#' units for depth values in column header. Defaults to 'm'.
#'
#' @param depth_header_prefix (optional) character string to be placed before
#' numeric depth values in column headers.
#'
#' @param depth_header_suffix (optional) character string to be placed after
#' numeric depth values in column headers.
#'
#' @param depth_header_precision (optional) integer, number of decimal places to
#' use for depth header values. If not provided, depth values are allowed to have
#' as many decimal places as required.
#'
#' @param depth_positive_down logical, whether or not depths should be given as
#' positive values. Defaults to TRUE.
#'
#' @param measurement_precision integer, number of decimal places to include for
#' measurement values. Defaults to 2.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_export_csv_generic(con = con, location_name = c("NGO-DD-1012", "NGO-DD-2004"),
#'                  output_directory = './', freq='daily')
#' dbpf_export_csv_generic(con = con, location_name = c("NGO-DD-1011"),
#'                  output_directory = './', freq='hourly')
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_export_csv_generic <- function(con, location_name, output_directory,
                                    freq='daily',
                                    unit_of_measurement='C',
                                    measurement_name='Ground_Temperature',
                                    date_header='Date',
                                    date_format= '%Y/%m/%d %H:%M',
                                    depth_header_units='m',
                                    depth_header_prefix=NULL,
                                    depth_header_suffix=NULL,
                                    depth_header_precision=2,
                                    depth_positive_down=T,
                                    measurement_precision=2){

  ## Download data from dbpf
  period <- switch(tolower(freq), 'daily'=24, 'hourly'=1)
  db_dat <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = period,
                                  unit_of_measurement = unit_of_measurement)

  if(nrow(db_dat)==0){ # nodata
    warning(sprintf("%s returned no data", location_name))
    return(NULL)
  }

  unit_conv <- switch(tolower(depth_header_units), 'm'=1, 'cm'=100, 'mm'=1e3)

  # Adjust depth/height format as appropriate
  db_dat$height <- -db_dat$height * unit_conv

  # round to 2 decimal places
  db_dat$agg_avg <- round(db_dat$agg_avg, measurement_precision)

  for (loc_i in location_name){
    db_dat_i <- db_dat[db_dat$loc_name == loc_i,]



    # build file name
    freq <- tolower(freq)
    measurement_name <- gsub(" ", '-', measurement_name)
    substring(freq, 1, 1) <- toupper(substring(freq, 1,1))
    fname <- sprintf("%s-%s-%s",
                     loc_i, freq, measurement_name )

    if (!missing(output_directory) & nrow(db_dat)==0){ # if there are no records
      write.csv(data.frame(date_header=''), file.path(output_directory, fname),
                row.names = F, quote=F)
      warning(sprintf("Location %s returned no records. An empty file has
                      been written.", loc_i))
      next()
    }

    #  reshape
    db_dat_i <- db_dat_i[,c("height", "agg_avg", "time")]


    out <- reshape2::dcast(db_dat_i, time ~ height, value.var="agg_avg",
                           fun.aggregate=function(x) x[1])
    names(out)[grepl('time', names(out))] <- date_header

    # adjust depth header names
    if (depth_positive_down==F){ # counter-intuitive here because it is made negative above to be in the right order
      names(out)[-1] <- as.character(-as.numeric(names(out)[-1]))
    }

    if (!missing(depth_header_precision)){
      names(out)[-1] <- sprintf(
        paste0('%.0', depth_header_precision,'f'), as.numeric(names(out)[-1]))
    }

    if (!is.null(depth_header_prefix)){
      names(out)[-1] <- gsub('^', depth_header_prefix, names(out)[-1])
    }

    if (!is.null(depth_header_suffix)){
      names(out)[-1] <- gsub('$', depth_header_suffix, names(out)[-1])
    }


    # adjust time format
    out[,1] <- strftime(strptime(out[,1], "%Y-%m-%d %T"), format=date_format)

    # write output
    if (!missing(output_directory)){
      outfile <- file.path(output_directory, paste(fname, ".csv", sep=''))
      write.csv(out, file = outfile, row.names=F, quote=F, na = '')
    }else{
      return(out)
    }

  }
  }
