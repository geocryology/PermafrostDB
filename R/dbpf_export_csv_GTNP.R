# =============================================================================
#'
#' @title Download database time series in GTNP-style ASCII format
#'
#' @description Downloads one or more time series from the carleton permafrost database
#' and exports them as a CSV that is formatted in the same way as GTN-P database
#' exports.
#'
#' @details Requires the PermafrostDB package.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character, one or more location names to convert.
#'
#' @param output_directory Character, path to output directory
#'
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is to
#' be aggregated from the database.
#' data.
#'
#' @param type character, used in file naming, recommended to be one of ("
#' Air_Temperature", "Surface_Temperature", "Ground_Temperature") to match GTN-P
#' file naming convention.
#'
#' @export
#' @author Nick Brown <nick.brown@@carleton.ca>
#' @importFrom utils write.csv
# =============================================================================

dbpf_export_csv_GTNP <- function(con, location_name, output_directory, freq='daily',
                             type='Ground_Temperature'){

  ## Download data from dbpf
  period <- switch(tolower(freq), 'daily'=24, 'hourly'=1)
  db_dat <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = 24)

  for (loc_i in location_name){
    db_dat_i <- db_dat[db_dat$loc_name == loc_i,]

  #
  # TODO[NB]: also download /format/write metadata if available
  #

  # TODO[NB]: automatically detect some of these params
  freq <- tolower(freq)
  substring(freq, 1, 1) <- toupper(substring(freq, 1,1))
  interval <- "Constant_Over_Interval"
  fname <- sprintf("%s-%s-%s-%s-Thermistor_Automated.timeserie",
                   loc_i,interval, freq, type )

  # change format to match GTN-P and reshape
  db_dat_i$height <- -db_dat_i$height
  db_dat_i <- db_dat_i[,c("height", "agg_avg", "time")]
  db_dat_i$agg_avg <- round(db_dat_i$agg_avg, 2)

  out <- reshape2::dcast(db_dat_i, time ~ height, value.var="agg_avg",
                         fun.aggregate=function(x) x[1])
  names(out)[grepl('time', names(out))] <- "Date/Depth"

  # write output
  outfile <- file.path(output_directory, paste(fname, ".csv", sep=''))
  write.csv(out, file = outfile, na='-999', row.names=F, quote=F)
 }
}

#con <- dbpf_con()
#dbpf_export_GTNP(con = con, location_name = c("NGO-DD-1012", "NGO-DD-2004"),
#                 output_directory = '~/Desktop', freq='daily')
