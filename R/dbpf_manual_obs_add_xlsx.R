# =============================================================================
#'
#' @title Adds a new manual observations to DB based on Excel file.
#'
#' @description Adds new manual observations to DB based on Excel file. See description
#'              below for details and look at Atlassian for template.
#'
#' @details Run in test mode first. If you have no DB login to write data, run
#'          in test mode with your login and then pass to someone who does.
#'
#' @param con Database connection object, as returned by dbpf_con()
#' 
#' @param file_xlsx Excel file containing new manual observation descriptions and these
#'                  columns: 
#'                  sensor_label, 
#'                  location_name, 
#'                  time_UTC,
#'                  numeric_value,
#'                  text_value,
#'                  height_min_metres,
#'                  height_max_metres
#'                  
#'                  the format of time_UTC must be "%Y-%m-%d %H:%M:%S+00"
#'
#' @param mode = 'test' (default, read-only) or 'insert' (will insert into DB)
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_manual_obs_add_xlsx <- function(con, file_xlsx, mode = 'test') {
  if (grepl("~", file_xlsx) == TRUE) {
    stop("Use full path (without '~') for file_xlsx.")
  }
  
  # open file and check
  data <- openxlsx::read.xlsx(file_xlsx, 1)  # read first sheet
  
  data$sensor_label <- as.character(data$sensor_label)
  data$location_name <- as.character(data$location_name)
  data$time_UTC <- as.POSIXct(data$time_UTC)
  data$numeric_value <- as.numeric(data$numeric_value)
  data$text_value <- as.character(data$text_value)
  data$height_min_metres <- as.numeric(data$height_min_metres)
  data$height_max_metres <- as.numeric(data$height_max_metres)
  
  dbpf_manual_obs_add_table(con=con, data=data, mode=mode)
}


dbpf_manual_obs_add_csv <- function(con, file_csv, mode = 'test') {
  if (grepl("~", file_csv) == TRUE) {
    stop("Use full path (without '~') for file_csv.")
  }
  
  # open file and check
  data <- read.csv(file_csv, header=T)  # read first sheet
  
  data$sensor_label <- as.character(data$sensor_label)
  data$location_name <- as.character(data$location_name)
  data$time_UTC <- as.POSIXct(data$time_UTC)
  data$numeric_value <- as.numeric(data$numeric_value)
  data$text_value <- as.character(data$text_value)
  data$height_min_metres <- as.numeric(data$height_min_metres)
  data$height_max_metres <- as.numeric(data$height_max_metres)
  
  dbpf_manual_obs_add_table(con=con, data=data, mode=mode)
}


dbpf_manual_obs_add_table <- function(con, data, mode = 'test') {
  # loop over rows
  for (r in 1:nrow(data)) {
    # only use rows without sensor_id

      res <- dbpf_manual_obs_add (con,
                             data$sensor_label[r],
                             data$location_name[r],
                             data$time_UTC[r],
                             data$numeric_value[r], 
                             data$text_value[r],
                             data$height_min_metres[r],
                             data$height_max_metres[r],
                             mode = mode)
      data$import_comment[r] <- res
      
      # if imported, add id to list
      if (grepl("Inserted", res) == TRUE) {
        data$id[r] <- stringr::str_sub(res, start = -36)
      } 
      
      if (toupper(mode) == "TEST"){
        if (grepl("Testing: passed", res)){
          print(paste("ROW", r, "OK"))
        } else {
          print(paste("ROW", r, "FAIL (", paste(data[r,], collapse=','), ")"))
        }
      }
  }
  
}
