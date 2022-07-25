# =============================================================================
#'
#' @title Reformat GP5W file
#'
#' @description Removes some things from GP5W files
#'
#' @param in_path character, path to csv directory
#' @param con    connection to SensorDB
#'
#'
#' @return Creates directory with new cleaned files.
#'
#' @details This function is very use-specific. It should only ever be
#'              applied to the following file structure:
#'              ------------
#'              Logger: #E50B38 'PT1000TEMP' - USP_EXP2 - (CGI) (...)
#'              Delta Time: 666 secs
#'              No,Time,#1:oC,#HK-Bat:V,#HK-Temp:oC
#'              (Parameter changed ('E50A4C_20150609132643'))
#'              1,20.06.2016 16:57:55,26.2559,3.616,32.69
#'              ...
#'              --- OR ---
#'              Logger: #E50B38 'PT1000TEMP' - USP_EXP2 - (CGI) (...)
#'              Delta Time: 666 secs
#'              No,Time,#1:oC,#2:oC,#3:oC,#HK-Bat:V,#HK-Temp:oC
#'              (Parameter changed ('E50A4C_20150609132643'))
#'              1,20.06.2016 16:57:55,26.2559,26.6283,26.8267,3.616,32.69
#'              ...
#'
#'              Function takes a file path, scans for csv files to be converted.
#'              Fun variables:
#'              in_path Desktop/Yk2021_test/testdir/
#'              new_dir Desktop/Yk2021_test/testdir_clean/
#'              in_file Desktop/Yk2021_test/testdir/testfile.csv
#'              out_file Desktop/Yk2021_test/testdir_clean/testfile.csv
#'
#' @export
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
#' @importFrom utils write.table
# =============================================================================
dbpf_GP5W_file_formatter <- function(con, in_path) {
  if (dir.exists(in_path) == FALSE) {
    return(paste0("Location ", in_path, " does not exist."))
  } else {
    new_dir <- paste(dirname(in_path), "/",
               basename(in_path), "formatted/", sep = "")
    dir.create(new_dir)
  }

  files <- list.files(in_path)

  for (file_name in files){
    out_file <- paste0(new_dir, file_name, sep = "")
    in_file <- paste0(in_path, file_name, sep = "")

    # Reading in first line of csv
    con_file <- file(in_file, "r")
    first_line <- stringr::str_replace(readLines(con_file,
                  n = 1), "#2:rH", "#2:%rH")
    close(con_file)

    data <- data.table::fread(in_file,
                  skip = "No,",
                  sep = ",",
                  stringsAsFactors = TRUE,
                  fill = TRUE)

    data <- as.data.frame(data)
    data[data == "(NO VALUE)"] <- "NaN"
    data[data == "NA"] <- "NaN"
    data <- data[, -grep("HK", colnames(data))]
    data <- data[!grepl("Parameter", data$No), ]
    data <- data[!grepl("Delta Time", data$No), ]
    data <- data[!grepl("Firmware Reset", data$No), ]
    data <- time_cleaner(con, first_line, data)
    if (data == FALSE) next

    write(first_line, file = out_file)
    write.table(data, file = out_file,
                append = TRUE,
                col.names = TRUE,
                row.names = FALSE,
                sep = ",",
                quote = FALSE)
    }
}


time_cleaner <- function(con, first_line, data) {
  if (grepl("Logger", first_line) == 1) {
    serial_number <- substr(stringr::str_extract(first_line, "\\#E5...."), 2, 7)
  } else {
    return("Cant find logger serial_number in file")
  }
  # Use device.id to find most recent observation in DB
  dev_id_query <- paste0("SELECT id FROM devices WHERE serial_number = '",
                        serial_number, "'")
  dev_id <- dbGetQuery(con, dev_id_query)
  if (obs_exist(con, dev_id)){
    print(paste0("Cleaning file from ", serial_number, "..."))
    obs_query <- paste0("SELECT COUNT(DISTINCT observations.device_id) AS n,",
                       " MAX(corrected_utc_time) FROM devices INNER JOIN ",
                       "devices_locations ON devices_locations.device_id = ",
                       "devices.id INNER JOIN locations ON ",
                       "devices_locations.location_id = locations.id INNER ",
                       "JOIN observations ON observations.location=locations.",
                       "coordinates WHERE devices.id='", dev_id, "' AND ",
                       "observations.device_id = '", dev_id, "' ")

    most_recent_obs_df <- dbGetQuery(con, obs_query)
    # Delete all times in csv before most recent observation.
    # Have to create temp column 'temp_time' to do this.
    data$temp_time <- as.POSIXct(gsub("\\.", "-", data$Time),
                                format="%d-%m-%Y %H:%M:%OS")
    data <- data[data[["temp_time"]] > most_recent_obs_df$max, ]
    data <- data[, -grep("temp_time", colnames(data))]
    # Fixing 'No' column
    if (length(data$No) < 1) {
      return(paste0(" (File ", serial_number, " already uploaded) \n"))
    } else {
      data$No <- seq(1, length(data$No))
    }
  }
  return(data)
}

obs_exist <- function(con, dev_id){
  obs_query <- paste0("SELECT COUNT(*) FROM observations ",
                     "WHERE device_id = '", dev_id, "'")
  obs_df <- dbGetQuery(con, obs_query)
  if (obs_df[1, 1] > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
