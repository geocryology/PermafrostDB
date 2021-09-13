# =============================================================================
#'
#' @title Used to swap logger associated with a pre-existing string of loggers.
#'
#' @description Inserts a data frame of new devices_locations into database.    
#'
#' @details Function will indicate whether the file you're about to upload is 
#'          safe for uploading. 
#'
#' @param dirPath To local observations .csv file directory. Files should follow 
#'                proper GP5W or FG2 naming convention. 
#'                ie. E50BDA_YYYYMMDDhhmmss.csv
#'
#' @param con DB connection as returned by dbpf_con(). For insert priviledges
#'            specific user name and password need to be supplied.
#' 
#' @return An output log indicating whether .csv is fit for upload to sensorDb
#'
#' @examples
#' dbpf_observations_upload_check(con, 'filePath'path/to/csv) 
#'
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
# =============================================================================


dbpf_observations_upload_check <- function(con, dirPath) {

  if (dir.exists(dirPath) == FALSE) {
    print(paste0("Location '", dirPath, "' does not exist."))
    return(0)
  }

  if (substr(dirPath,nchar(dirPath), nchar(dirPath)) != '/') {
    dirPath <- paste0(dirPath,'/',sep='')
  }

  # Initializing log dataframe 
  log <- data.frame(TEST=character(), 
                    STATUS=character(),
                    NOTES=character(),
                    stringsAsFactors=FALSE) 

  files <- list.files(dirPath, full.names = TRUE)

  for (filePath in files){

    log <- log[0,] # Clearing log for next file

    log <- rbind(log, file_exist(filePath))
    log <- rbind(log, identify_logger(con, filePath))
    log <- rbind(log, fileType(filePath))
    log <- rbind(log, is_file_cleaned(filePath))
    log <- rbind(log, is_file_uploaded(con, filePath, log))
    readline(prompt=paste0("Next file is ", 
                            basename(filePath),
                            ". Press [enter] to continue."))
    print.data.frame(log, row.names=FALSE, right=FALSE)

  }
}


file_exist <- function(filePath){

  if (file.exists(filePath) == TRUE) {
    result <- "Pass"
    note <- paste0("File ", basename(filePath), " exists.", sep='')
  }

  else {
    result <- "Fail"
    note <- paste0("File ",basename(filePath)," could not be found", sep='')
  }

  test.result <- data.frame("Does file exist?", result, note)      
  names(test.result) <- c("TEST", "STATUS", "NOTES")  
  return (test.result)
}

fileType <- function(filePath){

  confile <- file(filePath,"r")
  chars <- readLines(confile,n=1)
  close(confile)

  if (grepl("E53", chars) == 1 && grepl("GP5W", chars) == 1){
    note <- "This is a converted FG2 file."
    result <- "Pass"
  }

  else if (grepl("FG2", chars) == 1){
    note <- paste0("FG2 File not yet converted.")
    result <- "Fail"
  }

  else if (grepl("E50", chars) == 1 && grepl("GP5W", chars) == 1){
    note <- "GP5W file."
    result <- "Pass"
  }

  else {
    note <- "Cannot read file type."
    result <- "Fail"
  }

  test.result <- data.frame('Is file GP5W formatted?', result, note)      
  names(test.result) <- c("TEST", "STATUS", "NOTES")  
  return (test.result)

}

identify_logger <- function(con, filePath){

  result <- 'Fail'
  note <- 'Unable to perform test.'

  # If file is unformatted FG2 
  fileTypeDf <- fileType(filePath)
  if (head(fileTypeDf$STATUS, 1) == 'Fail'){
    # Extracting first 6 digits of filePath
    serial_number <- substr(basename(filePath), 1, 6)  
    serial_number <- substr(str_extract(serial_number, "E5...."), 1, 6) 
    # Ensuring serial_number is correct.
    if (is.na(serial_number)){
      result <- "Fail"
      note <- "Unable to read logger serial_number from file or file name."
    }
  }

  confile <- file(filePath,"r")
  firstLine <- readLines(confile,n=1)
  close(confile)

  # If file formatted FG2 or GP5W
  if (grepl("Logger", firstLine) == 1){
    serial_number <- substr(str_extract(firstLine, "\\#E5...."), 2, 7) 
    if (is.na(serial_number)){
      result <- "Fail"
      note <- "Unable to read logger serial_number"
    }
  }

  # If serial_number exists
  if (!is.na(serial_number)){
    query <- paste0("SELECT COUNT(*) FROM devices WHERE serial_number = '",
                    serial_number, "'")

    exists <- dbGetQuery(con, query)

    if (exists$count != 0){
      result <- 'Pass'
      note <- paste0("Serial_number ", serial_number, " exists in sensorDB.")
    }
  }

  test.result <- data.frame('Does logger exist?', result, note)      
  names(test.result) <- c("TEST", "STATUS", "NOTES")  
  return (test.result)

}

is_file_cleaned <- function(filePath){
  fileTypeDf <- fileType(filePath)
  if (head(fileTypeDf$STATUS, 1) == 'Fail'){
    result <- 'Fail'
    note <- 'File is an uncleaned unformatted FG2.'
  }

  else if (head(fileTypeDf$STATUS, 1) == 'Pass'){

    df <- try(read.csv(filePath, header=TRUE, skip=1), 
                silent = TRUE)

    if (class(df) != "try-error") {
      read.csv(filePath, header=TRUE, skip=1)
      further_testing <- TRUE
    } else {
      result <- 'Fail'
      note <- 'Cannot read to df. Needs to be cleaned.'
      further_testing <- FALSE
    }

    # If df is readable, further testing 
    if (further_testing) {
      if (!any(grepl("No", colnames(df)))){
        result <- 'Fail'
        note <- 'GP5W file is not cleaned. Contains HK Col.'
      }
      parDf <- df[grepl("Parameter",df$No),]
      deltDf <- df[grepl("Delta Time",df$No),]
      firmDf <- df[grepl("Firmware Reset",df$No),]

      if (nrow(parDf) > 0 || nrow(deltDf) > 0 || nrow(firmDf) >0){
        result <- 'Fail'
        note <- 'GP5W file is not cleaned. Bad rows.'
      }

      else {
        result <- "Pass"
        note <- "Visual scan of file still recommended before upload. "
      }
    }
  }


  test.result <- data.frame('Is file clean?', result, note)      
  names(test.result) <- c("TEST", "STATUS", "NOTES")  
  return (test.result)

}

is_file_uploaded <- function(con, filePath, log){
  # If file hasn't failed any other tests, or we at least have logger id:
  if (log$STATUS[2] == "Pass" ){

    serial_number <- substr(str_extract(log$NOTES[2], "E5...."), 1, 6)
    devIdQuery <- paste0("SELECT id FROM devices WHERE serial_number = '",
                         serial_number, "'")        
    obsQuery <- paste0("SELECT corrected_utc_time, location FROM observations ",
                       "WHERE device_id = '", dbGetQuery(con, devIdQuery), 
                       "' ORDER BY corrected_utc_time DESC LIMIT 1 ")
    most_recent_db_obs <- dbGetQuery(con, obsQuery)

    if (nrow(log[grep("Fail", log$STATUS),]) == 0 ){
      df <- tail(read.csv(filePath, header=FALSE), n=1)
      most_recent_file_obs <- as.POSIXct(gsub('\\.', '-', df$V2),
                                         format='%d-%m-%Y %H:%M:%OS')

      if (most_recent_db_obs$corrected_utc_time < most_recent_file_obs){
        result <- 'Pass'
        note <- paste0('File good to upload. Most recent db obsv: ',
                       most_recent_db_obs$corrected_utc_time)
      }
      else {
        result <- 'Fail'
        note <- 'This file has been uploaded.'
      }
    }
    else{
      result <- 'Fail'
      note <- paste0('Failed to read file. Most recent db obsv: ',
                     most_recent_db_obs$corrected_utc_time)
      }

  }

  else{
    result <- "Fail"
    note <- "Failed to read file or identify logger id. Clean first."
  }
  test.result <- data.frame('Is file already uploaded?', result, note)      
  names(test.result) <- c("TEST", "STATUS", "NOTES")  
  return (test.result)
}