# =============================================================================
#'
#' @title dbpf_GP5W_file_formatter.R
#' 
#' @description This function is very use-specific. It should only ever be
#'              applied to the following file structure:
#'              ------------
#'              Logger: #E50B38 'PT1000TEMP' - USP_EXP2 - (CGI) Expander for GP5W - (V2.7, Jan 12 2016)
#'              Delta Time: 666 secs
#'              No,Time,#1:oC,#HK-Bat:V,#HK-Temp:oC
#'              (Parameter changed ('E50A4C_20150609132643'))
#'              1,20.06.2016 16:57:55,26.2559,3.616,32.69
#'              ...              
#'              --- OR ---
#'              Logger: #E50B38 'PT1000TEMP' - USP_EXP2 - (CGI) Expander for GP5W - (V2.7, Jan 12 2016)
#'              Delta Time: 666 secs
#'              No,Time,#1:oC,#2:oC,#3:oC,#HK-Bat:V,#HK-Temp:oC
#'              (Parameter changed ('E50A4C_20150609132643'))
#'              1,20.06.2016 16:57:55,26.2559,26.6283,26.8267,3.616,32.69
#'              ...
#'              
#' @param inPath character, path to csv directory
#' @param con    connection to SensorDB
#' 
#' 
#' @return Creates directory with new cleaned files. 
#'   
#' @details Function takes a file path, scans for csv files to be converted.
#'              Fun variables: 
#'              inPath Desktop/Yk2021_test/testdir/
#'              newDir Desktop/Yk2021_test/testdir_clean/
#'              inFile Desktop/Yk2021_test/testdir/testfile.csv
#'              outFile Desktop/Yk2021_test/testdir_clean/testfile.csv
#'          
#' @examples
#' # Example: passing directory of R files
#' dbpf_GP5W_file_formatter('/usr/desktop/Yk2021/')
#' 
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
# =============================================================================
library("tools")
library("stringr")
library('data.table')
library('lubridate')
options(warn=-1)




dbpf_GP5W_file_formatter <- function(con, inPath) {
  # test for existence
  
  if (dir.exists(inPath) == FALSE) {
    cat("Location ", inPath, " does not exist.\n")
    return(0)
  }
  
  # If inPath doesn't end with '/', add
  if (substr(inPath,nchar(inPath), nchar(inPath)) != '/') {
    inPath <- paste0(inPath,'/',sep='')
  }
  
  # Creating location of new directory
  newDir <- paste(dirname(inPath), '/',basename(inPath),"formatted/",sep='')
  
  if (dir.exists(newDir) == FALSE) {
    dir.create(newDir)
    cat("Created directory to store formatted data files at: \n", newDir, "\n")
  }
  
  files <- list.files(inPath)
  
  # Open dir
  for (fileName in files){
    outFile <- paste0(newDir, fileName,sep='')
    inFile <- paste0(inPath, fileName,sep='')
    
    if (file.exists(outFile) == TRUE) {
      cat("Skipping ", fileName, " \n")
      next
    }
    
    # Reading in first line of csv
    conFile <- file(inFile,"r")
    firstLine <- readLines(conFile,n=1)
    close(conFile)
    
    # Open as DF, del HK col and del Parameter rows
    data <- fread(inFile, 
                  skip = "No,",
                  sep = ',',
                  stringsAsFactors = TRUE,
                  fill=TRUE)
    data <- as.data.frame(data)
    data <- data[, -grep("HK", colnames(data))]

    data <- data[!grepl("Parameter",data$No),]
    data <- data[!grepl("Delta Time",data$No),]
    data <- data[!grepl("Firmware Reset",data$No),]

    data <- time_cleaner(con, firstLine, data)
    if (data == FALSE) next
    
    write(firstLine, file=outFile)
    write.table(data, file=outFile, 
                append=TRUE, 
                col.names=TRUE, 
                row.names=FALSE,
                sep=',',
                quote=FALSE)
    }
}


time_cleaner <- function(con, firstLine, data){
  
  if (missing(con)){
    con <- dbpf_con()
  }
  
  # Get serial_number from firstLine
  if (grepl("Logger", firstLine) == 1){
    serial_number <- substr(str_extract(firstLine, "\\#E5...."), 2, 7) 
  }
  
  # Use device.id to find most recent observation in DB
  devIdQuery <- paste0("SELECT id FROM devices WHERE serial_number = '", serial_number, "'")        
  devID <- dbGetQuery(con, devIdQuery)
  obsQuery <- paste0("SELECT corrected_utc_time, location FROM observations ",
                  "WHERE device_id = '", devID, "' ORDER BY corrected_utc_time ",
                  "DESC LIMIT 1")
  most_recent_obs_df <- dbGetQuery(con, obsQuery)
  # Delete all times in csv before most recent observation.
  # Have to create temp column 'tempTime' to do this.
  data$tempTime <- as.POSIXct(gsub('\\.', '-', data$Time), format='%d-%m-%Y %H:%M:%OS')
  print(head(data, n = 1))
  data <- data[data[["tempTime"]] > most_recent_obs_df[1, 1], ]
  print(head(data, n=1))
  date <- (substr(most_recent_obs, 1, 10))
  print(most_recent_obs_df[1, 1])
  
  
  
  data <- data[, -grep("tempTime", colnames(data))]
  # Fixing 'No' column 
  if (length(data$No) < 1) {
    cat(" (File ", serial_number," already uploaded) \n")
    
    return(FALSE)
  }
  
  else {
    data$No <- seq(1, length(data$No))
    cat(" (File ", serial_number," clipped to this time stamp) \n")
  }

  return(data)
}
