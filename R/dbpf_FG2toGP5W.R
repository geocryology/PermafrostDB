# =============================================================================
#'
#' @title dbpf_FG2toGP5W
#' 
#'
#' @description Returns connection to permafrost database
#'
#' @param inPath character, path to FG2 directory or specific FG2 file
#' 
#' @return Creates directory with new files. 
#' 
#' @output Log of which files were converted
#'   
#' @details Function takes a file path, scans for FG2 files to be converted.
#'          
#' @examples
#' # Example: passing directory of R files
#' dbpf_FG2toGP5W('/usr/desktop/FG2CSVs/', '/usr/desktop/FG2CSVs/converted')
#' 
#' # Example: Passing a single file. 
#' dbpf_FG2toGP5W('/usr/desktop/FG2CSVs/11232020_fg2.csv', '/usr/desktop/FG2CSVs/converted')
#' 
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
# =============================================================================
library("tools")
library("stringr")
options(warn=-1)

dbpf_FG2toGP5W <- function(inPath){
  direc = FALSE
  file = FALSE
  # test for existence
  if (file_test("-f", inPath) == 1) file <- TRUE
  else if (dir.exists(inPath) == 1) direc <- TRUE
  else {
    print(paste0("Location '", inPath, "' does not exist."))
    return(0)
  }
  
  newDir = paste(dirname(inPath),"/convertedFG2",sep='')
  
  ############ DIRECTORY #################

  if (direc == TRUE){
    inPath <- rewritedir(inPath)
    inFiles <- list.files(inPath)
    log <- structure(vector(mode = "list", length = length(inFiles)),names=inFiles)
    print(paste("Files successfully converted and moved to '", newDir, "' include:",sep=''))
    for (file in inFiles){
      file <- paste(inPath,file,sep='')
      if (fileType(file) == "FG2"){
        convertFG2(file)
        # Log
        print(paste(basename(file),"converted."))
      }
    }
  }
  
  ############# FILE ###################
  
  else if (file == TRUE){
    if (fileType(inPath) == "FG2"){
      convertFG2(inPath)
      # Log
      print(paste(basename(inPath),"converted and moved to",newDir))
    }
  }
  
}


convertFG2 <- function(filePath){
  # Takes FG2 file path and converts to GP5W
  con <- file(filePath, open='r')
  lines <- readLines(con)
  
  direc <- paste(dirname(filePath),"convertedFG2", sep='/')
  
  # Checking to see if /convertedFG2 exists in cur dir
  if (dir.exists(direc) == 0) dir.create(direc)
  
  # Creating new file 
  newFile <- (basename(filePath))
  newDir <- paste0(dirname(filePath), "_GP5WFormatted")
  print(newFile)
  print(newDir)
  sink(paste(direc, newFile, sep='/'))
  
  for(line in lines){
    
    # check if it is an info line
    if (grepl("<", line) == 1){
      if (grepl("LOGGER", line) == 1){
        logger <- substr(str_extract(line, "\\$......\\>"), 2, 7) # Pulls out 6-Digit logger ID
        newLine <- sprintf("Logger: #%s 'PT1000TEMP' - USP_EXP2 - (CGI) Expander for GP5W - (V2.7, Jan 12 2016)\n", logger)
        cat(newLine)
      }
      else next
    }
    
    # non comment lines
    else{
      
      # NO,TIME,#1((unk.)),#2((unk.)),HK-TEMP(oC),HK-BAT(V)
      # No,Time,#1:oC,#HK-Bat:V,#HK-Temp:oC
      
      
      if (grepl("TIME", line) == 1){
        line <- gsub("NO", "No", line)
        line <- gsub("TIME", "Time", line)
        line <- gsub("HK-BAT", "#HK-Bat:V", line)
        line <- gsub("HK-TEMP(oC)", "#HK-Temp:oC", line)
        line <- gsub("\\(\\(unk\\.\\)\\)", ":oC", line) # Replace ((unk.))
        cat(line)
      }
      # Regular time stamp info, nothing to change.
      else cat(line)
    }
  } # End of for loop
  sink()
  close(con)
  
}

# Function returns whether file is FG2, GP5W, or unidentifiable(FALSE)
fileType <- function(inPath){
  # Is it a file?
  if (file_test("-f", inPath) == 1) {
    # Is file a csv?
    if (file_ext(inPath) == "csv"){
      chars <- readLines(file(inPath,"r"),n=1)
      # Is file a fg2?
      if (grepl("FG2", chars) == 1){
        return("FG2")
      }
    }
  }
  # File is not FG2
  return("Not an FG2") 
}

# Ensures directory feature a trailing '/'
rewritedir <- function(inPath){
  char <- substr(inPath, nchar(inPath), nchar(inPath))
  if (char != '/') inPath <- paste(inPath, '/', sep='')
  return(inPath)
}

dbpf_FG2toGP5W('/Users/hannahmacdonell/Desktop/E53159_20210819161555.csv')
# filePath <- '/Users/hannahmacdonell/Desktop/E53159_20210819161555.csv'
# fileName <- (basename(filePath)) # fileName
# newDir <- (paste0(dirname(filePath), "_GP5WFormatted/")) # newDir
# print(paste0(newDir, fileName))


