# =============================================================================
#'
#' @title Download database time series in CSV ASCII format
#'
#' @description Downloads data from the database in a csv format that approximates
#' the NTGS xlsx file standard for ground temperature data.
#'
#' @details None
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character, name of site for which data is to be desired.
#' Used to query the database.
#'
#' @param project_name Character, used in file header metadata
#'
#' @param file_name Character, path to a csv file to be written
#'
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is
#'  to be aggregated from the database.
#'
#' @export
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
#' @importFrom utils write.table
# =============================================================================
dbpf_export_csv_NWT <- function(con,
                                location_name,
                                project_name,
                                file_name,
                                project_number,
                                freq='daily'
                                ){
  if (missing(project_number)){
    project_number <- project_name
  }
  
  # Download data
  data <- dbpf_export_csv_generic(con, freq = freq,
                                  location_name=location_name,
                                  date_format = "%Y-%m-%d %H:%M:%S",
                                  date_header = "(DD/MM/YYYY HH:MM)")

  if (is.null(data)){
    return(NULL)
  }

  # Split into multiple parts if sensors move. TODO: this needs work.
  # right now it is only foxed. could be for moving depths too

  if (all(names(data)[-1] %in% c('0','0.1')) & ncol(data)==3){ # if data are mostly disjoint
    data <- list(D1=data[,c(1,3)], D2=data[,c(1,2)])
    out <- lapply(data, format_NWT,location_name = location_name,
                  project_name = project_name, project_number=project_number)
  }else{
    out <- format_NWT(con=con, data=data, location_name = location_name,
                      project_name = project_name, project_number=project_number)
  }

  # Output or return final result
  if (!missing(file_name)){
    if (class(out)=="list"){
      file.remove(
        list.files(dirname(file_name),
                  pattern=gsub("\\.csv$","_?[0-9]*.csv",basename(file_name)),
                  full.names=T))
      for (table in out){
        write.table(x = table, file = file_name_increment(file_name), sep=',',
                    quote=F, na='', row.names=F, col.names=T)
      }
    }else{
      write.table(x = out, file = file_name, sep=',',
                  quote=F, na='', row.names=F, col.names=T)
    }

  }else{
    return(out)
  }
}


# =============================================================================
#'
#' @title Format data in NTGS-style
#'
#' @description adds header information to a csv to conform with NTGS data
#' standards.
#'
#' @details None
#' 
#' @param con Database connection object, as returned by dbpf_con()
#' 
#' @param data data frame 
#' 
#' @param location_name Character, name of site for which data is to be desired.
#' Used to query the database.
#'
#' @param project_name Character, used in file header metadata
#'
#' @export
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
format_NWT <- function(con, data, location_name, project_name, project_number){
  # Get necessary metadata
  coords <- dbGetQuery(con, paste0(
    "SELECT name, ST_X(coordinates) as lon, ST_Y(coordinates) as lat
    FROM locations
    WHERE name = '", location_name, "'"))
  
  # Create df
  out <- data.frame(project_number=project_number,
                    project_name=project_name,
                    site_id=location_name,
                    latitude=coords$lat,
                    longitude=coords$lon,
                    `date_YYYY-MM-DD`=substr(data[,1], 0, 10),
                    `time_HH:MM:SS`=substr(data[,1], 12, 19))
  
  names(out)[6:7] = c("date_YYYY-MM-DD", "time_HH:MM:SS")
  
  # Append numeric data (as character)
  data[is.na(data)] <- -999
  names(data) <- gsub("^(-?[0-9\\\\.]*)$", "\\1_m", names(data))
  
  out <- cbind(out, data[, -1])
  names(out)[-seq(7)] = names(data)[-1]

  return(out)
}



# =============================================================================
#'
#' @title Increment filename
#'
#' @param filepath path to a file
#'
#' @description generates numerically incremented filename if filename exists#'
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
file_name_increment <- function(filepath){
  if(file.exists(filepath)){ #does file already exist
    file_orig <- filepath
    i <- 1
    while(file.exists(filepath)){
      filepath <- gsub("(\\.[^\\.]{2,4})$", paste0("_",i,"\\1"),file_orig)
      i <- i+1
    }
  }else{
    return(filepath)
  }
  return(filepath)
}