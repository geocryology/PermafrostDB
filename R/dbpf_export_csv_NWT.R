# =============================================================================
#'
#' @title Download database time series in CSV ASCII format 
#'
#' @description Downloads data from the database in a csv format that approximates
#' the NTGS xlsx file standard for ground temperature data.
#'
#' @details None
#' 
#' @param con Database connection object, as returned by dbpf_con
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
#' @examples
#' dbpf_export_csv_NWT(con, 'NGO-DD-1009', "Slave Province Surficial Materials and 
#' Permafrost Study")
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_export_csv_NWT <- function(con, 
                                location_name,
                                project_name,
                                file_name,
                                freq='daily',
                                split=T
                                ){
  
  # Download data
  data <- dbpf_export_csv_generic(con=con, freq = freq,
                                  location_name=location_name,
                                  date_format = "%d/%m/%Y %H:%M",
                                  date_header = "(DD/MM/YYYY HH:MM)")
  
  if (is.null(data)){
    return(NULL)
  }

  # Split into multiple parts if sensors move. TODO: this needs work.
  # right now it is only foxed. could be for moving depths too
  
  if (all(names(data)[-1] %in% c('0','0.1')) & ncol(data)==3){ # if data are mostly disjoint
    data <- list(D1=data[,c(1,3)], D2=data[,c(1,2)])
    out <- lapply(data, format_NWT,location_name = location_name,
                  project_name = project_name)
  }else{
    out <- format_NWT(data=data, location_name = location_name, 
                      project_name = project_name) 
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
                    quote=F, na='', row.names=F, col.names=F)
      }
    }else{
      write.table(x = out, file = file_name, sep=',',
                  quote=F, na='', row.names=F, col.names=F)
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
#' @param location_name Character, name of site for which data is to be desired.
#' Used to query the database.
#' 
#' @param project_name Character, used in file header metadata
#' 
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
format_NWT <- function(data, location_name, project_name){
  # Get necessary metadata
  coords <- dbGetQuery(con=con, paste0(
    "SELECT name, ST_X(coordinates) as lon, ST_Y(coordinates) as lat
    FROM locations 
    WHERE name = '",location_name,"'"))
  
  # Construct header metadata table
  top <- as.data.frame(matrix(data=character(), nrow=7, ncol=ncol(data)),
                       stringsAsFactors=F)
  top[1,1] <- "Project Name:"
  top[2,1] <- "Site ID:"
  top[3,1] <- "Latitude:"
  top[4,1] <- "Longitude:"
  top[6,1] <- "Date Time (UTC)"
  
  top[1,2] <- project_name
  top[2,2] <- location_name
  top[3,2] <- sprintf("%.4f",coords$lat)
  top[4,2] <- sprintf("%.4f",coords$lon)
  
  top[6,2:ncol(data)] <- "Measurement Depth (m)"
  top[7,] <- names(data)
  
  # Append numeric data (as character)
  data[is.na(data)] <- -999
  names(data) <- names(top)
  out <- rbind(top, as.matrix(data))
  
  return(out)
}



# =============================================================================
#'
#' @title Increment filename
#' 
#' @details None
#'
#' @description generates numerically incremented filename if existing filename 
#' is taken
#' 
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

file_name_increment <- function(filepath){
  if(file.exists(filepath)){ #does file already exist
    file_orig <- filepath
    i=1
    while(file.exists(filepath)){
      filepath <- gsub("(\\.[^\\.]{2,4})$", paste0("_",i,"\\1"),file_orig)
      i <- i+1
    }
  }else{
    return(filepath)
  }
  return(filepath)
}