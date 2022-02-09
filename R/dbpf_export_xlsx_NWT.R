# =============================================================================
#'
#' @title Write permafrost data as NWT xlsx standard
#'
#' @description Downloads data from the database in a csv format that is consistent
#' with the NTGS xlsx file standard for ground temperature data.
#' 
#' @details Details in NWT open report (unpublished)
#'
#' @param con Database connection object, as returned by dbpf_con()
#' 
#' @param location_name Character, name of site for which data is to be desired.
#' Used to query the database.
#' 
#' @param file_name Character, path to an xlsx file to be written
#' 
#' @param project_name Character, project name associated with the location_name
#'
#' @details This function relies on the Rtools library to create the xlsx file.
#' This can be downloaded from https://cran.r-project.org/bin/windows/Rtools/
#' 
#' @export
#' 
#' @examples
#'  \dontrun{
#' dbpf_export_xlsx_NWT(con=con, location_name = "NGO-DD-1009", 
#'     file_name = "~/NGO-DD-1009_Hourly_GroundTemperature.xlsx", 
#'     project_name = "SPSMPS")   
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_export_xlsx_NWT <- function(con, location_name,  
                                 project_name, file_name, freq='hourly', split=T){
  if (!require(openxlsx)){
    stop("Please install the openxslx package")
  }
  if (!grepl("xlsx$", file_name)){
    stop("Please ensure file_name ends in '.xlsx'")
  }
  
  # Download Data
  NWT_dat <- dbpf_export_csv_NWT(con=con,  freq=freq,
                             location_name=location_name, 
                             project_name=project_name, split=split)
  if (is.null(NWT_dat)){
    return(NULL)
  }
  
  if (class(NWT_dat)=="list"){
    file.remove(
      list.files(dirname(file_name), 
                 pattern=gsub("\\.csv$","_?[0-9]*.xlsx",basename(file_name)),
                 full.names=T))
    for (table in NWT_dat){
      convert_to_xlsx(table, file_name_increment(file_name), location_name)

    }
  }else{
  convert_to_xlsx(NWT_dat, file_name, location_name)
  }
  }


# =============================================================================
#'
#' @title Write NWT csv table to formatted xlsx
#'
#' @description Write NWT csv table to formatted xlsx
#' 
#' @details dbpf_export_xlsx_NWT is a wrapper for this function to allow for 
#' file generation when there is sensor movement.
#'
#' @param NWT_dat table returned from dbpf_export_csv_NWT
#' 
#' @param location_name NWT_dat a data.frame returned from dbpf_export_csv_NWT 
#' 
#' @param file_name Character, path to an xlsx file to be written
#' 
#' @examples
#' 
#'     
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
convert_to_xlsx <- function(NWT_dat, file_name, location_name){
  nro <- nrow(NWT_dat)
  nco <- ncol(NWT_dat)
  
  # Create file
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, location_name)
  openxlsx::writeData(wb, 1, NWT_dat, startRow = 1, startCol = 1, colNames = F)
  
  numdata <- apply(NWT_dat[7:nro, 2:nco, drop=F],2, as.numeric)
  
  openxlsx::writeData(wb, 1, numdata , startRow = 7, startCol = 2, colNames = F)
  
  latlon_data <- as.numeric(NWT_dat[3:4, 2])
  openxlsx::writeData(wb, 1, latlon_data , startRow = 3, startCol = 2, colNames = F)
  
  # Create Styles
  latlon <- openxlsx::createStyle(numFmt='0.0000',halign='left', valign = 'center')
  openxlsx::addStyle(wb, 1, style = latlon, rows = 3:4, cols = 2, gridExpand = T)
  
  datafmt <- openxlsx::createStyle(numFmt="NUMBER", halign='center', valign = 'center')
  openxlsx::addStyle(wb, 1, style = datafmt, rows = 8:nro, cols = 2:nco, gridExpand = T)
  
  header <- openxlsx::createStyle(numFmt = "TEXT", wrapText = T, halign='center', 
                                  valign = 'center', border=c("top", "bottom"))
  openxlsx::addStyle(wb, 1, style = header, rows = 6:7, cols = 1:nco, gridExpand = T)
  
  openxlsx::setColWidths(wb, 1, 1, widths = 21.29)
  openxlsx::setColWidths(wb, 1, 2:nco, widths = 13.29)
  
  # Write file
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}
