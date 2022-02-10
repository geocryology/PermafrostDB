# =============================================================================
#'
#' @title Write attributes from template
#'
#' @description Write any number of netCDF attributes from a csv template specifying
#' variable names and attribute properties.
#'
#' @param nc_object An object of class ncdf4, indicating what file to write to.
#'
#' @param templatefile character, path to a csv text file that provides
#' information about the attributes to add. See details
#'
#' @details This function allows the same variable attributes to be standardized
#' across a number of netcdf objects without having to change each one manually.
#'
#' The templatefile csv should have the following column headers:
#' variable_id: the name of the variable to which to add an attirubte. Leave blank
#' to create global attributes
#' attribute_name: the name of the desired attribute
#' attribute_value: the value of the attribute
#' precision: the precision of the attribute.  Can have values of "short",
#' "float", "double", or "text". If unspecified, the written precision is the same
#' as the variable whose attribute it is.
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
nc_attributes_from_template <- function(nc_object, templatefile, verbose=F){
  attr <- read.csv(templatefile, stringsAsFactors = F, )
  for (row in 1:nrow(attr)){
    attr_i <- attr[row,]
    if (attr_i$variable_id %in% c('', '0') | is.na(attr_i$variable_id)){
      var_id <- 0
    }else{
      var_id <- attr_i$variable_id
    }

    at_name <- attr_i$attribute_name
    at_val <- attr_i$attribute_value
    if (is.character(at_val) & !at_val %in% c('T','F') &
        !grepl('char', attr_i$precision, ignore.case=T)){
      at_val <- type.convert(at_val, as.is=T)
    }
    if (verbose){
      print(var_id)
      print(at_name)
      print(at_val)
      print(class(at_val))
    }
    if (attr_i$precision=='' | is.na(attr_i$precision)){
      ncdf4::ncatt_put(nc_object, var_id, at_name, at_val)
    }else{
      ncdf4::ncatt_put(nc_object, var_id, at_name, at_val, prec=attr_i$precision)
    }
  }
}
