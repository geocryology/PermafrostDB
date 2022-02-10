# =============================================================================
#'
#' @title Adds a new sensors to DB based on Excel file.
#'
#' @description Adds a new sensors to DB based on Excel file. See description
#'              below for details and look at Atlassian for template.
#'
#' @details Run in test mode first. If you have no DB login to write data, run
#'          in test mode with your login and then pass to someone who does.
#'
#'
#' @param file_xlsx Excel file containing new sensor descriptions and these
#'                  columns: label, device_id, type_of_measurement,
#'                  unit_of_measurement, accuracy, precision,
#'                  height_in_metres, serial_number, sensor_id
#'                  Column sensor_id is left blank for new sensors.
#'
#' @param mode = 'test' (default, read-only) or 'insert' (will insert into DB)
#'
#' @return Writes updated Excel file with same name. In the file, test results
#'         are reported and, if new sensors were made, their id is given.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' bpf_sensor_add_xlsx(con, "/Users/stgruber/sensor.xlsx", mode = 'test')
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================
dbpf_sensor_add_xlsx <- function(con, file_xlsx, mode = 'test') {
    if (grepl("~", file_xlsx) == TRUE) {
    		stop("Use full path (without '~') for file_xlsx.")
    }

    # open file and check
    data <- openxlsx::read.xlsx(file_xlsx, 1)  # read first sheet
    data$label <- as.character(data$label)
    data$device_id <- as.character(data$device_id)
    data$type_of_measurement <- as.character(data$type_of_measurement)
    data$unit_of_measurement <- as.character(data$unit_of_measurement)
    data$accuracy <- as.numeric(as.character(data$accuracy))
    data$precision <- as.numeric(as.character(data$precision))
    data$height_in_metres <- as.numeric(as.character(data$height_in_metres))
    data$serial_number <- as.character(data$serial_number)
    data$sensor_id <- as.character(data$sensor_id)
    data$unit_of_measurement[is.na(data$unit_of_measurement)] <- ""
    data$serial_number[is.na(data$serial_number)] <- ""
    data$sensor_id[is.na(data$sensor_id)] <- ""
    data$accuracy[is.na(data$accuracy)]   <- 0
    data$precision[is.na(data$precision)] <- 0
    data$height_in_metres[is.na(data$height_in_metres)] <- 0


    data$import_comment = ""  # add new column

    # loop over rows
    for (r in 1:nrow(data)) {
    	# only use rows without sensor_id
    	if (data$sensor_id[r] != "") {
    		data$import_comment[r] <- "Skipped: already has id"
    	} else {
    		res <- dbpf_sensor_add(con, data$device_id[r], data$label[r],
    		                       data$type_of_measurement[r],
                                   data$unit_of_measurement[r],
                                   data$accuracy[r], data$precision[r],
                                   data$height_in_metres[r],
                                   data$serial_number[r],
                                   mode = mode)
            data$import_comment[r] <- res

            # if imported, ass id to list
            if (grepl("Row inserted", res) == TRUE) {
            	data$sensor_id[r] <- str_sub(res, start = -36)
            }
    	}
    }


    #	print(data)

    #save
    write.xlsx(x = data, file = file_xlsx, sheetName = "Data", row.names = FALSE)
}