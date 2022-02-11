
# =============================================================================
#'
#' @title Plot trumpet curve
#'
#' @description Plots a trumpet curve for one location
#'
#' @details Plots a trumpet curve for one location and optionally includes
#'          near surface temperature and air temperature in this.
#' 
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param borehole_name Character string of the location to be plotted.
#'
#' @param gst_names Unit of measurments, defaults to "C"
#'
#' @param air_names Period over wich to aggregate time series [h], defaults
#'               to no aggregation (period = 'raw').
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "2015-09-01 00:00:00+00" (this example is the default)
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "2016-08-31 23:59:59+00" (this example is the default)
#'
#' @export
#' @examples
#' con <- dbpf_con()
#' dbpf_trumpet_curve(con, "NGO-DD-1009") # get all GST automatically
#'
#' # specify interval explicitly
#' dbpf_trumpet_curve(con, "NGO-DD-1009", time_b = "2015-09-01 00:00:00+00",
#'         time_e = "2016-08-31 23:59:59+00")
#'
#' # specify only one GST series for use, excluding the others
#' dbpf_trumpet_curve(con, "NGO-DD-1009", gst_names="NGO-DD-1009_ST01")
#'
#' # use air temperature
#' dbpf_trumpet_curve(con, "NGO-DD-1009", air_names=c("AIRT1LOW", "AIRT1TOP"))
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_trumpet_curve <- function(con, borehole_name, gst_names="", air_names="",
                          time_b = "2015-09-01 00:00:00+00",
                          time_e = "2016-08-31 23:59:59+00") {
    #establish connection
    if (missing(con)){
      con <- dbpf_con()
    }

    # borholes ========
    bh <- dbpf_observations_stats(con, borehole_name,
    						      time_b = time_b, time_e = time_e)
    #min(bh$min),max(bh$max)
    plot(bh$avg, bh$height, type="l",
         xlim=c(-25,15), ylim=c(min(bh$height),2),
         xlab="Temperature [\U00B0C]", ylab="Height [m]",
         main = paste("Ground thermal regime:",
         borehole_name))
    lines(bh$min, bh$height, col="blue")
    lines(bh$max, bh$height, col="red")

    #dots
    points(bh$avg, bh$height, col="black", pch=20)
    points(bh$min, bh$height, col="blue", pch=20)
    points(bh$max, bh$height, col="red", pch=20)

    #lines
    abline(v=0, lty = 2)
    abline(h=0)

    # GST ========
    if (gst_names[1] == "") {
    	gst_names <- paste0(borehole_name,
    	                    c("_ST01", "_ST02", "_ST03", "_ST04", "_ST05"))
    }
    gst <- dbpf_observations_stats(con, gst_names,
                                   time_b = time_b, time_e = time_e)
    if (nrow(gst) > 0) {
    	points(gst$avg, gst$height, col="black", pch=17)
    }

    # AIR ========
    if (air_names[1] != "") {
    	air <- dbpf_observations_stats(con, air_names,
    	                               time_b = time_b, time_e = time_e)
    	if (nrow(air) > 0) {
    		points(air$avg, air$height, col="black", pch=1)
    	}
    }

    #get values
    #mlstat <- dbpf_observations_stats(con, gst_names)
    rnge <- max(gst$avg) - min(gst$avg)
    mean <- mean(gst$avg)
    MAAT <- -8.565
    print(paste("MAGST:", round(mean,2),
                "Range:", round(rnge,2),
                "SurfO:", round(mean-MAAT,2)))

}


