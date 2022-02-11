# =============================================================================
#'
#' @title Time Series Analysis for GST
#'
#' @description Give a or multiple location names and it returns a list with snow cover start and end dates,
#'              a list with RD dates, a list with warming period dates and a list with zero curtain period
#'              dates. Furthermore, a plot is made for each location.
#'
#' @details The parameters can be adjusted. The different parameters are used by different functions.
#'
#'
#' @param con connection to PermafrostDB
#' 
#' @param inventory location names.  Can be a dataframe with column $name or a vector
#'
#'
#' @param out.path Path to directory where the plot should be saved. e.g: "~/Desktop/"
#'
#' @param time_b     Begin date for the import. Format: "1950-01-01 00:00:00+00"
#'
#' @param time_e     End date for the import. Format: "2050-01-01 00:00:00+00"
#'
#' @param v1    Max. daily standard deviation indicating snow for POSITIVE Ground Surface Temperature.
#'              Default is 0.1.
#'
#' @param v2    Max. daily standard deviation indicating snow for NEGATIVE Ground Surface Temperature.
#'              Default is 0.3.
#'
#' @param lengthsnow    Number of days that the snow cover should at least have to be selected as a valid snow cover.
#'                     Default is 5 days
#'
#' @param MDr.sd   Threshold for the mean daily standard deviation for a snow period.
#'
#' @param v 	  Threshold zero curtain (if dailymax & dailymin is within +/- v, this day is a zero curtain day)
#'              Default: 0.25
#'
#' @param tperc    Percentage of the 10-percent-quantile of the winter temperatures that the
#'                     warming period should at least reach. Default is 40 percent.
#'
#' @param sdoutlier   The number implies how many multiples of the standard deviation of the slope are taken to define a threshold.
#'             The threshold is used to detect slope outliers.
#'
#' @param sdsteep    The number implies what fraction of the standard deviation of the slope
#'               is taken to select the steepest part of a warming period.
#'
#' @param temp    The temperature boundary whithin the mean agg_avg of a zero curtain period has to be.
#'                Default: 0.2 degrees Celsius
#'
#' @param slopesd   Fraction of the standard deviation of the slope used as threshold for the slope. Default: 0.1
#'
#'
#' @param tempsd     Fraction of the mean daily standard deviation used as threshold for the daily sd. Default: 0.01
#'
#' @param lengthzc     Length that a zero curtain period should at least have. Default: 2 days.
#'
#' @param wateryear  start month of the water year. Default is 10 (October) according to the USGS.
#'
#' @return A list containing dataframes for the snow cover, RD dates, warming period start dates and
#'         zero curtain period dates. Each Dataframe has as well the location coordinates,
#'         the MAGST, the wateryear for which the MAGST was calculated as well as the number of the
#'         number of missing values of a certain water year.
#'         Furthermore, a plot is saved as a pdf file in the assigned directory.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' locationList <- dbpf_TSA_GST(con, "./", "NGO-DD-1004_ST01")
#' }
#' @author Thomas Knecht <t.knecht@@hotmail.com>
#' @importFrom ggplot2 aes geom_segment
# =============================================================================
dbpf_TSA_GST <- function(con, out.path, inventory, time_b="1950-01-01 00:00:00+00",time_e="2050-01-01 00:00:00+00",
                         v1 = 0.1, v2 = 0.3, lengthsnow = 5, MDr.sd = 0.3, v= 0.25,
                         tperc = 40, sdoutlier = 3, sdsteep = 0.75,
                         temp= 0.2, slopesd= 0.1, tempsd= 0.01, lengthzc= 2,wateryear=10){

  if (!requireNamespace('PermafrostTools')){
    cat(paste0("Missing package: 'PermafrostTools'",
           "\n    Please install it from the geocryology/R-packages repository"))
    return()
  }

  options(stringsAsFactors = FALSE)

  #retrieve location names and store it as vector
  if(class(inventory)=="character"){loc_names <- inventory}
  else if(class(inventory)=="data.frame"){loc_names <- inventory$name}
  else if(class(inventory)=="list"){loc_names <- inventory}
  else if(class(inventory)=="vector"){loc_names <- inventory}

  #function that does everything for one location
  data.list <- lapply(loc_names, FUN= function(X) ForOneLocation(X, con, out.path, time_b,time_e,
                                                                 v1, v2, lengthsnow, MDr.sd, v,
                                                                 tperc, sdoutlier, sdsteep,
                                                                 temp, slopesd, tempsd, lengthzc,wateryear))



  if(length(data.list)==1){

    data.list <- c("SnowCover"=data.list[[1]][1], "RD" = data.list[[1]][2], "Warming" = data.list[[1]][3], "ZeroCurtain" = data.list[[1]][4])

    return(data.list)

    }else{

    #make a list for snow cover with all locations
    SnowCover <- do.call( rbind, data.list)[,1]
    SnowCover <- do.call(rbind, SnowCover)


    #make a list for RD with all locations
    RD <- do.call( rbind, data.list)[,2]
    RD <- do.call( rbind, RD)


    #make a list for warming periods with all locations
    Warming <- do.call( rbind, data.list)[,3]
    Warming <- do.call( rbind, Warming)



    #make a list for zero curtains with all locations
    ZeroCurtain <- do.call( rbind, data.list)[,4]
    ZeroCurtain <- do.call( rbind, ZeroCurtain)



    #create a list with the snow cover-, RD-, Warming periods- and zero curtain tables.
    output.list <- list("SnowCover" = SnowCover, "RD" = RD, "Warming" = Warming, "ZeroCurtain" = ZeroCurtain)

    return(output.list)
  }



}

#Function that does everything for one location
ForOneLocation <- function(location, con, out.path, time_b,time_e, v1, v2, lengthsnow, MDr.sd, v,
                           tperc, sdoutlier, sdsteep, temp, slopesd, tempsd, lengthzc,wateryear){

  #imports data
  data.location <- PermafrostTools::TSA_data_import(con, location, time_b, time_e)

  #calculates snow period
  snow.period <- PermafrostTools::TSA_snow_cover(data.location, v1, v2, lengthsnow, MDr.sd)
  snow.period <- wtr_yr(snow.period)


  #detects RD
  RD.marcol <- PermafrostTools::TSA_RD(data.location, v, snow.period=snow.period)
  RD.marcol <- wtr_yr(RD.marcol)

  #detects warming period start dates
  warming.period <- PermafrostTools::TSA_warming_periods(data.location, tperc, sdoutlier, sdsteep, snow.period = snow.period)
  warming.period <- wtr_yr(warming.period)

  #detects zero curtains
  zero.curtain <- PermafrostTools::TSA_zero_curtain(data.location, temp, slopesd, tempsd, lengthzc, snow.period = snow.period)
  zero.curtain <- wtr_yr(zero.curtain)

  #creates plot
  invisible(f.plot(out.path, data.location=data.location, snow.period=snow.period, RD.marcol=RD.marcol, warming.periods=warming.period, zero.curtains=zero.curtain))

  #gives coordinates
  loc <- f.location(con, location)

  #calculates MAGST
  MAGST <- f.MAGST(data.location[[1]], wateryear)

  # adds coordinates and MAGST to the tables
  if(length(snow.period$loc_name)>0){
  snow.period <- merge(snow.period, loc, by = "loc_name")
  snow.period <- merge(snow.period, MAGST, by = "wateryear")
  snow.period <- snow.period[,c(2:7,1,8:10)]
  snow.period$wateryear <- NULL}

  if(length(RD.marcol$loc_name)>0){
  RD.marcol <- merge(RD.marcol, loc, by = "loc_name")
  RD.marcol <- merge(RD.marcol, MAGST, by = "wateryear")
  RD.marcol <- RD.marcol[,c(2:7,1,8:10)]
  RD.marcol$wateryear <- NULL}

  if(length(warming.period$loc_name)>0){
  warming.period <- merge(warming.period, loc, by = "loc_name")
  warming.period <- merge(warming.period, MAGST, by = "wateryear")
  warming.period <- warming.period[,c(2:12,1,13:15)]
  warming.period$wateryear <- NULL}

  if(length(zero.curtain$loc_name)>0){
  zero.curtain <- merge(zero.curtain, loc, by = "loc_name")
  zero.curtain <- merge(zero.curtain, MAGST, by = "wateryear")
  zero.curtain <- zero.curtain[,c(2:10,1,11:13)]
  zero.curtain$wateryear <- NULL}


  #creates list
  return.list <- list(snow.period, RD.marcol, warming.period, zero.curtain)

  print(location)

  return(return.list)
}



#selects coordinates for a location
f.location <- function(con,loc.name){


  query <- paste0("SELECT name AS loc_name, ST_X(coordinates) AS lon, ",
                  "ST_Y(coordinates) AS lat ",
                  "FROM locations WHERE name ='", loc.name, "'")
  loc <- dbGetQuery(con, query)

  loc$lon <- format(loc$lon, digits = 12)
  loc$lat <- format(loc$lat, digits = 12)

  return(loc)
}


# function to calculate the MAGST
#' @param obs_stat One dataframe of one location or a list of dataframes of multiple locations.
#'                 A dataframe needs at least the following columns:
#'                 Location name as loc_name;
#'                 Mean daily GST as agg_avg;
#'                 Minimum daily GST as agg_min;
#'                 Maximum daily GST as agg_max;
#'                 Daily sd as agg_sd;
#'                 Date as time (in POSIXct and the following format: YYYY-MM-DD)
#' 
#' @param wateryear ???
f.MAGST <- function(obs_stat, wateryear){

    # Convert dates into POSIXlt
    dates.posix <- as.POSIXlt(obs_stat$time)
    # Year offset
    offset <- ifelse(dates.posix$mon >= wateryear - 1, 1, 0)
    # Water year
    obs_stat$year <- dates.posix$year + 1900 + offset
    obs_stat$month <- wateryear


    MAGST.table <- do.call("rbind", as.list(
      by(obs_stat, obs_stat$year, function(X){

        year <- X$year[1]

        year_start <- X$time[1]#paste(year,"-",X$month[1],"-1",sep="")
        MAGST <- round(mean(X$agg_avg),digits=3)


        if((year %% 4) == 0) {
          if((year %% 100) == 0) {
            if((year %% 400) == 0) {
              total_count <- 366*72
            } else {
              total_count <- 365*72
            }
          } else {
            total_count <- 366*72
          }
        } else {
          total_count <- 365*72
        }

        actual_count <- sum(X$agg_cnt)
        missing_values <- total_count - actual_count

        output <- c(year, year_start, MAGST, missing_values)


      })))
    MAGST <- as.data.frame(MAGST.table)



    names(MAGST) <- c("wateryear","wateryear.start","MAGST","missing_values")
    row.names(MAGST) <- c(1:length(MAGST$wateryear))

    MAGST$wateryear.start <- as.POSIXct(MAGST$wateryear.start, origin="1970-01-01")

  return(MAGST)
}


# plot function
#' @importFrom ggplot2 theme_bw ylab xlab ggtitle scale_colour_manual guides guide_legend geom_line
f.plot <- function(out.path, data.location, snow.period, RD.marcol, warming.periods, zero.curtains){

  obs_stat <- data.location[[1]]

  graphics.off()

  Zeroheight <- (mean(obs_stat$agg_avg[obs_stat$agg_avg>0]))/2

  snowheight <- (max(obs_stat$agg_avg))/2

  yheight1 <- min(obs_stat$agg_avg)
  yheight2 <- max(obs_stat$agg_avg)

  title <- as.character(obs_stat$loc_name[1])


  if(is.na(warming.periods$Warming.Period.Start[1])){
    warmingplot <- geom_segment()
  }else{
    warmingplot <- geom_segment(aes(x=warming.periods$Warming.Period.Start,xend=warming.periods$Warming.Period.Start,y=yheight1,yend=yheight2, color="Warming_Start"),size=0.5)
  }

  if(is.na(zero.curtains$Zero.Curtain.Start[1])){
    zeroplot <- geom_segment()
  }else{
    zeroplot <- geom_segment(aes(x=zero.curtains$Zero.Curtain.Start,xend=zero.curtains$Zero.Curtain.End,y=Zeroheight,yend=Zeroheight, color="Zero_Curtains"),size=5)
  }

  if(is.na(snow.period$Snow.Start[1])){
    snowplot <- geom_segment()
  }else{
    snowplot <- geom_segment(aes(x=snow.period$Snow.Start,xend=snow.period$Snow.End,y=snowheight,yend=snowheight, color="Snow_Cover"),size=5)
  }


  if(is.na(RD.marcol$RD.Date[1])){
    rdplot <- geom_segment()
  }else{
    rdplot <- geom_segment(aes(x=RD.marcol$RD.Date,xend=RD.marcol$RD.Date,y=yheight1,yend=yheight2, color="RD_MarcOl"))
  }


  myplot <- ggplot() + geom_line(data=obs_stat, aes("time", "agg_avg", color="Mean_Temp")) +
    geom_line(data=obs_stat, aes("time", "agg_max", color="Max_Temp")) +
    geom_line(data=obs_stat, aes("time", "agg_min", color="Min_Temp")) +
    snowplot +
    zeroplot +
    warmingplot +
    rdplot +
    ylab(expression("Temperature"~(degree*C)*"")) +
    xlab("Date") +
    ggtitle(title) +
    scale_colour_manual("", values = c(Mean_Temp="black", Max_Temp="blue", Min_Temp="grey", Snow_Cover="green", Zero_Curtains="red", Warming_Start="black", RD_MarcOl="brown"))+
    guides(colour = guide_legend(override.aes = list(size=0.5))) +
    theme_bw()

  mypath <- file.path(out.path,paste(obs_stat$loc_name[1],".pdf",sep="") )
  pdf(file=mypath,width=7,height=5)
  print(myplot)
  dev.off()

}

wtr_yr <- function(obs_stat, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix <- as.POSIXlt(obs_stat$Snow.End)
  # Year offset
  offset <- ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  obs_stat$wateryear <- dates.posix$year + 1900 + offset
  # Return the water year
  return(obs_stat)
}

