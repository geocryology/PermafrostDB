# =============================================================================
#'
#' @title Adds new polygons to database
#'
#' @description Inserts a data frame of new plots into database. Insertion
#'              is done one plot at a time and feedback is provided.
#'              Duplicate names are identified.
#'              The default mode (mode='test') only tests the data to be
#'              inserted. Insert mode (mode='insert') requires a database
#'              connection generated with a login that has insert priviledge.
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'
#' @param con connection to database
#'
#' @param locations Data frame with these columns (type):
#'                  name (character);
#'                  NW_lat (numeric, latitude in WGMS84);
#'                  NW_lon (numeric, longitude in WGMS84);
#'                  NE_lat (numeric, latitude in WGMS84);
#'                  NE_lon (numeric, longitude in WGMS84);
#'                  SE_lat (numeric, latitude in WGMS84);
#'                  SE_lon (numeric, longitude in WGMS84);
#'                  SW_lat (numeric, latitude in WGMS84);
#'                  SW_lon (numeric, longitude in WGMS84);
#'                  elevation_in_metres (numeric);
#'                  comment (character);
#'                  record_observations (character, can only be 't' or 'f')
#'
#' @param mode Can be 'test' (default) or 'insert' (requires login with insert previledges)
#'
#' @param overlap Can be 'allow' or 'prevent' (default) and governs whether
#'                a new polygon can overlap an existing one.
#'
#'
#' @return List of all locations with a column 'inserted' indicating which
#'         ones were inserted, columns 'duplicate_name' indicates existing name
#'         conflict, ‘intersect_plot’ indicates that two plots intersect.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' result <- dbpf_plots_add(con, locations)
#' }
#' @author Thomas Knecht <t.knecht@hotmail.com>
#'         Stephan Gruber <stephan.gruber@carleton.ca>
# =============================================================================

dbpf_locations_add_plot <- function(con, locations, mode = "test",
                                                    overlap = "prevent") {

  #test mode
  test_mo <- (toupper(mode) == "TEST") + (toupper(mode) == "INSERT")
  if (test_mo != 1) {
    stop(paste("Parameter 'mode' must be either",
               "'test' or 'insert'."))
  }

  #test mode
  test_mo <- (toupper(overlap) == "PREVENT") + (toupper(overlap) == "ALLOW")
  if (test_mo != 1) {
    stop(paste("Parameter 'overlap' must be either",
               "'allow' or 'prevent'."))
  }


  #test information provided
  input <- subset(locations, select = c(name, NW_lon, NW_lat, SW_lon, SW_lat,
                                        NE_lon, NE_lat, SE_lon, SE_lat,
                                        elevation_in_metres, comment,
                                        record_observations))

  #fix column data type, add check columns
  input$NW_lon <- as.numeric(as.character(input$NW_lon))
  input$NW_lat <- as.numeric(as.character(input$NW_lat))
  input$SW_lon <- as.numeric(as.character(input$SW_lon))
  input$SW_lat <- as.numeric(as.character(input$SW_lat))
  input$NE_lon <- as.numeric(as.character(input$NE_lon))
  input$NE_lat <- as.numeric(as.character(input$NE_lat))
  input$SE_lon <- as.numeric(as.character(input$SE_lon))
  input$SE_lat <- as.numeric(as.character(input$SE_lat))
  input$elevation_in_metres <- as.numeric(as.character(input$elevation_in_metres))
  input$comment <- as.character(input$comment)
  input$record_observations <- as.character(input$record_observations)
  input$duplicate_name <- TRUE
  input$intersect_plot <- TRUE
  input$inserted <- FALSE

  # test for na and for entries other than t/f in record_observations
  test_ro <- nrow(input) - sum(input$record_observations == TRUE) -
    sum(input$record_observations == FALSE)
  if (test_ro != 0) stop("Entries for record_observations need to be 't' or 'f'!")

  # loop over locations and test
  for (r in 1:nrow(input)) {
    loc <- input[r,]

    # check for existing name
    query <- paste0("SELECT COUNT(DISTINCT id) FROM locations WHERE name = '" , loc$name, "'")
    if (dbGetQuery(con, query)$count == 0) {input$duplicate_name[r] <- FALSE}

    # make polygon string / ST_SetSRID
    pgs <- paste0("ST_SetSRID(ST_MakePolygon(ST_GeomFromText('LINESTRING(",
                   loc$NW_lon," ", loc$NW_lat,", ", loc$NE_lon," ", loc$NE_lat,", ",
                   loc$SE_lon," ", loc$SE_lat,", ", loc$SW_lon," ", loc$SW_lat,",",
                   loc$NW_lon," ", loc$NW_lat, ")')), 4326)")

    # find overlaping polygons
    query <- paste0("SELECT name FROM locations WHERE ST_Intersects(", pgs,
                    ", coordinates) AND ST_GeometryType(coordinates)='ST_Polygon';")
    poly_overlap <- dbGetQuery(con, query)$name
    poly_overlap <- paste(t(poly_overlap), collapse=", ")
    if (poly_overlap == "") {input$intersect_plot[r] <- FALSE}

    # decide: import or now
    prevent <- input$duplicate_name[r] + input$intersect_plot[r] * (toupper(overlap) != "ALLOW")


    #test feedback and insert or message
    if ((prevent == 0) * (toupper(mode) == "INSERT")) {
       query <- paste0("INSERT into locations (name, coordinates, ",
                      "elevation_in_metres, comment, record_observations) ",
                      "VALUES('", loc$name, "', ", pgs, "
                      , ", loc$elevation_in_metres, ", '",
                      loc$comment, "', '", loc$record_observations, "')")
        dbExecute(con, query)
        input$inserted[r] = TRUE
    } else if (prevent == 1) {
      print(paste0("!!! Location [", loc$name, "] not imported, check returned data frame."))
      print(paste0("Overlapping polygons: [", poly_overlap, "]"))
    }
  }
  return(input)
}




