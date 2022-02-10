# =============================================================================
#'
#' @title dbpf_shiny DESCRIPTION NOT YET OK
#'
#' @description Returns connection to permafrost database @ Carleton University
#'
#' @details Function establishes a connection to the data base and return a
#'          connection object for easy use. The database is "observations" on
#'          server permafrost.gcrc.carleton.ca. When finished, close connection
#'          object (see examples).
#'
#' @param user    User name, defaults to "readonly"
#' @param passwd  Password, default set for readonly account.
#'
#' @return Returns a DB connection object.
#'
#' @export
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
#'
# =============================================================================

# =============================================================================
#   RUN APP
# =============================================================================
#' @importFrom shiny runApp
dbpf_shiny <- function() {
    runApp(list(ui=ui,server=server))
}

# =============================================================================
#   SERVER
# =============================================================================
#' @import shiny
server <- function(input, output) {


  #

  # get data, ugly and not interactive for now
  con <- dbpf_con()
  data <- dbpf_observations_raw(con, "NGO-RC-172_ST01", unit_of_measurement = "C")

# selectig input range does not work yet
#                                time_b = input$dates[1],
#                                time_e = input$dates[2])

  # -------------------------------------------------------------------
  # Linked plots to navigate (left) and select points in a zoom (right)
  # -------------------------------------------------------------------

  # make changeable variables
  range <- reactiveValues(x = NULL, y = NULL)
  selec <- reactiveValues(set = rep(FALSE, nrow(data)))

  # navigation plot
  output$plot_navigate <- renderPlot({
    ggplot(data, aes(time, value)) +
      geom_point(aes(colour = selec$set)) +
      scale_color_manual(values=c("#000000", "#D55E00"))
  })

  # plot to zoom and select
  output$plot_zoom <- renderPlot({
    ggplot(data, aes(time, value)) +
      geom_point(aes(colour = selec$set)) +
      coord_cartesian(xlim = range$x, ylim = range$y, expand = FALSE) +
      scale_color_manual(values=c("#000000", "#D55E00"))
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot_navigate_brush
    if (!is.null(brush)) {
      range$x <- as.POSIXct(c(brush$xmin, brush$xmax),
                            origin = "1970-01-01", tz = "UTC")
      range$y <- c(brush$ymin, brush$ymax)

    } else {
      range$x <- NULL
      range$y <- NULL
    }
  })

  # Observe selections
  observe({
    # alter data frame with raw data
    selec$set[data$id %in% brushedPoints(data, input$plot_zoom_brush)$id] <- TRUE
  })


  # -------------------------------------------------------------------

  # show table of selected points
  #output$brush_info <- renderPrint({
  #  brushedPoints(data, input$plot_zoom_brush)
  #})

}



# =============================================================================
#   USER INTERFACE
# =============================================================================
ui <- fluidPage(
  fluidRow(
    column(width = 10, class = "well",
      #date range
      dateRangeInput("dates", label = h3("Date range for analysis"), start = "2015-01-01"),
 	  # plots
      h4("Overview plot (left) controls zoom (right). Select points in zoom."),
      fluidRow(
        column(width = 4,
          plotOutput("plot_navigate", height = 300,
            brush = brushOpts(
              id = "plot_navigate_brush",
              resetOnNew = TRUE
            )
          )
        ),
        column(width = 6,
          plotOutput("plot_zoom", height = 300,
            brush = brushOpts(
               id = "plot_zoom_brush"
            )
          )
        )
      )
    )
  ),
  fluidRow(
    column(width = 6,
      h4("Brushed points"),
      verbatimTextOutput("brush_info")
    )
  )
)





