#
# Shiny server function
#
function(input, output, session) {

  #
  # Load, filter and fit the model to the dataset in a reactive function. This stops these
  # actions from happening explicitly in each render function which would be a huge performance
  # overhead.
  #
  csoData = reactive({
    # Have we selected the way we want to import the data
    if (input$dataType == '')
      return(NULL)

    # Load in the dataset
    adjusted = ifelse(input$dataType == "adjusted", TRUE, FALSE)
    offline = ifelse(input$dataSource == "offline", TRUE, FALSE)
    dat = load_livereg(adjusted, offline)

    # Filter the dataset based on user input
    filterd_dat = filter(dat,
                         gender = input$gender,
                         age.group = input$ageGroup,
                         start.year = input$year[1],
                         end.year = input$year[2])

    # Make sure that input knots doesn't exceed the range
    inputs_knots = ifelse(rVals$num_knots > length(filterd_dat$data$time_points),
                          5,
                          rVals$num_knots)

    # Fit the model to the data
    fit(filterd_dat,
        spline.type = input$splineType,
        num.knots = inputs_knots)
  })

  # Reactive Values:
  #  num_knots - The number of spline knots
  rVals = reactiveValues(num_knots = 5)

  # We are not going to use the input$knots value directly as we need to reset it when
  # we change the splineType. This event is fired when the splineType is changed so we
  # reset the num_knots to a default value of 5.
  observeEvent(input$splineType, {
    rVals$num_knots = 5
  })

  # This event is fired when you move the slider input. Update the ReactiveValue for
  # num_knots.
  observeEvent(input$knots, {
    rVals$num_knots = input$knots
  })

  #
  # Render the CSO logo
  #
  output$csoImage = renderImage({
    # filename is ./images/CSO_logo.png
    filename = normalizePath(file.path("./images", "CSO_logo.png"))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = "CSO Logo Image")

  }, deleteFile = FALSE)

  #
  # Render the plot of the livereg_fit S3 object
  #
  output$plot = renderPlot({
    dat = csoData()
    if (is.null(dat))
      return(NULL)

    plot(dat)
  })

  #
  # Render the summary of the livereg_fit S3 object
  #
  output$summary = renderPrint({
    summary(csoData())
  })

  #
  # Render the table of the filtered CSO dataset
  #
  output$table = renderTable({
    df = data.frame(date = as.character(csoData()$data$time_points),
                    number = csoData()$data[1],
                    predicted.values = csoData()$predicted.values)
    df$date = substr(df$date, 1, 7)  # Have the date field as Year-Month
    df
  })


  #
  # Reactive slider widget - slider range calculated based on the dataset
  #
  output$knotSlider = renderUI({
    if (is.null(csoData()))
      return(NULL)

    # Get the maximum number of knots allowed
    max_knots = ifelse(input$splineType == "b",
                       10,
                       length(csoData()$data$time_points))

    # Get the number number of knots based on the spline type
    min_knots = ifelse(input$splineType == "b",
                       1,
                       5)

    # Make sure that input knots doesn't exceed the range
    inputs_knots = ifelse(rVals$num_knots > max_knots,
                          min_knots,
                          rVals$num_knots)

    # Render the slider back to the UI
    sliderInput("knots",
                "Spline Knots",
                min_knots,
                max_knots,
                value = inputs_knots,
                sep = "")
  })

}


