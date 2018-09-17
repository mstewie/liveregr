#
# Shiny UI function
#
fluidPage(titlePanel("CSO Live Register Data Analyser"),
          fluidRow(
            # SidePanel
            column(
              3,
              # Data Loder panel
              wellPanel(
                h4("Data Loader"),
                tags$small(
                  paste0(
                    "Note: The CSO Unemployment Live Register data is available as either a ",
                    "raw number or a seasonally adjusted number."
                  )
                ),
                hr(),
                # Raw Data or Seasonally Adjusted Data
                selectInput(
                  "dataType",
                  "Statistical Indicator",
                  c(
                    "",
                    "Numbers" = "raw",
                    "Seasonally Adjusted Numbers" = "adjusted"
                  ),
                  selected = ""
                ),
                # Whether or not to use Offline or CSO API data
                radioButtons(
                  "dataSource",
                  "Data Source:",
                  c("Offline" = "offline",
                    "StatBank API" = "api"),
                  selected = "offline"
                )
              ),
              conditionalPanel(
                # This filter panel gets displayed once the dataset selection is made
                condition = "input.dataType != ''",
                wellPanel(
                  h4("Filter"),
                  # Year range slider
                  sliderInput(
                    "year",
                    "Year",
                    1967,
                    2018,
                    value = c(2000, 2018),
                    sep = ""
                  ),
                  # Gender dropdown selection list
                  selectInput(
                    "gender",
                    "Gender",
                    c(
                      "Male" = "male",
                      "Female" = "female",
                      "Both" = "both"
                    ),
                    selected = "both"
                  ),
                  # Age Group dropdown selection list
                  selectInput(
                    "ageGroup",
                    "Age Group",
                    c(
                      "Under 25" = "under_25",
                      "25 and over" = "over_and_25",
                      "All" = "all"
                    ),
                    selected = "all"
                  )
                )
              )
            ),
            column(
              # Main Panel
              9,
              # No data loaded - Load the CSO Logo image
              conditionalPanel(condition = "input.dataType == ''",
                               imageOutput("csoImage")),
              # Data is loaded - Load the tabbed panel
              conditionalPanel(
                # Data loaded - Load the tabs
                condition = "input.dataType != ''",
                # 3 tabs - 'Plot', 'Table', 'Summary'
                tabsetPanel(
                  type = "tabs",
                  # Plot Tab
                  tabPanel(
                    "Plot",
                    # The plot goes in the first row
                    fluidRow(column(12,
                                    align = "center",
                                    plotOutput("plot"))),
                    # The spline knot slider goes in the second row
                    fluidRow(column(12,
                                    align = "center",
                                    hr())),
                    # Spline Configuration panel
                    wellPanel(fluidRow(column(
                      12,
                      align = "center",
                      h4("Spline Configuration")
                    )),
                    fluidRow(
                      column(
                        4,
                        # Radio buttons to select Spline Type
                        radioButtons(
                          "splineType",
                          "Spline Type:",
                          c("B-Spline" = "b",
                            "Smoothing" = "smoothing"),
                          selected = "smoothing",
                          inline = TRUE
                        )
                      ),
                      column(8,
                             # Reactive widget: Knot range calculated server side
                             # before the slider widget is sent to the UI...nice!
                             htmlOutput("knotSlider"))
                    ))
                  ),
                  # Table Tab
                  tabPanel("Table",
                           tableOutput("table")),
                  # Summary Tab
                  tabPanel("Summary",
                           verbatimTextOutput("summary"))
                )
              )
            )
          ))
