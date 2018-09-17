#' @title Loads the CSO Live Register data
#'
#' @description Loads the CSO Live Register unemployment data. The data can either be loaded from a offline JSON file (June 2018 data) included in the package or loaded directly from the CSO StatBank API.
#'
#' @param seasonally.adj Whether or not to load the seasonally adjusted live register data
#' @param use.offline.data Whether or not to use an offline data file from June 2018
#'
#' @return A \code{livereg} S3 object which contains the live register unemployment data and the timepoints associated with each data value.
#' @export
#'
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tidyr "separate"
#' @importFrom jsonlite "fromJSON"
#'
#' @examples
#' dat = load_livereg(seasonally.adj = TRUE, use.offline.data = TRUE)
load_livereg = function(seasonally.adj = TRUE,
                        use.offline.data = TRUE) {
  #
  # Load in the CSO LRM02 dataset. This can be done in 2 ways:
  #   (1) Load in the offline JSON file supplied with the package.
  #   (2) Read the JSON file from the CSO StatBank API. If this fails then the fallback
  #       is to load the offline JSON file.
  #
  live_reg_data = NULL;
  offiline_json_file = system.file("extdata", "LRM02.json", package = "liveregr")
  if (use.offline.data) {
    live_reg_data = fromJSON(offiline_json_file)
  } else {
    live_reg_data = tryCatch({
      fromJSON("https://www.cso.ie/StatbankServices/StatbankServices.svc/jsonservice/responseinstance/LRM02")
    }, error = function(e) {
      fromJSON(offiline_json_file)
    })
  }

  # Convience variables into the dataset
  raw_data = live_reg_data$dataset$value
  dims = live_reg_data$dataset$dimension

  # Set the start index into the data based on whether or not you want seasonally
  # adjusted data
  if (seasonally.adj) {
    start_idx = 2
  } else {
    start_idx = 1
  }

  # The CSO data is an alternating sequence of non-seasonally adjusted and seasonally
  # adjusted data
  live_reg_totals = raw_data[seq(start_idx, length(raw_data), 2)]

  # The number of time points in the dataset (data is monthly since 1967)
  num_time_points = live_reg_data$dataset$dimension$size[3]

  # Extract the data by age group first, this will then be split into gender groups
  by_age_data = data.frame(
    all =  live_reg_totals[1:(3*num_time_points)],
    under_25 = live_reg_totals[(3*num_time_points+1):(3*2*num_time_points)],
    over_and_25 = live_reg_totals[(2*3*num_time_points+1):(3*3*num_time_points)]
  )

  # Split the age group data by gender group
  by_gender_data = data.frame(
    both = data.frame(
      all = by_age_data$all[1:num_time_points],
      under_25 = by_age_data$under_25[1:num_time_points],
      over_and_25 = by_age_data$over_and_25[1:num_time_points]
    ),
    male = data.frame(
      all = by_age_data$all[(num_time_points+1):(2*num_time_points)],
      under_25 = by_age_data$under_25[(num_time_points+1):(2*num_time_points)],
      over_and_25 = by_age_data$over_and_25[(num_time_points+1):(2*num_time_points)]
    ),
    female = data.frame(
      all = by_age_data$all[(2*num_time_points+1):(3*num_time_points)],
      under_25 = by_age_data$under_25[(2*num_time_points+1):(3*num_time_points)],
      over_and_25 = by_age_data$over_and_25[(2*num_time_points+1):(3*num_time_points)]
    ),
    time_points = dims$Month$category$label %>% unlist,
    stringsAsFactors = FALSE
  )

  # Start building the 'livereg' S3 object with the relevant data
  livereg_obj = list(
    seasonally.adjusted = seasonally.adj,
    data = by_gender_data
  )

  # Separate the time_points variable into "year" and "month" columns.
  # The time_points variable format is: 'Year'M'Month'.
  livereg_obj$data %<>%
    separate(time_points,
             c("year", "mon"),
             sep = "M",
             convert = TRUE,
             remove = FALSE) %>%
    transform(month = month.name[mon],  # Month as a string
              # Convert Year Month into a Date object
              time_points = time_points %>%
                sub("M", "-", .) %>%
                paste0("-01") %>%
                as.Date("%Y-%m-%d"))

  # Turn the list into an S3 object
  class(livereg_obj) = "livereg"

  return(livereg_obj)
}

###############
# £nd Of File #
###############
