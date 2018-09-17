#' @title Filter CSO Live Register data
#'
#' @description \code{filter} filters the CSO Live Register unemployment data based on certain input options. These options are gender, age group, start year, and end year.
#'
#' @param object \code{livereg} S3 object
#' @param gender a character vector of gender choices: "both", "male", "female"
#' @param age.group a character vector of age group choices: "all", "under_25", "over_and_25"
#' @param start.year the start year of the data
#' @param end.year the end year of the data
#'
#' @return A filtered \code{livereg} S3 object which contain the live register unemployment data based on the input filtering parameters.
#' @export
#'
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @examples
#' dat = load_livereg(use.offline.data = TRUE)
#' the_1990s_data = filter(dat, start.year = 1990, end.year = 1999)
#' female_data = filter(dat, gender = c("female"))
filter = function(object,
                  gender = c("both", "male", "female"),
                  age.group = c("all", "under_25", "over_and_25"),
                  start.year = 1967,
                  end.year = 2018) {
  UseMethod('filter')
}

#' @export
filter.livereg = function(object,
                          gender = c("both", "male", "female"),
                          age.group = c("all", "under_25", "over_and_25"),
                          start.year = 1967,
                          end.year = 2018) {
  #
  # Validate the start.year and end.year inputs
  #
  if (start.year < 1967 || end.year < 1967) {
    stop("start.year and/or start.year must be greater than 1967")
  }

  if (start.year > end.year) {
    stop("start.year cannot be greater than end.year")
  }

  # Validate the gender input
  gender_arg = match.arg(gender,
                         choices = c("both", "male", "female"),
                         several.ok = TRUE)

  # Validate the age group input
  age_arg = match.arg(age.group,
                      choices = c("all", "under_25", "over_and_25"),
                      several.ok = TRUE)

  # Create a vector of "gender.age" strings based on the input parameters. These will be
  # the column names of the data which we are filtering on
  gender_age_pairs = c()
  for (gen in gender_arg) {
    for (age in age_arg) {
      gender_age_pairs = c(gender_age_pairs, paste(gen, age, sep = "."))
    }
  }

  # Add the time point column names to the list of dataset column names
  filtered_cols = c(gender_age_pairs, c("time_points", "year", "mon", "month"))

  # Filter the dataset based on input start and end year, and then extract the gender
  # and age group columns we are interested in
  object$data %<>%
    subset(year >= start.year & year <= end.year) %>%
    magrittr::extract(filtered_cols)

  return(object)
}

###############
# £nd Of File #
###############
