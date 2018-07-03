#Read FARS Data

#'

#' Reads a Fatality Analysis Reporting System (FARS) file in csv format and creates a dataframe (dplyr) from it.


#' @param filename A character string giving the name of the file to read.

#' @return This function returns a \code{dplyr::tbl_df} of the file.

#' @import dplyr

#' @note If the file or permission to it does not exist, an error is produced.

#'

#' @examples

#' test_data <- system.file("extdata", "accident_2014.csv.bz2", package = "fars")

#' fars_read(filename=test_data)

#'

#' @references \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}

#'

#' @export

fars_read <- function(filename) {

  if(!file.exists(filename))

    stop("file '", filename, "' does not exist")

  data <- suppressMessages({

    readr::read_csv(filename, progress = FALSE)

  })

  dplyr::tbl_df(data)

}



#' Generate FARS Filename

#' Generates a standard Fatality Analysis Reporting System (FARS) filename for the given year.

#'

#' @param year A 4 digit year, as integer format.

#'

#' @return This function returns a FARS filename in character string format.

#'

#' @note If year cannot be coerced to an integer, use NA.

#'

#' @examples

#' make_filename('2013')

#'

#' @references \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}

#'

#' @export

make_filename <- function(year) {

  year <- as.integer(year)

  sprintf("accident_%d.csv.bz2", year)

}

#' Read FARS Data for One or More Years

#'

#' Read in Fatality Analysis Reporting System (FARS) files and returns a dplyr tbl_df of the month and year columns for each file.

#'

#' @param years vector of 4 digit years, as integers.

#'

#' @return returns a \code{dplyr::tbl_df} with month and year columns.

#' @import dplyr

#' @note If the file for a specified year, or permission to it, does not exist, an error is produced.

#' @examples

#' \dontrun{fars_read_years(2013:2015)}

#' @references \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}

#' @export

fars_read_years <- function(years) {

  lapply(years, function(year) {

    file <- make_filename(year)

    tryCatch({

      MONTH=NULL

      dat <- fars_read(file)

      dplyr::mutate(dat, year = year) %>%

        dplyr::select(MONTH, year)

    }, error = function(e) {

      warning("invalid year: ", year)

      return(NULL)

    })

  })

}



#' Summarize FARS Data for one or several years

#'

#' Reads a series of Fatality Analysis Reporting System (FARS) files and returns a frequency table of fatal crashes by month and year.

#'

#' @inheritParams fars_read_years

#'

#' @return This function returns a data frame of the number of fatal crashes by month (row) and year (column).

#'

#' @import dplyr

#' @importFrom tidyr spread

#'

#' @note If the file for a specified year, or permission to it, does not exist, an error is produced.

#'

#' @examples

#' \dontrun{fars_summarize_years(2013:2015)}

#'

#' @references \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}

#'

#' @export

fars_summarize_years <- function(years) {

  dat_list <- fars_read_years(years)

  MONTH=NULL

  year=NULL

  dplyr::bind_rows(dat_list) %>%

    dplyr::group_by(year, MONTH) %>%

    dplyr::summarize(n = n()) %>%

    tidyr::spread(year, n)

}



#' Map FARS Crash Fatalities

#'

#' Reads a Fatality Analysis Reporting System (FARS) file and plots a map of fatal crashes.

#'

#' @param state.num Two digit FARS state reference number.

#' @param year Two digit year, coerced to integer.

#'

#' @return This function produces a plot of the fatal crashes on a map of the state.

#'

#' @import dplyr

#' @import maps

#' @importFrom graphics points

#' @note If the \code{state.num} provided is not in the FARS file, an error is shown.

#' @note If there are no fatal crashes for the state and year, a message is shown.

#' @note LONGITUD values > 900 are set to NA, LATITUDE values > 90 are set to NA

#'

#' @examples

#' \dontrun{fars_map_state('01','2014')}

#'

#' @references

#' FARS data: \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars};

#' State reference numbers: \url{https://crashstats.nhtsa.dot.gov} see publication 812296

#'

#' @export

fars_map_state <- function(state.num, year) {

  filename <- make_filename(year)

  data <- fars_read(filename)

  state.num <- as.integer(state.num)

  STATE=NULL # to avoid R CMD Check note

  if(!(state.num %in% unique(data$STATE)))

    stop("invalid STATE number: ", state.num)

  data.sub <- dplyr::filter(data, STATE == state.num)

  if(nrow(data.sub) == 0L) {

    message("nothing to plot")

    return(invisible(NULL))

  }

  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900

  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90

  with(data.sub, {

    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),

              xlim = range(LONGITUD, na.rm = TRUE))

    graphics::points(LONGITUD, LATITUDE, pch = 46)

  })

}
