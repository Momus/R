#' Read CSV files into a tibble.
#'
#' @description Using \code{\link[dplyr]{tbl_df}} read a file supported by that
#' function.
#' 
#' @param filename String contianing the name and relative path to a
#'     file.
#' @return A "tbl_df" tibble object.
#'
#'#' Errors:
#' \code{\dontrun{fars_read("invalid_file_name")}}
#' will produce a "file does not exist," error.
#'
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' \code{\dontrun{data2015 <- fars_read("./data/accident_2015.csv.bz2")}}
#' 
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' From the year, create a valid file name.
#'
#' @description Using \code{\link[dplyr]{tbl_df}} read a file supported by that
#' function.
#' 
#' @param year A string or integer representation of a year.
#'     
#' @return A string matching a valid file name for the dataset.
#'
#' Errors: Anything that can't be coerced by as.integer will
#' generate a warning, and the same file:
#' \code{\dontrun{
#        'zz <- make_filename("zz")
#'        Warning message:
#'        In make_filename("zz") : NAs introduced by coercion
#'       > zz
#'       [1] "accident_NA.csv.bz2"
#' }}
#'
#' 
#' @examples
#' \code{make_filename(2015)}
#' 
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Create a list of year tibbles.
#'
#' @description For each year passed into the function, create an
#'     tibble in a list that contains just the years and months from
#'     the dataset for those years.
#' 
#' 
#' @param years Vector of either intigers or strings correspondig to
#'     years for which the data set is available.
#'     
#' @return A list of  tibble objects.
#'
#' Errors: \code{\dontrun{fars_read_years(c("invalid_year"))}} will
#' produce an "invalid year" warning, and result in a list item with a
#' NULL value.
#'
#' @importFrom dplyr mutate select
#' 
#' @examples
#' \code{fars_read_years(c("2014", 2015))}
#' 
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' @export
#' 
#' Produce a summary of the number of accidents each month in the given years.
#'
#' @description For each year passed into the function, create a
#'     tibble listing the number of accidents per month for that year.
#'     
#' @param years Vector of either intigers or strings correspondig to
#'     years for which the data set is available.
#'     
#' @return A tibble of 12 rows (one per month) a column for month
#'     number, and as many year columns as were passed to the
#'     function.
#'
#' Errors: \code{\dontrun{fars_summarize_years(c("invalid_year"))}} will
#' produce an "invalid year" warning, and result in a empty column for
#' that year.
#' 
#' 
#' @importFrom dplyr bind_rowns group_by summarize
#' @importFrom tidyr spread
#' 
#' @examples
#' \code{fars_summarize_years(c("2014", 2015))}
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' map of a state with plotting all accidents for the given year.
#' 
#'
#' @description For each year and state number passed into the
#'     function, create a map of the state showing the location
#'     accidents in that year.
#'     
#' @param year Either string or integer representation of the year for
#'     which the map is to be drawn.
#' @param state Integer corresponding to the number of the state in
#'     the data set.
#'     
#' @return NULL; side effect: plots the map of the state.
#'
#' Errors:
#' \code{\dontrun{fars_summarize_years(valid_state, "invalid_year")}} will
#' produce an "file does not exist error."
#'
#' \code{\dontrun{fars_summarize_years(invalid_state, "valid_year")}} will
#' produce an "invalid state number" error.
#'
#' If a year/state combination does not have any accidents, a
#' "no accidents to plot" message will be generated as well as an
#' empty graphic.
#' 
#' @importFrom dplyr filter 
#' @importFrom maps map
#' @importFrom graphics points 
#' 
#' @examples
#' \code{fars_map_state(1, 2015)
#' 
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
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
