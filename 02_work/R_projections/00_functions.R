############################################################################## #
# Filename
#    00_functions.R
#
# Description
#   functions-file
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #


############################################################################## #
# checkRVersion
#
# Description
#   Check if your using the expected R version, as specified in the project
#   details.
#
# Input
#   -
#
# Output
#   invisible returns TRUE / FALSE if correct R version is used or not
############################################################################## #
checkRVersion <- function() {
  R_version_project <- getProjDetails("R_version")
  if (is.null(R_version_project)) {
    return(invisible(NULL))
  }
  R_version_used <- as.character(getRversion())
  if (R_version_project != R_version_used) {
    warning(
      "Warning: R version ",
      R_version_project,
      " is expected! You are using ",
      R_version_used,
      "!"
    )
    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}

############################################################################## #
# cranEx
#
# Description
#   search for CRAN examples (opens browser)
#
# Input
#   what:
#     function name (or any other string) to search examples for
#
# Output
#   -
#
# Credit: https://gist.github.com/jennybc/4a1bf4e9e1bb3a0a9b56
############################################################################## #
cranEx <- function(what) {
  url <-
    paste(
      "https://github.com/search?l=r&q=%22",
      as.character(substitute(what)),
      paste0(
        "%22+user%3Acran+language%3AR&",
        "ref=searchresults&type=Code&utf8=%E2%9C%93"
      ),
      sep = "",
      collapse = ""
    )
  browseURL(url)
}

############################################################################## #
# getProjDetails
#
# Description
#   get project details, if they are defined
#
# Input
#   type:
#     character giving the type of project details
#   get_all:
#     boolean indicating, if all project details should be returned as list
#   paste:
#     boolean, return value gets pasted if TRUE
#
# Output
#   value / vector of requested project details
############################################################################## #
getProjDetails <- function(type = c(
  "title",
  "wd_rproj",
  "project_directory",
  "authors",
  "R_version",
  "packages",
  "project_yaml_version"
),
get_all = FALSE,
paste = FALSE) {
  type <- match.arg(type)
  
  # check if project details are available
  if (!("RPROJ" %in% search())) {
    warning("No project details defined! Returning NULL!")
    return(NULL)
  } else if (get_all) {
    vapply(
      X = ls("RPROJ"),
      FUN = function(x)
        get(x, "RPROJ"),
      FUN.VALUE = ""
    )
  } else if (!exists(type, where = "RPROJ")) {
    warning(sQuote(type),
            " not defined in project details! Returning NULL!")
    return(NULL)
  }
  
  ret <- get(type, "RPROJ")
  if (paste) {
    ret <- paste(ret, collapse = ", ")
  }
  return(ret)
}



############################################################################## #
# getWDs
#
# Description
#   Get working directires as defined by varaibles starting with "wd_"
#
# Input
#   -
#
# Output
#   working directories suitable for checking with CheckWDs
############################################################################## #
getWDs <- function() {
  wd.names <- ls(pattern = "wd_", envir = .GlobalEnv)
  wds <-
    unlist(vapply(
      X = wd.names,
      FUN = function(x) {
        ret <- as.character(eval(parse(text = x)))
        if (length(ret) == 0)
          ret <- NA_character_
        ret
      },
      FUN.VALUE = ""
    ))
  return(wds)
}

############################################################################## #
# checkWDs
#
# Description
#   Check if working directories exist
#
# Input
#   wds
#     Named character vector holding the working directories to check.
#     Names give
#   create_missing_wds
#     boolean (default FALSE): should the function create directires, that does
#     not exist yet?
#
# Output
#   -
############################################################################## #
checkWDs <- function(wds, create_missing_wds = FALSE) {
  # loop to check existance of each directory
  check_ok <- TRUE
  for (i in 1:length(wds)) {
    wd <- wds[i]
    if (is.na(wd)) {
      message(paste0(names(wd), " is undefined!"))
    } else if (!dir.exists(wd)) {
      if (create_missing_wds) {
        dir.create(wd)
        message(paste0(names(wd), " = ", wd, " created!"))
      } else {
        message(paste0(names(wd), " not found:\n  ", wd))
        warning(
          paste0(
            "Working directory ",
            dQuote(names(wd)),
            " does not exist yet",
            " -> please create it!"
          )
        )
        check_ok <- FALSE
      }
    }
  }
  return(invisible(check_ok))
}


############################################################################## #
# pathTree
#
# Description
#   Print relative path of x with base "base"
#
# Input
#   wd
#     working directory of which to print the path
#   dir.subset
#     named subset of directories to search in for relative paths (default is
#     generated by function GetWDs)
#
# Output
#   -
############################################################################## #
pathTree <- function(wd, dirs.subset = NULL) {
  if (!require(dplyr))
    return(NULL)
  if (!require(data.tree))
    return(NULL)
  
  if (is.null(dirs.subset)) {
    dirs.subset <- getWDs()
  }
  
  path <- dirs.subset[grep(wd, dirs.subset)]
  # list.dirs(wd, recursive = TRUE)
  path.name <- names(dirs.subset[dirs.subset == wd])
  path.name <- ifelse(is.null(path.name), "root", path.name)
  path <-
    gsub(wd, paste0(dQuote(path.name), "/"), path)
  
  x <- lapply(strsplit(path, "/"), function(z) {
    as.data.frame(t(z), stringsAsFactors = FALSE)
  })
  x <- dplyr::bind_rows(x)
  x$pathString <-
    apply(as.matrix(x), 1, function(x)
      paste(trimws(na.omit(x)), collapse = "/"))
  x$wd_name <- names(path)
  mytree <- data.tree::as.Node(x)
  data.tree::Sort(mytree, "name")
  # print left hand sided
  capture.output(res <- as.data.frame(print(mytree, "wd_name")), file = "NUL")
  return(res) #print(res, right = F)
}


##############################################################################
# plot_prediction
#
# Description:
#   Creates a line plot that visualizes training (base period), test (true values),
#   and prediction data for a specified cohort. The function standardizes the 
#   population values from potentially different column names (provided as input)
#   into a common column ("population") for plotting. A vertical dashed line marks 
#   the end of the base period. The data are sorted by series and year to ensure 
#   proper connection of points.
#
# Input:
#   train_data:
#     A data frame containing training data with columns "index", "year", and a 
#     column specified by train_col_name representing the population values.
#   test_data:
#     A data frame containing test data with columns "index", "year", and a 
#     column specified by test_col_name representing the population values.
#   prediction_data:
#     A data frame containing prediction data with columns "index", "year", and a 
#     column specified by prediction_col_name representing the population values.
#   train_col_name:
#     A character string indicating the name of the column in train_data that 
#     contains the population values.
#   test_col_name:
#     A character string indicating the name of the column in test_data that 
#     contains the population values.
#   prediction_col_name:
#     A character string indicating the name of the column in prediction_data that 
#     contains the population values.
#   cohort:
#     A character or factor representing the cohort identifier used to filter the data.
#   prediction_method:
#     (Optional) A character string describing the prediction method, which is 
#     included in the plot's title. Defaults to an empty string.
#
# Output:
#   A ggplot object that displays the combined plot. The plot features:
#     - A line and point series for the training (base period) data.
#     - A line and point series for the test (true values) data.
#     - A line and point series for the prediction data.
#     - A vertical dashed line indicating the end of the base period.
##############################################################################
plot_prediction <- function(train_data,
                            test_data,
                            prediction_data,
                            train_col_name,
                            test_col_name,
                            prediction_col_name,
                            municipality_code,
                            sex,
                            age_group,
                            prediction_method = "") {
  # Filter the original train_data for the specific cohort and preserve 'year'
  cohort_train <- train_data %>% 
    filter(municipality_code == !!municipality_code, !!sex == sex, age_group == !!age_group) %>%
    mutate(year = as.numeric(year)) 
  
  base_end <- max(cohort_train$year)
  
  # Process train_data: keep year and rename the value column to 'population'
  train_data <- cohort_train %>%
    select(municipality_code, sex, age_group, year, population = !!rlang::sym(train_col_name)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(series = "Base period")
  
  # Process test_data similarly and add the base_end row for continuity
  test_data <- test_data %>%
    filter(municipality_code == !!municipality_code, sex == !!sex, age_group == !!age_group) %>%
    mutate(year = as.numeric(year)) %>%
    select(municipality_code, sex, age_group, year, population = !!rlang::sym(test_col_name)) %>%
    bind_rows(train_data %>% filter(year == base_end)) %>%
    mutate(series = "True values")
  
  # Process prediction_data similarly and add the base_end row
  prediction_data <- prediction_data %>%
    filter(municipality_code == !!municipality_code, sex == !!sex, age_group == !!age_group) %>%
    mutate(year = as.numeric(year)) %>%
    select(municipality_code, sex, age_group, year, population = !!rlang::sym(prediction_col_name)) %>%
    bind_rows(train_data %>% filter(year == base_end)) %>%
    mutate(series = "Prediction")
  
  # Combine the data sets
  train_test_data <- bind_rows(train_data, test_data)
  all_data <- bind_rows(train_test_data, prediction_data) %>%
    arrange(series, year)
  
  # Create the plot
  ggplot(all_data, aes(x = year, y = population, color = series)) +
    geom_line(linewidth = 1) +
    geom_point() +
    geom_vline(xintercept = base_end,
               linetype = "dashed",
               color = "blue") +
    labs(
      title = paste(prediction_method, "Prediction for Cohort", municipality_code, sex, age_group),
      x = "Year",
      y = "Population",
      color = "Data Series"
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Base period" = "black",
        "True values" = "blue",
        "Prediction" = "red"
      )
    ) +
    theme_minimal()
}


############################################################################## #
# calculate_population_change
#
# Description
#   Compute the absolute first differences in a numeric vector, representing the
#   change in population between consecutive time points.
#
# Input
#   x
#     Numeric vector of population values.
#
# Output
#   A numeric vector where the first element is NA and each subsequent element is
#   the difference between the current and previous value.
############################################################################## #
calculate_population_change <- function(x) {
  c(NA, x[2:length(x)] - x[1:(length(x) - 1)])
}

############################################################################## #
# calculate_population_change_factor
#
# Description
#   Compute the relative change factors (ratios) between consecutive values in a
#   numeric vector, representing the multiplicative change from one period to the next.
#   Any zero values in the input are replaced with a small constant (eps) to avoid
#   division by zero or undefined ratios.
#
# Input
#   x
#     Numeric vector of population values.
#
#   eps
#     A small positive number used to replace zero values in the input vector
#     to ensure safe computation of ratios. Default is 0.001.
#
# Output
#   A numeric vector where the first element is NA and each subsequent element is the
#   ratio of the current (possibly adjusted) value to its preceding (possibly adjusted) value.
############################################################################## #

calculate_population_change_factor <- function(x, eps = 0.1) {
  # Replace zeros with eps
  x_safe <- ifelse(x == 0, eps, x)
  # Calculate population change factor
  c(NA, x_safe[2:length(x_safe)] / x_safe[1:(length(x_safe) - 1)])
}

############################################################################## #
# gm_mean
#
# Description
#   Calculate the geometric mean of a numeric vector.
#
# Input
#   x
#     Numeric vector for which the geometric mean is to be computed.
#   na.rm
#     Logical value indicating whether NA values should be removed (default is TRUE).
#
# Output
#   A numeric value representing the geometric mean of the input vector.
############################################################################## #
gm_mean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}



##############################################################################
# mean_absolute_error
#
# Description
#   Calculate the mean absolute error (MAE) between two numeric vectors.
#
# Input
#   actual
#     Numeric vector containing the observed (true) values.
#   predicted
#     Numeric vector containing the predicted values. This vector must have the same
#     length as 'actual'.
#   na.rm
#     Logical value indicating whether NA values should be removed (default is TRUE).
#
# Output
#   A numeric value representing the mean absolute error between the actual and predicted values.
##############################################################################
mean_absolute_error <- function(actual, predicted, na.rm = TRUE) {
  if (length(actual) != length(predicted)) {
    stop("The 'actual' and 'predicted' vectors must have the same length.")
  }
  mean(abs(actual - predicted), na.rm = na.rm)
}



#' Plot Train, Test, and Prediction Series from Vectors
#'
#' Constructs a combined data frame from three pairs of year/value vectors
#' (train, test, prediction) and produces a ggplot showing each series with
#' vertical lines marking the end of training and start of prediction periods.
#'
#' @param years_train Numeric vector of years in the training period.
#' @param pop_train   Numeric vector of values (e.g. population) for the training period.
#' @param years_test  Numeric vector of years in the test (true) period.
#' @param pop_test    Numeric vector of values for the test (true) period.
#' @param years_pred  Numeric vector of years in the prediction period.
#' @param pop_pred    Numeric vector of predicted values for the prediction period.
#' @param title       Character string for the plot title (default: `"Population Prediction"`).
#' @param xlab        Character string for the x-axis label (default: `"Year"`).
#' @param ylab        Character string for the y-axis label (default: `"Population"`).
#'
#' @details
#' - Checks that each years/_ and pop/_ vector pair are the same length.  
#' - Builds three internal data frames with a `series` factor for coloring.  
#' - Adds a dashed vertical line at \code{max(years_train)} and a dotted line at \code{min(years_pred)}.  
#' - Uses a minimal ggplot theme and manual color mapping:  
#'   - Train period → black  
#'   - Test (true) values → blue  
#'   - Prediction → red  
#'
#' @return
#' A \code{ggplot} object. You can further customize it by adding layers.
plot_prediction_from_vectors <- function(years_train,
                                         pop_train,
                                         years_test,
                                         pop_test,
                                         years_pred,
                                         pop_pred,
                                         title = "Population Prediction",
                                         xlab  = "Year",
                                         ylab  = "Population") {
  # sanity checks
  stopifnot(
    length(years_train) == length(pop_train),
    length(years_test)  == length(pop_test),
    length(years_pred)  == length(pop_pred)
  )
  
  # Compute connection point
  max_train_year <- max(as.numeric(years_train))
  last_train_val <- tail(as.numeric(pop_train), 1)
  
  # build each series
  df_train <- data.frame(
    year   = as.numeric(years_train),
    value  = as.numeric(pop_train),
    series = "Train period",
    stringsAsFactors = FALSE
  )
  df_test <- data.frame(
    year   = c(max_train_year, as.numeric(years_test)),
    value  = c(last_train_val, pop_test),
    series = "Test (true) values",
    stringsAsFactors = FALSE
  )
  df_pred <- data.frame(
    year   = c(max_train_year, as.numeric(years_pred)),
    value  = c(last_train_val, pop_pred),
    series = "Prediction",
    stringsAsFactors = FALSE
  )
  
  # combine
  df_all <- rbind(df_train, df_test, df_pred)
  
  # plot
  ggplot(df_all, aes(x = year, y = value, color = series)) +
    geom_line(size = 1) +
    geom_point() +
    geom_vline(xintercept = max_train_year, linetype = "dashed") +
    labs(
      title = title,
      x     = xlab,
      y     = ylab,
      color = NULL
    ) +
    scale_color_manual(values = c(
      "Train period"       = "black",
      "Test (true) values" = "blue",
      "Prediction"         = "red"
    )) +
    theme_minimal()
}
