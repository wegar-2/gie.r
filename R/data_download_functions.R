library(data.table)
library(jsonlite)
library(assertive)
library(magrittr)
source("R/data_parsing_functions.R")


dtFetchData <- function(cCountryCode, cCompanyEic = NULL, cFacilityEic = NULL,
                        dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "gas") {

  # 1. paramaters validation ---------------------------------------------------
  # 1.1. cCountryCode
  assertive::is_character(x = cCountryCode)
  # 1.2. cCompanyEic
  if (!is.null(cCompanyEic)) {
    assertive::is_character(x = cCompanyEic)
  }
  # 1.3. cFacilityEic
  if (!is.null(cFacilityEic)) {
    assertive::is_character(x = cFacilityEic)
  }
  # 1.4. dateStartDate
  if (!is.null(dateStartDate)) {
    assertive::is_date(x = dateStartDate)
  }
  # 1.5. dateEndDate
  if (!is.null(dateEndDate)) {
    assertive::is_date(x = dateEndDate)
  }
  # 1.6. cGasOrLng
  assertive::is_character(cGasOrLng)
  cGasOrLng <- match.arg(arg = cGasOrLng, choices = c("gas", "lng"),
                         several.ok = FALSE)

  # 2. fork data download depending on the provided cCountryCode ---------------
  # 2.1. cCoreUrl
  if (cGasOrLng == "gas") {
    cCoreUrl <- "https://agsi.gie.eu/api/data/"
  } else {
    cCoreUrl <- "https://alsi.gie.eu/api/data/"
  }
  # 2.2. prepare targer cUrl
  if (!is.null(cCountryCode) & is.null(cCompanyEic) & is.null(cFacilityEic)) {
    message("Fetching country-level data...")
    cUrl <- paste0(cCoreUrl, cCountryCode)
  } else if (!is.null(cCountryCode) & !is.null(cCompanyEic) & is.null(cFacilityEic)) {
    message("Fetching (country+company)-level data...")
    cUrl <- paste0(cCoreUrl, cCompanyEic, "/", cCountryCode)
  } else if (!is.null(cCountryCode) & !is.null(cCompanyEic) & !is.null(cFacilityEic)) {
    message("Fetching (country+company)-level data...")
    cUrl <- paste0(cCoreUrl, cFacilityEic, "/", cCountryCode, "/", cCompanyEic)
  } else {
    stop("This case is not handled! ")
  }

  # 3. add clause limiting the time span ---------------------------------------
  if (!is.null(dateStartDate) | !is.null(dateEndDate)) {
    # 3.1. dateStartDate only
    if (!is.null(dateStartDate) & is.null(dateEndDate)) {
      cUrlTimeSpanClause <- paste0("?from=", format(x = dateStartDate, format = "%Y-%m-%d"))
    }
    # 3.2. dateEndDate only
    if (is.null(dateStartDate) & !is.null(dateEndDate)) {
      cUrlTimeSpanClause <- paste0("?till=", format(x = dateEndDate, format = "%Y-%m-%d"))
    }
    # 3.3. both dateStartDate and dateEndDate
    if (!is.null(dateStartDate) & !is.null(dateEndDate)) {
      cUrlTimeSpanClause <- paste0("?from=", format(x = dateStartDate, format = "%Y-%m-%d"),
                                   "&till=", format(x = dateEndDate, format = "%Y-%m-%d"))
    }
    cUrl <- paste0(cUrl, "/", cUrlTimeSpanClause)
  }

  # 4. download the data -------------------------------------------------------
  message("Fetching GIE data from URL: ", cUrl)
  # 4.1. download the data
  res <- try(expr = {
    jsonlite::read_json(path = cUrl, simplifyVector = T)
  }, silent = TRUE)
  # 4.2. handle error
  if (methods::is(object = res, class2 = "try-error")) {
    stop("Failed to fetch data from URL: ", cUrl, "; returning NULL. ")
    return(NULL)
  }
  # 4.3. check if the number of rows is larger than zero
  if (length(res) == 0L) {
    warning("Empty data set has been returned from the URL: ", cUrl, "; returning NULL. ")
    return(NULL)
  }

  # 5. cast as data.table and return -------------------------------------------
  dtRes <- data.table::as.data.table(x = res)

  return(dtRes)
}



dtFetchCountryData <- function(cCountryCode, dateStartDate = NULL, dateEndDate = NULL,
                               cGasOrLng = "gas") {

  # 1. call dtFetchData to get country-level data ------------------------------
  dtData <- try(expr = {
    dtFetchData(cCountryCode = cCountryCode, dateStartDate = dateStartDate, dateEndDate = dateEndDate,
                cGasOrLng = cGasOrLng)
  }, silent = TRUE)

  # 2. parse the downloaded data.table -----------------------------------------
  if (cGasOrLng == "gas") {
    dtData <- dtParseGasData(dtData = dtData)
  } else {
    dtData <- dtParseLngData(dtData = dtData)
  }
  return(dtData)
}



dtFetchCompanyData <- function(cCountryCode, cCompanyEic,
                               dateStartDate = NULL, dateEndDate = NULL,
                               cGasOrLng = "gas") {

  # 1. call dtFetchData to get company-level data ------------------------------
  dtData <- try(expr = {
    dtFetchData(cCountryCode = cCountryCode, cCompanyEic = cCompanyEic,
                dateStartDate = dateStartDate, dateEndDate = dateEndDate,
                cGasOrLng = cGasOrLng)
  }, silent = TRUE)

  # 2. parse the downloaded data.table -----------------------------------------
  if (cGasOrLng == "gas") {
    dtData <- dtParseGasData(dtData = dtData)
  } else {
    dtData <- dtParseLngData(dtData = dtData)
  }
  return(dtData)
}



dtFetchFacilityData <- function(cCountryCode, cCompanyEic, cFacilityEic,
                               dateStartDate = NULL, dateEndDate = NULL,
                               cGasOrLng = "gas") {

  # 1. call dtFetchData to get facility-level data -----------------------------
  dtData <- try(expr = {
    dtFetchData(cCountryCode = cCountryCode, cCompanyEic = cCompanyEic,
                cFacilityEic = cFacilityEic,
                dateStartDate = dateStartDate, dateEndDate = dateEndDate,
                cGasOrLng = cGasOrLng)
  }, silent = TRUE)

  # 2. parse the downloaded data.table -----------------------------------------
  if (cGasOrLng == "gas") {
    dtData <- dtParseGasData(dtData = dtData)
  } else {
    dtData <- dtParseLngData(dtData = dtData)
  }
  return(dtData)
}


