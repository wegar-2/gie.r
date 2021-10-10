


# ==============================================================================
# ==================== TESTING dtFetchCountryData FUNCTION  ====================
# ==============================================================================

testthat::test_that(desc = "testing dtFetchData function: fetching country-level data, GAS...", code = {
  # 1. fetch gas, unlimited data
  dtTest <- dtFetchCountryData(
    cCountryCode = "DE", dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch gas, start date only
  dtTest <- dtFetchCountryData(
    cCountryCode = "DE", dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch gas, end date only
  dtTest <- dtFetchCountryData(
    cCountryCode = "DE", dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch gas, start and end dates
  dtTest <- dtFetchCountryData(
    cCountryCode = "DE", dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})


testthat::test_that(desc = "testing dtFetchData function: fetching country-level data, LNG...", code = {
  # 1. fetch LNG, unlimited data
  dtTest <- dtFetchCountryData(
    cCountryCode = "PL", dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch LNG, start date only
  dtTest <- dtFetchCountryData(
    cCountryCode = "PL", dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch LNG, end date only
  dtTest <- dtFetchCountryData(
    cCountryCode = "PL", dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch LNG, start and end dates
  dtTest <- dtFetchCountryData(
    cCountryCode = "PL", dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})





# ==============================================================================
# =================== TESTING dtFetchCompanyData FUNCTION  =====================
# ==============================================================================

testthat::test_that(desc = "testing dtFetchCompanyData function: fetching company-level data, GAS...", code = {
  # 1. fetch gas, unlimited data
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch gas, start date only
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch gas, end date only
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch gas, start and end dates
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})


testthat::test_that(desc = "testing dtFetchCompanyData function: fetching company-level data, LNG...", code = {
  # 1. fetch LNG, unlimited data
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch LNG, start date only
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch LNG, end date only
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch LNG, start and end dates
  dtTest <- dtFetchCompanyData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})




# ==============================================================================
# =================== TESTING dtFetchFacilityData FUNCTION  ====================
# ==============================================================================

testthat::test_that(desc = "testing dtFetchFacilityData function: fetching facility-level data, GAS...", code = {
  # 1. fetch gas, unlimited data
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    cFacilityEic = "21Z000000000381H",
    dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch gas, start date only
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    cFacilityEic = "21Z000000000381H",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch gas, end date only
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    cFacilityEic = "21Z000000000381H",
    dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch gas, start and end dates
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "53XPL000000OSMP5",
    cFacilityEic = "21Z000000000381H",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "gas")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})


testthat::test_that(desc = "testing dtFetchFacilityData function: fetching facility-level data, LNG...", code = {
  # 1. fetch LNG, unlimited data
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    cFacilityEic = "21W000000000096L",
    dateStartDate = NULL, dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 2. fetch LNG, start date only
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    cFacilityEic = "21W000000000096L",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = NULL, cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 3. fetch LNG, end date only
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    cFacilityEic = "21W000000000096L",
    dateStartDate = NULL, dateEndDate = as.Date("2021-01-01"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
  # 4. fetch LNG, start and end dates
  dtTest <- dtFetchFacilityData(
    cCountryCode = "PL", cCompanyEic = "21X-PL-A-A0A0A-B",
    cFacilityEic = "21W000000000096L",
    dateStartDate = as.Date("2021-01-01"), dateEndDate = as.Date("2021-06-30"), cGasOrLng = "lng")
  testthat::expect_type(object = dtTest, type = "list")
  testthat::expect_true(object = data.table::is.data.table(dtTest))
})

