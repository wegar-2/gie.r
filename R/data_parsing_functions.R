
dtParseGasData <- function(dtData) {
  # 1. drop info column
  dtData$info <- NULL
  # 2. rename columns
  data.table::setnames(x = dtData,
                       old = c("status", "gasDayStartedOn", "gasInStorage",
                               "full",
                               "injection", "withdrawal",
                               "workingGasVolume",
                               "injectionCapacity", "withdrawalCapacity"),
                       new = c("data_status", "gas_day_started_on", "gas_in_storage_TWh",
                               "fullness_pctg",
                               "realised_daily_injection_GWh", "realised_daily_withdrawal_GWh",
                               "storage_capacity_TWh",
                               "daily_injection_capacity_GWh", "daily_withdrawal_capacity_GHh"))
  # 3. cast gas_day_started_on as Date class
  dtData$gas_day_started_on <- as.Date(dtData$gas_day_started_on, format = "%Y-%m-%d")
  # 4. cast data_status column to full names
  dtData$data_status <- sapply(X = dtData$data_status, FUN = function(x, lDictDataStatus) {
    lDictDataStatus[[x]]
  }, lDictDataStatus = lDictDataStatus) %>% unname()

  return(dtData)
}



dtParseLngData <- function(dtData) {

  # 1. drop info column
  dtData$info <- NULL
  # 2. rename columns
  data.table::setnames(x = dtData,
                       old = c("status", "gasDayStartedOn",
                               "lngInventory",
                               "sendOut",
                               "dtmi", "dtrs"),
                       new = c("data_status", "gas_day_started_on",
                               "lng_inventory_day_start_1000m3",
                               "lng_daily_sendout_GWh",
                               "declared_total_maximum_inventory_1000m3",
                               "declared_total_reference_daily_sendout_GWh"))
  # 3. cast gas_day_started_on as Date class
  dtData$gas_day_started_on <- as.Date(dtData$gas_day_started_on, format = "%Y-%m-%d")
  # 4. cast data_status column to full names
  dtData$data_status <- sapply(X = dtData$data_status, FUN = function(x, lDictDataStatus) {
    lDictDataStatus[[x]]
  }, lDictDataStatus = lDictDataStatus) %>% unname()

  return(dtData)
}
