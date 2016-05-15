# DataCollectorUtility.R

# TODO (data sets to load):
# - Get a global index benchmark data set such as MSCI ACWI and IWRD but assign UN country codes to benchmarks
# - Get more reliable source for index data constituients...
# - Get an automated OECD tax rates feed (wait for Quandl availability but implement OECD stats sourcing in the mean time)
# - Get an central bank target inflation rate feed, TBD aproximate using historic inflation rate
# - Get a feed of popular broker trading costs, ETFs for a start would be enough
# setwd("/home/rstudio") #TODO: uncomment for deployment
library("Quandl")
executionPath <- "/home/rstudio/RAutoInvest/"
source(paste(executionPath, ".pwd", sep=""))
Quandl.api_key(yourQuandlCode)
forceFullLoad <- FALSE
fullLoadDate <- "1980-01-01"
loadOK <- FALSE
data_storage_dir <- paste(executionPath,"INPUT", sep="")
myLog <- dget(paste(data_storage_dir,"/load.log",sep=""))
lastLoadDate <- as.Date(last(dget(paste(data_storage_dir,"/load.log", sep="")))[[1]])
start_date <- lastLoadDate-5 #Fetch more than than one day to ensure that the result isnot empty
currentDate <- as.Date(Sys.Date())
end_date <- currentDate
requestItemCounter <- 0
#
# # List of UN recognized countries (ex Kosovo due to uncertain status)
# CountryList <- read.csv(paste(data_storage_dir,"/CountryList.csv", sep=""))
# # List of EU members covered by EUROSTAT
# EUMemberList <- read.csv(paste(data_storage_dir,"/EUMemberList.csv", sep=""))
#
#
#
# sp1500_stock_ticker_universe <- read.csv(paste(executionPath,"INPUT/SP1500_Comp.csv", sep=""))
# sp1500_stock_ticker_universe <- sp1500_stock_ticker_universe$Symbol
#
#
# regionETF_ticker_universe <- c("EWA", "EWK", "EWZ", "EWC", "FXI", "ECH", "ICOL", "EWQ", "EWG",
#                "EWH", "INDA", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE",
#                "ERUS", "EWS", "EZA", "EWP", "EWD", "EWL", "THD", "TUR",
#                "EWU", "EUSA")
# # TODO: Create set of listing places per ETF
# regionETF_ticker_universe_names <- c("iShares Australia", "iShares Belgium", "iShares Brazil", "iShares Canada",
#                     "iShares China", "iShares Chile", "iShares Colombia", "iShares France", "iShares Germany",
#                     "iShares Hong Kong", "iShares India", "iShares Indonesia", "iShares Italy",
#                     "iShares Japan", "iShares Malaysia", "iShares Mexico", "iShares Netherlands", "iShares Philippines",
#                     "iShares Russia", "iShares Singapore","iShares South Africa", "iShares Spain", "iShares Sweden",
#                     "iShares Switzerland", "iShares Thailand", "iShares Turkey", "iShares UK", "iShares US")

sectorETF_ticker_universe <- c("IXG", "EXI", "FLM", "IGF", "GNR", "IXN", "JXI", "RXI", "KXI", "IXJ", "MXI", "IFGL", "IYR") # IFNA replaced by IYR
# Missing Sectors: Aerospace and Defense, Media, Retail, Travel-Leisure-Entertainment, Food and Beverage, Transportation
sectorETF_ticker_universe_names <- c("iShares Global Financials", "iShares Global Industrials", "First Trust Global Engineering and Construction",
                                     "iShares Global Infrastructure", "SPDR Global Natural Resources", "iShares Global Technology", "iShares Global Utilities",
                                     "iShares Global Consumer Discretionary", "iShares Global Consumer Staples", "iShares Global Healthcare",
                                     "iShares Global Materials", "iShares International Developed Real Estate exUS", "iShares North America Real Estate")
NYSE_listing <- c("EWA", "EWK", "EWZ", "EWC", "FXI", "ECH", "EWQ", "EWG", "EWH","EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE", "EWS", "EZA", "EWP", "EWD", "EWL", "THD", "TUR", "EWU", "EUSA", "IXG", "EXI", "FLM", "IGF", "GNR", "IXN", "JXI", "RXI", "KXI", "IXJ", "MXI", "IFGL", "IYR")
NYSEARCA_listing <- c("EWA", "EWK", "EWZ", "EWC", "FXI", "ECH", "ICOL", "EWQ", "EWG", "EWH", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE", "ERUS", "EWS", "EZA", "EWP", "EWD", "EWL", "THD", "TUR", "EWU", "EUSA", "IXG", "EXI", "FLM", "IGF", "GNR", "IXN", "JXI", "RXI", "KXI", "IXJ", "MXI")
# AMEX_listing <- c("EWA", "EWK", "EWZ", "EWC", "FXI", "ECH", "EWQ", "EWG", "EWH", "INDA", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE", "ERUS", "EWS", "EZA", "EWP", "EWD", "EWL", "THD", "TUR", "EWU", "EUSA", "IXG", "EXI", "FLM", "IGF", "GNR", "IXN", "JXI", "RXI", "KXI", "IXJ", "MXI")
#
# lifeExpectancy <- Quandl(paste("WORLDBANK/WLD_SP_DYN_LE00_IN", sep=""), type="zoo", authcode = yourQuandlCode)
# histInflationRate <- Quandl(paste("WORLDBANK/WLD_FP_CPI_TOTL_ZG", sep=""), type="zoo", authcode = yourQuandlCode)
# govBondYield <- Quandl(paste("UNDATA/IFS_YLD_EMU", sep=""), type="zoo", authcode = yourQuandlCode)
# savingRate <- Quandl(paste("EUROSTAT/TSDEC240_11", sep=""), type="zoo", authcode = yourQuandlCode)
# sp1500 <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "days"))
# regionETF <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "days"))
sectorETF <- zoo(c(), seq.Date(from = start_date, to = end_date, by = "days"))
# shsOut_sp1500 <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "quarters"))
# bookValue_sp1500 <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "quarters"))
# eps_sp1500 <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "quarters"))
# div_sp1500 <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "quarters"))
#
#
# # Fetch life expectancy data
# nameVect <- c()
# for(i in 1:nrow(CountryList)) {
#
#   lifeExpectancyCountry <- try(Quandl(paste("WORLDBANK/",c(as.character(CountryList[i,]$Country.Code)),"_SP_DYN_LE00_IN", sep=""), type="zoo",
#                                    authcode = yourQuandlCode))
#   if(class(lifeExpectancyCountry) != "try-error") {
#     cat(paste("Found life expectancy in WORLDBANK data for: ",CountryList[i,]$Country.Name,"\n",sep=""))
#   }
#   # Else set to world average
#   if(class(lifeExpectancyCountry) == "try-error") {
#     lifeExpectancyCountry <- try(Quandl(paste("WORLDBANK/WLD_SP_DYN_LE00_IN", sep=""), type="zoo",
#                                      authcode = yourQuandlCode))
#     cat(paste("Did NOT FIND life expectancy in WORLDBANK data for: ",CountryList[i,]$Country.Name," -> REPLACED with world average\n",sep=""))
#   }
#
#   lifeExpectancy <- merge.zoo(lifeExpectancy, lifeExpectancyCountry)
#   nameVect <- c(nameVect, as.character(CountryList[i,]$Country.Code))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(CountryList)) {
#     loadOK <- TRUE
#   }
# }
# lifeExpectancy <- lifeExpectancy[,2:ncol(lifeExpectancy)] # Since initiation has been on WLD data we need to remove that initial set before proceeding
# colnames(lifeExpectancy) <- c(nameVect)
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# dput(lifeExpectancy,paste(data_storage_dir,"/lifeExpectancy.data.R",sep="")) # Note that many NA still exist
# cat("Data storage: COMPLETELY SUCCEEDED,  existing data has been replaced")
#
# # Fetch household saving rate data
# nameVect <- c()
# for(i in 1:nrow(EUMemberList)) {
#
#   savingRateCountry <- try(Quandl(paste("EUROSTAT/TSDEC240_",c(as.character(EUMemberList[i,2])), sep=""), type="zoo",
#                                       authcode = yourQuandlCode))
#   if(class(savingRateCountry) != "try-error") {
#     cat(paste("Found savings rate in EUROSTAT data for: ",EUMemberList[i,3],"\n",sep=""))
#   }
#   # Else set to EU average
#   if(class(savingRateCountry) == "try-error") {
#     savingRateCountry <- try(Quandl(paste("EUROSTAT/TSDEC240_11", sep=""), type="zoo",
#                                         authcode = yourQuandlCode))
#     cat(paste("Did NOT FIND saving rate in EUROSTAT data for: ",EUMemberList[i,3]," -> REPLACED with EU average\n",sep=""))
#   }
#
#   savingRate <- merge.zoo(savingRate, savingRateCountry)
#   nameVect <- c(nameVect, as.character(EUMemberList[i,1]))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(EUMemberList)) {
#     loadOK <- TRUE
#   }
# }
# savingRate <- savingRate[,2:ncol(savingRate)] # Since initiation has been on WLD data we need to remove that initial set before proceeding
# colnames(savingRate) <- c(nameVect)
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# dput(savingRate,paste(data_storage_dir,"/savingRate.data.R",sep="")) # Note that many NA still exist
# cat("Data storage: COMPLETELY SUCCEEDED,  existing data has been replaced")
#
# # Fetch historic inflation rate data
# nameVect <- c()
# for(i in 1:nrow(CountryList)) {
#
#   histInflationRateCountry <- try(Quandl(paste("WORLDBANK/",c(as.character(CountryList[i,]$Country.Code)),"_FP_CPI_TOTL_ZG", sep=""), type="zoo",
#                                       authcode = yourQuandlCode))
#   if(class(histInflationRateCountry) != "try-error") {
#     cat(paste("Found historic inflation rate in WORLDBANK data for: ",CountryList[i,]$Country.Name,"\n",sep=""))
#   }
#   # Else set to world average
#   if(class(histInflationRateCountry) == "try-error") {
#     histInflationRateCountry <- try(Quandl(paste("WORLDBANK/WLD_FP_CPI_TOTL_ZG", sep=""), type="zoo",
#                                         authcode = yourQuandlCode))
#     cat(paste("Did NOT FIND historic inflation rate in WORLDBANK data for: ",CountryList[i,]$Country.Name," -> REPLACED with world average\n",sep=""))
#   }
#
#   histInflationRate <- merge.zoo(histInflationRate, lifeExpectancyCountry)
#   nameVect <- c(nameVect, as.character(CountryList[i,]$Country.Code))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(CountryList)) {
#     loadOK <- TRUE
#   }
# }
# histInflationRate <- histInflationRate[,2:ncol(histInflationRate)] # Since initiation has been on WLD data we need to remove that initial set before proceeding
# colnames(histInflationRate) <- c(nameVect)
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# dput(histInflationRate,paste(data_storage_dir,"/histInflationRate.data.R",sep="")) # Note that many NA still exist
# cat("Data storage: COMPLETELY SUCCEEDED,  existing data has been replaced")
#
# # Fetch government bond yield data
# nameVect <- c()
# for(i in 1:nrow(CountryList)) {
#
#   govBondYieldCountry <- try(Quandl(paste("UNDATA/IFS_YLD_",c(as.character(CountryList[i,]$Country.Code)), sep=""), type="zoo",
#                                          authcode = yourQuandlCode))
#   if(class(govBondYieldCountry) != "try-error") {
#     cat(paste("Found government bond yield in UNDATA data for: ",CountryList[i,]$Country.Name,"\n",sep=""))
#     govBondYield <- merge.zoo(govBondYield, govBondYieldCountry[,ncol(govBondYieldCountry)])
#     nameVect <- c(nameVect, as.character(CountryList[i,]$Country.Code))
#   }
#   # Else set to world average
#   if(class(govBondYieldCountry) == "try-error") {
# # #     govBondYieldCountry <- try(Quandl(paste("UNDATA/IFS_YLD_EMU", sep=""), type="zoo",
# # #                                            authcode = yourQuandlCode))
#     cat(paste("Did NOT FIND government bond yield in UNDATA data for: ",CountryList[i,]$Country.Name," -> EXCLUDE from results\n",sep=""))
#   }
#
# #   govBondYield <- merge.zoo(govBondYield, govBondYieldCountry)
# #   nameVect <- c(nameVect, as.character(CountryList[i,]$Country.Code))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
# #   if(requestItemCounter>500) {
# #     cat(paste("Start sleep for 200sec:",Sys.time()))
# #     Sys.sleep(200)
# #     requestItemCounter <- 0
# #   }
# #   if(i==length(CountryList)) {
# #     loadOK <- TRUE
# #   }
# }
# govBondYield <- govBondYield[,2:ncol(govBondYield)] # Since initiation has been on EMU data we need to remove that initial set before proceeding
# colnames(govBondYield) <- c(nameVect)
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# dput(govBondYield,paste(data_storage_dir,"/govBondYield.data.R",sep="")) # Note that many NA still exist
# cat("Data storage: COMPLETELY SUCCEEDED,  existing data has been replaced")
#
# # Fetch price data for sp1500
# nameVect <- c()
# for(i in 1:length(sp1500_stock_ticker_universe)) {
#   # Try loading NYSE, always try NYSE first, but TODO: check if this is a safe assumption
#   # TODO: Check why some legitimate symbols, e.g. ENDP, do not get loaded
#
#   stock_close <- try(Quandl(c(paste("GOOG/NYSE_",sp1500_stock_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
#                             start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#   if(class(stock_close) != "try-error") {
#     cat(paste("Found on NYSE: ","GOOG/NYSE_",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   # Else try with NASDAQ
#   if(class(stock_close) == "try-error") {
#     stock_close <- try(Quandl(c(paste("GOOG/NASDAQ_",sp1500_stock_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
#                               start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(stock_close) != "try-error") {
#       cat(paste("Found on NASDAQ: ", "GOOG/NASDAQ_",sp1500_stock_ticker_universe[i],"\n",sep=""))
#     }
#   }
#   # If still failed to retrieve any data NA the whole data set
#   if (class(stock_close) == "try-error") {
#     stock_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
#                        seq.Date(from = start_date, to = end_date,by = "days"))
#     cat(paste("NOT FOUND: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   sp1500 <- merge.zoo(sp1500, stock_close)
#   nameVect <- c(nameVect, as.character(sp1500_stock_ticker_universe[i]))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(sp1500_stock_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(sp1500) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/sp1500.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/sp1500.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with sp1500
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(sp1500[1]) >= index(old[1])) &&  (index(sp1500[nrow(sp1500)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(sp1500[nrow(sp1500)]) ) || ( index(old[nrow(old)]) < index(sp1500[1]) ) ) {
#     dput(rbind(sp1500,old),paste(data_storage_dir,"/sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(sp1500[1]) < index(old[1])) &&  (index(old[1]) <= index(sp1500[nrow(sp1500)])) )
#       ||
#         ( (index(sp1500[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(sp1500[nrow(sp1500)])) ) ) {
#     dput(rbind(sp1500[!(index(sp1500) %in% index(old)),],old),paste(data_storage_dir,"/sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(sp1500,paste(data_storage_dir,"/sp1500.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }
#
# # Fetch price data for region ETFs
# nameVect <- c()
# for(i in 1:length(regionETF_ticker_universe)) {
#   # Try loading NYSE, always try NYSE first
#   if(regionETF_ticker_universe[i] %in% NYSE_listing) {
#     etf_close <- try(Quandl(c(paste("GOOG/NYSE_",regionETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
#                             start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(etf_close) != "try-error") {
#       cat(paste("Found on NYSE: ","GOOG/NYSE_",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#     # If still failed to retrieve any data NA the whole data set
#     if (class(etf_close) == "try-error") {
#       etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
#                        seq.Date(from = start_date, to = end_date,by = "days"))
#       cat(paste("NOT FOUND: ",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#   }
#   # Only try ARCA if the symbol is not in NYSE set but can be found in ARCA listing set!
#   if(!(regionETF_ticker_universe[i] %in% NYSE_listing) && (regionETF_ticker_universe[i] %in% NYSEARCA_listing)) {
#     etf_close <- try(Quandl(c(paste("GOOG/NYSEARCA_",regionETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
#                             start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(etf_close) != "try-error") {
#       cat(paste("Found on ARCA: ","GOOG/NYSEARCA_",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#     # If still failed to retrieve any data NA the whole data set
#     if (class(etf_close) == "try-error") {
#       etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
#                        seq.Date(from = start_date, to = end_date,by = "days"))
#       cat(paste("NOT FOUND: ",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#   }
#   # Only try AMEX if the symbol is not in NYSE set but can be found in AMEX listing set!
#   if(!(regionETF_ticker_universe[i] %in% NYSE_listing) && (regionETF_ticker_universe[i] %in% AMEX_listing)) {
#     etf_close <- try(Quandl(c(paste("GOOG/AMEX_",regionETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
#                             start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(etf_close) != "try-error") {
#       cat(paste("Found on AMEX: ","GOOG/AMEX_",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#     # If still failed to retrieve any data NA the whole data set
#     if (class(etf_close) == "try-error") {
#       etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
#                        seq.Date(from = start_date, to = end_date,by = "days"))
#       cat(paste("NOT FOUND: ",regionETF_ticker_universe[i],"\n",sep=""))
#     }
#   }
#   regionETF <- merge.zoo(regionETF, etf_close)
#   nameVect <- c(nameVect, as.character(regionETF_ticker_universe[i]))
#
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(regionETF_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(regionETF) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/regionETF.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/regionETF.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with regionETF
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(regionETF[1]) >= index(old[1])) &&  (index(regionETF[nrow(regionETF)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(regionETF[nrow(regionETF)]) ) || ( index(old[nrow(old)]) < index(regionETF[1]) ) ) {
#     dput(rbind(regionETF,old),paste(data_storage_dir,"/regionETF.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(regionETF[1]) < index(old[1])) &&  (index(old[1]) <= index(regionETF[nrow(regionETF)])) )
#       ||
#         ( (index(regionETF[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(regionETF[nrow(regionETF)])) ) ) {
#     dput(rbind(regionETF[!(index(regionETF) %in% index(old)),],old),paste(data_storage_dir,"/regionETF.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(regionETF,paste(data_storage_dir,"/regionETF.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }

# Fetch price data for sector ETFs
nameVect <- c()
for(i in 1:length(sectorETF_ticker_universe)) {
  # Try loading NYSE, always try NYSE first
  if(sectorETF_ticker_universe[i] %in% NYSE_listing) {
    etf_close <- try(Quandl(c(paste("GOOG/NYSE_",sectorETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
                            start_date = start_date, end_date = end_date,authcode = yourQuandlCode)[,4])
    if(class(etf_close) != "try-error") {
      cat(paste("Found on NYSE: ","GOOG/NYSE_",sectorETF_ticker_universe[i],"\n",sep=""))
    }
    # If still failed to retrieve any data NA the whole data set
    if (class(etf_close) == "try-error") {
      etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
                       seq.Date(from = start_date, to = end_date,by = "days"))
      cat(paste("NOT FOUND: ",sectorETF_ticker_universe[i],"\n",sep=""))
    }
  }
  # Only try ARCA if the symbol is not in NYSE set but can be found in ARCA listing set!
  if(!(sectorETF_ticker_universe[i] %in% NYSE_listing) && (sectorETF_ticker_universe[i] %in% NYSEARCA_listing)) {
    etf_close <- try(Quandl(c(paste("GOOG/NYSEARCA_",sectorETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
                            start_date = start_date, end_date = end_date,authcode = yourQuandlCode)[,4])
    if(class(etf_close) != "try-error") {
      cat(paste("Found on ARCA: ","GOOG/NYSEARCA_",sectorETF_ticker_universe[i],"\n",sep=""))
    }
    # If still failed to retrieve any data NA the whole data set
    if (class(etf_close) == "try-error") {
      etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
                       seq.Date(from = start_date, to = end_date,by = "days"))
      cat(paste("NOT FOUND: ",sectorETF_ticker_universe[i],"\n",sep=""))
    }
  }
  # Only try AMEX if the symbol is not in NYSE set but can be found in AMEX listing set!
  if(!(sectorETF_ticker_universe[i] %in% NYSE_listing) && (sectorETF_ticker_universe[i] %in% AMEX_listing)) {
    etf_close <- try(Quandl(c(paste("GOOG/AMEX_",sectorETF_ticker_universe[i],sep="")), column=4, collapse="daily", type="zoo",
                            start_date = start_date, end_date = end_date,authcode = yourQuandlCode)[,4])
    if(class(etf_close) != "try-error") {
      cat(paste("Found on AMEX: ","GOOG/AMEX_",sectorETF_ticker_universe[i],"\n",sep=""))
    }
    # If still failed to retrieve any data NA the whole data set
    if (class(etf_close) == "try-error") {
      etf_close <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "days"))),
                       seq.Date(from = start_date, to = end_date,by = "days"))
      cat(paste("NOT FOUND: ",sectorETF_ticker_universe[i],"\n",sep=""))
    }
  }
  sectorETF <- merge.zoo(sectorETF, etf_close)
  nameVect <- c(nameVect, as.character(sectorETF_ticker_universe[i]))

  requestItemCounter <- requestItemCounter+1
  # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
  if(requestItemCounter>500) {
    cat(paste("Start sleep for 200sec:",Sys.time()))
    Sys.sleep(200)
    requestItemCounter <- 0
  }
  if(i==length(sectorETF_ticker_universe)) {
    loadOK <- TRUE
  }
}
colnames(sectorETF) <- nameVect
# TODO:re structure time range checks to avoid data loads for data that would never be merged
# Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
if(file.exists(paste(data_storage_dir,"/sectorETF.data.R",sep=""))) {
  old <- dget(paste(data_storage_dir,"/sectorETF.data.R",sep=""))
  #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with sectorETF
  # Do not merge if new data time range is already covered by existing data
  if( (index(sectorETF[1]) >= index(old[1])) &&  (index(sectorETF[nrow(sectorETF)]) <= index(old[nrow(old)])) ) {
    cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
  }
  # Merge if new data covers different time range than existing data
  if( (index(old[1]) > index(sectorETF[nrow(sectorETF)]) ) || ( index(old[nrow(old)]) < index(sectorETF[1]) ) ) {
    dput(rbind(sectorETF,old),paste(data_storage_dir,"/sectorETF.data.R",sep="")) # Note that many NA still exist
    cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
  }
  # Merge only new data if time ranges overlap
  if( ( (index(sectorETF[1]) < index(old[1])) &&  (index(old[1]) <= index(sectorETF[nrow(sectorETF)])) )
      ||
      ( (index(sectorETF[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(sectorETF[nrow(sectorETF)])) ) ) {
    dput(rbind(sectorETF[!(index(sectorETF) %in% index(old)),],old),paste(data_storage_dir,"/sectorETF.data.R",sep="")) # Note that many NA still exist
    cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
  }
} else {
  dput(sectorETF,paste(data_storage_dir,"/sectorETF.data.R",sep="")) # Note that many NA still exist
  cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
}
#
# # Fetch outstanding shares data for sp1500
# nameVect <- c()
# for(i in 1:length(sp1500_stock_ticker_universe)) {
#
#   # First try the diluted shares
#   shsOut <- try(Quandl(c(paste("SEC/",sp1500_stock_ticker_universe[i],"_WEIGHTEDAVERAGENUMBEROFDILUTEDSHARESOUTSTANDING_Q",sep="")), collapse="quarterly", type="zoo",
#                        start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#   if(class(shsOut) != "try-error") {
#     cat(paste("Found shsOut: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#
#   # Next, if there are no diluted shares out, try the basics
#   if(class(shsOut) == "try-error") {
#     shsOut <- try(Quandl(c(paste("SEC/",sp1500_stock_ticker_universe[i],"_WEIGHTEDAVERAGENUMBEROFSHARESOUTSTANDINGBASIC_Q",sep="")), collapse="quarterly", type="zoo",
#                          start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(shsOut) != "try-error") {
#       cat(paste("Found shsOut: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#     }
#   }
#   # If all failed to retrieve any data NA the whole data set
#   # TODO: maybe set an option to retrieve the latest in case of a very narrow time window chosen?
#   if (class(shsOut) == "try-error") {
#     shsOut <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "quarters"))),
#                   seq.Date(from = start_date, to = end_date,by = "quarters"))
#     cat(paste("NOT FOUND: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   index(shsOut) <- as.Date(index(shsOut))
#   shsOut_sp1500 <- merge.zoo(shsOut_sp1500, shsOut)
#   nameVect <- c(nameVect, as.character(sp1500_stock_ticker_universe[i]))
#
#
#   # This is continued from previous loop to ensure we start where we have left off before.
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(sp1500_stock_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(shsOut_sp1500) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/shsOut_sp1500.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/shsOut_sp1500.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with shsOut_sp1500
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(shsOut_sp1500[1]) >= index(old[1])) &&  (index(shsOut_sp1500[nrow(shsOut_sp1500)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(shsOut_sp1500[nrow(shsOut_sp1500)]) ) || ( index(old[nrow(old)]) < index(shsOut_sp1500[1]) ) ) {
#     dput(rbind(shsOut_sp1500,old),paste(data_storage_dir,"/shsOut_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(shsOut_sp1500[1]) < index(old[1])) &&  (index(old[1]) <= index(shsOut_sp1500[nrow(shsOut_sp1500)])) )
#       ||
#         ( (index(shsOut_sp1500[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(shsOut_sp1500[nrow(shsOut_sp1500)])) ) ) {
#     dput(rbind(shsOut_sp1500[!(index(shsOut_sp1500) %in% index(old)),],old),paste(data_storage_dir,"/shsOut_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(shsOut_sp1500,paste(data_storage_dir,"/shsOut_sp1500.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }
#
# # Fetch book value data for sp1500
# nameVect <- c()
# for(i in 1:length(sp1500_stock_ticker_universe)) {
#
#   # Try to get the data
#   bveq <- try(Quandl(c(paste("SEC/",sp1500_stock_ticker_universe[i],"_STOCKHOLDERSEQUITY_Q",sep="")), collapse="quarterly", type="zoo",
#                        start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#   if(class(bveq) != "try-error") {
#     cat(paste("Found bveq: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#
#   # If all failed to retrieve any data NA the whole data set
#   # TODO: maybe set an option to retrieve the latest in case of a very narrow time window chosen?
#   if (class(bveq) == "try-error") {
#     bveq <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "quarters"))),
#                   seq.Date(from = start_date, to = end_date,by = "quarters"))
#     cat(paste("NOT FOUND: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   index(bveq) <- as.Date(index(bveq))
#   bookValue_sp1500 <- merge.zoo(bookValue_sp1500, bveq)
#   nameVect <- c(nameVect, as.character(sp1500_stock_ticker_universe[i]))
#
#
#   # This is continued from previous loop to ensure we start where we have left off before.
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(sp1500_stock_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(bookValue_sp1500) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/bookValue_sp1500.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/bookValue_sp1500.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with bookValue_sp1500
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(bookValue_sp1500[1]) >= index(old[1])) &&  (index(bookValue_sp1500[nrow(bookValue_sp1500)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(bookValue_sp1500[nrow(bookValue_sp1500)]) ) || ( index(old[nrow(old)]) < index(bookValue_sp1500[1]) ) ) {
#     dput(rbind(bookValue_sp1500,old),paste(data_storage_dir,"/bookValue_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(bookValue_sp1500[1]) < index(old[1])) &&  (index(old[1]) <= index(bookValue_sp1500[nrow(bookValue_sp1500)])) )
#       ||
#         ( (index(bookValue_sp1500[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(bookValue_sp1500[nrow(bookValue_sp1500)])) ) ) {
#     dput(rbind(bookValue_sp1500[!(index(bookValue_sp1500) %in% index(old)),],old),paste(data_storage_dir,"/bookValue_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(bookValue_sp1500,paste(data_storage_dir,"/bookValue_sp1500.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }
#
# # Fetch earnings per share data sp1500
# nameVect <- c()
# for(i in 1:length(sp1500_stock_ticker_universe)) {
#
#   # Try to get the data
#   eps <- try(Quandl(c(paste("SEC/",sp1500_stock_ticker_universe[i],"_EARNINGSPERSHAREDILUTED_Q",sep="")), collapse="quarterly", type="zoo",
#                      start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#   if(class(eps) != "try-error") {
#     cat(paste("Found eps: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#
#   # Next, if there are no diluted eps, try the basics
#   if(class(shsOut) == "try-error") {
#     eps <- try(Quandl(c(paste("SEC/",sp1500_stock_ticker_universe[i],"_EARNINGSPERSHAREBASIC_Q",sep="")), collapse="quarterly", type="zoo",
#                       start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#     if(class(shsOut) != "try-error") {
#       cat(paste("Found shsOut: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#     }
#   }
#
#   # If all failed to retrieve any data NA the whole data set
#   # TODO: maybe set an option to retrieve the latest in case of a very narrow time window chosen?
#   if (class(eps) == "try-error") {
#     eps <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "quarters"))),
#                 seq.Date(from = start_date, to = end_date,by = "quarters"))
#     cat(paste("NOT FOUND: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   index(eps) <- as.Date(index(eps))
#   eps_sp1500 <- merge.zoo(eps_sp1500, eps)
#   nameVect <- c(nameVect, as.character(sp1500_stock_ticker_universe[i]))
#
#
#   # This is continued from previous loop to ensure we start where we have left off before.
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(sp1500_stock_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(eps_sp1500) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/eps_sp1500.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/eps_sp1500.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with eps_sp1500
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(eps_sp1500[1]) >= index(old[1])) &&  (index(eps_sp1500[nrow(eps_sp1500)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(eps_sp1500[nrow(eps_sp1500)]) ) || ( index(old[nrow(old)]) < index(eps_sp1500[1]) ) ) {
#     dput(rbind(eps_sp1500,old),paste(data_storage_dir,"/eps_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(eps_sp1500[1]) < index(old[1])) &&  (index(old[1]) <= index(eps_sp1500[nrow(eps_sp1500)])) )
#       ||
#         ( (index(eps_sp1500[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(eps_sp1500[nrow(eps_sp1500)])) ) ) {
#     dput(rbind(eps_sp1500[!(index(eps_sp1500) %in% index(old)),],old),paste(data_storage_dir,"/eps_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(eps_sp1500,paste(data_storage_dir,"/eps_sp1500.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }
#
# # Fetch dividends data sp1500
# nameVect <- c()
# for(i in 1:length(sp1500_stock_ticker_universe)) {
#
#   # Try to get the data
#   div <- try(Quandl(c(paste("SEC/DIV_",sp1500_stock_ticker_universe[i],sep="")), collapse="quarterly", type="zoo",
#                     start_date = start_date, end_date = end_date,authcode = yourQuandlCode))
#   if(class(div) != "try-error") {
#     cat(paste("Found div: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#
#   # If all failed to retrieve any data NA the whole data set
#   # TODO: maybe set an option to retrieve the latest in case of a very narrow time window chosen?
#   if (class(div) == "try-error") {
#     div <- zoo(rep(NA,length(seq.Date(from = start_date, to = end_date,by = "quarters"))),
#                seq.Date(from = start_date, to = end_date,by = "quarters"))
#     cat(paste("NOT FOUND: ",sp1500_stock_ticker_universe[i],"\n",sep=""))
#   }
#   index(div) <- as.Date(index(div))
#   div_sp1500 <- merge.zoo(div_sp1500, div)
#   nameVect <- c(nameVect, as.character(sp1500_stock_ticker_universe[i]))
#
#
#   # This is continued from previous loop to ensure we start where we have left off before.
#   requestItemCounter <- requestItemCounter+1
#   # Sleep 6min when 1000 items have been requested in order to not breach Quandl load limit
#   if(requestItemCounter>500) {
#     cat(paste("Start sleep for 200sec:",Sys.time()))
#     Sys.sleep(200)
#     requestItemCounter <- 0
#   }
#   if(i==length(sp1500_stock_ticker_universe)) {
#     loadOK <- TRUE
#   }
# }
# colnames(div_sp1500) <- nameVect
# # TODO:re structure time range checks to avoid data loads for data that would never be merged
# # Note that the merge opertaion is restricted to 2^31 rows, this should work for daily data collection for 0.5m years, but may break 100 years if moving to seconds
# # Also not that the code assumes only continuous time series series are used, inserting dates inbetween dates will fail as the code only looks at start and end elements!
# if(file.exists(paste(data_storage_dir,"/div_sp1500.data.R",sep=""))) {
#   old <- dget(paste(data_storage_dir,"/div_sp1500.data.R",sep=""))
#   #testSet <- window(old,start = "2014-01-01", end="2014-09-01") # TODO: remove and replace testSet with div_sp1500
#   # Do not merge if new data time range is already covered by existing data
#   if( (index(div_sp1500[1]) >= index(old[1])) &&  (index(div_sp1500[nrow(div_sp1500)]) <= index(old[nrow(old)])) ) {
#     cat("Data storage: COMPLETELY FAILED, new data time range is already covered by existing data")
#   }
#   # Merge if new data covers different time range than existing data
#   if( (index(old[1]) > index(div_sp1500[nrow(div_sp1500)]) ) || ( index(old[nrow(old)]) < index(div_sp1500[1]) ) ) {
#     dput(rbind(div_sp1500,old),paste(data_storage_dir,"/div_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: COMPLETELY SUCCEEDED, new data covers different time range than existing data")
#   }
#   # Merge only new data if time ranges overlap
#   if( ( (index(div_sp1500[1]) < index(old[1])) &&  (index(old[1]) <= index(div_sp1500[nrow(div_sp1500)])) )
#       ||
#         ( (index(div_sp1500[1]) <=  index(old[nrow(old)])) &&  (index(old[nrow(old)]) < index(div_sp1500[nrow(div_sp1500)])) ) ) {
#     dput(rbind(div_sp1500[!(index(div_sp1500) %in% index(old)),],old),paste(data_storage_dir,"/div_sp1500.data.R",sep="")) # Note that many NA still exist
#     cat("Data storage: PARTIALLY SUCCEEDED, new data merged with old data, new overlapping data skipped")
#   }
# } else {
#   dput(div_sp1500,paste(data_storage_dir,"/div_sp1500.data.R",sep="")) # Note that many NA still exist
#   cat("Data storage: COMPLETELY SUCCEEDED, no existing data has been found")
# }

if(loadOK) {
  myLog[[length(myLog)+1]] <- as.Date(Sys.Date())
  dput(myLog,paste(data_storage_dir,"/load.log",sep="")) # TODO: on deployment activate logging
}
