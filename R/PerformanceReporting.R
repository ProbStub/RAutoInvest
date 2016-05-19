library(PerformanceAnalytics)
executionPath <- "/home/rstudio/RAutoInvest/"
source(paste(executionPath, ".pwd", sep=""))

# TODO: Compute proper ACCOUNT returns, since the current retuns are pure portfolio returns, disregarding cash-flow addition/withdrawal

DailyPortfolioWeightsPast <- function (userId) {

  userID <- userId

  investmentHorizons <- dget(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))

  dailyAssetWeightFiles <- list.files(paste(executionPath,"/OUTPUT/", sep=""),
                                      pattern=paste(userID,"_",
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestStrategy,
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestUniverse,
                                                    "_backtest_weights*", sep=""))
  # TODO: more robust number of assets calculation (there might be more or lesse depending on the date and universe composition changes)
  lastFile <- read.csv(paste(executionPath, "OUTPUT/", last(dailyAssetWeightFiles),sep=""))
  numAssets <- ncol(lastFile)-1
  # TODO: Better accounting for switched, removed and additional assets
  nameAssets <- colnames(lastFile[,c(-1)])
  dailyAssetDates <- as.Date(substr(dailyAssetWeightFiles,start=nchar(dailyAssetWeightFiles[1])-22,stop=nchar(dailyAssetWeightFiles[1])-13))
  dailyAssetWeights <- matrix(nrow=length(dailyAssetWeightFiles),ncol=numAssets)
  startWeights <- zoo(matrix(rep(0, numAssets),nrow=1, ncol=numAssets))

  # Determine the last trading day before the first day investments are made
  prevTradeDate <- 1
  while(!isBizday(timeDate(first(dailyAssetDates)-prevTradeDate))) {
    prevTradeDate <- prevTradeDate+1
  }
  index(startWeights) <- first(dailyAssetDates)-prevTradeDate

  for (i in 1:length(dailyAssetWeightFiles)) {
    inputData <- read.csv(paste(executionPath, "OUTPUT/", dailyAssetWeightFiles[i],sep=""))
    dailyAssetWeights[i,] <- as.matrix(last(inputData[,c(-1)]))
  }

  colnames(dailyAssetWeights) <- nameAssets
  dailyAssetWeights <- zoo(dailyAssetWeights)
  index(dailyAssetWeights) <- dailyAssetDates

  # Note: Asset weights are beginning of day target weights, to capture the same days returns, adding a zero line for the day before account start
  dailyAssetWeights <- rbind(dailyAssetWeights,startWeights)
  dailyAssetWeights <- dailyAssetWeights[isBizday(timeDate(index(dailyAssetWeights))),]

  if(isBizday(timeDate(Sys.Date()))) {
    dailyAssetWeights <- dailyAssetWeights[c(-nrow(dailyAssetWeights)),]
  }

  return(dailyAssetWeights)
}

DailyPortfolioWeightsCurrent <- function (userId) {

  userID <- userId

  investmentHorizons <- dget(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))

  dailyAssetWeightFiles <- list.files(paste(executionPath,"/OUTPUT/", sep=""),
                                      pattern=paste(userID,"_",
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestStrategy,
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestUniverse,
                                                    "_backtest_weights*", sep=""))
  # TODO: more robust number of assets calculation (there might be more or lesse depending on the date and universe composition changes)
  lastFile <- read.csv(paste(executionPath, "OUTPUT/", last(dailyAssetWeightFiles),sep=""))
  numAssets <- ncol(lastFile)-1
  # TODO: Better accounting for switched, removed and additional assets
  nameAssets <- colnames(lastFile[,c(-1)])
  dailyAssetDates <- as.Date(substr(dailyAssetWeightFiles,start=nchar(dailyAssetWeightFiles[1])-22,stop=nchar(dailyAssetWeightFiles[1])-13))
  dailyAssetWeights <- matrix(nrow=length(dailyAssetWeightFiles),ncol=numAssets)
  startWeights <- zoo(matrix(rep(0, numAssets),nrow=1, ncol=numAssets))

  # Determine the last trading day before the first day investments are made
  prevTradeDate <- 1
  while(!isBizday(timeDate(first(dailyAssetDates)-prevTradeDate))) {
    prevTradeDate <- prevTradeDate+1
  }
  index(startWeights) <- first(dailyAssetDates)-prevTradeDate

  for (i in 1:length(dailyAssetWeightFiles)) {
    inputData <- read.csv(paste(executionPath, "OUTPUT/", dailyAssetWeightFiles[i],sep=""))
    dailyAssetWeights[i,] <- as.matrix(last(inputData[,c(-1)]))
  }

  colnames(dailyAssetWeights) <- nameAssets
  dailyAssetWeights <- zoo(dailyAssetWeights)
  index(dailyAssetWeights) <- dailyAssetDates

  # Note: Asset weights are beginning of day target weights, to capture the same days returns, adding a zero line for the day before account start
  dailyAssetWeights <- rbind(dailyAssetWeights,startWeights)
  if(isBizday(timeDate(Sys.Date()))) {
    dailyAssetWeights <- dailyAssetWeights[isBizday(timeDate(index(dailyAssetWeights))),]
  }

  return(dailyAssetWeights[c(nrow(dailyAssetWeights)),])
}

DailyPortfolioReturnsPast <- function(userId) {
  userID <- userId

  investmentHorizons <- dget(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))

  dailyAssetWeightFiles <- list.files(paste(executionPath,"/OUTPUT/", sep=""),
                                      pattern=paste(userID,"_",
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestStrategy,
                                                    investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestUniverse,
                                                    "_backtest_weights*", sep=""))
  # TODO: more robust number of assets calculation (there might be more or lesse depending on the date and universe composition changes)
  lastFile <- read.csv(paste(executionPath, "OUTPUT/", last(dailyAssetWeightFiles),sep=""))
  numAssets <- ncol(lastFile)-1
  # TODO: Better accounting for switched, removed and additional assets
  nameAssets <- colnames(lastFile[,c(-1)])
  dailyAssetWeights <- DailyPortfolioWeightsPast(userID)
  dailyAssetDates <- index(dailyAssetWeights)
  # Fetch price data for sector ETFs
  nameVect <- c()
  requestItemCounter <- 0
  sectorETF_ticker_universe <- nameAssets
  NYSE_listing <- nameAssets
  start_date <- first(dailyAssetDates)-5 #get more days seeding missing value replace & data load error of narrow time-window
  end_date <- last(dailyAssetDates)
  sectorETF <- zoo(c(), seq.Date(from = start_date, to = end_date,by = "days"))

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
  sectorETF <- sectorETF[isBizday(timeDate(index(sectorETF))),] # Take only business days
  sectorETFPrice <- na.fill(sectorETF,0)
  # Select only a window for which the client is invested and removing current day price as it might be guessed only
  sectorETFPrice <- window(sectorETFPrice, start=first(dailyAssetDates), end=last(dailyAssetDates))
  sectorETFReturns <- CalculateReturns(sectorETFPrice)
  sectorETFReturns <- na.fill(sectorETFReturns,0)
  positionReturns <- zoo(sectorETFReturns*dailyAssetWeights)
  portfolioReturns <- zoo(rowSums(sectorETFReturns*dailyAssetWeights))
  index(portfolioReturns) <- index(sectorETFPrice)
  portfolioReturns <- cbind(portfolioReturns,positionReturns)

  return(portfolioReturns)
}

DailyBechmarkReturnsPast <- function(bmTicker, bmTradingVenue, userId) {

  userID <- userId
  quandlCode <- paste("GOOG/",bmTradingVenue,"_", bmTicker, sep="")
  dailyAssetDates <- index(DailyPortfolioWeightsPast(userID))

  # Get benchmark price data
  benchmarkPrice <- Quandl(quandlCode, collapse="daily",type="zoo", start_date = first(dailyAssetDates), end_date = last(dailyAssetDates), authcode = yourQuandlCode)[,4]
  benchmarkPrice <- benchmarkPrice[isBizday(timeDate(index(benchmarkPrice))),]
  benchmarkReturns <- CalculateReturns(benchmarkPrice)
  benchmarkReturns <- na.fill(benchmarkReturns,0)
  return(benchmarkReturns)
}

CumPortfolioReturnsPast <- function(userId) {

  portfolioReturns <- DailyPortfolioReturnsPast(userId)

  # Compute cumulative returns
  cumPortfolioReturns <- sapply(seq(1,ncol(portfolioReturns),1), function(x) cumprod(portfolioReturns[,x] + 1) - 1)
  colnames(cumPortfolioReturns) <- colnames(portfolioReturns)
  cumPortfolioReturns <- zoo(cumPortfolioReturns, index(portfolioReturns))
  return(cumPortfolioReturns)
}

CumBechmarkReturnsPast <- function(bmTicker, bmTradingVenue, userId) {

  benchmarkReturns <- DailyBechmarkReturnsPast(bmTicker, bmTradingVenue, userId)

  # Compute cumulative returns
  cumBechmarkReturns <- cumprod(benchmarkReturns + 1) - 1
  return(cumBechmarkReturns)
}





