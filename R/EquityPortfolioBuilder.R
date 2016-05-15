# Load RMetrics tools
library(fPortfolio) # Varios portfolio management tools
library(urca) # primarily for the VECM model
library(vars) # Also required for the VECM models
library(zoo) # Time series package
library(BLCOP) # Black-Litterman and other tools
library(RCurl) # Required for merge of list object to work properly.
library(PerformanceAnalytics) # Required for return estimation and calculation
library(Quandl) # Required to load source data

file_root_dir = ".."

source("/home/rstudio/RAutoInvest/.pwd")
Quandl.api_key(yourQuandlCode)

ResidualIncomeValue<- function(symbol)
{

  # Deprecated
}

## Function to calculate return expectations from market prices
# One period ahead forecast
StatisticalVECMReturnEstimation <- function(x, ci, monthlyAssetPrices, assetNames, assetNumber)
{
  data <- window(monthlyAssetPrices, start = start(monthlyAssetPrices), end = x)
  Lobs <- t(tail(data, 1))
  vec <- ca.jo(data, ecdet = "none", spec = "transitory")
  m <- vec2var(vec, r = 1)
  fcst <- predict(m, n.ahead = 1, ci = ci)
  LU <- matrix(unlist(fcst$fcst),
               ncol = 4, byrow = TRUE)[, c(2, 3)]
  RE <- rep(0, assetNumber)
  PView <- LU[, 1] > Lobs
  NView <- LU[, 2] < Lobs
  RE[PView] <- (LU[PView, 1] / Lobs[PView, 1] - 1)
  RE[NView] <- (LU[NView, 1] / Lobs[NView, 1] - 1)
  names(RE) <- assetNames
  return(RE)
}

## Function to calculate return expectations from accounting data with market inputs
# Frequency: Annual
# Interpolation: Spline interpolation for missing values, since at any given moment some stocks may not have provided annual figures yet
# Valuation period assumption: 10 years
# Growth rate assumption: multi-stage linear decreasing current return-to-equity to required return
# Accounting assumption: Clean surplus accounting assumption and no adjustments for securties holdings and off-balance-sheet assets
# this could be done either with EPS or NI but EPS will be used since it is more likely to be available <- TODO: validate availability claim
#   # CFA Program Curruculum V4, L2, 2010, p591-592 Test case:
#   epsGrowthRateMatrix <- cbind(c(0.2173, 0.2665, rep(0.25,8)), c(0.2173, 0.2665, rep(0.25,8)))
#   requiredReturns <- c(0.15, 0.15)
#   payRate <- c(0.30, 0.30)
#   bookValueProjectedMatrix[1,] <- c(19.5950)
#   bookValueProjectedMatrix[2,] <- c(20.8460)
#   bookValueProjectedMatrix[3,] <- c(23.1180)
#   epsProjectedMatrix[1,] <- c(0)
#   epsProjectedMatrix[2,] <- c(4.2560)
#   epsProjectedMatrix[3,] <- c(5.5560)
#   residualIncomeProjectedMatrix[1,] <- c(0)
#   residualIncomeProjectedMatrix[2,] <- c(1.3175)
#   residualIncomeProjectedMatrix[3,] <- c(2.4291)
#   residualIncomeProjectedPVMatrix[1,] <- c(0)
#   residualIncomeProjectedPVMatrix[2,] <- c(1.15)
#   residualIncomeProjectedPVMatrix[3,] <- c(1.84)
#   # Inside for loop set: i<- i+2 # For testing CFA example only!
# TODO:
# - URGENT: define a list object of length observation and each element of length of assets to store the expected returns
# - Return a list object with each list item being a 10y forecast starting at that date
# - produce forecasts 10 years out from each observation
# - make input values agnostic to period choice (i.e., allow for weekly or quarterly data)
# - adjust methdo signature for annualRfr and monthlyShSOut ... instead of annualShSOut...
# - make sure that all figures are always with the right periodicity (monthly/annual) especially the return figures!!!!!!
# - Retest with CFA example, take care anch check all the anualizatisn are taken care off!!!
#
# - Validate sound es of setting ROE,payRate, requiredReturn to zeros... possibly a shift down to the earlies available data is better, but would need to happen before function is call!
# - current betas/required-returns are calculated from first date subsequently for each monthly until the last date, replace with a 12 month roling window instead
# - replace the last(col(<something>)) with seq(1,ncol(<something>),1) or so to avoid potentially large matrix stuff
# - make forecast range flexible
# - evaluate whether negative required returns should be permissable
# - allow scenarios for required returns
# - Rewrite at least the first/outer for loop for lapply() used and then run through mclapply() or something
# - use more resonable NA replacement the the current spline interpolation since that may
#   cause serious return volatility once late company reports are finally filed and returns jub up/down
FundamentalResidualIncomeReturnEstimation <- function(rfr, monthlyBenchmarkPrices, monthlyAssetPrices,
                                                      annualShsOut, annualEps, annualBookValueOfEquity, annualDiv){
  shsOut <- annualShsOut
  eps <- annualEps
  bookValueOfEquity <- annualBookValueOfEquity
  div <- annualDiv
  periodDivisor <- 1

  inputDataType <- unique(c(class(index(monthlyAssetPrices)),class(index(monthlyBenchmarkPrices)),class(index(shsOut)),
                            class(index(eps)), class(index(bookValueOfEquity)), class(index(div))))
  if("yearmon" == inputDataType && length(inputDataType) == 1)
  {
    periodDivisor <- 12
  } # TODO: Else throw an error!

  expectedReturns <- replicate(nrow(monthlyAssetPrices),list(1:(1+ncol(monthlyAssetPrices))))
  rfr <- rfr/periodDivisor
  ## Input sim
  # rfr= 0.01
  # monthlyAssetPrices <- Quandl(c(paste("WIKI/",SP500_Comp[2:3,1],".11",sep="")),collapse="monthly", type="zoo",start_date = "2007-01-03", authcode = yourQuandlCode)
  # monthlyBenchmarkPrices <- Quandl("YAHOO/INDEX_GSPC", collapse="monthly",type="zoo", start_date = "2008-01-01", authcode = yourQuandlCode)
  # shsOut <- Quandl(c(paste("SEC/",SP500_Comp[2:3,1],"_WEIGHTEDAVERAGENUMBEROFDILUTEDSHARESOUTSTANDING_A",sep="")), collapse="annual", type="zoo", authcode = yourQuandlCode)
  # names(shsOut) <- c(as.character(SP500_Comp$Symbol[2:3]))
  # eps <- Quandl(c(paste("SEC/",SP500_Comp[2:3,1],"_EARNINGSPERSHAREDILUTED_A",sep="")), collapse="annual", type="zoo", authcode = yourQuandlCode)
  # names(eps) <- c(as.character(SP500_Comp$Symbol[2:3]))
  # bookValueOfEquity <- Quandl(c(paste("SEC/",SP500_Comp[2:3,1],"_STOCKHOLDERSEQUITY_A",sep="")), collapse="annual", type="zoo", authcode = yourQuandlCode)
  # names(bookValueOfEquity) <- c(as.character(SP500_Comp$Symbol[2:3]))
  # div <- Quandl(c(paste("SEC/DIV_",SP500_Comp[2:3,1],sep="")), collapse="annual", type="zoo", authcode = yourQuandlCode)
  # names(div) <- c(as.character(SP500_Comp$Symbol[2:3]))
  # div <- na.fill(div,0)

  # Process accounting data input
  bps <-  bookValueOfEquity/shsOut
  payRate <- abs(div/eps)
  payRate <- na.fill(payRate,0)
  # BFR list fix: payRate <- sapply((payRate[(nrow(payRate)-1):nrow(payRate)]),FUN=max)
  roe <- na.fill(eps/bps,0)/periodDivisor
  # BFR list fix: roe <- last(na.spline(eps/bps))

  # Process market data input
  currentPrices <- monthlyAssetPrices# BFR list fix: currentPrices <- last(monthlyAssetPrices)
  assetBetas <- t(sapply(seq(from = 1, to = nrow(monthlyAssetPrices), by = 1), function (x) CAPM.beta(Return.calculate(monthlyAssetPrices[1:x,]), Return.calculate((monthlyBenchmarkPrices[1:x,])[,4]), rfr)))# BFR list fix: assetBetas <- CAPM.beta(Return.calculate(monthlyAssetPrices), Return.calculate((monthlyBenchmarkPrices)[,4]), rfr)
  colnames(assetBetas) <- colnames(monthlyAssetPrices) # BFR list fix: nothing
  rownames(assetBetas) <- index(monthlyAssetPrices) # BFR list fix: nothing
  assetBetas <- as.zoo(assetBetas,index(monthlyAssetPrices)) # BFR list fix: nothing
  marketReturns <- cbind(1,sapply(seq(from = 1, to = nrow(monthlyAssetPrices), by=1), function(x)
    mean(Return.calculate((monthlyBenchmarkPrices)[1:x,4])[2:length(Return.calculate((monthlyBenchmarkPrices)[1:x,4]))]))) # BFR list fix: marketReturns <- mean(((Return.calculate((monthlyBenchmarkPrices)[,4])[2:nrow(monthlyBenchmarkPrices)])))
  rownames(marketReturns) <- rownames(assetBetas)
  marketReturns <- zoo(marketReturns[,2],index(monthlyAssetPrices))
  requiredReturns <- rfr + assetBetas * (marketReturns-rfr)# BFR list fix requiredReturns <- rfr + assetBetas * (marketReturns-rfr)
  requiredReturns <- na.fill(requiredReturns,0) # BFR list fix: nothing


  # Create a 10 year forecast for each month in the sample
  # TODO: remove h=1 restriction
  for(h in 1:nrow(monthlyAssetPrices)) {
    # Compute residual income input components
    # BFR list fix: length.out = 10 was used instead of length.out = 10*periodDivisor as well as
    #               matrix(nrow=11, ncol=ncol(bps)) and length.out = 11 was used instead of matrix(nrow=1+10*periodDivisor, ncol=ncol(bps)) and length.out = 1+10*periodDivisor and
    epsGrowthRateMatrix <- sapply(last(col(bps)), function(x) seq(from=as.numeric(roe[h,x]),to = as.numeric(requiredReturns[h,x]),length.out = 10*periodDivisor)) # BFR list fix: roe[,x] instead of roe[h,x] and requiredReturns[,x] instead of requiredReturns[h,x]
    colnames(epsGrowthRateMatrix) <- colnames(bps)
    rownames(epsGrowthRateMatrix) <- seq(from=index(bps[h,])+1/periodDivisor,by = 1/periodDivisor, length.out = 10*periodDivisor) # BFR list fix: seq(from=index(last(bps))+1,by = 1, length.out = 10*periodDivisor)

    bookValueProjectedMatrix <- matrix(nrow=1+10*periodDivisor, ncol=ncol(bps))
    bookValueProjectedMatrix[1,] <- bps[h,] # TODO: nee3ds to address NA cases!! BFR list fix: last(na.spline(eps))
    colnames(bookValueProjectedMatrix) <- colnames(bps)
    rownames(bookValueProjectedMatrix) <- seq(from=index(bps[h,]),by = 1/periodDivisor, length.out = 1+10*periodDivisor) # BFR list fix: seq(from=index(last(bps)),by = 1, length.out = 11)

    epsProjectedMatrix <- matrix(nrow=1+10*periodDivisor, ncol=ncol(bps))
    epsProjectedMatrix[1,] <- eps[h,] # TODO: nee3ds to address NA cases!! BFR list fix: last(na.spline(eps))
    colnames(epsProjectedMatrix) <- colnames(bps)
    rownames(epsProjectedMatrix) <- seq(from=index(bps[h,]),by = 1/periodDivisor, length.out = 1+10*periodDivisor) # BFR list fix: see bookValueProjectedMatrix

    residualIncomeProjectedMatrix <- matrix(nrow=1+10*periodDivisor, ncol=ncol(bps))
    residualIncomeProjectedMatrix[1,] <- rep(0,length.out = ncol(bps))
    colnames(residualIncomeProjectedMatrix) <- colnames(bps)
    rownames(residualIncomeProjectedMatrix) <- seq(from=index(bps[h,]),by = 1/periodDivisor, length.out = 1+10*periodDivisor) # BFR list fix: see bookValueProjectedMatrix

    residualIncomeProjectedPVMatrix <- matrix(nrow=1+10*periodDivisor, ncol=ncol(bps))
    residualIncomeProjectedPVMatrix[1,] <- rep(0,length.out = ncol(bps))
    colnames(residualIncomeProjectedPVMatrix) <- colnames(bps)
    rownames(residualIncomeProjectedPVMatrix) <- seq(from=index(bps[h,]),by = 1/periodDivisor, length.out = 1+10*periodDivisor) # BFR list fix: see bookValueProjectedMatrix

    # Note: First rows contain latest actual values rather tnan forecasts!
    for(i in 2:(1+10*periodDivisor)) {
      epsProjectedMatrix[i,] <- bookValueProjectedMatrix[i-1,]*(epsGrowthRateMatrix[i-1,])
      bookValueProjectedMatrix[i,] <- bookValueProjectedMatrix[i-1,]+(1-payRate[h,])*epsProjectedMatrix[i,]
      residualIncomeProjectedMatrix[i,] <- epsProjectedMatrix[i,]-bookValueProjectedMatrix[i-1,]*as.numeric(requiredReturns[h,])
      residualIncomeProjectedPVMatrix[i,] <- residualIncomeProjectedMatrix[i,]/((1+requiredReturns[h,])^(i-1))
    }

    assetValuationForecasts <-  bookValueProjectedMatrix[1,]+colSums(residualIncomeProjectedPVMatrix)

    expectedReturns[[h]] <- c(as.numeric(marketReturns[h]),as.numeric((assetValuationForecasts-currentPrices[h,])/currentPrices[h,]))
  }

  return(expectedReturns)
}

## Function to calculate Black-Litterman (BL) estimator
BLPosteriorEstimator <- function(x, spec = NULL, ...){
  Parameters <- getParams(spec)
  viewsBL = lapply(Parameters$TimeIndex, function (x) BLViews(P = Parameters$Pick,
                                                              q = c(coredata(Parameters$View[time(Parameters$View) == x, ])),
                                                              confidences = Parameters$Confidence,
                                                              assetNames = Parameters$AssetNames))
  # Note: BLViews was lapplyed/vector porcessed hence the right view needs to be found using which
  # TODO: posibly rather use Idzorek or Walters master formula here to compute the posterior distribution, BLCOP may have errors
  PostDist <- lapply(Parameters$TimeIndex, function(x) posteriorEst(views = viewsBL[[which(Parameters$TimeIndex %in% x)]],
                                                                    mu = colMeans(window(as.zoo(Parameters$PriorDist),
                                                                                         start = start(as.zoo(Parameters$PriorDist)),
                                                                                         end = x)),
                                                                    sigma = cov(window(as.zoo(Parameters$PriorDist),
                                                                                       start = start(as.zoo(Parameters$PriorDist)),
                                                                                       end = x)),
                                                                    tau = Parameters$Tau,
                                                                    kappa = Parameters$Kappa))
  # RMetrics estimator slot expects a list of mu and Sigma parameter to be returned
  list(mu = PostDist[[length(Parameters$TimeIndex)]]@posteriorMean,
       Sigma = PostDist[[length(Parameters$TimeIndex)]]@posteriorCovar)
}

MeanVarianceCVAR <- function(data, spec, constraints, backtest) # Input parameters are standard parameters handed in by portfolioBacktesting
{
  ## Extract Parameters from back test object, may contain proprietary parameter, the structure is a list object
  Parameters <- getStrategyParams(backtest)
  ## Strategy to use when optimizing the weights, this can be any proprietary code as long as a fPORTFOLIO object is created in the end
  setType(spec) <- Parameters$OptType
  setEstimator(spec) <- "covOGKEstimator"
  setSolver(spec) <- "solveRglpk.CVAR" # Note that the naming of the solvers changes compared to the documentation in the book!!!
  strategyPortfolio <- tangencyPortfolio(data, spec, constraints)
  ## Return must return an object of fPORTFOLIO type and creating that may involve some work if non ofthe RMetrics supplied function is used TODO: check of its done in RMetrics code
  return(strategyPortfolio)
}

BlackLittermanFundamental <- function(data, spec, constraints, backtest) # Input parameters are standard parameters handed in by portfolioBacktesting
{
  ## Extract Parameters from back test object, may contain proprietary parameter, the structure is a list object
  Parameters <- getStrategyParams(backtest)
  Assets <- as.zoo(Parameters$PriceData)
  Benchmark <- Parameters$BenchmarkData
  rfr <- Parameters$Rfr
  annualShsOut <- Parameters$ShsOut
  annualEps <- Parameters$Eps
  annualBookValueOfEquity <- Parameters$BookValueOfEquity
  annualDiv <- Parameters$Div

  ## Strategy to use when optimizing the weights, this can be any proprietary code as long as a fPORTFOLIO object is created in the end
  setType(spec) <- Parameters$OptType

  # Aggregating as month-end series
  AssetMonthly <- aggregate(Assets, as.yearmon, tail, 1)
  AssetNameVect <- colnames(AssetMonthly)
  AssetsNum <- ncol(AssetMonthly) # Since the Fundamental returns function does not contemplate index allocations, whiel VECM does!!
  # Computing asset returns, remember that this is only required if returns have not been previously computed ....
  AssetsReturns <- (AssetMonthly / lag(AssetMonthly, k = -1) -1.0) # TODO: use Return.calculate()
  ## Assigning Black-Litterman (BL) estimator and coresponding parameters
  # Calculate the views in terms of estimated returns per asset
  # TODO: replace the StatisticalVECMReturnEstimation with a fundamental estimator, the VECM only works for a relatively small Investment universe
  #   ReturnEst <- lapply(AssetsTimeIdx, StatisticalVECMReturnEstimation, ci = 0.5,
  #                       monthlyAssetPrices = AssetMonthly, assetNames = AssetNameVect, assetNumber = AssetsNum)
  ReturnEst <- FundamentalResidualIncomeReturnEstimation(rfr=rfr, monthlyBenchmarkPrices=Benchmark, monthlyAssetPrices=AssetMonthly[,2:ncol(AssetMonthly)],
                                                         annualShsOut=annualShsOut, annualEps=annualEps,
                                                         annualBookValueOfEquity=annualBookValueOfEquity, annualDiv=annualDiv)
  # Note: VECM requires a smaller matrix and hence a limitation on assets to use, RI estimatiosn do not have this limitation
  # Background: We need enough data excluded to avoid overfitting but still enough data included to avoid qv becoming a singular matrix
  # For VECM we would use: [-c(1:(1+4*(ncol(AssetMonthly))))]
  # For RI we would use: [-c(1:3)]
  # TODO: dynamically determine number to exclude using RetCheck <- round(svd(na.fill(matrix(unlist(ReturnEst[-c(1:10)]),ncol = AssetsNum, byrow = TRUE),0))$d,1)
  #       URGENT FIX REQUIRED IF BL RI should work, possible investigation points: fat matrix (more columns then rows) and hence too little data and time frame
  ReturnEst <- ReturnEst[-c(1:3)]
  AssetsTimeIdx <- index(AssetMonthly)[-c(1:3)]
  # Define matrix of BL-view per asset and time period in qv
  qv <- zoo(matrix(unlist(ReturnEst),
                   ncol = AssetsNum, byrow = TRUE), AssetsTimeIdx)
  qv <- na.fill(qv,0) # BFR list Fix: nothing # ensure that there are really no NA in the matrix, cause NAs may result in the fundamental return est.!!
  colnames(qv) <- AssetNameVect
  # Pick BL-matrix for directional views
  P <- diag(AssetsNum)
  colnames(P) <- AssetNameVect
  # Define the portfolio specification for BL
  setEstimator(spec) <- "BLPosteriorEstimator"
  spec@model$params<-merge(getParams(spec), # Do not change any existing parameters or thier ordering
                           list(PriorDist = AssetsReturns,
                                TimeIndex = AssetsTimeIdx,
                                AssetNames = AssetNameVect,
                                View = qv,
                                Pick = P,
                                Confidence = rep(1, AssetsNum),
                                Tau = 1, # TODO: Revise for a more sensible setting such as 1/length(AssetsTimeIdx), or E. Allaj 2013 or J. Walters 2013
                                Kappa = 1))

  #setSolver(spec) <- "solveRglpk.CVAR" # Note that the naming of the solvers changes compared to the documentation in the book!!!
  # TODO URGE: tangencyPortfolio() call fails for fundamental returns with "Error in if (STATUS != 0) { : argument is of length zero" and CEVM returns with ""
  # Investigation points: check if inbound matrix is too extremen in any way, to many values too much noise, not invertable, sqare any anything
  # Observation: ReturnsEst does not include asset names when it is from fundamental... using the Rglpk solver returns a different error "Length of bound indices must be equal to the length of the corresponding bound values."
  strategyPortfolio <- tangencyPortfolio(data, spec, constraints)
  ## Return must return an object of fPORTFOLIO type and creating that may involve some work if non ofthe RMetrics supplied function is used TODO: check of its done in RMetrics code
  return(strategyPortfolio)
}

BlackLittermanVECM <- function(data, spec, constraints, backtest) # Input parameters are standard parameters handed in by portfolioBacktesting
{
  ## Extract Parameters from back test object, may contain proprietary parameter, the structure is a list object
  Parameters <- getStrategyParams(backtest)
  Assets <- as.zoo(Parameters$PriceData)
  Benchmark <- Parameters$BenchmarkData

  ## Strategy to use when optimizing the weights, this can be any proprietary code as long as a fPORTFOLIO object is created in the end
  setType(spec) <- Parameters$OptType

  # Aggregating as month-end series
  AssetMonthly <- aggregate(Assets, as.yearmon, tail, 1)
  AssetNameVect <- colnames(AssetMonthly)
  AssetsNum <- ncol(AssetMonthly) # Since the Fundamental returns function does not contemplate index allocations, whiel VECM does!!
  # Computing asset returns, remember that this is only required if returns have not been previously computed ....
  AssetsReturns <- (AssetMonthly / lag(AssetMonthly, k = -1) -1.0) # TODO: use Return.calculate()
  ## Assigning Black-Litterman (BL) estimator and coresponding parameters
  # Calculate the views in terms of estimated returns per asset

  # Note: VECM requires a smaller matrix and hence a limitation on assets to use, RI estimatiosn do not have this limitation
  # Background: We need enough data excluded to avoid overfitting but still enough data included to avoid qv becoming a singular matrix
  # For VECM we would use: [-c(1:(1+4*(ncol(AssetMonthly))))]
  # For RI we would use: [-c(1:3)]
  # TODO: dynamically determine number to exclude using RetCheck <- round(svd(na.fill(matrix(unlist(ReturnEst[-c(1:10)]),ncol = AssetsNum, byrow = TRUE),0))$d,1)
  #       URGENT FIX REQUIRED IF BL RI should work, possible investigation points: fat matrix (more columns then rows) and hence too little data and time frame
  AssetsTimeIdx <- index(AssetMonthly)[-c(1:(1+4*(ncol(AssetMonthly))))]
  ReturnEst <- lapply(AssetsTimeIdx, StatisticalVECMReturnEstimation, ci = 0.5,
                      monthlyAssetPrices = AssetMonthly, assetNames = AssetNameVect, assetNumber = AssetsNum)
  ReturnEst <- ReturnEst[-c(1:(1+4*(ncol(AssetMonthly))))]

  # Define matrix of BL-view per asset and time period in qv
  qv <- zoo(matrix(unlist(ReturnEst),
                   ncol = AssetsNum, byrow = TRUE), AssetsTimeIdx)
  qv <- na.fill(qv,0) # BFR list Fix: nothing # ensure that there are really no NA in the matrix, cause NAs may result in the fundamental return est.!!
  colnames(qv) <- AssetNameVect
  # Pick BL-matrix for directional views
  P <- diag(AssetsNum)
  colnames(P) <- AssetNameVect
  # Define the portfolio specification for BL
  setEstimator(spec) <- "BLPosteriorEstimator"
  spec@model$params<-merge(getParams(spec), # Do not change any existing parameters or thier ordering
                           list(PriorDist = AssetsReturns,
                                TimeIndex = AssetsTimeIdx,
                                AssetNames = AssetNameVect,
                                View = qv,
                                Pick = P,
                                Confidence = rep(1, AssetsNum),
                                Tau = 1, # TODO: Revise for a more sensible setting such as 1/length(AssetsTimeIdx), or E. Allaj 2013 or J. Walters 2013
                                Kappa = 1))

  #setSolver(spec) <- "solveRglpk.CVAR" # Note that the naming of the solvers changes compared to the documentation in the book!!!
  # TODO URGE: tangencyPortfolio() call fails for fundamental returns with "Error in if (STATUS != 0) { : argument is of length zero" and CEVM returns with ""
  # Investigation points: check if inbound matrix is too extremen in any way, to many values too much noise, not invertable, sqare any anything
  # Observation: ReturnsEst does not include asset names when it is from fundamental... using the Rglpk solver returns a different error "Length of bound indices must be equal to the length of the corresponding bound values."
  strategyPortfolio <- tangencyPortfolio(data, spec, constraints)
  ## Return must return an object of fPORTFOLIO type and creating that may involve some work if non ofthe RMetrics supplied function is used TODO: check of its done in RMetrics code
  return(strategyPortfolio)
}

myCDaR <- function (object) # Input parameters are standard parameters handed in by backtestStats
{
  .cdar <- function(x) {
    alpha <- getAlpha(x)
    R <- as.numeric(getSeries(x) %*% getWeights(x))
    dd <- 100 * drawdowns(as.timeSeries(R)/100)
    z <- quantile(dd, probs = alpha)
    mean(dd[dd <= z])
  }
  portfolios <- object$strategyList
  ans <- sapply(portfolios, FUN = .cdar)
  dates <- sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])
  alpha <- getAlpha(portfolios[[1]])

  return(timeSeries(ans, charvec = dates, units = paste("CDaR", alpha, sep = "."))) # Needs to be a timeSeries
}

BuildPriceTimeSeries <- function(benchmarkSymbol, universeSymbolList, src="file", fileDir="", fileType="csv") {

  returnObject <- read.zoo(paste(fileDir, as.character(benchmarkSymbol), ".",fileType, sep=""), sep=",", FUN=as.Date, header=TRUE)
  returnObject <- returnObject[,6]
  nameVect <- c(benchmarkSymbol)
  for ( i in 1:length(as.character(c(universeSymbolList)))) {
    stock_close <- read.zoo(paste(fileDir, as.character(universeSymbolList[i]), ".",fileType, sep=""), sep=",", FUN=as.Date, header=TRUE)
    returnObject <- merge.zoo(returnObject, stock_close[,6])
    nameVect <- c(nameVect, as.character(universeSymbolList[i]))
  }
  colnames(returnObject) <- nameVect

  return(returnObject)
}

BuildFundamentalTimeSeries <- function(universeSymbolList, src="file", fileDir="", fileType="csv") {
  # TODO: Potentially catch the symbols that cannot be loaded:
  #            print(paste(universeSymbolList[i]," not loaded!")); failedSymbols <- c(failedSymbols, universeSymbolList[i]);

  returnObject <- ResidualIncomeValue(universeSymbolList[1])
  nameVect <- c(universeSymbolList[1])
  for ( i in 2:length(as.character(c(universeSymbolList)))) {
    anError <- tryCatch(residIncValue <- ResidualIncomeValue(universeSymbolList[i]), error = function(e) e)
    if(inherits(anError, "error")) next

    anError <- tryCatch(returnObject <- merge.xts(returnObject, residIncValue), error = function(e) e)
    if(inherits(anError, "error")) next
    nameVect <- c(nameVect, as.character(universeSymbolList[i]))
  }
  colnames(returnObject) <- nameVect

  return(returnObject)
}



#####
# Run functions
PortfolioConstruction <- function(type="statistical", userid="42", selection="all", start_date="2007-01-03", end_date="2014-10-31", sampleN=500, sampleT=100, benchmark="SP500", backtest=TRUE) {
  # Builds a portfolio according to instructions provided
  #
  # Args:
  #  type: Either "statistical" for simple numeric optimization or "fundamental" to first select assets on fundamental criteria and the compute optimal weights
  #  selection: Either select "all" or a "sample"
  #  sampleN: If a sample should be drawn define how large it should be
  #  sampleT: If a sample should be drawn define how often a sample should be drawn
  #  bencmark: Define the benchmark either to be "SP500" or "SP1500"
  #  backtest: Boolean to define bether the strategy should be backtested (TRUE) or just compute weights for the next period (FALSE)
  #
  # Details:
  #  Fundamental data is not available for the complete SP1500 universe. See the exclusion listbelow for tickers that will be excluded at all times
  #  The backtest for statistical and fundamental optimization differs because including the fundamental constraints in the optimization framework is incomplete
  #  Samples are drawn at random.
  #
  # Target portfolio building process:
  # 1. Load and estimate target prices and other key indicators for universe and, based on screening rules,  FundamentalRecomendation, record reason for inclusion
  # 2. Select assets with defined apreciation expectation (% increase of price to target value)  from the FundamentalRecommendation (ex the ones that are similar and ex the ones not applicable for investor
  # 3. Optimize portfolio weights and backtest while employing:
  # 3.1 a custom strategy to consider fundamental and economic data etc. and uses entropypooling and so forth...
  # 3.2 a custom smoother with higer sensitivity when vola is high and the revers, including trading as well as rebalancing and trading cost model constraints
  #
  # Returns:
  #
  #
  # TODO:
  #  - Remove 10 entry load limitation
  #  - Properly accunt for inclusions and removals in index not only for additions ... run some update_list
  #  - Thing about what is best in terms of connecting different periods for optimization, may be have seperate periods for each index change
  #  - Find a way to limit restriction to 239 assets when using the RMetrics Backtesting function

  exclusionList = c("ADP", "ALOG", "AMED", "BBOX", "BF", "BPFH", "BRK", "BRK", "CAKE", "CAMP", "CINF", "CME", "COST", "CPLA", "CTRE", "CVEO", "DNOW",
                    "FARO", "AST", "FB", "FCS", "FINL", "FISV", "FITB", "FRED", "FTD", "GILD", "GPOR", "GSM", "HAS", "HOT", "HUB", "HUB", "INFA",
                    "INTU", "JW", "JW", "JW", "MAR", "MATW", "MGAM", "MOG", "MOG", "MOG", "NANO", "NOG", "PETS", "POWI", "ROSE", "SCOR", "SIAL",
                    "SKYW", "SLAB", "SMRT", "STMP", "STRA", "SYNA", "TIME", "TMP", "TRAK", "TRIP", "TRMB", "TSCO", "TW", "UTIW", "WIN", "WM", "WPG")

  SP1500_Comp <- read.csv(paste(executionPath,"INPUT/SP1500_Comp.csv", sep=""))
  SP500_Comp <- read.csv(paste(executionPath,"INPUT/SP1500_Comp.csv", sep=""))

  regionETF <- c("EWA", "EWK", "EWZ", "EWC", "ECH", "FXI", "ICOL", "EWQ", "EWG",
                 "EWH", "INDA", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE",
                 "IWM", "EWS","EZA", "EWP", "EWD", "EWL", "THD", "TUR",
                 "EWU", "EUSA")
  regionETFNames <- c("iShares Australia", "iShares Belgium", "iShares Brazil", "iShares Canada",
                      "iShares China", "iShares Chile", "iShares Colombia", "iShares France", "iShares Germany",
                      "iShares Hong Kong", "iShares India", "iShares Indonesia", "iShares Italy",
                      "iShares Japan", "iShares Malaysia", "iShares Mexico", "iShares Netherlands", "iShares Philippines",
                      "iShares Russia", "iShares Singapore","iShares South Africa", "iShares Spain", "iShares Sweden",
                      "iShares Switzerland", "iShares Thailand", "iShares Turkey", "iShares UK", "iShares US")

  if(type == "statistical") {
    # TODO: eleminiate 10 symbol restriction
    SP500 <- BuildPriceTimeSeries(benchmarkSymbol = "SP500", universeSymbolList = as.character(SP500_Comp$Symbol[1:10]), fileDir = "~/Documents/Financial data/SP/")
    SP500 <- window(SP500,start = as.Date(start_date),end = as.Date(end_date))
    NameVect <- names(SP500)
    colNamesWithNAs <- colnames(SP500)[colSums(is.na(SP500[1,])) > 0]
    colIDsWithNAs <- which(names(SP500) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(SP500) %in% c(colNamesWithNAs))

    SP500.RET4 <- as.timeSeries(returnSeries(SP500[,colIDsWithoutNAs]))
    SP500.RET4 <- na.omit(SP500.RET4) # A single missing value will break the fPortfolio Optimizer since Sigma cannot be built...
    colnames(SP500.RET4) <- NameVect[colIDsWithoutNAs] # Col Names must be properly set up to ensure smoother and performance function find the asset names

    spSpec <- portfolioSpec()
    spConstraints <- "LongOnly"
    spBacktest <- portfolioBacktest()

    # Custom portfolio strategy definition
    setStrategyFun(spBacktest) <- "MeanVarianceCVAR"
    setStrategyParams(spBacktest) <- list(OptType = "CVAR", Est = "1", Solver = "1" )

    setWindowsHorizon(spBacktest) <- "12m"
    setSmootherLambda(spBacktest) <- "6m"
    spFormula <- (paste(paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))
    #spFormula <- as.Formula((paste("SP500 ~ ",paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))) # must match colname in data set es the first which is index
    #spFormula <- SP500~A+AA+AAPL+ABC+ABT+ACE+ACN+ACT+ADBE+ADI+ADM+ADP+ADS+ADSK+AEE+AEP+AES+AET+AFL+AGN+AIG+AIV+AIZ+AKAM+ALL+ALTR+ALXN+AMAT+AME+AMGN+AMP+AMT+AMZN+AN+AON+APA+APC+APD+APH+ARG+ATI+AVB+AVP+AVY+AXP+AZO+BA+BAC+BAX+BBBY+BBT+BBY+BCR+BDX+BEN+BF_B+BHI+BIIB+BK+BLK+BLL+BMS+BMY+BRCM+BRK_B+BSX+BTU+BWA+BXP+C+CA+CAG+CAH+CAM+CAT+CB+CBG+CBS+CCE+CCI+CCL+CELG+CERN+CF+CHK+CHRW+CI+CINF+CL+CLX+CMA+CMCSA+CME+CMG+CMI+CMS+CNP+CNX+COF+COG+COH+COL+COP+COST+CPB+CRM+CSC+CSCO+CSX+CTAS+CTL+CTSH+CTXS+CVC+CVS+CVX+D+DD+DE+DGX+DHI+DHR+DIS+DISCA+DLTR+DNB+DNR+DO+DOV+DOW+DRI+DTE+DTV+DUK+DVA+DVN+EA+EBAY+ECL+ED+EFX+EIX+EL+EMC+EMN+EMR+EOG+EQR+EQT+ESRX+ESS+ESV+ETFC+ETN+ETR+EW+EXC+EXPD+EXPE+F+FAST+FCX+FDO+FDX+FE+FFIV+FIS+FISV+FITB+FLIR+FLR+FLS+FMC+FOSL+FOXA+FRX+FSLR+FTI+FTR+GAS+GCI+GD+GE+GGP+GHC+GILD+GIS+GLW+GMCR+GME+GNW+GPC+GPS+GRMN+GS+GT+GWW+HAL+HAR+HAS+HBAN+HCBK+HCN+HCP+HD+HES+HIG+HOG+HON+HOT+HP+HPQ+HRB+HRL+HRS+HSP+HST+HSY+HUM+IBM+ICE+IFF+INTC+INTU+IP+IPG+IR+IRM+ISRG+ITW+IVZ+JBL+JCI+JEC+JNJ+JNPR+JOY+JPM+JWN+K+KEY+KIM+KLAC+KMB+KMX+KO+KR+KSS+KSU+L+LB+LEG+LEN+LH+LLL+LLTC+LLY+LM+LMT+LNC+LOW+LRCX+LUK+LUV+M+MA+MAC+MAR+MAS+MAT+MCD+MCHP+MCK+MCO+MDLZ+MDT+MET+MHFI+MHK+MKC+MMC+MMM+MNST+MO+MON+MOS+MRK+MRO+MS+MSFT+MSI+MTB+MU+MUR+MWV+MYL+NBL+NBR+NDAQ+NE+NEE+NEM+NFLX+NFX+NI+NKE+NOC+NOV+NRG+NSC+NTAP+NTRS+NU+NUE+NVDA+NWL+OI+OKE+OMC+ORCL+ORLY+OXY+PAYX+PBCT+PBI+PCAR+PCG+PCL+PCLN+PCP+PDCO+PEG+PEP+PETM+PFE+PFG+PG+PGR+PH+PHM+PKI+PLD+PLL+PNC+PNR+PNW+POM+PPG+PPL+PRGO+PRU+PSA+PVH+PWR+PX+PXD+QCOM+R+RAI+RDC+REGN+RF+RHI+RHT+RIG+RL+ROK+ROP+ROST+RRC+RSG+RTN+SBUX+SCG+SCHW+SE+SEE+SHW+SIAL+SJM+SLB+SNA+SNDK+SO+SPG+SPLS+SRCL+SRE+STI+STJ+STT+STX+STZ+SWK+SWN+SWY+SYK+SYMC+SYY+T+TAP+TE+TEG+TGT+THC+TIF+TJX+TMK+TMO+TROW+TRV+TSCO+TSN+TSO+TSS+TWX+TXN+TXT+TYC+UA+UNH+UNM+UNP+UPS+URBN+USB+UTX+VAR+VFC+VIAB+VLO+VMC+VNO+VRSN+VRTX+VTR+VZ+WAG+WAT+WDC+WEC+WFC+WFM+WHR+WIN+WLP+WM+WMB+WMT+WU+WY+WYN+WYNN+X+XEC+XEL+XL+XLNX+XOM+XRAY+XRX+YHOO+YUM+ZION+ZMH
    funk <- c("","",""); funk[2] <- "SP500"; funk[3] <- "~"; funk[3] <- spFormula # where spFormula does not include "SP500~" which works now it only complains aboutthe benchmarkName!!!
    spPortfolios <- portfolioBacktesting(formula = funk, data = SP500.RET4, spec = spSpec,
                                         constraints = spConstraints, backtest = spBacktest, trace = FALSE)
    setSmootherInitialWeights(spBacktest) <- rep(1/(ncol(SP500.RET4)-1), ncol(SP500.RET4)-1) # This is where an investor would insert his current allocation
    spSmooth <- portfolioSmoothing(object = spPortfolios, backtest =spBacktest)

    backtestPlot(spSmooth)
    spNetPerf <- netPerformance(spSmooth)
    spBackTestStats <- backtestStats(spSmooth, FUN ="myCDaR")

    myProposalAnalysis <- c()
    myProposalAnalysis$spPortfolios <- spPortfolios
    myProposalAnalysis$spSmooth <- spSmooth
    myProposalAnalysis$spNetPerf <- spNetPerf
    myProposalAnalysis$spBackTestStats <- spBackTestStats

    return(myProposalAnalysis)
  }
  if(type == "statisticalRegionETF") {
    #     # TODO: eleminiate 10 symbol restriction
    #     bui <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[1:10],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui1 <- Quandl(c(paste("GOOG/AMEX_",regionETF[11],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui2 <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[12:20],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui3 <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[21:28],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    # SP500_Bench <- Quandl("YAHOO/INDEX_GSPC", collapse="daily",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    MSCI_ACWI_Bench <- Quandl("GOOG/NYSE_ACWI", collapse="daily",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    #     cui <- bui[,seq(4,ncol(bui),by=5)]
    #     cui1 <- bui1[,seq(4,ncol(bui1),by=5)]
    #     cui2 <- bui2[,seq(4,ncol(bui2),by=5)]
    #     cui3 <- bui3[,seq(4,ncol(bui3),by=5)]
    #SP500 <- BuildPriceTimeSeries(benchmarkSymbol = "SP500", universeSymbolList = as.character(SP500_Comp$Symbol[1:10]), fileDir = "~/Documents/Financial data/SP/")
    #     SP500 <- merge(MSCI_ACWI_Bench[,4], cui, cui1, cui2, cui3)
    SP500 <- merge(MSCI_ACWI_Bench[,4], window(dget(paste(file_root_dir,"/INPUT/regionETF.data.R",sep="")), start = start_date, end = end_date))
    #     colnames(SP500) <- c("SP500",regionETF[1:28])
    colnames(SP500) <- c("ACWI",names(SP500[,2:ncol(SP500)]))
    NameVect <- names(SP500)
    colNamesWithNAs <- colnames(SP500)[colSums(is.na(SP500[1,])) > 0]
    colIDsWithNAs <- which(names(SP500) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(SP500) %in% c(colNamesWithNAs))

    SP500.RET4 <- as.timeSeries(returnSeries(SP500[,colIDsWithoutNAs]))
    SP500.RET4 <- na.omit(SP500.RET4) # A single missing value will break the fPortfolio Optimizer since Sigma cannot be built...
    colnames(SP500.RET4) <- NameVect[colIDsWithoutNAs] # Col Names must be properly set up to ensure smoother and performance function find the asset names

    spSpec <- portfolioSpec()
    spConstraints <- "LongOnly" # ETFs cannot be soled short
    spConstraints <- c(paste("minW[1:",(ncol(SP500.RET4)-1),"] = 0.0",sep=""), #BFR list fix: NAssets
                       paste("maxW[1:",(ncol(SP500.RET4)-1),"] = 0.3", sep="")) #"LongOnly" #BFR list fix: NAssets
    spBacktest <- portfolioBacktest()

    # Custom portfolio strategy definition
    setStrategyFun(spBacktest) <- "MeanVarianceCVAR"
    setStrategyParams(spBacktest) <- list(OptType = "CVAR", Est = "1", Solver = "1" )

    setWindowsHorizon(spBacktest) <- "24m"
    setSmootherLambda(spBacktest) <- "12m"
    spFormula <- (paste(paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))
    #spFormula <- as.Formula((paste("SP500 ~ ",paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))) # must match colname in data set es the first which is index
    #spFormula <- SP500~A+AA+AAPL+ABC+ABT+ACE+ACN+ACT+ADBE+ADI+ADM+ADP+ADS+ADSK+AEE+AEP+AES+AET+AFL+AGN+AIG+AIV+AIZ+AKAM+ALL+ALTR+ALXN+AMAT+AME+AMGN+AMP+AMT+AMZN+AN+AON+APA+APC+APD+APH+ARG+ATI+AVB+AVP+AVY+AXP+AZO+BA+BAC+BAX+BBBY+BBT+BBY+BCR+BDX+BEN+BF_B+BHI+BIIB+BK+BLK+BLL+BMS+BMY+BRCM+BRK_B+BSX+BTU+BWA+BXP+C+CA+CAG+CAH+CAM+CAT+CB+CBG+CBS+CCE+CCI+CCL+CELG+CERN+CF+CHK+CHRW+CI+CINF+CL+CLX+CMA+CMCSA+CME+CMG+CMI+CMS+CNP+CNX+COF+COG+COH+COL+COP+COST+CPB+CRM+CSC+CSCO+CSX+CTAS+CTL+CTSH+CTXS+CVC+CVS+CVX+D+DD+DE+DGX+DHI+DHR+DIS+DISCA+DLTR+DNB+DNR+DO+DOV+DOW+DRI+DTE+DTV+DUK+DVA+DVN+EA+EBAY+ECL+ED+EFX+EIX+EL+EMC+EMN+EMR+EOG+EQR+EQT+ESRX+ESS+ESV+ETFC+ETN+ETR+EW+EXC+EXPD+EXPE+F+FAST+FCX+FDO+FDX+FE+FFIV+FIS+FISV+FITB+FLIR+FLR+FLS+FMC+FOSL+FOXA+FRX+FSLR+FTI+FTR+GAS+GCI+GD+GE+GGP+GHC+GILD+GIS+GLW+GMCR+GME+GNW+GPC+GPS+GRMN+GS+GT+GWW+HAL+HAR+HAS+HBAN+HCBK+HCN+HCP+HD+HES+HIG+HOG+HON+HOT+HP+HPQ+HRB+HRL+HRS+HSP+HST+HSY+HUM+IBM+ICE+IFF+INTC+INTU+IP+IPG+IR+IRM+ISRG+ITW+IVZ+JBL+JCI+JEC+JNJ+JNPR+JOY+JPM+JWN+K+KEY+KIM+KLAC+KMB+KMX+KO+KR+KSS+KSU+L+LB+LEG+LEN+LH+LLL+LLTC+LLY+LM+LMT+LNC+LOW+LRCX+LUK+LUV+M+MA+MAC+MAR+MAS+MAT+MCD+MCHP+MCK+MCO+MDLZ+MDT+MET+MHFI+MHK+MKC+MMC+MMM+MNST+MO+MON+MOS+MRK+MRO+MS+MSFT+MSI+MTB+MU+MUR+MWV+MYL+NBL+NBR+NDAQ+NE+NEE+NEM+NFLX+NFX+NI+NKE+NOC+NOV+NRG+NSC+NTAP+NTRS+NU+NUE+NVDA+NWL+OI+OKE+OMC+ORCL+ORLY+OXY+PAYX+PBCT+PBI+PCAR+PCG+PCL+PCLN+PCP+PDCO+PEG+PEP+PETM+PFE+PFG+PG+PGR+PH+PHM+PKI+PLD+PLL+PNC+PNR+PNW+POM+PPG+PPL+PRGO+PRU+PSA+PVH+PWR+PX+PXD+QCOM+R+RAI+RDC+REGN+RF+RHI+RHT+RIG+RL+ROK+ROP+ROST+RRC+RSG+RTN+SBUX+SCG+SCHW+SE+SEE+SHW+SIAL+SJM+SLB+SNA+SNDK+SO+SPG+SPLS+SRCL+SRE+STI+STJ+STT+STX+STZ+SWK+SWN+SWY+SYK+SYMC+SYY+T+TAP+TE+TEG+TGT+THC+TIF+TJX+TMK+TMO+TROW+TRV+TSCO+TSN+TSO+TSS+TWX+TXN+TXT+TYC+UA+UNH+UNM+UNP+UPS+URBN+USB+UTX+VAR+VFC+VIAB+VLO+VMC+VNO+VRSN+VRTX+VTR+VZ+WAG+WAT+WDC+WEC+WFC+WFM+WHR+WIN+WLP+WM+WMB+WMT+WU+WY+WYN+WYNN+X+XEC+XEL+XL+XLNX+XOM+XRAY+XRX+YHOO+YUM+ZION+ZMH
    funk <- c("","",""); funk[2] <- names(SP500)[1]; funk[3] <- "~"; funk[3] <- spFormula # where spFormula does not include "SP500~" which works now it only complains aboutthe benchmarkName!!!
    spPortfolios <- portfolioBacktesting(formula = funk, data = SP500.RET4, spec = spSpec,
                                         constraints = spConstraints, backtest = spBacktest, trace = FALSE)
    setSmootherInitialWeights(spBacktest) <- rep(1/(ncol(SP500.RET4)-1), ncol(SP500.RET4)-1) # This is where an investor would insert his current allocation
    spSmooth <- portfolioSmoothing(object = spPortfolios, backtest =spBacktest)
    # Write results to storrage
    write.csv(spSmooth$stats,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_stats_",Sys.time(),".csv",sep = ""))
    write(paste(as.character(spSmooth$backtest@windows), as.character(spSmooth$backtest@strategy), as.character(spSmooth$backtest@smoother), sep = "NEXT_SPEC_ITEM"),
          paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_spec_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$smoothWeights,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_weights_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$P,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_monthly_portfolio_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$data,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyBenchmark,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_benchmark_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyAssets,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_asset_returns_",Sys.time(),".csv",sep = ""))
    # Plot results to screen
    backtestPlot(spSmooth)
    spNetPerf <- netPerformance(spSmooth)
    spBackTestStats <- backtestStats(spSmooth, FUN ="myCDaR")

    # Prepare return object
    myProposalAnalysis <- c()
    myProposalAnalysis$spPortfolios <- spPortfolios
    myProposalAnalysis$spSmooth <- spSmooth
    myProposalAnalysis$spNetPerf <- spNetPerf
    myProposalAnalysis$spBackTestStats <- spBackTestStats

    return(myProposalAnalysis)
  }
  if(type == "statisticalSectorETF") {
    #     # TODO: eleminiate 10 symbol restriction
    #     bui <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[1:10],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui1 <- Quandl(c(paste("GOOG/AMEX_",regionETF[11],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui2 <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[12:20],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    #     bui3 <- Quandl(c(paste("GOOG/NYSEARCA_",regionETF[21:28],sep="")), column=4, collapse="daily", type="zoo", start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    #     Sys.sleep(30) # Sleep to avoide Quandl API timeout
    # SP500_Bench <- Quandl("YAHOO/INDEX_GSPC", collapse="daily",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    MSCI_ACWI_Bench <- Quandl("GOOG/NYSE_ACWI", collapse="daily",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    #     cui <- bui[,seq(4,ncol(bui),by=5)]
    #     cui1 <- bui1[,seq(4,ncol(bui1),by=5)]
    #     cui2 <- bui2[,seq(4,ncol(bui2),by=5)]
    #     cui3 <- bui3[,seq(4,ncol(bui3),by=5)]
    #SP500 <- BuildPriceTimeSeries(benchmarkSymbol = "SP500", universeSymbolList = as.character(SP500_Comp$Symbol[1:10]), fileDir = "~/Documents/Financial data/SP/")
    #     SP500 <- merge(MSCI_ACWI_Bench[,4], cui, cui1, cui2, cui3)
    SP500 <- merge(MSCI_ACWI_Bench[,4], window(dget(paste(file_root_dir,"/INPUT/sectorETF.data.R",sep="")), start = start_date, end = end_date))
    #     colnames(SP500) <- c("SP500",regionETF[1:28])
    colnames(SP500) <- c("ACWI",names(SP500[,2:ncol(SP500)]))
    NameVect <- names(SP500)
    # TOOD: Verify if the NA logic below is really doing what it is supposed to do!
    colNamesWithNAs <- colnames(SP500)[colSums(is.na(SP500[1,])) > 0]
    colIDsWithNAs <- which(names(SP500) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(SP500) %in% c(colNamesWithNAs))

    SP500.RET4 <- as.timeSeries(returnSeries(SP500))
    SP500.RET4 <- na.omit(SP500.RET4) # A single missing value will break the fPortfolio Optimizer since Sigma cannot be built...
    # colnames(SP500.RET4) <- NameVect[colIDsWithoutNAs] # Col Names must be properly set up to ensure smoother and performance function find the asset names

    spSpec <- portfolioSpec()
    spConstraints <- "LongOnly" # ETFs cannot be soled short
    spConstraints <- c(paste("minW[1:",(ncol(SP500.RET4)-1),"] = 0.0",sep=""), #BFR list fix: NAssets
                       paste("maxW[1:",(ncol(SP500.RET4)-1),"] = 0.3", sep="")) #"LongOnly" #BFR list fix: NAssets
    spBacktest <- portfolioBacktest()

    # Custom portfolio strategy definition
    setStrategyFun(spBacktest) <- "MeanVarianceCVAR"
    setStrategyParams(spBacktest) <- list(OptType = "CVAR", Est = "1", Solver = "1" )

    setWindowsHorizon(spBacktest) <- "24m"
    setSmootherLambda(spBacktest) <- "12m"
    spFormula <- paste(colnames(SP500.RET4[,2:ncol(SP500.RET4)]),collapse="+")
    #spFormula <- as.Formula((paste("SP500 ~ ",paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))) # must match colname in data set es the first which is index
    #spFormula <- SP500~A+AA+AAPL+ABC+ABT+ACE+ACN+ACT+ADBE+ADI+ADM+ADP+ADS+ADSK+AEE+AEP+AES+AET+AFL+AGN+AIG+AIV+AIZ+AKAM+ALL+ALTR+ALXN+AMAT+AME+AMGN+AMP+AMT+AMZN+AN+AON+APA+APC+APD+APH+ARG+ATI+AVB+AVP+AVY+AXP+AZO+BA+BAC+BAX+BBBY+BBT+BBY+BCR+BDX+BEN+BF_B+BHI+BIIB+BK+BLK+BLL+BMS+BMY+BRCM+BRK_B+BSX+BTU+BWA+BXP+C+CA+CAG+CAH+CAM+CAT+CB+CBG+CBS+CCE+CCI+CCL+CELG+CERN+CF+CHK+CHRW+CI+CINF+CL+CLX+CMA+CMCSA+CME+CMG+CMI+CMS+CNP+CNX+COF+COG+COH+COL+COP+COST+CPB+CRM+CSC+CSCO+CSX+CTAS+CTL+CTSH+CTXS+CVC+CVS+CVX+D+DD+DE+DGX+DHI+DHR+DIS+DISCA+DLTR+DNB+DNR+DO+DOV+DOW+DRI+DTE+DTV+DUK+DVA+DVN+EA+EBAY+ECL+ED+EFX+EIX+EL+EMC+EMN+EMR+EOG+EQR+EQT+ESRX+ESS+ESV+ETFC+ETN+ETR+EW+EXC+EXPD+EXPE+F+FAST+FCX+FDO+FDX+FE+FFIV+FIS+FISV+FITB+FLIR+FLR+FLS+FMC+FOSL+FOXA+FRX+FSLR+FTI+FTR+GAS+GCI+GD+GE+GGP+GHC+GILD+GIS+GLW+GMCR+GME+GNW+GPC+GPS+GRMN+GS+GT+GWW+HAL+HAR+HAS+HBAN+HCBK+HCN+HCP+HD+HES+HIG+HOG+HON+HOT+HP+HPQ+HRB+HRL+HRS+HSP+HST+HSY+HUM+IBM+ICE+IFF+INTC+INTU+IP+IPG+IR+IRM+ISRG+ITW+IVZ+JBL+JCI+JEC+JNJ+JNPR+JOY+JPM+JWN+K+KEY+KIM+KLAC+KMB+KMX+KO+KR+KSS+KSU+L+LB+LEG+LEN+LH+LLL+LLTC+LLY+LM+LMT+LNC+LOW+LRCX+LUK+LUV+M+MA+MAC+MAR+MAS+MAT+MCD+MCHP+MCK+MCO+MDLZ+MDT+MET+MHFI+MHK+MKC+MMC+MMM+MNST+MO+MON+MOS+MRK+MRO+MS+MSFT+MSI+MTB+MU+MUR+MWV+MYL+NBL+NBR+NDAQ+NE+NEE+NEM+NFLX+NFX+NI+NKE+NOC+NOV+NRG+NSC+NTAP+NTRS+NU+NUE+NVDA+NWL+OI+OKE+OMC+ORCL+ORLY+OXY+PAYX+PBCT+PBI+PCAR+PCG+PCL+PCLN+PCP+PDCO+PEG+PEP+PETM+PFE+PFG+PG+PGR+PH+PHM+PKI+PLD+PLL+PNC+PNR+PNW+POM+PPG+PPL+PRGO+PRU+PSA+PVH+PWR+PX+PXD+QCOM+R+RAI+RDC+REGN+RF+RHI+RHT+RIG+RL+ROK+ROP+ROST+RRC+RSG+RTN+SBUX+SCG+SCHW+SE+SEE+SHW+SIAL+SJM+SLB+SNA+SNDK+SO+SPG+SPLS+SRCL+SRE+STI+STJ+STT+STX+STZ+SWK+SWN+SWY+SYK+SYMC+SYY+T+TAP+TE+TEG+TGT+THC+TIF+TJX+TMK+TMO+TROW+TRV+TSCO+TSN+TSO+TSS+TWX+TXN+TXT+TYC+UA+UNH+UNM+UNP+UPS+URBN+USB+UTX+VAR+VFC+VIAB+VLO+VMC+VNO+VRSN+VRTX+VTR+VZ+WAG+WAT+WDC+WEC+WFC+WFM+WHR+WIN+WLP+WM+WMB+WMT+WU+WY+WYN+WYNN+X+XEC+XEL+XL+XLNX+XOM+XRAY+XRX+YHOO+YUM+ZION+ZMH
    funk <- c("","",""); funk[2] <- names(SP500)[1]; funk[3] <- "~"; funk[3] <- spFormula # where spFormula does not include "SP500~" which works now it only complains aboutthe benchmarkName!!!
    spPortfolios <- portfolioBacktesting(formula = funk, data = SP500.RET4, spec = spSpec,
                                         constraints = spConstraints, backtest = spBacktest, trace = FALSE)
    setSmootherInitialWeights(spBacktest) <- rep(1/(ncol(SP500.RET4)-1), ncol(SP500.RET4)-1) # This is where an investor would insert his current allocation
    spSmooth <- portfolioSmoothing(object = spPortfolios, backtest =spBacktest)
    # Write results to storrage
    write.csv(spSmooth$stats,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_stats_",Sys.time(),".csv",sep = ""))
    write(paste(as.character(spSmooth$backtest@windows), as.character(spSmooth$backtest@strategy), as.character(spSmooth$backtest@smoother), sep = "NEXT_SPEC_ITEM"),
          paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_spec_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$smoothWeights,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_weights_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$P,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_monthly_portfolio_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$data,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyBenchmark,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_benchmark_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyAssets,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_asset_returns_",Sys.time(),".csv",sep = ""))
    # Plot results to screen
    backtestPlot(spSmooth)
    spNetPerf <- netPerformance(spSmooth)
    spBackTestStats <- backtestStats(spSmooth, FUN ="myCDaR")

    # Prepare return object
    myProposalAnalysis <- c()
    myProposalAnalysis$spPortfolios <- spPortfolios
    myProposalAnalysis$spSmooth <- spSmooth
    myProposalAnalysis$spNetPerf <- spNetPerf
    myProposalAnalysis$spBackTestStats <- spBackTestStats

    return(myProposalAnalysis)
  }
  if(type == "fundamentalOld") {
    sp1500Symbols <- as.character(SP1500_Comp$Symbol)
    sp1500Symbols <- sp1500Symbols[which(!sp1500Symbols %in% exclusionList)] # Exclude data for which we know not to have even fundamental data files

    # TODO: After na.spline() SP1500 values in 2007 seem way off, especially since there is no data fro 2007... maybe set debug point on condition sp1500 > 10000?
    sp1500Price <- BuildPriceTimeSeries(benchmarkSymbol = "SP1500", universeSymbolList = sp1500Symbols, fileDir = "~/Documents/Financial data/SP/")
    curruptedPriceDataList <- unique(sp1500Symbols[which(!sp1500Symbols %in% names(sp1500Price))])
    sp1500Price <- na.spline(sp1500Price)

    sp1500Value <- BuildFundamentalTimeSeries(sp1500Symbols, fileDir = "~/Documents/Financial data/SP/")
    sp1500Value <- merge(sp1500Value, xts(, index(sp1500Price)), join = "right") # For every day with a price insert an index for value
    curruptedValueDataList <- unique(sp1500Symbols[which(!sp1500Symbols %in% names(sp1500Value))])
    curruptedValueDataList <- c(curruptedValueDataList, names(sp1500ValueColAligned[,(colSums(is.na(sp1500ValueColAligned)) == nrow(sp1500ValueColAligned))]))
    sp1500Value <- na.locf(sp1500Value)

    # Plot some chart is required use: plot.zoo(sp1500Price[,2], col=2); lines.default(index(sp1500ValueCopy[!is.na(sp1500ValueCopy[,1]),1]), sp1500ValueCopy[!is.na(sp1500ValueCopy[,1]),1], col=3)
    # TODO: Check that the curruptedPriceDataList does not include all symbols due to missing NAs...
    sp1500PriceColAligned <- sp1500Price[,(!names(sp1500Price) %in% c(curruptedValueDataList, curruptedPriceDataList))] # Exclude data which did not yield results
    sp1500ValueColAligned <- sp1500Value[,(!names(sp1500Value) %in% c(curruptedValueDataList, curruptedPriceDataList))] # Exclude data which did not yield results

    # END backtesting  parameters
    startDate=as.Date("1.1.2008", format="%d.%m.%Y")
    horizonMonths = 12
    rebalanceMonths = 1
    endDate = end(sp1500PriceColAligned)

    minriskSpec <- portfolioSpec()
    setTargetReturn(minriskSpec) <- 0.05
    # END backtesting  parameters

    # Since to.monthly() won't work because the NAs would confuse the function we need to hack a monthly series.
    # Unfortunatly the daily2monthly() is good to generate the proper months it alignes at end of the month, to savely (even in leap years) get to
    # the first of the monthy we tip over by one day and then substract a whole month, this data sequence is used to select actual data from available data
    sp1500PriceColAlignedMonthy <- sp1500PriceColAligned[as.Date(rownames(daily2monthly(as.timeSeries(sp1500PriceColAligned))))+1-months(1),]
    sp1500ValueColAlignedMonthy <- sp1500ValueColAligned[as.Date(rownames(daily2monthly(as.timeSeries(sp1500ValueColAligned))))+1-months(1),]

    numberOfPeriods = nrow(sp1500PriceColAlignedMonthy)

    # TODO: Rewrite using rollapply()
    # TODO: Allow daily rebalance ...
    optWeights <- sp1500PriceColAlignedMonthy
    optWeights[,1:ncol(optWeights)] <- 0

    # TODO: RUNS, BUT WEIGHST ARE ALL 0 CHECK OPTIMIZATION CALL PARAMETERS, THEN CHECK DATA LOADING ERRORS WHEN ASSIGNING "sp1500ValueColAlignedMonthy"
    # => windowPrices/Values length is inconsisten the months(1) fix is not neccesarily correct! Avoid look-ahead- biase!
    for ( i in 0:numberOfPeriods) {
      windowStartDate <- startDate + months(i)
      windowEndDate <- windowStartDate + months(horizonMonths-1+i) # Needs to exclude 1 month to avoid overlapping
      windowDateSeq <- seq(windowStartDate, windowEndDate , "1 months")
      windowPrices <- sp1500PriceColAlignedMonthy[windowDateSeq, 2:ncol(sp1500PriceColAlignedMonthy)] # Exclude SP1500 index prices in firts column
      windowValues <- sp1500ValueColAlignedMonthy[windowDateSeq, 1:ncol(sp1500ValueColAlignedMonthy)]

      # Filter for only those symbols where ther is a known residual income and that is actually above the current stock price
      if(length(windowPrices[windowEndDate,] < as.zoo(windowValues[windowEndDate,])) == 0) {

        # Do nothing because weights are initialized as zero in the beginning.

      }
      if(length(windowPrices[windowEndDate,] < as.zoo(windowValues[windowEndDate,])) != 0) {
        windowHighValueSymbols <- colnames(windowPrices)[colSums(!is.na(windowPrices[windowEndDate,] < as.zoo(windowValues[windowEndDate,]))) > 0]

        # Compare with revious period and record changed symbols and reason
        #optInclReason[windowEndDate,] = ""

        # Optimize only for high value symbols and end of window period but harvesting price volatility for the previous 12 months!
        windowReturns <- as.timeSeries(returnSeries(windowPrices[,windowHighValueSymbols]))
        # Make sure there are no NAs left (one might always be teher from the returns calculation
        windowReturns <- na.omit(windowReturns)
        minriskPortfolio <- efficientPortfolio(data = windowReturns,
                                               spec = minriskSpec,
                                               constraints = "LongOnly")

        # Update weights
        optWeights[windowEndDate, c(windowHighValueSymbols)] <- c(minriskPortfolio@portfolio@portfolio$weights)
      }

    }
    # Remove col where all entries are NA: colnames(sp1500ValueColAligned)[colSums(is.na(sp1500ValueColAligned[1,])) == nrow(sp1500ValueColAligned)]
    # Remove all rows where there are NA: rowSums(is.na(sp1500ValueColAligned)) > 0
    # TODO: get only those where Price < Value, remove similar assets, run portfolio weight optimization, iterate over available data horizon
    # sp1500Value[!is.na(sp1500Value[,1]),1]
  }
  if(type == "BlackLitermanFundamental") {
    # TODO: extend Quandel data sourcing, bunch of 50 symbols at a time should work
    # Gets the 11th column (adust. close price) for first 1:10 SP500 symbols starting 1.1.07 as zoo type from the WIKI database
    # quandldata = Quandl(c(paste("WIKI/",SP500_Comp[1:10,1],".11",sep="")),type="zoo",start_date = "2007-01-03", authcode = yourQuandlCode)
    # myBench <- Quandl("YAHOO/INDEX_GSPC",type="zoo",start_date = "2007-01-03", authcode = yourQuandlCode)
    # SP500 <- merge(myBench[,6],oldQuandldata)
    # names(SP500) <- c("SP500", as.character(SP500_Comp$Symbol[1:50]))
    #
    # TODO: Make investment universe user defined
    #  data(EuStockMarkets)
    # myBench <- Quandl("YAHOO/INDEX_GSPC",type="zoo",start_date = "1991-05-01", end_date = "1998-09-08", authcode = yourQuandlCode)
    #  SP500 <- merge(myBench[1:1860,6],zoo(EuStockMarkets,index(myBench)))
    # names(SP500) <- c("SP500", colnames(EuStockMarkets))
    # TODO: Add a universe from iShares ETFs:
    #     markets <- c("EWA", "EWK", "EWZ", "EWC", "ECH", "FXI", "ICOL", "EWQ", "EWG",
    #                  "EWH", "INDA", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE",
    #                  "IWM", "EWS","EZA", "EWP", "EWD", "EWL", "THD", "TUR",
    #                  "EWU", "EUSA")
    # TODO: parametrize past horizon (i.e., based on investment horizon)

    # Market data retrieval
    # TODO: eleminiate 10 symbol restriction... but observe that VECM return estimate would either break then or need either daily or longer history
    # TODO: at some point get this from Quandl too, would need to identify primary listing through SEC data lookup and then trieve price from Google, don't forget to include SP500 in here!
    SP500 <- BuildPriceTimeSeries(benchmarkSymbol = "SP500", universeSymbolList = as.character(SP500_Comp$Symbol[1:10]), fileDir = "~/Documents/Financial data/SP/")
    SP500 <- window(SP500,start = as.Date(start_date),end = as.Date(end_date))
    NameVect <- names(SP500)
    NAssets <- length(names(SP500))-1 # Symbols minus benchmark = number of assets
    colNamesWithNAs <- colnames(SP500)[colSums(is.na(SP500[1,])) > 0]
    rfr= 0.01
    monthlyBenchmarkPrices <- Quandl("YAHOO/INDEX_GSPC", collapse="monthly",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)

    # Fundamental data retrieval
    # BFR list fix: collapse="annual" as used instead of collapse="monthly" and annual data "_A" was fetched instead of quarterly "_Q"
    monthlyStdTimeIdx <- (as.zoo(as.yearmon(seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = "month")))) # BFR list fix: nothing
    index(monthlyStdTimeIdx) <- as.yearmon(seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = "month")) # BFR list fix: nothing

    shsOut <- Quandl(c(paste("SEC/",SP500_Comp[1:10,1],"_WEIGHTEDAVERAGENUMBEROFDILUTEDSHARESOUTSTANDING_Q",sep="")), collapse="monthly", type="zoo",
                     start_date = start_date, end_date = end_date,authcode = yourQuandlCode)
    shsOut <- merge(monthlyStdTimeIdx,shsOut,fill=NA) # BFR list fix: nothing
    shsOut <- na.locf(shsOut) # BFR list fix: nothing
    shsOut <- shsOut[,-1] # Index only removed here to ensure that all is well aligned witr stdTimeIdx BFR list fix: nothing
    names(shsOut) <- c(as.character(SP500_Comp$Symbol[1:10]))
    colNamesWithNAs <- c(colNamesWithNAs, colnames(shsOut)[colSums(is.na(shsOut[(nrow(shsOut)-2):nrow(shsOut),])) > 1])

    eps <- Quandl(c(paste("SEC/",SP500_Comp[1:10,1],"_EARNINGSPERSHAREDILUTED_Q",sep="")), collapse="monthly", type="zoo",
                  start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    eps <- merge(monthlyStdTimeIdx,eps,fill=NA) # BFR list fix: nothing
    eps <- na.locf(eps) # BFR list fix: nothing
    eps <- eps[,-1] # BFR list fix: nothing
    names(eps) <- c(as.character(SP500_Comp$Symbol[1:10]))
    colNamesWithNAs <- c(colNamesWithNAs, colnames(eps)[colSums(is.na(eps[(nrow(eps)-2):nrow(eps),])) > 1])

    bookValueOfEquity <- Quandl(c(paste("SEC/",SP500_Comp[1:10,1],"_STOCKHOLDERSEQUITY_Q",sep="")), collapse="monthly", type="zoo",
                                start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    bookValueOfEquity <- merge(monthlyStdTimeIdx,bookValueOfEquity,fill=NA) # BFR list fix: nothing
    bookValueOfEquity <- na.locf(bookValueOfEquity) # BFR list fix: nothing
    bookValueOfEquity <- bookValueOfEquity[,-1] # BFR list fix: nothing
    names(bookValueOfEquity) <- c(as.character(SP500_Comp$Symbol[1:10]))
    colNamesWithNAs <- c(colNamesWithNAs, colnames(bookValueOfEquity)[colSums(is.na(bookValueOfEquity[(nrow(bookValueOfEquity)-2):nrow(bookValueOfEquity),])) > 1])

    div <- Quandl(c(paste("SEC/DIV_",SP500_Comp[1:10,1],sep="")), collapse="monthly", type="zoo",
                  start_date = start_date, end_date = end_date, authcode = yourQuandlCode)
    div <- merge(monthlyStdTimeIdx,div,fill=NA) # BFR list fix: nothing
    div <- na.locf(div) # BFR list fix: nothing
    div <- div[,-1] # BFR list fix: nothing
    names(div) <- c(as.character(SP500_Comp$Symbol[1:10]))
    colNamesWithNAs <- c(colNamesWithNAs, colnames(div)[colSums(is.na(div[(nrow(div)-2):nrow(div),])) > 1])

    #Remove any assets with any data gaps, align between market and fundamental data sets
    colNamesWithNAs <- unique(colNamesWithNAs)
    colIDsWithNAs <- which(names(SP500) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(SP500) %in% c(colNamesWithNAs))
    SP500.RET4 <- as.timeSeries(returnSeries(SP500[,colIDsWithoutNAs]))
    SP500.RET4 <- na.omit(SP500.RET4) # A single missing value will break the fPortfolio Optimizer since Sigma cannot be built...
    colnames(SP500.RET4) <- NameVect[colIDsWithoutNAs] # Col Names must be properly set up to ensure smoother and performance function find the asset names
    colIDsWithoutNAsOrig <- colIDsWithoutNAs

    colIDsWithNAs <- which(names(shsOut) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(shsOut) %in% c(colNamesWithNAs))
    shsOut <- shsOut[,colIDsWithoutNAs]

    colIDsWithNAs <- which(names(eps) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(eps) %in% c(colNamesWithNAs))
    eps <- eps[,colIDsWithoutNAs]

    colIDsWithNAs <- which(names(bookValueOfEquity) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(bookValueOfEquity) %in% c(colNamesWithNAs))
    bookValueOfEquity <- bookValueOfEquity[,colIDsWithoutNAs]

    colIDsWithNAs <- which(names(div) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(div) %in% c(colNamesWithNAs))
    div <- div[,colIDsWithoutNAs]

    # Note: Add back SP500 (colid 1) from the colID list ...
    colIDsWithoutNAs <- c(1,colIDsWithoutNAsOrig)

    div <- na.fill(div,0)

    spSpec <- portfolioSpec()
    spConstraints <- c(paste("minW[1:",(ncol(div)+1),"] = -0.8",sep=""), #BFR list fix: NAssets
                       paste("maxW[1:",(ncol(div)+1),"] = 0.8", sep="")) #"LongOnly" #BFR list fix: NAssets
    spBacktest <- portfolioBacktest()

    # Custom portfolio strategy definition
    # Note that the price data needs to exclude the benchmark for backtesting...
    # TODO: verify if the monthly myBlStrat calculations conflict with potentially non-monthly backtesting/rebalancing
    # TODO: check by BL only adjusts weights once... and verify that regurts are only calculated for the prior periods when backtesting is performed.
    setStrategyFun(spBacktest) <- "BlackLittermanFundamental"
    setStrategyParams(spBacktest) <- list(OptType = "CVAR",
                                          Est = "1",
                                          Solver = "1",
                                          PriceData = na.omit(SP500[,colIDsWithoutNAs])[,2:ncol(na.omit(SP500[,colIDsWithoutNAs]))],
                                          BenchmarkData = monthlyBenchmarkPrices,
                                          Rfr = rfr,
                                          ShsOut = shsOut,
                                          Eps = eps,
                                          BookValueOfEquity= bookValueOfEquity,
                                          Div = div)

    setWindowsHorizon(spBacktest) <- "12m"
    setSmootherLambda(spBacktest) <- "6m"
    spFormula <- (paste(paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))
    #spFormula <- as.Formula((paste("SP500 ~ ",paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))) # must match colname in data set es the first which is index
    #spFormula <- SP500~A+AA+AAPL+ABC+ABT+ACE+ACN+ACT+ADBE+ADI+ADM+ADP+ADS+ADSK+AEE+AEP+AES+AET+AFL+AGN+AIG+AIV+AIZ+AKAM+ALL+ALTR+ALXN+AMAT+AME+AMGN+AMP+AMT+AMZN+AN+AON+APA+APC+APD+APH+ARG+ATI+AVB+AVP+AVY+AXP+AZO+BA+BAC+BAX+BBBY+BBT+BBY+BCR+BDX+BEN+BF_B+BHI+BIIB+BK+BLK+BLL+BMS+BMY+BRCM+BRK_B+BSX+BTU+BWA+BXP+C+CA+CAG+CAH+CAM+CAT+CB+CBG+CBS+CCE+CCI+CCL+CELG+CERN+CF+CHK+CHRW+CI+CINF+CL+CLX+CMA+CMCSA+CME+CMG+CMI+CMS+CNP+CNX+COF+COG+COH+COL+COP+COST+CPB+CRM+CSC+CSCO+CSX+CTAS+CTL+CTSH+CTXS+CVC+CVS+CVX+D+DD+DE+DGX+DHI+DHR+DIS+DISCA+DLTR+DNB+DNR+DO+DOV+DOW+DRI+DTE+DTV+DUK+DVA+DVN+EA+EBAY+ECL+ED+EFX+EIX+EL+EMC+EMN+EMR+EOG+EQR+EQT+ESRX+ESS+ESV+ETFC+ETN+ETR+EW+EXC+EXPD+EXPE+F+FAST+FCX+FDO+FDX+FE+FFIV+FIS+FISV+FITB+FLIR+FLR+FLS+FMC+FOSL+FOXA+FRX+FSLR+FTI+FTR+GAS+GCI+GD+GE+GGP+GHC+GILD+GIS+GLW+GMCR+GME+GNW+GPC+GPS+GRMN+GS+GT+GWW+HAL+HAR+HAS+HBAN+HCBK+HCN+HCP+HD+HES+HIG+HOG+HON+HOT+HP+HPQ+HRB+HRL+HRS+HSP+HST+HSY+HUM+IBM+ICE+IFF+INTC+INTU+IP+IPG+IR+IRM+ISRG+ITW+IVZ+JBL+JCI+JEC+JNJ+JNPR+JOY+JPM+JWN+K+KEY+KIM+KLAC+KMB+KMX+KO+KR+KSS+KSU+L+LB+LEG+LEN+LH+LLL+LLTC+LLY+LM+LMT+LNC+LOW+LRCX+LUK+LUV+M+MA+MAC+MAR+MAS+MAT+MCD+MCHP+MCK+MCO+MDLZ+MDT+MET+MHFI+MHK+MKC+MMC+MMM+MNST+MO+MON+MOS+MRK+MRO+MS+MSFT+MSI+MTB+MU+MUR+MWV+MYL+NBL+NBR+NDAQ+NE+NEE+NEM+NFLX+NFX+NI+NKE+NOC+NOV+NRG+NSC+NTAP+NTRS+NU+NUE+NVDA+NWL+OI+OKE+OMC+ORCL+ORLY+OXY+PAYX+PBCT+PBI+PCAR+PCG+PCL+PCLN+PCP+PDCO+PEG+PEP+PETM+PFE+PFG+PG+PGR+PH+PHM+PKI+PLD+PLL+PNC+PNR+PNW+POM+PPG+PPL+PRGO+PRU+PSA+PVH+PWR+PX+PXD+QCOM+R+RAI+RDC+REGN+RF+RHI+RHT+RIG+RL+ROK+ROP+ROST+RRC+RSG+RTN+SBUX+SCG+SCHW+SE+SEE+SHW+SIAL+SJM+SLB+SNA+SNDK+SO+SPG+SPLS+SRCL+SRE+STI+STJ+STT+STX+STZ+SWK+SWN+SWY+SYK+SYMC+SYY+T+TAP+TE+TEG+TGT+THC+TIF+TJX+TMK+TMO+TROW+TRV+TSCO+TSN+TSO+TSS+TWX+TXN+TXT+TYC+UA+UNH+UNM+UNP+UPS+URBN+USB+UTX+VAR+VFC+VIAB+VLO+VMC+VNO+VRSN+VRTX+VTR+VZ+WAG+WAT+WDC+WEC+WFC+WFM+WHR+WIN+WLP+WM+WMB+WMT+WU+WY+WYN+WYNN+X+XEC+XEL+XL+XLNX+XOM+XRAY+XRX+YHOO+YUM+ZION+ZMH
    funk <- c("","",""); funk[2] <- "SP500"; funk[3] <- "~"; funk[3] <- spFormula # where spFormula does not include "SP500~" which works now it only complains aboutthe benchmarkName!!!
    spPortfolios <- portfolioBacktesting(formula = funk, data = SP500.RET4, spec = spSpec,
                                         constraints = spConstraints, backtest = spBacktest, trace = FALSE)
    setSmootherInitialWeights(spBacktest) <- rep(1/(ncol(SP500.RET4)-1), ncol(SP500.RET4)-1) # This is where an investor would insert his current allocation
    spSmooth <- portfolioSmoothing(object = spPortfolios, backtest =spBacktest)
    # Write results to storrage
    write.csv(spSmooth$stats,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_stats_",Sys.time(),".csv",sep = ""))
    write(paste(as.character(spSmooth$backtest@windows), as.character(spSmooth$backtest@strategy), as.character(spSmooth$backtest@smoother), sep = "NEXT_SPEC_ITEM"),
          paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_spec_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$smoothWeights,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_weights_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$P,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_monthly_portfolio_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$data,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyBenchmark,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_benchmark_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyAssets,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_asset_returns_",Sys.time(),".csv",sep = ""))
    write.csv(div,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_dividends_per_share_",Sys.time(),".csv",sep = ""))
    write.csv(eps,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_earnings_per_shares_",Sys.time(),".csv",sep = ""))
    write.csv(shsOut,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_shares_outstanding_",Sys.time(),".csv",sep = ""))
    write.csv(bookValueOfEquity,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_bookValueOfEquity_",Sys.time(),".csv",sep = ""))
    # TODO: Write the fundamental data to a file too
    # Plot results to screen
    backtestPlot(spSmooth)
    spNetPerf <- netPerformance(spSmooth)
    spBackTestStats <- backtestStats(spSmooth, FUN ="myCDaR")

    # Prepare return object
    myProposalAnalysis <- c()
    myProposalAnalysis$spPortfolios <- spPortfolios
    myProposalAnalysis$spSmooth <- spSmooth
    myProposalAnalysis$spNetPerf <- spNetPerf
    myProposalAnalysis$spBackTestStats <- spBackTestStats

    return(myProposalAnalysis)

    # TODO: save backtest results and combine with returns/fundamental forecasts at that time...
    # spSmooth$smoothWeights
    # spSmooth$stats
    # spSmooth$portfolioReturns # Really the cumulative returns since inception ...
  }
  if(type == "BlackLitermanVECM") {
    # TODO: extend Quandel data sourcing, bunch of 50 symbols at a time should work
    # Gets the 11th column (adust. close price) for first 1:10 SP500 symbols starting 1.1.07 as zoo type from the WIKI database
    # quandldata = Quandl(c(paste("WIKI/",SP500_Comp[1:10,1],".11",sep="")),type="zoo",start_date = "2007-01-03", authcode = yourQuandlCode)
    # myBench <- Quandl("YAHOO/INDEX_GSPC",type="zoo",start_date = "2007-01-03", authcode = yourQuandlCode)
    # SP500 <- merge(myBench[,6],oldQuandldata)
    # names(SP500) <- c("SP500", as.character(SP500_Comp$Symbol[1:50]))
    #
    # TODO: Make investment universe user defined
    #  data(EuStockMarkets)
    # myBench <- Quandl("YAHOO/INDEX_GSPC",type="zoo",start_date = "1991-05-01", end_date = "1998-09-08", authcode = yourQuandlCode)
    #  SP500 <- merge(myBench[1:1860,6],zoo(EuStockMarkets,index(myBench)))
    # names(SP500) <- c("SP500", colnames(EuStockMarkets))
    # TODO: Add a universe from iShares ETFs:
    #     markets <- c("EWA", "EWK", "EWZ", "EWC", "ECH", "FXI", "ICOL", "EWQ", "EWG",
    #                  "EWH", "INDA", "EIDO", "EWI", "EWJ", "EWM", "EWW", "EWN", "EPHE",
    #                  "IWM", "EWS","EZA", "EWP", "EWD", "EWL", "THD", "TUR",
    #                  "EWU", "EUSA")
    # TODO: parametrize past horizon (i.e., based on investment horizon)

    # Market data retrieval
    # TODO: eleminiate 10 symbol restriction... but observe that VECM return estimate would either break then or need either daily or longer history
    # TODO: at some point get this from Quandl too, would need to identify primary listing through SEC data lookup and then trieve price from Google, don't forget to include SP500 in here!
    SP500 <- BuildPriceTimeSeries(benchmarkSymbol = "SP500", universeSymbolList = as.character(SP500_Comp$Symbol[1:10]), fileDir = "~/Documents/Financial data/SP/")
    SP500 <- window(SP500,start = as.Date(start_date),end = as.Date(end_date))
    NameVect <- names(SP500)
    NAssets <- length(names(SP500))-1 # Symbols minus benchmark = number of assets
    colNamesWithNAs <- colnames(SP500)[colSums(is.na(SP500[1,])) > 0]
    rfr= 0.01
    monthlyBenchmarkPrices <- Quandl("YAHOO/INDEX_GSPC", collapse="monthly",type="zoo", start_date = start_date, end_date = end_date, authcode = yourQuandlCode)

    # Fundamental data retrieval
    # BFR list fix: collapse="annual" as used instead of collapse="monthly" and annual data "_A" was fetched instead of quarterly "_Q"
    monthlyStdTimeIdx <- (as.zoo(as.yearmon(seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = "month")))) # BFR list fix: nothing
    index(monthlyStdTimeIdx) <- as.yearmon(seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = "month")) # BFR list fix: nothing



    #Remove any assets with any data gaps, align between market and fundamental data sets
    colNamesWithNAs <- unique(colNamesWithNAs)
    colIDsWithNAs <- which(names(SP500) %in% colNamesWithNAs)
    colIDsWithoutNAs <- which(!names(SP500) %in% c(colNamesWithNAs))
    SP500.RET4 <- as.timeSeries(returnSeries(SP500[,colIDsWithoutNAs]))
    SP500.RET4 <- na.omit(SP500.RET4) # A single missing value will break the fPortfolio Optimizer since Sigma cannot be built...
    colnames(SP500.RET4) <- NameVect[colIDsWithoutNAs] # Col Names must be properly set up to ensure smoother and performance function find the asset names
    colIDsWithoutNAsOrig <- colIDsWithoutNAs

    spSpec <- portfolioSpec()
    spConstraints <- c(paste("minW[1:",(ncol(div)+1),"] = -0.8",sep=""), #BFR list fix: NAssets
                       paste("maxW[1:",(ncol(div)+1),"] = 0.8", sep="")) #"LongOnly" #BFR list fix: NAssets
    spBacktest <- portfolioBacktest()

    # Custom portfolio strategy definition
    # Note that the price data needs to exclude the benchmark for backtesting...
    # TODO: verify if the monthly myBlStrat calculations conflict with potentially non-monthly backtesting/rebalancing
    # TODO: check by BL only adjusts weights once... and verify that regurts are only calculated for the prior periods when backtesting is performed.
    setStrategyFun(spBacktest) <- "BlackLittermanVECM"
    setStrategyParams(spBacktest) <- list(OptType = "CVAR",
                                          Est = "1",
                                          Solver = "1",
                                          PriceData = na.omit(SP500[,colIDsWithoutNAs])[,2:ncol(na.omit(SP500[,colIDsWithoutNAs]))],
                                          BenchmarkData = monthlyBenchmarkPrices)

    setWindowsHorizon(spBacktest) <- "12m"
    setSmootherLambda(spBacktest) <- "6m"
    spFormula <- (paste(paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))
    #spFormula <- as.Formula((paste("SP500 ~ ",paste(NameVect[colIDsWithoutNAs[2:length(colIDsWithoutNAs)]],collapse="+")))) # must match colname in data set es the first which is index
    #spFormula <- SP500~A+AA+AAPL+ABC+ABT+ACE+ACN+ACT+ADBE+ADI+ADM+ADP+ADS+ADSK+AEE+AEP+AES+AET+AFL+AGN+AIG+AIV+AIZ+AKAM+ALL+ALTR+ALXN+AMAT+AME+AMGN+AMP+AMT+AMZN+AN+AON+APA+APC+APD+APH+ARG+ATI+AVB+AVP+AVY+AXP+AZO+BA+BAC+BAX+BBBY+BBT+BBY+BCR+BDX+BEN+BF_B+BHI+BIIB+BK+BLK+BLL+BMS+BMY+BRCM+BRK_B+BSX+BTU+BWA+BXP+C+CA+CAG+CAH+CAM+CAT+CB+CBG+CBS+CCE+CCI+CCL+CELG+CERN+CF+CHK+CHRW+CI+CINF+CL+CLX+CMA+CMCSA+CME+CMG+CMI+CMS+CNP+CNX+COF+COG+COH+COL+COP+COST+CPB+CRM+CSC+CSCO+CSX+CTAS+CTL+CTSH+CTXS+CVC+CVS+CVX+D+DD+DE+DGX+DHI+DHR+DIS+DISCA+DLTR+DNB+DNR+DO+DOV+DOW+DRI+DTE+DTV+DUK+DVA+DVN+EA+EBAY+ECL+ED+EFX+EIX+EL+EMC+EMN+EMR+EOG+EQR+EQT+ESRX+ESS+ESV+ETFC+ETN+ETR+EW+EXC+EXPD+EXPE+F+FAST+FCX+FDO+FDX+FE+FFIV+FIS+FISV+FITB+FLIR+FLR+FLS+FMC+FOSL+FOXA+FRX+FSLR+FTI+FTR+GAS+GCI+GD+GE+GGP+GHC+GILD+GIS+GLW+GMCR+GME+GNW+GPC+GPS+GRMN+GS+GT+GWW+HAL+HAR+HAS+HBAN+HCBK+HCN+HCP+HD+HES+HIG+HOG+HON+HOT+HP+HPQ+HRB+HRL+HRS+HSP+HST+HSY+HUM+IBM+ICE+IFF+INTC+INTU+IP+IPG+IR+IRM+ISRG+ITW+IVZ+JBL+JCI+JEC+JNJ+JNPR+JOY+JPM+JWN+K+KEY+KIM+KLAC+KMB+KMX+KO+KR+KSS+KSU+L+LB+LEG+LEN+LH+LLL+LLTC+LLY+LM+LMT+LNC+LOW+LRCX+LUK+LUV+M+MA+MAC+MAR+MAS+MAT+MCD+MCHP+MCK+MCO+MDLZ+MDT+MET+MHFI+MHK+MKC+MMC+MMM+MNST+MO+MON+MOS+MRK+MRO+MS+MSFT+MSI+MTB+MU+MUR+MWV+MYL+NBL+NBR+NDAQ+NE+NEE+NEM+NFLX+NFX+NI+NKE+NOC+NOV+NRG+NSC+NTAP+NTRS+NU+NUE+NVDA+NWL+OI+OKE+OMC+ORCL+ORLY+OXY+PAYX+PBCT+PBI+PCAR+PCG+PCL+PCLN+PCP+PDCO+PEG+PEP+PETM+PFE+PFG+PG+PGR+PH+PHM+PKI+PLD+PLL+PNC+PNR+PNW+POM+PPG+PPL+PRGO+PRU+PSA+PVH+PWR+PX+PXD+QCOM+R+RAI+RDC+REGN+RF+RHI+RHT+RIG+RL+ROK+ROP+ROST+RRC+RSG+RTN+SBUX+SCG+SCHW+SE+SEE+SHW+SIAL+SJM+SLB+SNA+SNDK+SO+SPG+SPLS+SRCL+SRE+STI+STJ+STT+STX+STZ+SWK+SWN+SWY+SYK+SYMC+SYY+T+TAP+TE+TEG+TGT+THC+TIF+TJX+TMK+TMO+TROW+TRV+TSCO+TSN+TSO+TSS+TWX+TXN+TXT+TYC+UA+UNH+UNM+UNP+UPS+URBN+USB+UTX+VAR+VFC+VIAB+VLO+VMC+VNO+VRSN+VRTX+VTR+VZ+WAG+WAT+WDC+WEC+WFC+WFM+WHR+WIN+WLP+WM+WMB+WMT+WU+WY+WYN+WYNN+X+XEC+XEL+XL+XLNX+XOM+XRAY+XRX+YHOO+YUM+ZION+ZMH
    funk <- c("","",""); funk[2] <- "SP500"; funk[3] <- "~"; funk[3] <- spFormula # where spFormula does not include "SP500~" which works now it only complains aboutthe benchmarkName!!!
    spPortfolios <- portfolioBacktesting(formula = funk, data = SP500.RET4, spec = spSpec,
                                         constraints = spConstraints, backtest = spBacktest, trace = FALSE)
    setSmootherInitialWeights(spBacktest) <- rep(1/(ncol(SP500.RET4)-1), ncol(SP500.RET4)-1) # This is where an investor would insert his current allocation
    spSmooth <- portfolioSmoothing(object = spPortfolios, backtest =spBacktest)
    # Write results to storrage
    write.csv(spSmooth$stats,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_stats_",Sys.time(),".csv",sep = ""))
    write(paste(as.character(spSmooth$backtest@windows), as.character(spSmooth$backtest@strategy), as.character(spSmooth$backtest@smoother), sep = "NEXT_SPEC_ITEM"),
          paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_spec_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$smoothWeights,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_weights_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$P,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_backtest_monthly_portfolio_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$data,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_input_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyBenchmark,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_benchmark_returns_",Sys.time(),".csv",sep = ""))
    write.csv(spSmooth$monthlyAssets,paste(file_root_dir,"/OUTPUT/",userid,"_",type,"_monthly_asset_returns_",Sys.time(),".csv",sep = ""))
    # Plot results to screen
    backtestPlot(spSmooth)
    spNetPerf <- netPerformance(spSmooth)
    spBackTestStats <- backtestStats(spSmooth, FUN ="myCDaR")

    # Prepare return object
    myProposalAnalysis <- c()
    myProposalAnalysis$spPortfolios <- spPortfolios
    myProposalAnalysis$spSmooth <- spSmooth
    myProposalAnalysis$spNetPerf <- spNetPerf
    myProposalAnalysis$spBackTestStats <- spBackTestStats

    return(myProposalAnalysis)

    # TODO: save backtest results and combine with returns/fundamental forecasts at that time...
    # spSmooth$smoothWeights
    # spSmooth$stats
    # spSmooth$portfolioReturns # Really the cumulative returns since inception ...
  }
}


