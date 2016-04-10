# PortfolioControler.R

# TODO: remove issude with time horizon seting wheteh the length setting using as.Date(.., format(%Y)) does not work as expected!!

## Run time control parameters
# setwd("/home/rstudio") #TODO: uncomment for deployment
currentDate <- as.Date(Sys.Date())
executionPath <- "../"
source(paste(executionPath,"R/EquityPortfolioBuilder.R",sep=""))
source(".pwd")
Quandl.api_key(yourQuandlCode)
debugonce(PortfolioConstruction)
start <- TRUE # If true this will start a regular user visit to the site
initialize <- TRUE # DANGER: if TRUE this will delet all previous users and reset to the initial sample user!

## Define data structures
# All settings can be changed in investment screen; Different degrees of information is collected depending on how deep the sign-up process proceeds
# TODO: add historic horizons for comparision when changes were made and virtual horizon if changes are made in user settings that would have changed the horizon but user declined the changes
investmentHorizons <- data.frame(t(rep(0,23)))
colnames(investmentHorizons) <- c("UserID", # Globally unique ID of client
                                  "HorizonID", # Auto increment integer, unique ID within each client setting: 1 (default)
                                  "HorizonGoalName", # String to allow client to identify the horizon: Retirement saving (default)
                                  "InvestStartDate", # Date as YYYY-MM-DD: now() (default) or user specified
                                  "InvestDurationYears", # Decimal: now() to RetirementAge or user specified or suggested duration of users with similar HorizonGoalName
                                  "InitialInvestAmt", # Decimal: TotalDisclosedWealth - SUM(investmentHorizons$InitialInvestAmt[UserID==userID] - LiquidityNeedsAmt or user specified or suggested % of TotalDiscloasedWealth of users with similar HorizonGoalName
                                  "PeriodIncomeAmt", # Decimal: TotalDisclosedIncome or user specified or suggested Pct of TotalDisclaosedIncome to TotalDisclosedWealth of users with similar HorizonGoalName
                                  "PeriodSaveRate", # Savings rate: .20 (default) or suggested rate of users with similar age, income, wealth or get OECD average for domicile country
                                  "PeriodInflation", # Inflation rate over the investment horizon: 0.02 (default), or get target inflaction per domicile country or user input
                                  "PeriodGovYield", # Government bonds yield: 0.02 (default), or get burrent yield over the length of the chosen investment huration or user input
                                  "ReturnTargetPct", # Return rate: (ReturnTargetAmt/InitialInvestmentAmt)/(1-CapitalTaxRate) or Accrual-Equivalent-After-Tax-Returns or user specified
                                  "SingleTargetAmt", # Determine if client saves to receive an annuity of a lump sum: TRUE (default), FALSE
                                  "ReturnTargetAmt", # If SingleTargetAmt is TRUE set return amount to global MSCI 10y return (default) or ask for input  else: Present value of 60% of TotalDisclosedIncome for the duration of years after RetirementAge for the CurrentAge cohort at discount rate of 10year inflation+goverment bond returns in domicile country, ask for user specification of PV inputs
                                                     # assumedInf <- 0.01; assumedGovRate=0.015; assumedInc <- 100000; assumedCF <- rep(((assumedInc)*(1+assumedInf)^10)*(1-newPeriodSaveRate),times = 10); sum(sapply(seq(from=1,to = 10, by=1), function(x) (assumedCF[x]*(1+assumedInf)^x)/(1+assumedGovRate)^x)) # TODO: get historic government bond yields and inflation rates, use expected life for times=/to= values
                                  "RiskAbility", # Expessed in standard deviation of ActiveInvestmentUniverse multiples: 1 (default), 2 (TotalDisclosedWealth/TotalDisclosedIncome > 10)
                                  "RiskWillingness", # Expessed in standard deviation of ActiveInvestmentUniverse multiples: 1 (default)
                                  "RiskTollerance", # Min(RiskAbility,RiskWillingness) or user specified with a warning
                                  "IncomeTaxRate", # Determined by DomicileCountry or user specified or determined from users with similar domicile, wealth, income, age
                                  "CapitalTaxRate", # Determined by DomicileCountry or user specified or determined from users with similar domicile, wealth, income, age
                                  "SectorExclusionList", # List of industry names to exclude when selecting the investment universe: {}(default)
                                  "SectorReturnList", # List of returns for specific investment assets: {}(default)
                                  "LiquidityNeedsAmt", # Ammount required to fund period of living: 3 months TotalDisclosedIncome (default) or user specified
                                  "ActiveInvestUniverse", # Determined after a backtested configuration has been chosen: SectorETF (default)
                                  "ActiveInvestStrategy") # Combined with ActiveInvestUniverse to define optimization process : statistical (default)

# All settings can be changed in user setting screen; Different degrees of information is collected depending on how deep the sign-up process proceeds
existingUsers <- data.frame(t(rep(0,23)))
colnames(existingUsers) <-c("UserID", # Globally unique ID of client: cookieID (default), unique client ID
                            "TotalDisclosedWealth", # How much the client says he owns
                            "TotalDisclosedIncome", # How much the client says he earnes per year
                            "SignUpStatus", # If pending, load all previously specified settings: pending (default), user
                                  "CurrentAge", # Current age in years of client
                                  "BirthDate", # Optional exact specification of client birth date
                                  "RetirementAge", # When client plans to stop earning an income: 65 (default)
                            "LifeExpectancy", # Expected years before death at time of birth: WorldBank estimate for the country or world everage or user input
                                  "ProfileType", # Specifies how much engagement the use takes, derived during sign up or changed in user settings: basic (default, warn user about incomplete investor profile), detailed or advanced
                                  "InvestType", # If individualist or spontaneous allow more risk willingness, determined by games, trading pattern or user settings: cautious (default), methodical, individualistic, spontaneous
                                  "ClientType", # Largely matches investor type and determines which display and features are active and defaulted: adventurer (default, cautious), warrior (spontaneous), king (methodical and individualistic)
                                  "ExhibitLossAversion", # If true warn of too conservative choices, determined by games, trading pattern or user settings: FALSE (default), TRUE
                                  "ExhibitBiasedExpectations", # If true warn of irational risk taking, determined by games, trading pattern or user settings: FALSE (default), TRUE
                                  "ExhibitMentalAccounting", # If true warn of portfolio context impact, trading pattern or user settings: FALSE (default), TRUE
                                  "CurrentStageInLife", # The more early stage the more risk willingness allowed: foundation (default), accumulation, maintenance, distribution
                                  "SourceOfWealth", # If entrepreneur or investor allow more risk willingness: income (default), inherited, windfall, investor, entrepreneur
                                  "DomicileCountry", # Depending on domicile we may not offer services or different types of service or if we require special legal treatment
                                  "AccountType", # Determines whether we bill user or not: free (default), basic, advanced
                                  "EMail", # Primary client communication channel, determined form Google profile
                                  "MobileNr", # Secondary client communication and required for two way authentication
                                  "ActiveTwoFactorAuth", # Whether client wants to receive an SMS code before a log in is authorized: FALSE (default), TRUE
                                  "ActiveAutoTrade", # Whether client want to execute trades automaticall: FALSE (default), TRUE
                                  "ApplicableBroker") # Determines cost models when estimating trading cost: InteraktiveBroker, SwissQuote, etc.
## Load support data sets
if(initialize) {
  existingUsers <- rbind(existingUsers, c(42, 100000, 100000,"user", 42, "",65, 72, "basic", "cautious", "adventurer", FALSE, FALSE, FALSE,
                                          "foundation", "income", "CHE", "free", "ProbStub@users.noreply.github.com", "0012345678901", FALSE, FALSE,
                                          "InteractiveBroker"))
  investmentHorizons <- rbind(investmentHorizons, c(42, 1, "Retirement saving", "2015-08-15", 24, 100000, 100000, .20, 0.02, 0.02, TRUE, 2000000,
                                                    .13, 1, 1, 1, .20, .20, "", "", 25000, "SectorETF", "statistical"))
}
if(!initialize) {
  existingUsers <- dget(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))
}

taxRates <- read.csv(paste(executionPath,"INPUT/TaxRatesOECD2013.csv", sep=""), stringsAsFactors=FALSE)
lifeExpectancy <- dget(paste(executionPath, "INPUT/lifeExpectancy.data.R",sep=""))
savingRate <- dget(paste(executionPath, "INPUT/savingRate.data.R",sep=""))
govBondYield <- dget(paste(executionPath, "INPUT/govBondYield.data.R",sep=""))
histInflationRate <- dget(paste(executionPath, "INPUT/histInflationRate.data.R",sep=""))


# existingUsers <- list() # Age, InvestableAssets, RiskAversion, ReturnTarget, ... PersonaliytType, StafeInLife, SourceofWealth
# existingUsers[[42]] <- c(42,100000,"statisticalSectorETF")
# genericRetirementAge <- 65
# genericInvestmentUniverse <- "statisticalSectorETF"

## Start user interaction
if(start==TRUE) {
  userid <- readline("If you are an existing user, tell your user ID, else pick a user ID to start:") # TODO: Will be discovered through cookie and subsequent Google/FB sign-on, ensure that UserID are unique

  if(!is.vector(existingUsers[[userid]])){
    userage <- as.numeric(readline("Age: "))
    userinvestableassets <- as.numeric(readline("Investable assets: "))
    userincome <- as.numeric(readline("Annual income before tax: ")) # TODO: Guessed from location income level.
    userdomicile <- "CHE" # TODO: guessed from user IP address location

    # Create a BASIC user entry based on the information provided
    newUserID <- userid
    newTotalDisclosedWealth <- userinvestableassets
    newTotalDisclosedIncome <- userincome
    newDomicileCountry <- userdomicile # TODO: user input
    newSignUpStatus <- "pending"
    newCurrentAge <- userage
    newBirthDate <- "" # TODO: user input
    newRetirementAge <- 65 # TODO: user input
    # life expectancy determination
    if( length(lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]) == 1 && newCurrentAge >=3) {
      newLifeExpectany <- lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]
    }
    if( length(lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]) != 1 && newCurrentAge >=3) {
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newLifeExpectany <- first((lifeExpectancy[,c(newDomicileCountry)]))
      }
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average WLD
        newLifeExpectany <- first((lifeExpectancy[,c("WLD")]))
      }
    }
    if(newCurrentAge <=2 ){ # If 3rd birth date has not been reached take the latest life expectancy data available
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newLifeExpectany <- last((lifeExpectancy[,c(newDomicileCountry)]))
      }
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average WLD
        newLifeExpectany <- last((lifeExpectancy[,c("WLD")]))
      }
    }
    newProfileType <- "basic" # TODO: user input
    newInvestType <- "cautious" # TODO: Guessing from game or social media
    newClientType <- "adventurer" # TODO: Guessing from game or social media
    newExhibitLossAversion <- FALSE # TODO: Guessing from game or social media
    newExhibitBiasedExpectations <- FALSE # TODO: Guessing from game or social media
    newExhibitMentalAccounting <- FALSE # TODO: Guessing from game or social media
    newCurrentStageInLife <- "foundation" # TODO: Guessing from game or social media
    newSourceOfWealth <- "income" # TODO: Guessing from game or social media
    newAccountType <- "free" # TODO: user input
    newEmail <- "NA" # TODO: user input
    newMobileNr <- "NA" # TODO: user input
    newActiveTwoFactorAuth <- FALSE # TODO: user input
    newActiveAutoTrade <- FALSE # TODO: user input
    newApplicableBroker <- "NA" # TODO: user input
    existingUsers <- rbind(existingUsers, c(newUserID, newTotalDisclosedWealth, newTotalDisclosedIncome, newSignUpStatus, newCurrentAge, newBirthDate,
                                            newRetirementAge, newLifeExpectany, newProfileType, newInvestType, newClientType, newExhibitLossAversion,
                                            newExhibitBiasedExpectations, newExhibitMentalAccounting, newCurrentStageInLife,
                                            newSourceOfWealth, newDomicileCountry, newAccountType, newEmail, newMobileNr, newActiveTwoFactorAuth,
                                            newActiveAutoTrade, newApplicableBroker))
    # Generate a BASIC investment horizon for the new user
    newHorizonID <- 1
    newHorizonGoalName <- "Retirement savings"
    newInvestStartDate <- as.Date(Sys.Date())
    newInvestDurationYears <- newRetirementAge-newCurrentAge # TODO: user input
    newInitialInvestAmt <- newTotalDisclosedWealth # TODO: user input
    newPeriodIncome <- newTotalDisclosedIncome # TODO: user input
    # Tax rates determination
    if(length(taxRates[taxRates$Code %in% c(newDomicileCountry),1])==1) {
      newIncomeTaxRate <- taxRates[taxRates$Code %in% c(newDomicileCountry),3]/100 # TODO: user input
      newCapitalTaxrate <- taxRates[taxRates$Code %in% c(newDomicileCountry),4]/100 # TODO: user input
    }
    if(length(taxRates[taxRates$Code %in% c(newDomicileCountry),1])!=1) {
      newIncomeTaxRate <- (colSums(taxRates[,3:4])/nrow(taxRates))[1]/100 # TODO: user input
      newCapitalTaxrate <- (colSums(taxRates[,3:4])/nrow(taxRates))[2]/100 # TODO: user input
    }
    # Savings rate determination
    if( length(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodSaveRate <- as.numeric(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(savingRate[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodSaveRate <- as.numeric(last((na.trim(savingRate)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(savingRate[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodSaveRate <- as.numeric((rowSums(last(na.trim(savingRate)))/ncol(savingRate))/100) # TODO: user input
      }
    }
    # Inflation rate determination
    if( length(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodInflation <- as.numeric(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(histInflationRate[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodInflation <- as.numeric(last((na.locf(histInflationRate)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(histInflationRate[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodInflation <- as.numeric((rowSums(last(na.locf(histInflationRate)))/ncol(histInflationRate))/100) # TODO: user input
      }
    }
    # Government bond yield determination
    if( length(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodGovYield <- as.numeric(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(govBondYield[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodGovYield <- as.numeric(last((na.locf(govBondYield)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(govBondYield[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodGovYield <- as.numeric((rowSums(last(na.locf(govBondYield)))/ncol(govBondYield))/100) # TODO: user input
      }
    }
    newSingleTargetAmt <- TRUE # TODO: user input
    if(newSingleTargetAmt) { # If nothing else is specified we assume a single lump sum for retirement
      assumedSpendingDuration <- as.numeric(round(newLifeExpectany-newRetirementAge,0))
      # First compute the annual income required at the time of retirenment (end of investment period) and strech out the payment for the expected life
      assumedCF <- rep(((newPeriodIncome)*(1+newPeriodInflation)^newInvestDurationYears)*(1-newPeriodSaveRate),times = assumedSpendingDuration);
      newReturnTargetAmt <- sum(sapply(seq(from=1,to = assumedSpendingDuration, by=1), function(x) (assumedCF[x]*(1+newPeriodInflation)^x)/(1+newPeriodGovYield)^x))
    }
    if(!newSingleTargetAmt) {
      # TODO: ask for length of funds usage in years
    }
    newReturnTargetPct <- ((newReturnTargetAmt/newInitialInvestAmt)^(1/newInvestDurationYears)-1)*(1-newCapitalTaxrate) # TODO: user input # Note: target amount takes account for inflation, no further addition required
    newRiskAbility <- 1 # TODO: user input
    newRiskWillingness <- 1 # TODO: user input
    newRiskTollerance <- min(newRiskAbility, newRiskWillingness)

    newSectorExclusionList <- "NA" # TODO: user input
    newSectorReturnList <- "NA" # TODO: user input
    newLiquidityNeedsAmt <- (newPeriodIncome/12)*2 # TODO: user input
    newActiveInvestUniverse <- "SectorETF" # TODO: user input
    newActiveInvestStrategy <- "statistical" # TODO: user input
    investmentHorizons <- rbind(investmentHorizons,c(newUserID, newHorizonID, newHorizonGoalName, newInvestStartDate, newInvestDurationYears,
                                                     newInitialInvestAmt, newPeriodIncome, newPeriodSaveRate, newPeriodInflation, newPeriodGovYield,
                                                     newReturnTargetPct,newSingleTargetAmt, newReturnTargetAmt, newRiskAbility, newRiskWillingness, newRiskTollerance,
                                                     newIncomeTaxRate, newCapitalTaxrate, newSectorExclusionList, newSectorReturnList,
                                                     newLiquidityNeedsAmt, newActiveInvestUniverse, newActiveInvestStrategy))
    writeLines("Thank you! Base on the data collected we have fomulated the following
               profile which we will be using to create an investment proposal for you:")
    writeLines(sapply(seq(from=1, to=ncol(existingUsers), by=1),
                      function (x) paste(colnames(existingUsers[userid,])[x],": ", existingUsers[existingUsers[,1] %in% userid,x],sep="")))
    writeLines(sapply(seq(from=1, to=ncol(investmentHorizons), by=1),
                      function (x) paste(colnames(investmentHorizons[userid,])[x],": ", investmentHorizons[investmentHorizons[,1] %in% userid,x],sep="")))
    # Run a backtest simulation with basic client information
    backtestEndDate <- currentDate

    # TODO: remove the 5 year backtest limitation once longer time series data is available
    if(newInvestDurationYears>=5) {
      backtestDuration<- 5
    }
    if(newInvestDurationYears<5) {
      backtestDuration <- newInvestDurationYears
    }
    backtestStartDate <- as.Date(paste((as.numeric(format(currentDate, "%Y"))-backtestDuration), format(currentDate,"%m-%d"), sep="-"))
    PortfolioConstruction(type = paste(newActiveInvestStrategy,newActiveInvestUniverse, sep="")
                          ,start_date = backtestStartDate, end_date = backtestEndDate, userid = userid)
  }

  detailedProfileOrSignup <- readline("Would you like to provide additional information (type more) or sign-up (type signup) with us right now): ")

  if(detailedProfileOrSignup == "more")
  {
    # Create a DETAILED user entry based on the information provided
    newUserID <- userid
    newTotalDisclosedWealth <- userinvestableassets
    newTotalDisclosedIncome <- userincome
    userdomicile <- as.character(readline(paste("Your current domicile is ", newDomicileCountry,
                                                "(OK to accept; else type CHE, DEU, USA, MEX, etc): ", sep="")))
    if(userdomicile == "OK")
    {
      newDomicileCountry <- newDomicileCountry
    }
    if(userdomicile != "OK")
    {
      newDomicileCountry <- userdomicile
    }
    newSignUpStatus <- "pending"
    newCurrentAge <- userage
    newBirthDate <- "" # TODO: user input
    newRetirementAge <- 65 # TODO: user input
    # life expectancy determination
    if( length(lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]) == 1 && newCurrentAge >=3) {
      newLifeExpectany <- lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]
    }
    if( length(lifeExpectancy[as.Date(as.numeric(format(currentDate,format = "%Y"))-newCurrentAge),c(newDomicileCountry)]) != 1 && newCurrentAge >=3) {
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newLifeExpectany <- first((lifeExpectancy[,c(newDomicileCountry)]))
      }
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average WLD
        newLifeExpectany <- first((lifeExpectancy[,c("WLD")]))
      }
    }
    if(newCurrentAge <=2 ){ # If 3rd birth date has not been reached take the latest life expectancy data available
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newLifeExpectany <- last((lifeExpectancy[,c(newDomicileCountry)]))
      }
      if(length(lifeExpectancy[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average WLD
        newLifeExpectany <- last((lifeExpectancy[,c("WLD")]))
      }
    }
    newProfileType <- "basic" # TODO: user input
    newInvestType <- "cautious" # TODO: Guessing from game or social media
    newClientType <- "adventurer" # TODO: Guessing from game or social media
    newExhibitLossAversion <- FALSE # TODO: Guessing from game or social media
    newExhibitBiasedExpectations <- FALSE # TODO: Guessing from game or social media
    newExhibitMentalAccounting <- FALSE # TODO: Guessing from game or social media
    newCurrentStageInLife <- "foundation" # TODO: Guessing from game or social media
    newSourceOfWealth <- "income" # TODO: Guessing from game or social media
    newAccountType <- "free" # TODO: user input
    newEmail <- "NA" # TODO: user input
    newMobileNr <- "NA" # TODO: user input
    newActiveTwoFactorAuth <- FALSE # TODO: user input
    newActiveAutoTrade <- FALSE # TODO: user input
    newApplicableBroker <- "NA" # TODO: user input
    existingUsers <- rbind(existingUsers, c(newUserID, newTotalDisclosedWealth, newTotalDisclosedIncome, newSignUpStatus, newCurrentAge, newBirthDate,
                                            newRetirementAge, newLifeExpectany, newProfileType, newInvestType, newClientType, newExhibitLossAversion,
                                            newExhibitBiasedExpectations, newExhibitMentalAccounting, newCurrentStageInLife,
                                            newSourceOfWealth, newDomicileCountry, newAccountType, newEmail, newMobileNr, newActiveTwoFactorAuth,
                                            newActiveAutoTrade, newApplicableBroker))
    # Generate a DETAILED investment horizon for the new user
    newHorizonID <- 1
    newHorizonGoalName <- "Retirement savings"
    newInvestStartDate <- as.Date(Sys.Date()) # TODO: user input HERE
    userinvestdurationyears <- as.numeric(readline(paste("Your investment duration is assumed to be ", newInvestDurationYears,
                                                " years (OK to accept; else type length in years): ", sep="")))
    if(userinvestdurationyears == "OK")
    {
      newInvestDurationYears <- newInvestDurationYears
    }
    if(userinvestdurationyears != "OK")
    {
      newInvestDurationYears <- userinvestdurationyears
    }
    newInitialInvestAmt <- newTotalDisclosedWealth # TODO: user input HERE
    newPeriodIncome <- newTotalDisclosedIncome # TODO: user input HERE
    # Tax rates determination
    if(length(taxRates[taxRates$Code %in% c(newDomicileCountry),1])==1) {
      newIncomeTaxRate <- taxRates[taxRates$Code %in% c(newDomicileCountry),3]/100 # TODO: user input HERE
      newCapitalTaxrate <- taxRates[taxRates$Code %in% c(newDomicileCountry),4]/100 # TODO: user input HERE
    }
    if(length(taxRates[taxRates$Code %in% c(newDomicileCountry),1])!=1) {
      newIncomeTaxRate <- (colSums(taxRates[,3:4])/nrow(taxRates))[1]/100 # TODO: user input HERE
      newCapitalTaxrate <- (colSums(taxRates[,3:4])/nrow(taxRates))[2]/100 # TODO: user input HERE
    }
    # Savings rate determination
    if( length(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodSaveRate <- as.numeric(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(savingRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(savingRate[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodSaveRate <- as.numeric(last((na.trim(savingRate)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(savingRate[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodSaveRate <- as.numeric((rowSums(last(na.trim(savingRate)))/ncol(savingRate))/100) # TODO: user input
      }
    }
    # Inflation rate determination
    if( length(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodInflation <- as.numeric(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(histInflationRate[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(histInflationRate[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodInflation <- as.numeric(last((na.locf(histInflationRate)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(histInflationRate[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodInflation <- as.numeric((rowSums(last(na.locf(histInflationRate)))/ncol(histInflationRate))/100) # TODO: user input
      }
    }
    # Government bond yield determination
    if( length(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) == 1) {
      newPeriodGovYield <- as.numeric(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]/100) # TODO: user input
    }
    if( length(govBondYield[(format(currentDate,format = "%Y")),c(newDomicileCountry)]) != 1 ) {
      if(length(govBondYield[index(0),c(newDomicileCountry)]) == 1) { # Verify if there is at least some country data
        newPeriodGovYield <- as.numeric(last((na.locf(govBondYield)[,c(newDomicileCountry)]))/100) # TODO: user input
      }
      if(length(govBondYield[index(0),c(newDomicileCountry)]) != 1) { # In case of no country data default to world average
        newPeriodGovYield <- as.numeric((rowSums(last(na.locf(govBondYield)))/ncol(govBondYield))/100) # TODO: user input
      }
    }
    newSingleTargetAmt <- TRUE # TODO: user input
    if(newSingleTargetAmt) { # If nothing else is specified we assume a single lump sum for retirement
      assumedSpendingDuration <- as.numeric(round(newLifeExpectany-newRetirementAge,0))
      # First compute the annual income required at the time of retirenment (end of investment period) and strech out the payment for the expected life
      assumedCF <- rep(((newPeriodIncome)*(1+newPeriodInflation)^newInvestDurationYears)*(1-newPeriodSaveRate),times = assumedSpendingDuration);
      newReturnTargetAmt <- sum(sapply(seq(from=1,to = assumedSpendingDuration, by=1), function(x) (assumedCF[x]*(1+newPeriodInflation)^x)/(1+newPeriodGovYield)^x))
    }
    if(!newSingleTargetAmt) {
      # TODO: ask for length of funds usage in years
    }
    newReturnTargetPct <- ((newReturnTargetAmt/newInitialInvestAmt)^(1/newInvestDurationYears)-1)*(1-newCapitalTaxrate) # TODO: user input # Note: target amount takes account for inflation, no further addition required
    newRiskAbility <- 1 # TODO: user input HERE (optional and revise behaviroual parameters in the same process!)
    newRiskWillingness <- 1 # TODO: user input HERE (optional and revise behaviroual parameters in the same process!)
    newRiskTollerance <- min(newRiskAbility, newRiskWillingness)

    newSectorExclusionList <- "NA" # TODO: user input
    newSectorReturnList <- "NA" # TODO: user input
    newLiquidityNeedsAmt <- (newPeriodIncome/12)*2 # TODO: user input
    newActiveInvestUniverse <- "SectorETF" # TODO: user input
    newActiveInvestStrategy <- "statistical" # TODO: user input
    investmentHorizons <- rbind(investmentHorizons,c(newUserID, newHorizonID, newHorizonGoalName, newInvestStartDate, newInvestDurationYears,
                                                     newInitialInvestAmt, newPeriodIncome, newPeriodSaveRate, newPeriodInflation, newPeriodGovYield,
                                                     newReturnTargetPct,newSingleTargetAmt, newReturnTargetAmt, newRiskAbility, newRiskWillingness, newRiskTollerance,
                                                     newIncomeTaxRate, newCapitalTaxrate, newSectorExclusionList, newSectorReturnList,
                                                     newLiquidityNeedsAmt, newActiveInvestUniverse, newActiveInvestStrategy))
    writeLines("Thank you! Based on the data collected we have fomulated the following
               profile which we will be using to create an investment proposal for you:")
    writeLines(sapply(seq(from=1, to=ncol(existingUsers), by=1),
                      function (x) paste(colnames(existingUsers[userid,])[x],": ", existingUsers[existingUsers[,1] %in% userid,x],sep="")))
    writeLines(sapply(seq(from=1, to=ncol(investmentHorizons), by=1),
                      function (x) paste(colnames(investmentHorizons[userid,])[x],": ", investmentHorizons[investmentHorizons[,1] %in% userid,x],sep="")))


  }

  if(detailedProfileOrSignup == "signup")
  {
    # TODO: collect sign-up data
  }


  if(is.vector(existingUsers[[userid]])){
    # TODO: complete data collection for existing users.
    userage <- readline("Age: ")
    userinvestableassets <- readline("Investable assets: ")
  }
}

## save user data
dput(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))



