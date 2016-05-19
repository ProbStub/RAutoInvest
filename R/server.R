#####
# Run time control parameters
# setwd("/home/rstudio") #TODO: uncomment for deployment
executionPath <- "/home/rstudio/RAutoInvest/"
source("/home/rstudio/RAutoInvest/.pwd")
#####
# Loading required code/packages
library(shiny)
library(dygraphs)
library(uuid)
source(paste(executionPath,"/R/EquityPortfolioBuilder.R",sep=""))
source(paste(executionPath,"/R/PerformanceReporting.R",sep=""))

#####
# Global data structures

# User creditials and basic contact data of existing users
if(file.exists(paste(executionPath, "INPUT/existingUsersCredentials.data.R",sep=""))) {
  existingUsersCredentials <- dget(paste(executionPath, "INPUT/existingUsersCredentials.data.R",sep=""))
}
if(!file.exists(paste(executionPath, "INPUT/existingUsersCredentials.data.R",sep=""))) {
  existingUsersCredentials <- matrix(nrow=1,ncol=4)
  colnames(existingUsersCredentials) <- c("UserID", "UserName", "UserPassword", "UserEmail")
}

loggedInUserID <<- ""
newInvestmentHorizon <<- ""
newExistingUsers <<- ""


# Profile setting of existing users
# All settings can be changed in user setting screen; Different degrees of information is collected depending on how deep the sign-up process proceeds
if(file.exists(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))) {
  existingUsers <- dget(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))
}
if(!file.exists(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))) {
  existingUsers <- data.frame(t(rep(0,24)))
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
                              "BitCoinAddress", # The bitcoin address to use for collecting the investable funds
                              "MobileNr", # Secondary client communication and required for two way authentication
                              "ActiveTwoFactorAuth", # Whether client wants to receive an SMS code before a log in is authorized: FALSE (default), TRUE
                              "ActiveAutoTrade", # Whether client want to execute trades automaticall: FALSE (default), TRUE
                              "ApplicableBroker") # Determines cost models when estimating trading cost: InteraktiveBroker, SwissQuote, etc.
}

# Investment horizon profiles of existing users
# All settings can be changed in investment screen; Different degrees of information is collected depending on how deep the sign-up process proceeds
# TODO: add historic horizons for comparision when changes were made and virtual horizon if changes are made in user settings that would have changed the horizon but user declined the changes
if(file.exists(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))) {
  investmentHorizons <- dget(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))
}
if(!file.exists(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))) {
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
}

# TODO: add file.exists() procedure to avoid unexpected issues
taxRates <- read.csv(paste(executionPath,"INPUT/TaxRatesOECD2013.csv", sep=""), stringsAsFactors=FALSE)
lifeExpectancy <- dget(paste(executionPath, "INPUT/lifeExpectancy.data.R",sep=""))
savingRate <- dget(paste(executionPath, "INPUT/savingRate.data.R",sep=""))
govBondYield <- dget(paste(executionPath, "INPUT/govBondYield.data.R",sep=""))
histInflationRate <- dget(paste(executionPath, "INPUT/histInflationRate.data.R",sep=""))

#####
# User interaction
shinyServer(function(input, output, session) {

  # Place to store "global variables"
  rv <- reactiveValues()

  # First global variable: the default message when app opens
  rv$message <- renderText("Hey, Jude")
  rv$userAge <- NA
  rv$investAmount <- NA
  rv$userName <- NA
  rv$password <- NA
  rv$newUserName <- NA
  rv$newUserEMail <- NA
  rv$newUserBitCoinAddress <- NA
  rv$newUserPassword <- NA
  rv$isUser <- FALSE
  rv$proposal <- NA
  rv$artificialReturn <- rnorm(3650,0.0001,0.005)


  # This defines an empy element page
  emptyElement <- renderUI({
    do.call(list,
            list(
              call("invisible")
            ))
  })

  # These is the navigation bar content
  navBar1 <- renderUI({
    do.call(list,
            list(
              call("numericInput", "userAge", "Age:", rv$userAge, 0)
            ))
  })
  navBar2 <- renderUI({
    do.call(list,
            list(
              call("numericInput", "investAmount","Amount:", rv$investAmount, 0)
            ))
  })
  navBar3 <- renderUI({
    output$dummy3 <- renderText("Ready to invest?")
    do.call(list,
            list(
              call("textOutput", "dummy3"),
              call("actionButton", "proposalBasic", "Go")
            ))
  })
  navBar4 <- renderUI({
    output$dummy4 <- renderText("Already a user?")
    do.call(list,
            list(
              call("textOutput", "dummy4"),
              call("actionButton", "login", "Login")
            ))
  })

  navBar4LoggedIn <- renderUI({
    do.call(list,
            list(
              call("actionButton", "logout", "Logout")
            ))
  })

  # This is the home page content for logged-in user
  homeNewUserPage <- renderUI({
    output$debugtext <- renderText(paste("HOME NEW USER ", sep=""))
    output$startPageChart <- renderDygraph({
      dygraph(cbind("Mutual fund" = zoo(cumsum(rv$artificialReturn-(.0070/365)),as.Date("2004-01-01")+0:3650), # TER typical mutual fund 0.7% p.a.
                    "AuM based advisor" = zoo(cumsum(rv$artificialReturn-(.0025/365)),as.Date("2004-01-01")+0:3650), # AuM fees Wealthfront 0.25% p.a.
                    "Fixed fee advisor" = zoo(cumsum(rv$artificialReturn-(.00018/365)),as.Date("2004-01-01")+0:3650)), # Fixed fee USD 15 p.a. on 1'000'000 invested (0.18%),
              main = "Cumulative return of typical fund, AuM robo-advisor and us") %>%
        dyRangeSelector()
    })
    do.call(list,
            list(
              call("textOutput", "debugtext"),
              call("dygraphOutput","startPageChart")
            ))
  })

  # This is the home page content for logged-in user
  homeExistUserPage <- renderUI({

    # Fetch data
    portfolioWeights <- as.data.frame(DailyPortfolioWeightsPast(loggedInUserID))
    portfolioWeights$Date <- rownames(portfolioWeights)
    portfolioWeights <- portfolioWeights[,c(ncol(portfolioWeights), seq(1,ncol(portfolioWeights)-1,1))]
    portfolioReturns <- DailyPortfolioReturnsPast(loggedInUserID)
    cumPortfolioReturns <- CumPortfolioReturnsPast(loggedInUserID)
    benchmarkReturnsACWI <- DailyBechmarkReturnsPast("ACWI","NASDAQ",loggedInUserID)
    cumBenchmarkReturnsACWI <- CumBechmarkReturnsPast("ACWI","NASDAQ",loggedInUserID)

    largestCommonIndex <- index(na.omit(cbind(portfolioReturns[,1], benchmarkReturnsACWI)))

    # Link dats to visual elements
    output$debugtext <- renderText(paste("HOME EXISTING USER SIGNIN", loggedInUserID, sep=""))

    output$homeExistUserPageDailyReturnChart <- renderDygraph({
      dygraph(cbind("Portfolio return" = portfolioReturns[largestCommonIndex,1],
                    "Benchmark return (ACWI)" = benchmarkReturnsACWI[largestCommonIndex,]),
              main = "Daily returns") %>%
        dyRangeSelector()
    })

    output$homeExistUserPageCumReturnChart <- renderDygraph({
      dygraph(cbind("Portfolio return" = cumPortfolioReturns[largestCommonIndex,1],
                    "Benchmark return (ACWI)" = cumBenchmarkReturnsACWI[largestCommonIndex,]),
              main = "Cumulative returns") %>%
        dyRangeSelector()
    })

    output$homeExistUserPageWeightsTable <- renderDataTable({
      portfolioWeights
      })

    # Display visual elements
    do.call(list,
            list(call("textOutput", "debugtext")))
    tabsetPanel(
      tabPanel("Overview",do.call(list,list(call("dygraphOutput","homeExistUserPageDailyReturnChart")))), # TODO: Total assets, daily change (%, value), automatedinsights summary text, daily target weights, current positions, scales to visualize goal matching, recommended alternative managers (and thier matching)
      tabPanel("Performance over time",do.call(list,list(call("dygraphOutput","homeExistUserPageCumReturnChart")))), # TODO: portfolio, benchmark (several options, custom BM creation), individual portfolio elements, cummulative, daily, contributions (sector, currency, asset class, region), overlays (recessions, momentum, bull/bear market), cashflow impacts visualization (in sector, asset class, region, cash account etc.)
      #tabPanel("Portfolio structure",do.call(list,list(call("dygraphOutput","homeExistUserPageCumReturnChart")))), # TODO: chart + % for reginal allocation - timeline play button, bar chart for current sectors, chart for sector weights, bar chart for current currency, chart for currency weights, bar chart for current asset classes, chart for asset class weights
      #tabPanel("Risk analysis",do.call(list,list(call("dygraphOutput","homeExistUserPageCumReturnChart")))), # TODO: VaR over time, Max drawdown over time, risk-factor exposure over time,
      #tabPanel("Sources of return",do.call(list,list(call("dygraphOutput","homeExistUserPageCumReturnChart")))), # TODO: top-down brinson+FI table plus 12m roling chart,  multi-factor (smart beta factors + FI) table plus 12m roling chart
      tabPanel("Trade history",do.call(list,list(call("dataTableOutput","homeExistUserPageWeightsTable"))))
      # TODO: add additinal panels for performance over time, performance structure, risk analysis, trading history
    )
  })

  # This is the settings button shown for existing users wehn logged-in
  homeExistUserSettings <- renderUI({
    do.call(list,
            list(
              call("actionButton", "enterSettings", "Settings")
            ))
  })

  # This is the basic proposal page
  proposalBasicPage <- renderUI({
    output$debugtext <- renderText(paste("Age: ",rv$userAge,"; Amount: ", rv$investAmount, " BASIC ",
                                         "Total portfolio return: ", 100*rv$proposal$spNetPerf$Calendar[1,3],
                                         "% vs total benchmar return of: ", 100*rv$proposal$spNetPerf$Calendar[2,3], "%", sep=""))
    output$proposalBasicWeightChart <- renderDygraph({
      dygraph(zoo(rv$proposal$spSmooth$weights,
                  order.by = as.Date(row.names(rv$proposal$spSmooth$weights))),
              main = "Portfolio assets weights over time") %>%
        dyRangeSelector()
    })
    output$proposalBasicReturnChart <- renderDygraph({
      dygraph(zoo(cbind(rv$proposal$spSmooth$portfolioReturns, rv$proposal$spSmooth$benchmarkReturns),
                  order.by = as.Date(row.names(rv$proposal$spSmooth$portfolioReturns))),
              main = "Portfolio vs benchmark returns over time") %>%
        dyRangeSelector()
    })
    output$proposalBasicWealthProjectionChart <- renderDygraph({
      dygraph(zoo(rv$investAmount*(1+cbind(rv$proposal$spSmooth$portfolioReturns, rv$proposal$spSmooth$benchmarkReturns)),
                  order.by = as.Date(row.names(rv$proposal$spSmooth$portfolioReturns))),
              main = "Projected wealth of an investment in the portfolio vs benchmark") %>%
        dyRangeSelector()
    })

    do.call(list,list(call("textOutput", "debugtext")))
    tabsetPanel(
      tabPanel("Projected wealth",do.call(list,list(call("dygraphOutput","proposalBasicWealthProjectionChart")))),
      tabPanel("Comparative returns",do.call(list,list(call("dygraphOutput","proposalBasicReturnChart")))),
      tabPanel("Asset weights changes",do.call(list,list(call("dygraphOutput","proposalBasicWeightChart"))))
      # TODO: add fourth panael for "Source of returns" and show a table with attribution analysis figures
    )
  })

  # This is the basic proposal navigation 1 content
  proposalBasicNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "signUp", "Sign-up now")
            ))
  })

  # This is the basic proposal navigation 2 content
  proposalBasicNav2 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "moreDetail", "Add more detail")
            ))
  })

  # This is the detailed proposal page
  proposalDetailPage <- renderUI({
    output$debugtext <- renderText(paste("Age: ",rv$userAge,"; Amount: ", rv$investAmount, " DETAILED",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext")
            ))
  })

  # This is the detail proposal navigation 1 content
  proposalDetailNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "signUp", "Sign-up now")
            ))
  })

  # This is the detail proposal navigation 2 content
  proposalDetailNav2 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "advanced", "Advanced")
            ))
  })

  # This is the advanced proposal disclaimer acceptance page
  proposalAdvancedDisclaimPage <- renderUI({
    output$debugtext <- renderText(paste("DISCLAIMER NOTICE",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext")
            ))
  })

  # This is the advanced proposal disclaimer acceptance
  proposalAdvancedDisclaimNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "acceptDisclaimer", "Agree")
            ))
  })

  # This is the advanced proposal page
  proposalAdvancedPage <- renderUI({
    output$debugtext <- renderText(paste("Age: ",rv$userAge,"; Amount: ", rv$investAmount, " ADVANCED",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext")
            ))
  })

  # This is the advanced proposal navigation 1 content
  proposalAdvancedNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "signUp", "Sign-up now")
            ))
  })

  # This is the sign-up page
  signupPage <- renderUI({
    output$debugtext <- renderText(paste("SIGNUP",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext"),
              call("textInput", "newUserEmail","Choose a contact e-mail:", rv$newUserEmail),
              call("textInput", "newUserBitCoinAddress","From which bitcoin address to transfer the money:", rv$newUserBitCoinAddress),
              call("textInput", "newUserName","Choose a username:", rv$newUserName),
              call("textInput", "newUserPassword","Choose a password:", rv$newUserPassword)
            ))
  })

  # This is the advanced proposal navigation 1 content
  signupNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "signUpComplete", "Ready. To. Go.")
            ))

  })

  # This is the login page
  loginPage <- renderUI({
    output$debugtext <- renderText(paste("LOGIN",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext"),
              call("textInput", "userName","Username:", rv$userName),
              call("textInput", "userPassword","Password:", rv$userPassword)
            ))
  })

  # This is the login navigation 1 content
  loginNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "loginOK", "Login"),
              call("actionButton", "loginCancel", "Cancel")
            ))
  })

  # This is the settings page
  settingsPage <- renderUI({
    output$debugtext <- renderText(paste("SETTINGS",sep=""))
    do.call(list,
            list(
              call("textOutput", "debugtext")
            ))
  })

  # This is settings navigation 1 content
  settingsNav1 <- renderUI({
    do.call(list,
            list(
              call("actionButton", "saveSettings", "Save settings")
            ))
  })

  #TODO: Logg of button, file load and chart display


  # Set the initial UI components
  output$navbarUI1 <- navBar1
  output$navbarUI2 <- navBar2
  output$navbarUI3 <- navBar3
  output$navbarUI4 <- navBar4
  output$mainpanelUI <- homeNewUserPage


  # Event action when a user clicks "Go" button
  observeEvent(input$proposalBasic,{

    # Consolidate environment data
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    rv$newUserID <- UUIDgenerate()
    # TODO: Remove artificial date setting
    rv$currentDate <- as.Date("2015-03-12")

    # Collect user input data
    userage <- rv$userAge
    userinvestableassets <- rv$investAmount
    newUserID <- rv$newUserID
    currentDate <- rv$currentDate

    # Create a BASIC user entry based on the information provided; Determine infered user data, setting default values
    userincome <- rv$investAmount*.10 # TODO: Guessed from location income level.
    userdomicile <- "CHE" # TODO: guessed from user IP address location
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
    newBitCoinAddress <- "NA" # TODO: user input
    newMobileNr <- "NA" # TODO: user input
    newActiveTwoFactorAuth <- FALSE # TODO: user input
    newActiveAutoTrade <- FALSE # TODO: user input
    newApplicableBroker <- "NA" # TODO: user input
    newExistingUsers <<- c(newUserID, newTotalDisclosedWealth, newTotalDisclosedIncome, newSignUpStatus, newCurrentAge, newBirthDate,
                            newRetirementAge, newLifeExpectany, newProfileType, newInvestType, newClientType, newExhibitLossAversion,
                            newExhibitBiasedExpectations, newExhibitMentalAccounting, newCurrentStageInLife,
                            newSourceOfWealth, newDomicileCountry, newAccountType, newEmail, newBitCoinAddress, newMobileNr, newActiveTwoFactorAuth,
                            newActiveAutoTrade, newApplicableBroker)
    existingUsers <- rbind(existingUsers, newExistingUsers)
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
    newInvestmentHorizon <<- c(newUserID, newHorizonID, newHorizonGoalName, newInvestStartDate, newInvestDurationYears,
                               newInitialInvestAmt, newPeriodIncome, newPeriodSaveRate, newPeriodInflation, newPeriodGovYield,
                               newReturnTargetPct,newSingleTargetAmt, newReturnTargetAmt, newRiskAbility, newRiskWillingness, newRiskTollerance,
                               newIncomeTaxRate, newCapitalTaxrate, newSectorExclusionList, newSectorReturnList,
                               newLiquidityNeedsAmt, newActiveInvestUniverse, newActiveInvestStrategy)
    investmentHorizons <- rbind(investmentHorizons,newInvestmentHorizon)
    # Run a backtest simulation with BASIC client information
    backtestEndDate <- currentDate

    # TODO: remove the 2 year backtest limitation once longer time series data is available
    if(newInvestDurationYears>=3) {
      backtestDuration<- 3
    }
    if(newInvestDurationYears<3) {
      backtestDuration <- newInvestDurationYears
    }
    backtestStartDate <- as.Date(paste((as.numeric(format(currentDate, "%Y"))-backtestDuration), format(currentDate,"%m-%d"), sep="-"))
    rv$proposal <<- PortfolioConstruction(type = paste(newActiveInvestStrategy,newActiveInvestUniverse, sep="")
                                          ,start_date = backtestStartDate, end_date = backtestEndDate, userid = newUserID)

    # Setup of user interface
    output$mainpanelUI <- proposalBasicPage
    output$mainpanenavUI1 <- proposalBasicNav1
    output$mainpanenavUI2 <- proposalBasicNav2

  })

  # Event action when a user clicks "Add more detail" button
  observeEvent(input$moreDetail,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- proposalDetailPage
    output$mainpanenavUI1 <- proposalDetailNav1
    output$mainpanenavUI2 <- proposalDetailNav2
  })

  # Event action when a user clicks "Advanced" button
  observeEvent(input$advanced,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- proposalAdvancedDisclaimPage
    output$mainpanenavUI1 <- proposalAdvancedDisclaimNav1
    output$mainpanenavUI2 <- emptyElement
  })

  # Event action when a user clicks "Accept Disclaimer" button
  observeEvent(input$acceptDisclaimer,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- proposalAdvancedPage
    output$mainpanenavUI1 <- proposalAdvancedNav1
  })

  # Event action when a user clicks "Sign Up" button
  observeEvent(input$signUp,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- signupPage
    output$mainpanenavUI1 <- signupNav1
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Complete SignUp" button
  # TODO: Hash on e-mail or something for user ID and the collusion with existing users
  # TODO: If a user exists, do not create the user and ask for other credentials
  # TODO: Send a user verification e-mail and use captcha human identification
  observeEvent(input$signUpComplete,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    rv$newUserName <- input$newUserName
    rv$newUserEmail <- input$newUserEmail
    rv$newUserBitCoinAddress <- input$newUserBitCoinAddress
    rv$newUserPassword <- input$newUserPassword
    newUserID <- rv$newUserID
    loggedInUserID <<- newUserID
    newUserCredentials <- c(newUserID, rv$newUserName, rv$newUserPassword, rv$newUserEmail)

    existingUsersCredentials <- rbind(existingUsersCredentials, newUserCredentials)
    row.names(existingUsersCredentials) <- existingUsersCredentials[,1]
    dput(existingUsersCredentials,paste(executionPath, "INPUT/existingUsersCredentials.data.R",sep=""))

    existingInvestmentHorizons <- rbind(investmentHorizons, newInvestmentHorizon)
    dput(existingInvestmentHorizons, paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))

    existingUsers <- rbind(existingUsers, newExistingUsers)
    dput(existingInvestmentHorizons, paste(executionPath, "INPUT/existingUsers.data.R",sep=""))

    output$navbarUI1 <- emptyElement
    output$navbarUI2 <- emptyElement
    output$navbarUI3 <- homeExistUserSettings
    output$navbarUI4 <- navBar4LoggedIn
    output$mainpanelUI <- homeExistUserPage
    output$mainpanenavUI1 <- emptyElement
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Login" button
  observeEvent(input$login,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- loginPage
    output$mainpanenavUI1 <- loginNav1
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Logout" button
  observeEvent(input$logout,{
    output$navbarUI1 <- navBar1
    output$navbarUI2 <- navBar2
    output$navbarUI3 <- navBar3
    output$navbarUI4 <- navBar4
    output$mainpanelUI <- homeNewUserPage
    output$mainpanenavUI1 <- emptyElement
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Submit login credentials" button
  observeEvent(input$loginOK,{
    rv$userName <- input$userName
    rv$userPassword <- input$userPassword
    if(as.character(existingUsersCredentials[existingUsersCredentials[,2]==rv$userName,3]!=rv$userPassword)[2]) {
      output$navbarUI1 <- navBar1
      output$navbarUI2 <- navBar2
      output$navbarUI3 <- navBar3
      output$navbarUI4 <- navBar4
      output$mainpanelUI <- loginPage
      output$mainpanenavUI1 <- loginNav1
      output$mainpanenavUI2 <- emptyElement
      output$mainpanenavUI3 <- emptyElement
      output$mainpanenavUI4 <- emptyElement
    }
    if(as.character(existingUsersCredentials[existingUsersCredentials[,2]==rv$userName,3]==rv$userPassword)[2]) {
      output$navbarUI1 <- emptyElement
      output$navbarUI2 <- emptyElement
      output$navbarUI3 <- homeExistUserSettings
      output$navbarUI4 <- navBar4LoggedIn
      output$mainpanelUI <- homeExistUserPage
      output$mainpanenavUI1 <- emptyElement
      output$mainpanenavUI2 <- emptyElement
      output$mainpanenavUI3 <- emptyElement
      output$mainpanenavUI4 <- emptyElement
      loggedInUserID <<- existingUsersCredentials[existingUsersCredentials[,2]==rv$userName & existingUsersCredentials[,2]==rv$userPassword,1][2]
    }
  })

  # Event action when a user clicks "Cancel login" button
  observeEvent(input$loginCancel,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- homeNewUserPage
    output$mainpanenavUI1 <- emptyElement
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Settings" button
  observeEvent(input$enterSettings,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- settingsPage
    output$mainpanenavUI1 <- settingsNav1
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

  # Event action when a user clicks "Save settings" button
  observeEvent(input$saveSettings,{
    rv$userAge <- input$userAge
    rv$investAmount <- input$investAmount
    output$mainpanelUI <- homeExistUserPage
    output$mainpanenavUI1 <- emptyElement
    output$mainpanenavUI2 <- emptyElement
    output$mainpanenavUI3 <- emptyElement
    output$mainpanenavUI4 <- emptyElement
  })

})

