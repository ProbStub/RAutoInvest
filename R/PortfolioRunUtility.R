#####
# Run time control parameters
# setwd("/home/rstudio") #TODO: uncomment for deployment
executionPath <- "/home/rstudio/RAutoInvest/"
source("/home/rstudio/RAutoInvest/.pwd")
currentDate <- as.Date(Sys.Date())
#####
# Loading required code/packages
source(paste(executionPath,"/R/EquityPortfolioBuilder.R",sep=""))

# Get existing user investment profiles
# TODO: check for existing files and cycle through all users and all their investment goals
userID <- 17
existingUsers <- dget(paste(executionPath, "INPUT/existingUsers.data.R",sep=""))
investmentHorizons <- dget(paste(executionPath, "INPUT/investmentHorizons.data.R",sep=""))
horizonActiveInvestStrategy <- investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestStrategy
horizonActiveInvestUniverse <- investmentHorizons[investmentHorizons$UserID==userID,]$ActiveInvestUniverse

# Run a backtest simulation with BASIC client information
# TODO: Extend EquityPortfolioBuilder with an option to run with out backtest
backtestEndDate <- currentDate

# TODO: remove the 2 year backtest limitation once longer time series data is available
backtestDuration<- 4

# TODO: choce dates so that they align with current date!!!!

backtestStartDate <- as.Date(paste((as.numeric(format(currentDate, "%Y"))-backtestDuration), format(currentDate,"%m-%d"), sep="-"))
PortfolioConstruction(type = paste(horizonActiveInvestStrategy,horizonActiveInvestUniverse, sep=""),
                      start_date = backtestStartDate,
                      end_date = backtestEndDate, userid = userID)
#TODO: Get this to work, the NA procedure is failing for some reason. investigate data, make sure that it works!!!! THEN adjust the NA logic


