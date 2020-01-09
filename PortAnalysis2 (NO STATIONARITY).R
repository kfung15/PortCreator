#Name: Kennard Fung
#Project: Tangency Portfolio Maker (Without Stationarity Checker feature)

#Before running this program:
#1. Create a new folder in your computer. Note the path of the folder you created.
#2. Prepare a CSV (comma separated values) file containing the stock ticker names that you want.
#3. Make sure the first cell of that CSV file is called "Symbol" (without the quotes).
#4. Make sure that CSV file is in the folder that you created.


#Clear global environment command (recommended to run first, so that things are less likely to mess up)
rm(list = ls(all.names = TRUE))

# Set options and load packages
options(digits=4, width=70)
library("PerformanceAnalytics")
library("tseries")
library("zoo")
library("boot")
library("quadprog")
library("dplyr")
library("tidyimpute")
#Stationarity Checker
library("aTSA")

#Import the relevant stock csv file and the noshorts file
#CHANGE the working directory to your own folder (put portfolio_noshorts.r into that folder!)
#Replace all the \ in the directory with /, or else you will run into an error
#For example, instead of setwd("\Users\kenfung\Desktop\FreshFinance"), 
#do setwd("/Users/kenfung/Desktop/FreshFinance") instead
#This is the same for Windows users!
setwd("/Users/kenfung/Desktop/FreshFinance")
#CHANGE the file name to the index csv file (the file with all the tickers)
#MAKE SURE that the column name of that csv file is Symbol
index = read.csv(file="LQ45.csv", header=TRUE, stringsAsFactors=FALSE)
#CHANGE: Copy the filepath above below, and add the portfolio_noshorts.r at the end. Use my
#path as an example. Windows is the same.
source(file="/Users/kenfung/Desktop/FreshFinance/portfolio_noshorts.r")
#Just in case something goes wrong, don't worry about it
colnames(index) = c("Symbol")

#Grab the ticker names
index.names = index$Symbol

#(CHANGE) set the desired start and end dates
start.date = "2007-01-01"
end.date = "2020-01-01"

#----------------------------------
#Just run this code, these will get deleted later on because they might be duplicated
#For Indonesia, let the default two assets be SMSM and SHIP (because they go before JAN 2007)

#CHANGE: you can switch the compression to "m" or "y" for year. Run ?get.hist.quote to learn more.
SMSM.prices = get.hist.quote(instrument="SMSM.JK", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
SHIP.prices = get.hist.quote(instrument="SHIP.JK", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")


index(SMSM.prices) = as.yearmon(index(SMSM.prices))
index(SHIP.prices) = as.yearmon(index(SHIP.prices))

projectPrices.z = zoo()
projectPrices.z = merge(SMSM.prices,projectPrices.z)
projectPrices.z = merge(SHIP.prices,projectPrices.z)
colnames(projectPrices.z) = c("SMSM.JK", "SHIP.JK")

overall_stocklist = c("SMSM.JK","SHIP.JK")
problem_stocks = c()

projectPrices.df = as.data.frame(projectPrices.z,drop = FALSE)
projectPrices.df[is.na(projectPrices.df)] = 0
write.csv(projectPrices.df, file = "Prices.csv")
projectPrices.df = read.csv("Prices.csv")
projectPrices.z = zoo(projectPrices.df, order.by = index(SMSM.prices))
projectPrices.z = projectPrices.z[,2:ncol(projectPrices.z)]

#------------------------------------------------

#Grab historical data for stocks in your CSV file
for(asset in index.names){
  attempt = tryCatch(
    {
      #CHANGE: make sure to modify the second argument of asset.name to reflect the region of
      #the stocks that you are looking at.
      #For example, Jakarta is .JK, London is .L, Amsterdam is .AS
      #If you are unsure, go to Yahoo Finance and look for a ticker.\
      #There will be a dot something at the end of the ticker. Use thqt.
      asset.name = paste(asset, ".JK", sep = "")
      #Get the prices first
      #CHANGE: ensure that your compression matches the one above for SHIP and SMSM
      prices = get.hist.quote(instrument=asset.name, start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
      index(prices) = as.yearmon(index(prices))
      projectPrices.z = merge(prices,projectPrices.z)
      
    },
    #Skip the asset if unable to get the prices
    warning = function(cond) {
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    },
    
    #Skip the asset if unable to get the prices
    error = function(cond) {
      message("Here's the original error message:")
      message(cond)
      return(NULL)
    },
    finally = {
    }
    
  )
  #If there was a warning or an error, move on to the next asset
  if(is.null(attempt)){
    problem_stocks = c(problem_stocks, asset.name)
    next
  }
  #Next, check if there are NAs in the data. If yes, kick it out and move on to next asset.
  sumNA = sum(is.na(projectPrices.z))
  if(sumNA > 0){
    projectPrices.z = projectPrices.z[, colnames(projectPrices.z) != "Adjusted"]
    next
  }
  
  #Update the column names of projectPrices zoo file
  overall_stocklist = c(asset.name, overall_stocklist)
  colnames(projectPrices.z) = overall_stocklist
  
}

#Convert projectPrices to df for easy viewing
projectPrices.df = as.data.frame(projectPrices.z,drop=FALSE)

#Immediately drop the last two columns
projectPrices.df = projectPrices.df[,2:ncol(projectPrices.df)-2]

#Refresh the overall stocklist
overall_stocklist = colnames(projectPrices.df)

#Delete any stocks with negative adjusted closing prices
#Must do this, otherwise cannot calculate continously compounded returns later on.
problem_stocks = c()

for(stock in overall_stocklist){
  for(n in 1:nrow(projectPrices.df)){
    if((as.numeric(as.character(projectPrices.df[,stock][n]))) < 0){
      problem_stocks = c(problem_stocks,stock)
    }
    
  }
  
}

problem_stocks = as.data.frame(problem_stocks)
problem_stocks = distinct(problem_stocks)

problem_index = c()
for(stock in problem_stocks){
  print(as.character(stock))
  problem_index = c(problem_index, which(overall_stocklist == as.character(stock)))
  
}

problem_index = as.numeric(problem_index)

overall_stocklist = overall_stocklist[-c(problem_index)]
overall_stocklist = as.data.frame(overall_stocklist)

#Refresh the projectPrices.df
projectPrices.df = projectPrices.df[c(overall_stocklist)]


#Don't worry about this
write.csv(projectPrices.df, file = "ProjectPrices.csv")
projectPrices.df = read.csv("ProjectPrices.csv")
projectPrices.df = projectPrices.df[,2:ncol(projectPrices.df)]


#Convert df to zoo and adjust the date index
Indoprice_trial.z = zoo(projectPrices.df, order.by = index(SMSM.prices))

#calculate the cc returns
projectReturns.z = diff(log(Indoprice_trial.z))

#Convert returns zoo to df for easy viewing
projectReturns.df = as.data.frame(projectReturns.z,drop=FALSE)

#Grab the >0 sharpe ratios for the filtered assets
#State the risk free asset rate (remember to convert to CC returns first!)
muhat.vals = colMeans(projectReturns.df)
sd.vals = apply(projectReturns.df, 2, sd)
#This is the 30-year Indonesian bond interest rate, adjusted for Baa2 (-175bp)
#and converted to continuously compounded form r = ln(1 + R)
#Original rate is 7.865%
#(CHANGE: the adjusted_rate to account for Moody's rating) (original - Moody's modifier)
adjusted_rate = (0.07865 - 0.0175)
rf = log(1 + adjusted_rate)/12

sharpe.vals = (muhat.vals-rf)/sd.vals
sharpe.vals.df = as.data.frame(sharpe.vals)
sharpe.vals.df = subset(sharpe.vals.df, sharpe.vals.df$sharpe.vals > 0)

sharpe.vals.df = tibble::rownames_to_column(sharpe.vals.df)
sharpe.vals.df.sorted = sharpe.vals.df %>% arrange(desc(sharpe.vals.df$sharpe.vals))

#Get the returns from the top 45 (or how ever many) sharpe ratio assets
top_zoo = zoo()
top_names = c()
nrow(sharpe.vals.df.sorted)
if(nrow(sharpe.vals.df.sorted) < 45){
  top_length = nrow(sharpe.vals.df.sorted)
} else{
  top_length = 45
}
for(x in 1:top_length){
  top_zoo = cbind(projectReturns.z[,sharpe.vals.df.sorted[x,1]],top_zoo)
  top_names = c(sharpe.vals.df.sorted[x,1],top_names)
}
top_zoo = merge(top_zoo)
colnames(top_zoo) = top_names

top_zoo_df = as.data.frame(top_zoo)

#Dumb workaround because the things in the original df were factors not numeric -.-
write.csv(top_zoo_df, file = "TopZoo.csv")
top_zoo_df = read.csv("TopZoo.csv")
#Delete the first column coz unnecessary dates
top_zoo_df = top_zoo_df[,2:ncol(top_zoo_df)]


# Compute descriptive statistics
# There are other stats like skewness and kurtosis if you need them
# Kurtosis is excess kurtosis, where the baseline is kurt = 3
muhat.vals.topzoo = colMeans(top_zoo_df)
sd.vals.topzoo = apply(top_zoo_df, 2, sd)
cov.mat.topzoo = var(top_zoo_df)
cor.mat.topzoo = cov2cor(cov.mat.topzoo)
var.vals.topzoo = sd.vals^2
skew.vals.topzoo = skewness(top_zoo_df)
kurt.vals.topzoo = kurtosis(top_zoo_df)
UniDesStat.df.topzoo = summary(top_zoo_df)

## Compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(muhat.vals.topzoo, cov.mat.topzoo, rf, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=rf)
plot(tan.port.ns)

## compute tangency portfolio with no short sales (Annualized)
tan.port.ns.annual <- tangency.portfolio(muhat.vals.topzoo*12, cov.mat.topzoo, rf*12, shorts=FALSE)
tan.port.ns.annual
summary(tan.port.ns.annual, risk.free=rf*12)
plot(tan.port.ns.annual)

annual_weights_df = as.data.frame(tan.port.ns.annual$weights)
colnames(annual_weights_df) = "Weights"

#From the annual weights, delete the tickers zero weights (cleaning up)
for_deletion = c()
for(x in 1:top_length){
  if(tan.port.ns.annual$weights[x] == 0){
    for_deletion = c(x,for_deletion)
  }
}

annual_weights_df = tibble::rownames_to_column(annual_weights_df)
#CHANGE: See that 0.00 at the end? You can adjust the Weights filter. For example,
#if you wanted to filter out stocks that constitute less than 5% of the portfolio,
#you can change that 0.00 to 0.05.
annual_weights_df = annual_weights_df %>% filter(annual_weights_df$Weights > 0.00)
colnames(annual_weights_df) = c("Ticker", "Weights")
annual_weights_df

#CHANGE: the file name that you want to export the results to. It will be exported to the
#folder that you specified as your working directory at the beginning
#Run getwd() to check your working directory
write.csv(annual_weights_df, file = "TopStocks.csv")

#Backtesting the portfolio
ticker_names = annual_weights_df$Ticker
start_data = c()
end_data = c()

#Grab the two data points (start, end) for all the tickers in the portfolio
#CHANGE: the start and end dates below to reflect the parameters that you desire!
for(ticker in ticker_names){
  start_with = get.hist.quote(instrument=ticker, start=start.date,
                               end="2007-01-02", quote="AdjClose",
                               provider="yahoo", origin="1970-01-01",
                               compression="m", retclass="zoo")
  end_with = get.hist.quote(instrument=ticker, start=end.date,
                            end="2020-01-02", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
  start_data = c(start_data, as.numeric(start_with$Adjusted))
  end_data = c(end_data, as.numeric(end_with$Adjusted))
  
}

#Calculate the overall portfolio % change for the whole period.
#This is assuming that you bought it on the start date and held it all the way
#till the end date.
start_data = as.data.frame(start_data)
end_data = as.data.frame(end_data)

percent_change_data = ((end_data - start_data)/start_data)
colnames(percent_change_data) = c("percent_change")
rownames(percent_change_data) = ticker_names

port_change = percent_change_data * annual_weights_df$Weights
colnames(port_change) = c("overall_change")
port_change

port_change_figure = paste((sum(port_change$overall_change) * 100), "%", sep = "")
port_change_figure


#--------------------------------------------------------------------


