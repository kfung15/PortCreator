#Name: Kennard Fung
#Project: Tangency Portfolio Maker (Machine Generated, Asset Weight Limited) 
#(Without Stationarity Checker feature)

#Even if you set a seed, the results will differ if the market is open (if you set the end date to the latest month)

#Before running this program:
#1. Create a new folder in your computer. Note the path of the folder you created.
#2. Prepare a CSV (comma separated values) file containing the stock ticker names that you want.
#3. Make sure the first cell of that CSV file is called "Symbol" (without the quotes).
#4. Make sure that CSV file is in the folder that you created.


#Clear global environment command (recommended to run first, so that things are less likely to mess up)
rm(list = ls(all.names = TRUE))

#Set the Seed
#For reproducibility of results, seed a seed first. The seed can be any positive integer.
#WARNING: if you choose to rerun this program again and get the same results, 
#you must COMPLETELY close out of R and open this program AGAIN! Otherwise, you WILL get 
#different results.
set.seed(1237)

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
  
  #Next, check for liquidity. If the overall trading liquidity is less than 1B rupiah, drop the asset.
  #CHANGE the start and end dates to fit your specs.
  volume = get.hist.quote(instrument=asset.name, start="2020-01-01",
                          end="2020-01-02", quote="Volume",
                          provider="yahoo", origin="1970-01-01",
                          compression="m", retclass="zoo")
  adjclose = get.hist.quote(instrument=asset.name, start="2020-01-01",
                            end="2020-01-02", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")
  #Better to go higher than lower. Assume 21 trading days in a month (instead of 20)
  liquidity = (volume * adjclose)/21
  if(liquidity < 200000000){
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
overall_stocklist_list = c()
for(stock in overall_stocklist){
  overall_stocklist_list = c(overall_stocklist_list,(as.character(stock)))
}
projectPrices.new.df = projectPrices.df[c(overall_stocklist_list)]


#Don't worry about this
write.csv(projectPrices.new.df, file = "ProjectPrices.csv")
projectPrices.new.df = read.csv("ProjectPrices.csv")
projectPrices.new.df = projectPrices.new.df[,2:ncol(projectPrices.df)]


#Convert df to zoo and adjust the date index
Indoprice_trial.z = zoo(projectPrices.new.df, order.by = index(SMSM.prices))

#calculate the cc returns
projectReturns.z = diff(log(Indoprice_trial.z))

#Convert returns zoo to df for easy viewing
projectReturns.df = as.data.frame(projectReturns.z,drop=FALSE)
#------------------------------------------------
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

#Just cleaning up the top_zoo_df column names
for(n in 1:ncol(top_zoo_df)){
  if(nchar(colnames(top_zoo_df[n]))>7){
    colnames(top_zoo_df)[n] = substr(colnames(top_zoo_df)[n],1,7)
  }
}

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
#------------------------------------------------
#This is where the asset weight limiting happens...

#Choose how many different portfolios you want to generate in each instance
portfolios_to_generate = 1000

#Choose how many time you want to generate portfolios
total_iterations = 1000

#Initialize best_so_far to catch the best portfolios at the very end!
best_so_far = data.frame()

#For the iteration progress counter
progress_count = 0

for(i in 1:total_iterations){
random_weights_list = data_frame(rep(1,portfolios_to_generate))
for(x in 1:length(colnames(top_zoo_df))){
  g = as.data.frame(runif(portfolios_to_generate, min=0.04, max=0.1))
  random_weights_list = cbind(random_weights_list,g)
}

random_weights_list = random_weights_list[,2:ncol(random_weights_list)]
random_weights_list = t(random_weights_list)

muhat_list = c()
sd_list = c()

#Calculate the expected returns and SDs for each of the hypothetical portfolios
for(x in 1:ncol(random_weights_list)){
  muhat_temp = t(as.matrix(muhat.vals.topzoo)) %*% as.matrix(random_weights_list[,x])
  sd_temp = (t(as.matrix(random_weights_list[,x])) %*% cov.mat.topzoo) %*% as.matrix(random_weights_list[,x])
  muhat_list = c(muhat_list,muhat_temp)
  sd_list = c(sd_list,sd_temp)
}

muhat_list = as.matrix(muhat_list)
sd_list = as.matrix(sd_list)

#Convert cc muhat to simple muhat and create a new column
muhat_list_simple = c()
for(muhat in muhat_list){
  muhat_simple_temp = exp(muhat) - 1
  muhat_list_simple = c(muhat_list_simple, muhat_simple_temp)
}

muhat_list_simple = as.matrix(muhat_list_simple)


#Calculate the Sharpe Ratios for each of the muhats and SDs
sharpe_list = c()
for(x in 1:nrow(muhat_list)){
  sharpe_temp = (muhat_list[x,] - rf)/sd_list[x,]
  sharpe_list = c(sharpe_list,sharpe_temp)
}

sharpe_list = as.matrix(sharpe_list)

#-----------------------------------------------------------------
#Repeat the ER (simple, cc), SD and Sharpes again, but this time for annual
muhat_list_annual = 12 * muhat_list
sd_list_annual = sqrt(12) * sd_list

muhat_list_annual_simple = c()
for(muhat in muhat_list_annual){
  muhat_simple_annual_temp = exp(muhat) - 1
  muhat_list_annual_simple = c(muhat_list_annual_simple, muhat_simple_annual_temp)
}

muhat_list_annual_simple = as.matrix(muhat_list_annual_simple)

#Calculate the Sharpe Ratios for each of the muhats and SDs
sharpe_list_annual = c()
for(x in 1:nrow(muhat_list_annual)){
  sharpe_annual_temp = (muhat_list_annual[x,] - (rf*12))/sd_list_annual[x,]
  sharpe_list_annual = c(sharpe_list_annual,sharpe_annual_temp)
}

sharpe_list_annual = as.matrix(sharpe_list_annual)

#-----------------------------------------------------------------

#Flip back the random_weights_list and rename the column names with the stock tickers
random_weights_list = t(random_weights_list)
colnames(random_weights_list) = colnames(top_zoo_df)

#shove the muhat, SDs and Sharpe list to the end!
random_weights_list = cbind(random_weights_list,muhat_list)
random_weights_list = cbind(random_weights_list,muhat_list_simple)
random_weights_list = cbind(random_weights_list,sd_list)
random_weights_list = cbind(random_weights_list,sharpe_list)

random_weights_list = cbind(random_weights_list,muhat_list_annual)
random_weights_list = cbind(random_weights_list,muhat_list_annual_simple)
random_weights_list = cbind(random_weights_list,sd_list_annual)
random_weights_list = cbind(random_weights_list,sharpe_list_annual)

#rename those last three columns
first_col = ncol(random_weights_list) - 7
last_col = as.numeric(ncol(random_weights_list))
colnames(random_weights_list)[first_col:last_col] = c("ERCCMonthly","ERSimpleMonthly", "SDMonthly", "SharpeMonthly","ERCCAnnual","ERSimpleAnnual", "SDAnnual", "SharpeAnnual")

#Sort the random_weights_list by Sharpe values

sorted_weights_list = as.data.frame(random_weights_list) %>% arrange(desc(SharpeAnnual))

#Check the total weights allocated to the assets
last_asset_column = as.numeric(ncol(random_weights_list) - 8)
assets_weight_temp = as.data.frame(rowSums(sorted_weights_list[,1:last_asset_column]))
sorted_weights_list = cbind(sorted_weights_list,assets_weight_temp)

portfolio_to_consider = sorted_weights_list[1,]

#Grab the first row of sorted_weights_list with the highest Sharpe ratio and put it in
#the best_so_far basket. 
#If something already exists in the best_so_far basket, check to see if the incoming sorted_weights_list[1,]
#Sharpe is bigger than the one that already exists. If not, skip to the next instance.

sharpe_to_consider = as.numeric(portfolio_to_consider[ncol(portfolio_to_consider) - 1])

progress_count = progress_count + 1
print(paste("Progress So Far:", progress_count, "/", total_iterations))

portfolio_weight = as.numeric(portfolio_to_consider[ncol(portfolio_to_consider)])

if(portfolio_weight > 1){
  next
}else if(ncol(best_so_far) == 0){
  best_so_far = portfolio_to_consider
  best_sharpe_to_compare = as.numeric(sorted_weights_list[1,][ncol(sorted_weights_list) - 1])
}else if (sharpe_to_consider > best_sharpe_to_compare){
  best_so_far = portfolio_to_consider
  best_sharpe_to_compare = as.numeric(portfolio_to_consider[1,][ncol(portfolio_to_consider) - 1])
}else{
  next
}

}

#Just a minor cosmetic change
colnames(best_so_far)[ncol(best_so_far)] = c("Total Asset Weight")

#Transposed so my life is easier later on.
best_so_far = t(best_so_far)


#---------------------------------------------------------------------

#Backtesting the data

#Backtesting the portfolio
ticker_names = colnames(top_zoo_df)
start_data = c()
end_data = c()

for(n in 1:length(ticker_names)){
  if(colnames(top_zoo_df)[n]>7){
    colnames(top_zoo_df)[n] = substr(colnames(top_zoo_df)[n],1,7)
  }
}

ticker_names = colnames(top_zoo_df)

#Grab the two data points (start, end) for all the tickers in the portfolio
#CHANGE: the start and end dates below to reflect the parameters that you desire!
#format is YYYY-MM-DD
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
  
  #Sys.sleep(1)
  
}

#Calculate the overall portfolio % change for the whole period.
#This is assuming that you bought it on the start date and held it all the way
#till the end date.
#This is simple returns, so no need for conversion at the end.
start_data = as.data.frame(start_data)
end_data = as.data.frame(end_data)

percent_change_data = ((end_data - start_data)/start_data)
colnames(percent_change_data) = c("percent_change")
rownames(percent_change_data) = ticker_names

asset_weights = best_so_far[1:(nrow(best_so_far) - 9)]

port_change = percent_change_data * asset_weights
colnames(port_change) = c("overall_change")
port_change

port_change_figure = paste((sum(port_change$overall_change) * 100), "%", sep = "")
port_change_figure

#--------------------------------------------------------------------
#Putting everything at the end for neatness sake!
#--------------------------------------------------------------------
#Monthly Stats
paste("Monthly Portfolio Expected Returns:", best_so_far["ERSimpleMonthly",]*100, "%")
paste("Monthly Portfolio Standard Deviation:", best_so_far["SDMonthly",] * 100, "%")
paste("Monthly Portfolio Sharpe Ratio:", best_so_far["SharpeMonthly",])

#Annual Stats
paste("Annual Portfolio Expected Returns:", best_so_far["ERSimpleAnnual",] * 100, "%")
paste("Annual Portfolio Standard Deviation:", best_so_far["SDAnnual",] * 100, "%")
paste("Annual Portfolio Sharpe Ratio:", best_so_far["SharpeAnnual",])

port_change_figure
#--------------------------------------------------------------------
#At the end, export a csv file of all the results that you got



#transpose best_so_far, because I need to add the port_change_figure from above
port_change_figure.df = as.data.frame(port_change_figure)
best_so_far_tpose = as.data.frame(t(best_so_far))
best_so_far_tpose = cbind(best_so_far_tpose,port_change_figure.df)
#Transpose back because I like my data.frames vertical
best_so_far_tpose = t(best_so_far_tpose)

#Export the data
write.csv(best_so_far_tpose, file = "Best Portfolio So Far (2nd Try seed 1237).csv")

