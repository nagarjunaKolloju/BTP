# MACD, RSI, Bollinger Bands and News Strategy Backtesting Script for Multiple Stocks with Portfolio Statistics:

# SET WORKING DIRECTORY AND CLEAR ENVIRONMENT
library(quantmod) # Loads the quantmod package, providing technical analysis tools
library(TTR) # Loads the TTR package, offering additional tools for time series analysis
library(dplyr) # Loads the dplyr package, used for data manipulation and wrangling
library(ggplot2) # Loads the ggplot2 package, used for creating various data visualizations
library(rstudioapi) # Loads the rstudioapi package, for interacting with RStudio functionalities (optional)

library(scales) # Loads the scales package, used for data visualization
library(xts) # Loads the xts package, used for time series analysis
library(tidyquant) # Loads the tidyquant package, used for quantitative financial analysis
library(tibble) # Loads the tibble package, used for data manipulation
library(Quandl) # Loads the Quandl package, used for financial and economic data
library(LDATS) # Loads the LDATS package, used for data analysis and visualization
current_path <- rstudioapi::getActiveDocumentContext()$path  # Get current R script path (optional)
setwd(dirname(current_path)) # Set the working directory to the location of the script (optional)
rm(list = ls()) # Remove all objects from the current R environment
options(scipen = 999) # Set the scientific notation penalty to 999 (optional)
Quandl.api_key('juF1uFeb6zXckqi_zddC') # Set the API key for NASDAQ data 


# SET PARAMETERS
initialequity <- 100000  # Initialize starting capital to $100,000
maxTrade <- 9000  # Set maximum value per trade to $9,000
maxLong <- 20  # Set maximum number of long positions to 20
maxShort <- 20  # Set maximum number of short positions to 20
symbols <- sp500 <- tq_index("SP500")[, c(1, 6)]$symbol  # Load S&P 500 symbols into the 'symbols' variable
symbols <- symbols[symbols != "-"]  # "-" from the list of symbols, as it is not a valid symbol

# Function to fetch S&P 500 data from NASDAQ
getSP500Data <- function() {
  data_list <- vector("list", length(symbols)) # Create a list to store data for each symbol
  names(data_list) <- symbols # Set the names of the list to the symbol names
  for (symbol in symbols) { # Loop through each symbol to fetch data
    tryCatch({ # Use tryCatch to handle errors and continue fetching data for other symbols
      print(paste("Fetching data for", symbol)) # Print a message to indicate progress
      data_list[[symbol]] <- Quandl.datatable("SHARADAR/SEP", date.gte=Sys.Date()-(365*2),ticker=symbol) # Fetch data for the symbol
    }, error = function(e) { # Handle errors by printing an error message
      message(paste("Error fetching data for", symbol, ":", e$message)) # Print the error message
    }) # End of tryCatch block
  } # End of loop
  data_combined <- do.call(rbind, data_list) # Combine the data for all symbols into a single data frame
  rownames(data_combined)<-seq(1,nrow(data_combined),1) # Add row numbers to the data frame
  data_combined<-na.omit(data_combined) # Remove rows with missing values
  return(data_combined) # Return the combined data frame
}

# Function to fetch insider trading data for S&P 500 companies
getSP500InsiderData <- function() {  # Define a function to fetch insider trading data for S&P 500 companies
  data_list <- vector("list", length(symbols))  # Initialize an empty list to store data for each symbol
  names(data_list) <- symbols  # Name each element of the list after a symbol
  for (symbol in symbols) {  # Loop through each symbol in the S&P 500 index
    tryCatch({  # Try to fetch data for the current symbol
      print(paste("Fetching data for", symbol))  # Print a message indicating which symbol is being processed
      temp <- Quandl.datatable("SHARADAR/SF2", ticker=symbol, paginate = TRUE)  # Fetch insider trading data from Quandl's SHARADAR/SF2 dataset
      if (!is.null(temp) && nrow(temp) > 0) {  # Check if the fetched data is not null and has at least one row
        temp <- temp[!is.na(temp$isdirector) & 
                      !is.na(temp$isofficer) & 
                      !is.na(temp$istenpercentowner) & 
                      !is.na(temp$transactiondate) & 
                      !is.na(temp$transactionshares)& 
                      !is.na(temp$transactionpricepershare), ]  # Filter the data to include only rows with non-null values for specific columns
        temp$transactiondate <- as.Date(temp$transactiondate, format = "%Y-%m-%d")  # Convert the transaction date column to a Date format
        data_list[[symbol]] <- temp[temp$transactiondate >= Sys.Date() - (365*2), ]  # Store the filtered data in the data_list
      } else {
        message(paste("No data available for", symbol))  # Print a message if no data is available for the current symbol
      }
    }, error = function(e) {
      message(paste("Error fetching data for", symbol, ":", e$message))  # Print an error message if an error occurs while fetching data
    })
  }
  data_combined <- do.call(rbind, data_list)  # Combine the data for all symbols into a single data frame
  View(data_combined)
  rownames(data_combined) <- seq(1, nrow(data_combined), 1)  # Reset the row names of the combined data frame
  return(data_combined)  # Return the combined data frame
}

# Function to preprocess insider Trading Data
preprocessInsiderData <- function(data) {  
  director_weight <- 1.5  # Define the weight for directors
  ten_percent_owner_weight <- 1.3  # Define the weight for 10% owners
  officer_weight <- 1.0  # Define the weight for officers
  data$weight <- ifelse(data$isdirector == 'Y', director_weight,  # Assign weights based on role
                        ifelse(data$isofficer == 'Y', officer_weight,
                              ifelse(data$istenpercentowner == 'Y', ten_percent_owner_weight, 1)))
  data$transactionvalue <- as.numeric(data$transactionvalue)  # Convert transaction value to numeric
  data$transactionshares <- as.numeric(data$transactionshares)  # Convert transaction shares to numeric
  data$weighted_transaction_value <- data$transactionpricepershare * data$transactionshares * data$weight  # Calculate weighted transaction value
  data$weighted_shares <- abs(data$transactionshares) * data$weight  # Calculate weighted shares
  summary_data <- data %>%
    group_by(ticker, transactiondate) %>%
    summarise(  # Summarize the data to calculate the average transaction value for each symbol on each transaction date
      insiderAvgTransaction = sum(weighted_transaction_value, na.rm = TRUE)/sum(weighted_shares, na.rm = TRUE),  # Calculate average transaction value
      .groups = 'drop'
    )
  return (summary_data)  # Return the preprocessed data
}


universe <- getSP500Data()  # Get the S&P 500 data and data for each symbol in the index
insider_data <- getSP500InsiderData()  # Get insider trading data for S&P 500 companies
insider_data <- preprocessInsiderData(insider_data)  # Preprocess insider trading data
insider_data$transactiondate <- as.Date(insider_data$transactiondate, format = "%Y-%m-%d")  # Convert transaction date to Date format
universe$date <- as.Date(universe$date, format = "%Y-%m-%d")  # Convert date to Date format
universe <- left_join(universe, insider_data, by = c("date" = "transactiondate", "ticker" = "ticker"))  # Left join universe and insider data by date and symbol
universe <- rename(universe, symbol = ticker)


# Function to generate technical indicators for a given symbol
genIndicators <- function(sym) {
  print(paste('Generating Indicators for symbol:', sym))  # Print a message indicating which symbol is being processed
  stock <- subset(universe, universe$symbol == sym)  # Subset the universe data for the specific symbol
  if(nrow(stock) == 0){  # Check if there is no data available for the symbol
    print(paste("No data available for", sym))  # Print a message indicating no data is available
    return (NULL)  # Return NULL
  }
  # Convert to xts
  stock.xts <- xts(stock[, c("open", "high", "low", "close", "volume", "insiderAvgTransaction")], 
                  as.Date(stock$date))  # Convert the data to an xts format
  # Lag variables
  temp.xts <- stock.xts  # Create a temporary xts object
  temp.xts$high <- stats::lag(temp.xts$high, 1)  # Calculate the lagged high value
  temp.xts$low <- stats::lag(temp.xts$low, 1)  # Calculate the lagged low value
  temp.xts$close <- stats::lag(temp.xts$close, 1)  # Calculate the lagged close value
  lagged_close <- temp.xts$close  # Extract the lagged close values
  stock.xts$lagged_close <- lagged_close  # Add the lagged close values to the xts object
  stock.xts$insiderSignal <- stats::lag(temp.xts$insiderAvgTransaction, 1)  # Calculate the lagged insider signal
  # Calculate the Relative Strength Index (RSI)
  stock.xts$RSI <- tryCatch(  
    RSI(lagged_close, n = 14),
    error = function(e) {  # Handle any errors that occur during RSI calculation
      print(paste("Error calculating RSI for", sym, ":", e$message))  # Print an error message
      return(rep(NA, length(lagged_close)))  # Return NA values
    }
  )
  # Calculate the Moving Average Convergence Divergence (MACD)
  macd_results <- tryCatch(  
    MACD(lagged_close, 12, 26),
    error = function(e) {  # Handle any errors that occur during MACD calculation
      print(paste("Error calculating MACD for", sym, ":", e$message))  # Print an error message
      return(list(macd = rep(NA, length(lagged_close)), signal = rep(NA, length(lagged_close))))  # Return NA values
    }
  )
  stock.xts$MACD <- macd_results$macd  # Add the MACD values to the xts object
  stock.xts$signal <- macd_results$signal  # Add the MACD signal values to the xts object
  # Calculate Bollinger Bands (BBands)
  bb<-tryCatch({  
    bb<-BBands(HLC(temp.xts), n = 20, maType="SMA", sd = 2)  
  }, warning=function(w) {bb<-NULL }, error=function(e) {bb<-NULL})  # Handle any errors or warnings that occur during BBands calculation
  if (is.null(bb)) {  # Check if BBands calculation failed
    stock.xts$lowerBand<-NA  # Set lower band to NA
    stock.xts$upperBand<-NA  # Set upper band to NA
    stock.xts$range<-NA  # Set range to NA
  } else {  # If BBands calculation was successful
    stock.xts$lowerBand<-bb$dn  # Add the lower band values to the xts object
    stock.xts$upperBand<-bb$up  # Add the upper band values to the xts object
    stock.xts$range<-bb$up-bb$dn  # Add the range values to the xts object
  }
  # Convert back to data frame
  stock <- data.frame(stock.xts)  # Convert the xts object back to a data frame
  date <- as.Date(rownames(stock))  # Extract the dates from the row names
  stock <- cbind(date, stock)  # Combine the dates with the data frame
  stock <- cbind(sym, stock)  # Add the symbol as the first column
  names(stock)[1] <- "symbol"  # Rename the first column to "symbol"
  stock <- subset(stock, date >= as.Date("2023-01-01"))  # Subset the data to start from January
  rownames(stock) <- seq(1, nrow(stock), 1)  # Reset rownames
  return(stock)   # Return the data frame with added indicators
}


# Function to generate trading signals for a given symbol
genSignals=function(sym){ 
  print(paste('Generating Signals for symbol: ',sym))  # Print a message indicating which symbol is being processed
  stock <- subset(indicators, indicators$symbol == sym)  # Subset the indicators data for the specific symbol
  if(nrow(stock) == 0){  # Check if there is no data available for the symbol
    return (NULL)  # Return NULL
  }
  stock.xts <- xts(stock[,c(3:ncol(stock))], stock$date)  # Convert the data to an xts format (optional)
  # Normalize and calculate insider influence
  max_insider_signal <- max(abs(stock.xts$insiderSignal), na.rm = TRUE)  # Calculate the maximum absolute insider signal
  stock.xts$insiderInfluence <- (stock.xts$insiderSignal / max_insider_signal) * 0.2  # Calculate the insider influence
  # Adjust RSI thresholds based on insider influence
  stock.xts$RSI_entry_threshold <- 30 + (stock.xts$insiderInfluence * 5)  # Increase RSI entry threshold if negative influence
  stock.xts$RSI_exit_threshold <- 70 + (stock.xts$insiderInfluence * 5)  # Decrease RSI exit threshold if negative influence
  # Long entry and exit conditions using dynamic thresholds and smoothed indicators
  stock.xts$long.entry <- ifelse(  # Create a long entry signal
    (stock.xts$lagged_close < stock.xts$lowerBand & stock.xts$MACD > stock.xts$signal) | 
    (stock.xts$RSI < stock.xts$RSI_entry_threshold), 1, 0)
  stock.xts$long.exit <- ifelse(  # Create a long exit signal
    (stock.xts$lagged_close > stock.xts$lowerBand) | 
    (stock.xts$RSI > 45) | 
    (stock.xts$MACD < stock.xts$signal) , 1, 0)
  # Short entry and exit conditions
  stock.xts$short.entry <- ifelse(  # Create a short entry signal
    (stock.xts$lagged_close > stock.xts$upperBand) & 
    (stock.xts$RSI > stock.xts$RSI_exit_threshold) & 
    (stock.xts$MACD < stock.xts$signal), 1, 0)
  stock.xts$short.exit <- ifelse(  # Create a short exit signal
    (stock.xts$lagged_close < stock.xts$upperBand) | 
    (stock.xts$RSI < 45) | 
    (stock.xts$MACD > stock.xts$signal), 1, 0)
  stock <- data.frame(stock.xts)  # Convert the xts object back to a data frame
  date <- as.Date(rownames(stock))  # Extract the dates from the row names
  stock <- cbind(date, stock)    # Combine the dates with the data frame
  stock <- cbind(sym, stock)      # Add the symbol as the first column
  names(stock)[1] <- "symbol"     # Rename the first column to "symbol"
  rownames(stock) <- seq(1, nrow(stock), 1)  # Reset the row names
  return(stock)   # Return the data frame with added signal columns
}


# Function to close positions based on exit signals
closePositions=function(day,equity,position){  # Define a function to close positions
  cash<-0  # Initialize cash to 0
  closed<-NULL  # Initialize closed positions to NULL
  if (!is.null(position)) {  # Check if there are any positions
    longposition<-subset(position,type=="Long")  # Subset long positions
    shortposition<-subset(position,type=="Short")  # Subset short positions
    candidates<-subset(signals,signals$date==day&  # Check for short exit signals
                        (signals$short.exit==1))[,c(1,2,6)]  # Extract symbol, date, and price
    names(candidates)[2]<-"closedate"  # Rename date column to closedate
    names(candidates)[3]<-"outprice"  # Rename price column to outprice
    closeshort<-merge(shortposition,candidates,by="symbol")  # Close short positions
    candidates<-subset(signals,signals$date==day&  # Check for long exit signals
                        (signals$long.exit==1))[,c(1,2,6)]    
    names(candidates)[2]<-"closedate"  # Rename date column to closedate
    names(candidates)[3]<-"outprice"  # Rename price column to outprice
    closelong<-merge(longposition,candidates,by="symbol")  # Close long positions
    closed<-rbind(closeshort,closelong)  # Combine closed short and long positions
    if (nrow(closed)>0) {  # Check if there are any closed positions
      closed$closecash<-closed$outprice*closed$position  # Calculate closing cash
      closed$sellprice<-ifelse(closed$type=="Long",closed$outprice,closed$sellprice)  # Update sell price
      closed$buyprice<-ifelse(closed$type=="Short",closed$outprice,closed$buyprice)  # Update buy price
      closed$profit<-(closed$sellprice-closed$buyprice)*abs(closed$position)  # Calculate profit
      cash<-sum(closed$closecash) - nrow(closed)*1 + 0.005*closed$position  # Calculate aggregate cash including transaction cost
    } else closed<-NULL  # If no closed positions, set closed to NULL
  }
  return(list(closed=closed,cashin=cash))  # Return closed positions and cash
}

# Function to open positions based on entry signals
openPositions=function(day,equity,position){  # Define a function to open positions
  cash=0  # Initialize cash to 0
  transcost<-0  # Initialize transaction cost to 0
  opened<-NULL  # Initialize opened positions to NULL
  if (!is.null(position)) {  # Check if there are any existing positions
    longposition<-subset(position,type=="Long")[,c(1,2)]  # Subset long positions
    names(longposition)[2]<-"dummy"  # Rename column to dummy
    shortposition<-subset(position,type=="Short")[,c(1,2)]  # Subset short positions
    names(shortposition)[2]<-"dummy"  # Rename column to dummy
    candidates<-subset(signals,signals$date==day&  # Check for short entry signals
                        signals$short.entry==1)
    temp<-merge(candidates,shortposition,by="symbol",all.x=TRUE)  # Merge candidates with short positions
    openshort<-subset(temp,is.na(dummy))  # Check for short positions to open
    if (nrow(openshort)>0) {  # Check if there are any short positions to open
      openshort<-openshort[,c(1:ncol(openshort)-1)]  # Remove dummy column
      openshort$type<-"Short"  # Set type to Short
    } else {openshort<-NULL}  # If no short positions, set to NULL
    candidates<-subset(signals,signals$date==day&  # Check for long entry signals
                        signals$long.entry==1)
    temp<-merge(candidates,longposition,by="symbol",all.x=TRUE)  # Merge candidates with long positions
    openlong<-subset(temp,is.na(dummy))  # Check for long positions to open
    if (nrow(openlong)>0) {
      openlong<-openlong[,c(1:ncol(openlong)-1)]  # Remove dummy column
      openlong$type<-"Long"  # Set type to Long
    } else {openlong<-NULL}  # If no long positions, set to NULL
    opened<-rbind(openlong,openshort)  # Combine short and long positions
    if (!is.null(opened)) {  # Check if opened positions is not NULL
      if (nrow(opened)==0) opened<-NULL  # If no rows, set to NULL
    }
  } else {
    opened<-subset(signals,signals$date==day&  # If no positions, check for all signals
                    (signals$short.entry==1|signals$long.entry==1)) 
    if (nrow(opened)==0) {opened<-NULL} else {  # If no signals, set to NULL
      opened$type<-ifelse(opened$short.entry==1,  # Set type based on entry signal
                          "Short","Long")}
  }
  if (!is.null(opened)) {  # Check if opened positions is not NULL
    opened$buyprice<-ifelse(opened$type=="Long",opened$open,NA)  # Set buy price
    opened$sellprice<-ifelse(opened$type=="Short",opened$open,NA)  # Set sell price
    opened<-opened[order(opened$range),]  # Sort by range
    # Separate long and short trades
    long_trades <- subset(opened, type == "Long")
    short_trades <- subset(opened, type == "Short")
    # Apply maxlong and maxshort constraints
    if (nrow(long_trades) > maxLong) {
      long_trades <- long_trades[1:maxLong,]
    }
    if (nrow(short_trades) > maxShort) {
      short_trades <- short_trades[1:maxShort,]
    }
    # Re-combine after applying constraints
    opened <- rbind(long_trades, short_trades)
    numtrades <- nrow(opened)  # Count the number of trades

    if(numtrades > 0){  # If there are trades
      softmax_scores <- softmax(1/opened$range)  # Normalize trade amounts based on risk
      tradeamounts <- softmax_scores * equity  # Calculate trade amounts
      tradeamounts <- pmin(tradeamounts, maxTrade)  # Limit trade amounts to maxTrade
      opened$position<-ifelse(opened$type=="Long",  # Set position size
                                  trunc(tradeamounts/opened$open),    # Round down to nearest whole number
                                  -trunc(tradeamounts/opened$open))   # Negative position for shorts
      opened$opencash<-ifelse(opened$type=="Long",  # Update cash position
                                  opened$buyprice*opened$position,0)
      opened$opencash<-ifelse(opened$type=="Short",
                                  opened$sellprice*opened$position,opened$opencash)
      opened<-subset(opened,opened$position!=0)  # Remove rows with zero position
      cash<-sum(opened$opencash) - (numtrades*1 + 0.005*opened$position)  # Calculate total cash
    } else {opened<-NULL}  # If no trades, set opened to NULL
  } 
  return(list(opened=opened,cashout=cash))  
}

# Function to apply trading rules based on signals and positions
applyRules=function(currdate,equity,position){
  netopen<-position                                        # netopen will hold all open positions after any close orders
  close.results<-closePositions(currdate,equity,position)  # close any orders for which we have positions and signals
  if (!is.null(close.results$closed)) {                    # Did we actually close out any positions
    temp<-close.results$close[,c(1,2)]                     # if we we need to remove them from our open positions
    names(temp)[2]<-"dummy"                                # we need one field to check if it is empty after the merge
    temp<-merge(position,temp,by="symbol",all.x=TRUE)      # and we don't want to generate duplicate columns, hence dummy
    netopen<-subset(temp,is.na(temp$dummy))                # so if dummy is NA, then the position is not closed
    netopen<-netopen[,c(1:ncol(netopen)-1)]                # get rid of the dummy column
    equity<-equity+close.results$cashin                    # update our equity position with the cash from closing
  }
  open.results<-openPositions(currdate,equity,netopen)     # now check for opening new positions
  return(list(open=open.results$opened,close=close.results$closed,
        posnetofcloses=netopen,cashin=close.results$cash,cashout=open.results$cash))
}

# Function to calculate portfolio statistics
portfolioStats <- function(trades, pvalue, tdays) {
  tradedays <- length(unique(trades$date))  # Count unique trading days
  totaldays <- length(tdays)  # Count total days in analysis
  pctdaystraded <- tradedays / totaldays  # Calculate % of days with trades
  totaltrades <- nrow(trades)  # Count total trades
  pdiff <- c(0, diff(pvalue))  # Calculate daily price difference
  preturn <- pdiff/pvalue + 1  # Calculate daily return on each day
  shorttrades <- sum(trades$type == "Short")  # Count short trades
  longtrades <- sum(trades$type == "Long")  # Count long trades
  # Calculate returns for each trade
  trades$return_long <- ifelse(trades$type == "Long", (trades$outprice / trades$buyprice) - 1, NA)  # Long returns
  trades$return_short <- ifelse(trades$type == "Short", (trades$sellprice / trades$outprice) - 1, NA)  # Short returns
  trades$return <- ifelse(trades$type == "Long", trades$return_long, trades$return_short)  # Overall returns
  # Calculate % of winning trades
  pct_winning_long <- sum(trades$return_long > 0, na.rm = TRUE) / longtrades  # % of winning long trades
  pct_winning_short <- sum(trades$return_short > 0, na.rm = TRUE) / shorttrades  # % of winning short trades
  # Calculate average returns
  avg_return_long <- mean(trades$return_long, na.rm = TRUE)  # Average long return
  avg_return_short <- mean(trades$return_short, na.rm = TRUE)  # Average short return
  # Calculate overall % of winning trades
  overall_pct_winning <- sum(trades$return > 0, na.rm = TRUE) / totaltrades  # Overall % of winning trades
  cumreturn <- cumprod(preturn)  # Cumulative return
  maxreturn <- cummax(cumreturn)  # Maximum return
  maxdraw <- min((cumreturn - maxreturn) / maxreturn) * 100  # Maximum drawdown %
  drawdown_lengths <- rle((cumreturn - maxreturn) / maxreturn < 0)  # Drawdown period lengths
  maxstreak <- max(drawdown_lengths$lengths[drawdown_lengths$values])  # Longest drawdown period
  meanreturn <- mean(preturn, na.rm = TRUE) - 1  # Mean daily return
  sharpe <- meanreturn / sd(preturn, na.rm = TRUE) * sqrt(252)  # Sharpe ratio
  maxy <- max(c(max(cumreturn, na.rm = TRUE), max(maxreturn, na.rm = TRUE), max(preturn, na.rm = TRUE)))  # Max value for y-axis
  # Calculate average number of open positions per day
  avg_open_positions <- mean(table(trades$date))  # Average open positions
  # Calculate average duration for which a position is open
  if ("closedate" %in% names(trades) && "date" %in% names(trades)) {
    trades$duration <- as.numeric(as.Date(trades$closedate) - as.Date(trades$date))  # Calculate duration
    avg_open_duration <- mean(trades$duration, na.rm = TRUE)  # Average open duration
  } else {
    avg_open_duration <- NA  # Set to NA if necessary data is not available
  }
  # Plot the portfolio results
  max_data_value <- max(cumreturn, maxreturn, preturn, na.rm = TRUE)  # Get the maximum data value
  ylim_upper <- max_data_value * 1.02  # Extend the limit by 5% above the maximum value
  plot(tdays, cumreturn, type = "l", col = "black", lwd = 2,
      xlab = "Time Period", ylab = "Portfolio Return",
      main = "Portfolio Results", ylim = c(0.95, ylim_upper),
      xaxt = "n", cex.lab = 0.75, cex.axis = 0.75, cex.main = 0.85)
  # Add lines for maximum return and portfolio return
  lines(tdays, maxreturn, col = "red", lwd = 2)
  lines(tdays, preturn, col = "blue", lwd = 2)
  # Customize the x-axis with date formatting
  axis.Date(1, at = seq(min(tdays), max(tdays), by = "3 months"),
            format = "%b %Y", cex.axis = 0.8)
  # Add gridlines for better readability
  abline(h = seq(0.5, ylim_upper, by = 0.1), col = "lightgray", lty = "dotted")
  abline(v = seq(min(tdays), max(tdays), by = "3 months"), col = "lightgray", lty = "dotted")
  # Add a legend to the plot
  legend("topleft", legend = c("Cumulative Return", "Max Return", "Portfolio Return"),
        col = c("black", "red", "blue"), lwd = 2, bty = "n", cex = 0.75)
  # Compile performance measures into a list
  performance <- list(
    totaltrades = totaltrades,
    longtrades = longtrades,
    pct_winning_long = pct_winning_long * 100,
    avg_return_long = avg_return_long * 100,
    shorttrades = shorttrades,
    pct_winning_short = pct_winning_short * 100,
    avg_return_short = avg_return_short * 100,
    overall_pct_winning = overall_pct_winning * 100,
    cumreturn = cumreturn[length(cumreturn)],
    meanreturn = meanreturn,
    sharpe = sharpe,
    maxdraw = maxdraw,
    maxdraw_period = maxstreak,
    avg_open_positions = avg_open_positions,
    avg_open_duration = avg_open_duration
  )
  return(performance) 
}

# Run the backtesting process
indicators <- NULL  # Initialize an empty data frame for indicators
for (sym in symbols) {  # Loop through each symbol
  temp <- genIndicators(sym)  # Generate indicators for the symbol
  indicators <- rbind(indicators, temp)  # Combine indicators into the data frame
}
signals <- NULL  # Initialize an empty data frame for signals
for (sym in symbols) {  # Loop through each symbol
  temp <- genSignals(sym)  # Generate signals for the symbol
  signals <- rbind(signals, temp)  # Combine signals into the data frame
}
View(signals)
View(universe)
tdays <- unique(signals$date)  # Get unique trading days from signals
position <- NULL  # Initialize empty data frames for tracking positions
closed <- NULL
pvalue <- rep(0, length(tdays))  # Create a vector to track portfolio value
currentcash <- initialequity  # Set initial cash amount
for (day in 1:length(tdays)) {  # Loop through each trading day
  currdate <- tdays[day]  # Get the current date
  print(currdate)  # Print progress update
  results <- applyRules(currdate, currentcash, position)  # Apply trading rules
  position <- rbind(results$posnetofcloses, results$open)  # Update open positions
  closed <- rbind(closed, results$close)  # Track closed positions
  currentcash <- currentcash + results$cashin - results$cashout  # Update cash balance
  if (!is.null(position)) {  # If there are open positions
    temp <- subset(indicators, indicators$date == currdate)[, c(1, 6)]  # Extract current prices
    names(temp)[2] <- "currprice"  # Rename price column
    currpos <- merge(position, temp)  # Merge positions with current prices
    currpos$value <- currpos$position * currpos$currprice  # Calculate position value
    pvalue[day] <- sum(currpos$value, na.rm = TRUE)  # Add position value to portfolio
  } else {
    pvalue[day] <- 0  # Set portfolio value to 0 if no open positions
  }
  pvalue[day] <- pvalue[day] + currentcash  # Add cash balance to portfolio value
}
performance<-portfolioStats(closed,pvalue,tdays) # Plot portfolio results
performance  # Print the portfolio performance measures
performance_df <- as.data.frame(performance)

# Transpose the dataframe
transposed_df <- as.data.frame(t(performance_df))

# Give the transposed dataframe column names (optional, depends on your requirement)
colnames(transposed_df) <- c("Value")

# Print the transposed dataframe
View(transposed_df)

# Visual Analysis of Technical Indicators
mpc_full_data <- signals %>%
  filter(symbol == "MPC") %>%
  arrange(date)

# Plot for Open and Close Prices with Bollinger Bands
price_band_plot <- ggplot(mpc_full_data, aes(x = date)) +
  geom_line(aes(y = open, colour = "Open Price")) +
  geom_line(aes(y = close, colour = "Close Price")) +
  geom_line(aes(y = lowerBand, colour = "Lower Band")) +
  geom_line(aes(y = upperBand, colour = "Upper Band")) +
  scale_colour_manual(values = c("Open Price" = "green", "Close Price" = "black",
                                "Lower Band" = "purple", "Upper Band" = "orange")) +
  theme_minimal() +
  labs(title = "MPC Stock Prices and Bollinger Bands",
      x = "Date", y = "Price") +
  theme(legend.title = element_blank())
print(price_band_plot)

# Plot for MACD and Signal
macd_plot <- ggplot(mpc_full_data, aes(x = date)) +
  geom_line(aes(y = MACD, colour = "MACD")) +
  geom_line(aes(y = signal, colour = "Signal")) +
  scale_colour_manual(values = c("MACD" = "red", "Signal" = "blue")) +
  theme_minimal() +
  labs(title = "MPC MACD and Signal",
      x = "Date", y = "Value") +
  theme(legend.title = element_blank())
print(macd_plot)

# Plot for RSI
rsi_plot <- ggplot(mpc_full_data, aes(x = date)) +
  geom_line(aes(y = RSI, colour = "RSI")) +
  scale_colour_manual(values = c("RSI" = "brown")) +
  theme_minimal() +
  labs(title = "MPC RSI Indicator",
      x = "Date", y = "RSI Value") +
  theme(legend.title = element_blank())
print(rsi_plot)
