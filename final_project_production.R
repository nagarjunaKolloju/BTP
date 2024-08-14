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
library(tidytext) # Loads the tidytext package, used for text mining and analysis
library(httr) # Loads the httr package, used for HTTP requests
library(jsonlite) # Loads the jsonlite package, used for JSON data manipulation
library(IBrokers) # Loads the IBrokers package, used for Interactive Brokers API integration
current_path <- rstudioapi::getActiveDocumentContext()$path  # Get current R script path (optional)
setwd(dirname(current_path)) # Set the working directory to the location of the script (optional)
rm(list = ls()) # Remove all objects from the current R environment
options(scipen = 999) # Set the scientific notation penalty to 999 (optional)
Quandl.api_key('juF1uFeb6zXckqi_zddC') # Set the API key for NASDAQ data 
API_KEY <- "c2a4466856af4567afb7019a526e9174" # Set the API key for the News API

IBport <- 7497 # Initial portfolio value
maxTrade <- 9000 # Maximum allowed trade amount
maxLong <- 20 # Maximum number of day trades allowed
maxShort <- 20 # Maximum number of day trades allowed
trade_date <- Sys.Date() # Current date for trading
symbols <- sp500 <- tq_index("SP500")[, c(1, 6)]$symbol  # Load S&P 500 symbols into the 'symbols' variable
symbols <- symbols[symbols != "-"]  # "-" from the list of symbols, as it is not a valid symbol

# Function to fetch S&P 500 data and data for each symbol in the index YAHOO FINACE To Verify
getSP500Data <- function() {
  data_list <- vector("list", length(symbols)) # Create a list to store data for each symbol
  names(data_list) <- symbols # Set the names of the list to the symbol names
  for (symbol in symbols) { # Loop through each symbol to fetch data
    tryCatch({ # Use tryCatch to handle errors and continue fetching data for other symbols
      print(paste("Fetching data for", symbol)) # Print a message to indicate progress
      data_list[[symbol]] <- Quandl.datatable("SHARADAR/SEP", date.gte=trade_date-100,ticker=symbol) # Fetch data for the symbol
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

marketData <- getSP500Data()
insiderData <- getSP500InsiderData()
insiderData <- preprocessInsiderData(insiderData)
marketData <- left_join(marketData, insiderData, by = c("date" = "transactiondate", "ticker" = "ticker"))
marketData <- select(marketData, ticker, date, open, high, low, close, volume, insiderAvgTransaction)
write.csv(marketData, "productionFinal.csv", row.names = FALSE)
# Generate technical indicators
genIndicators <- function(sym, stock) {
    print(paste('Generating Indicators for symbol:', sym)) # Print a message to indicate progress
    if (nrow(stock) == 0) { # Check if data is available for the symbol
      stop(paste("No data found for symbol:", sym)) # Stop and print an error message
    } 
    # Add new row with today's date and NA for other columns
    new_row <- tibble(date = trade_date,ticker = sym,open = NA,high = NA,low = NA,
    close = NA,volume = NA,lagged_close=NA,RSI=NA,MACD=NA,signal=NA,lowerBand=NA,upperBand=NA, range = NA,insiderSignal=NA)
    stock <- bind_rows(stock, new_row) # Add the new row to the data frame
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
    stock <- subset(stock, date == trade_date) # To start backtesting from 2023 Jan
    row.names(stock) <- 1:nrow(stock)  # Reset rownames
    return(stock)  
}



# Generate trading signals
genSignals <- function(stock) {
  if (nrow(stock) == 0) { # Check if data is available
    message("No data available.")
    return(NULL)
  }
  message(paste('Generating Signals for date:', as.Date(trade_date))) # Print a message to indicate progress
  # Filter data for the current date and calculate trading signals
  stock$insiderInfluence <- (stock$insiderSignal / max(abs(stock$insiderSignal), na.rm = TRUE)) * 0.2
  # Adjust RSI thresholds based on insider influence
  stock$RSI_entry_threshold <- 30 + (stock$insiderInfluence * 5)  # Increase RSI entry threshold if negative influence
  stock$RSI_exit_threshold <- 70 + (stock$insiderInfluence * 5)
  stock <- mutate(stock,
      long.entry = ifelse((lagged_close < lowerBand & MACD > signal) | RSI < RSI_entry_threshold, 1, 0),
      # Long exit conditions
      long.exit = ifelse(lagged_close > lowerBand | RSI > 45 | MACD < signal, 1, 0),
      # Short entry conditions
      short.entry = ifelse(lagged_close > upperBand & RSI > RSI_exit_threshold & MACD < signal, 1, 0),
      # Short exit conditions
      short.exit = ifelse(lagged_close < upperBand | RSI < 45 | MACD > signal, 1, 0),
      currentPrice = lagged_close  # Assuming 'lagged_close' represents the latest available price
  )
  stock <- stock %>% 
  select(date, symbol, long.entry, short.entry, long.exit, short.exit, currentPrice, range) %>%
  na.omit()
  # Testing Purposes  
  new_row <- data.frame(
    date = c(Sys.Date(), Sys.Date()),
    symbol = c("AAPL", "MSFT"), 
    long.entry = c(1, 0),
    short.entry = c(0, 1),
    long.exit = c(0, 0),
    short.exit = c(0, 0),
    currentPrice = c(100, 200),
    range = c(5, 10)
  )
  stock <- rbind(stock, new_row)
  bing_lexicon <- get_sentiments("bing")  # Load Bing sentiment lexicon
  for (i in 1:nrow(stock)) {
    symbol <- stock$symbol[i]
    long.entry <- as.numeric(stock$long.entry[i])
    short.entry <- as.numeric(stock$short.entry[i])
    sentiment <- "neutral"
    if (long.entry == 1 | short.entry == 1) {
      url <- paste0("https://newsapi.org/v2/everything?q=", symbol, "&from=", 
      as.Date(trade_date - 5), "&to=", as.Date(trade_date), "&sortBy=popularity&apiKey=", API_KEY)
      news <- GET(url)
      if (status_code(news) == 200) {
        jsonData <- fromJSON(rawToChar(news$content))
        titles <- jsonData$articles$description[1:20]
        sentiment_scores <- tibble(title = titles) %>%
          unnest_tokens(word, title) %>%
          inner_join(bing_lexicon, by = "word") %>%
          summarise(sentiment = sum(case_when(sentiment == "positive" ~ 1, sentiment == "negative" ~ -1, TRUE ~ 0)))
        if (sentiment_scores$sentiment > 0) { sentiment <- "positive"} 
        else if (sentiment_scores$sentiment < 0) { stock$sentiment <- "negative"}
      }
    }
    stock$long.entry[i] <- ifelse(sentiment == "positive" && long.entry == 1, 1, 0)
    stock$short.entry[i] <- ifelse(sentiment == "negative" && short.entry == 1, 1, 0)
  }
  if (nrow(stock) > 0) {
    message("Signals generated successfully.")
    # Select specific columns to return, remove any rows containing NA values
    return (stock)
  } else {
    message("No trading data available for the specified date.")
    return(NULL)
  }
}



processDataAndSignals <- function(marketData) {
  # Generate indicators for each unique symbol
  indicators_list <- lapply(unique(marketData$ticker), function(sym) {
    sym_data <- marketData[marketData$ticker == sym, ] # Filter data for the symbol
    genIndicators(sym, sym_data) # Generate indicators for the symbol
  })
  indicators <- do.call(rbind, indicators_list) # Combine the indicators for all symbols
  rownames(indicators) <- seq(1, nrow(indicators), 1) # Add row numbers to the data frame
  signals <- genSignals(indicators) # Generate trading signals
  rownames(signals) <- seq(1, nrow(signals), 1) # Add row numbers to the data frame
  return(signals)
}

tradingSignals <- processDataAndSignals(marketData) # Process data and generate trading signals
View(tradingSignals) # View the trading signals data frame
# Function to review trades before execution
reviewTrades <- function(equity, trades, CLOSING = FALSE) {
  initialEquity <- equity  # Keep track of initial equity for final calculation
  done <- FALSE
  while (!done && nrow(trades) > 0) {
    print(trades[, c("symbol", "action", "currentPrice", "quantity")])
    choice <- toupper(readline(prompt = "Choose D)elete trade, C)hange position, M)odify Price, E)xecute, Q)uit without trading: "))
    cat("Your choice: ", choice, "\n")  # This will print the choice and add a newline
    switch(choice,
      "Q" = {
        print("Exiting without trading.")
        return(list(trades = NULL, equity = initialEquity))
      },
      "E" = {
        if (sum(trades$currentPrice * trades$quantity) > equity) {
          print("Total trade value exceeds available equity. Please adjust your trades.")
        } else {
          print("Executing all reviewed trades.")
          done <- TRUE
        }
      },
      "D" = {
        rownum <- as.integer(readline("Enter the row number of the trade to delete: "))
        if (!is.na(rownum) && rownum >= 1 && rownum <= nrow(trades)) {
          trades <- trades[-rownum, , drop = FALSE]
          rownames(trades) <- seq_len(nrow(trades))
          print("Trade deleted.")
        } else {
          print("Invalid row number.")
        }
      },
      "M" = {
        rownum <- as.integer(readline("Enter the row number of the trade to modify the price: "))
        if (rownum >= 1 && rownum <= nrow(trades)) {
          newPrice <- as.numeric(readline("Enter the new price: "))
          if (!is.na(newPrice) && newPrice > 0) {
            potential_new_total = sum(trades$currentPrice * trades$quantity) - (trades$currentPrice[rownum] * trades$quantity[rownum]) + (newPrice * trades$quantity[rownum])
            if (potential_new_total > equity) {
              print("Modifying to this price exceeds available equity.")
            } else {
              trades$currentPrice[rownum] <- newPrice
              print("Price modified.")
            }
          } else {
            print("Invalid price entered.")
          }
        } else {
          print("Invalid row number.")
        }
      },
      "C" = {
        rownum <- as.integer(readline("Enter the row number of the trade to change the quantity: "))
        if (rownum >= 1 && rownum <= nrow(trades)) {
          newPosition <- as.numeric(readline("Enter the new position size: "))
          if (!is.na(newPosition) && newPosition > 0) {
            if (CLOSING && newPosition > trades$quantity[rownum]) {
              print("Cannot increase position size for closing trades.")
            } else {
              potential_new_total = sum(trades$currentPrice * trades$quantity) - (trades$currentPrice[rownum] * trades$quantity[rownum]) 
              + (trades$currentPrice[rownum] * newPosition)
              if (potential_new_total > equity) {
                print("Increasing to this quantity exceeds available equity.")
              } else {
                trades$quantity[rownum] <- newPosition
                print("Position size changed.")
              }
            }
          } else { print("Invalid position size entered. Enter a positive number.") }
        } else { print("Invalid row number.") }
      },
      { print("Invalid option. Please choose again.") }
    )
  }
  # Calculate the final equity after all trades based on short or long
  cashOut <- sum(trades$currentPrice[trades$action == "BUY"] * trades$quantity[trades$action == "BUY"])
  cashIn <- sum(trades$currentPrice[trades$action == "SELL"] * trades$quantity[trades$action == "SELL"])
  finalEquity <- initialEquity + cashIn - cashOut
  print(paste("Final Equity:", finalEquity))
  return(list(trades = trades, equity = finalEquity))
}



# Function to place orders for trades
placeOrderTrades <- function(trades, twsConnection, CLOSING = FALSE) {
  if (is.null(twsConnection) || !isConnected(twsConnection)) {
    twsConnection <- twsConnect(port = 7497)
    print("Reconnected to TWS.")
  }
  if (!is.null(trades) && nrow(trades) > 0) {
    for (i in seq_len(nrow(trades))) {
      if (is.na(trades$symbol[i]) || is.na(trades$action[i]) || is.na(trades$quantity[i])) {
          print(paste("Invalid data for trade", i, ": skipping."))
          next  # Skip this iteration
      }
      symbol <- trades$symbol[i]
      action <- trades$action[i]
      numShares <- trades$quantity[i]
      currentPrice <- trades$currentPrice[i]  # Assuming this column exists and is correctly populated
      type_of_order = ifelse(CLOSING, "MOC", "MKT") # Market on Close or Market on Open
      print("Placing order...") 
      contract <- tryCatch({
          twsEquity(as.character(symbol), "SMART", primary = "ISLAND", currency = "USD")
      }, error = function(e) {
          print(paste("Failed to create contract for", symbol, ": ", e$message))
          return(NULL)
      })
      if (is.null(contract)) next  # Skip this trade if contract creation failed
      orderId <- reqIds(twsConnection)
      order <- tryCatch({
          twsOrder(orderId, action = action, totalQuantity = numShares, orderType = type_of_order)
      }, error = function(e) {
          print(paste("Failed to create order for", symbol, ": ", e$message))
          return(NULL)
      })
      if (is.null(order)) next  # Skip this trade if order creation failed
      # Attempt to place the order
      result <- tryCatch({
          Id = placeOrder(twsConnection, contract, order)
          
          TRUE
      }, error = function(e) {
          print(paste("Failed to place order for", symbol, ": ", e$message))
          FALSE
      })
      if (result) {
          print(paste("Order successfully placed for", symbol, "to", action, "with quantity", numShares))
      }
      else{ print(paste("Failed to place order for", symbol, "to", action, "with quantity", numShares))}
    # Sys.sleep(3) # Wait for 2 seconds before placing the next order
    }
  } else { print("No valid trades to process or connection issue.")}
}


# Main function to process and execute trades
executeTrades <- function(signals, positions, equity, maxTrade, twsConnection) {
  # Process close trades
  closeTrades <- signals[signals$long.exit == 1 | signals$short.exit == 1, ] # Filter trades to close
  closeTrades <- enrichTradeInfo(closeTrades, positions, CLOSING = TRUE)  # Function to add action, price, quantity based on current positions
  if(!is.null(closeTrades) && nrow(closeTrades) > 0){
    print("Executing close trades:")
    closedResults <- reviewTrades(equity, closeTrades, CLOSING = TRUE) # Review trades before execution
    equity<-closedResults$equity # Update equity after closing trades
    placeOrderTrades(closedResults$trades, twsConnection, CLOSING = TRUE) # Place orders for closing trades
  }
  Sys.sleep(5)  # Wait for a few seconds before executing open trades
  # Process open trades
  openTrades <- signals[signals$long.entry == 1 | signals$short.entry == 1, ] # Filter trades to open
  openTrades <- enrichTradeInfo(openTrades, positions, CLOSING = FALSE, equity, maxTrade)  # Calculate trade details like action, price, quantity
  if(!is.null(openTrades) && nrow(openTrades) > 0){
    print("Executing open trades...")
    openResults<- reviewTrades(equity, openTrades) # Review trades before execution
    placeOrderTrades(openResults$trades, twsConnection) # Place orders for opening trades
  }
  print("Done with trade execution.")
}

enrichTradeInfo <- function(trades, positions, CLOSING = FALSE, equity = NULL, maxTrade = NULL) {
  if (CLOSING) {
    # Subset positions into long and short positions
    openLong <- subset(positions, position > 0)  # Filter long positions
    openShort <- subset(positions, position < 0) # Filter short positions
    # Merge trade details with open long and short positions
    longTrades <- merge(trades, openLong, by = "symbol", all.x = FALSE)
    shortTrades <- merge(trades, openShort, by = "symbol", all.x = FALSE)
    # Assign actions based on the type of trades - for long positions we want to SELL to close
    longTrades$action <- ifelse(longTrades$long.exit == 1, "SELL", "HOLD")  # 'HOLD' or other default if no exit
    longTrades$quantity <- abs(longTrades$position)  # Use absolute value to ensure positive quantities
    # Assign actions for short positions - for short positions we want to BUY to close
    shortTrades$action <- ifelse(shortTrades$short.exit == 1, "BUY", "HOLD")  # 'HOLD' or other default if no exit
    shortTrades$quantity <- abs(shortTrades$position)
    trades <- rbind(longTrades, shortTrades) # Combine the long and short trades
    trades <- subset(trades, action != "HOLD") # Remove trades where no action is required
    return (trades)
  } else {
    num_open_trades <- nrow(trades)
    if (num_open_trades > 0 && !is.null(equity) && !is.null(maxTrade) && equity > 0 && maxTrade > 0) {
      longTrades <- subset(trades, long.entry == 1)  # Filter long trades
      shortTrades <- subset(trades, short.entry == 1)  # Filter short trades
      longTrades <- head(longTrades, maxLong)  # Limit the number of long trades
      shortTrades <- head(shortTrades, maxShort)  # Limit the number of short trades
      trades <- rbind(longTrades, shortTrades)  # Combine long and short trades
      if(nrow(trades) > 0){
        softmax_scores <- softmax(1/trades$range)
        tradeAmounts <- softmax_scores * equity
        tradeAmounts <- pmin(tradeAmounts, maxTrade)
        trades$quantity <- trunc(tradeAmounts/trades$currentPrice)
        trades$action <- ifelse(trades$long.entry == 1, "BUY", "SELL")
        return(trades)
      }
      return (NULL)
    } else {
      if (num_open_trades == 0) {
        print("No open trades to process.")
      } else {
        print("Insufficient funds or invalid trade parameters.")
      }
      return(NULL)
    }
  }
  print("No trades to process.")
  return(NULL)
}

# Function to close open network connections
openCons <- showConnections(all = TRUE) # Get all open connections
for (i in 1:nrow(openCons)) { # Loop through each connection
    conn_info <- openCons[i, ] # Get connection information
    if (conn_info["class"] == "sockconn" && conn_info["isopen"] == "opened") { # Check if it's a socket connection and open
        conn_id <- as.numeric(rownames(openCons)[i]) # Get the connection ID
        message(paste("Closing network connection with ID:", conn_id)) # Print a message
        close(getConnection(conn_id)) # Close the connection
    } # End of if statement
} # End of loop

# Function to connect to TWS with retry logic
connectToTWS <- function(port = IBport) { # Default port for TWS
  tryCatch({ # Use tryCatch to handle errors
    message("Attempting to connect to TWS...") # Print a message
    conn <- twsConnect(port = port) # Connect to TWS
    if (is.null(conn)) { # Check if the connection is successful
      stop("Failed to establish a connection.") # Stop and print an error message
    } # End of if statement
    message("Connected to TWS successfully.") # Print a success message
    return(conn) # Return the connection object
  }, error = function(e) { # Handle errors
    message("Error during connection: ", e$message) # Print the error message
    return(NULL)  # Return NULL if the connection fails
  }) # End of tryCatch block
} # End of function

# Function to disconnect from TWS
retryConnect <- function(maxRetries = 5) { # Default maximum number of retries
  attempts <- 1 # Initialize the number of attempts 
  while (attempts <= maxRetries) { # Loop until the maximum number of retries is reached
    conn <- connectToTWS() # Attempt to connect to TWS
    if (!is.null(conn)) { # Check if the connection is successful
      return(conn) # Return the connection object
    } # End of if statement
    message(paste("Retrying connection attempt:", attempts)) # Print a message
    attempts <- attempts + 1 # Increment the number of attempts
    Sys.sleep(5)  # wait for 5 seconds before retrying
  } # End of while loop
  stop("Connection attempts exceeded, check TWS/IB Gateway settings and network.") # Stop and print an error message
} # End of function

# Function to request account updates
twsConnection <- retryConnect() # Connect to TWS
if (is.null(twsConnection)) { # Check if the connection is successful
  stop("Failed to connect to TWS/IB Gateway.") # Stop and print an error message
} # End of if statement


portfolioInfo <- reqAccountUpdates(twsConnection) # Request account updates
if (is.null(portfolioInfo)) { # Check if the account updates are successful
  stop("Failed to retrieve account information.") # Stop and print an error message
} # End of if statement
equity <- as.numeric(portfolioInfo[[1]]$AvailableFunds[1])/2 # Get the available funds for trading
print(paste("Available funds for trading:", equity)) # Print the available funds
currentPositions <- twsPortfolioValue(portfolioInfo) # Get the current positions
print(currentPositions)
## Testing Purposes
if(is.null(currentPositions)){
  currentPositions <- data.frame(symbol = character(), 
                                sectype = character(), 
                                marketValue = numeric(), 
                                averageCost = numeric(), 
                                return = numeric(), 
                                position = numeric(), 
                                realizedPNL = numeric())
  new_row <- data.frame(symbol = c("QCOM", "AMGN"), 
                      sectype = "STK", 
                      marketValue = c(runif(1), runif(1)), 
                      averageCost = c(runif(1), runif(1)), 
                      return = c(runif(1), runif(1)), 
                      position = c(100, -100), 
                      realizedPNL = c(0, 0))
  currentPositions <- rbind(currentPositions, new_row)
} else {
  colnames(currentPositions)[colnames(currentPositions) == "local"] <- "symbol"
}
print("Current positions:") # Print a message
print(currentPositions) # Print the current positions              
executeTrades(tradingSignals, currentPositions, equity, maxTrade, twsConnection) # Execute trades
# Disconnect from TWS
twsDisconnect(twsConnection)




