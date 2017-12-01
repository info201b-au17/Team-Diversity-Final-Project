install.packages("TTR")
install.packages("quantmod")
library("dplyr")
library("data.table")
library("ggplot2")
library("TTR")
library("quantmod")
# library("Rblpapi")   Not 100% sure if we need this. But leaving it for now just be safe


# Function to fetch google stock data
google_stocks <- function(sym, start.date, end.date) {
  # sy, sm, sd, ey, em, ed correspond to
  # start year, start month, start day, end year, end month, and end day
  
  require(data.table)
  
  # Fetch data from google
  google_out = tryCatch(
    suppressWarnings(
      fread(paste0("http://www.google.com/finance/historical",
                   "?q=", sym,
                   "&startdate=", paste(start.date, sep = "+"),
                   "&enddate=", paste(end.date, sep = "+"),
                   "&output=csv"), sep = ",")),
    error = function(e) NULL
  )
  View(google_out)
  # If successful, rename first column
  if(!is.null(google_out)){
    names(google_out)[1] = "Date"
  }
  start.date <- as.Date(start.date, format = "%d-%b-%y")
  google_out <- filter(google_out, as.Date(Date, format = "%d-%b-%y") > start.date)
  return(google_out)
}

# Fetches the data for an individual stock
ROKU_data <- google_stocks('ROKU', "01-Nov-17", Sys.Date())
View(ROKU_data)

ROKU_data$Date <- as.Date(ROKU_data$Date, format = "%d-%b-%y")

ggplot(ROKU_data, aes(Date, Close, group = 1)) +
  geom_point(aes(color = Volume)) +
  geom_line() 

SP500_ETF_data <- google_stocks("SPY", "01-Nov-10", Sys.Date())    # S&P500 ETF Fund
View(SP500_ETF_data)
# Fetches the S&P500 Data from a certain year to a certain year
sp500 <- new.env()
sp500.data <- function(start.date, end.date) {
  start.date <- "2014-01-01"
  end.date <- Sys.Date()
  getSymbols("^GSPC", src = "yahoo", from=start.date, to=end.date)
  SPC <- GSPC$GSPC.Close  # plot(SPC)
}

sp500.2014.data <- sp500.data("2014-01-01", Sys.Date())

# Fetches the NASDAQ Data from a certain year to a certain year
nasdaq<- new.env()
nasdaq.data <- function(start.date, end.date) { 
  getSymbols("^NDX", env = nasdaq, src = "yahoo", from=start.date, to=end.date)
  ndx <- nasdaq$NDX
  NDX <- ndx$NDX.Close
}

nasdaq.2014.data <- nasdaq.data("2014-01-01", Sys.Date())

# Fetches the Dow Jones Data from a certain year to a certain year
dowjones<- new.env()
dow.jones.data <- function(start.date, end.date) {
  getSymbols("^DJI", env = dowjones, src = "yahoo", from=start.date, to=end.date)
  djia <- dowjones$DJI
  DJIA <- djia$DJI.Close
}

dow.Jones.2014.data <- dow.jones.data("2014-01-01", Sys.Date())

# Fetches the Ticker Symbol of a given stock given the name of the company
listings <- stockSymbols()
get.stock.ticker <- function(stock.name) {
  stock.ticker <- listings %>% filter(grepl(stock.name, listings$Name)) %>% select(Symbol)
}

Apple.Ticker <- get.stock.ticker("Apple Inc.")
JohnsonJohnson.Ticker <- get.stock.ticker("Johnson & Johnson")
