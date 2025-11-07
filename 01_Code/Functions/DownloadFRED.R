#' Downloads Data from FRED
#'
#' Conveniently downloads data from FRED using the fredr package.
#' @param Tickers A character vector of FRED series IDs.
#' @param start_date A date object to indicate the start of the period download.
#' @param end_date A date object to indicate the end of the period download. The System Date is set to default.
#' @param api_key A character string for your FRED API key.
#' @param frequency A character string for the data frequency (e.g., "m", "d", "q"). Default is "m".
#' @importFrom fredr fredr_set_key
#' @importFrom fredr fredr
#' @importFrom xts xts
#' @export

DownloadFRED <- function(Tickers,
                         start_date,
                         end_date = NULL,
                         api_key,
                         frequency = "m") {
  
  # Set the API key
  fredr_set_key(api_key)
  
  # Set default end date if NULL
  if (is.null(end_date)) {
    end_date <- Sys.Date()
  }
  
  # Loop over all tickers using lapply (similar to your Yahoo function)
  list_of_xts <- lapply(Tickers, function(ticker) {
    
    # Download data for one ticker
    Temp <- fredr(series_id = ticker,
                  observation_start = start_date,
                  observation_end = end_date,
                  frequency = frequency)
    
    # Convert the fredr data.frame to an xts object
    xts(Temp$value, as.Date(Temp$date))
  })
  
  # Combine the list of xts objects into one
  data_set <- do.call(cbind, list_of_xts)
  
  # Set column names
  colnames(data_set) <- Tickers
  
  return(data_set)
}