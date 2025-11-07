#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#

#==============================================================================#
#==== 1 - Working Directory & Libraries =======================================#
#==============================================================================#

silent=F
.libPaths()

Path <- "C:/Users/TristanLeiter/Documents/Privat/ARM2/05_Project"

#==== 1A - Libraries ==========================================================#

## Needs to enable checking for install & if not then autoinstall.

packages <- c("xts")

for(i in 1:length(packages)){
  package_name <- packages[i]
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, character.only = TRUE)
    cat(paste("Package '", package_name, "' was not installed. It has now been installed and loaded.\n", sep = ""))
  } else {
    cat(paste("Package '", package_name, "' is already installed and has been loaded.\n", sep = ""))
  }
  library(package_name, character.only = TRUE)
}

#==== 1B - Functions ==========================================================#



#==== 1C - Parameters =========================================================#

## Time Period.
system_date <- Sys.Date()
start_date_train <- "1926"
end_date_train <- "2015"
train_period <- paste(start_date_train, "/", end_date_train, sep = "")

## Paths.
Data_Path <- file.path(Path, "02_Data")
Charts_Path <- file.path(Path, "03_Charts")


### Plot Parameters
width <- 7500/4
height <- 2750/3


#==============================================================================#
#==== 02 - Paper Replication (Moreira and Muir (2017)) ========================#
#==============================================================================#

#==== 02A - Source the Input Data =============================================#

## KF-Data is obtained from "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html".
Path <- file.path(Data_Path, "F-F_Research_Data_Factors_daily.csv")
Data <- read.csv(Path) # Loads the Kenneth French Data (Fama/French 3 Factors [Daily]).
Data <- Data[-nrow(Data),] # Removes the last row. Not relevant for us.

Date <- as.Date(Data$Date, format = "%Y%m%d")
Mkt_ret <- xts(Data$Mkt.RF, Date)

#==== 02B - Main Computation (realized variance) ==============================#

## For each month t, compute the realized variance using all daily returns within
## that month. Note that the number of trading days per month is not always the
## same.
## Hint: Estimate ˆσ2t (f) over month t rather than from 22 trading days preceding month t + 1.



#==============================================================================#
#==== 03 - Analysis ===========================================================#
#==============================================================================#



#==============================================================================#