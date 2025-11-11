#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#

## To-Do:
## Part 1: Paper replication: Configuration of the combined plot. Other stuff is done.
##                            Regression output nicely with stargazer.

#==============================================================================#
#==== 1 - Working Directory & Libraries =======================================#
#==============================================================================#

silent=F
.libPaths()

Path <- file.path(here::here(""))  ## Uses the here package. Needs to be installed first.
# Path <- "C:/Users/TristanLeiter/Documents/Privat/ARM2/MoreiraMuir_2017_Replication"

#==== 1A - Libraries ==========================================================#

## Needs to enable checking for install & if not then autoinstall.

packages <- c("xts", "here",
              "lubridate",    ## month() function.
              "ggplot2", "dplyr", "quantmod", "patchwork", "fredr",
              "scales", "stargazer",
              "sandwich",    ## For Neway-West adjusted SE.
              "lmtest"
              )

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

## DownloadFRED.
source("01_Code/Functions/DownloadFRED.R")

## Nice plot formating.
theme_nice <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

#==== 1C - Parameters =========================================================#

## Time Period.
system_date <- Sys.Date()
start_date_train <- "1926"
end_date_train <- "2015"
train_period <- paste(start_date_train, "/", end_date_train, sep = "")
test_period <- paste(start_date_train, "/", "2025-07-31", sep = "")

## Paths.
Data_Path <- file.path(Path, "02_Data")
Charts_Path <- file.path(Path, "03_Charts")

PaperReplication_Charts_Path <- file.path(Charts_Path, "Replication")
PaperExtension_Charts_Path <- file.path(Charts_Path, "Extension")
PaperExtensionFactor_Charts_Path <- file.path(Charts_Path, "ExtensionFactor")


### Plot Parameters
blue <- "#004890"
red <- "#B22222"
orange <- "#F37021"
gold <- "#BBBB1F"
grey <- "#708090"

width <- 3750
height <- 1833


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
Mkt_ret <- Mkt_ret / 100 ## To have the returns in decimals, not in percent.

rf_ret <- xts(Data$SMB, Date)
rf_ret <- rf_ret / 100 ## To have the returns in decimals, not in percent.

Size_ret <- xts(Data$SMB, Date)
Size_ret <- Size_ret / 100 ## To have the returns in decimals, not in percent.

#==== 02B - Main Computation - Computation Replication ========================#

## For each month t, compute the realized variance using all daily returns within
## that month. Note that the number of trading days per month is not always the
## same.
## Hint: Estimate ˆσ2t (f) over month t rather than from 22 trading days preceding month t + 1.

##===============##
## Parameters and Data Preparation.
##===============##

## Data preparation.
Mkt_ret_train <- Mkt_ret[train_period]
risk_free_2015 <- rf_ret[train_period]

##===============##
## Compute the monthly volatility.
##===============##

tryCatch({

## First get the month change Position. All 0's and the 1 are in the same month.
date_monthly <- month(as.Date(index(Mkt_ret_train), format="%d/%m/%Y"))
date_monthly <- diff(date_monthly)
date_monthly[date_monthly < 0] <- 1

dates <- index(Mkt_ret_train)
dates <- dates[date_monthly == 1]

last_Date <- as.Date(index(Mkt_ret_train[nrow(Mkt_ret_train),]), format="%d/%m/%Y")
first_Date <- as.Date(index(Mkt_ret_train[1,]), format="%d/%m/%Y")-1
dates <- c(first_Date,dates, last_Date)

## Compute the realized variance per month.

n_Months <- length(dates)-1
realized_variance_monthly_loop <- numeric(n_Months)

for(period in 1:n_Months){
  Time_Period <- paste(dates[period]+1, "/", dates[period+1], sep = "")
  Mkt_ret_train_Temp <- Mkt_ret_train[Time_Period]
  Variance <- var(as.numeric(Mkt_ret_train_Temp))
  realized_variance_monthly_loop[period] <- Variance
  
}

}, silent = TRUE)

## Now implement it with the function in the xts package.
realized_variance_monthly <- apply.monthly(Mkt_ret_train, FUN = var) ## Realized variance.

## Check if we can use the function. Yes we can do it since none are different.
any(!realized_variance_monthly_loop == realized_variance_monthly) ## Check whether they align.

##===============##
## Construct the VM-portfolio.
##===============##

tryCatch({

## Compute the monthly portfolio return.
ret_incl_rf <- Mkt_ret_train + risk_free_2015
return_incl_rf_monthly <- apply.monthly(ret_incl_rf, FUN = function(x) prod(1 + x) - 1)
return_rf_monthly <- apply.monthly(risk_free_2015, FUN = function(x) prod(1 + x) - 1)
return_monthly <- return_incl_rf_monthly - return_rf_monthly

## Set up the data for the VM-portfolio construction.
realized_variance_monthly_lagged <- stats::lag(realized_variance_monthly, k = 1)
combined_data <- merge(return_monthly, realized_variance_monthly_lagged, join = "inner")
colnames(combined_data) <- c("monthly_return", "lagged_variance")

managed_return_unscaled <- combined_data$monthly_return / combined_data$lagged_variance

## Compute the constant c.
## We choose c so that the managed portfolio has the same unconditional standard 
## deviation as the buy-and-hold portfolio.

sd_original <- sd(combined_data$monthly_return[-1]) ## To have the same length.
sd_unscaled <- sd(managed_return_unscaled, na.rm = TRUE)

c <- sd_original / sd_unscaled

## Now scale the managed returns.
managed_return_scaled <- c * managed_return_unscaled
colnames(managed_return_scaled) <- "managed_return"

}, silent = TRUE)

##===============##
## Construct the VM-portfolio.
##===============##

tryCatch({
  
quintile_breaks <- quantile(combined_data$lagged_variance, 
                            probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                            na.rm = TRUE)
  
quintile_groups <- cut(combined_data$lagged_variance,
                       breaks = quintile_breaks,
                       labels = c("Low Vol", "2", "3", "4", "High Vol"),
                       include.lowest = TRUE)

combined_data$quintile_group <- quintile_groups
  
}, silent = TRUE)

##===============##
## Build the four bar charts.
##===============##

tryCatch({
  
NBER_recessions <- DownloadFRED(Tickers = "USREC",
                                start_date = as.Date("1926-01-01"),
                                end_date = Sys.Date(),
                                api_key = "ea754f3d4a236f5d4b2c8957fae36c4a")

Data_Visualisation <- cbind(combined_data, 
                            managed_return_scaled, 
                            NBER_recessions)
Data_Visualisation <- na.locf(Data_Visualisation, fromLast = TRUE)
Data_Visualisation <- Data_Visualisation[as.Date(index(combined_data))]

## Remove the first row, just a duplicate since we have no data for it due to the scaling.
Data_Visualisation <- Data_Visualisation[-1,]
colnames(Data_Visualisation) <- c("monthly_return", "lagged_variance", "quintile_group", 
                                  "managed_return", "NBERREC")
Data_Visualisation_df <- data.frame(Date = index(Data_Visualisation), coredata(Data_Visualisation))

## Ensure the correct labels for plotting.
Pos <- which(Data_Visualisation_df$quintile_group == 1)
Data_Visualisation_df[Pos,"quintile_group"] <- "Low Vol"

Pos <- which(Data_Visualisation_df$quintile_group == 5)
Data_Visualisation_df[Pos,"quintile_group"] <- "High Vol"

## now compute the summary statistics for each quintile.

Data_Summary_df <- Data_Visualisation_df %>%
  mutate(quintile_group = factor(quintile_group, 
                                 levels = c("Low Vol", "2", "3", "4", "High Vol"))) %>%
  filter(!is.na(quintile_group)) %>%
  group_by(quintile_group) %>%
  summarise(
    avg_return_ann = mean(monthly_return, na.rm = TRUE) * 12,
        sd_return_ann = sd(monthly_return, na.rm = TRUE) * sqrt(12),
          e_r_var_r = mean(monthly_return, na.rm = TRUE) / var(monthly_return, na.rm = TRUE),
            prob_recession = mean(NBERREC, na.rm = TRUE)
  )

}, silent = TRUE)

##===============##
## Plot the four bar charts.
##===============##

tryCatch({

## Plot 1: Average annualized return.
p1 <- ggplot(Data_Summary_df, aes(x = quintile_group, y = avg_return_ann)) +
  geom_col(fill = blue) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Average Annualized Return", x = NULL, y = "Ann. Return") +
  theme_nice()

Path <- file.path(PaperReplication_Charts_Path, "01_Average_Annualized_Return.png")
ggsave(
  filename = Path,
  plot = p1,
  width = width,
  height = height,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

## Plot 2: Annualized volatility.
p2 <- ggplot(Data_Summary_df, aes(x = quintile_group, y = sd_return_ann)) +
  geom_col(fill = red) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Annualized Standard Deviation", x = NULL, y = "Ann. Std. Dev.") +
  theme_nice()

Path <- file.path(PaperReplication_Charts_Path, "02_Annualized_Volatility.png")
ggsave(
  filename = Path,
  plot = p2,
  width = width,
  height = height,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

## Plot 4: Ratio E[R] / Var(R)
p3 <- ggplot(Data_Summary_df, aes(x = quintile_group, y = e_r_var_r)) +
  geom_col(fill = orange) +
  labs(title = "Ratio E[R] / Var(R)", x = "Variance Quintile", y = "Ratio") +
  theme_nice()

Path <- file.path(PaperReplication_Charts_Path, "03_Ratio_ExpReturn_Variance.png")
ggsave(
  filename = Path,
  plot = p3,
  width = width,
  height = height,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

## Plot 4: Recession Probability.
p4 <- ggplot(Data_Summary_df, aes(x = quintile_group, y = prob_recession)) +
  geom_col(fill = gold) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Probability of Recession", x = "Variance Quintile", y = "Probability") +
  theme_nice()

Path <- file.path(PaperReplication_Charts_Path, "04_Recession_Probability.png")
ggsave(
  filename = Path,
  plot = p4,
  width = width,
  height = height,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

## Plot them together.
## Need to work on the configuration of the combined plot.
# Combined_Plot <- (p1 | p2) / (p3 | p4)
# 
# Path <- file.path(PaperReplication_Charts_Path, "05_Combined_Plot.png")
# ggsave(
#   filename = Path,
#   plot = Combined_Plot,
#   width = width,
#   height = height,
#   units = "px",
#   dpi = 300,
#   limitsize = FALSE
# )


}, silent = TRUE)

##===============##
## Run the regression.
##===============##

tryCatch({
  
  Data_Visualisation_df$monthly_return_ann_pct <- Data_Visualisation_df$monthly_return * 100 * 12
  Data_Visualisation_df$managed_return_ann_pct <- Data_Visualisation_df$managed_return * 100 * 12
  
  reg_1 <- lm(managed_return_ann_pct ~ monthly_return_ann_pct, 
              data = Data_Visualisation_df, 
              na.action = na.omit)
  summary_table1 <- summary(reg_1)
  
  ## Report the Regresssion results:
  intercept_alpha <- coef(summary_table1)[1, 1]
  slope_beta <- coef(summary_table1)[2, 1]
  r_squared <- summary_table1$r.squared
  rmse <- summary_table1$sigma
  n_obs <- nobs(reg_1)
  
  cat(sprintf("Intercept (Alpha): %.4f\n", intercept_alpha))
  cat(sprintf("Slope (Beta):      %.4f\n", slope_beta))
  cat(sprintf("R-squared:         %.4f\n", r_squared))
  cat(sprintf("RMSE:              %.4f\n", rmse))
  cat(sprintf("Observations:      %d\n", n_obs))
  
})

##===============##
## Plot the regression.
##===============##

tryCatch({
  
  regression_plot <- ggplot(Data_Visualisation_df, 
                            aes(x = monthly_return_ann_pct, y = managed_return_ann_pct)) +
    geom_point(alpha = 0.5, color = "gray50") +
    geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1.2) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Volatility-Managed Portfolio vs. Original Market Return",
      subtitle = "Regression: Managed Return = Alpha + Beta * Market Return",
      x = "Original Monthly Market Return (Annualized %)",
      y = "Managed Portfolio Return (Annualized %)"
    ) +
    theme_nice()  
  
  Path <- file.path(PaperReplication_Charts_Path, "06_Regression_Plot.png")
  ggsave(
    filename = Path,
    plot = regression_plot,
    width = width,
    height = height,
    units = "px",
    dpi = 300,
    limitsize = FALSE
  )
  
##===============##
## Regression Output with stargazer.
##===============##
  
Path <- file.path(PaperReplication_Charts_Path, "07_Regression_Summary.html")
  
stargazer(
  reg_1, 
    type = "html",
    out = Path,
    title = "Volatility-Managed Portfolio Regression",
    dep.var.labels = "Managed Portfolio Return (Ann. %)",
    covariate.labels = "Market Return (Ann. %)",
    header = FALSE, 
    align = TRUE, 
    report = "vctp*", 
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = "Residual Std. Error corresponds to the RMSE."
  )
  
  
  
}, silent = TRUE)


#==============================================================================#
#==== 03 - Extension of paper results =========================================#
#==============================================================================#

#==== 03a - Estimation of d-day variance ======================================#

Chart_Path <- PaperExtension_Charts_Path

##======================##
## Parameters.
##======================##

id <- 11815352
D <- as.numeric(substr(as.character(id), 7, 7)) + 
     as.numeric(substr(as.character(id), 8, 8)) + 10

Mkt_ret_test <- Mkt_ret[test_period]
risk_free_2025 <- rf_ret[test_period]
Size_ret_test <- Size_ret[test_period]
lag_NW <- 6

##======================##
## Functions.
##======================##

calculate_d_day_variance <- function(daily_returns_window) {
  
  D_local <- length(daily_returns_window)
  mean_return <- mean(daily_returns_window, na.rm = TRUE)
  sum_sq_dev <- sum((daily_returns_window - mean_return)^2, na.rm = TRUE)
  sigma_sq_hat <- (D_local / 22) * sum_sq_dev
  
  return(sigma_sq_hat)
}

#==== 03b - VM-Portfolio (Market) =============================================#

##======================##
## Compute the d-day variance.
##======================##

realized_variance_monthly <- rollapply(Mkt_ret_test, 
                                  width = D, 
                                  FUN = calculate_d_day_variance, 
                                  align = "right",
                                  fill = NA)

realized_variance_monthly <- na.omit(realized_variance_monthly)
eom_indices <- endpoints(realized_variance_monthly, on = "months")
realized_variance_monthly <- realized_variance_monthly[eom_indices]

colnames(realized_variance_monthly) <- "d_day_variance"
cat("\n--- D-Day Variance (Monthly) ---\n")
print(head(realized_variance_monthly))

##===============##
## Construct the VM-portfolio.
##===============##

tryCatch({
  
  ## Compute the monthly portfolio return.
  ret_incl_rf <- Mkt_ret_train + risk_free_2025
  return_incl_rf_monthly <- apply.monthly(ret_incl_rf, FUN = function(x) prod(1 + x) - 1)
  return_rf_monthly <- apply.monthly(risk_free_2025, FUN = function(x) prod(1 + x) - 1)
  return_monthly <- return_incl_rf_monthly - return_rf_monthly
  
  ## Set up the data for the VM-portfolio construction.
  realized_variance_monthly_lagged <- stats::lag(realized_variance_monthly, k = 1)
  combined_data <- merge(return_monthly, realized_variance_monthly_lagged, join = "inner")
  colnames(combined_data) <- c("monthly_return", "lagged_variance")
  
  managed_return_unscaled <- combined_data$monthly_return / combined_data$lagged_variance
  
  ## Compute the constant c.
  ## We choose c so that the managed portfolio has the same unconditional standard 
  ## deviation as the buy-and-hold portfolio.
  
  sd_original <- sd(combined_data$monthly_return[-1]) ## To have the same length.
  sd_unscaled <- sd(managed_return_unscaled, na.rm = TRUE)
  
  c <- sd_original / sd_unscaled
  
  ## Now scale the managed returns.
  managed_return_scaled <- c * managed_return_unscaled
  colnames(managed_return_scaled) <- "managed_return"
  managed_return_scaled_market <- managed_return_scaled
  
}, silent = TRUE)

#==== 03c - VM-Portfolio (Factor) =============================================#

##======================##
## Compute the d-day variance for the size factor.
##======================##

realized_variance_monthly <- rollapply(Size_ret_test, 
                                       width = D, 
                                       FUN = calculate_d_day_variance, 
                                       align = "right",
                                       fill = NA)

realized_variance_monthly <- na.omit(realized_variance_monthly)
eom_indices <- endpoints(realized_variance_monthly, on = "months")
realized_variance_monthly <- realized_variance_monthly[eom_indices]

colnames(realized_variance_monthly) <- "d_day_variance"
cat("\n--- D-Day Variance (Monthly) ---\n")
print(head(realized_variance_monthly))

##===============##
## Construct the VM-factor portfolio.
##===============##

tryCatch({
  
  ## Compute the monthly portfolio return.
  return_monthly <- apply.monthly(Size_ret_test, FUN = function(x) prod(1 + x) - 1)
  
  ## Set up the data for the VM-portfolio construction.
  realized_variance_monthly_lagged <- stats::lag(realized_variance_monthly, k = 1)
  combined_data <- merge(return_monthly, realized_variance_monthly_lagged, join = "inner")
  colnames(combined_data) <- c("monthly_return", "lagged_variance")
  
  managed_return_unscaled <- combined_data$monthly_return / combined_data$lagged_variance
  
  ## Compute the constant c.
  ## We choose c so that the managed portfolio has the same unconditional standard 
  ## deviation as the buy-and-hold portfolio.
  
  sd_original <- sd(combined_data$monthly_return, na.rm = TRUE)
  sd_unscaled <- sd(managed_return_unscaled, na.rm = TRUE)
  
  c <- sd_original / sd_unscaled
  
  ## Now scale the managed returns.
  managed_return_scaled <- c * managed_return_unscaled
  colnames(managed_return_scaled) <- "managed_return"
  managed_return_scaled_factor <- managed_return_scaled
  
}, silent = TRUE)

#==============================================================================#
#==== 04 - Comparison & Performance Analysis ==================================#
#==============================================================================#

##======================##
## Functions.
##======================##

ComputeDrawdowns <- function(R, geometric = TRUE) {
  
  if (!is.matrix(R) || !is.numeric(R)) {
    stop("Input 'R' must be a numeric matrix.")
  }
  
  drawdown_matrix <- apply(R, 2, function(x_col) {
    if (geometric) {
      Return.cumulative <- cumprod(1 + x_col)
    } else {
      Return.cumulative <- 1 + cumsum(x_col)
    }
    
    maxCumulativeReturn <- cummax(c(1, Return.cumulative))[-1]
    drawdown <- Return.cumulative / maxCumulativeReturn - 1
    return(drawdown)
  })
  return(drawdown_matrix)
}

findComputedDrawdowns <- function(drawdown){
  MaxDrawdown <- -abs(min(drawdown, na.rm = TRUE))
  return(MaxDrawdown)
}
#==== 04a - Data Preparation ==================================================#

return_monthly_market <- apply.monthly(Mkt_ret_test, FUN = function(x) prod(1 + x) - 1)
return_monthly_factor <- apply.monthly(Size_ret_test, FUN = function(x) prod(1 + x) - 1)

Data_Analysis <- cbind(return_monthly_market, 
                       return_monthly_factor,
                       managed_return_scaled_market,
                       managed_return_scaled_factor)
Data_Analysis <- na.omit(Data_Analysis) ## Need to remove the first row.

#==== 04b - Compute the performance measures ==================================#

##===============##
## Sharpe Ratio.
##===============##

## Time period / annualization.
Years_passed <- abs(as.numeric(head(index(Data_Analysis), n = 1) -
                                 tail(index(Data_Analysis), n = 1)) / 365.25)
Time_Factor <- nrow(Data_Analysis) / Years_passed

## Geometric_Return
GeoReturn <- apply(as.matrix(Data_Analysis) + 1, MARGIN = 2, FUN = prod)
GeoReturn_monthly <- GeoReturn^(1 / nrow(Data_Analysis)) - 1
GeoReturn_annualized <- GeoReturn^(Time_Factor/nrow(Data_Analysis))-1

## Volatility.
Variance <- apply(as.matrix(Data_Analysis), MARGIN = 2, FUN = var)
Volatility_monthly <- sqrt(Variance)
Volatility_annualized <- sqrt(Variance) * sqrt(Time_Factor)

## Geometric Return divided by the Volatility. (In this case Sharpe Ratio bc we work with excess returns).
SR <-  GeoReturn_monthly / (Volatility_monthly)
SR_annualized <-  GeoReturn_annualized / (Volatility_annualized)

##===============##
## Maximum Drawdown.
##===============##

drawdowns <- ComputeDrawdowns(as.matrix(Data_Analysis))
MaxDrawdown <- apply(as.matrix(drawdowns), MARGIN = 2, FUN = findComputedDrawdowns)

#==== 04c - Alpha relative to the market ======================================#

##===============##
## Market.
##===============##

reg_data <- Data_Analysis[,c(1,2)]
colnames(reg_data) <- c("Market", "VM")

reg_vm <- lm(VM ~ Market, 
             data = reg_data, 
             na.action = na.omit)

summary(reg_vm)

## NW-Adjustment.
nw_vcov <- NeweyWest(reg_vm, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
coefs <- coeftest(kyle_model_period_1, vcov. = nw_vcov)
tidy_results <- broom::tidy(coefs)

alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
model_stats <- broom::glance(kyle_model_period_1) %>%
  dplyr::rename(
    f.statistic = statistic, 
    f.p.value = p.value
  )

combined_summary <- tidyr::crossing(tidy_results, model_stats)
combined_summary <- data.frame(combined_summary)

##===============##
## Factor.
##===============##

reg_data <- Data_Analysis[,c(3,4)]
colnames(reg_data) <- c("Market", "VM")

reg_vm_factor <- lm(VM ~ Market, 
             data = reg_data, 
             na.action = na.omit)

summary(reg_vm_factor)

#==== 04d - Visualisation =====================================================#

##===============##
## Factor.
##===============##

Path <- file.path(PaperReplication_Charts_Path, "07_Regression_Summary.html")

stargazer(
  reg_1, 
  type = "html",
  out = Path,
  title = "Volatility-Managed Portfolio Regression",
  dep.var.labels = "Managed Portfolio Return (Ann. %)",
  covariate.labels = "Market Return (Ann. %)",
  header = FALSE, 
  align = TRUE, 
  report = "vctp*", 
  omit.stat = c("f", "adj.rsq", "ll"),
  notes = "Residual Std. Error corresponds to the RMSE."
)











#==============================================================================#