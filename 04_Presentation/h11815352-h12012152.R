########################################################
# Assignment ARM2
########################################################

# --- Load Required Packages (Install if necessary) ---
# This block checks if packages are installed. If not, it installs them.
packages_to_check <- c("lmtest", "sandwich", "here")

for (pkg in packages_to_check) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste("Package", pkg, "not found. Installing...\n"))
    install.packages(pkg)
  }
  # Load the package after ensuring it's installed
  library(pkg, character.only = TRUE)
}

# ------ load data ------
# SET YOUR CORRECT PATH HERE IN CASE PATH PICKS UP THE WRONG ONE
# setwd("C:\\Users\\filip\\OneDrive - WU Wien\\1_Uni Master\\Q1_ARM2\\Project")
setwd(file.path(here::here("")))

# # Fama data
# df <- read.csv("data_Fama.csv")
# # NBER data
# recession_data <- read.csv("data_NBER.csv")
# # momentum data
# mom_data <- read.csv("data_momentum.csv")[,c("date","ret")]
# 
# save(df, recession_data, mom_data, file = "h11815352-h12012152.RData")

load("h11815352-h12012152.RData")

# ------ prepare data ------ 

df$Mkt.RF <- df$Mkt.RF/100
df$RF <- df$RF/100

df$SMB <- df$SMB/100
df$HML <- df$HML/100

df$Date <- as.Date(as.character(df$Date), format = "%Y%m%d")
df$Year <- as.numeric(format(df$Date, "%Y"))
df$Month <- as.numeric(format(df$Date, "%m"))

recession_data$DATE <- as.Date(recession_data$observation_date, format = "%Y-%m-%d")

recession_data$Year <- as.numeric(format(recession_data$DATE, "%Y"))
recession_data$Month <- as.numeric(format(recession_data$DATE, "%m"))

colnames(recession_data)[which(names(recession_data) == "USREC")] <- "NBER_Recession"

recession_data <- recession_data[, c("Year", "Month", "NBER_Recession")]

colnames(mom_data) <- c("Date", "MOM")
mom_data$Date <- as.Date(as.character(mom_data$Date), format = "%Y-%m-%d")
mom_data$Year <- as.numeric(format(mom_data$Date, "%Y"))
mom_data$Month <- as.numeric(format(mom_data$Date, "%m"))

############################ 1 ############################

################## Step 1. Estimate monthly variance

results_list <- list()
for(yr in 1926:2015) {
  for(mth in 1:12) {
    month_data <- subset(df, Year == yr & Month == mth)
    
    if (nrow(month_data) > 0) {
      daily_mkt <- month_data$Mkt.RF + month_data$RF
      
      # Compound market and RF returns
      monthly_mkt_return <- prod(1 + daily_mkt) - 1
      monthly_rf_return <- prod(1 + month_data$RF) - 1
      
      # Calculate the final monthly excess return
      monthly_excess_return <- monthly_mkt_return - monthly_rf_return
      
      
      # Realized Variance (on daily excess returns)
      realized_var <- var(month_data$Mkt.RF)
      
      
      month_result <- data.frame(Year = yr, 
                                 Month = mth, 
                                 Return = monthly_excess_return, 
                                 Variance = realized_var)
      
      list_name <- paste(yr, mth, sep = "-")
      results_list[[list_name]] <- month_result
    }
  }
}

data_monthly <- do.call(rbind, results_list)


################## Step 2. Construct the volatility-managed portfolio

non_c <- data_monthly$Return[-1] / data_monthly$Variance[-nrow(data_monthly)]
sd_non_c <- sd(non_c)

sd_buyhold <- sd(data_monthly$Return[-1])

c <- sd_buyhold/sd_non_c

data_monthly$managed_returns <- c(NA, c * non_c)

################## Step 3. Sort months into variance quintiles

data_monthly$lagged_Variance <- c(NA, data_monthly$Variance[-nrow(data_monthly)])
data_analysis <- data_monthly[-1,c("Year", "Month", "Return", "managed_returns", "lagged_Variance")]

quintile_breaks <- quantile(data_analysis$lagged_Variance, 
                            probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                            na.rm = TRUE)

data_analysis$Vol_Quintile <- cut(data_analysis$lagged_Variance,
                                 breaks = quintile_breaks,
                                 labels = c("Low Vol", "2", "3", "4", "High Vol"),
                                 include.lowest = TRUE)

################## Step 4. Reproduce the figures and tables

# Merge NBER with your existing data_analysis frame
data_analysis <- merge(data_analysis, recession_data, by = c("Year", "Month"), all.x = TRUE)

data_analysis <- na.omit(data_analysis)

# 1. Average monthly return (annualized)
avg_ret_monthly <- aggregate(Return ~ Vol_Quintile, data = data_analysis, FUN = mean)
avg_ret_annualized <- avg_ret_monthly$Return * 12

# 2. Standard deviation of returns (annualized)
std_dev_monthly <- aggregate(Return ~ Vol_Quintile, data = data_analysis, FUN = sd)
std_dev_annualized <- std_dev_monthly$Return * sqrt(12)

# 3. Ratio E[R]/Var(R)
var_ret_monthly <- aggregate(Return ~ Vol_Quintile, data = data_analysis, FUN = var)
er_var_ratio <- avg_ret_monthly$Return / var_ret_monthly$Return

# 4. Probability of being in a recession
# Since NBER_Recession is 0 or 1, the mean is the probability
prob_recession <- aggregate(NBER_Recession ~ Vol_Quintile, data = data_analysis, FUN = mean)

# Get quintile names for plotting labels
quintile_labels <- prob_recession$Vol_Quintile


# Generate Figure 1 (Four Bar Charts) ---

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) # Set margins

# Plot 1: Average Return
barplot(avg_ret_annualized * 100, # Convert to percentage
        names.arg = quintile_labels,
        main = "Average Return",
        ylab = "Avg. Return (%)",
        col = "navy",
        las = 2,         # <-- Makes x-axis labels vertical
        cex.names = 0.8  # <-- Reduces x-axis label font size
)

# Plot 2: Standard Deviation
barplot(std_dev_annualized * 100, # Convert to percentage
        names.arg = quintile_labels,
        main = "Standard Deviation",
        ylab = "Std. Dev. (%)",
        col = "darkred",
        las = 2,         # <-- Makes x-axis labels vertical
        cex.names = 0.8  # <-- Reduces x-axis label font size
)

# Plot 3: E[R]/Var(R)
barplot(er_var_ratio,
        names.arg = quintile_labels,
        main = "E[R] / Var(R)",
        ylab = "Ratio",
        col = "darkgreen",
        las = 2,         # <-- Makes x-axis labels vertical
        cex.names = 0.8  # <-- Reduces x-axis label font size
)

# Plot 4: Probability of Recession
barplot(prob_recession$NBER_Recession,
        names.arg = quintile_labels,
        main = "Probability of Recession",
        ylab = "Probability",
        col = "darkgoldenrod",
        las = 2,         # <-- Makes x-axis labels vertical
        cex.names = 0.8  # <-- Reduces x-axis label font size
)

# Reset plotting window to default
par(mfrow = c(1, 1))


# Run Regression

cat("\n--- Table I (Panel A, Col 1) Regression Results ---\n")

# Scale variables as in the paper
data_analysis$managed_returns_ann_pct <- data_analysis$managed_returns * 1200
data_analysis$Return_ann_pct <- data_analysis$Return * 1200

# Run the regression
model <- lm(managed_returns_ann_pct ~ Return_ann_pct, data = data_analysis)
model_summary <- summary(model)

# Get the coefficient test results using the NW covariance matrix
nw_coeftest <- coeftest(model, vcov = vcovHAC(model))

# Print the coefficient table
cat("\n--- Newey-West Corrected Coefficient Table ---\n")
print(nw_coeftest)

# Create and print summary statistics
cat(paste("\n--- Summary Statistics ---", "\n"))
cat(paste("R-squared:", round(model_summary$r.squared, 4), "\n"))
cat(paste("Observations:", nobs(model), "\n"))
cat(paste("RMSE:", round(model_summary$sigma, 4), "\n"))


############################ 2 ############################

D <- 10 + 5 + 2 # for both of us (D = 17) (12012152 and 11815352)
df <- df[df$Date <= as.Date("2025-07-31"),]

# ---- merge mom data ----
df <- merge(df, mom_data, by = c("Date", "Year", "Month"), all = FALSE)

################## Step 1. Estimate monthly variance

results_list_mkt <- list()
results_list_mom <- list()
# Loop through all months
for(yr in 1926:2025) {
  for(mth in 1:12) {
    
    # Isolate data for the current month
    month_data <- subset(df, Year == yr & Month == mth)
    
    if (nrow(month_data) > 0) {
      
      # --- 1. Calculate Monthly Return (f_t+1) ---
      
      ### mkt
      daily_mkt <- month_data$Mkt.RF + month_data$RF
      monthly_mkt_return <- prod(1 + daily_mkt) - 1
      monthly_rf_return <- prod(1 + month_data$RF) - 1
      monthly_excess_return_mkt <- monthly_mkt_return - monthly_rf_return
      
      ### mom
      # mom is already an excess return
      monthly_excess_return_mom <- prod(1 + month_data$MOM) - 1
      
      
      # --- 2. Estimate D-day Variance (sigma_t^2) ---
      # Find the row index for the last day of this month in the *full* df
      end_of_month_index <- max(which(df$Year == yr & df$Month == mth))
      
      # Check if we have enough historical data (D days) to calculate variance
      if (end_of_month_index >= D) {
        
        # Select the last D daily returns *ending* on the last day of the month
        ret_mkt <- df$Mkt.RF[(end_of_month_index - D + 1):end_of_month_index]
        ret_mom <- df$MOM[(end_of_month_index - D + 1):end_of_month_index]
        
        # Calculate variance using the correct formula
        realized_var_mkt <- (D / 22) * sum((ret_mkt - mean(ret_mkt))^2)
        realized_var_mom <- (D / 22) * sum((ret_mom - mean(ret_mom))^2)
        
      } else {
        # Not enough data (e.g., first few months of 1926)
        realized_var_mkt <- NA
        realized_var_mom <- NA
      }
      
      
      # --- 3. Store Results ---
      
      month_result_mkt <- data.frame(Year = yr, 
                                     Month = mth, 
                                     Return = monthly_excess_return_mkt, 
                                     Variance = realized_var_mkt)
      month_result_mom <- data.frame(Year = yr, 
                                     Month = mth, 
                                     Return = monthly_excess_return_mom, 
                                     Variance = realized_var_mom)
      
      list_name <- paste(yr, mth, sep = "-")
      results_list_mkt[[list_name]] <- month_result_mkt
      results_list_mom[[list_name]] <- month_result_mom
    }
  }
}

# Combine all monthly results into a single data frame
data_monthly_mkt <- do.call(rbind, results_list_mkt)
data_monthly_mom <- do.call(rbind, results_list_mom)

################## Step 2. Construct the volatility-managed portfolio

# same as in part 1
vola_returns <- function(data) {
  non_c <- data$Return[-1] / data$Variance[-nrow(data)]
  sd_non_c <- sd(non_c)
  
  sd_buyhold <- sd(data$Return[-1])
  
  c <- sd_buyhold/sd_non_c
  
  data$managed_returns <- c(NA, c * non_c)
  return(data)
}

data_monthly_mkt <- vola_returns(data_monthly_mkt)
data_monthly_mom <- vola_returns(data_monthly_mom)


################## Step 3. Compare results

# Function to calculate Sharpe Ratios
calculate_sharpe <- function(returns) {
  monthly_sharpe <- (prod(1+returns)^(1/length(returns))-1) / sd(returns)
  annualized_sharpe <- (prod(1+returns)^(12/length(returns))-1) / (sd(returns)*sqrt(12))
  return(c(monthly = monthly_sharpe, annualized = annualized_sharpe))
}

# Function to calculate Maximum Drawdown
calculate_max_drawdown <- function(returns) {
  # Calculate cumulative wealth index
  wealth_index <- cumprod(1 + returns)
  # Calculate previous peaks
  previous_peaks <- cummax(c(1, wealth_index))[-1] # Start with 1, remove it
  # Calculate drawdowns
  drawdowns <- (wealth_index - previous_peaks) / previous_peaks
  # Return the most negative value
  return(min(drawdowns))
}

# Function to calculate Alpha (with Newey-West t-stats and p-values)
# 'y_returns' = portfolio to test
# 'x_benchmark' = benchmark (e.g., market or original factor)
calculate_regression_alpha <- function(y_returns, x_benchmark) {
  # Run the simple OLS regression
  model <- lm(y_returns ~ x_benchmark)
  
  # Get Newey-West (HAC) robust standard errors
  nw_summary <- coeftest(model, vcov = vcovHAC(model))
  
  # Extract monthly alpha (intercept) and its stats
  alpha_monthly <- nw_summary["(Intercept)", "Estimate"]
  alpha_t_stat <- nw_summary["(Intercept)", "t value"]
  alpha_p_value <- nw_summary["(Intercept)", "Pr(>|t|)"] 
  
  # Annualize the alpha
  alpha_annualized_pct <- alpha_monthly * 12 * 100
  
  return(list(alpha_monthly = alpha_monthly, 
              alpha_annualized_pct = alpha_annualized_pct, 
              t_stat = alpha_t_stat,
              p_value = alpha_p_value))
}


# --- 2. Prepare Data ---

# Remove NA rows to align all time series (from lagging variance/returns)
data_mkt_clean <- na.omit(data_monthly_mkt)
data_mom_clean <- na.omit(data_monthly_mom)

# Define the benchmark return streams
mkt_benchmark <- data_mkt_clean$Return # Original Market Excess Return
mom_benchmark <- data_mom_clean$Return # Original MOM Excess Return


# --- 3. Calculate Metrics for All 4 Portfolios ---

results_summary <- list()

# --- A. Original Market ---
ret_orig_mkt <- data_mkt_clean$Return
sharpe_orig_mkt <- calculate_sharpe(ret_orig_mkt)
mdd_orig_mkt <- calculate_max_drawdown(ret_orig_mkt)
# Alpha vs. Market (itself) is 0 by definition
capm_alpha_orig_mkt <- list(alpha_annualized_pct = 0, t_stat = NA, p_value = NA)
# Alpha vs. Original (itself) is 0 by definition
rel_alpha_orig_mkt <- list(alpha_annualized_pct = 0, t_stat = NA, p_value = NA)

results_summary[["Original Market"]] <- c(
  Sharpe_Monthly = sharpe_orig_mkt["monthly"],
  Sharpe_Annualized = sharpe_orig_mkt["annualized"],
  Max_Drawdown_Pct = mdd_orig_mkt * 100,
  CAPM_Alpha_Ann_Pct = capm_alpha_orig_mkt$alpha_annualized_pct,
  CAPM_Alpha_t_Stat = capm_alpha_orig_mkt$t_stat,
  CAPM_Alpha_p_value = capm_alpha_orig_mkt$p_value, 
  Relative_Alpha_Ann_Pct = rel_alpha_orig_mkt$alpha_annualized_pct,
  Relative_Alpha_t_Stat = rel_alpha_orig_mkt$t_stat,
  Relative_Alpha_p_value = rel_alpha_orig_mkt$p_value 
)

# --- B. Managed Market ---
ret_mgmt_mkt <- data_mkt_clean$managed_returns
sharpe_mgmt_mkt <- calculate_sharpe(ret_mgmt_mkt)
mdd_mgmt_mkt <- calculate_max_drawdown(ret_mgmt_mkt)
# Alpha vs. Market (mkt_benchmark)
capm_alpha_mgmt_mkt <- calculate_regression_alpha(ret_mgmt_mkt, mkt_benchmark)
# Alpha vs. Original (also mkt_benchmark)
rel_alpha_mgmt_mkt <- capm_alpha_mgmt_mkt

results_summary[["Managed Market"]] <- c(
  Sharpe_Monthly = sharpe_mgmt_mkt["monthly"],
  Sharpe_Annualized = sharpe_mgmt_mkt["annualized"],
  Max_Drawdown_Pct = mdd_mgmt_mkt * 100,
  CAPM_Alpha_Ann_Pct = capm_alpha_mgmt_mkt$alpha_annualized_pct,
  CAPM_Alpha_t_Stat = capm_alpha_mgmt_mkt$t_stat,
  CAPM_Alpha_p_value = capm_alpha_mgmt_mkt$p_value, 
  Relative_Alpha_Ann_Pct = rel_alpha_mgmt_mkt$alpha_annualized_pct,
  Relative_Alpha_t_Stat = rel_alpha_mgmt_mkt$t_stat,
  Relative_Alpha_p_value = rel_alpha_mgmt_mkt$p_value 
)

# --- C. Original MOM ---
ret_orig_mom <- data_mom_clean$Return
sharpe_orig_mom <- calculate_sharpe(ret_orig_mom)
mdd_orig_mom <- calculate_max_drawdown(ret_orig_mom)
# Alpha vs. Market (mkt_benchmark)
capm_alpha_orig_mom <- calculate_regression_alpha(ret_orig_mom, mkt_benchmark)
# Alpha vs. Original (itself) is 0 by definition
rel_alpha_orig_mom <- list(alpha_annualized_pct = 0, t_stat = NA, p_value = NA)

results_summary[["Original MOM"]] <- c(
  Sharpe_Monthly = sharpe_orig_mom["monthly"],
  Sharpe_Annualized = sharpe_orig_mom["annualized"],
  Max_Drawdown_Pct = mdd_orig_mom * 100,
  CAPM_Alpha_Ann_Pct = capm_alpha_orig_mom$alpha_annualized_pct,
  CAPM_Alpha_t_Stat = capm_alpha_orig_mom$t_stat,
  CAPM_Alpha_p_value = capm_alpha_orig_mom$p_value, 
  Relative_Alpha_Ann_Pct = rel_alpha_orig_mom$alpha_annualized_pct,
  Relative_Alpha_t_Stat = rel_alpha_orig_mom$t_stat,
  Relative_Alpha_p_value = rel_alpha_orig_mom$p_value 
)

# --- D. Managed MOM ---
ret_mgmt_mom <- data_mom_clean$managed_returns
sharpe_mgmt_mom <- calculate_sharpe(ret_mgmt_mom)
mdd_mgmt_mom <- calculate_max_drawdown(ret_mgmt_mom)
# Alpha vs. Market (mkt_benchmark)
capm_alpha_mgmt_mom <- calculate_regression_alpha(ret_mgmt_mom, mkt_benchmark)
# Alpha vs. Original (mom_benchmark)
rel_alpha_mgmt_mom <- calculate_regression_alpha(ret_mgmt_mom, mom_benchmark)

results_summary[["Managed MOM"]] <- c(
  Sharpe_Monthly = sharpe_mgmt_mom["monthly"],
  Sharpe_Annualized = sharpe_mgmt_mom["annualized"],
  Max_Drawdown_Pct = mdd_mgmt_mom * 100,
  CAPM_Alpha_Ann_Pct = capm_alpha_mgmt_mom$alpha_annualized_pct,
  CAPM_Alpha_t_Stat = capm_alpha_mgmt_mom$t_stat,
  CAPM_Alpha_p_value = capm_alpha_mgmt_mom$p_value, 
  Relative_Alpha_Ann_Pct = rel_alpha_mgmt_mom$alpha_annualized_pct,
  Relative_Alpha_t_Stat = rel_alpha_mgmt_mom$t_stat,
  Relative_Alpha_p_value = rel_alpha_mgmt_mom$p_value 
)


# --- 4. Print the Final Results Table ---

# Combine list into a data frame
final_results_table <- do.call(rbind, results_summary)

# Print the rounded table
cat("\n--- Performance Summary Table ---\n")
print(round(final_results_table, 4))