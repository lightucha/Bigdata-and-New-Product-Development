#####################################################################################
### Project : Armacord case study
### Script : case1_armacord.R
### Description : Armacord case study
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("httr", "readxl", "dplyr", "forecast", "reshape2",
          "tseries", "ggplot2", "lubridate", "zoo", "tidyr")
sapply(pkgs, require, character.only = T)

# Load data
url <- "http://idisk.unist.ac.kr:8081/api.link/3d_baLIMHabCQeYL.xlsx"
GET(url, write_disk("case_1.xlsx", overwrite = T))
wbm <- read_excel("case_1.xlsx", sheet = 2)
nvw <- read_excel("case_1.xlsx", sheet = 3)


#####################################################################################
### Data preparation: Wires By Month (wbm)
#####################################################################################

# Describe data
str(wbm)
# Notice that the year columns are character, there is an additional empty column, and
# the Month column has been imported as numeric

# Remove unnecessary rows and columns
wbm <- wbm[1:12, -c(1, ncol(wbm))]

# Convert each date column to numeric
wbm <- wbm %>% mutate_all(funs(as.numeric(ifelse(grepl("^-*[[:digit:]]", .), ., NA))))


#####################################################################################
### Outlier detection using linear fit
#####################################################################################

# Convert data into time series
wbm_long   <- melt(wbm)                         # warning can be ignored
wbm_long$t <- 1:nrow(wbm_long)

# To find outliers, fit a linear model and calculate standardized residuals
fit_lm <- lm(value ~ t, data = wbm_long, na.action = na.exclude)
sresid <- rstandard(fit_lm)

# Plot
plot(wbm_long$t, wbm_long$value, xlab = "Time", ylab = "Wire Transfer", pch = 16, col = "blue")
abline(fit_lm, col = "red")

# Identify any residuals with absolute value greater than 2.5
# There is clearly a large positive outlier on April 2012,
# After double checking, we determined that this was a data entry error and
# should be replaced with NA
id.out                 <- which(!is.na(sresid) & abs(sresid) > 2.5)
wbm_long$value[id.out] <- NA

# 2nd round
fit_lm <- lm(value ~ t, data = wbm_long, na.action = na.exclude)
sresid <- rstandard(fit_lm)

# Plot
plot(wbm_long$t, wbm_long$value, xlab = "Time", ylab = "Wire Transfer", pch = 16, col = "blue")
abline(fit_lm, col = "red")

# There is a large negative outlier on July 2008
# After double checking, we determined that this was a data entry error and
# should be replaced with NA
id.out                 <- which(!is.na(sresid) & abs(sresid) > 2.5)
wbm_long$value[id.out] <- NA

# Automate the procedure
wbm_long   <- melt(wbm)                      # reset the data
wbm_long$t <- 1:nrow(wbm_long)
id.out     <- NULL
repeat{
  fit_lm                 <- lm(value ~ t, data = wbm_long, na.action = na.exclude)
  sresid                 <- rstandard(fit_lm)
  id.out.new             <- which(!is.na(sresid) & abs(sresid) > 2.5)
  if(sum(id.out.new) == 0){break}
  wbm_long$value[id.out.new] <- NA
  id.out                     <- union(id.out, id.out.new)
}

# plot the cleaned data
plot(wbm_long$value, type = "o", main = "Wire by Month", 
     xlab = "Time", ylab = "Wire Transfer")


#####################################################################################
### Imputation using time series interpolation
#####################################################################################

# Convert the data into a time series with a frequency of 12 months
wbm_ts <- ts(wbm_long$value, start = c(2007, 1), frequency = 12)

# Uses periodic stl decomposition for interpolation to impute missing data
wbm_ts_imp <- na.interp(wbm_ts)

# Plot time series with and without imputed values
plot(wbm_ts_imp, col = "red", lty = 2, type = "o", main = "Wire by Month", ylab = "Wire Transfer")
lines(wbm_ts, type = "o")


#####################################################################################
### Find possible cases of money laundering
#####################################################################################

# Observing the seasonal plot, there is clearly seasonality in the data,
# with a major peak in August. 
ggseasonplot(wbm_ts_imp)

# ---------- Approach 1 --------------# 
# Decompose time series into seasonal, trend, and remainder components
fit_stl    <- stl(wbm_ts_imp, s.window = "periodic")
remainders <- fit_stl$time.series[, 3]
#hist(remainders)

# Look for remainder less than 2 times the 10% quantile or 
# greater than 2 times the 90% quantile of remainders 
qnt.a1    <- quantile(remainders, probs = c(.1, .90))
id.out.a1 <- which(remainders < 2 * qnt.a1[1] | remainders > 2 * qnt.a1[2])

# ---------- Approach 2 --------------# 
# Use the tsoutliers from the forecast package 
# to identify outliers and estimate their replacements
tsoutliers(wbm_ts_imp)
id.out.a2 <- tsoutliers(wbm_ts_imp)$index

# ---------- Approach 3 --------------# 
# Fit an ARIMA model to the data (following these steps: https://www.otexts.org/fpp/8/7)
fit_autoarima <- auto.arima(wbm_ts_imp)

# Look at the residuals and try identify outliers like before:
residuals <- residuals(fit_autoarima)
#hist(residuals)

# Look for remainder less 2 times the 10% quantile or 
# greater than 2 times the 90% quantile of remainders 
qnt.a3    <- quantile(residuals, probs = c(.1, .90))
id.out.a3 <- which(residuals < 2 * qnt.a3[1] | residuals > 2 * qnt.a3[2])

# Crosscheck outliers detected by different approaches
plot(wbm_ts_imp, col = "red", lty = 2, type = "o", main = "Wire by Month", 
     ylab = "Wire Transfer", ylim = c(320, 510))
lines(wbm_ts, type = "o")
points(as.zoo(wbm_ts_imp)[id.out.a1], pch = 0, cex = 2, col = "red")       # A1
points(as.zoo(wbm_ts_imp)[id.out.a2], pch = 1, cex = 2, col = "limegreen") # A2
points(as.zoo(wbm_ts_imp)[id.out.a3], pch = 2, cex = 2, col = "blue")      # A3


#####################################################################################
### Data preparation: November 2010 Wires (nvw)
#####################################################################################

# Describe data
str(nvw)
# Notice that the year columns are character, there is an additional empty column, and
# the Month column has been imported as numeric

# Change the column names to lower case and remove the spaces
names(nvw) <- gsub(" ", "_", tolower(names(nvw)))

# Remove excess blank rows appended to bottom of dataframe
nvw <- nvw[rowSums(is.na(nvw)) != ncol(nvw),]

# The time_of_transaction column has been converted to date time, let convert back to only time
nvw$time_of_transaction <- format(nvw$time_of_transaction, "%H:%M")

# Create a new column with both the date and time
nvw$dt_of_transaction <- as.POSIXct(paste(nvw$date_of_transaction, 
                                          nvw$time_of_transaction), 
                                    format = "%Y-%m-%d %H:%M")

# Arrange data by date and time of transaction
nvw <- nvw %>% arrange(dt_of_transaction)

# Convert transaction_amount to numeric
# Find non numeric rows / non positive rows and replace them with NA
id.nnr                           <- which(!grepl("^-*[[:digit:]]", nvw$transaction_amount))
id.npr                           <- which(nvw$transaction_amount < 0)
id.clear                         <- union(id.nnr, id.npr)
nvw$transaction_amount[id.clear] <- NA
nvw$transaction_amount           <- as.numeric(nvw$transaction_amount)

# Remove duplicate rows
id.dup <- which(duplicated(nvw))
nvw    <- nvw[-id.dup, ]

# Determine the transaction type from the transaction id
nvw$transfer_type                               <- NA
nvw$transfer_type[grepl("INCWT", nvw$trans_id)] <- "incoming"
nvw$transfer_type[grepl("OUTWT", nvw$trans_id)] <- "outgoing"
nvw$transfer_type[grepl("\\$C",  nvw$trans_id)] <- "cancelled"


#####################################################################################
### Find suspicious points and replace with NA
#####################################################################################

# Find outliers
outliers   <- boxplot.stats(nvw$transaction_amount, coef = 20)$out
id.out.box <- which((nvw$transaction_amount %in% outliers) == T)

# After going through the records, determine these are data entry errors,
# therefore we make them NA
nvw$transaction_amount[id.out.box] <- NA

# Incoming = $396,553.6 (this may vary with you definition of outliers)
ic.sum <- sum(nvw$transaction_amount[nvw$transfer_type == "incoming"], na.rm = T)

# Outgoing = $431,658.1 (this may vary with you definition of outliers)
og.sum <- sum(nvw$transaction_amount[nvw$transfer_type == "outgoing"], na.rm = T)


#####################################################################################
### Visualize daily and hourly transaction trend
#####################################################################################

# Generate daily transaction data
nvw_daily <- nvw %>% group_by(date_of_transaction, transfer_type) %>% 
             dplyr::summarise(amount = sum(transaction_amount, na.rm = T)) %>% 
             arrange(date_of_transaction) %>% ungroup()

# Plot daily transactions (I am old-fashioned)
tf.d.cc <- nvw_daily$transfer_type == "cancelled"
tf.d.ic <- nvw_daily$transfer_type == "incoming"
tf.d.og <- nvw_daily$transfer_type == "outgoing"
plot (nvw_daily$date_of_transaction[tf.d.ic], nvw_daily$amount[tf.d.ic], main = "Daily Transactions",
      ylim = c(600, 20000), xlab = "Time", ylab = "Amount", type = "o", col = "blue")
lines(nvw_daily$date_of_transaction[tf.d.og], nvw_daily$amount[tf.d.og], type = "o", col = "red")
lines(nvw_daily$date_of_transaction[tf.d.cc], nvw_daily$amount[tf.d.cc], type = "o", col = "limegreen")

# Add hour (binned) column
nvw$date_hour <- floor_date(nvw$dt_of_transaction, "hour")

# Generate hourly transaction data
nvw_hourly <- nvw %>% group_by(date_hour, transfer_type) %>% 
              dplyr::summarise(amount = sum(transaction_amount, na.rm = T)) %>% 
              arrange(date_hour) %>% ungroup()

# Plot hourly transactions (I am old-fashioned)
tf.h.cc <- nvw_hourly$transfer_type == "cancelled"
tf.h.ic <- nvw_hourly$transfer_type == "incoming"
tf.h.og <- nvw_hourly$transfer_type == "outgoing"
plot (nvw_hourly$date_hour[tf.h.ic], nvw_hourly$amount[tf.h.ic], main = "Hourly Transactions",
      ylim = c(0, 17000), xlab = "Time", ylab = "Amount", type = "l", col = "blue")
lines(nvw_hourly$date_hour[tf.h.og], nvw_hourly$amount[tf.h.og], type = "l", col = "red")
lines(nvw_hourly$date_hour[tf.h.cc], nvw_hourly$amount[tf.h.cc], type = "l", col = "limegreen")


#####################################################################################
### Find possible cases of money laundering
#####################################################################################

nvw_hourly_spread <- nvw_hourly %>% filter(transfer_type != "cancelled") %>% 
                     group_by(date_hour) %>% spread(transfer_type, amount, fill = 0) %>%
                     dplyr::mutate(total = incoming + outgoing) %>% ungroup() %>%
                     arrange(date_hour) %>% dplyr::select(-date_hour)

# 7 hours per day, 7 days per week
nvw_hourly_ts <- msts(nvw_hourly_spread, seasonal.periods = c(7, 7))
nvw_test      <- nvw_hourly_ts[, "incoming"] # "total" or "incoming" or "outgoing"

# ---------- Approach 1 --------------# 
# Decompose time series into seasonal, trend, and remainder components
fit_stl    <- stl(nvw_test, s.window = 7)
remainders <- fit_stl$time.series[, 3]
#hist(remainders)

# Look for remainder less than 2 times the 10% quantile or 
# greater than 2 times the 90% quantile of remainders 
qnt.a1     <- quantile(remainders, probs = c(.1, .9))
id2.out.a1 <- which(remainders < 2 * qnt.a1[1] | remainders > 2 * qnt.a1[2])

# ---------- Approach 2 --------------# 
# Fit an TBATS model to the data (TBATS deals with multiple seasonality (hourly and weekly))
fit_tbats <- tbats(nvw_test)

# Look at the residuals and try identify outliers like before:
residuals <- residuals(fit_tbats)
#hist(residuals)

# Look for remainder less 1 times the 10% quantile or 
# greater than 1 times the 90% quantile of remainders 
qnt.a2     <- quantile(residuals, probs = c(.1, .9))
id2.out.a2 <- which(residuals < 1 * qnt.a2[1] | residuals > 1 * qnt.a2[2])

# ---------- Approach 3 --------------# 
# Fit ETS model to the data
fit_ets <- ets(nvw_test)

# Look at the residuals and try identify outliers like before:
residuals <- residuals(fit_ets)
#hist(residuals)

# Look for remainder less 2 times the 10% quantile or 
# greater than 2 times the 90% quantile of remainders 
qnt.a3     <- quantile(residuals, probs = c(.1, .9))
id2.out.a3 <- which(residuals < 2 * qnt.a3[1] | residuals > 2 * qnt.a3[2])

# Crosscheck outliers detected by different approaches
plot(nvw_test, col = "black", type = "o", main = "Wire by Month", ylab = "Wire Transfer")
points(as.zoo(nvw_test)[id2.out.a1], pch = 0, cex = 2, col = "red")       # A1
points(as.zoo(nvw_test)[id2.out.a2], pch = 1, cex = 2, col = "limegreen") # A2
points(as.zoo(nvw_test)[id2.out.a3], pch = 2, cex = 2, col = "blue")      # A3

# Possible money laundary using incoming stats
nvw_hourly[nvw_hourly$transfer_type == "incoming",][id2.out.a3,]

