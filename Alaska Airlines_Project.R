# ******** USE REQUIRED LIBRARIES ********
library(forecast)
library(zoo)

# *********** DATA PREPARATION ***********

# SET WORKING DIRECTORY FOR LOCATING FILES
setwd("C:/Users/STSC/Documents/SEM 2/BAN 673/5. DATASETS")

# CREATE DATAFRAME
Alaska.data <- read.csv("AlaskaRevenue.csv")

# SEE THE FIRST 6 RECORDS OF THE FILE
head(Alaska.data)

# SEE THE LAST 6 RECORDS OF THE FILE
tail(Alaska.data)


# ********* TIME SERIES DATASET ***********

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# allrevenue.ts IS FOR PERIOD INCLUDING PRE-COVID (2009-2019), COVID (2020-2021 )AND POST-COVID PERIODS (2022-2023)

allrevenue.ts <- ts(Alaska.data$Revenue, start = c(2009,1), end = c(2023,4), freq = 4)
allrevenue.ts

# revenue.ts IS FOR PERIOD EXCLUDING COVID AND POST-COVID PERIOD
revenue.ts <- ts(Alaska.data$Revenue, start = c(2009,1), end = c(2019,4), freq = 4)
revenue.ts

# ****** PLOT OF TIME SERIES DATASET ******

# DATA PLOT OF HISTORICAL DATA FROM 2009 TO 2023 USING plot() FUNCTION
plot(allrevenue.ts, 
     xlab = "Time", ylab = "Revenue (in Millions of Dollars)", ylim = c(400, 3000), bty = "l",
     xaxt = "n", xlim = c(2009, 2025.25), main = "Alaska Airlines Revenue Data (2009-2023)", lwd = 2, col="brown") 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))

# PLOT OF TIME SERIES COMPONENTS FOR THE HISTORICAL DATA FROM 2009 TO 2023
allrevenue.stl <- stl(allrevenue.ts, s.window = "periodic")
autoplot(allrevenue.stl, main = "Alaska Airlines Revenue - Time Series Components (2009-2023)") 

# ** AUTOCORRELATION FOR PRE-COVID PERIOD **

# PLOT OF AUTOCORRELATION FOR DIFFERENT LAGS (PRE-COVID : 2009 - 2019)
autocor <- Acf(revenue.ts, lag.max = 4, main = "Autocorrelation Chart for Alaska Airlines Revenue Data (Pre-Covid : 2009 - 2019)")

# AUTOCORRELATION COEFFICIENTS FOR VARIOUS LAGS
lag <- round(autocor$lag,0)
ACF <- round(autocor$acf,3)
data.frame(lag,ACF)

# ******** TEST FOR PREDICATBILITY ********

#------------- APPROACH 1 : HYPOSTHESIS TESTING USING AR(1) MODEL -------------#

# USE Arima() FUNCTION TO FIT AR(1) MODEL FOR ALASKA REVENUE
# THE ARIMA MODEL OF order = c(1,0,0) GIVES AN AR(1) MODEL
revenue.ar1<- Arima(revenue.ts, order = c(1,0,0))
summary(revenue.ar1)

# APPLY Z-TEST TO TEST THE NULL HYPOTHESIS THAT BETA COEFFICIENT OF AR(1) = 1
ar1 <- 0.9580
s.e. <- 0.0417
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

#--------- APPROACH 2: EXAMINATION OF ACF FOR FIRST DIFFERENCED SERIES --------#

# CREATE FIRST DIFFERENCED ALASKA REVENUE DATA USING lag1
diff.revenue.ts <- diff(revenue.ts, lag = 1)
diff.revenue.ts

# AUTOCORRELATION FOR FIRST DIFFERENCED ALASKA REVENUE
Acf(diff.revenue.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Alaska Revenue Data")

# ************ DATA PARTITION *************

# TOTAL NO. OF PERIOD (PRE-COVID PERIOD) LENGTH(revenue.ts) = 44 (11 YEARS)
# nvalid = 12 QUARTERS (3 YEARS), FROM Q1-2017 TO Q4-2019
# nTrain = 32 QUARTERS (8 YEARS), FROM Q1-2009 TO Q4-2016

nValid <- 12
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2009, 1), end = c(2009, nTrain))
train.ts
valid.ts <- window(revenue.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid))
valid.ts

# CREATION OF TIME SERIES DATASET USING THE FUNCTION ts()
# future.ts IS FOR COVID (2020-2021 ) AND POST-COVID PERIODS (2022-2023)

future.ts <- ts(Alaska.data$Revenue, start = c(2020,1), end = c(2023,4), freq = 4)
future.ts

# ******** PLOT OF DATA PARTITION *********

# PLOT OF TIME SERIES DATA FOR "TRAINING" DATASET
plot(train.ts,
     xlab = "Time", ylab = "Revenue (in Million $)", 
     xlim = c(2009, 2024.25), ylim = c(400,3000),
     bty = "l",  xaxt = "n", lwd ="2",
     main = "TIME SERIES PLOT FOR PARTITION DATASET")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))

# ADDING THE TIME SERIES PLOT FOR "VALIDATION" DATASET (BLUE)
lines(valid.ts, col = "blue", lwd = "2")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# ********* DEVELOPMENT OF MODELS **********

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# NAIVE FORECAST FOR VALIDATION DATA 
revenue.naive.pred <- naive(train.ts, h = nValid)
revenue.naive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(revenue.naive.pred$mean, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# NAIVE FORECAST FOR FUTURE PERIOD
revenuef.naive.pred <- naive(revenue.ts, h = 16)
revenuef.naive.pred$mean

# PLOT THE PREDICTIONS FOR NAIVE FORECAST
plot(revenuef.naive.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 1 : NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(revenuef.naive.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts, col = "black", lwd = 2)
legend(2008,2800, legend = c("Revenue (2009-2023)", 
                             "Naive Forecast for Pre-Covid Period (2009-2019)",
                             "Naive Forecast for Future (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#


#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# SEASONAL NAIVE FORECAST FOR VALIDATION DATA 
revenue.snaive.pred <- snaive(train.ts, h = nValid)
revenue.snaive.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(revenue.snaive.pred$mean, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# SEASONAL NAIVE FORECAST FOR FUTURE PERIOD 
revenuef.snaive.pred <- snaive(revenue.ts, h = 16)
revenuef.snaive.pred$mean

# PLOT THE PREDICTIONS FOR SEASONAL NAIVE FORECAST
plot(revenuef.snaive.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 2 : SEASONAL NAIVE FORECAST", col = "green", lwd =2) 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(revenuef.snaive.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts, col = "black", lwd = 2)
legend(2008,2800, legend = c("Revenue (2009-2023)", 
                             "Seasonal Naive Forecast for Pre-Covid Period (2009-2019)",
                             "Seasonal Naive Forecast for Future (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR ENTIRE DATASET
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)

#-------------------- MODEL 3 : TWO-LEVEL FORECASTING MODEL -------------------#


#--- MODEL 3A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
trend.seas.res <- trend.seas$residuals
trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
ma.trail.res_2 <- rollmean(trend.seas.res, k = 2, align = "right")
ma.trail.res_2
ma.trail.res_3 <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res_3
ma.trail.res_4 <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR FUTURE PERIOD
ma.trail.res.pred_2 <- forecast(ma.trail.res_2, h = nValid, level = 0)
ma.trail.res.pred_2
ma.trail.res.pred_3 <- forecast(ma.trail.res_3, h = nValid, level = 0)
ma.trail.res.pred_3
ma.trail.res.pred_4 <- forecast(ma.trail.res_4, h = nValid, level = 0)
ma.trail.res.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
fst.2level_2 <- trend.seas.pred$mean + ma.trail.res.pred_2$mean
fst.2level_2
fst.2level_3 <- trend.seas.pred$mean + ma.trail.res.pred_3$mean
fst.2level_3
fst.2level_4 <- trend.seas.pred$mean + ma.trail.res.pred_4$mean
fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
valid_2.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred_2$mean, 
                             fst.2level_2), 3)
names(valid_2.df) <- c("Revenue", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid_2.df

valid_3.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                               ma.trail.res.pred_3$mean, 
                               fst.2level_3), 3)
names(valid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
valid_3.df

valid_4.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                               ma.trail.res.pred_4$mean, 
                               fst.2level_4), 3)
names(valid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
valid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(fst.2level_2, valid.ts), 3)
round(accuracy(fst.2level_3, valid.ts), 3)
round(accuracy(fst.2level_4, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY
tot.trend.seas <- tslm(revenue.ts ~ trend  + season)
summary(tot.trend.seas)

# LEVEL 1 REGRESSION FORECAST FOR FUTURE PERIOD
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 16, level = 0)
tot.trend.seas.pred

# REGRESSION RESIDUALS FOR ENTIRE DATASET
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
tot.ma.trail.res_2 <- rollmean(tot.trend.seas.res, k = 2, align = "right")
tot.ma.trail.res_2
tot.ma.trail.res_3 <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res_3
tot.ma.trail.res_4 <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR FUTURE PERIOD
tot.ma.trail.res_2.pred <- forecast(tot.ma.trail.res_2, h = 16, level = 0)
tot.ma.trail.res_2.pred
tot.ma.trail.res_3.pred <- forecast(tot.ma.trail.res_3, h = 16, level = 0)
tot.ma.trail.res_3.pred
tot.ma.trail.res_4.pred <- forecast(tot.ma.trail.res_4, h = 16, level = 0)
tot.ma.trail.res_4.pred

# TWO-LEVEL FORECAST FOR FUTURE PERIODS 
tot.fst.2level_2 <- tot.trend.seas.pred$mean + tot.ma.trail.res_2.pred$mean
tot.fst.2level_2
tot.fst.2level_3 <- tot.trend.seas.pred$mean + tot.ma.trail.res_3.pred$mean
tot.fst.2level_3
tot.fst.2level_4 <- tot.trend.seas.pred$mean + tot.ma.trail.res_4.pred$mean
tot.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR FUTURE PERIODS
future_2.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res_2.pred$mean, 
                                tot.fst.2level_2), 3)
names(future_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future_2.df

future_3.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res_3.pred$mean, 
                                tot.fst.2level_3), 3)
names(future_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future_3.df

future_4.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res_4.pred$mean, 
                                tot.fst.2level_4), 3)
names(future_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1
plot(allrevenue.ts, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), lwd =1, xaxt = "n",
     main = "MODEL 3A : LEVEL 1 REGRESSION MODEL WITH LINEAR TREND & SEASONALITY") 
axis(1, at = seq(2009, 2024,1), labels = format(seq(2009, 2024, 1)))
lines(tot.trend.seas$fitted, col = "yellow", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "green", lwd = 2)
legend(2008,2800, legend = c("Revenue (2009-2023)", 
                             "Regression Forecast for Pre-Covid Period (2009-2019)",
                             "Regression Forecast for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2 
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(-400, 450), 
     bty = "l", xaxt = "n", xlim = c(2009, 2024), lwd =2, col = "brown", 
     main = "MODEL 3A : LEVEL 2 TRAILING MA MODEL FOR RESIDUALS") 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(tot.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(tot.ma.trail.res_2.pred$mean, col = "red", lwd = 4, lty = 1)
lines(tot.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(tot.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(tot.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2009, 300, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(-400,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(-400,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_2, revenue.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_3, revenue.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_4, revenue.ts), 3)

#--MODEL 3B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY 
quad.seas <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(quad.seas)

# REGRESSION RESIDUALS FOR LEVEL 1
quad.seas.res <- quad.seas$residuals
quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
ma.trail.qres_2 <- rollmean(quad.seas.res, k = 2, align = "right")
ma.trail.qres_2
ma.trail.qres_3 <- rollmean(quad.seas.res, k = 3, align = "right")
ma.trail.qres_3
ma.trail.qres_4 <- rollmean(quad.seas.res, k = 4, align = "right")
ma.trail.qres_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR VALIDATION PERIOD
quad.seas.pred <- forecast(quad.seas, h = nValid, level = 0)
quad.seas.pred$mean

# REGRESSION RESIDUALS FOR VALIDATION PERIOD
quad.seas.res.valid <- valid.ts - quad.seas.pred$mean
quad.seas.res.valid

# TRAILING MA FORECAST FOR RESIDUALS FOR FUTURE PERIOD
ma.trail.qres.pred_2 <- forecast(ma.trail.qres_2, h = nValid, level = 0)
ma.trail.qres.pred_2
ma.trail.qres.pred_3 <- forecast(ma.trail.qres_3, h = nValid, level = 0)
ma.trail.qres.pred_3
ma.trail.qres.pred_4 <- forecast(ma.trail.qres_4, h = nValid, level = 0)
ma.trail.qres.pred_4

# TWO-LEVEL FORECAST FOR VALIDATION PERIOD
qfst.2level_2 <- quad.seas.pred$mean + ma.trail.qres.pred_2$mean
qfst.2level_2
qfst.2level_3 <- quad.seas.pred$mean + ma.trail.qres.pred_3$mean
qfst.2level_3
qfst.2level_4 <- quad.seas.pred$mean + ma.trail.qres.pred_4$mean
qfst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
qvalid_2.df <- round(data.frame(valid.ts, quad.seas.pred$mean, 
                               ma.trail.qres.pred_2$mean, 
                               qfst.2level_2), 3)
names(qvalid_2.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
qvalid_2.df

qvalid_3.df <- round(data.frame(valid.ts, quad.seas.pred$mean, 
                               ma.trail.qres.pred_3$mean, 
                               qfst.2level_3), 3)
names(qvalid_3.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
qvalid_3.df

qvalid_4.df <- round(data.frame(valid.ts, quad.seas.pred$mean, 
                               ma.trail.qres.pred_4$mean, 
                               qfst.2level_4), 3)
names(qvalid_4.df) <- c("Revenue", "Regression.Fst", 
                       "MA.Residuals.Fst", "Combined.Fst")
qvalid_4.df

# FORECAST ACCURACY FOR VALIDATION PERIOD
round(accuracy(qfst.2level_2, valid.ts), 3)
round(accuracy(qfst.2level_3, valid.ts), 3)
round(accuracy(qfst.2level_4, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# LEVEL 1 : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
tot.quad.seas <- tslm(revenue.ts ~ trend + I(trend^2) + season)
summary(tot.quad.seas)

# LEVEL 1 REGRESSION FORECAST FOR FUTURE PERIOD
tot.quad.seas.pred <- forecast(tot.quad.seas, h = 16, level = 0)
tot.quad.seas.pred

# REGRESSION RESIDUALS FOR LEVEL 1
tot.quad.seas.res <- tot.quad.seas$residuals
tot.quad.seas.res

# LEVEL 2 : TRAILING MA MODEL TO FORECAST RESIDUALS
quad.ma.trail.res_2 <- rollmean(tot.quad.seas.res, k = 2, align = "right")
quad.ma.trail.res_2
quad.ma.trail.res_3 <- rollmean(tot.quad.seas.res, k = 3, align = "right")
quad.ma.trail.res_3
quad.ma.trail.res_4 <- rollmean(tot.quad.seas.res, k = 4, align = "right")
quad.ma.trail.res_4

# LEVEL 2 TRAILING MA FORECAST FOR RESIDUALS FOR FUTURE PERIOD
quad.ma.trail.res_2.pred <- forecast(quad.ma.trail.res_2, h = 16, level = 0)
quad.ma.trail.res_2.pred$mean
quad.ma.trail.res_3.pred <- forecast(quad.ma.trail.res_3, h = 16, level = 0)
quad.ma.trail.res_3.pred$mean
quad.ma.trail.res_4.pred <- forecast(quad.ma.trail.res_4, h = 16, level = 0)
quad.ma.trail.res_4.pred$mean

# TWO-LEVEL FORECAST FOR FUTURE PERIODS
quad.fst.2level_2 <- tot.quad.seas.pred$mean + quad.ma.trail.res_2.pred$mean
quad.fst.2level_2
quad.fst.2level_3 <- tot.quad.seas.pred$mean + quad.ma.trail.res_3.pred$mean
quad.fst.2level_3
quad.fst.2level_4 <- tot.quad.seas.pred$mean + quad.ma.trail.res_4.pred$mean
quad.fst.2level_4

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR FUTURE PERIODS
futureq_2.df <- round(data.frame(tot.quad.seas.pred$mean, quad.ma.trail.res_2.pred$mean, 
                                quad.fst.2level_2), 3)
names(futureq_2.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
futureq_2.df

futureq_3.df <- round(data.frame(tot.quad.seas.pred$mean, quad.ma.trail.res_3.pred$mean, 
                                quad.fst.2level_3), 3)
names(futureq_3.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
futureq_3.df

futureq_4.df <- round(data.frame(tot.quad.seas.pred$mean, quad.ma.trail.res_4.pred$mean, 
                                quad.fst.2level_4), 3)
names(futureq_4.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
futureq_4.df

# PLOT THE PREDICTIONS FOR LEVEL 1 
plot(allrevenue.ts, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), lwd =1, xaxt = "n",
     main = "MODEL 3B : LEVEL 1 REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY") 
axis(1, at = seq(2009, 2024,1), labels = format(seq(2009, 2024, 1)))
lines(tot.quad.seas$fitted, col = "yellow", lwd = 2)
lines(tot.quad.seas.pred$mean, col = "green", lwd = 2)
legend(2008,2800, legend = c("Revenue (2009-2023)", 
                             "Regression Forecast for Pre-Covid Period (2009-2019)",
                             "Regression Forecast for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# PLOT THE PREDICTIONS FOR LEVEL 2
plot(tot.quad.seas.res, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(-400, 450), 
     bty = "l", xaxt = "n", xlim = c(2009, 2024), lwd =2, col = "brown", 
     main = "MODEL 3B : LEVEL-2 RESIDUALS & TRAILING MA FOR RESIDUALS") 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(quad.ma.trail.res_2, col = "red", lwd = 2, lty = 1)
lines(quad.ma.trail.res_2.pred$mean, col = "red", lwd = 2, lty = 1)
lines(quad.ma.trail.res_3, col = "orange", lwd = 2, lty = 1)
lines(quad.ma.trail.res_3.pred$mean, col = "orange", lwd = 2, lty = 1)
lines(quad.ma.trail.res_4, col = "blue", lwd = 2, lty = 1)
lines(quad.ma.trail.res_4.pred$mean, col = "blue", lwd = 2, lty = 1)
legend(2009, 400, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=2) for Residuals", 
                             "Trailing MA (k=3) for Residuals",
                             "Trailing MA (k=4) for Residuals"), 
       col = c("brown", "red", "orange","blue"), 
       lty = c(1, 1, 1, 1), lwd =c(2, 2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(-400,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(-400,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_2, revenue.ts), 3)
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_3, revenue.ts), 3)
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_4, revenue.ts), 3)

#------------------ MODEL 4 : AUTOMATED HOLT-WINTER'S MODEL ------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 
# MODEL : (M, Ad, A); alpha = 0.0002, beta = 0.0001, gamma = 0.0001, DAMPING PARAMETER phi = 0.978

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
HW.ZZZ <- ets(revenue.ts, model = "ZZZ")
HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.7178, beta = 0.0003, gamma = 0.0022

# AUTOMATED HW'S MODEL FORECAST FOR FUTURE PERIOD
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 16 , level = 0)
HW.ZZZ.pred

# PLOT THE PREDICTIONS FOR AUTOMATED HW'S MODEL
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 4 : AUTOMATED HOLT-WINTER'S MODEL", 
     lty = 1, col = "green", lwd = 2) 
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(HW.ZZZ.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, 
       legend = c("Revenue (2009-2023)", 
                  "Automated Holt-Winter's Model Forecast for Pre-Covid Period (2009-2019)",
                  "Automated Holt-Winter's Model Forecast for Future (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(HW.ZZZ.pred$fitted, revenue.ts), 3)

#------------------------- MODEL 5 : REGRESSION MODELS ------------------------#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

#--------------- MODEL 5A : REGRESSION MODEL WITH LINEAR TREND ----------------#

train.lin <- tslm(train.ts ~ trend)
summary(train.lin)

# FORECAST FOR VALIDATION DATA
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.lin.pred$mean, valid.ts), 3)

#-------------- MODEL 5B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)

# FORECAST FOR VALIDATION DATA
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.quad.pred$mean, valid.ts), 3)

#---------------- MODEL 5C : REGRESSION MODEL WITH SEASONALITY ----------------#

train.season <- tslm(train.ts ~ season)
summary(train.season)

# FORECAST FOR VALIDATION DATA
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.season.pred$mean, valid.ts), 3)

#-------- MODEL 5D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

# FORECAST FOR VALIDATION DATA
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

#------ MODEL 5E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY --------#

train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.season)

# FORECAST FOR VALIDATION DATA
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred$mean

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

#--------------- MODEL 5A : REGRESSION MODEL WITH LINEAR TREND ----------------#

lin.trend <- tslm(revenue.ts ~ trend)
summary(lin.trend)

# FORECAST FOR FUTURE PERIOD
lin.trend.pred <- forecast(lin.trend, h = 16, level = 0)
lin.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND
plot(lin.trend.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 5A : REGRESSION MODEL WITH LINEAR TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(lin.trend.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, legend = c("Revenue (2009-2023)", 
                             "Regression Model with Linear Trend for Pre-Covid Period (2009-2019)",
                             "Regression Model with Linear Trend for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(lin.trend.pred$fitted, revenue.ts),3)

#-------------- MODEL 5B : REGRESSION MODEL WITH QUADRATIC TREND --------------#

quad.trend <- tslm(revenue.ts ~ trend + I(trend^2))
summary(quad.trend)

# FORECAST FOR FUTURE PERIOD
quad.trend.pred <- forecast(quad.trend, h = 16, level = 0)
quad.trend.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND
plot(quad.trend.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 5B : REGRESSION MODEL WITH QUADRATIC TREND ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(quad.trend.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, legend = c("Revenue (2009-2023)", 
                             "Regression Model with Quadratic Trend for Pre-Covid Period (2009-2019)",
                             "Regression Model with Quadratic Trend for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(quad.trend.pred$fitted, revenue.ts),3)

#---------------- MODEL 5C : REGRESSION MODEL WITH SEASONALITY ----------------#

revenue.season <- tslm(revenue.ts ~ season)
summary(revenue.season)

# FORECAST FOR FUTURE PERIOD
revenue.season.pred <- forecast(revenue.season, h = 16, level = 0)
revenue.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH SEASONALITY BUT NO TREND
plot(revenue.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 5C : REGRESSION MODEL WITH SEASON ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(revenue.season.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, legend = c("Revenue (2009-2023)", 
                             "Regression Model with Seasonality for Pre-Covid Period (2009-2019)",
                             "Regression Model with Seasonality for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(revenue.season.pred$fitted, revenue.ts),3)

#-------- MODEL 5D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#

lin.season <- tslm(revenue.ts ~ trend + season)
summary(lin.season)

# FORECAST FOR FUTURE PERIOD
lin.season.pred <- forecast(lin.season, h = 16, level = 0)
lin.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
plot(lin.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 5D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(lin.season.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, legend = c("Revenue (2009-2023)", 
                             "Regression Model with Linear Trend & Seasonality for Pre-Covid Period (2009-2019)",
                             "Regression Model with Linear Trend & Seasonality for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(lin.season.pred$fitted, revenue.ts),3)

#------- MODEL 5E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#

quad.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
summary(quad.season)

# FORECAST FOR FUTURE PERIOD
quad.season.pred <- forecast(quad.season, h = 16, level = 0)
quad.season.pred$mean

# PLOT THE PREDICTIONS FOR REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY
plot(quad.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 5E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY", 
     lty = 1, col = "green", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(quad.season.pred$fitted, col = "yellow", lwd = 2)
lines(allrevenue.ts)
legend(2008,2900, legend = c("Revenue (2009-2023)", 
                             "Regression Model with Quadratic Trend & Seasonality for Pre-Covid Period (2009-2019)",
                             "Regression Model with Quadratic Trend & Seasonality for Future Period (2020-2023)"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(quad.season.pred$fitted, revenue.ts),3)

#-------------- MODEL 6 : AUTOCORRELATION & AUTOREGRESSIVE MODEL --------------#
#--------------------- AUTOMATED HW'S MODEL + AR(1) MODEL ---------------------#

Acf(train.ts, lag.max = 8, main = "Autocorrelation for Alaska Airlines' Revenue Training Data Set")
Acf(valid.ts, lag.max = 8, main = "Autocorrelation for Alaska Airlines' Revenue Validation Data Set")

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# LEVEL 1 : AUTO HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 
# MODEL : (M, Ad, A); alpha = 0.0002, beta = 0.0001, gamma = 0.0001, DAMPING PARAMETER phi = 0.978

# AUTO HW'S MODEL FORECAST FOR VALIDATION DATA
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
train.residuals <- hw.ZZZ.pred$residuals
train.residuals

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE TRAINING PERIOD
Acf(train.residuals, lag.max = 8, 
    main = "Autocorrelation for Training Residuals of Alaska Airlines' Revenue Data")

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
res.ar1 <- Arima(hw.ZZZ$residuals, order = c(1,0,0))
summary(res.ar1)

# FORECAST FOR VALIDATION DATA
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# AUTOCORRELATION FOR AUTO HW MODEL'S RESIDUALS FOR THE VALIDATION PERIOD
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Alaska Airlines'Revenue Validation Data's Residuals of Residuals")

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR VALIDATION PERIOD
valid.two.level.pred <- hw.ZZZ.pred$mean + res.ar1.pred$mean
valid.df <- round(data.frame(valid.ts, hw.ZZZ.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Revenue","Reg.Forecast",
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(valid.two.level.pred, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# LEVEL 1 : AUTOMATED HOLT-WINTER'S EXPONENTIAL SMOOTHING MODEL WITH ets = "ZZZ" 
HW.ZZZ <- ets(revenue.ts, model = "ZZZ")
HW.ZZZ 
# MODEL : (M, A, M); alpha = 0.7178, beta = 0.0003, gamma = 0.0022

# AUTOMATED HW'S MODEL FORECAST FOR FUTURE PERIOD
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 16 , level = 0)
HW.ZZZ.pred

# LEVEL 2 : AR(1) MODEL FOR AUTO HW MODEL'S RESIDUALS
residual.ar1 <- Arima(HW.ZZZ$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 16, level = 0)
summary(residual.ar1)

# AUTOCORRELATION FOR AR(1) MODEL'S RESIDUALS 
Acf(residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# TWO-LEVEL FORECAST FOR FUTURE PERIOD
HW.ZZZ.ar1.pred <- HW.ZZZ.pred$mean + residual.ar1.pred$mean
HW.ZZZ.ar1.pred

# LEVEL 1, LEVEL 2, TWO-LEVEL FORECAST FOR FUTURE PERIOD
table.df <- round(data.frame(HW.ZZZ.pred$mean, 
                             residual.ar1.pred$mean, HW.ZZZ.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

# PLOT THE PREDICTIONS FOR TWO-LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL)
plot(allrevenue.ts, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 6 : TWO LEVEL MODEL (AUTO HW'S MODEL + AR(1) MODEL", 
     lty = 1, col = "black", lwd = 2)  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(HW.ZZZ$fitted + residual.ar1$fitted, col = "yellow", lwd = 2)
lines(HW.ZZZ.ar1.pred, col = "green", lwd = 2)
legend(2009,2900, legend = c("Revenue (2009-2023)", 
                             "Two-Level Forecast for Pre-Covid Period", 
                             "Two-Level Forecast for Future Period"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(HW.ZZZ$fitted + residual.ar1$fitted, revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#

#--------------------------- FOR VALIDATION PERIOD ----------------------------#

# AUTO-ARIMA MODEL FOR THE TRAINING PERIOD
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# FORECAST FOR VALIDATION DATA
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# FORECAST ACCURACY FOR VALIDATION DATA
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

#------------------------------ FOR FUTURE PERIOD -----------------------------#

# AUTO-ARIMA MODEL FOR THE ENTIRE DATASET
revenue.auto.arima <- auto.arima(revenue.ts)
summary(revenue.auto.arima)

# FORECAST FOR FUTURE PERIOD
revenue.auto.arima.pred <- forecast(revenue.auto.arima, h = 16, level = 0)
revenue.auto.arima.pred$mean

# PLOT THE PREDICTIONS FOR AUTO-ARIMA MODEL
plot(allrevenue.ts, 
     xlab = "Time", ylab = "Revenue (in Million $)", ylim = c(400, 3000), 
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "MODEL 7 : AUTO-ARIMA MODEL")  
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)))
lines(revenue.auto.arima$fitted, col = "yellow", lwd = 2)
lines(revenue.auto.arima.pred$mean, col = "green", lwd = 2)
legend(2009,2900, legend = c("Revenue (2009-2023)", 
                             "Auto-ARIMA Forecast for Pre-Covid Period", 
                             "Auto-ARIMA Forecast for Future Period"), 
       col = c("black", "yellow" , "green"), 
       lty = c(1, 1, 1), lwd =c(2, 2, 2), bty = "n")

# PLOT VERTICAL LINES & HORIZONTAL ARROWS ON THE CHART TO DESCRIBE TRAINING, VALIDATION AND FUTURE PREDICTION INTERVALS
lines(c(2017,2017), c(0,3000)) # FOR TRAINING DATASET
lines(c(2020,2020), c(0,3000)) # FOR VALIDATION DATASET
text(2012.5,3000, "TRAINING")
text(2018.5,3000, "VALIDATION", col = "blue")
text(2022.2, 3000, "FUTURE", col ="green")
arrows(2009,2900,2016.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2017.1,2900,2019.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "blue")
arrows(2020.1,2900,2023.9, 2900,
       code = 3, length = 0.1, lwd = 1, angle = 30, col = "green")

# FORECAST ACCURACY FOR FUTURE PERIOD
round(accuracy(revenue.auto.arima.pred$fitted, revenue.ts), 3)

# PERFORMANCE OF DEVELOPED MODELS ON revenue.ts (2009-2019)

#---------------------------- MODEL 1 : NAIVE MODEL ---------------------------#
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)

#------------------------ MODEL 2 : SEASONAL NAIVE MODEL ----------------------#
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)

#--- MODEL 3A : REGRESSION (LINEAR TREND & SEASONALITY) + TRAILING MA MODEL ---#
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_2, revenue.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_3, revenue.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res_4, revenue.ts), 3)

#--MODEL 3B : REGRESSION (QUADRATIC TREND & SEASONALITY) + TRAILING MA MODEL --#
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_2, revenue.ts), 3)
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_3, revenue.ts), 3)
round(accuracy(tot.quad.seas.pred$fitted+quad.ma.trail.res_4, revenue.ts), 3)

#------------------ MODEL 4 : AUTOMATED HOLT-WINTER'S MODEL -------------------#
round(accuracy(HW.ZZZ.pred$fitted, revenue.ts), 3)

#--------------- MODEL 5A : REGRESSION MODEL WITH LINEAR TREND ----------------#
round(accuracy(lin.trend.pred$fitted, revenue.ts),3)

#-------------- MODEL 5B : REGRESSION MODEL WITH QUADRATIC TREND --------------#
round(accuracy(quad.trend.pred$fitted, revenue.ts),3)

#---------------- MODEL 5C : REGRESSION MODEL WITH SEASONALITY ----------------#
round(accuracy(revenue.season.pred$fitted, revenue.ts),3)

#-------- MODEL 5D : REGRESSION MODEL WITH LINEAR TREND & SEASONALITY ---------#
round(accuracy(lin.season.pred$fitted, revenue.ts),3)

#------- MODEL 5E : REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY -------#
round(accuracy(quad.season.pred$fitted, revenue.ts),3)

#---------------- MODEL 6 : AUTOMATED HW'S MODEL + AR(1) MODEL ----------------#
round(accuracy(HW.ZZZ$fitted + residual.ar1$fitted, revenue.ts),3) 

#- MODEL 7 : AUTOMATED AUTOREGRESSIVE INTEGRATED MOVING AVERAGE (ARIMA) MODEL -#
round(accuracy(revenue.auto.arima.pred$fitted, revenue.ts), 3)

# ************ ESTIMATION OF LOSS OF REVENUE FROM THE BEST 2 MODELS ************

#----- REGRESSION MODEL WITH LINEAR TREND & SEASONALITY + TRAILING MA MODEL----#

# FORECAST USING REGRESSION MODEL WITH LINEAR TREND & SEASONALITY FOR FUTURE PERIOD
lin.season_2level <- tot.trend.seas.pred$fitted + tot.ma.trail.res_2
lin.season.pred <- forecast(lin.season_2level, h = 16, level = 0)
lin.season.pred$mean

# ESTIMATION OF LOSS OF REVENUE
Loss_LTS <- future.ts - lin.season.pred$mean  
Loss_LTS

# COMPARISON OF FORECAST FOR FUTURE PERIOD
Reg_LTS.df <- round(data.frame((time(future.ts)),future.ts, lin.season.pred$mean, Loss_LTS),3)
names(Reg_LTS.df) <- c("Time Period","Actual Revenue (in Million $)","Forecasted Revenue (in Million $)","Loss of Revenue (in Million $)")
Reg_LTS.df

total_loss_LTS <- sum(Reg_LTS.df$`Loss of Revenue (in Million $)`, na.rm = TRUE) # TOTAL LOSS OF REVENUE (2020-2023)
total_revenue_LTS <- sum(Reg_LTS.df$`Forecasted Revenue (in Million $)`, na.rm = TRUE) # ACTUAL REVENUE (2020-2023)
percentage_loss_LTS <- (total_loss_LTS / total_revenue_LTS) * 100 # PERCENTAGE LOSS OF REVENUE (2020-2023)
cat("Total Loss of Revenue:", total_loss_LTS, "(in Million $) \n")
cat("Percentage Loss of Revenue:", percentage_loss_LTS, "%\n")

#--- REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY + TRAILING MA MODEL---#

# FORECAST USING REGRESSION MODEL WITH QUADRATIC TREND & SEASONALITY FOR FUTURE PERIOD
quad.season_2level <- tot.quad.seas.pred$fitted + quad.ma.trail.res_2
quad.season.pred <- forecast(quad.season_2level, h = 16, level = 0)
quad.season.pred$mean

# ESTIMATION OF LOSS OF REVENUE
Loss_QTS <- future.ts - quad.season.pred$mean 
Loss_QTS

# COMPARISON OF FORECAST FOR FUTURE PERIOD
Reg_QTS.df <- round(data.frame((time(future.ts)),future.ts, quad.season.pred$mean, Loss_QTS),3)
names(Reg_QTS.df) <- c("Time Period","Actual Revenue (in Million $)","Forecasted Revenue (in Million $)","Loss of Revenue (in Million $)")
Reg_QTS.df

total_loss_QTS <- sum(Reg_QTS.df$`Loss of Revenue (in Million $)`, na.rm = TRUE) # TOTAL LOSS OF REVENUE (2020-2023)
total_revenue_QTS <- sum(Reg_QTS.df$`Forecasted Revenue (in Million $)`, na.rm = TRUE) # FORECASTED REVENUE (2020-2023)
percentage_loss_QTS <- (total_loss_QTS / total_revenue_QTS) * 100 # PERCENTAGE LOSS OF REVENUE (2020-2023)
cat("Total Loss of Revenue:", total_loss_QTS, "(in Million $)\n")
cat("Percentage Loss of Revenue:", percentage_loss_QTS, "%\n")

