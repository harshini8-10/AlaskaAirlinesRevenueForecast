# Time-Series-Forecast---Revenue---Alaska-Airlines
This repository contains the project "Time Series Forecasting of Alaska Airlines Revenue and Pandemic Impact", which aims to assess the financial impact of the COVID-19 pandemic on Alaska Airlines' revenue using time series analysis techniques.

# Title of the Project 
Time Series Forecasting of Alaska Airlines Revenue and Pandemic Impact

# Description 
This project estimates the financial impacts of the COVID-19 pandemic on Alaska Airlines' revenue. Using quarterly revenue data from 2009 to 2023, various time series models were developed for analysis to determine the best model and hence gauge revenue fluctuations during the pandemic and post-pandemic period.

# Goals 
•	Determine the best time series forecasting model

•	Forecast revenue for COVID-19 and post-COVID-19 periods using the chosen model.

•	Analyze the impact of COVID-19 on Alaska Airlines' revenue.

•	Compare forecasted revenue with actual data to quantify the pandemic's impact.

# Technologies Used 
•	R Programming : For data preprocessing, model development, visualization and analysis.

•	Time Series Models : Naïve model, Seasonal Naïve model, Two-level model (Regression + Trailing MA), Automated Holt Winter’s Model, Regression Models, Two-level model (Automated Holt Winter’s Model + Autoregressive Model of Order 1), and Automated Autoregressive Integrated Moving Average (ARIMA) Model

# Challenges and Future Work 
•	Challenges: Ensuring data accuracy, model selection based on multiple accuracy metrics, and handling the unpredictability of pandemic effects.

•	Future Work: Implementing more advanced models and exploring additional external factors affecting airline revenue.

# Table of Contents 
1.	Introduction
2.	Project Objectives
3.	Project Scope
4.	Methodology
5.	How to Install and Run the Project
6.	How to Use the Project
7.	Forecasting Process and Analysis
8.	Results
9.	Conclusion

# 1.	Introduction
The COVID-19 pandemic has severely affected various industries, with the airline sector being one of the hardest hits. This project aims to analyze to determine the best model for the revenue data until pre-COVID period and use the same to forecast the revenue for COVID & post-COVID periods. This then aids in assessing the impact on Alaska Airlines using original revenue data for those periods.

# 2.	Project Objectives
•	Determine the Best Time Series Analysis Model: Using historical revenue data for training and validation, identify the most accurate forecasting model.

•	Forecast Revenue for COVID and Post-COVID Periods: Apply the best model to predict future revenue during and after the pandemic.

•	Compare Forecasted vs. Actual Revenue: Assess the accuracy of forecasts and quantify the pandemic's impact on revenue.

# 3.	Project Scope
•	Time Period: Quarterly revenue data from Q1 2009 to Q4 2023, focusing on the COVID period (2020-2021) and post-COVID period (2022-2023).

•	Data Source: https://www.macrotrends.net/stocks/charts/ALK/alaska-air/revenue

•	Procedure: Develop various time series analysis models, determine the best model using accuracy metrics and deploying the same to forecast and compare revenue for pandemic and post-pandemic periods.

# 4.	Methodology
Data Preparation
•	Data Transformation: Convert raw revenue data into CSV format suitable for analysis.

•	Data Partitioning: Split data into training (2009-2016), validation (2017-2019), and future (2020-2023) sets.

Forecasting Models:

i.	Naïve Model
ii.	Seasonal Naïve Model
iii.	Two-Level Forecasting models
      a.	Regression Model with Linear Trend + Trailing Moving Average Models for window widths k = 2, 3, and 4
      
      b.	Regression Model with Quadratic Trend + Trailing Moving Average Models for window widths k = 2, 3, and 4
      
iv.	Automated Holt-Winter’s Model
v.	Regression Models
    a.	Regression Model with Linear Trend
    
    b.	Regression Model with Quadratic Trend
    
    c.	Regression Model with No Trend but Seasonality
    
    d.	Regression Model with Linear Trend but Seasonality
    
    e.	Regression Model with Quadratic Trend but Seasonality
    
vi.	Two-Level Forecasting model : Automated Holt-Winter’s Model + Autoregressive Model
vii.	Automated ARIMA Model

# 5.	How to Install and Run the Project
•	Install R : Ensure R is installed on your machine.

•	Install Required Packages : 
install.packages("forecast") 
install.packages("zoo") 

•	Clone the Repository: Clone this GitHub repository to your local machine.
git clone https://github.com/harshini8-10/AlaskaAirlinesRevenueForecast.git

•	Run the Analysis Script : Navigate to the project directory and run the R script.
Alaska Airlines_Project.R
