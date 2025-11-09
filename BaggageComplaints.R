
# 1. INTRODUCTION


# 1.1. PROJECT GOAL

# The data set contains monthly observations from 2004 to 2010 for United Airlines, American
# Eagle, and Hawaiian Airlines. The variables in the data set include:
# - Baggage - The total number of passenger complaints for theft of baggage contents, or for
#   lost, damaged, or misrouted luggage for the airline that month.
# - Scheduled - The total number of flights scheduled by that airline that month.
# - Cancelled - The total number of flights cancelled by that airline that month.
# - Enplaned - The total number of passengers who boarded a plane with the airline that month.



# 2. DATA PREPARATION


# 2.1. LOADING PACKAGES

pacman::p_load(tidyverse,
               seasonal,
               zoo,
               tsibble,
               feasts,
               fable,
               fabletools,
               anomalize,
               slider,
               yardstick,
               fpp2,
               SmartEDA,
               DescTools,
               DataExplorer,
               factoextra,
               ggcorrplot,
               ggtime,
               GGally,
               ggforce,
               hexbin,
               corrplot,
               cowplot,
               tidymodels,
               knitr,
               kableExtra,
               gridExtra,
               data.table,
               patchwork,
               tidymodels)


# 2.2. DATA IMPORT


# Note that I have imported the dataset from my laptop.

getwd()
setwd("C:/Users/Utente/Documents/Damiano/R/Mentoring/23.FlightBaggageTimeSeries/rawData")
getwd()

DataOrigin <- read_delim("baggageComplaints.csv")

setwd("C:/Users/Utente/Documents/Damiano/R/Mentoring/23.FlightBaggageTimeSeries")
getwd()


# 2.3. DATASET INTEGRITY CHECK

# Dataset structure, datatype and quality analysis

options(scipen = 999) # used to disable scientific notation when printing numbers.

str(DataOrigin)

# Date is character format and must be converted into date.

DataOriginQuality1 <- ExpData (DataOrigin, type = 1)
# 100% of rows are complete. Just to be on the safe side, let's check out nas

DataOriginNA <- tibble(colNames = colnames(DataOrigin),
                       NAs = colSums(is.na(DataOrigin)))  # Counts missing values in each column

# No missing values.


DataOriginQuality2 <- ExpData (DataOrigin, type = 2)

# There are three different airlines: this will be the key of the time series.


summary(DataOrigin)



# 2.4. FEATURE ENGINEERING 

# In this project, feature engineering is aimed to:
# 1. Select only one airline. Since I want to highlight the very plain and simple workflow
# of time series analysis, I'm looking for simplifying whatever is possible, starting from
# the number of sub series. American Eagle is the chosen one by pure chance.
# 2. After this filter, the column "Airline" is no longer useful since it will store the one and
# only name of "American Eagle". Thus, it can be deselected.
# 3. "Month" and "Year" columns will probably be useless, but can be kept for possible uses.
# 4. Finally, the analysis will be carried on absolute values and not on proportions (i.e.
# complaints vs enplanned); this should keep, again, everything as simple as possible and 
# allow to focus on the analysis process in itself.

Data <- DataOrigin|>
  filter(Airline == "American Eagle")|>
  mutate(Date = my(Date))|>
  select(-Airline)


str(Data)

DataQuality1 <- ExpData(Data, type = 1)

DataQuality2 <- ExpData(Data, type = 2)


# Data quality is fine, so it's possible to check for regularity, meaning that the proper sequence
# of dates is respected.


DataInterval1 <- Data |> 
  distinct(Date) |> 
  pull(Date) |> 
  diff()|>
  as_tibble()|> 
  count(value)



DataInterval2 <- Data |> 
  distinct(Date)                  

# There are 83 gaps out of 84 values. Looks good!



# 2.5. TSIBBLE CONVERSION, INSPECTION AND ADJUSTMENT

DataTsibble <- Data|>
  mutate(Date = yearmonth(Date))|>
  as_tsibble(index = Date)


# I check if the tsibble is effectively regular
is_regular(DataTsibble)
# it's regular

# I check for NAs for every value in the dataset, even if the focus will be on
# "Baggage" (complaints)


columnName <- Data|>
  select("Baggage":"Enplaned")|>
  colnames()

FunctionCheckNAs <- function(columnName){
  
  columnNameSubset <- Data|>
    select(all_of(columnName)) 
  
  tibble(
    column = columnName,
    NAs = sum(is.na(columnNameSubset))
  )
}

DataTibbleNAs <-
  map(.x = columnName,                   
      .f = FunctionCheckNAs)|>
  list_rbind()


# no NAs


# Check for duplicate entries (for each key)

# NOTE: Since date is the index of your tsibble, you can't use group_by(date).
# Instead, use index_by() to structure your time series properly before summarizing.
# This maintains date as a time series index while correctly grouping by coffeeName.

DataTsibble|> 
  index_by(Date)|> 
  summarize(count = n())|> 
  filter(count > 1)

# No duplicates in Date column.


# I now want to detect gaps inside the single product (coffeeName).
# Note that While is.regular() focuses on the global time index (date), scan_gaps() highlights
# irregularities at the coffeeName level (key), showing days where some items had missing
# observations.

DataTsibbleGaps <- DataTsibble|>
  scan_gaps()|>
  as_tibble()|>
  summarize(gapsCount = n())|>
  arrange(desc(gapsCount))

# No gaps to fill





# 3. EXPLORATIVE DATA ANALYSIS


# 1. During EDA, every analysis will be executed on the original dataset. In time series analysis,
#    the goal of EDA is to inform the model about the entire temporal pattern.

# 2. EDA is supposed to be executed on the entire dataset, and only after EDA, data partitioning
#    takes place following a chronological approach (train set is composed of least recent
#    occurencies, while test set have more recent data).

# 3. The main goal of EDA is to identify the full temporal signature before defining the
#    forecasting task.

# 4. During EDA it might be useful to use the dataset either in tibble or in tsibble datatype.
#    For example, SmartEDA works with tibble and not tsibble. It's appropriate to create the
#    tibble as well to  use if and when necessary alongside the tsibble.


options(scipen = 999) # Avoid scientific notation
options(digits = 3)   # Set number of significant digits to 4

DataTibble <- DataTsibble|>
  as_tibble()



# 3.1. DISTRIBUTION ANALYSIS

# 3.1.1. Distribution visualization 

DataTibble |>
  ggplot(aes(x = Baggage)) +
  stat_bin(
    aes(y = after_stat(count)),
    binwidth = 2500,
    boundary = 0,
    alpha = 0.6,
    colour = "white",
    geom = "bar"
  ) +
  stat_bin(
    aes(y = after_stat(count), label = after_stat(count)),
    binwidth = 2500,
    boundary = 0,
    geom = "text",
    size = 3,
    vjust = 1.5
  ) +
  scale_x_continuous(breaks = seq(0, max(DataTibble$Baggage), by = 2500)) +
  labs(
    title = "Overall distribution of complaints",
    x = "number of complaint per month",
    y = "frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)
  )


# 3.1.2. Skewness, kurtosis and outlier

DataEdaNum <- DataTibble|>
  ExpNumStat(#by = "G",
    Qnt = c(0.25, 0.75),
    MesofShape = 2,
    Outlier = TRUE,
    round = 2)|>
  select(-c("Group", "TN", "nNeg", "nPos", "nZero", "NegInf", "PosInf", "NA_Value",
            "Per_of_Missing", "sum")) 

# Skewness is equal to 0.70. Values bigger than 1 or smaller than -1 recall	a high skewness.
# Skwness is moderate.

# Outliers are not present.



# 3.1.3. Normality test

shapiroTest <- shapiro.test(DataTibble$Baggage)

DataTibbleShapiroTest <- tibble(BaggageShapiroW = shapiroTest$statistic,
                                BaggageShapiroP_value = shapiroTest$p.value) 

# p-value is massively below 0.05 treshold. We don't accept null hypotesis of normality.



# 3.2. TIME SERIES VISUALIZATION

# Visualization refers to both the whole time series and its components.

# Let's start from global series. First of all, I want to see which product are mostly chosen
# and then I want to see how purchases are distributed on the timeline.



# 3.2.1. Trend and seasonality detection


# Individual trend 

DataTsibble|>
  ggplot(aes(x = Date, y = Baggage))+
  geom_line(linewidth = 0.5)+
  geom_point(size = 0.75) +
  scale_x_yearmonth(
    date_breaks = "3 months",
    date_labels = "%m %Y" 
  ) +
  ggtitle("American Eagle - Complaints over time") +
  theme(
    axis.text.x = element_text(size = 6)
  )

# Complaints have increased from 2004 to 2007, when they reached their peak, and then have slowed 
# down until 2010. From 2007 till the end of these observation, a downward trend is detectable.



DataTsibble |> 
  ggtime::gg_season(y = Baggage,
                    period = "1y",
                    #facet_period = "1m",
                    label_repel = TRUE)+
  ggtitle("Seasonal complaints")


# The curves of every observed year are pretty similar. A kind of seasonality seems to exist.

# Let's check it via gg_subseries()

DataTsibble |> 
  gg_subseries(y = Baggage,
               period = "1y")+
  ggtitle("American Eagle - seasonal complaint part II")



# 3.2.3. Decomposition

# The decomposition and component analysis follows this steps:
# 1. define a model (STL can be a default choice at this stage) 
# 2. decompose the time series into its components
# 3. visualize decomposition results
# 4. analyse components' features


# DEFINE THE MODEL | I need to define some parameters to set the STL model:
# In order to set the STL model, the following parameters have to be set:
# - trend(window): 13. This allows the trend to capture changes over approximately a year, but
#                  still be responsive to longer-term shifts. You should use an odd number.
# - trend(degree): 1. This is the degree of the polynomial used for the LOESS trend smoothing.
#                  A degree of 1 uses a linear polynomial is the most common choice,
#                  computationally efficient and often sufficient.
# - season(window): 13. A value of 13 is a common choice, as it's just over a full seasonal cycle
#                   and provides a good balance of smoothing without losing important seasonal
#                   details.
# - season(degree): 1. This is the degree of the polynomial for the LOESS seasonal smoothing.
#                   Similar to the trend degree, a degree of 1 is standard and uses a linear
#                   polynomial.
# - robust: TRUE. just to be on safe side even if no outliers have been detected.


DataDecStl <- DataTsibble|>
  model(STL(Baggage ~
              trend(window = 13,
                    degree = 1) + 
              season(window = 13,
                     degree = 1), 
            robust = TRUE))

# EXTRACT COMPONENTS | The goal here is to keep a tibble with every component for every
# value of the column "number"

ComponentStl <- components(DataDecStl)|>
  mutate(sum = (trend + season_year + remainder),
         check = (Baggage - sum))

# PLOT THE COMPONENTS | Rather than using autoplot(), I prefer to pivot the dataset and use ggplot in order to get
# some better visual feedback.

ComponentStl|>
  pivot_longer(cols = Baggage:remainder,
               names_to = "component",
               values_to = "value")|>
  ggplot(aes(x = Date,
             y = value,
             color = component))+
  geom_line()+
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 8))



# ANALYSE TIME SERIE FEATURES | This is the toughest part. Plenty of notes have to be written
# to document process and evaluation.


StlFeatures <- as_tibble(ComponentStl)|>
  summarize(StlTrendStrength  =
              max(0, 1 - var(remainder, na.rm = TRUE) / var(season_adjust, na.rm = TRUE)),
            StlSeasonalStrength =
              max(0, 1 - var(remainder, na.rm = TRUE) / var(season_year + remainder, na.rm = TRUE)),
            StlRemainderStrength =
              1 - var(trend + season_year, na.rm = TRUE) / var(Baggage, na.rm = TRUE),
            StlSeasonalPeak =
              which.max(season_year) %% frequency(ComponentStl),
            StlSeasonalTrough =
              which.min(season_year) %% frequency(ComponentStl),
            StlSpikiness = var(diff(remainder, differences = 2, na.rm = TRUE)),
            StlLinearity = summary(lm(trend ~ seq_along(trend)))$r.squared,
            StlCurvature = sum(abs(diff(trend, differences = 2)), na.rm = TRUE)
  )|>
  pivot_longer(cols = StlTrendStrength:StlCurvature,
               names_to = "featureName",
               values_to = "featureValue")



# Verdict: The analysis shows a time series with a very strong, non-linear trend and a moderately
#          strong seasonal pattern. The irregular component is highly volatile and contains many
#          spikes or outliers, which are a dominant feature of the series.



# 3.2.4.  Autocorrelation analysis

# A lag up to 12 periods (in this case: months) is consistent with data handled.
# Since time series is monthly, a lag equal to 12 means that autocorrelation analysis will be
# executed for the first 12 months:
# - ACF will show correlation between an observation and the observation of day -1, -2 until -12. 
# - PACF will will show correlation between an observation and the observation of 12 months before.

DataTsibble|> 
  gg_tsdisplay(y = Baggage,
               plot_type = 'partial',
               lag = 12) +
  labs(title = "Trend and autocorrelation", y = "")


# Time Series Plot
# - Trend | Time seris trend has a visible upward trend from 2004, peaking around mid-2007. After the peak,
# there's a clear downward trend towards the end of the series.
# This suggests a non-stationary mean.
# - Seasonality | The plot also shows a cyclical or seasonal pattern. The data seems to peak and
# trough on a regular, recurring basis, which suggests a seasonal component.
# - Irregularity | There is some noise or random fluctuation around the trend and seasonal patterns.

# ACF (Autocorrelation Function)
# The ACF plot shows a slow decay in the correlation as the lag
# increases. All the lags up to 12 months are significant. This is a classic signature of a
# non-stationary series that has a trend and/or a seasonal component. The high correlation at lag
# 12 confirms the presence of strong yearly seasonality.

# PACF (Partial Autocorrelation Function)
# The PACF plot shows a single significant spike at lag 1.
# This is a classic pattern for a series with a strong trend. After the first lag, the partial
# autocorrelation drops off quickly and stays within the significance lines, except for a small
# spike at lag 12. This further supports the conclusion of yearly seasonality.


# 3.3  STATIONARY TEST

# A stationary test determines if a time series has consistent statistical properties over
# time. This is a critical step because many common forecasting models, such as ARIMA, assume
# the data is stationary to produce reliable results.

# The test returns a p-value and a test statistic. You use these to decide whether to accept
# or reject the null hypothesis (H0), which states that the time series is non-stationary
# (e.g., in the Augmented Dickey-Fuller (ADF) test).

EdaStationarityTest <- DataTsibble |>
  features(Baggage,
           unitroot_pp) 



# 3.4. VARIABILITY ANALYSIS

# Moving/Rolling mean and standard deviation

# A window of 12 can be kept for a trailing average.

# I need to create a function for coefficient of variation since it's not a built in formula in 
# slider

FunctionCv <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}


EdaMovingAv <-  DataTsibble|>
  mutate(centeredBaggageAvg = slide_mean(Baggage, before = 6, after = 6),
         centeredBaggageSd = slide(Baggage, .f = sd, .before = 6, .after = 6),
         centeredBaggageCv = slide(Baggage, .f = FunctionCv, .before = 6, .after = 6),
         trailingBaggageAvg = slide_mean(Baggage, before = 11),
         trailingBaggageSd = slide(Baggage, .f = sd, .before = 11),
         trailingBaggageCv = slide(Baggage, .f = FunctionCv, .before = 11))|>
  mutate(centeredBaggageSd = unlist(centeredBaggageSd),
         centeredBaggageCv = unlist(centeredBaggageCv),
         trailingBaggageSd = unlist(trailingBaggageSd),
         trailingBaggageCv = unlist(trailingBaggageCv))

str(EdaMovingAv)


# A further step is to visualize all of this.

PlotMovingAv <- EdaMovingAv|>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Baggage, color = "Raw Data"),
            linewidth = 0.7,
            linetype = "solid") +  
  geom_line(aes(y = centeredBaggageAvg, color = "Centered Moving Average"),
            linewidth = 0.7,
            linetype = "dashed") +  
  geom_line(aes(y = trailingBaggageAvg, color = "Trailing Moving Average"),
            linewidth = 0.7,
            linetype = "dotted") +  
  labs(title = "Comparison of Moving Averages",
       subtitle = "Raw Data vs. Centered Moving Average vs. Trailing Moving Average",
       x = "Date", y = "Value",
       color = "Legend") +  # Legend title
  theme_minimal()


PlotMovingSd <- EdaMovingAv|>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = centeredBaggageSd, color = "Centered Moving Sd"),
            linewidth = 0.7,
            linetype = "twodash") +  # Trailing Moving Sd
  geom_line(aes(y = trailingBaggageSd, color = "Trailing Moving Sd"),
            linewidth = 0.7,
            linetype = "twodash") +  # Trailing Moving Sd
  labs(title = "Comparison of Moving Standard deviations",
       subtitle = "Centered Moving Sd vs. Trailing Moving Sd",
       x = "Date", y = "Value",
       color = "Legend") +  # Legend title
  theme_minimal()


PlotMovingCv <- EdaMovingAv|>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = centeredBaggageCv, color = "Centered Moving CV"),
            linewidth = 0.7,
            linetype = "dashed") +  
  geom_line(aes(y = trailingBaggageCv, color = "Trailing Moving CV"),
            linewidth = 0.7,
            linetype = "dotted") +  
  labs(title = "Comparison of Moving CV",
       subtitle = "Centered Moving CV vs. Trailing Moving CV",
       x = "Date", y = "Value",
       color = "Legend") +  # Legend title
  theme_minimal()


PlotMovingAv / PlotMovingSd / PlotMovingCv



# Center and trailing moving average walk along pretty similarly. They show a downward trend,
# after the peak at the beginning of 2007.



# 3.4.	IMPLICATIONS FOR MODELING

# 3.4.1. Distribution analysis

# 1. Both from a visual glance, and from Shapiro-Wilk test, the distribution doesn't show a clear
# normal distribution.
# 2. Skwness is moderate and outliers aren't present.


# 3.4.2. Visualization

# Trend and season

# 1. Regarding to trend, complaints have increased from 2004 to 2007, when they reached their peak,
# and then have slowed down until 2010. From 2007 till the end of these observation, a downward
# trend is detectable.
# 2. As far as seasonality is concerned, looking at the monthly average of complaints, it seems that
# holiday period record more issues: December, January, June, July and August.

# Takeaway: a seasonal (monthly) model like SARIMA may be useful and consistent with data structure.


# Component analysis

# 1. Time series' component analysis returns a very strong, non-linear trend and a moderately
# strong seasonal pattern.
# 2. The irregular component is highly volatile and contains many spikes, which are a dominant
# feature of the series.

# Takeaway: volatility, along with seasonality, should lead toward differencing.


# Autocorrelation

# 1. The ACF (Autocorrelation Function) plot shows a slow decay in the correlation as the lag
# increases. All the lags up to 12 months are significant. This is a classic signature of a
# non-stationary series that has a trend and/or a seasonal component. The high correlation at lag
# 12 confirms the presence of strong yearly seasonality.
# 2. The PACF (Partial Autocorrelation Function) plot shows a single significant spike at lag 1.
# This is a classic pattern for a series with a strong trend. After the first lag, the partial
# autocorrelation drops off quickly and stays within the significance lines, except for a
# spike at lag 12 which supports the conclusion of yearly seasonality.

# Takeaway: again, trend e seasonality seem confirmed.


# 3.4.3. Stationary analysis

# This time series is likely non-stationary, which means that it's unsuitable for models
# that require stationarity, such as ARIMA, unless differencing it. 

# Takeaway: the need for differencing is confirmed.


# 3.4.4. Variability analysis

# This step have detected heteroscedasticity which suggest to use a power transformation
# to stabilize the variance.


# 3.4.5. Conclusions

# Based on the exploratory analysis, the time series is characterized by a non-stationary process
# with both a clear trend and a strong yearly seasonal pattern. The Phillips-Perron unit root test,
# with a p-value greater than 0.05, confirms the non-stationarity.

# Takeaway 1: differencing is needed.
# The ACF plot's slow decay and the significant spikes in the PACF plot at lag 1 confirm the
# presence of a trend, while the significant correlation at lag 12 in both plots indicates strong
# yearly seasonality.

# Takeaway 2: the model should take seasonality into account.
# Further analysis using moving averages confirmed these findings. The 13-month centered moving
# average effectively removed the seasonality, revealing a complex, non-linear trend that rises
# from 2004 to a peak in 2007 before declining. The moving standard deviation and coefficient of
# variation plots indicate that the series' variability is stable over time. This suggests that
# the assumption of constant variance is met.

# Takeaway 3: data transformation (such as a log or Box-Cox) is necessary.

# In conclusion, the time series is a suitable candidate for a SARIMA model due to its
# non-stationarity, seasonality, and stable variance. The modeling process will begin by applying
# a transformation and a first-order seasonal differencing (D=1) with a lag of 12 to remove
# the strong seasonal component. The ACF and PACF plots of the seasonally differenced series
# will then be used to determine if an additional non-seasonal differencing is required and to
# identify the appropriate non-seasonal (p, q) and seasonal (P, Q) parameters for the model.



# 4. MODELING

# This stage's aim is to train and test a SARIMA model. 


# 4.1. DATA PREPARATION AND PARTITIONING 

# Just after splitting data into train and test, we need either to stabilize variance (via
# transformation) and to reduce/erase non-stationarity (via differencing). In this specific case,
# we only need to differencing.
# These activities are to be operated on the train set and in order to handle data leakage risk.


# 4.1.1 Data Partitioning

# At this stage, the tsibble formatted time series is to be splitted;
# splitting tsibble is the best practice because it ensures that the crucial time
# index and key structure of the time series data are preserved and correctly handled during
# the partitioning process.

DataTrainSize <- round(nrow(DataTsibble) * 0.8)  # 80% for training

DataTrain <- DataTsibble |>
  slice(1:DataTrainSize) # slice() è tidyverse

DataTest <- DataTsibble |>
  slice((DataTrainSize + 1):nrow(DataTsibble))


# 4.1.2. Transformation and differencing (on train set - in this order)

# EDA has made clear that both transformation and differencing are needed because of
# heteroschedasticity (variance is not stable) and non-stationariety respectively.

# With regard of differencing, since both seasonality and non stationery have been detected,
# starting with seasonal differencing is the preferable because it often removes both the
# seasonality and a significant part of the non-seasonal trend in one step, potentially
# simplifying the required additional non-seasonal differencing. These are the potential steps to
# follow:



# 4.1.2.1. Transforming

# Once the recipe has been set (based on DataTrain to avoid data leakage), I use it to
# transform both train and test set. 


TransformationRecipe <- DataTrain|>
  recipe(Baggage ~ Date)|>
  step_BoxCox(Baggage)|>
  prep()

TransformationLambdaValue <- TransformationRecipe$steps[[1]]$lambda


DataTrainTransformed <- TransformationRecipe|>
  juice()|>
  as_tsibble(index = Date)



DataTestTransformed <- TransformationRecipe |>
  bake(new_data = DataTest)|>
  as_tsibble(index = Date)


DataTrainTransformedMovingSd <- DataTrainTransformed|>
  mutate(
    centeredBaggageSd = slide(Baggage, .f = sd, .before = 6, .after = 6),
    trailingBaggageSd = slide(Baggage, .f = sd, .before = 11)
  )|>
  mutate(
    centeredBaggageSd = unlist(centeredBaggageSd),
    trailingBaggageSd = unlist(trailingBaggageSd)
  )

DataTrainTransformedMovingSd|>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = centeredBaggageSd, color = "Centered Moving Sd"),
            linewidth = 0.7,
            linetype = "twodash") +  
  geom_line(aes(y = trailingBaggageSd, color = "Trailing Moving Sd"),
            linewidth = 0.7,
            linetype = "twodash") +  
  labs(title = "Comparison of Moving Standard deviations post transformation",
       subtitle = "Centered Moving Sd vs. Trailing Moving Sd",
       x = "Date", y = "Value",
       color = "Legend") +  # Legend title
  theme_minimal()

# Standard deviation has assumed a more regular shape with a downwards trend, it assumes values
# ranging from 0.65 to 0.35 (if we consider the centered SD).
# The more stable standard deviation (ranging from 0.35-0.65) compared to the original data
# confirms successful variance stabilization, making the series more suitable for ARIMA
# modeling which assumes constant variance.


# 4.1.2.2. Differencing

# Let's execeute a unit root test first.

DataTrainTransformed |>
  features(difference(Baggage, lag = 12), unitroot_kpss)

# Test hypotheses are:
# H₀ (null): The series IS stationary
# H₁ (alternative): The series is NOT stationary

# With results such as 1.13 and 0.01 for statistic and p-value respectively, a p-value lower
# than 0.05 suggest to reject the null hypothesis.
# This lead to the conclusion that the seasonally differenced series is still NOT stationary.

# This suggests that seasonal differencing alone (lag 12) hasn't fully achieved stationarity.

# An option to manage this issue is to add AR(1) term to handle remaining autocorrelation,
# avoidind a second differencing that could introduce unnecessary complexity.



# Now, we visualize the same differencing.

DataTrainTransformed|>
  gg_tsdisplay(difference(x = Baggage,
                          lag = 12),
               plot_type='partial',
               lag_max = 24)


# 4.1.3. Time-Dependent Feature Creation (on Transformed/Differenced Train Set)

# This further step is meant to create new stationary predictors (e.g., lagged or rolling average
# features of the target variable) for use in an ARIMAX or supervised machine learning model.
# Since the primary focus of this analysis is a pure SARIMA model, which relies on its internal
# auto-regressive structure and not on other predictor columns, this step will not be
# executed. However, it remains a crucial part of the overall workflow, in particular when
# a machine learning model is used to make predictions.



# 4.2. MODEL FITTING


# 4.2.1. Fitting SARIMA model(s).

# Based on previous analysis, the model to fit is: SARIMA(1,0,0)×(0,1,0)[12] where:

# - pdq(1, 0, 0): Defines the non-seasonal components:
#   p=1 (AR): The Autoregressive term you chose to model the residual correlation at Lag 1.
#   d=0: No further non-seasonal differencing is applied (since you decided to use AR(1) instead).
#   q=0 (MA): No non-seasonal Moving Average term.

# - PDQ(0, 1, 0): Defines the seasonal components:
#   P=0 (SAR): No Seasonal Autoregressive term.
#   D=1: The seasonal differencing (lag 12) applied to the data.
#   Q=0 (SMA): No Seasonal Moving Average term.


ModelsFit <- DataTrainTransformed|>
  model(
    arima100010 = ARIMA(Baggage ~ 0 + pdq(1,0,0) + PDQ(0,1,0)), # mod da ACF
    arima100110 = ARIMA(Baggage ~ 0 + pdq(1,0,0) + PDQ(1,1,0)),
    arima100011 = ARIMA(Baggage ~ 0 + pdq(1,0,0) + PDQ(0,1,1)),
    arima100111 = ARIMA(Baggage ~ 0 + pdq(1,0,0) + PDQ(1,1,1)),
    auto = ARIMA(Baggage, stepwise = FALSE, approx = FALSE) # mod automatico
  )



ModelsFit %>% pivot_longer(everything(), 
                           names_to = "Model name",
                           values_to = "Cost")



# 4.2.2. Model selection with AICc

ModelsFit|>
  glance()|>           # 1. Extracts model performance metrics
  arrange(AICc)|>      # 2. Sorts by the primary selection metric
  select(.model:BIC)   # 3. Selects the relevant output columns


# The automatic model selected ARIMA(0,1,0)(0,1,1)[12], which differs from our theory-driven
# approach by: 
# 1. Using non-seasonal differencing (d=1) instead of AR(1)
# 2. Adding a Seasonal MA term (Q=1) instead of just seasonal differencing
# This suggests the seasonal pattern has a moving average structure that our initial ACF/PACF
# analysis didn't fully capture.


# 4.2.3. Residual analysis

# 4.2.3.1. ACF of residuals

# To define ACF of residuals, it is necessary to set a lag.
# The rule of thumb is to analyze the ACF/PACF up to two or three seasonal periods to ensure all
# seasonality is captured and removed. 36 means three times the seasonal period.


ModelsFit|>
  select(auto)|>
  gg_tsresiduals(lag_max=36)


# Auto model's residual represents two minor overshoot of critical levels, while "our" SARIMA
# model shows three overshoot one of which (lag 12) is pretty significant.
# Specifically:
# Non-seasonal lags (lags 1-11): the bars at the early lags (1, 2, 3, etc.) are all well within
#                                the blue dashed confidence intervals. This indicates that the
#                                non-seasonal terms (the p and q components) chosen by the
#                                automatic model were effective in capturing the short-term
#                                dependence.
# Seasonal lags (lags 12, 24, 36): the bars at the seasonal lags are also not statistically
#                                  significant (they do not cross the blue lines). This confirms
#                                  that the automatic model correctly identified the need for and
#                                  implemented a seasonal difference (D=1) to eliminate the annual
#                                  cycle.
# Overall conclusion: based solely on the ACF plot, the auto model is a very good fit. It has
#                     successfully reduced the time series to uncorrelated residuals.



# 4.2.3.2. Residuals' portmanteau test (to assess white noise)

# Keeping in mind that dof = p + q + P + Q and setting lag=36 to mantain toconsistency with the
# visual ACF analysis, ensuring full coverage of the seasonal effects, it is possible to execute
# the following test on each model.

# The Ljung-Box test checks the null hypothesis (H0) that the residuals are independently
# distributed, which means they are white noise.
# We want the p-value to be greater than the significance level (alfa, usually 0.05).
# If p>0.05, we fail to reject H0 and conclude the residuals are white noise.

# It is advisable to execute the test on different lags. 


# Define the lags you want to test
lags_to_test <- c(24, 36, 48)

# Run Ljung-Box test for each lag individually
Residual_100010 <- map(
  .x = lags_to_test, # qui fissiamo il placeholder. Convenzione stilistica col "." davanti
  .f = ~ augment(ModelsFit) %>%
    filter(.model == "arima100010") %>%
    features(.var = .innov,
             features = ljung_box,
             lag = .x,
             dof = 1) %>%
    mutate(lag_tested = .x))|>
  list_rbind() # map restituisce liste; qui trasformo le liste in righe di un'unica tsibble

Residual_auto <- map(
  .x = lags_to_test,
  .f = ~ augment(ModelsFit) |>
    filter(.model == "auto") |>
    features(.var = .innov,
             features = ljung_box,
             lag = .x,
             dof = 1) |>
    mutate(lag_tested = .x)
) |>
  list_rbind()


# The Ljung-Box test checks the null hypothesis (H0) that the residuals are independently
# distributed (white noise) up to the tested lag.
# We seek a p-value >0.05 to confirm the model is adequate.

# Lag Tested (L): 24	P-Value: 0.26	Conclusion: FAIL to REJECT H0 (means that Residuals are white
#                                                 noise at this lag span).
# Lag Tested (L):36	P-Value: 0.19	Conclusion: FAIL to Reject H0 (means: Residuals are white noise at
#                                               this lag span).
# Lag Tested (L): 48	P-Value: 0.49	Conclusion: FAIL to Reject H0 (means: Residuals are white noise at
#                                                 this lag span).


# While the model passes the test.


# 4.3. FORECASTING

# Once the ARIMA(0,1,0)×(0,1,1)12 model hase been accepted,it's time to move to forecasting, thus
# entering the Testing/Evaluation Phase.
# The crucial step in this phase is generating forecasts and comparing them against the test set 
# (the held-out, future data).

# Assuming the best-fit model is the "auto" model structure, ARIMA(0,1,0)×(0,1,1)[12], this model 
# is to be applied on the test set


# 4.3.1. Generate Forecasts

# Here the point is to set an horizon equal to the number of 
# observations of the test set, in order to To accurately calculate a single out-of-sample
# accuracy metric (like RMSE or MAPE) for the entire holdout period.
# The goal here is to create a single code block that handles the transformation, reversal
# and structure conversion simultaneously.


ForecastAutoModelOriginalScale <- ModelsFit |>
  select(auto) |>
  forecast(h = nrow(DataTestTransformed)) |>
  hilo(level = 95) |>  # Get 80% and 95% prediction intervals
  unpack_hilo(`95%`) |>  # Unpack into separate columns
  as_tibble()|> 
  select(-c(.model, Baggage))|>
  rename(lower_95 = '95%_lower',
         upper_95 = '95%_upper')|>
  mutate(.mean = inv_box_cox(.mean,
                             lambda = TransformationLambdaValue),
         lower_95 = inv_box_cox(lower_95,
                                lambda = TransformationLambdaValue),
         upper_95 = inv_box_cox(upper_95,
                                lambda = TransformationLambdaValue)
  )|>
  as_tsibble(index = Date)    



# 4.3.3. Plotting and Comparison

# Using the more flexible ggplot2 package, it's possible to visualize real data from
# train set (the older one) and from test set (the most recent one).
# Real test set are compared with forecasts and their prediction interval

# Plot the forecasts alongside the historical and test data
ggplot() +
  # Training data
  geom_line(data = DataTrain, aes(x = Date, y = Baggage, colour = "Training Data")) +
  # Test data (actuals)
  geom_line(data = DataTest, aes(x = Date, y = Baggage, colour = "Actual Test")) +
  # Forecast mean
  geom_line(data = ForecastAutoModelOriginalScale, aes(x = Date, y = .mean, colour = "Forecast"), 
            linetype = "dashed") +
  # Prediction interval ribbon
  geom_ribbon(data = ForecastAutoModelOriginalScale, 
              aes(x = Date, ymin = lower_95, ymax = upper_95, fill = "95% PI"), 
              alpha = 0.2) +
  scale_colour_manual(name = "Series",
                      values = c("Training Data" = "black", 
                                 "Actual Test" = "red", 
                                 "Forecast" = "blue")) +
  scale_fill_manual(name = "Interval",
                    values = c("95% PI" = "blue")) +
  labs(y = "Baggage Complaints", 
       title = "Forecast vs. Actuals",
       subtitle = "Forecast with 95% PI")



# 4.4. EVALUATING

# 4.4.1. Refit Models to Include Benchmarks
# We'll refit your final chosen model (arima_auto) along with the two simplest benchmarks,
# ensuring all models are trained on transformed DataTrain.

ModelsFitComparison <- DataTrainTransformed |>
  model(
    arima_auto = ARIMA(Baggage,            # Your chosen, validated model structure
                       stepwise = FALSE,
                       approx = FALSE),
    naive = NAIVE(Baggage),                # Simple, non-seasonal benchmark (Y_t = Y_{t-1})
    seasonal_naive = SNAIVE(Baggage)       # Seasonal benchmark (Y_t = Y_{t-12})
  )


# 4.4.2. Generate Combined Forecasts
# Now we generate forecasts for all three models over the DataTest period; again, transformed
# data will be used.

ForecastsModelsComparison <- ModelsFitComparison |>
  forecast(h = nrow(DataTestTransformed)) |>
  hilo(level = 95) |>
  unpack_hilo(`95%`) |>
  as_tibble() |> 
  select(-Baggage) |>
  rename(lower_95 = `95%_lower`, upper_95 = `95%_upper`) |>
  mutate(.mean = inv_box_cox(.mean, lambda = TransformationLambdaValue),
         lower_95 = inv_box_cox(lower_95, lambda = TransformationLambdaValue),
         upper_95 = inv_box_cox(upper_95, lambda = TransformationLambdaValue)) |>
  left_join(DataTest |> select(Date, Baggage), by = "Date") |>
  rename(actual = Baggage, forecast = .mean)



# 4.4.3. Evaluate All Models Head-to-Head
# In order to fully control the metrics, I'll set a manual calculation.

# Manual accuracy calculation
ForecastsModelsMetric <- ForecastsModelsComparison |>
  group_by(.model) |>
  summarise(
    RMSE = sqrt(mean((forecast - actual)^2)),
    MAE = mean(abs(forecast - actual)),
    MAPE = mean(abs((actual - forecast) / actual)) * 100
  ) |>
  arrange(MAE)


# Key takeaways:

# Your ARIMA model is performing much better than the simple benchmarks - this validates that
# the complexity was worth it!
# The seasonal_naive does better than naive, which suggests there's seasonality in your data
# (which ARIMA is capturing even better)
# MAPE of ~9% is quite reasonable for forecasting - it means on average your forecasts are off
# by about 9%

# Conclusion: You can confidently choose the ARIMA model. It's clearly adding value beyond
# simple benchmarks.


ggplot() +
  geom_line(data = DataTrain, aes(x = Date, y = Baggage, colour = "Training Data")) +
  geom_line(data = DataTest, aes(x = Date, y = Baggage, colour = "Actual Test Data"), size = 1) +
  geom_line(data = ForecastsModelsComparison, 
            aes(x = Date, y = forecast, colour = .model), 
            linetype = "dashed") +
  scale_colour_manual(name = "Series",
                      values = c("Training Data" = "black", 
                                 "Actual Test Data" = "red",
                                 "arima_auto" = "blue",
                                 "naive" = "green",
                                 "seasonal_naive" = "purple")) +
  labs(y = "Baggage Complaints", title = "Model Comparison")








