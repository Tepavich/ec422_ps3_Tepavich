library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# Adding the three monthly values gives the total quarterly production while taking the mean would only give the average monthly production. The quarterly series does appear to still be seasonal.

retail_ts |> autoplot(sales)

retail_ts |> autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
#The NSA line has spikes near about January every year while the SA line does not. The two series follow the same trend. The SA'd line still shows the recession in 2008 as well as COVID.

can_gas |> autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    x = NULL,
    y = "Production (Millions of Cubic Meters)"
    ) +
  theme_minimal() 

can_gas |> autoplot(Volume) +
  geom_smooth() +
  labs(
    title = "Canadian Natural Gas Production",
    x = NULL,
    y = "Production (Millions of Cubic Meters)"
  ) +
  theme_minimal() 
#I added geom_smooth() to the can_gas plot to show the trend in the data.

#The size of the seasonal swing in can_gas changes over time. 
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |> autoplot(log_volume) + 
  labs(
    title = "Log of Canadian Natural Gas Production",
    x = NULL,
    y = "Log(volume)"
  ) +
  theme_minimal() 
#After taking the log of the data, the seasonal swings are more consistent over time. The data took on a more concave shape. I think the logs are a good idea for can_gas because it definitely leveled out the seasonality that was changing with the trend over time.

retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "NSA Retail Sales",
    x = NULL,
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Log of NSA Retail Sales",
    x = NULL,
    y = "Log(Sales)"
  ) +
  theme_minimal()
#The case for logging this data is weaker than that of can_gas because the seasonal swings are more even and do not change after logging. 

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct) +
  labs(
    title = "Growth in Canadian Natural Gas Production",
    x = NULL,
    y = "Percent"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Difference Between Direct and Log Growth Rates",
    x = NULL,
    y = "Absolute Difference (%)"
  ) +
  theme_minimal()
#The log difference approximation is pretty good in this case as most of the values are small, although it's probably less accurate during the larger spikes in the differences.

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()
#The month-over-month growth rates for the two series are very similar, although the NSA series has more volatility. The SA series is smoother and easier to interpret. The SA series also shows a large drop in growth during the 2008 recession and a large spike in growth during the COVID pandemic.
#It makes sense for the NSA series to be noisier because it has those monthly spikes in January that the SA series does not have.

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
#The seasonal pattern in the NSA series is very strong, the SA series is much smoother and easier to interpret than the NSA series.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y = "Log(Volume)"
  ) +
  theme_minimal()
#Winter months tend to have higher production which makes sense because natural gas is used for heating. 

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
#The seasonal factor is the highest in January, with sales about $1.5 billion higher than the average month. The seasonal swings appear to be getting smaller as time progresses.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
#My adjustment kind of matches the official one in some places; it differs the greatest in the years around 2020.

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
#estimated average monthly growth rate is 0.4%
#June has the most significant seasonal effect

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
#The fitted line tracks pretty well but it misses some spikes from 1960-1980.

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
#There is visible structure left in the residuals. The Ljung box test came back with a p value of nearly zero meaning we reject the null hypothesis of white noise.

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#the first and last forecasts are on January 1980 and 2005
#the snaive model had an MAE=0.562 and an RMSE=0.702
#the tslm model had an MAE=4.01 and an RMSE=4.36
#based on these metrics, the snaive model has better forecast accuracy. Both the MAE and RMSE agree.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
#DM stat = -31.278; pvalue < 2.2e-16
#Reject the null hypothesis of equal forecast accuracy meaning the models were significantly different. 

