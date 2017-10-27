c('sweep', 'forecast', 'tidyquant', 'timetk', 'tibbletime') -> list_ispack 
sapply(list_ispack, install.packages)

c('sweep', 'forecast', 'tidyquant', 'timetk', 'tibbletime') -> list_ldpack
lapply(list_ldpack, library, character.only = TRUE) 

#### tidyquant package 
#### http://www.business-science.io/code-tools/2017/10/23/demo_week_tidyquant.html

# Get Stock Prices from Yahoo! Finance

# Create a vector of stock symbols
FANG_symbols <- c("FB", "AMZN", "NFLX", "GOOG")

# Pass symbols to tq_get to get daily prices
FANG_data_d <- FANG_symbols %>%
  tq_get(get = "stock.prices", from = "2014-01-01", to = "2016-12-31")

# Show the result
FANG_data_d

# Plot data
FANG_data_d %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) + 
  geom_line() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Visualize Financial Data")

# Economic Data from the FRED

# Create a vector of FRED symbols
FRED_symbols <- c('ETOTALUSQ176N',    # All housing units
                  'EVACANTUSQ176N',   # Vacant
                  'EYRVACUSQ176N',    # Year-round vacant
                  'ERENTUSQ176N'      # Vacant for rent
)

# Pass symbols to tq_get to get economic data
FRED_data_m <- FRED_symbols %>%
  tq_get(get="economic.data", from = "2001-04-01")

# Show results
FRED_data_m

# Plot data
FRED_data_m %>%
  ggplot(aes(x = date, y = price, color = symbol)) + 
  geom_line() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Visualize Economic Data")

# Change periodicity from daily to monthly using to.period from xts
FANG_data_m <- FANG_data_d %>%
  group_by(symbol) %>%
  tq_transmute(
    select      = adjusted,
    mutate_fun  = to.period,
    period      = "months"
  )

FANG_data_m

# Daily data
FANG_data_d %>%
  ggplot(aes(date, adjusted, color = symbol)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_color_tq() +
  theme_tq() +
  labs(title = "Before transformation: Too Much Data")

# Monthly data
FANG_data_m %>%
  ggplot(aes(date, adjusted, color = symbol)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_color_tq() +
  theme_tq() +
  labs(title = "After transformation: Easier to Understand")


# Lags - Get first 5 lags

# Pro Tip: Make the new column names first, then add to the `col_rename` arg
column_names <- paste0("lag", 1:5)

# First five lags are output for each group of symbols
FANG_data_d %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  tq_mutate(
    select     = adjusted,
    mutate_fun = lag.xts,
    k          = 1:5,
    col_rename = column_names
  )

### timetk package 
### http://www.business-science.io/code-tools/2017/10/24/demo_week_timetk.html
###

# Beer, Wine, Distilled Alcoholic Beverages, in Millions USD
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")
beer_sales_tbl

# Plot Beer Sales
beer_sales_tbl %>%
  ggplot(aes(date, price)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Beer Sales: 2007 through 2016")

beer_sales_tbl %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()

# Augment (adds data frame columns)
beer_sales_tbl_aug <- beer_sales_tbl %>%
  tk_augment_timeseries_signature()

beer_sales_tbl_aug

# linear regression model used, but can use any model
fit_lm <- lm(price ~ ., data = select(beer_sales_tbl_aug, -c(date, diff)))

summary(fit_lm)

# Retrieves the timestamp information
beer_sales_idx <- beer_sales_tbl %>%
  tk_index()

tail(beer_sales_idx)

# Make future index
future_idx <- beer_sales_idx %>%
  tk_make_future_timeseries(n_future = 12)

future_idx

new_data_tbl <- future_idx %>%
  tk_get_timeseries_signature()

new_data_tbl

####
#### Sweep package 
####

# Beer, Wine, Distilled Alcoholic Beverages, in Millions USD
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")
beer_sales_tbl

beer_sales_tbl %>%
  ggplot(aes(date, price)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Beer Sales: 2007 through 2016")

# Convert from tbl to ts
beer_sales_ts <- tk_ts(beer_sales_tbl, start = 2010, freq = 12)
beer_sales_ts

has_timetk_idx(beer_sales_ts)

fit_arima <- auto.arima(beer_sales_ts)

fit_arima

# sw_tidy - Get model coefficients
sw_tidy(fit_arima)

# sw_glance - Get model description and training set accuracy measures
sw_glance(fit_arima) %>%
  glimpse()

# sw_augment - get model residuals
sw_augment(fit_arima, timetk_idx = TRUE)

# Plotting residuals
sw_augment(fit_arima, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(title = "Residual diagnostic") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

# Forecast next 12 months
fcast_arima <- forecast(fit_arima, h = 12)

# sw_sweep - tidies forecast output
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)

fcast_tbl

actuals_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2017-01-01", to = "2017-12-31")

# Visualize the forecast with ggplot
fcast_tbl %>%
  ggplot(aes(x = index, y = price, color = key)) +
  # 95% CI
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  # 80% CI
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  # Prediction
  geom_line() +
  geom_point() +
  # Actuals
  geom_line(aes(x = date, y = price), color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(aes(x = date, y = price), color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  labs(title = "Beer Sales Forecast: ARIMA", x = "", y = "Thousands of Tons",
       subtitle = "sw_sweep tidies the auto.arima() forecast output") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

#### tibbletime package 
#### http://www.business-science.io/code-tools/2017/10/26/demo_week_tibbletime.html

# Load libraries
library(tibbletime) # Future of tidy time series analysis
library(tidyquant)  # Loads tidyverse, tq_get()

# Stock Prices from Yahoo! Finance
FANG_symbols <- c("FB", "AMZN", "NFLX", "GOOG")

FANG_tbl_d <- FANG_symbols %>%
  tq_get(get = "stock.prices", from = "2014-01-01", to = "2016-12-31") 

FANG_tbl_d <- FANG_tbl_d %>%
  group_by(symbol)

FANG_tbl_d

# Setup plotting function that can be reused later
ggplot_facet_by_symbol <- function(data, x, y, group = NULL) {
  
  # Setup expressions
  x_expr     <- rlang::enquo(x)
  y_expr     <- rlang::enquo(y)
  group_expr <- rlang::enquo(group)
  
  if (group_expr == ~NULL) { 
    # No groups
    g <- data %>%
      ggplot(aes(x = rlang::eval_tidy(rlang::`!!`(x_expr)), 
                 y = rlang::eval_tidy(rlang::`!!`(y_expr)), 
                 color = symbol)) +
      labs(x = quo_name(x_expr),
           y = quo_name(y_expr))
  } else {
    # Deal with groups
    g <- data %>%
      ggplot(aes(x = rlang::eval_tidy(rlang::`!!`(x_expr)), 
                 y = rlang::eval_tidy(rlang::`!!`(y_expr)), 
                 color = symbol,
                 group = rlang::eval_tidy(rlang::`!!`(group_expr)) 
      )
      )  +
      labs(x = quo_name(x_expr),
           y = quo_name(y_expr),
           group = quo_name(group_expr))
  }
  
  # Add faceting and theme
  g <- g +
    geom_line() +
    facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
    scale_color_tq() +
    theme_tq()
  
  return(g)
}

# Plot adjusted vs date
FANG_tbl_d %>%
  ggplot_facet_by_symbol(date, adjusted) +
  labs(title = "FANG Stocks: Adjusted Prices 2014 through 2016")

# Convert to tbl_time
FANG_tbl_time_d <- FANG_tbl_d %>%
  as_tbl_time(index = date) 

# Show the tbl_time object we created
FANG_tbl_time_d

# Plot the tbl_time object
FANG_tbl_time_d %>%
  ggplot_facet_by_symbol(date, adjusted) +
  labs(title = "Working with tbltime: Reacts same as tbl class")

# time_filter by day
FANG_tbl_time_d %>%
  time_filter(2014-06-01 ~ 2014-06-15) %>%
  # Plotting
  ggplot_facet_by_symbol(date, adjusted) +
  geom_point() +
  labs(title = "Time Filter: Use functional notation to quickly subset by time",
       subtitle = "2014-06-01 ~ 2014-06-15")

# time_filter by month
FANG_tbl_time_d %>%
  time_filter(~ 2014-03) %>%
  # Plotting
  ggplot_facet_by_symbol(date, adjusted) +
  geom_point() +
  labs(title = "Time Filter: Use shorthand for even easier subsetting",
       subtitle = "~ 2014-03")

# Summarize functions over time periods such as weekly, monthly, etc
FANG_tbl_time_d %>%
  time_summarise(period = "yearly",
                 adj_min   = min(adjusted),
                 adj_max   = max(adjusted),
                 adj_range = adj_max - adj_min
  )

# Summarize by 2-Month periods
FANG_min_max_by_2m <- FANG_tbl_time_d %>%
  time_summarise(period = 2 ~ m,
                 adj_min   = min(adjusted),
                 adj_max   = max(adjusted),
                 adj_med   = median(adjusted)
  ) %>%
  gather(key = key, value = value, adj_min, adj_max, adj_med) 

# Plot using our plotting function, grouping by key (min, max, and median)
FANG_min_max_by_2m %>%
  ggplot_facet_by_symbol(date, value, group = key) +
  geom_point() +
  labs(title = "Summarizing Data By 2-Months (Bi-Monthly)",
       subtitle = "2~m")

# Convert from daily to monthly periodicity
FANG_tbl_time_d %>%
  as_period(period = "month") %>%
  # Plotting
  ggplot_facet_by_symbol(date, adjusted) +
  labs(title = "Periodicity Change from Daily to Monthly") +
  geom_point()

# Convert from daily to bi-monthly periodicity
FANG_tbl_time_d %>%
  as_period(period = 2~m) %>%
  # Plotting
  ggplot_facet_by_symbol(date, adjusted) +
  labs(title = "Periodicity Change to Daily to Bi-Monthly",
       subtitle = "2~m") +
  geom_point()

# Convert from daily to bi-monthly periodicity
FANG_tbl_time_d %>%
  as_period(period = 6~m) %>%
  # Plotting
  ggplot_facet_by_symbol(date, adjusted) +
  labs(title = "Periodicity Change to Daily to Bi-Annually",
       subtitle = "6~m") +
  geom_point()

# Rolling 60-day mean
roll_mean_60 <- rollify(mean, window = 60)

FANG_tbl_time_d %>%
  mutate(mean_60 = roll_mean_60(adjusted)) %>%
  select(-c(open:volume)) %>%
  # Plot
  ggplot_facet_by_symbol(date, adjusted) +
  geom_line(aes(y = mean_60), color = palette_light()[[6]]) +
  labs(title = "Rolling 60-Day Mean with rollify")

# Rolling correlation
roll_corr_60 <- rollify(~ cor(.x, .y, use = "pairwise.complete.obs"), window = 60)

FANG_tbl_time_d %>%
  mutate(cor_60 = roll_corr_60(open, close)) %>%
  select(-c(open:adjusted)) %>%
  # Plot
  ggplot_facet_by_symbol(date, cor_60) +
  labs(title = "Rollify: 60-Day Rolling Correlation Between Open and Close Prices")

# Quantile tbl function
quantile_tbl <- function(x) {
  q <- quantile(x) 
  tibble(
    quantile_name  = names(q),
    quantile_value = q
  )
}

# Test the function
quantile_tbl(1:100)

# Rollified quantile function
roll_quantile_60 <- rollify(quantile_tbl, window = 60, unlist = FALSE)

# Apply rolling quantile 
FANG_quantile_60 <- FANG_tbl_time_d %>%
  mutate(rolling_quantile = roll_quantile_60(adjusted)) %>%
  select(-c(open:adjusted)) %>%
  filter(!is.na(rolling_quantile)) %>%
  unnest()

FANG_quantile_60

FANG_quantile_60 %>%
  ggplot_facet_by_symbol(date, quantile_value, group = quantile_name) +
  labs(title = "Rollify: Create Rolling Quantiles")
