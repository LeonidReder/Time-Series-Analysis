# Preamble ----------------------------------------------------------------

rm(list = ls())
install <- TRUE
if (install) {
  install.packages(c("zoo", "readxl", "tidyverse", "plotly", "ggthemes", "forecast", "lmtest", "gridExtra"))
}
library(zoo)
library(readxl)
library(tidyverse)
library(plotly)
library(ggthemes)
library(forecast)
library(gridExtra)
library(lmtest)

col_pal <- colorblind_pal()(8)
names(col_pal) <- c("black", "orange", "lightblue", "brightgreen",
                    "brightyellow", "blue", "red", "pink")
select <- dplyr::select

sp5 <- read.csv("sp500_monthly.csv", header = TRUE, dec = ".", sep = ",")
sp5 <- sp5 %>%
  mutate(Date = as.Date(Date))


# 1a) ----------------------------------------------------------------------

plot_nom_sp5 <- sp5 %>% ggplot(
  aes(x = Date, y = sp500)) +
  geom_line(linewidth = 0.8, colour = col_pal[["blue"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("S&P500 Monthly Index (Nominal)") +
  theme_bw()

plot_nom_sp5

# 1b) ----------------------------------------------------------------------

sp5 <- sp5 %>%
  mutate(log_sp5 = 100 * log(sp500))

plot_index_log <- sp5 %>%
  ggplot(aes(x = Date, y = log_sp5)) +
  geom_line(linewidth = 0.8, colour = col_pal[["lightblue"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("S&P500 Monthly Index (Log Scale)") +
  theme_bw()

plot_index_log

# log-scaling helps interpret constant growth and compare recessions


# 1c) ----------------------------------------------------------------------

sp5 <- sp5 %>%
  mutate(log_diff_sp5 = c(NA, diff(log_sp5)))

plot_index_dlog <- sp5 %>%
  ggplot(aes(x = Date, y = log_diff_sp5)) +
  geom_line(linewidth = 0.8, colour = col_pal[["red"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Log Differences of the S&P500 Monthly Index") +
  theme_bw()

ggplotly(plot_index_dlog)
?ggplotly

#     - log differences approximates percent changes from month to month (for small values)
#     - stabilizes the variance of time series where the variable experiences exponential growth
#     - thus, makes the series more stable and stationary, which helps for computing AR(I)MA models


# 1d) ----------------------------------------------------------------------

# First, compute the percent changes of sp500 manually

sp5 <-  sp5 %>%
  mutate(percent_delta_sp5 = (sp500 / dplyr::lag(sp500, 1) - 1) * 100)

plot_index_pct_chg <- sp5 %>%
  ggplot(aes(x = Date, y = percent_delta_sp5)) +
  geom_line(linewidth = 0.8, colour = col_pal[["orange"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percentage Change of S&P500 Monthly Index") +
  theme_bw()

ggplotly(plot_index_pct_chg)
  
# Comparison plot

plot_compare_chg <- sp5 %>% 
  ggplot(aes(x = Date)) +
  # Add line for the log difference (alpha for transparency)
  geom_line(aes(y = log_diff_sp5, colour = "Log-difference"), linewidth = 0.8, alpha = 0.8) +
  # Add line for the percentage change
  geom_line(aes(y = percent_delta_sp5, colour = "Percentage change"), linewidth = 0.8, alpha = 0.8) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Log Differences vs. Actual % Change of S&P500") +
  # Adjust the colours manually to get nice legend labels
  scale_colour_manual(
    values = c("Log-difference" = col_pal[["red"]], "Percentage change" = col_pal[["orange"]])
  ) +
  theme_bw() +
  # tell ggplot where to put the legend
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.925),
        legend.title = element_blank(),
        legend.background = element_rect(color = NA, fill = NA))

ggplotly(plot_compare_chg, tooltip = c("x", "y", "colour")) %>%
  layout(legend = list(x = 0, orientation = "h", title = element_blank()))


# 1e) ----------------------------------------------------------------------

sp5 <- sp5 %>%
  mutate(log_real_sp5 = 100 * (log(sp500) - log(CPI)))

plot_index_real <- sp5 %>%
  ggplot(aes(x = Date, y = log_real_sp5)) +
  geom_line(linewidth = 0.8, colour = col_pal[["brightgreen"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("S&P500 Monthly Index (Real)") +
  theme_bw()

plot_index_real

plot_index_nom_v_real <- sp5 %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = log_sp5, colour = "Nominal"), linewidth = 0.8, alpha = 0.8) +
  geom_line(aes(y = log_real_sp5, colour = "Real"), linewidth = 0.8, alpha = 0.8) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("S&P500 Monthly Index: Nominal vs Real") +
  scale_colour_manual(
    values = c("Nominal" = col_pal[["lightblue"]], "Real" = col_pal[["brightgreen"]])
  ) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.925),
        legend.title = element_blank(),
        legend.background = element_rect(color = NA, fill = NA))

plot_index_nom_v_real

##

# 2) ---------------------------------------------------------------------

## 

expert <- read.csv("expert.csv", header = TRUE, sep = ",", dec = ".")

# Data expressed as balance: Everyone thinks inflation will go down (up, stay the same) - value of 100 (-100, 0)

# 2a) ---------------------------------------------------------------------

summary(expert)

expert <- expert %>%
  mutate(Date = as.Date(Date))

# Select US-specific data from base dataset to create separate dataframe
expert_us <- expert %>%
  select(Date, contains("US"))

# Plot 
(plot_US_exp <- expert_us %>% pivot_longer(
  cols = -Date,
  names_to = "variable",
  values_to = "value"
) %>%
    ggplot(aes(x = Date, y = value, colour = variable)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgrey") +
    geom_line(linewidth = 0.8) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    xlab(NULL) + ylab(NULL) +
    ggtitle("Forecast Errors from Expectations Balances (U.S.)") +
    scale_colour_manual(
      values = c("E_BAL_Y_US" = col_pal[["pink"]], "E_BAL_P_US" = col_pal[["red"]], "E_BAL_I3_US" = col_pal[["orange"]]),
      labels = c("E_BAL_Y_US" = "GDP Growth", "E_BAL_P_US" = "Inflation", "E_BAL_I3_US" = "3M Interest Rate")
    ) +
    theme_bw() +
    guides(
      color = guide_legend(override.aes = list(linewidth = 1))
    ) +
    theme(axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(color = NA, fill = NA),
          legend.text = element_text(
            size = 12, margin = margin(r = 0.25, unit = "cm")),
          legend.spacing.x = unit(0.35, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.key.width = unit(1.3, "cm"),
          legend.key.height = unit(0.75, "cm"),
    ) +
    facet_wrap(~variable, ncol = 1, scales = "free_y",
               labeller = as_labeller(c(
                 "E_BAL_Y_US" = "GDP Growth",
                 "E_BAL_P_US" = "Inflation",
                 "E_BAL_I3_US" = "3M Interest Rate"
               )))
)

# Histogram for each variable
expert_us %>% pivot_longer(
  cols = -Date,
  names_to = "series",
  values_to = "value"
) %>% 
  # instead of "colour" we use "fill" for histogram bars
  ggplot(aes(x = value, fill = series)) +
  # Add histogram geometry with 50 bins and dark grey borders
  geom_histogram(bins = 50, position = "identity", colour = "darkgrey") +
  xlab("Forecast Error") + ylab("Count") +
  ggtitle("Histogram of Forecast Errors from Expectations Balances (U.S.)") +
  scale_fill_manual(
    values = c("E_BAL_Y_US" = col_pal[["pink"]], "E_BAL_P_US" = col_pal[["red"]], "E_BAL_I3_US" = col_pal[["orange"]]),
    labels = c("E_BAL_Y_US" = "GDP Growth", "E_BAL_P_US" = "Inflation", "E_BAL_I3_US" = "3M Interest Rate")
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12, margin = margin(r = 0.25, unit = "cm")),
        legend.spacing.x = unit(0.45, "cm"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.75, "cm")) +
  facet_wrap(~series, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(
               "E_BAL_Y_US" = "GDP Growth",
               "E_BAL_P_US" = "Inflation",
               "E_BAL_I3_US" = "3M Interest Rate"
             )))

# Summary statistics of forecast errors

expert_us %>%
  # select all columns other than "Date"
  select(-Date) %>% 
  # apply a function to all columns (2 indicates columns)
  apply(2, function(x) c(
    # Calculate summary statistics, removing NAs
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )) %>% 
  # transpose the result for better readability
  t() %>% 
  # convert to tibble and add variable names as a column
  as_tibble(rownames = "Variable")

# - Forecast errors show some persistence in the time series plots, indicating some systematic component.
# - Forecast errors are not centered around zero, but on average negative
# - This means that the forecasts tend to over-predict the actual outcomes, indicating potential bias in the forecasting models. (
#   actual value is lower than the expected one)

# 2b) ---------------------------------------------------------------------

# Common procedure is to conduct a Box and Pierce Test. Therefore, we need to compute the autocovariance function first

acf_us <- expert_us %>%
  # Select all columns except Date
  select(-Date) %>% 
  # Apply function to each column to compute ACF up to lag 12
  # the "." in Acf() refers to the current column being processed
  map(~Acf(., plot = FALSE, lag.max = 12)$acf) %>% 
  # Convert the list of ACFs to a tibble
  as_tibble() %>% 
  # Create a variable for lag values from 0 to 12
  mutate(lag = 0:12) %>% 
  # Keep only lags greater than 0
  filter(lag > 0) %>% 
  # Reshape data to long format for plotting
  pivot_longer(
    cols = -lag,
    names_to = "series",
    values_to = "acf"
  )

plot_acfs_us <- acf_us %>% 
  ggplot(aes(x = lag, y = acf, colour = series)) +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_hline(yintercept = 2/sqrt(nrow(expert_us)), linetype = "dashed", colour = "darkgrey") +
  geom_hline(yintercept = -2/sqrt(nrow(expert_us)), linetype = "dashed", colour = "darkgrey") +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.2), limits = c(-1, 1)) +
  xlab("Lag") + ylab("ACF") +
  ggtitle("ACF of Forecast Errors from Expectations Balances (U.S.)") +
  scale_colour_manual(
    values = c("E_BAL_Y_US" = col_pal[["pink"]], "E_BAL_P_US" = col_pal[["red"]], "E_BAL_I3_US" = col_pal[["orange"]]),
    labels = c("E_BAL_Y_US" = "GDP Growth", "E_BAL_P_US" = "Inflation", "E_BAL_I3_US" = "3M Interest Rate")
  ) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(linewidth = 1))) +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12, margin = margin(r = 0.25, unit = "cm")),
        legend.spacing.x = unit(0.35, "cm"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.key.width = unit(1.3, "cm"),
        legend.key.height = unit(0.75, "cm")) +
  facet_wrap(~series, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(
               "E_BAL_Y_US" = "GDP Growth",
               "E_BAL_P_US" = "Inflation",
               "E_BAL_I3_US" = "3M Interest Rate"
             )))

plot_acfs_us

# Calculates and plots autocorrelation functions for all US forecast error series, dashed lines are the 2*(1/sqrt(number of obs)) significance bounds.
# High values indicate a strongly serially correlated variable

Box.test(expert_us$E_BAL_I3_US, lag = 12, type = "Box-Pierce")


