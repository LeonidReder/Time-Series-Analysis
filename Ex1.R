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

# 2a) ---------------------------------------------------------------------

summary(expert)



