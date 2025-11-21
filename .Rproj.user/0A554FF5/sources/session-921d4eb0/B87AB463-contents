# Preamble
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


# Task 1
# a) 

plot_nom_sp5 <- sp5 %>% ggplot(
  aes(x = Date, y = sp500)) +
  geom_line(linewidth = 0.8, colour = col_pal[["blue"]]) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("S&P500 Monthly Index (Nominal)") +
  theme_bw()

plot_nom_sp5

# b) 

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

# c)
