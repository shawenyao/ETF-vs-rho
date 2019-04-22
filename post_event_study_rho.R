suppressWarnings(library(tidyverse))
suppressWarnings(library(ggridges))
suppressWarnings(library(zoo))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))

set.seed(350)

setwd("C:/Users/Wenyao/Desktop/R/ETF-vs-rho/")
source("functions/functions_save_svg.R", echo = FALSE)


#==== input ====
sp500_daily_return <- import("input/sp500_daily_return.csv")
sp500_constituents <- import("input/sp500_constituents.csv")
sp500_new_addition_return <- import("input/sp500_new_addition_return.csv")


#==== put all tables together ====
full_table <- sp500_new_addition_return %>% 
  # bring in sp500 return of the same day
  left_join(sp500_daily_return %>% select(date, market_return = return), by ="date") %>% 
  # bring in the inclusion date
  left_join(sp500_constituents %>% select(symbol = Symbol, t0 = `Date first added`), by = "symbol") %>% 
  mutate(
    date_yearmon = date %>% as.yearmon(),
    t0_yearmon = t0 %>% as.yearmon()
  )


#==== calculate monthly correlation and beta ====
event_study <- full_table %>% 
  group_by(symbol, date_yearmon) %>% 
  summarise(
    rho = cor(return, market_return),
    t0_yearmon = unique(t0_yearmon)
  ) %>% 
  mutate(
    t = round((date_yearmon - t0_yearmon) * 12),
    prior_or_post = if_else(t <= 0, "Before", "After") %>% 
      factor(level = c("After", "Before")),
    t_text = t %>% 
      factor(
        level = seq(from = 12, to = -12, by = -1)
      )
  )


#==== summarise ====
paired_t_test <- event_study %>% 
  group_by(symbol, prior_or_post) %>% 
  summarise(
    rho = mean(rho, na.rm = TRUE)
  ) %>% 
  spread(prior_or_post, rho) %>% 
  na.omit()
t.test(
  paired_t_test$Before, 
  paired_t_test$After, 
  paired = TRUE, 
  alternative = "less"
)

event_study_summary <- event_study %>% 
  group_by(t) %>% 
  summarise(
    rho = mean(rho, na.rm = TRUE)
  ) 


#==== plot ====
plot1_monthly_rho <- event_study_summary %>% 
  ggplot(aes(x = t, y = rho)) +
  geom_line(size = 2, linejoin = "round", lineend = "round", color = "gray66") +
  geom_point(color = "dodgerblue3", size = 5) +
  geom_point(color = "white", size = 2) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
  ) +
  labs(x = "Months since Inclusion in Index", y = "Average Correlation")

plot2_monthly_rho_distribution <- event_study %>% 
  ggplot(aes(x = rho, y = t_text, fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    name = "Probability",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  xlim(-0.25, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
  ) +
  labs(x = "Correlation", y = "Months since Inclusion in Index")

plot3_pre_post_distribution <- event_study %>% 
  ggplot(aes(x = rho, y = prior_or_post, fill = factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE, 
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  scale_fill_manual(
    name = "Probability",
    values = c("#E3F6FB", "#D6EEEF", "#A4CDCF", "#6CB4CC")
  ) +
  xlim(-0.25, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank()
  ) +
  labs(x = "Correlation", y = "")


#==== output ====
save_svg(plot = plot1_monthly_rho, file_name = "output/event_study1_monthly_rho.svg", width = 5, height = 3)
ggsave("paper/plot/event_study1_monthly_rho.eps", plot = plot1_monthly_rho, width = 12, height = 8, units = "cm")

save_svg(plot = plot2_monthly_rho_distribution, file_name = "output/event_study2_monthly_rho_distribution.svg", width = 5, height = 9)

save_svg(plot = plot3_pre_post_distribution, file_name = "output/event_study3_pre_post_distribution.svg", width = 5, height = 3)
ggsave("paper/plot/event_study3_pre_post_distribution.eps", plot = plot3_pre_post_distribution, width = 12, height = 8, units = "cm")

# play sound when finished
beep(sound = 2)

warnings()
