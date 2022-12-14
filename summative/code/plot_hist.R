library(tidyverse); library(haven); library(here)

bac_data <- 
  here("summative/data/hansen_dwi.dta") %>% 
  read_dta() %>% 
  mutate(
    # code into drive under influence (dui) = 1 if bac1 >= 0.08
    dui = ifelse(bac1 >= 0.08, 1, 0),
    
    # create a quadratic term from bac1
    bac1_sq = bac1^2
    
    )

# bac histogram
# output equivalent to stata function histogram bac1, discrete width(0.001)
bac_data %>% 
  ggplot(aes(x = bac1)) +
  geom_histogram(binwidth = 0.001, fill = "grey60", color = NA) + 
  geom_vline(xintercept = 0.08, size = 0.3) +
  geom_vline(xintercept = 0.15, size = 0.3) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.1), 
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"),
                     expand = c(0, 0.01)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 500),
                     labels = c("0", "500", "1,000", "1,500", "2,000"),
                     limits = c(0, 2050),
                     expand = c(0, 0)) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Frequency") +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.4),
    axis.line = element_line(size = 0.4),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
    axis.text.x = element_text(size = 11, margin = margin(5, 0, 0, 0)),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(6, units = "pt"),
    axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
    axis.title.x = element_text(size = 13, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
    
  )

ggsave("summative/figures/bac_histogram_discrete.pdf", height = 6, width = 10)


# output equivalent to stata function histogram bac1, width(0.001)
bac_data %>% 
  mutate(bac1 = bac1 - 0.0005) %>% 
  ggplot(aes(x = bac1)) +
  geom_histogram(binwidth = 0.001, fill = "grey60", color = NA) +
  geom_vline(xintercept = 0.08 - 0.0005, size = 0.3) +
  geom_vline(xintercept = 0.15 - 0.0005, size = 0.3) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.1), 
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"),
                     expand = c(0, 0.01)) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000),
                     labels = c("0", "1,000", "2,000", "3,000"),
                     limits = c(0, 3700),
                     expand = c(0, 0)) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Frequency") +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.4),
    axis.line = element_line(size = 0.4),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
    axis.text.x = element_text(size = 11, margin = margin(5, 0, 0, 0)),
    axis.ticks = element_line(size = 0.4),
    axis.ticks.length = unit(6, units = "pt"),
    axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
    axis.title.x = element_text(size = 13, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
    
  )

ggsave("summative/figures/bac_histogram_continuous.pdf", height = 6, width = 10)
