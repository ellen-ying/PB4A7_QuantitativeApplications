library(tidyverse);library(haven);library(here);library(rdrobust);library(ggpubr);library(wesanderson)

bac_data <- 
  here("summative/data/hansen_dwi.dta") %>% 
  read_dta() %>% 
  mutate(
    # code into drive under influence (dui) = 1 if bac1 >= 0.08
    dui = ifelse(bac1 >= 0.08, 1, 0) %>% factor(),
    
    # create a quadratic term from bac1
    bac1_sq = bac1^2
    
  )

# get the data in [0.06, 0.11]
dat_tirmmed <- bac_data %>% filter(bac1 >= 0.06 & bac1 <= 0.11)

# line with OLS estimator
# number of breaks left to the cutoff
n_left <- 
  filter(dat_tirmmed, bac1 < 0.08) %>% 
  nrow() %>% 
  min(sqrt(.), 10*log(.)/log(10)) %>% 
  floor() + 1
# number of breaks right to the cutoff
n_right <- 
  filter(dat_tirmmed, bac1 >= 0.08) %>% 
  nrow() %>% 
  min(sqrt(.), 10*log(.)/log(10)) %>% 
  floor() + 1

min_bac <- min(dat_tirmmed$bac1)
#min_bac <- .030999999
max_bac_lower <- filter(dat_tirmmed, bac1 < 0.08)$bac1 %>% max()
max_bac <- max(dat_tirmmed$bac1) 
breaks <- c(seq(min_bac, max_bac_lower, length.out = n_left), 
            seq(0.08, max_bac, length.out = n_right))

# plotting
cmogram <- 
  dat_tirmmed %>% 
  # create bins with one break at the cutoff point
  mutate(bins = cut(bac1, breaks = breaks)) %>% 
  # select(bins, bac1) %>% arrange(bac1) %>% View()
  group_by(bins) %>% 
  summarize(
    recidivism = mean(recidivism), # calculate the mean recidivism in each group
    bac = mean(bac1), # roughly the midpoint of each bin
    dui = ifelse(bac >=0.08, 1, 0) %>% factor()
  ) %>% 
  ggplot(aes(x = bac-0.0005, y = recidivism)) +
  geom_point(show.legend = FALSE, color = "#728C94") +
  # superimpose a fitted line model and confidence interval using the raw data
  geom_smooth(data = dat_tirmmed,
              aes(x = bac1-0.0005, y = recidivism, color = dui),
              method = "lm", formula = y ~ x, 
              fill = "#CE0000", alpha = 0.2, size = 0.5,
              inherit.aes = FALSE, show.legend = FALSE) +
  geom_vline(xintercept = 0.08, size = 0.3, linetype = 2) +
  scale_x_continuous(breaks = seq(0.06, 0.11, by = 0.01), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.07, 0.15, by = 0.02)) +
  scale_color_discrete(type = c("#CE0000", "#CE0000")) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Recidivism") +
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

ggsave(plot = cmogram, "summative/figures/cmogram.pdf", height = 6, width = 10)

# quadratic model
cmogram_qua <- 
  dat_tirmmed %>% 
  # create bins with one break at the cutoff point
  mutate(bins = cut(bac1, breaks = breaks)) %>% 
  # select(bins, bac1) %>% arrange(bac1) %>% View()
  group_by(bins) %>% 
  summarize(
    recidivism = mean(recidivism), # calculate the mean recidivism in each group
    bac = mean(bac1), # roughly the midpoint of each bin
    dui = ifelse(bac >=0.08, 1, 0) %>% factor()
  ) %>% 
  ggplot(aes(x = bac-0.0005, y = recidivism)) +
  geom_point(show.legend = FALSE, color = "#728C94") +
  # superimpose a fitted line model and confidence interval using the raw data
  geom_smooth(data = dat_tirmmed,
              aes(x = bac1-0.0005, y = recidivism, color = dui),
              method = "lm", formula = y ~ poly(x, 2), 
              fill = "#CE0000", alpha = 0.2, size = 0.5,
              inherit.aes = FALSE, show.legend = FALSE) +
  geom_vline(xintercept = 0.08, size = 0.3, linetype = 2) +
  scale_x_continuous(breaks = seq(0.06, 0.11, by = 0.01), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.07, 0.15, by = 0.02)) +
  scale_color_discrete(type = c("#CE0000", "#CE0000")) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Recidivism") +
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


# rdplot
rd <- with(dat_tirmmed,
     rdplot(recidivism, bac1,
            c = 0.08, # cutoff point
            p = 1, # order of the polynomial
            binselect = "qs", # method for select number of bins
            masspoints = FALSE,
            ci = 95,
            hide = TRUE)
     )

c = 0.08 # threshold
rdplot_mean_bin = rd$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = rd$vars_bins[,"rdplot_mean_y"]
y_hat           = rd$vars_poly[,"rdplot_y"]
x_plot          = rd$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  rd$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  rd$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  rd$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c][-1]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c][-1]
x_plot_l=x_plot[x_plot<c]
# collect dots for ploting lines in one tibble
line <- tibble(x = c(x_plot_l, x_plot_r),
       y = c(y_hat_l, y_hat_r),
       dui = c(rep(0, 499), rep(1, 500)) %>% factor()) 

rdplot <- 
  ggplot() + 
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), 
            col = "#728C94", na.rm = TRUE) +
  geom_line(data = line, aes(x = x, y = y, color = dui), 
            na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE) +
  geom_ribbon(aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, ymax = rdplot_cir_bin),
              fill = "#CE0000", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0.06, 0.11, by = 0.01), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.07, 0.17, by = 0.02)) +
  scale_color_discrete(type = c("#CE0000", "#CE0000")) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Recidivism") +
  geom_vline(xintercept = 0.08, size = 0.3, linetype = 2) +
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

ggsave(plot = rdplot, "summative/figures/rdplot.pdf", height = 6, width = 10)


# rdplot quadratic
rd_qua <- with(dat_tirmmed,
           rdplot(recidivism, bac1,
                  c = 0.08, # cutoff point
                  p = 2, # order of the polynomial
                  binselect = "qs", # method for select number of bins
                  masspoints = FALSE,
                  ci = 95,
                  hide = TRUE)
)

c = 0.08 # threshold
rdplot_mean_bin_qua = rd_qua$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y_qua   = rd_qua$vars_bins[,"rdplot_mean_y"]
y_hat_qua           = rd_qua$vars_poly[,"rdplot_y"]
x_plot_qua          = rd_qua$vars_poly[,"rdplot_x"]
rdplot_cil_bin_qua =  rd_qua$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin_qua =  rd_qua$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin_qua =  rd_qua$vars_bins[,"rdplot_mean_bin"]
y_hat_r_qua=y_hat_qua[x_plot_qua>=c][-1]
y_hat_l_qua=y_hat_qua[x_plot_qua<c]
x_plot_r_qua=x_plot_qua[x_plot_qua>=c][-1]
x_plot_l_qua=x_plot_qua[x_plot_qua<c]
# collect dots for ploting lines in one tibble
line_qua <- tibble(x = c(x_plot_l_qua, x_plot_r_qua),
               y = c(y_hat_l_qua, y_hat_r_qua),
               dui = c(rep(0, 499), rep(1, 500)) %>% factor()) 

rdplot_qua <- 
  ggplot() + 
  geom_point(aes(x = rdplot_mean_bin_qua, y = rdplot_mean_y_qua), 
             col = "#728C94", na.rm = TRUE) +
  geom_line(data = line_qua, aes(x = x, y = y, color = dui), 
            na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE) +
  geom_ribbon(aes(x = rdplot_mean_bin_qua, ymin = rdplot_cil_bin_qua, ymax = rdplot_cir_bin_qua),
              fill = "#CE0000", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0.06, 0.11, by = 0.01), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.07, 0.17, by = 0.02)) +
  scale_color_discrete(type = c("#CE0000", "#CE0000")) +
  labs(x = "Blood Alcohol Content (BAC)", y = "Recidivism") +
  geom_vline(xintercept = 0.08, size = 0.3, linetype = 2) +
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


combined <- 
  ggarrange(cmogram, cmogram_qua, rdplot, rdplot_qua, 
            labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
ggsave(plot = combined, "summative/figures/combined.pdf", height = 8, width = 12)
