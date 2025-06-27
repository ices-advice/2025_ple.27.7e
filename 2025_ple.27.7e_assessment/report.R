### ------------------------------------------------------------------------ ###
### Prepare plots and tables for report ####
### ------------------------------------------------------------------------ ###


## Before: method/advice.rds
## After:  figures in report/figures/
##         tables in report/tables

library(icesTAF)
taf.libPaths()
library(icesAdvice)
library(cat3advice)
library(ggplot2)
library(tidyr)
library(dplyr)

mkdir("report")
mkdir("report/figures")
mkdir("report/tables")

### ------------------------------------------------------------------------ ###
### load advice and catch ####
### ------------------------------------------------------------------------ ###
advice <- readRDS("method/advice.rds")
catch <- read.taf("data/advice_history.csv")
catch_7d <- read.csv("data/catch_7d.csv")

discard_survival <- 50


### ------------------------------------------------------------------------ ###
### harvest rates - total, dead, landings ####
### ------------------------------------------------------------------------ ###
df_catch <- catch %>%
  select(year, discards = ICES_discards_stock,
         landings = ICES_landings_stock) %>%
  pivot_longer(cols = -year) %>%
  filter(!is.na(value))
  
c_max <- df_catch %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(catch = sum(value)) %>%
  dplyr::select(catch) %>%
  max(na.rm = TRUE)
yr_min_c <- min(df_catch$year)
yr_max_c <- max(df_catch$year)
cols_c_colours <- c(landings = "#002b5f", discards = "#28b3e8")
p_catch <- ggplot() +
  geom_col(data = df_catch,
                    aes(x = year, y = value/1000, fill = name),
                    na.rm = TRUE) +
  scale_fill_manual("",
                             values = cols_c_colours) + 
  coord_cartesian(ylim = c(0, c_max/1000 * 1.1), 
                           xlim = c(yr_min_c - 1, yr_max_c + 1), 
                           expand = FALSE) +
  labs(x = "", y = "Catches in 1000 tonnes", 
                title = "Catches") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
                 axis.title.x = element_blank(),
                 legend.position = "bottom",
                 legend.key.size = unit(0.5, "lines"),
                 plot.title = element_text(face = "bold", 
                                                    colour = "#002b5f"))
#p_catch

idx_max <- max(advice@I@idx$index, na.rm = TRUE)
p_idx <- ggplot() +
  geom_line(data = advice@I@idx,
                     aes(x = year, y = index),
                     color = "#077c6c",
                     na.rm = TRUE) +
  coord_cartesian(ylim = c(0, idx_max * 1.1), 
                           xlim = c(yr_min_c - 1, yr_max_c + 1), 
                           expand = FALSE) +
  labs(x = "", y = "Biomass index in kg/(hr m beam)", 
                title = "Biomass Index") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
                 axis.title.x = element_blank(),
                 legend.position = "bottom",
                 legend.key.height = unit(0.5, "lines"),
                 plot.title = element_text(face = "bold", 
                                                    colour = "#097e6e"))
#p_idx

### calculate harvest rates
df_hr <- full_join(catch %>%
                     select(year, discards = ICES_discards_stock,
                            landings = ICES_landings_stock),
                   advice@I@idx, by = "year") %>%
  mutate(hr_total = (landings + discards)/index,
         hr_dead = (landings + discards * 0.5)/index,
         hr_landings = (landings)/index) %>%
  select(year, hr_total, hr_dead, hr_landings) %>%
  pivot_longer(-1) %>%
  filter(!is.na(value)) %>%
  mutate(name = factor(name,
                       levels = c("hr_total", "hr_dead", "hr_landings"),
                       labels = c("HR[total]", "HR[dead]", "HR[landings]")))
hr_max <- max(df_hr$value, na.rm = TRUE)

p_hr <- ggplot() +
  geom_line(data = df_hr,
            aes(x = year, y = value, linetype = name),
            color = "#ed6028", 
            na.rm = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  coord_cartesian(ylim = c(0, hr_max * 1.1),
                  xlim = c(2003 - 1, 2023 + 1),
                  expand = FALSE) +
  scale_linetype("", 
                 labels = c(expression(HR[total]), expression(HR[dead]),
                            expression(HR[landings]))) + 
  labs(x = "", y = "Harvest rate", 
       title = "Relative harvest rate (catch / biomass index)") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", 
                                  colour = "#ed6028"))
#p_hr
(p_catch + p_idx)/p_hr
ggsave("report/figures/hr_versions.png", width = 16, height = 12, units = "cm",
       dpi = 300, type = "cairo")

### plot final used in chr rule
plot(advice@F@HR)
ggsave("report/figures/hr_final.png", width = 16, height = 12, units = "cm",
       dpi = 300, type = "cairo")

### harvest rate time series and target harvest 
plot(advice@F, show.data = FALSE)
ggsave("report/figures/hr_status.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### chr rule - figures ####
### ------------------------------------------------------------------------ ###

### A - reference catch
### compare realised catch to advice
catch_dead <- catch %>%
  mutate(catch_dead = ICES_landings_stock + ICES_discards_stock/2) %>%
  select(year, catch_dead, 
         advice = advice_catch_stock)
catch_dead %>%
  pivot_longer(-year) %>%
  mutate(name = factor(name, levels = c("advice", "catch_dead"),
                       labels = c("Advice", "Dead catch"))) %>%
  filter(year >= 2016) %>%
  ggplot() +
  geom_col(mapping = aes(x = year, y = value/1000, fill = name),
           position = "dodge") +
  geom_hline(yintercept = 2, linetype = "1111", linewidth = 0.3) + 
  geom_line(data = catch_dead %>%
              mutate(uptake = catch_dead/advice) %>%
              filter(year >= 2016),
            aes(x = year, y = uptake * 100/50)) +
  scale_fill_discrete("") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(name = "Catch (1000 tonnes)",
                     sec.axis = sec_axis(transform = ~.*50,
                                         name = "Advice uptake (%)")) +
  labs(x = "Year") +
  theme_bw(base_size = 8) +
  theme(axis.title.y.right = element_text(angle = 90))
ggsave("report/figures/catch_advice.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### biomass index - I and b ####
plot(advice@b)
ggsave("report/figures/chr_b.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### harvest rate
plot(advice@F, show.data = FALSE)
ggsave("report/figures/chr_HR.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### chr rule - advice table ####
### ------------------------------------------------------------------------ ###
### create ICES advice style table
### numbers are rounded following ICES rounding rules


### print to screen
advice(advice)
### save in file
capture.output(advice(advice), file = "report/tables/advice_table.txt")

### ------------------------------------------------------------------------ ###
### advice for 7e area ####
### ------------------------------------------------------------------------ ###

### 7e area discard rate
discard_rate_7e <- catch %>%
  select(year, discards = ICES_discards_7e, landings = ICES_landings_7e,
         catch = ICES_catch_7e) %>%
  mutate(discard_rate = discards/catch) %>%
  filter(year >= 2012) %>%
  summarise(discard_rate = mean(discard_rate, na.rm = TRUE)) %>% 
  as.numeric()

### catch in 7d forecast for 7e stock
catch_7d <- catch_7d %>%
  mutate(catch = landings + discards)

### advice in 7e
advice_catch_7e <- advice@advice - catch_7d$catch
advice_landings_7e <- advice_catch_7e * (1 - discard_rate_7e)
advice_discards_7e <- advice_catch_7e * discard_rate_7e
advice_discards_dead_7e <- advice_discards_7e * (1 - discard_survival/100)
advice_discards_surviving_7e <- advice_discards_7e * (discard_survival/100)



advice_7e <- paste0(
  paste(rep("-", 80), collapse = ""), "\n",
  "Plaice in Division 7.e", "\n",
  paste(rep("-", 80), collapse = ""), "\n",
  paste0(format(paste0("Catch of the stock in Division 7.d in ",
                       advice@years[1]), width = 48), 
         " | ", 
         format(paste0(round(catch_7d$catch), " tonnes"), width = 29, 
                justify = "right"),
         "\n"),
  paste0(format("Catch in Division 7.e corresponding to the", width = 48), 
         " | ", "\n",
         format("   advice for the stock", width = 48), " | ",
         format(paste0(round(advice_catch_7e), " tonnes"), width = 29, 
                justify = "right"),
         "\n"),
  paste0(format("Area based discard rate", width = 48), " | ", 
         format(paste0(icesAdvice::icesRound(discard_rate_7e * 100), "%"), 
                width = 29, justify = "right")),
         "\n",
  paste0(format("Discard survival", width = 48), " | ", 
         format(paste0(icesAdvice::icesRound(discard_survival), "%"), 
                width = 29, justify = "right")),
  "\n",
  paste0(format("Landings in Division 7.e corresponding to the", width = 48), 
         " | ", "\n",
         format("   advice", width = 48), 
         " | ",
         format(paste0(round(advice_landings_7e), " tonnes"), width = 29, 
                justify = "right"),
         "\n"),
  paste0(format("Total discards in Division 7.e corresponding to", width = 48), 
         " | ", "\n",
         format("   the advice", width = 48), 
         " | ",
         format(paste0(round(advice_discards_7e), " tonnes"), width = 29, 
                justify = "right"),
         "\n"),
  paste(rep("-", 80), collapse = ""), "\n"
)
cat(advice_7e)

writeLines(advice_7e, "report/tables/advice_table_7e.txt")

### ------------------------------------------------------------------------ ###
### exceptional circumstances - biomass index vs MSE ####
### ------------------------------------------------------------------------ ###
### compare biomass index values to values from MSE projection
### (reference set)

### load percentiles
df_perc <- read.csv("boot/data/idxB_percentiles.csv")

df_perc <- df_perc %>%
  pivot_wider(names_from = iter, values_from = data)

df <- full_join(df_perc, advice@I@idx, by = "year")

df %>%
  ggplot() +
  geom_line(aes(x = year, y = `50%`)) +
  #geom_line(aes(x = year, y = index)) + 
  geom_point(aes(x = year, y = index, colour = "Observations"),
             shape = 4) +
  geom_ribbon(aes(x = year, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.1,
              show.legend = FALSE) +
  geom_ribbon(aes(x = year, ymin = `25%`, ymax = `75%`), alpha = 0.1,
              show.legend = FALSE) +
  scale_colour_manual("", values = c("Observations" = "black")) +
  #geom_line(aes(x = year, y = `50%`), linewidth = 0.4) +
  coord_cartesian(ylim = c(0, 3.9), xlim = c(2002.5, 2043.5),
                  expand = FALSE) + 
  labs(x = "Year", y = "UK-FSP biomass index (kg/hr m beam)") + 
  theme_bw(base_size = 8) +
  theme(strip.text.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        legend.key = element_blank(),
        legend.background = element_blank())
ggsave("report/figures/idx_vs_MSE.png", width = 10, height = 6, units = "cm",
       dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### advice sheet assessment summary table ####
### ------------------------------------------------------------------------ ###

df_smry <- catch %>%
  select(year, 
         landings = ICES_landings_stock, 
         discards = ICES_discards_stock) %>%
  mutate(discards = ifelse(is.na(discards), 0, discards)) %>%
  mutate(catch = landings + discards) %>%
  mutate(catch_dead = landings + discards/2) %>%
  full_join(advice@I@idx, by = "year") %>%
  mutate(harvest_rate = catch_dead/index) %>%
  filter(!is.na(harvest_rate)) %>%
  mutate(landings = round(landings),
         discards = round(discards),
         catch = round(catch),
         catch_dead = round(catch_dead),
         index = icesRound(index),
         harvest_rate = round(harvest_rate))
write.csv(df_smry, file = "report/tables/smry.csv", row.names = FALSE)
