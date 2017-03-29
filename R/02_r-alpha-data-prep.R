### r-alpha project respirometry


# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(plotrix)
library(stringr)
library(readxl)
library(gridExtra)



# read in data ------------------------------------------------------------

data_raw <- read_excel("data-raw/mar27_16C_photo_resp_Oxygen.xlsx", skip = 8) %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))


id_key <- read_csv("data-raw/mar27_respirometry.csv") %>% 
	select(-notes)

cell_densities <- read_csv("data-raw/r-alpha-mar27-cell-densities.csv")
## blanks here are a2, b4, c1, d3

data_raw %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data <- data_raw %>% 
	select(-b5) %>% 
	filter(time_min > 64.87) %>% 
	select(t_internal_c, everything()) %>% 
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "c2", "d1"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 152, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "16")


data %>% 
	filter(well_id == "a5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id)


data_photosynthesis <- data %>% 
	filter(time_min < 152) %>% 
	filter(time_min > 65)

data_respiration <- data %>% 
	filter(time_min > 153) %>% 
	filter(time_min < 241)

data_photosynthesis %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_respiration %>% 
	ggplot(aes(x = time_min, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")


## get control slopes

average_control_slope_resp<- data_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp[[1]]


average_control_slope_photo <- data_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo[[1]]


photosynthesis <- data_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo[[1]]) %>% 
	mutate(temperature = "16")


respiration <- data_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp[[1]]) %>% 
	mutate(temperature = "16")

all_data_16 <- full_join(photosynthesis, respiration, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 

all_data_16b <- left_join(all_data_16, id_key)

all_data_16c <- left_join(all_data_16b, cell_densities) %>% 
	mutate(cell_density = as.numeric(cell_density))


all_data_16c %>% 
	group_by(temperature.x, treatment, cell_density) %>% 
	summarise_each(funs(mean, std.error), gross_photosynthesis, corrected_photosynthesis_slope, corrected_respiration_slope) %>% 
ggplot(aes(x = cell_density, y = corrected_respiration_slope_mean*-1)) + geom_point() +
	# geom_errorbar(aes(ymin = gross_photosynthesis_mean - gross_photosynthesis_std.error, ymax = gross_photosynthesis_mean + gross_photosynthesis_std.error), width = 0.1) +
	geom_smooth(method = "lm")


all_data_16c %>% 
	group_by(temperature.x, treatment, cell_density) %>% 
	summarise_each(funs(mean, std.error), gross_photosynthesis, corrected_photosynthesis_slope, corrected_respiration_slope) %>% View
	ggplot(aes(x = cell_density, y = gross_photosynthesis_mean)) + geom_point() +
	# geom_errorbar(aes(ymin = gross_photosynthesis_mean - gross_photosynthesis_std.error, ymax = gross_photosynthesis_mean + gross_photosynthesis_std.error), width = 0.1) +
	geom_smooth(method = "lm")



percapita_fluxes <- all_data_16c %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	mutate(percapita_metabolic_rate = rate_estimate/cell_density) %>%
	mutate(flux_type = str_replace(flux_type, "corrected_respiration_slope", "respiration")) %>% 
	ggplot(aes(x = cell_density, y = percapita_metabolic_rate)) + geom_point(size = 4, alpha = 0.5, color = "blue") + 
	# geom_smooth(method = "lm") +
	facet_wrap( ~ flux_type) + theme_bw() + xlab("cell density (population abundance)") +
	ylab("per capita metabolic rate (mgO2/individual/hr)") +
	theme(text = element_text(size=10))

	
population_fluxes <- all_data_16c %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	mutate(percapita_metabolic_rate = rate_estimate/cell_density) %>% 
	mutate(flux_type = str_replace(flux_type, "corrected_respiration_slope", "respiration")) %>% 
	ggplot(aes(x = cell_density, y = rate_estimate)) + geom_point(size = 4, alpha = 0.5, color = "blue") + 
	# geom_smooth(method = "lm") +
	facet_wrap( ~ flux_type) + 
	theme_bw() + xlab("cell density (population abundance)") +
	ylab("population oxygen flux (mgO2/hr)") + 
	theme(text = element_text(size=10))

grid.arrange(population_fluxes, percapita_fluxes, nrow = 2)

all_data_16c %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	mutate(percapita_metabolic_rate = rate_estimate/cell_density) %>% 
group_by(flux_type) %>% 
	do(tidy(lm(log10(percapita_metabolic_rate) ~ log10(cell_density), data = .), conf.int = TRUE)) %>%
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "corrected_respiration_slope", "respiration")) %>% 
	ggplot(aes(x = flux_type, y = estimate)) + geom_point(size = 4) +
	geom_errorbar(aes(ymin= conf.low, ymax = conf.high), width = 0.1) + theme_bw() + ylab("alpha") +
	geom_hline(yintercept = 0)


?log

df <- all_data_16c %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	mutate(percapita_metabolic_rate = rate_estimate/cell_density) %>% 
	mutate(flux_type = str_replace(flux_type, "corrected_respiration_slope", "respiration"))





df %>% 
	group_by(flux_type) %>% 
	ggplot(aes(x = log10(cell_density), y = log10(percapita_metabolic_rate))) + geom_point(size = 3, color = "blue", alpha = 0.5) +
geom_smooth(method = "lm") + facet_wrap( ~ flux_type) + theme_bw() + xlab("ln cell density") + ylab("ln per capita metabolic rate")

?abline

plot(df$cell_density, df$rate_estimate)
abline(a = 0, b = 1)
