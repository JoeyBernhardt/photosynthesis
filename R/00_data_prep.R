## looking at photosynthesis data!!
## last updated by JB, feb 8 2017


# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(broom)


# read in data ------------------------------------------------------------

data_raw <- read_csv("data-raw/Phyto_resp_scenedesmus_feb8_2017_Oxygen.csv")


data <- clean_names(data_raw)

## get the date time in the right format
data <- data %>% 
	mutate(date_time = dmy_hms(date_time))

### look at how temperature drifts over time
ggplot(data = data, aes(x = date_time, y = t_internal_c)) + geom_point()
	
data_pro <-  data %>% 
	select(-c2) %>% 
	gather(key = well_id, value = oxygen, 3:25) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "a4", "a6", "b1", "b3", "b5", "c1", "c3", "c5", "c6", "d1", "d4", "d6"), "phyto", "control")) %>%
		mutate(rate_type = ifelse(time_min < 38.88, "photosynthesis", "respiration"))


data_pro %>% 
	# filter(rate_type == "photosynthesis") %>% 
	filter(time_min < 100) %>% 
	filter(treatment == "phyto") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap( ~ well_id, scales = "free") + geom_smooth(method = lm) + theme_bw() + ylab("oxygen (mg/L)")



# get control slopes ------------------------------------------------------

data_pro %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = well_id, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
	geom_hline(yintercept = 0)

## get the mean control slope when lights are on

average_control_slope <- data_pro %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope = mean(estimate)) 

average_control_slope[[1]]


### now get the indidual slopes for photosynthesis

data_pro %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_slope = estimate - average_control_slope[[1]]) %>%
	ggplot(aes(x = well_id, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
	geom_hline(yintercept = 0)

### get the photosynthesis slope, when lights are on
photosynthesis <- data_pro %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope[[1]])

#### Now onto to the dark side!

## 1. get the control slope when lights are off

data_pro %>% 
	filter(rate_type == "respiration") %>% 
	# filter(treatment == "phyto") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap( ~ well_id) + geom_smooth(method = lm)


average_control_slope_resp <- data_pro %>% 
	filter(rate_type == "respiration") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp[[1]]


data_pro %>% 
	filter(rate_type == "respiration") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_slope = estimate - average_control_slope_resp[[1]]) %>%
	ggplot(aes(x = well_id, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
	geom_hline(yintercept = 0)

## 2. get the respiration slope
respiration <- data_pro %>% 
	filter(rate_type == "respiration") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp[[1]])

### now calculate NPP

all_data <- full_join(photosynthesis, respiration, by = "well_id") %>% 
	mutate(GPP = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 

### plot all the flux rates

all_data %>%
	gather(key = flux_type, value = rate_estimate, GPP, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	ggplot(aes(x = well_id, y = rate_estimate, color = flux_type)) + geom_point(size = 6) +
	geom_hline(yintercept = 0) + ylab("oxygen flux (mg O2 /L * hr)") + theme_bw()

