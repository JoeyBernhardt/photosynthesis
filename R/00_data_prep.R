## looking at photosynthesis data!!
## last updated by JB, feb 8 2017


# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(plotrix)


# read in data ------------------------------------------------------------

data_raw <- read_csv("data-raw/Phyto_resp_scenedesmus_feb8_2017_Oxygen.csv")
data_raw2 <- read_csv("data-raw/feb_13_photo_resp_24.csv")
data_raw3 <- read_csv("data-raw/feb13_photo_resp_20C_Oxygen.csv")

data_2 <- clean_names(data_raw2)
data_3 <- clean_names(data_raw3)
## get the date time in the right format
data2 <- data_2 %>% 
	mutate(date_time = dmy_hms(date_time))
data3 <- data_3 %>% 
	mutate(date_time = dmy_hms(date_time))

### look at how temperature drifts over time
ggplot(data = data_2, aes(x = date_time, y = t_internal_c)) + geom_point()
	
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


# feb 13 data -------------------------------------------------------------

data_processed24 <-  data2 %>% 
	select(-b5) %>% 
	filter(time_min > 11) %>% 
	gather(key = well_id, value = oxygen, 3:25) %>%
	mutate(treatment = ifelse(well_id %in% c("a1", "a4", "b4", "c2", "c4", "c6", "d1", "d3", "d5"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 105, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "24")


data_processed20 <-  data3 %>% 
	select(-b5) %>% 
	filter(time_min > 11) %>% 
	gather(key = well_id, value = oxygen, 3:25) %>%
	mutate(treatment = ifelse(well_id %in% c("a1", "a4", "b4", "c2", "c4", "c6", "d1", "d3", "d5"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 52, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "20")

all_processed <- bind_rows(data_processed20, data_processed24)



all_processed %>% 
	# filter(time_min > 11) %>% 
	filter(treatment == "phyto") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap( ~ well_id + temperature) + theme_bw() + ylab("oxygen (mg/L)")


ps20 <- data_processed20 %>%
	filter(time_min > 11) %>% 
	filter(treatment == "phyto") %>% 
	mutate(temperature = "20")


average_control_slope_resp_20 <- data_processed20 %>%  
	filter(rate_type == "respiration") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_20[[1]]


average_control_slope_photo_20 <- data_processed20 %>%  
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_photo_20[[1]]



ps24 <- data_processed24 %>%
	filter(time_min > 11) %>% 
	mutate(temperature = "24") %>% 
	filter(treatment == "phyto")

average_control_slope_resp_24 <- data_processed24 %>%  
	filter(rate_type == "respiration") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_24[[1]]


average_control_slope_photo_24 <- data_processed24 %>%  
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>% 
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_photo_24[[1]]


photosynthesis20 <- data_processed20 %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_20[[1]]) %>% 
	mutate(temperature = "20")

photosynthesis24 <- data_processed24 %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_24[[1]])  %>% 
	mutate(temperature = "24")



respiration20 <- data_processed20 %>% 
	filter(rate_type == "respiration") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_20[[1]])  %>% 
	mutate(temperature = "20")

respiration24 <- data_processed24 %>% 
	filter(rate_type == "respiration") %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_24[[1]])  %>% 
	mutate(temperature = "24")


all_data_20 <- full_join(photosynthesis20, respiration20, by = "well_id") %>% 
	mutate(GPP = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


all_data_24 <- full_join(photosynthesis24, respiration24, by = "well_id") %>% 
	mutate(GPP = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


all_data_feb13 <- bind_rows(all_data_20, all_data_24) 
### plot all the flux rates

all_data_feb13 %>%
	gather(key = flux_type, value = rate_estimate, GPP, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	ggplot(aes(x = well_id, y = rate_estimate, color = flux_type)) + geom_point(size = 6) +
	geom_hline(yintercept = 0) + ylab("oxygen flux (mg O2 /L * hr)") + theme_bw() +
	facet_wrap( ~ temperature.x)


all_data_feb13 %>%
	gather(key = flux_type, value = rate_estimate, GPP, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	group_by(temperature.x, flux_type) %>% 
	summarise_each(funs(mean, std.error), rate_estimate) %>%
	# mutate(mean = ifelse(grepl("respiration", flux_type), mean*-1, mean)) %>% 
	ggplot(aes(x = temperature.x, y = mean)) + geom_point(size = 3) +
	facet_wrap( ~ flux_type) + geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1) +
	xlab("temperature") +
	geom_smooth(method = "lm") + 
	ylab("oxygen flux (mg/L*hour)") + geom_hline(yintercept = 0) + theme_bw() 

all_data_feb13 %>%
	ungroup() %>% 
	gather(key = flux_type, value = rate_estimate, GPP, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	# group_by(temperature.x, flux_type) %>% 
	# summarise_each(funs(mean, std.error), rate_estimate) %>% 
	# ungroup() %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>%
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View


all_data_feb13 %>%
	ungroup() %>% 
	gather(key = flux_type, value = rate_estimate, GPP, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	# group_by(temperature.x, flux_type) %>% 
	# summarise_each(funs(mean, std.error), rate_estimate) %>% 
	# ungroup() %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>% 
	filter(flux_type == "GPP") %>% 
	ggplot(aes(x = inverse_temp, y = rate_estimate)) + geom_point() +
	geom_smooth(method = lm) +
	scale_x_reverse()
