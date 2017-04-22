## Feb 16 and 17 respirometry data

# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(plotrix)
library(stringr)

### ok for some reason, it looks like data collected from runs of the respirometer that weren't the first run 
## of the day are substantially lower...I think that might be 



# read in data ------------------------------------------------------------

data_raw12 <- read_csv("data-raw/feb15_photo_resp_12CTT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw16 <- read_csv("data-raw/feb16_photo_resp_16C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw20 <- read_csv("data-raw/feb16_photo_resp_20C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw19 <- read_csv("data-raw/feb17_photo_resp_19C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw22 <- read_csv("data-raw/feb17_photo_resp_22C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw24 <- read_csv("data-raw/feb17_photo_resp_24C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))

data_raw8 <- read_csv("data-raw/feb23_photo_resp_8C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))
data_raw8b <- read_csv("data-raw/feb24_photo_resp_8C_TT_oxygen.csv") %>% 
	clean_names() %>% 
	mutate(date_time = dmy_hms(date_time))


# look at data ------------------------------------------------------------


## blanks here are a2, b4, c1, d3

data_raw12 %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data_12 <- data_raw12 %>% 
	select(-c2) %>% 
	filter(time_min > 100) %>% 
	select(t_internal_c, everything()) %>% 
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "b4", "c1", "d3"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 185, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "12")


data_12 %>% 
	filter(well_id == "a5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id)


data_12_photosynthesis <- data_12 %>% 
	filter(time_min < 185) %>% 
	filter(time_min > 130)

data_12_respiration <- data_12 %>% 
	filter(time_min > 240) %>% 
	filter(time_min < 300)

data_12_photosynthesis %>% 
	# filter(well_id == "a6") %>% 
	filter(time_min > 120) %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_12_respiration %>% 
	filter(well_id == "a6") %>% 
	ggplot(aes(x = time_min, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")



# calculate control slopes ------------------------------------------------

average_control_slope_resp_12 <- data_12_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_12[[1]]


average_control_slope_photo_12 <- data_12_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_12[[1]]


photosynthesis12 <- data_12_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_12[[1]]) %>% 
	mutate(temperature = "12")


respiration12 <- data_12_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_12[[1]]) %>% 
	mutate(temperature = "12")

all_data_12 <- full_join(photosynthesis12, respiration12, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 



# now moving onto 16C -----------------------------------------------------

data_raw16 %>% 
	filter(time_min > 50) %>% 
	ggplot(aes(x = date_time, y = t_internal_c)) + geom_line() 

data_16 <- data_raw16 %>% 
	select(-c2) %>% 
	filter(time_min > 50) %>% 
	select(t_internal_c, everything()) %>%
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "b5", "c1", "c6", "d4"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 75, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "16")

data_16 %>% 
	filter(well_id == "a5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id)


# pick the right portions of the time series to use -----------------------

data_16_photosynthesis <- data_16 %>% 
	filter(time_min < 75) 

data_16_respiration <- data_16 %>% 
	filter(time_min > 100) %>% 
	filter(time_min < 130)

data_16_photosynthesis %>% 
	# filter(well_id == "b2") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_16_respiration %>% 
	filter(well_id == "b2") %>% 
	ggplot(aes(x = time_min, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")


# calculate the control slopes --------------------------------------------


average_control_slope_resp_16 <- data_16_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_16[[1]]


average_control_slope_photo_16 <- data_16_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_16[[1]]


photosynthesis16 <- data_16_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_16[[1]]) %>% 
	mutate(temperature = "16")


respiration16 <- data_16_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_16[[1]]) %>% 
	mutate(temperature = "16")

all_data_16 <- full_join(photosynthesis16, respiration16, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 



# now moving onto 20C -----------------------------------------------------

data_raw20 %>% 
	filter(time_min > 45) %>% 
	ggplot(aes(x = date_time, y = t_internal_c)) + geom_line() 

data_20 <- data_raw20 %>% 
	select(-c2) %>% 
	filter(time_min > 45) %>% 
	select(t_internal_c, everything()) %>%
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "b5", "c1", "c6", "d4"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 75, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "20")


# 20C pick the right portions of the time series to use -----------------------

data_20_photosynthesis <- data_20 %>% 
	filter(time_min < 62) 

data_20_respiration <- data_20 %>% 
	filter(time_min > 90) 

data_20_photosynthesis %>% 
	# filter(well_id == "b2") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_20_respiration %>% 
	filter(well_id == "b2") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 20C calculate the control slopes --------------------------------------------


average_control_slope_resp_20 <- data_20_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_20[[1]]


average_control_slope_photo_20 <- data_20_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_20[[1]]


photosynthesis20 <- data_20_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_20[[1]]) %>% 
	mutate(temperature = "20")


respiration20 <- data_20_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_20[[1]]) %>% 
	mutate(temperature = "20")

all_data_20 <- full_join(photosynthesis20, respiration20, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


# now onto 24C ------------------------------------------------------------

data_raw24 %>% 
	filter(time_min > 66) %>% 
	ggplot(aes(x = date_time, y = t_internal_c)) + geom_line() 

data_24 <- data_raw24 %>% 
	select(-c2) %>% 
	filter(time_min > 66) %>% 
	select(t_internal_c, everything()) %>%
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "a5", "b1", "b6", "c6", "d4"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 114, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "24")

# 24C pick the right portions of the time series to use -----------------------

data_24_photosynthesis <- data_24 %>% 
	filter(time_min < 114) 

data_24_respiration <- data_24 %>% 
	filter(time_min > 130) 

data_24_photosynthesis %>% 
	# filter(well_id == "d1") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_24_respiration %>% 
	filter(well_id == "b2") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 24C calculate the control slopes --------------------------------------------


average_control_slope_resp_24 <- data_24_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_24[[1]]


average_control_slope_photo_24 <- data_24_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_24[[1]]


photosynthesis24 <- data_24_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_24[[1]]) %>% 
	mutate(temperature = "24")


respiration24 <- data_24_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_24[[1]]) %>% 
	mutate(temperature = "24")

all_data_24 <- full_join(photosynthesis24, respiration24, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 

summary_24 <- all_data_24 %>% 
	ungroup() %>% 
	summarise(mean_gross_photosynthesis = mean(gross_photosynthesis),
						mean_respiration = mean(corrected_respiration_slope))

summary_20 <- all_data_20 %>% 
	ungroup() %>% 
	summarise(mean_gross_photosynthesis = mean(gross_photosynthesis),
						mean_respiration = mean(corrected_respiration_slope))


# now onto 22C ------------------------------------------------------------

data_raw22 %>% 
	filter(time_min > 30) %>% 
	ggplot(aes(x = date_time, y = t_internal_c)) + geom_line() 

data_22 <- data_raw22 %>% 
	select(-c2) %>% 
	# filter(time_min > 66) %>% 
	select(t_internal_c, everything()) %>%
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "a5", "b1", "b6", "c6", "d4"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 114, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "22")

# 22C pick the right portions of the time series to use -----------------------

data_22_photosynthesis <- data_22 %>% 
	filter(time_min > 30) %>% 
	filter(time_min < 90) 

data_22_respiration <- data_22 %>% 
	filter(time_min > 120) 

data_22_photosynthesis %>% 
	# filter(well_id == "c5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_22_respiration %>% 
	filter(well_id == "c5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 22C calculate the control slopes --------------------------------------------


average_control_slope_resp_22 <- data_22_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_22[[1]]


average_control_slope_photo_22 <- data_22_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_22[[1]]


photosynthesis22 <- data_22_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_22[[1]]) %>% 
	mutate(temperature = "22")


respiration22 <- data_22_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_22[[1]]) %>% 
	mutate(temperature = "22")

all_data_22 <- full_join(photosynthesis22, respiration22, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 

# now onto 19C ------------------------------------------------------------

data_raw19 %>% 
	filter(time_min > 30) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data_19 <- data_raw19 %>% 
	select(-c2) %>% 
	# filter(time_min > 66) %>% 
	select(t_internal_c, everything()) %>%
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "a5", "b1", "b6", "c6", "d4"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 80, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "19")

# 19C pick the right portions of the time series to use -----------------------

data_19_photosynthesis <- data_19 %>% 
	filter(time_min > 30) %>% 
	filter(time_min < 80) 

data_19_respiration <- data_19 %>% 
	filter(time_min > 110) 

data_19_photosynthesis %>% 
	# filter(well_id == "c5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_19_respiration %>% 
	filter(well_id == "c5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 19C calculate the control slopes --------------------------------------------


average_control_slope_resp_19 <- data_19_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_19[[1]]


average_control_slope_photo_19 <- data_19_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_19[[1]]


photosynthesis19 <- data_19_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_19[[1]]) %>% 
	mutate(temperature = "19")


respiration19 <- data_19_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_19[[1]]) %>% 
	mutate(temperature = "19")

all_data_19 <- full_join(photosynthesis19, respiration19, by = "well_id") %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) 


# now onto 8C -------------------------------------------------------------

data_raw8 %>% 
	filter(time_min > 19.33) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data_8 <- data_raw8 %>% 
	select(-c2) %>% 
	# filter(time_min > 66) %>% 
	select(t_internal_c, everything()) %>% 
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "b2", "b6", "c3", "d2"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 155, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "8")

data_8 %>% 
	filter(time_min > 20) %>% 
	filter(time_min < 293) %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap( ~ well_id)

# 8C pick the right portions of the time series to use -----------------------

data_8_photosynthesis <- data_8 %>% 
	filter(time_min > 20) %>% 
	filter(time_min < 155) 

data_8_photosynthesis2 <- data_8 %>% 
	filter(time_min > 300) 


data_8_respiration <- data_8 %>% 
	filter(time_min > 175) %>% 
	filter(time_min < 293)

data_8_photosynthesis2 %>% 
	filter(well_id == "b1") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_8_respiration %>% 
	filter(well_id == "c4") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 8C calculate the control slopes --------------------------------------------


average_control_slope_resp_8 <- data_8_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_8[[1]]


average_control_slope_photo_8 <- data_8_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_8[[1]]


photosynthesis8 <- data_8_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_8[[1]]) %>% 
	mutate(temperature = "8")


respiration8 <- data_8_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_8[[1]]) %>% 
	mutate(temperature = "8")

all_data_8 <- full_join(photosynthesis8, respiration8, by = "well_id") %>% 
	filter(corrected_photosynthesis_slope > 0) %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) %>% 
	filter(gross_photosynthesis > 0) %>% 
	filter(corrected_respiration_slope < 0)

# now onto 8C -b -------------------------------------------------------------

data_raw8b %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data_8b <- data_raw8b %>% 
	select(-c2) %>% 
	filter(time_min > 100) %>% 
	select(t_internal_c, everything()) %>% 
	gather(key = well_id, value = oxygen, 4:26) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "b1", "b5", "c5", "d1"), "control", "phyto")) %>%
	mutate(rate_type = ifelse(time_min < 310, "photosynthesis", "respiration")) %>% 
	mutate(temperature = "8")

data_8b %>% 
	filter(time_min > 190) %>% 
	filter(time_min < 410) %>% 
	ggplot(aes(x = time_min, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap( ~ well_id)

# 8C pick the right portions of the time series to use -----------------------

data_8b_photosynthesis <- data_8b %>% 
	filter(time_min > 275) %>% 
	filter(time_min < 300) 

data_8b_respiration <- data_8b %>% 
	filter(time_min > 385) %>% 
	filter(time_min < 420) 

data_8b_photosynthesis %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 

data_8b_respiration %>% 
	filter(time_min > 100) %>% 
	ggplot(aes(x = time_min, y = t_internal_c)) + geom_line() 


data_8b_photosynthesis %>% 
	filter(well_id == "c4") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = treatment)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

data_8b_respiration %>% 
	# filter(well_id == "c5") %>% 
	ggplot(aes(x = date_time, y = oxygen, color = rate_type)) + geom_point() +
	facet_wrap(~ well_id) + geom_smooth(method = "lm")

# 8C calculate the control slopes --------------------------------------------


average_control_slope_resp_8b <- data_8b_respiration %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_resp = mean(estimate)) 

average_control_slope_resp_8b[[1]]


average_control_slope_photo_8b <- data_8b_photosynthesis %>%  
	filter(treatment == "control") %>% 
	group_by(well_id) %>% 
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	ungroup() %>%
	summarise(average_control_slope_photo = mean(estimate)) 

average_control_slope_photo_8b[[1]]


photosynthesis8b <- data_8b_photosynthesis %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_photosynthesis_slope = estimate - average_control_slope_photo_8b[[1]]) %>% 
	mutate(temperature = "8")


respiration8b <- data_8b_respiration %>% 
	filter(treatment == "phyto") %>% 
	group_by(well_id) %>%
	do(tidy(lm(oxygen ~ date_time, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(corrected_respiration_slope = estimate - average_control_slope_resp_8b[[1]]) %>% 
	mutate(temperature = "8")

all_data_8b <- full_join(photosynthesis8b, respiration8b, by = "well_id") %>% 
	filter(corrected_photosynthesis_slope > 0) %>% 
	mutate(gross_photosynthesis = corrected_photosynthesis_slope + (-1*corrected_respiration_slope)) %>% 
	filter(gross_photosynthesis > 0) %>% 
	filter(corrected_respiration_slope < 0)



all_data_TT <- bind_rows(all_data_12, all_data_16, all_data_19, all_data_20, all_data_22, all_data_24, all_data_8b, all_data_8, .id = "id")


all_data_TT %>% 
	group_by(temperature.x) %>% 
	summarise_each(funs(mean, std.error), gross_photosynthesis, corrected_photosynthesis_slope, corrected_respiration_slope) %>% 
	View

all_data_TT %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	# filter(temperature.x != 20) %>% 
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ggplot(aes(x = temperature.x, y = rate_estimate)) + geom_point(size = 4, alpha = 0.5, color = "blue") + 
	geom_smooth(method = "lm") + facet_wrap( ~ flux_type) + theme_bw()


all_data_TT %>%
	ungroup() %>% 
	# filter(temperature.x != 20) %>% 
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>%
	filter(flux_type %in% c("gross_photosynthesis", "corrected_respiration_slope")) %>% 
	ggplot(aes(x = flux_type, y = estimate)) + geom_point(size = 4) +
	geom_errorbar(aes(ymin= conf.low, ymax = conf.high), width = 0.1) + theme_bw()



# biovolumes --------------------------------------------------------------


biovols <- read_csv("data-raw/biovols_respirometry_Tetrahelis.csv")

mean_cell_size <- biovols %>% 
	summarise(mean_cell_size = mean(cell_size))

biovol_summary <- biovols %>% 
	mutate(date = str_replace(date, "Feb17", "Feb 17")) %>% 
	mutate(total_biovol = cell_density * 843.6033) %>% 
	group_by(date) %>% 
	summarise(mean_total_biovol = mean(total_biovol)) 

mean_biovolume_feb15 <- biovol_summary$mean_total_biovol[biovol_summary$date == "Feb 15"]
mean_biovolume_feb16 <- biovol_summary$mean_total_biovol[biovol_summary$date == "Feb 16"]
mean_biovolume_feb17 <- biovol_summary$mean_total_biovol[biovol_summary$date == "Feb 17"]
mean_biovolume_feb23 <- biovol_summary$mean_total_biovol[biovol_summary$date == "Feb 23"]
mean_biovolume_feb24 <- biovol_summary$mean_total_biovol[biovol_summary$date == "Feb 24"]
# mean_biovolume_feb23 <- 466813*842.57
## 12 is feb 15
## 16 and 20 are feb 16
## 19, 20, 22, 24 are feb 17


all_data_corr <- all_data_TT %>% 
	mutate(biovolume = NA) %>% 
	mutate(biovolume = ifelse(temperature.x == 12, mean_biovolume_feb15/5, biovolume)) %>%
	mutate(biovolume = ifelse(temperature.x == 16, mean_biovolume_feb16/5, biovolume)) %>%
	mutate(biovolume = ifelse(temperature.x == 20, mean_biovolume_feb16/5, biovolume)) %>%
	mutate(biovolume = ifelse(temperature.x == 19, mean_biovolume_feb17/5, biovolume)) %>%
	mutate(biovolume = ifelse(temperature.x == 22, mean_biovolume_feb17/5, biovolume)) %>%
	mutate(biovolume = ifelse(temperature.x == 24, mean_biovolume_feb17/5, biovolume)) %>% 
	mutate(biovolume = ifelse(temperature.x == 8, mean_biovolume_feb24/5, biovolume)) %>% 
	mutate(biovolume = ifelse(id == 8, mean_biovolume_feb23/5, biovolume)) %>% 
	mutate(gross_photosynthesis_corr = corrected_photosynthesis_slope/biovolume + (-1*corrected_respiration_slope/biovolume)) %>% 
	mutate(respiration_corr = corrected_respiration_slope/biovolume)
	

write_csv(all_data_corr, "data-processed/flux_rates.csv")


# plots and Ea calcs ------------------------------------------------------

all_data_corr <- read_csv("data-processed/flux_rates.csv")

all_data_corr %>% 
	ungroup() %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>%
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_estimate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
	filter(term != "(Intercept)") %>% 
	filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>%
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "repiration_corr", "respiration")) %>% 
	ggplot(aes(x = flux_type, y = estimate)) + geom_point(size = 4) +
	geom_errorbar(aes(ymin= conf.low, ymax = conf.high), width = 0.1) + theme_bw() + ylab("activation energy (Ea)")
	
	all_data_corr %>% 
		ungroup() %>% 
		filter(temperature.x != 19) %>%
		filter(temperature.x != 22) %>%
		gather(key = flux_type, value = rate_estimate, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
		mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
		filter(rate_estimate > 0) %>%
		filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
		mutate(temperature.x = as.numeric(temperature.x)) %>% 
		ungroup() %>% 
		mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
		mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
		mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>%
		filter(flux_type == "gross photosynthesis") %>% 
		ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + geom_abline(slope = 0.32, intercept = -5.4, color = "orange", size = 4) +
		geom_point(size = 4, alpha = 0.0000001, color = "blue") + 
		# geom_smooth(method = "lm") + facet_wrap( ~ flux_type, scales = "free") +
		theme_bw() +
		ylab("oxygen flux (mg O2/L*hr*um3)") +
		xlab("temperature (1/kT)") + theme(text = element_text(size=20)) +
		scale_x_reverse()
		
ggsave("figures/mass_normalized_flux_slopes.png", width = 12, height = 8)




### photosynthesis prediction plot
all_data_corr %>% 
	ungroup() %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>%
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>%
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + geom_abline(slope = 0.32, intercept = -5.4, color = "orange", size = 4) +
	geom_point(size = 4, alpha = 0.0000001, color = "blue") + 
	# geom_smooth(method = "lm") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab("oxygen flux (mg O2/L*hr*um3)") +
	xlab("temperature (1/kT)") + theme(text = element_text(size=24)) +
	scale_x_reverse()

ggsave("figures/photosynthesis-prediction-plot.pdf")

### photosynthesis with data plot
all_data_corr %>% 
	ungroup() %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>%
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>%
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + geom_abline(slope = 0.32, intercept = -5.4, color = "orange", size = 4) +
	geom_point(size = 4, alpha = 0.5, color = "blue") + 
	geom_smooth(method = "lm", size = 2) +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab("oxygen flux (mg O2/L*hr*um3)") +
	xlab("temperature (1/kT)") + theme(text = element_text(size=24)) +
	scale_x_reverse()

ggsave("figures/photosynthesis-data-plot.pdf")


### respiration prediction plot
all_data_corr %>% 
	ungroup() %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>%
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>%
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + 
	geom_abline(slope = 0.65, intercept = 7, color = "orange", size = 4) +
	geom_point(size = 4, alpha = 0.0000000005, color = "blue") + 
	# geom_smooth(method = "lm") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab("oxygen flux (mg O2/L*hr*um3)") +
	xlab("temperature (1/kT)") + theme(text = element_text(size=24)) +
	scale_x_reverse()

ggsave("figures/respiration-prediction-plot.pdf")


### respiration data plot
all_data_corr %>% 
	ungroup() %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>%
	gather(key = flux_type, value = rate_estimate, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) %>%
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + 
	geom_abline(slope = 0.65, intercept = 7, color = "orange", size = 4) +
	geom_point(size = 4, alpha = 0.5, color = "blue") + 
	geom_smooth(method = "lm", size =2, color = "blue") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab("oxygen flux (mg O2/L*hr*um3)") +
	xlab("temperature (1/kT)") + theme(text = element_text(size=24)) +
	scale_x_reverse()

ggsave("figures/respiration-data-plot.pdf")


