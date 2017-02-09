## looking at photosynthesis data!!



# load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)


# read in data ------------------------------------------------------------

data_raw <- read_csv("data-raw/Phyto_resp_scenedesmus_feb8_2017_Oxygen.csv")


data <- clean_names(data_raw)

data <- data %>% 
	mutate(date_time = dmy_hms(date_time))

ggplot(data = data, aes(x = date_time, y = t_internal_c)) + geom_point()
	
data_pro <-  data %>% 
	select(-c2) %>% 
	gather(key = well_id, value = oxygen, 3:25) %>%
	mutate(treatment = ifelse(well_id %in% c("a2", "a4", "a6", "b1", "b3", "b5", "c1", "c3", "c5", "c6", "d1", "d4", "d6"), "phyto", "control")) %>%
		mutate(rate_type = ifelse(time_min < 38.88, "photosynthesis", "respiration"))


data_resp <- data_resp[data_resp$date_time > "2017-02-08 11:08:37"]	

data_pro %>% 
	filter(rate_type == "photosynthesis") %>% 
	filter(treatment == "phyto") %>% 
	ggplot(aes(x = date_time, y = oxygen)) + geom_point() +
	facet_wrap( ~ well_id) + geom_smooth(method = lm)

?gather

