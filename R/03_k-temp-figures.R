

library(tidyverse)
library(cowplot)
library(stringr)


flux_rates_raw <- read_csv("data-processed/flux_rates.csv")
flux_rates <- flux_rates_raw %>% 
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
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15))))

### figure 1 for k-temp paper


respiration_plot <- flux_rates %>% 
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + 
	geom_point(size = 4, alpha = 0.5) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]~ um^-3~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=14, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 14)) +
	geom_point(size = 4, shape = 1, color = "black") 


photosynthesis_plot <- flux_rates %>% 
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_estimate))) + 
	geom_point(size = 4, alpha = 0.5) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]~ um^-3~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=14, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 14)) +
	geom_point(size = 4, shape = 1, color = "black") 


figure1 <- plot_grid(photosynthesis_plot, respiration_plot, labels = c("A) Photosynthesis", "B) Respiration"), ncol = 2, nrow = 1, label_x = 0, hjust = 0)


save_plot("figures/k-temp-figure1.png", figure1, nrow = 1, ncol = 2, base_height = 4.5, base_width = 5)
