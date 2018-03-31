library(tidyverse)
library(cowplot)
library(stringr)
library(broom)
library(extrafont)
loadfonts()

flux_rates_raw <- read_csv("data-processed/flux_rates.csv")
flux_rates <- flux_rates_raw %>% 
	filter(temperature.x != 19) %>%
	filter(temperature.x != 22) %>% 
	mutate(net_photosynthesis = corrected_photosynthesis_slope/biovolume) %>% 
	gather(key = flux_type, value = rate_estimate, net_photosynthesis, gross_photosynthesis, gross_photosynthesis_corr, respiration_corr, contains("corrected")) %>% 
	mutate(rate_estimate = rate_estimate * 3600) %>% 
	mutate(rate_estimate = ifelse(grepl("respiration", flux_type), rate_estimate*-1, rate_estimate)) %>% 
	filter(rate_estimate > 0) %>%
	# filter(flux_type %in% c("gross_photosynthesis_corr", "respiration_corr")) %>% 
	mutate(temperature.x = as.numeric(temperature.x)) %>% 
	ungroup() %>% 
	mutate(flux_type = str_replace(flux_type, "gross_photosynthesis_corr", "gross photosynthesis")) %>% 
	mutate(flux_type = str_replace(flux_type, "respiration_corr", "respiration")) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature.x+273.15)))) 


# tt_mass <- tt %>% 
# 	mutate(cell_biomass_MD = 0.3584378*(cell_volume)^1.088) %>% 
# 	mutate(cell_biomass_R = 0.47*(cell_volume)^0.99) %>% 
# 	mutate(cell_biomass_M = 0.109 *(cell_volume)^0.991) %>% 
# 	mutate(population_biomass_M = cell_biomass_M * cell_density) %>% 
# 	mutate(population_biomass_MD = cell_biomass_MD * cell_density) %>% 
# 	mutate(population_biomass_R = cell_biomass_R * cell_density)

flux2 <- flux_rates %>% 
	mutate(rate_biomassMD = 0.3584378*(rate_estimate)^1.088)%>% 
	mutate(rate_biomassR = 0.47*(rate_estimate)^0.99) %>% 
	mutate(rate_biomassM = 0.109*(rate_estimate)^0.991) %>% 
	mutate(rate_biomassMq = 0.109*(rate_estimate^(3/4))^0.991) %>% 
	mutate(population_biomass = 0.109*(biovolume)^0.991) %>% 
	mutate(mass_quarter = population_biomass^(1/4))


flux2 %>% 
	group_by(temperature.y) %>% 
	distinct(well_id) %>% tally() %>% View

respiration_plot_menden <- flux2 %>% 
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassMD))) + 
	geom_point(size = 4, alpha = 0.2) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

photosynthesis_plot_menden <- flux2 %>% 
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassMD))) + 
	geom_point(size = 4, alpha = 0.2) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

respiration_plot_reynolds <- flux2 %>% 
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassR))) + 
	geom_point(size = 4, alpha = 0.2) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

photosynthesis_plot_reynolds <- flux2 %>% 
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassR))) + 
	geom_point(size = 4, alpha = 0.2) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

respiration_plot_montagnes <- flux2 %>% 
	filter(flux_type == "respiration") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassM))) +
	geom_smooth(method = "lm", size =2, color = "black") +
	geom_point(size = 4, alpha = 0.2) + 
	geom_point(size = 4, shape = 1) + 
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

photosynthesis_plot_montagnes <- flux2 %>% 
	filter(flux_type == "gross photosynthesis") %>% 
	ggplot(aes(x = inverse_temp, y = log(rate_biomassM))) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	geom_point(size = 4, alpha = 0.2) + 
	geom_point(size = 4, shape = 1) + 
	# facet_wrap( ~ flux_type, scales = "free") +
	theme_bw() +
	ylab(bquote('ln oyxygen flux ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(), axis.line = element_line(colour = "black")) +
	theme(text = element_text(size=15, family = "Helvetica")) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") + ggtitle("Temperature (°C)") +
	theme(plot.title = element_text(hjust = 0.5, size = 15)) +
	geom_point(size = 4, shape = 1, color = "black") 

fig1_montagnes <- plot_grid(photosynthesis_plot_montagnes, respiration_plot_montagnes, labels = c("A) Photosynthesis", "B) Respiration"), label_fontface = "plain", ncol = 2, nrow = 1, label_x = 0, hjust = 0)
save_plot("figures/k-temp-figure1-montagnes.pdf", fig1_montagnes, nrow = 1, ncol = 2, base_height = 4, base_width = 4.4)



fig1_biomass <- plot_grid(photosynthesis_plot_reynolds, respiration_plot_reynolds,photosynthesis_plot_menden, respiration_plot_menden,photosynthesis_plot_montagnes, respiration_plot_montagnes, labels = c("A) Photosynthesis", "B) Respiration"), label_fontface = "plain", ncol = 2, nrow = 3, label_x = 0, hjust = 0)

save_plot("figures/k-temp-figure1-biomass.pdf", fig1_biomass, nrow = 3, ncol = 2, base_height = 5, base_width = 5.5)

flux2 %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_biomassMD) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

flux2 %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type == "respiration") %>% 
lm(log(rate_biomassR) ~ inverse_temp, data = .) %>% summary()


flux2 %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_biomassM*((population_biomass)^(1/4))) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

flux2 %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_biomassM) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

flux2 %>% 
	filter(rate_estimate > 0) %>% 
	group_by(flux_type) %>% 
	do(tidy(lm(log(rate_biomassM) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

	flux2 %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type == "gross photosynthesis") %>% 
	lm(log(rate_biomassM) ~ inverse_temp, data = .) %>% summary()

flux2 %>% 
	filter(rate_estimate > 0) %>% 
	filter(flux_type == "respiration") %>% 
	lm(log(rate_biomassM) ~ inverse_temp, data = .) %>% summary()


# CUE ---------------------------------------------------------------------

CUE <- flux2 %>% 
	select(id, well_id, temperature.y, rate_biomassM, flux_type, inverse_temp) %>% 
	spread(key = flux_type, value = rate_biomassM) %>% 
	mutate(CUE = 1- (respiration/`gross photosynthesis`)) %>% 
	mutate(ratio = `gross photosynthesis`/respiration) %>% 
	mutate(log_ratio = log(`gross photosynthesis`)/log(respiration))


CUE %>% 
	ggplot(aes(x = inverse_temp, y = CUE)) +
	geom_point(size = 4, alpha = 0.5) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	ylab("Carbon use efficiency (1-R/P)") +
	geom_point(size = 4, shape = 1, color = "black") +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") +
	theme(text = element_text(size=12, family = "Arial")) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
	theme(plot.title = element_text(hjust = 0.5, size = 14)) +
	theme_bw() +
	theme(text = element_text(size=12, family = "Arial"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_rect(colour = "black", size=0.5),
				plot.title = element_text(hjust = 0.5, size = 12)) +
	ggtitle("Temperature (°C)") 
ggsave("figures/CUE.pdf", width = 5, height = 4)
	

CUE %>% 
	filter(ratio < 40) %>% 
	ggplot(aes(x = inverse_temp, y = log(ratio))) +
	geom_point(size = 4, alpha = 0.5) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	ylab(bquote('Ln GP/R ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") +
	theme(text = element_text(size=12, family = "Arial")) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
	theme(plot.title = element_text(hjust = 0.5, size = 14)) +
	theme_bw() +
	theme(text = element_text(size=12, family = "Arial"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_rect(colour = "black", size=0.5),
				plot.title = element_text(hjust = 0.5, size = 12)) +
	ggtitle("Temperature (°C)") +
	geom_point(size = 4, shape = 1, color = "black") 
ggsave("figures/GP_to_R.pdf", width = 5, height = 4)



CUE %>% 
	filter(ratio < 40) %>% 
	ggplot(aes(x = inverse_temp, y = ratio)) +
	geom_point(size = 4, alpha = 0.5) + 
	geom_smooth(method = "lm", size =2, color = "black") +
	ylab(bquote('Ln GP/R ('*mg ~O[2]*' ug '*C^-1~hr^-1*')')) +
	scale_x_reverse(sec.axis = sec_axis(~((1/(.*8.62 * 10^(-5)))-273.15))) + xlab("Temperature (1/kT)") +
	theme(text = element_text(size=12, family = "Arial")) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
	theme(plot.title = element_text(hjust = 0.5, size = 14)) +
	theme_bw() +
	theme(text = element_text(size=12, family = "Arial"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_rect(colour = "black", size=0.5),
				plot.title = element_text(hjust = 0.5, size = 12)) +
	ggtitle("Temperature (°C)") +
	geom_point(size = 4, shape = 1, color = "black") 
ggsave("figures/GP_to_R.pdf", width = 5, height = 4)


	CUE %>% 
do(tidy(lm(log(ratio) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
		CUE %>% 
		lm(log(ratio) ~ inverse_temp, data = .) %>% summary()

	CUE %>% 
		lm(ratio ~ inverse_temp, data = .) %>% summary()
	
	
CUE %>% 
	filter(ratio < 40) %>% 
	do(tidy(lm(ratio ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

CUE %>% 
	do(tidy(lm(ratio ~ temperature.y, data = .), conf.int = TRUE)) %>% View



# net photosynthesis ------------------------------------------------------

unique(flux_rates$flux_type)

flux_rates %>% 
	filter(flux_type == "gross_photosynthesis")
