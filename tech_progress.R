library(tidyverse)
library(ggplot2)
source("theme_simple.R")
library(ggrepel)
library(janitor)
library(systemfonts)

# potential technologies
# Price of solar energy 1/$
# Light intensity W/cm^2
# Telecommunications speed: bits km/s
# Slow light pulse decay: pico seconds
# Particle energy in particle accelerator
# Price of pocket calculator
# Laser focused intensity: W/cm^2
# Annual CO2 emissions: tons per capita
# Number of written words per year / on the internet
# Lines of code per year

# AI Stuff:
# Machine learning parameter counts over time: number
# Dataset size: tokens
# Effective compute: flop
# AI spending
# GPU price performance



ai <- read_csv("data/ai-compute.csv") %>% 
        clean_names() %>% 
        #filter(year > 2000) %>% 
        rename(flops = training_computation_petaflop) %>% 
        filter(!is.na(flops)) %>% 
        group_by(year) %>% 
        filter(flops == max(flops, na.rm=TRUE)) %>% 
        #summarise(pflops = max(Training_computation_petaflop, na.rm=TRUE)) %>% 
        ungroup() %>% 
        #mutate(prop_ai = flops / min(flops,na.rm=TRUE)) %>% 
        select(entity, year, flops) %>% 
        arrange(year) # %>% 
       # mutate(flops = (flops - mean(flops)) / sd(flops)) 
ai

dna <- read_csv("data/sequencing-cost.csv") %>%
        clean_names() %>%
        mutate(mb_per_dollar = 1/cost_per_mb) %>%
        #mutate(prop_dna = mb_per_dollar / min(mb_per_dollar)) %>%
        select(year, mb_per_dollar)
dna

moore <- read_csv("data/transistors-per-microprocessor.csv") %>%
                clean_names() %>%
                #filter(Year > 2000) %>% 
                rename(trans = transistors_per_microprocessor) %>% 
                #mutate(prop_trans = trans / min(trans)) %>% 
                select(year, trans)

cost <- read_csv("data/costs-of-66-different-technologies-over-time.csv") %>% 
                clean_names() %>% 
                filter(entity %in% c("DRAM", "Laser Diode", "Photovoltaics")) %>% 
                select(-code) %>% 
                mutate(technology_cost = 1/technology_cost) %>%  # thing per dollar
                pivot_wider(id_cols = year, names_from = entity, 
                                values_from = technology_cost) %>% 
                clean_names()

all <- ai %>% 
        full_join(dna, by = "year") %>% 
        full_join(moore, by = "year") %>% 
        full_join(cost, by = "year") %>%
        pivot_longer(cols = flops:photovoltaics) %>% 
        filter(!is.na(value))
factor(all$name)

p <- ggplot(all, aes(year, value, color = name)) + 
        geom_point(size = 3, alpha = 0.5) +
        geom_smooth(se=FALSE) +
        #geom_label(all %>% filter(name == "flops")) +
        # make labels only for AI
        #geom_label(aes(label = ifelse(name == 'flops', NA, entity))) +
        # geom_label_repel(aes(label = Entity),
        #                  box.padding   = 0.35, 
        #                  point.padding = 0.5,
        #                  segment.color = 'grey50') +
        theme_simple(axis_lines = TRUE, grid_lines = FALSE) +
        scale_color_viridis_d("",  option = "D", labels = c(
                                                    "DRAM:\nMb per USD",
                                                    "AI:\nTraining FLOPS\nper model",
                                                    "Laser Diode:\nW/cm^2 per USD",
                                                    "DNA:\nMb sequence\nper USD",
                                                    "Photovoltaics:\nW/cm^2 per USD",
                                                    "Hardware:\nTransistors\nper microchip"
                                                    )) +
        #scale_y_continuous(breaks = c(1e+02, 1e+05, 1e+08), labels = c("100", "10,000", "100,000,000")) +
        scale_y_log10() +
        # start from 1970
        scale_x_continuous(breaks = seq(1970, 2020, 5), limits = c(1970, 2020)) +
        theme(legend.position = "top",
              legend.title=element_blank()) 
p
ggsave("figs/tech2.jpg", width = 5, height = 5)     