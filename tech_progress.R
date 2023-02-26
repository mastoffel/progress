library(tidyverse)
library(ggplot2)
source("theme_simple.R")
library(ggrepel)

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
        filter(year > 2000) %>% 
        rename(flops = Training_computation_petaflop) %>% 
        filter(!is.na(flops)) %>% 
        group_by(year) %>% 
        filter(flops == max(flops, na.rm=TRUE)) %>% 
        #summarise(pflops = max(Training_computation_petaflop, na.rm=TRUE)) %>% 
        ungroup() %>% 
        mutate(prop_ai = flops / min(flops,na.rm=TRUE)) %>% 
        rename(Year = year) %>% 
        select(Entity, Year, prop_ai)
ai

dna <- read_csv("data/sequencing-cost.csv") %>% 
        mutate(mb_per_dollar = 1/cost_per_mb) %>% 
        mutate(prop_dna = mb_per_dollar / min(mb_per_dollar)) %>% 
        select(Year, prop_dna)

moore <- read_csv("data/transistors-per-microprocessor.csv") %>% 
                filter(Year > 2000) %>% 
                rename(trans = `Transistors per microprocessor`) %>% 
                mutate(prop_trans = trans / min(trans)) %>% 
                select(Year, prop_trans)

all <- ai %>% 
        left_join(dna) %>% 
        left_join(moore) %>% 
        pivot_longer(cols = starts_with("prop")) %>% 
        mutate(Entity = ifelse(name == "prop_ai", Entity, NA))

p <- ggplot(all, aes(Year, value, color = name)) + 
        geom_point(size = 3, alpha = 0.5) +
        geom_smooth(se=FALSE) +
        # geom_label_repel(aes(label = Entity),
        #                  box.padding   = 0.35, 
        #                  point.padding = 0.5,
        #                  segment.color = 'grey50') +
        theme_simple(axis_lines = TRUE, grid_lines = FALSE) +
        scale_color_viridis_d("",  option = "D", labels = c("AI:\nTraining FLOPS\nper model", 
                                                    "DNA:\nMb sequence\nper USD",
                                                    "Hardware:\nTransistors\nper microchip")) +
        #scale_y_continuous(breaks = c(1e+02, 1e+05, 1e+08), labels = c("100", "10,000", "100,000,000")) +
        scale_y_log10("x-times increase relative to 2001",
                      breaks = c(1e+02, 1e+05, 1e+08, 1e+11), 
                      labels = c("100", "10,000", "100,000,000", "100,000,000,000")) +
        theme(legend.position = "top",
              legend.title=element_blank()) 
p
ggsave("figs/tech.jpg", width = 5.3, height = 4.1)        
        

