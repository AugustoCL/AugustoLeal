# install required packages
install.packages("GetTDData", "tidyverse", "extrafont", "ggrepel", "ggthemes") # run if you dont have installed before

# load packages and font
library(tidyverse)
library(GetTDData)
library(extrafont)
font_import()
loadfonts(device = "win")
custom_font <- "Gadugi"

# download data 
download.TD.data(asset.codes = "NTN-B") 
NTNB <- read.TD.files(asset.codes = "NTN-B")

# filter bonds by maturity
venc_ntnbs <- as.Date(c("2020-08-15", "2024-08-15", "2035-05-15", "2050-08-15"))

# wrangle the data
NTNB %>% 
  filter(matur.date %in% venc_ntnbs, # filter bonds by maturity 
         ref.date > as.Date("2019-08-01"), # filter by date of negociation
         !str_detect(asset.code,"Principal")) %>% # exclude zero coupons bonds 
  mutate(taxa = yield.bid*100,
         NTN_B = format(matur.date, "%Y/%m"),
         label = if_else(ref.date == min(ref.date),
                         format(matur.date, "%Y"),
                         NA_character_)) %>%
  arrange(NTN_B,desc(ref.date), .by_group = TRUE) %>%
  select(-c(yield.bid, asset.code, price.bid)) %>% 
  
  # configure the plot
  ggplot(aes(x = ref.date,y = taxa, color = NTN_B)) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = as.Date("2020-03-10"), 
             linetype = "dashed", color = "darkgrey", size = .5) +
  geom_vline(xintercept = as.Date("2020-04-23"), 
             linetype = "dashed", color = "darkgrey", size = .5) +
  geom_label(aes(x = as.Date("2020-03-15"), y = 1, label = "Moro Day"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = .25, 
             family= custom_font, 
             size = 3) + 
  geom_curve(aes(x = as.Date("2020-04-01"), y = 1.2, 
                 xend = as.Date("2020-04-21"), yend = 1.7), 
             colour = "#555555", 
             size= 0.6, 
             curvature = -0.3,
             arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label(aes(x = as.Date("2020-01-16"), y = 5, label = "Corona Day"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = .25, 
             family= "Gadugi", 
             size = 3) + 
  geom_curve(aes(x = as.Date("2020-02-11"), y = 5.2, 
                 xend = as.Date("2020-03-08"), yend = 5.7), 
             colour = "#555555", 
             size= 0.6, 
             curvature = -0.4,
             arrow = arrow(length = unit(0.02, "npc"))) +
  ggrepel::geom_label_repel(aes(label = label),
                   na.rm = TRUE,
                   inherit.aes = TRUE,
                   family = custom_font) +
  ggtitle("NTN-B - Juros Semestrais") + 
  xlab(NULL)+ ylab("Taxa % a.a") + 
  labs(caption = "Dados: Tesouro Direto\nElaboração: @AugustoCL") +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x),
                     breaks = seq(0,6,by = 1)) +
  ggthemes::theme_clean(base_family = custom_font) +
  theme(legend.position = "none", 
        plot.title =  element_text(hjust = 0.5),
        plot.background = element_rect(color = "white")) +
  
  # save the plot
  ggsave(filename = "ntnb_plot.png",width = 6.4, height = 4.8)

##### code created by @AugustoCL