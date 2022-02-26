# Sub-national cholera data for Nigeria and DRC 
# Data cleaning and summary plots 

library(dplyr)
library(ggplot2)
library(zoo)
library(sf)
library(viridis)
library(ggpubr)

# Fig. 1
# NGA
nga <- read.csv("NGA-Table 1.csv")
nga <- nga[c(2,3,4,6,7)]
Date2 <- data.frame(strsplit(nga$Date, " +"))
Date2 <- data.frame(t(Date2))
rownames(Date2) <- NULL
nga <- cbind(nga, Date2)
nga <- nga %>% group_by(Year, month) %>% mutate(monthly_cases = sum(Cases, na.rm = TRUE))
nga <- nga %>% group_by(Year, month) %>% mutate(monthly_deaths = sum(Deaths, na.rm = TRUE))
nga <- nga[-c(2,3)]
nga <- nga %>% distinct()
nga$month <- match(nga$month, month.abb)
nga$Date <- as.yearmon(paste(nga$Year, nga$month), "%Y %m")
nga <- nga %>% gather(C_D, n, 4:5)
nga$C_D[nga$C_D == "monthly_cases"] <- "Cases"
nga$C_D[nga$C_D == "monthly_deaths"] <- "Deaths"
p1 <- ggplot(nga, aes(x=Date, y=n, color = C_D, fill= C_D)) + 
  geom_bar(stat="identity", position = "stack") + scale_color_manual(values = c("#952C80FF", "#FD9A6AFF")) + 
  scale_fill_manual(values = c("#952C80FF", "#FD9A6AFF")) + 
  theme_bw() + labs(color = "NGA", fill = "NGA", y = "Monthly Cases/Deaths") + 
  theme(legend.position = "top")

# COD
cod <- read.csv("COD-Table 1.csv")
cod <- cod[c(2,3,4,7,8)]

Date2 <- data.frame(strsplit(cod$Date, " +"))
Date2 <- data.frame(t(Date2))
rownames(Date2) <- NULL
cod <- cbind(cod, Date2)
cod$month <- match(cod$month, month.abb)
cod <- cod %>% group_by(Year, month) %>% mutate(monthly_cases = sum(Cases, na.rm = TRUE))
cod <- cod %>% group_by(Year, month) %>% mutate(monthly_deaths = sum(Deaths, na.rm = TRUE))
cod <- cod[-c(4,5)]
cod <- cod %>% distinct()
cod$Date <- as.yearmon(paste(cod$Year, cod$month), "%Y %m")
cod <- cod %>% gather(C_D, n, 3:4)
cod$C_D[cod$C_D == "monthly_cases"] <- "Cases"
cod$C_D[cod$C_D == "monthly_deaths"] <- "Deaths"
p2 <- ggplot(cod, aes(x=Date, y=n, color = C_D, fill= C_D)) + 
  geom_bar(stat="identity", position = "stack") + scale_color_manual(values = c("#952C80FF", "#FD9A6AFF")) + 
  scale_fill_manual(values = c("#952C80FF", "#FD9A6AFF")) + 
  theme_bw() + labs(color = "COD", fill = "COD", y = "Monthly Cases/Deaths") + 
  theme(legend.position = "top")

ggarrange(p1,p2, nrow = 1)

# Fig. 2
# Shapefiles were taken from:
  # COD: https://data.humdata.org/dataset/drc-administrative-boundaries-levels-0-2
  # NGA: https://datacatalog.worldbank.org/dataset/nigeria-administrative-boundaries-2017
# NGA
nga_shp <- st_read("nga_admbnda_adm1_osgof_20161215.shp", stringsAsFactors = FALSE)
nga_map_cases <- nga_plots[c(3,4)]
nga_map_deaths <- nga_plots[c(3,5)]
names(nga_map_cases)[1]<-paste("admin1Name")
names(nga_map_deaths)[1]<-paste("admin1Name")
nga_map_cases <- na.omit(nga_map_cases)
nga_map_deaths <- na.omit(nga_map_deaths)
sum(nga_map_cases$Cases)
375891
nga_map_cases <- nga_map_cases %>% group_by(admin1Name) %>% mutate(state_cases = sum(Cases))
nga_map_cases <- nga_map_cases[-c(2)]
nga_map_cases <- nga_map_cases %>% distinct()
nga_map_cases <- nga_map_cases %>% mutate(perc_state_cases = ((state_cases/375891)*100))
nga_shp <- merge(nga_shp, nga_map_cases, by = "admin1Name", all = TRUE)
nga_map_cases_plot <- ggplot(nga_shp) + geom_sf(aes(fill=perc_state_cases), lwd = 0) + 
  labs(fill = "State Cases (% Total Cases)") + scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme_void() + geom_sf_text(aes(label = admin1Name), size = 3, check_overlap = TRUE, color = "#EA4F88")
sum(nga_map_deaths$Deaths)
10181
nga_map_deaths <- nga_map_deaths %>% group_by(admin1Name) %>% mutate(state_deaths = sum(Deaths))
nga_map_deaths <- nga_map_deaths[-c(2)]
nga_map_deaths <- nga_map_deaths %>% distinct()
nga_map_deaths <- nga_map_deaths %>% mutate(perc_state_deaths = ((state_deaths/10181)*100))
nga_shp <- merge(nga_shp, nga_map_deaths, by = "admin1Name", all = TRUE)
nga_map_deaths_plot <- ggplot(nga_shp) + geom_sf(aes(fill=perc_state_deaths), lwd = 0) + 
  labs(fill = "State Deaths (% Total Deaths)") + scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme_void() + geom_sf_text(aes(label = admin1Name), size = 3, check_overlap = TRUE, color = "#EA4F88")

# COD
cod_shp <- st_read("cod_admbnda_adm1_rgc_itos_20190911.shp", stringsAsFactors = FALSE)
cod_map_cases <- cod_plots[c(3,4)]
cod_map_deaths <- cod_plots[c(3,5)]
names(cod_map_cases)[1]<-paste("ADM1_FR")
names(cod_map_deaths)[1]<-paste("ADM1_FR")
cod_map_cases <- na.omit(cod_map_cases)
cod_map_deaths <- na.omit(cod_map_deaths)
sum(cod_map_cases$Cases)
398115
cod_map_cases <- cod_map_cases %>% group_by(ADM1_FR) %>% mutate(province_cases = sum(Cases))
cod_map_cases <- cod_map_cases[-c(2)]
cod_map_cases <- cod_map_cases %>% distinct()
cod_map_cases <- cod_map_cases %>% mutate(perc_province_cases = ((province_cases/398115)*100))
cod_shp <- merge(cod_shp, cod_map_cases, by = "ADM1_FR", all = TRUE)
cod_shp <- cod_shp[-c(2,15,29),]
cod_map_cases_plot <- ggplot(cod_shp) + geom_sf(aes(fill=perc_province_cases), lwd = 0) + 
  labs(fill = "Province Cases (% Total Cases)") + scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme_void() + geom_sf_text(aes(label = ADM1_FR), size = 3, check_overlap = TRUE, color = "#EA4F88")
sum(cod_map_deaths$Deaths)
12498
cod_map_deaths <- cod_map_deaths %>% group_by(ADM1_FR) %>% mutate(province_deaths = sum(Deaths))
cod_map_deaths <- cod_map_deaths[-c(2)]
cod_map_deaths <- cod_map_deaths %>% distinct()
cod_map_deaths <- cod_map_deaths %>% mutate(perc_province_deaths = ((province_deaths/12498)*100))
cod_shp <- merge(cod_shp, cod_map_deaths, by = "ADM1_FR", all = TRUE)
cod_shp <- cod_shp[-c(2,15,29),]
cod_map_deaths_plot <- ggplot(cod_shp) + geom_sf(aes(fill=perc_province_deaths), lwd = 0) + 
  labs(fill = "Province Deaths (% Total Deaths)") + scale_fill_viridis_c(option = "magma",direction = -1) + 
  theme_void() + geom_sf_text(aes(label = ADM1_FR), size = 3, check_overlap = TRUE, color = "#EA4F88")

ggarrange(nga_map_cases_plot, nga_map_deaths_plot, cod_map_cases_plot, cod_map_deaths_plot, nrow = 2, ncol = 2)

