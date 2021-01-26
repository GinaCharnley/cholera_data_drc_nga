# Sub-national cholera data for Nigeria and DRC 
# Data cleaning and summary plots 

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(sf)
library(viridis)

nga <- read_excel("Library/Mobile Documents/com~apple~Preview/Documents/PhD/Chapter 3 - Drought:Conflict:Cholera, DRC&NGA/Data/Cholera/cholera_plots.xlsx", 
                    +     sheet = "NGA")
cod <- read_excel("Library/Mobile Documents/com~apple~Preview/Documents/PhD/Chapter 3 - Drought:Conflict:Cholera, DRC&NGA/Data/Cholera/cholera_plots.xlsx", 
                    +     sheet = "COD")

# Fig. 1
# NGA
nga_plots <- nga[c(2,3,4,6,7)]
date <- nga_plots$Date
df_date <- data.frame(date = date, year = as.numeric(format(date, format = "%Y")),
                      month = as.numeric(format(date, format = "%m")),
                      day = as.numeric(format(date, format = "%d")))
nga_plots <- cbind(nga_plots, df_date)
nga_plots <- nga_plots[c(2,3,4,5,8,9)]
nga_cases <- nga_plots[c(1,3,5,6)]
nga_deaths <- nga_plots[c(1,4,5,6)]
nga_cases <- na.omit(nga_cases)
nga_cases <- nga_cases %>% group_by(month, Year) %>% mutate(monthly_cases = sum(Cases))
nga_cases <- nga_cases[-c(2,4)]
nga_cases <- nga_cases %>% distinct()
nga_cases$Date <- as.yearmon(paste(nga_cases$Year, nga_cases$month), "%Y %m")
nga_cases_plot <- ggplot(nga_cases, aes(x = Date, y = monthly_cases)) + geom_line(color = "#A3319F") + 
  theme_bw() + labs(y = "Monthly Cases", title = "NGA, cases")
nga_deaths <- na.omit(nga_deaths)
nga_deaths <- nga_deaths %>% group_by(month, Year) %>% mutate(monthly_deaths = sum(Deaths))
nga_deaths <- nga_deaths[-c(2,4)]
nga_deaths <- nga_deaths %>% distinct()
nga_deaths$Date <- as.yearmon(paste(nga_deaths$Year, nga_deaths$month), "%Y %m")
nga_deaths_plot <- ggplot(nga_deaths, aes(x = Date, y = monthly_deaths)) + geom_line(color = "#DF488D") + 
  theme_bw() + labs(y = "Monthly Deaths", title = "NGA, deaths")
# COD
cod_plots <- cod[c(2,3,4,7,8)]
date <- cod_plots$Date
df_date <- data.frame(date = date, year = as.numeric(format(date, format = "%Y")),
                      month = as.numeric(format(date, format = "%m")),
                      day = as.numeric(format(date, format = "%d")))
cod_plots <- cbind(cod_plots, df_date)
cod_plots <- cod_plots[c(2,3,4,7,8)]
cod_cases <- cod_plots[c(1,2,4,5)]
cod_deaths <- cod_plots[c(1,3,4,5)]
cod_cases <- na.omit(cod_cases)
cod_cases <- cod_cases %>% group_by(month, Year) %>% mutate(monthly_cases = sum(Cases))
cod_cases <- cod_cases[-c(2,4)]
cod_cases <- cod_cases %>% distinct()
cod_cases$Date <- as.yearmon(paste(cod_cases$Year, cod_cases$month), "%Y %m")
cod_cases_plot <- ggplot(cod_cases, aes(x = Date, y = monthly_cases)) + geom_line(color = "#FA7876") + 
  theme_bw() + labs(y = "Monthly Cases", title = "COD, cases")
cod_deaths <- na.omit(cod_deaths)
cod_deaths <- cod_deaths %>% group_by(month, Year) %>% mutate(monthly_deaths = sum(Deaths))
cod_deaths <- cod_deaths[-c(2,4)]
cod_deaths <- cod_deaths %>% distinct()
cod_deaths$Date <- as.yearmon(paste(cod_deaths$Year, cod_deaths$month), "%Y %m")
cod_deaths_plot <- ggplot(cod_deaths, aes(x = Date, y = monthly_deaths)) + geom_line(color = "#F3B584") + 
  theme_bw() + labs(y = "Monthly Deaths", title = "COD, deaths")

ggarrange(nga_cases_plot, nga_deaths_plot, cod_cases_plot, cod_deaths_plot, nrow = 2, ncol = 2)

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



