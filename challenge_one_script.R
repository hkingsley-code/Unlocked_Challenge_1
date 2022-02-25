library(ggplot2)
library(dplyr)
library(cowplot)
library(rworldmap)

energy <- read.csv("energy_use_data_11-29-2021.csv")
fire <- read.csv("fires_data_11-29-2021.csv")
land <- read.csv("land_cover_data_11-30-2021.csv")
temp <- read.csv("temperature_change_data_11-29-2021.csv")
waste <- read.csv("waste_disposal_data_11-29-2021.csv")


# step one create 2019 fire map just to get some code progress - make it plotly interactive over years for bonus points 
map <- joinCountryData2Map(fire%>%filter(Year == 2019, Item == 'Grassland') , joinCode = "ISO3", nameJoinColumn = "Area.Code..ISO3." )
mapCountryData(map, nameColumnToPlot =  "Value")

# step 2 join all tables -- atempt to do it in widest possible format - one row per country per year 
fire_wide <- tidyr::pivot_wider(fire,id_cols = c("Area.Code..ISO3.", "Year"),
                                names_from = "Item" ,values_from = "Value", names_prefix = "Fire_"  )
fire_wide$Fire_Total <- rowSums(fire_wide%>%select(starts_with("Fire")), na.rm = T)
fire_wide[is.na(fire_wide)] <- 0

energy_wide <- tidyr::pivot_wider(energy,id_cols = c("Area.Code..ISO3.", "Year"),
                                               names_from = "Item" ,values_from = "Value", names_prefix = "Energy_"  )
energy_wide$Energy_Total <- rowSums(energy_wide%>%select(starts_with("Energy")), na.rm = T)
energy_wide[is.na(energy_wide)] <- 0


land_wide <- tidyr::pivot_wider(land,id_cols = c("Area.Code..ISO3.", "Year"),
                                  names_from = "Item" ,values_from = "Value", names_prefix = "Land_"  )
land_wide$Land_Total <- rowSums(land_wide%>%select(starts_with("Land")), na.rm = T)
land_wide[is.na(land_wide)] <- 0


temp_wide <- temp%>%filter(Months == "Meteorological year")%>%select(Area.Code..ISO3., Year, Temp_Change = Value )


waste_wide <- waste%>%select(Area.Code..ISO3., Year, Waste_Burn = Value)


all_data <- fire_wide%>%
  left_join(energy_wide, by = c(c("Area.Code..ISO3.", "Year")))%>%
  left_join(land_wide, by = c(c("Area.Code..ISO3.", "Year")))%>%
  left_join(temp_wide, by = c(c("Area.Code..ISO3.", "Year")))%>%
  left_join(waste_wide, by = c(c("Area.Code..ISO3.", "Year")))
  
  
# step 3 create model to predict fire area per country 

## poked around with this and its clear that there's not a smoking gun here for prediction purposes. 
## likly past fire predicts future fire. drought and wind data would be powerful. There is work that could be done here 

reg <- lm(data = all_data, Fire_Total ~ Energy_Total + Land_Total + Waste_Burn + Temp_Change)

reg <- lm(data = all_data, Fire_Total ~ Energy_Total + Land_Total + Waste_Burn + Temp_Change)

# step 4 create visual of 2019 fire risk vs 2019 actual fires 





### two new ideas. One can we correlate change in covered area form  year x to 2019 with fire increase from x to 2019 


change_tree_area <- all_data%>%
  filter(Year %in% c(2001, 2018))%>%
  tidyr::pivot_wider(id_cols = "Area.Code..ISO3.", names_from = Year, values_from = c("Land_Tree-covered areas", "Fire_Total"))%>%
  mutate(land_change = `Land_Tree-covered areas_2018` - `Land_Tree-covered areas_2001`,
         fire_change = `Fire_Total_2018` - `Fire_Total_2001`, 
         land_change_percent = (land_change / `Land_Tree-covered areas_2001`) * 100,
         land_change_percent = ifelse(land_change_percent %in% c(Inf, NA, NaN), 0, land_change_percent)
         
         )

avg_fire <- all_data%>%
  filter(Year >= 2001, Year <=2018)%>%
  group_by(Area.Code..ISO3. )%>%
  summarise('Avg Area Burned (in Hectares)' = mean(Fire_Total, na.rm =T))


map <- joinCountryData2Map(change_tree_area , joinCode = "ISO3", nameJoinColumn = "Area.Code..ISO3." )
mapCountryData(map, nameColumnToPlot =  "land_change_percent",
               mapTitle = "Percent Change in Tree Covered Land (color) vs  \n Average Total Land Area Burned by Fires (circles) \n 2001-2018",
               oceanCol =  'lightblue',
               colourPalette = RColorBrewer::brewer.pal(7, "RdYlGn")
               
               )
map <- joinCountryData2Map(avg_fire , joinCode = "ISO3", nameJoinColumn = "Area.Code..ISO3." )

mapBubbles(map, nameZSize = "Avg Area Burned (in Hectares)",
           plotZeroVals = TRUE,
           oceanCol = 'lightblue',
           landCol = "wheat",add = TRUE
    
           )



## final descison is to do plotly of tree areas over time




### just plotly image of fires throughout years 
## maybe some visual with fires by year of top countries 



## average temp change or sum temp change for each country 






