rm(list= ls())
graphics.off()

library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(randomForest)
library(pdp)

#setwd("C:/Users/malit/Box/2.Classes/Geog_563_Analytical_workflow/project/Data")
#just run this without setting up  working directory
fire <- read.csv("Selected_fire_information.csv")  # This is the fire data, fire locations and names 



#Abiotic covariates>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cov <- read.csv("Fire_data.csv")  #this is the abiotic covariates of the fire
unique(cov$Name)
cov <- cov[!apply(cov, 1, function(row) all(is.na(row) | row == "")), ]  #removing the because NA bugs the  spatial objects in the later steps
cov <- cov %>% mutate(cov_id = row_number())  #getting some sort of an id


unique_fire_ids <- unique(fire$FIRE_ID) # we wanted to extract the 2024 fires from fire data frame

correct_abiotic_cov <- cov %>%
  filter(Name %in% unique_fire_ids) #cov has the fire points in oregon, we want extract the fire points abotic covs


#Biotic covariates>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
Biotic_cov <- read.csv("ZonalStats_AFG_PFG_SHR_TRE_ByFireArea_2021_2024.csv") #biotic covariates

#OBJECTID_1 and FIRE_ID are the same in the two frames. since fire df has the xy coordinates these two needs to be joined
Biotic_cov_with_xy <- Biotic_cov %>%
  left_join(fire %>% select(FIRE_ID, x, y), by = c("OBJECTID_1" = "FIRE_ID"))

Biotic_cov_with_xy <- Biotic_cov_with_xy[, c(2:7, 17,18)] #extracting the necessary columns



############################################################################################################################
###############extracting the info we need to make maps
###############################just making maps################################################################

cov_sub <- correct_abiotic_cov %>%
  select(id = cov_id, x, y) %>%
  mutate(type = "cov")  #labelling the rows that we need for later process. covariate table as "cov"

fire_sub <- fire %>%
  select(id = FIRE_ID, x, y) %>%
  mutate(type = "fire")  #fire data frame as "fire"



full_xy <- bind_rows(cov_sub, fire_sub)  
full_xy_sf <- st_as_sf(full_xy, coords = c("x", "y"), crs = 4326)  #for the maps we need to make our df to spatial objects. st_as_sf is in sf lib
                                                                  


utm_crs <- 2991  #region's EPSG
full_xy_sf_proj <- st_transform(full_xy_sf, crs = utm_crs) #trasnforming the right coordinates

#partitioning the sf object for fire and covariates
fire_sf <- full_xy_sf_proj %>% filter(type == "fire")  
cov_sf  <- full_xy_sf_proj %>% filter(type == "cov")

#maps
mapview(fire_sf, col.regions = "red", layer.name = "Fires") 


#fire locations + coavariates locations
mapview(fire_sf, col.regions = "red", layer.name = "Fires") +
  mapview(cov_sf, col.regions = "green", layer.name = "Covariates location")
  
#################done making maps##################################################################




#Biotic
Biotic_cov_with_xy$year <- as.Date(paste0(Biotic_cov_with_xy$year, "-01-01")) #converting this format "%Y-%m-%d" because later in the process (time series), we need this column as as.date
Biotic_cov_with_xy <- Biotic_cov_with_xy %>%
  mutate(Year = format(year, "%Y")) ##since we are only interested in years, extract the year


#Abiotic
correct_abiotic_cov$Date <- as.Date(paste0(correct_abiotic_cov$Date, "-01"), format = "%Y-%m-%d") 

cov_2020_2024 <- correct_abiotic_cov %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31")) #extracting the covariates from 2020 - 2024

cov_2020_2024_2 <- cov_2020_2024 %>%
  mutate(Year = format(Date, "%Y"))  #since we are only interested in years


#both bio and abio
str(Biotic_cov_with_xy)
str(cov_2020_2024_2)


#some coordinates were round up to five and some to four in the following dfs. so we need to keep this consistent
#we are planning to joing the abiotic covariates and biotic covariates by locations and the year
#to join these and use it
Biotic_cov_with_xy <- Biotic_cov_with_xy %>%
  mutate(
    x = round(x, 4),
    y = round(y, 4),
    Year = as.character(Year)  # keep it character to match cov_2020_2024_2
  )

cov_2020_2024_2 <- cov_2020_2024_2 %>%
  mutate(
    x = round(x, 4),
    y = round(y, 4),
    Year = as.character(Year)
  )



#both biotic and abiotic covs are matched by location and the year
matched_df <- inner_join(
  cov_2020_2024_2,
  Biotic_cov_with_xy,
  by = c("x", "y", "Year")
)

head(matched_df)



#maybe the right way to do this is this

# First: summarize manually
yearly_summary <- matched_df %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_ppt = mean(tmean..degrees.C., na.rm = TRUE),
    sd_ppt = sd(tmean..degrees.C., na.rm = TRUE),
    n = n(),
    se_ppt = sd_ppt / sqrt(n)
  )

# Then plot
ggplot(yearly_summary, aes(x = Year, y = mean_ppt, group = 1)) +
  geom_line(color = "#800000") +
  geom_ribbon(aes(ymin = mean_ppt - se_ppt, ymax = mean_ppt + se_ppt), fill = "#800000", alpha = 0.3) +
  theme_classic() +
  labs(x = "Year", y = "", title = "Mean precipitation")







#######################RF model#######################################################
###############################################################################
#we are focusing on 2020 - 2024


#setting up binary for RF classification model
#Year 2024 is set as 1 and others set as 0
fire_rf <- matched_df %>%
  mutate(is_bad_fire = ifelse(Year == 2024, 1, 0))  

#all the covariates that goes into the model are included in here
fire_rf <- fire_rf %>%
  select(is_bad_fire,
         tmean = tmean..degrees.C.,
         ppt = ppt..mm.,
         vpd = vpdmax..hPa.,
         AFG,PFG,SHR, TRE)


#model training
rf_model <- randomForest(
  factor(is_bad_fire) ~ .,
  data = fire_rf,
  importance = TRUE,
  ntree = 500
)

# Numeric importance values
importance(rf_model)

# Visual plot
varImpPlot(rf_model, main = "Variable Importance for 2024 ")
#Important by both metrics → fires in 2024 occurred under higher VPD
#Also very influential → higher temps may be linked to 2024 fires



partialPlot(rf_model, pred.data = fire_rf, x.var = "vpd")
partialPlot(rf_model, pred.data = fire_rf, x.var = "tmean")


ggplot(fire_rf, aes(x = vpd, fill = factor(is_bad_fire))) +
  geom_density(alpha = 0.4) + theme_classic()












###############not important this is just for the testing###################################
#there are just this and that I have been tesing############################################
###########dnt run##########################################################################
# Join: which cov points fall within 1000m of any fire point
cov_near_fire <- st_join(
  cov_sf,
  st_buffer(fire_sf, dist = 10000),
  join = st_intersects,
  left = FALSE,
  suffix = c("_cov", "_fire")
)


unique(cov_near_fire$id_fire)
#[1]  73  35  36  41  60  67  84 153 156 157 168 278 315 323 329 331 353 362 415 424

# 1. Filter cov points near fire_id == 1
CCC <- cov_near_fire %>% filter(id_fire == 73)

# 2. Filter the corresponding fire point itself
FFF <- fire_sf %>% filter(id == 73)

# 3. Visualize together
mapview(fire_sf, col.regions = "red", layer.name = "Fire Point 1")+
  mapview(cov_near_fire, col.regions = "blue")

# 3. Visualize together
mapview(FFF, col.regions = "red", layer.name = "Fire Point 1") +
  mapview(CCC, col.regions = "blue", layer.name = "Cov points within 200m")


# Get cov info near fire_id == 100 (for example)
cov_ids <- cov_near_fire %>% filter(id_fire == 73) %>% pull(id_cov)

cov_original <- cov %>% filter(cov_id %in% cov_ids)

# Get the original fire point
fire_original <- fire %>% filter(FIRE_ID == 73)

class(cov_original$Date)
cov_original$Date <- as.Date(paste0(cov_original$Date, "-01"), format = "%Y-%m-%d")



library(dplyr)


#time series for the environmental variables for each fire
cov_original$ppt..mm.
head(cov_original)
ggplot(data = cov_original, aes(x=Date, y=ppt..mm.)) +
  geom_line(color = "#800000")+
  labs(x = "Date (by month)", y = "Precipitation (mm)")+
  theme_classic()






















