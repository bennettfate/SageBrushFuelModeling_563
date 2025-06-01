rm(list= ls())
graphics.off()

library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(randomForest)
library(pdp)
library(ggfortify)
library(cluster)
library(caret)

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
head(Biotic_cov_with_xy)



# Calculate mean biotic varibale per year
#we make the abitoc time series maps later. 
summary <- Biotic_cov_with_xy %>%
  group_by(year) %>%
  summarise(mean_bio = mean(TRE, na.rm = TRUE))

# Create the bar plot
ggplot(summary, aes(x = year, y = mean_bio)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_classic() +
  labs(
   # title = "Mean AFG by Fire Year",
    x = "Fire Year",
    y = "Mean TRE (%)"
  ) +
  theme(
    aspect.ratio = 0.7,
    axis.text.y = element_text(colour = "black", size = 16), 
    axis.text.x = element_text(colour = "black", size = 16),  
    legend.position = "right",
    axis.title.x = element_text(colour = "black", size = 17),  
    axis.title.y = element_text(colour = "black", size = 17),  
    axis.ticks = element_line(colour = "black", size = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    plot.title = element_text(colour = "black", size = 24, hjust = 0.5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.text = element_text(size = 17, colour = "black"), 
    legend.title = element_text(size = 17),
    legend.key=element_blank(),
    legend.key.size = unit(0.5, 'cm'),  # Increased legend key size
  )

ggsave(filename="TRE.jpeg", width= 11, height=6, units = "in", dpi=600)

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

matched_df$vpdmax..hPa.

#maybe the right way to do this is this

# First: summarize manually
yearly_summary <- matched_df %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    mean_value = mean(TRE, na.rm = TRUE),
    sd_shades = sd(TRE, na.rm = TRUE),
    n = n(),
    sd_shades = sd_shades / sqrt(n)
  )

yearly_summary$Year <- as.numeric(yearly_summary$Year)


# Then plot
ggplot(yearly_summary, aes(x = Year, y = mean_value, group = 1)) +
  geom_line(color = "black", linewidth = 1) +
  geom_ribbon(aes(ymin = mean_value - sd_shades, ymax = mean_value + sd_shades), fill = "#800000", alpha = 0.3) +
  scale_x_continuous(breaks = 2021:2024, limits = c(2021, 2024)) +
  labs(x = "Year", y = "", title = "TRE") +
  theme(
    aspect.ratio = 0.7,
    axis.text.y = element_text(colour = "black", size = 16), 
    axis.text.x = element_text(colour = "black", size = 16),  
    legend.position = "right",
    axis.title.x = element_text(colour = "black", size = 17),  
    axis.title.y = element_text(colour = "black", size = 17),  
    axis.ticks = element_line(colour = "black", size = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    plot.title = element_text(colour = "black", size = 24, hjust = 0.5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.text = element_text(size = 17, colour = "black"), 
    legend.title = element_text(size = 17),
    legend.key=element_blank(),
    legend.key.size = unit(0.5, 'cm'),  # Increased legend key size
  )


ggsave(filename="TRE.jpeg", width= 11, height=6, units = "in", dpi=600)






#######################RF model#######################################################
###############################################################################
#we are focusing on 2020 - 2024
#setting up binary for RF classification model
#Year 2024 is set as 1 and others set as 0
fire_rf <- matched_df %>%
  mutate(is_bad_fire = ifelse(Year == 2024, 1, 0))  


table(fire_rf$is_bad_fire)

#all the covariates that goes into the model are included in here
fire_rf <- fire_rf %>%
  select(is_bad_fire,
         tmean = tmean..degrees.C.,
         ppt = ppt..mm.,
         vpd = vpdmax..hPa.,
         AFG,PFG,SHR,TRE)

set.seed(123)

# Split data: 70% train, 30% test
train_idx <- sample(nrow(fire_rf), 0.7 * nrow(fire_rf))
train_data <- fire_rf[train_idx, ]
test_data <- fire_rf[-train_idx, ]

# Train model
rf_model <- randomForest(
  factor(is_bad_fire) ~ ., 
  data = train_data, 
  importance = TRUE, 
  ntree = 500,
  mtry = 3
)

# Predict on test set
test_preds <- predict(rf_model, newdata = test_data)
# Confusion matrix
confusionMatrix(test_preds, factor(test_data$is_bad_fire))
varImpPlot(rf_model, main = "Variable Importance for 2024")


##########
library(pROC)
roc_obj <- roc(test_data$is_bad_fire, as.numeric(test_preds))
plot(roc_obj, col = "darkred")




#########################

# Extract variable importance
vip_df <- as.data.frame(importance(rf_model))
vip_df$Variable <- rownames(vip_df)


# focus on MeanDecreaseGini or MeanDecreaseAccuracy
vip_df <- vip_df %>%
  arrange(desc(MeanDecreaseGini))

# Plot
ggplot(vip_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "black") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Variable Importance in 2024 fires",
    x = "Variables",
    y = "Gini Index"
  ) +
  theme(
    aspect.ratio = 0.8,
    axis.text.y = element_text(colour = "black", size = 20), 
    axis.text.x = element_text(colour = "black", size = 20),  
    legend.position = "right",
    axis.title.x = element_text(colour = "black", size = 17),  
    axis.title.y = element_text(colour = "black", size = 17),  
    axis.ticks = element_line(colour = "black", size = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
    plot.title = element_text(colour = "black", size = 24, hjust = 0.5),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.text = element_text(size = 17, colour = "black"), 
    legend.title = element_text(size = 17),
    legend.key=element_blank(),
    legend.key.size = unit(0.5, 'cm'),  # Increased legend key size
  )

ggsave(filename="Gini Index.jpeg", width= 11, height=6, units = "in", dpi=600)







partialPlot(rf_model, pred.data = fire_rf, x.var = "vpd")
partialPlot(rf_model, pred.data = fire_rf, x.var = "tmean")


ggplot(fire_rf, aes(x = vpd, fill = factor(is_bad_fire))) +
  geom_density(alpha = 0.4) + theme_classic()
########################





































