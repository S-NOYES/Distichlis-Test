#REPLICATE SCALE 

setwd("~/Desktop/distichlis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsq)
library(broom)
library(writexl)
EOYBdata <- read.csv("EOYBdata.csv") 

#filter out zones 3 and 4 from sites with more than 5 years data with only the desired columns

tableFull <- EOYBdata %>% select(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass) %>% 
  filter(locationID %in% c(3,4), 
         marshName %in% c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
                          "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
                          "Upper Phillips"))

#Combine both brownsvilles into Brownsville North Left
Brownsville <- select(filter(EOYBdata, marshName %in% c("Brownsville North", "N. Brownsville Left")), c(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass))
Brownsville$marshName <- "Brownsville North Left"

###################################### Upper Phillips Creek 

UPC <- read.csv("UPC.csv")

UPChighMarsh <- select(filter(UPC, marshRegion == 4), c(EOYBYear, marshRegion, speciesName, locationName, Replicate, unknownMass, totalMass))
UPChighMarsh$marshRegion <- 3
UPChighMarsh <- rename(UPChighMarsh, Transect = locationName)

UPCT <- read.csv("UPC 2.csv")

UPCtransition <- select(filter(UPCT), c(EOYBYear, marshRegion, speciesName, Transect, Replicate, unknownMass, totalMass))
UPCtransition$marshRegion <- 4

UPCtransition <- UPCtransition %>% mutate(Transect = case_when(
  Transect == "PP2" ~ "A", 
  Transect == "E5" ~ "B", 
  Transect == "E1" ~ "C", 
  Transect == "S1" ~ "D"))

upperPhillips <- rbind(UPChighMarsh, UPCtransition)
upperPhillips$marshName <- ("Upper Phillips")
upperPhillips <- rename(upperPhillips, locationID = marshRegion)

#################

tableComplete <- rbind(Brownsville, tableFull, upperPhillips)

tableComplete[ tableComplete == "NA" ] = NA
tableComplete$Transect <- as.character(tableComplete$Transect)
tableComplete$Replicate <- as.character(tableComplete$Replicate)
tableComplete$speciesName <- as.character(tableComplete$speciesName)
tableComplete$totalMass <- as.numeric(tableComplete$totalMass)

tableComplete <- na.omit(tableComplete)

tableComplete <-tableComplete[!(tableComplete$unknownMass > 0),]
tableComplete <- tableComplete[!(tableComplete$totalMass == 0),]

tableComplete$speciesName[tableComplete$speciesName== "Distichlis spicata"] <- "Distichlis.spicata"

marshes <- c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
             "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
             "Upper Phillips", "Brownsville North Left")

#################### Replicate Scale

HotStart <- select(filter(tableComplete, locationID %in% c("3", "4")), 
                   c(EOYBYear, marshName, locationID, Transect, Replicate, totalMass, speciesName)) 

#group by all variables excpet total mass, extract rows with repeated identifiers into new dataframe
repeatKeys <- HotStart %>% group_by_at(vars(-totalMass)) %>% filter(n() >1)

#extract all unique rows into its own dataframe
uniqueKeys <- HotStart %>% group_by_at(vars(-totalMass)) %>% filter(n() == 1)

#spread wide 
replicateWide <- spread(uniqueKeys, speciesName, totalMass)

#convert all NAs in the table to 0s
replicateWide[is.na(replicateWide)] <-0

#create column AllSpeciesBiomass with row sums excluding columns 1:5
replicateWide$AllSpeciesBiomass <- apply(replicateWide[,-c(1:5)], 1, sum) 

#create column PercentDistichlis by dividing distichlis mass by replicate sum and multiplying by 100
replicateScale <- mutate(replicateWide, PercentDistichlis = (Distichlis.spicata / AllSpeciesBiomass)*100)

#remove rows with less than 2 replicates
replicateRemover <- replicateScale %>% group_by(EOYBYear, marshName, locationID, Transect) %>% filter(n() > 1)

#average the total species mass and percent distichlis of each replicate within transects
transectScale <- replicateRemover %>% group_by(EOYBYear, marshName, locationID, Transect) %>% summarise_at(c("AllSpeciesBiomass", "PercentDistichlis"), mean, na.rm = TRUE)

#remove years if there isn't at least two transects
transectRemover <- transectScale %>% group_by(EOYBYear, marshName, locationID) %>% filter(n() > 1)

#average the total species mass and percent distichlis of each transect within a year
zoneScale <- transectRemover %>% group_by(EOYBYear, marshName, locationID) %>% summarise_at(c("AllSpeciesBiomass", "PercentDistichlis"), mean, na.rm = TRUE)

#filter out high marsh values only
highMarshZone <- zoneScale %>% filter(locationID == 3)

#filter out transition values only
transitionZone <- zoneScale %>% filter(locationID == 4)

#removes year if both zones do not have data
zoneRemover <- zoneScale %>% group_by(EOYBYear, marshName) %>% filter(n() > 1)

#average the total species mass and percent distichlis of each zone within a year
siteScale <- zoneRemover %>% group_by(EOYBYear, marshName) %>% summarise_at(c("AllSpeciesBiomass", "PercentDistichlis"), mean, na.rm = TRUE)

#see what we're working with
print(siteScale)

###################

#this function finds the max value above the mean
SDMax <- function(x){
  Max <- (mean(x)+(2*sd(x)))
  return(Max)
}

#this function finds the min value below the mean
SDMin <- function(x){
  Min <- (mean(x)-(2*sd(x)))
  return(Min)
}

################################################################################## High Marsh GRAPH ##########################################


#function for graphing Replicate Scale (manually change marshes )
repGrapher <- function(x){
  
  #zoneRemover is the data frame we want for site graph style A, this line cycles through each marsh name
  replicateScale <- filter(replicateScale, locationID == 4, marshName == x)
  
  #apply the max outlier function to the All Species column
  AllMax <- SDMax(replicateScale$AllSpeciesBiomass)
  
  #apply the min outlier function to the All Species column
  AllMin <- SDMin(replicateScale$AllSpeciesBiomass)
  
  #apply the max outlier function to the Distichlis column
  DisMax <- SDMax(replicateScale$PercentDistichlis)
  
  #apply the min outlier function to the Distichlis column
  DisMin <- SDMin(replicateScale$PercentDistichlis)
  
  #filters out values frome the data frame that fall outside the range of the max and min values for each column
  rep_noOutliers <- filter(replicateScale, AllSpeciesBiomass > AllMin & AllSpeciesBiomass < AllMax & PercentDistichlis > DisMin & PercentDistichlis < DisMax)
  
  
  pdf(file = "RepScale_Transition_afteredits.pdf", onefile = TRUE)
  ggplot(rep_noOutliers, aes(AllSpeciesBiomass, PercentDistichlis, colour = EOYBYear))+ scale_color_continuous(breaks = round) +
    geom_point(size=3)+labs(col = "EOYBYear")+ylab('% Distichlis')+xlab('All Species Biomass')+
    ggtitle(x, "Replicate Scale - Transition Zone")+
    scale_x_continuous(limits = c(0,200)) + scale_y_continuous(limits = c(0,100)) + stat_smooth(method = lm, se = FALSE, linetype = "dashed", colour="Red", size = 0.6) + geom_smooth(method = 'glm', linetype = "dashed", se = FALSE, colour="Navy", size = 0.5, method.args = list(family = 'poisson'))
}
lapply(marshes, repGrapher)
dev.off()

################################################################################## High Marsh Model Summary  ##########################################

#creates an empty list that the for loop will fill
modelOutputList <- list()


for(x in marshes){
  
  #zoneRemover is the data frame we want for site graph style A, this line cycles through each marsh name
  replicateScale_HM <- filter(replicateScale, marshName == x, locationID == 4)
  
  #apply the max outlier function to the All Species column
  AllMax <- SDMax(replicateScale_HM$AllSpeciesBiomass)
  
  #apply the min outlier function to the All Species column
  AllMin <- SDMin(replicateScale_HM$AllSpeciesBiomass)
  
  #apply the max outlier function to the Distichlis column
  DisMax <- SDMax(replicateScale_HM$PercentDistichlis)
  
  #apply the min outlier function to the Distichlis column
  DisMin <- SDMin(replicateScale_HM$PercentDistichlis)
  
  #filters out values frome the data frame that fall outside the range of the max and min values for each column
  noOutliers <- filter(replicateScale_HM, AllSpeciesBiomass > AllMin & AllSpeciesBiomass < AllMax & PercentDistichlis > DisMin & PercentDistichlis < DisMax)
  
  
  # "tidy" and "glance" used in the two lines below take the linear model summary (like the one the old approach outputs) and filters out the important values into a dataframe
  #"tidy" does not include the adjusted r squared so thats why we have to do "glance" as well
  
  #linear model summary with "tidy" filters the normal linear model summary and creates a datsa frame with a select group of values (we will choose from here later)
  Linear_model <- tidy(lm(PercentDistichlis ~ AllSpeciesBiomass, data = noOutliers))
  #View(Linear_model)
  
  #linear model summary with "glance" gives us a group of other variables found in the original linear model summary, from here we will filter the the adjusted r quared value later
  Linear_model_2 <- glance(lm(PercentDistichlis ~ AllSpeciesBiomass, data = noOutliers))
  #View(Linear_model_2)
  
  #Now from these two dataframes that have the values we need, we will call out only the ones we want 
  #call them out by specifying the exact location of the cells within the dataframes "Linear_model" and "Linear_model_2"
  # we are also defining the column names these specified values will go into in the new dataframe 
  
  #this is the slope value or the "All species Biomass estimate" value 
  AllSpeciesBiomass_Estimate <- c(Linear_model[2,2])
  
  #this is the intercept or the "estiamte intercept"
  Intercept_Estimate <- c(Linear_model[1,2])
  
  #we only need the p value associated with 'AllSpeciesBiomass" so that is the one that is called here
  p.value <- c(Linear_model[2,5])
  
  #adjusted r squared value
  adjusted.r.squared <- c(Linear_model_2[1,2])
  
  #adding a model type column to catagorize the model type for the dataframe
  Model_Type <- c("Linear_Model")
  
  #adding a site column to specify the marsh of the model beign run 
  Site <- c(x)
  
  #create the new data frame with only the values we need specified above from the Linear Model
  lm_summary <- data.frame(Site, Model_Type, AllSpeciesBiomass_Estimate, Intercept_Estimate, p.value, adjusted.r.squared)
  #fix some column name issues 
  lm_final <- lm_summary %>% rename (intercept.estimate = estimate.1, AllSpeciesBiomass.estimate = estimate, adjusted.r.squared = adj.r.squared)
  
  ####Poisson Model 
  #remeber warnings here are abundant but chill 
  ### IF we end up going with quasi poisson, just change out "poisson" with "quasipoisson" wheverever you see it
  
  #"tidy" output gives us intercept, slope and p value for poisson model
  Poisson_model <- tidy(glm(PercentDistichlis ~ AllSpeciesBiomass, family="poisson", data = noOutliers))
  summary(Poisson_model)
  
  # adjusted r squared is different for poisson, we have to use a specific line of code to get it
  poisson_r <- glm(PercentDistichlis ~ AllSpeciesBiomass, family="poisson", data = noOutliers)
  summary(poisson_r)
  rsq(poisson_r, adj = TRUE)
  
  #calling out only the values we need from the outputs putting them in a new dataframe like we did for the linear model
  AllSpeciesBiomass_Estimate <- c(Poisson_model[2,2])
  Intercept_Estimate <- c(Poisson_model[1,2])
  p.value<- c(Poisson_model[2,5])
  adjusted.r.squared <- c(rsq(poisson_r, adj = TRUE))
  Model_Type <- c("poisson_Model")
  Site <- c(x)
  
  #new data frame with only the values we need for Poisson Model
  poisson_summary <- data.frame(Site, Model_Type, AllSpeciesBiomass_Estimate, Intercept_Estimate, p.value, adjusted.r.squared)
  
  #rename columns to match the linear model dataframe 
  poisson_final <- poisson_summary %>% rename (intercept.estimate = estimate.1, AllSpeciesBiomass.estimate = estimate)
  
  #combine the Linear Model and Poisson Model dataframe summaries together  
  model_output <- rbind(lm_final, poisson_final)
  
  ################
  
  modelOutputList[[x]] <- model_output
  
}


Rep_Scale_Transition <- bind_rows(modelOutputList)


#export combined dataframe as an excel to desktop
write_xlsx(Rep_Scale_Transition , "~/Desktop/distichlis\\Rep_Scale_transition_afteredits .xlsx")

############################################################################  
#### going to have to change fucntion for regional scale to match years 






