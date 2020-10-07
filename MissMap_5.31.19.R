###-----------------------Set up
setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation")
getwd()
library(Amelia) 
library(reshape2)
library(ggplot2)
library(haven)
library(tidyverse)
library(labelled)

###-----------------------Get Data
survey <- read_spss("Emily_Dissertation_3.1.19_EndofDay.sav")

###-----------------------Data Manipulation / remove unneeded variables
#Rename the study ID variable in the survey so it's consistent with the other naming conventions
survey <- survey %>% 
  rename(study_id = StudyID)

#Remove study ID variable label
survey$study_id <- remove_var_label(survey$study_id)  

#Keep only the variables I want

#Make a list of variables to keep
## Eventually, I'll be doing this with the imputed data - but I do still need to keep the other variables for the imputation

keep <- c("study_id", #unique identifier
          "V1Completion", #survey completion
          "V2Completion",
          "V3Completion",
          "V4Completion",
          "Total_Surveys_Completed", #variable calculated to count surveys comp.
          "RACE_ETH", # demographics
          "FIRSTGEN",
          "FEMALE",
          "SouthcomplexBinary_2015", #did student live in South complex in 2015?
          "Max_ACT_Composite", #Prior Achievement
          "SS18_Persistence", #Outcome
          "V1ESE1", "V1ESE2", "V1ESE3", "V1ESE4", "V1ESE5", #motivation
          "V2ESE1", "V2ESE2", "V2ESE3", "V2ESE4", "V2ESE5",
          "V3ESE1", "V3ESE2", "V3ESE3", "V3ESE4", "V3ESE5",
          "V4ESE1", "V4ESE2", "V4ESE3", "V4ESE4", "V4ESE5",
          "V1Int1", "V1Int2", "V1Int3", "V1Int4", "V1Int5",
          "V2Int1", "V2Int2", "V2Int3", "V2Int4", "V2Int5",
          "V3Int1", "V3Int2", "V3Int3", "V3Int4", "V3Int5",
          "V4Int1", "V4Int2", "V4Int3", "V4Int4", "V4Int5",
          "V1Att2", "V1Att3", "V1Att4", "V1Att5",
          "V2Att2", "V2Att3", "V2Att4", "V2Att5",
          "V3Att2", "V3Att3", "V3Att4", "V3Att5",
          "V4Att2", "V4Att3", "V4Att4", "V4Att5",
          "V1Util1", "V1Util2", "V1Util5",
          "V2Util1","V2Util2", "V2Util5",
          "V3Util1", "V3Util2", "V3Util5",
          "V4Util1", "V4Util2", "V4Util5")

for_imputation <- survey  %>% 
  select(one_of(keep))

###-----------------------Create Composite Variables
#write_sav(for_imputation, "5.22.19_data_forcomposites.sav")
#Go to SPSS here
#ran this file
  #Compute_Scales_3.6.19.sps
#created new file with the scales/composites 
  #5.22.19_data_forcomposites_withcomposites
#bring it back in

missmap_data <- read_spss("5.22.19_data_forcomposites_withcomposites.sav")

###-----------------------Last Step of Viz Prep: Select only the right variables
#Getting rid of the item-level
#looking instead at the composites (but with missing data present)

missmap_data <- missmap_data %>% 
  select(c("RACE_ETH",
           "FIRSTGEN",
           "FEMALE",
           "SouthcomplexBinary_2015",
           "Max_ACT_Composite",
           "SS18_Persistence",  
           "V1_ESE",                 
           "V2_ESE",                  
           "V3_ESE",                  
           "V4_ESE",                 
           "V1_Val",                  
           "V2_Val",                  
           "V3_Val",                 
           "V4_Val"))

#Rename the variables to be more meaningful
missmap_data<- missmap_data %>% 
  rename("Race" = "RACE_ETH",
         "First-gen" = "FIRSTGEN",
         "Gender" = "FEMALE",
         "Campus_Residence" = "SouthcomplexBinary_2015",
         "ACT" = "Max_ACT_Composite",
         "Persistence" = "SS18_Persistence",
         "T1PerCom" = "V1_ESE",
         "T2PerCom" = "V2_ESE",
         "T3PerCom" = "V3_ESE",
         "T4PerCom" = "V4_ESE",
         "T1Val" = "V1_Val",
         "T2Val" = "V2_Val",
         "T3Val" = "V3_Val",
         "T4Val" = "V4_Val")

###-----------------------Missing Data Viz

# Function for making box data frame
boxy <- function(df){
  data.frame(xmin = seq(0.5, ncol(df) - 0.5),
             xmax = seq(1.5, ncol(df) + 0.5),
             ymin = 0.5, ymax = nrow(df) + 0.5)
}

#Create Function
ggplot_missing <- function(x){
  df_box <- boxy(x)
  df_rast <- x %>% is.na %>% melt
  
  ggplot() +
    geom_raster(data = df_rast,
                aes(x = Var2,
                    y = Var1,
                    fill = value)) +
    geom_rect(data = df_box, 
              aes(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax),
              colour = "cyan1", fill = NA, size = 2) + 
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle = 90, hjust = 1, size = 10)) + 
    labs(x = "Variables in Dataset",
         y = "Observations")
}

#Feed the function my new data
ggplot_missing(missmap_data)

#Save the plot
ggsave("MissMap_5.31.19.png")
