# Analysis0_VideoAppraisalsQCPilots
##############################   OWNERSHIP   #####################################
# This script is written by Asli Erdemli.
# Last updated in Aug 2025, adapted from a real project which was scripted in Oct 2022. 
# The real project's data and information was hidden or replaced.
# However, this script has kept specifics to what one might expect from raw data collected and downloaded from Gorilla, the online  experiment buider.    
#
# 
##############################   PURPOSE   #####################################
# To import, tidy and quality check (QC) *pilot* data collected through Gorilla for LeProject. This is useful to screen bots or inattentive participants which have passed Gorilla's bot check task. 

##############################   Structure   #####################################

# - For each participant we plot the timeseries per feature. This allows for visual QC. 
# - For statistical QC, the data is checked for some obvious correlations. 
# - Participants are categorized in two lists according to whether the correlations are as expected : approved and unapproved lists. 
# - For unapproved participants, the analyst can visually check the timeseries and decide whether to keep that participant. 
#
##############################   DATASET   #####################################
# Among other variables, the dataset contains the following variables of interest: 
# - appraisals (3 different sets) 
# - emotions (2 different sets)
# - bodymaps
#
##############################   REFERENCES   #####################################
# This script is guided by: https://emljames.github.io/GorillaR/GorillaR_Part1.html


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() # Will empty memory

######################## Load packages #############################
# Set Library for this project
.libPaths("C:/Users/erdemli/Documents/LeProject/VideosProjectLib")

# Install the package in that library and activate it
#install.packages("tidyverse")
#install.packages("vioplot")
#install.packages("png")
#install.packages("ggfortify")
# install.packages("psych")
# install.packages ("lme4")
# update.packages(ask = FALSE)
# install.packages ("irr")

library(png)
library(tidyverse)
library(magrittr)
library(vioplot)
library(ggfortify)
library(psych)
library(irr)

########################   IMPORT DATA   #######################################

# Where is all your Gorilla data? Should contain all task files, starting with "data_"
DataFile <- ("C:/Users/erdemli/Documents/LeProject/data/raw_data/")

# Get names of all data files, that contain "task" in the name.
# With full path
FileFullNames <-
  list.files(DataFile, pattern = "task" , full.names = TRUE)
# With only file names
FileShortName <-
  list.files(DataFile, pattern = "task", full.names = FALSE)


# Loop through the raw Gorilla task files and rename them so that instead of 
# something like "data_exp_95245-v22_task-96zk.csv" you have a dataframe called 
# "Instructions ans training - appraisal task 1_"
for (iFile in FileFullNames) {
  # Some files are empty and bug the loop. I want the loop to continue even
  # if some files are empty.
  # We create an error counter.
  skip_to_next <- FALSE
  
  #Read csv file as dataframe
  rawData <- read.csv(iFile)
  
  # If the file isn't empty, then a node name can be get from the "Tree Node Key"
  # column of our rawData.
  tryCatch(
    expr = {
      #Get name of the task node
      nodeName <- rawData[1, "Task.Name"]
      VSversion <- rawData[1, "VideoZoneSubset"]
      nodeName <- paste(nodeName, VSversion, sep="_", collapse= NULL)
      assign(nodeName, rawData)
    },
    error = function(e) {
      skip_to_next <<- TRUE
      message ("Hey, a warning for file ", iFile)
      print(e)
    },
    
    finally = {
      message('All done, quitting.')
    }
  )
  
  if (skip_to_next) {
    next
  }
  
}




#####################   EXTRACT ONLY RELEVANT ROWS   ###########################


### For appraisal and emotion tasks, extract only response rows.

# Let's make a list of our data frames
apprDfList <- ls(pattern='^appraisal')

# For each data frame of appraisals and emotions (= things that contain sliders)... 
for (iDF in apprDfList) {
  
  # Get only rows with responses to the sliders, not initial slider values etc
  dfResponses <-
    get(iDF) %>% filter(Zone.Type == "response_slider_endValue")
  
  

  
  # Count the participants you have. And how many observations per participant you have. 
  # Since we have 32 videos, this should be a 32 * nb of sliders in that task. 
  measuresPerParticipant <-
    dfResponses %>% group_by(Participant.Private.ID) %>%  count()
  
  # Exclude participants from the dataset (some cause datadoubling, giving errors
  # for the next parts)
  dfIncluded <-
    dfResponses %>% filter(!Participant.Private.ID == 7319126)
  
  # Select relevant variables and extract them to another dataframe:
  dfRelevant <- dfIncluded %>%
    select(
      Participant.Private.ID,
      Participant.Public.ID, # For Amazon M Turk data: Participant.Private.ID,
      randomiser.d39u,
      VideoZoneSubset,
      Trial.Number,
      Zone.Name,
      Response,
      dfIncluded$VideoZoneSubset[1]
    )
  
  # Spread sliders 1 2 3 4 5 6 and 7 to different columns instead of being stacked in rows.
  dfRelevantW <- dfRelevant %>% spread(Zone.Name, Response)
  
  #Get name of the task node
  taskName <- dfRelevantW[1, "randomiser.d39u"]
  VSversion <- dfRelevantW[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfRelevantW)
  
}


#########################   Rename the sliders   ###############################

## For Appraisals 1 dataframes: (remember, we have 3 subsets of videos) 
dfAppr1List <- ls(pattern='^Appraisals 1')

for (iDF in dfAppr1List) {
  dfTemp<- get(iDF) %>% rename(
    Pleasant = slider1,
    Unpleasant = slider2,
    Familiar = slider3,
    Unexpected = slider4,
    Comprehensible = slider5,
    Complex = slider6,
    Coping = slider7
  ) 
  
  #Get name of the task node
  taskName <- dfTemp[1, "randomiser.d39u"]
  VSversion <- dfTemp[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfTemp)
}


## For Appraisals 2 dataframes : 

dfAppr2List <- ls(pattern='^Appraisals 2')

for (iDF in dfAppr2List) {
  
  dfTemp<- get(iDF) %>% rename(
    Acceptable = slider1,
    Chance = slider2,
    PeopleCaused = slider3,
    Intentional = slider4,
    Relevant = slider5,
    Engaged = slider6,
    Wonder = slider7
  )
  
  #Get name of the task node
  taskName <- dfTemp[1, "randomiser.d39u"]
  VSversion <- dfTemp[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfTemp)
}



## For Appraisals 3 dataframes : 

dfAppr3List <- ls(pattern='^Appraisals 3')

for (iDF in dfAppr3List) {
  
  dfTemp<- get(iDF) %>%  rename(
    Shorten = slider1,
    Mask = slider2,
    Awake = slider3,
    PhysiolA = slider4,
    Calm = slider5,
    SexualA = slider6
  )
  
  #Get name of the task node
  taskName <- dfTemp[1, "randomiser.d39u"]
  VSversion <- dfTemp[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfTemp)
}


## For Emotions 1 dataframes : 

dfEmo1List <- ls(pattern='^Emotions 1')

for (iDF in dfEmo1List) {
  
  dfTemp<- get(iDF)  %>% rename(
    Sadness = slider1,
    Anger = slider2,
    Fear = slider3,
    Disgust = slider4,
    Surprise = slider5,
    Embarassment = slider6,
    Enjoyment = slider7
  )
  
  #Get name of the task node
  taskName <- dfTemp[1, "randomiser.d39u"]
  VSversion <- dfTemp[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfTemp)
}


## For Emotions 2 dataframes : 

dfEmo2List <- ls(pattern='^Emotions 2')

for (iDF in dfEmo2List) {
  
  dfTemp<- get(iDF)  %>% rename(
    Curiosity = slider1,
    Awe = slider2,
    Confusion = slider3,
    Excitement = slider4,
    Boredom = slider5,
    Interest = slider6
  )
  
  
  #Get name of the task node
  taskName <- dfTemp[1, "randomiser.d39u"]
  VSversion <- dfTemp[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfTemp)
}

## For body maps task, extract only canvas painting.

dfBodyMapsList <- ls(pattern = "^VideoBodyMaps")
for (iDF in dfBodyMapsList){
  BodyMapsdf <-
    get(iDF) %>% filter(Zone.Type == "canvas_painting")
  
  #Get name of the task node
  taskName <- "BodyMaps"
  VSversion <- BodyMapsdf[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, BodyMapsdf)
  
}



###################   SAVE THESE PREPROCESSED DATAFRAMES   #####################

datalist <- ls(pattern = "^Appraisals |^Emotions|^BodyMaps_")
for (iDFw in datalist){
  
  filename = sprintf('VideosProjectAnalysisSubFiles/%s.csv', iDFw)
  
  write_csv(
    get(iDFw),
    file = filename,
  
  )
}

#Remove the not necessary stuff , to keep R environment light. Keep only 
rm(list = grep("^Appraisals |^Emotions|^BodyMaps_", ls(),value = TRUE,
               invert = TRUE)) #will clear all objects includes hidden objects.
gc() # Will empty memory



###################   Quality Check   ##########################################


# These Appraisals1, Appraisals2, Appraisals3, Emotions1, Emotions2 have all the
# participants in them.
# For Quality Check, we want to display... 

datalist <- ls(pattern = "^Appraisals |^Emotions")


## Times-series plots
# Make visuals of each task for each participant to have a graphical check of how the answers are. 
for (iDF in datalist){
  
  # We need to get the content of that string
  iDF<-get(iDF)
  
  #We change it to tibble to see more easily column classes.(and because tibbles are more modern than dataframes)
  iDF_t<- as_tibble(iDF)
  
  #We change the columns measuring dimension as numeric, 
  #because they are seen as characters and this interferes with data analysis. 
  iDF_t <- iDF_t %>% mutate(across(7:ncol(iDF), as.numeric))
    
  IDF_t_byP<- iDF_t %>% group_by(Participant.Private.ID)
  
  tally(IDF_t_byP) #counts rows by participant


  #Time-series of my measures, ordered by Trial.Number 
  time_series <- ts(IDF_t_byP[,c(5, 6:ncol(IDF_t_byP))])
  
  # Make a list of tibbles containing each participants data separately
  mydflist<-IDF_t_byP %>% group_split()
  
  # Plot each participants tibble separately
  mydflist %>% map_df(~plot.ts(.x[,c( 7:ncol(.x))],  main = paste(.x$Participant.Private.ID[1], .x$VideoZoneSubset[1], sep="_")))
  

  #Rename the tibble, so that our data environments is no longer dataframes but tibbles. 
  taskName <- iDF_t[1, "randomiser.d39u"]
  VSversion <- iDF_t[1, "VideoZoneSubset"]
  taskName <- paste(taskName, VSversion, sep="_", collapse= NULL)
  assign(taskName, iDF_t)
  
  }


# Create lists for approved and unapproved participants. 
approvedList<- list()
unapprovedList<- list()

## Correlations of Appraisals 1
dataListAppr1 <- ls(pattern = "^Appraisals 1")
for (iDF in dataListAppr1){
  
  iDF<-as_tibble(get(iDF))
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iDF <- iDF %>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )
  
  # Make a list of tibbles containing each participants data separately
  idfList<- iDF  %>% group_by(Participant.Private.ID) %>% group_split() 
  
  #Give out number of participants 
  iDF %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  # Plot each participants tibble separately
  temp_corr_ID <- list(map(idfList, ~cor.test(.x$Pleasant,.x$Unpleasant, method= "spearman", exact = FALSE)),  map(idfList, ~.x$Participant.Private.ID[1]), map(idfList, ~.x$Participant.Public.ID[1]))
  
  corr_and_ID <- temp_corr_ID %>% transpose() 
  
  
  for (iParticipant in corr_and_ID ){
    # Print the result
    message("Spearman correlation coefficient of Pleasant - Unpleasant is:", iParticipant[[1]]$estimate)
    # Print the result 
    message("Spearman correlation p-value is: ", iParticipant[[1]]$p.value)
  
    
    if (iParticipant[[1]]$p.value < 0.05){
      message("Participant ",iParticipant[[2]],  " looks OK.")
      message("Participant ",iParticipant[[3]],  " looks OK.")
      approvedList <- append(approvedList, iParticipant[[3]])
      
    }
    else{message("Participant ", iParticipant[[2]],  " does not have a significant Plesant-Unpleasant correlation.")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      # Print the unapproved participant's plot. 
      
      unapprovedPData<- iDF %>% group_by(Participant.Private.ID) %>% 
        filter(Participant.Private.ID == iParticipant[[2]]) 
      view(unapprovedPData)
        plot.ts(unapprovedPData[,c( 7:ncol(unapprovedPData))], main = paste("Unapproved_", unapprovedPData$Participant.Private.ID[1], unapprovedPData$VideoZoneSubset[1], sep="_"))
      
      }
  
  }
}

## Correlations of Appraisals 2
dataListAppr2 <- ls(pattern = "^Appraisals 2")
for (iDF in dataListAppr2){
  
  iDF<-as_tibble(get(iDF))
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iDF <- iDF %>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )
  
  # Make a list of tibbles containing each participants data separately
  idfList<- iDF %>% group_by(Participant.Private.ID) %>% group_split()
  
  #Give out number of participants 
  iDF %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  
  # Plot each participants tibble separately
  temp_corr_ID <- list(map(idfList, ~cor.test(.x$Intentional,.x$PeopleCaused, method= "spearman", exact = FALSE)),  map(idfList, ~.x$Participant.Private.ID[1]), map(idfList, ~.x$Participant.Public.ID[1]))
  
  corr_and_ID <- temp_corr_ID %>% transpose() 
  
  for (iParticipant in corr_and_ID ){
    
    # Print the result
    message("Spearman correlation coefficient of PeopleCaused - Intentional is:", iParticipant[[1]]$estimate)
    # Print the result 
    message("Spearman correlation p-value is: ", iParticipant[[1]]$p.value)
    
    if (iParticipant[[1]]$p.value < 0.05){
      message("Participant ",iParticipant[[2]],  " looks OK.")
      message("Participant ",iParticipant[[3]],  " looks OK.")
      approvedList <- append(approvedList, iParticipant[[3]])
      
    }
    else{message("Participant ", iParticipant[[2]],  " does not have a significant PeopleCaused-Intentional correlation.")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      # Print the unapproved participant's plot. 
      
      unapprovedPData<- iDF %>% group_by(Participant.Private.ID) %>% 
        filter(Participant.Private.ID == iParticipant[[2]]) 
      view(unapprovedPData)
      
      plot.ts(unapprovedPData[,c( 7:ncol(unapprovedPData))], main = paste("Unapproved_", unapprovedPData$Participant.Private.ID[1], unapprovedPData$VideoZoneSubset[1], sep="_"))
      
      }
  }
}

## Correlations of Appraisals 3
dataListAppr3 <- ls(pattern = "^Appraisals 3")
for (iDF in dataListAppr3){
  
  iDF<-as_tibble(get(iDF))
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iDF <- iDF %>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )
  
  # Make a list of tibbles containing each participants data separately
  idfList<- iDF %>% group_by(Participant.Private.ID) %>% group_split()
  
  #Give out number of participants 
  iDF %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  # Plot each participants tibble separately
  temp_corr_ID <- list(map(idfList, ~cor.test(.x$Mask,.x$Shorten, method= "spearman", exact = FALSE)),  map(idfList, ~.x$Participant.Private.ID[1]), map(idfList, ~.x$Participant.Public.ID[1]))
  
  corr_and_ID <- temp_corr_ID %>% transpose() 
  
  for (iParticipant in corr_and_ID ){
    
    # Print the result
    message("Spearman correlation coefficient of Mask - Shorten is:", iParticipant[[1]]$estimate)
    # Print the result 
    message("Spearman correlation p-value is: ", iParticipant[[1]]$p.value)
    
    if (is.na(iParticipant[[1]]$p.value)){
      message("Participant ",iParticipant[[2]],  "has no p value (NA).")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      
      } 
    else if (iParticipant[[1]]$p.value < 0.05) {  
      message("Participant ",iParticipant[[2]],  " looks OK.")
      message("Participant ",iParticipant[[3]],  " looks OK.")
      approvedList <- append(approvedList, iParticipant[[3]])
  
    } 
    
    
    
    else{message("Participant ", iParticipant[[2]],  " does not have a significant Mask - Shorten correlation.")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      
      # Print the unapproved participant's plot. 
      
      unapprovedPData<- iDF %>% group_by(Participant.Private.ID) %>% 
        filter(Participant.Private.ID == iParticipant[[2]]) 
      view(unapprovedPData)
 
      plot.ts(unapprovedPData[,c( 7:ncol(unapprovedPData))], main = paste("Unapproved_", unapprovedPData$Participant.Private.ID[1], unapprovedPData$VideoZoneSubset[1], sep="_"))
      
    } 
    
  }
}

## Correlations of Emotions 1
dataListEmo1 <- ls(pattern = "^Emotions 1")
for (iDF in dataListEmo1){
  
  iDF<-as_tibble(get(iDF))
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iDF <- iDF %>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )
  
  
  # Make a list of tibbles containing each participants data separately
  idfList<- iDF %>% group_by(Participant.Private.ID) %>% group_split()
  
  #Give out number of participants 
  iDF %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  # Plot each participants tibble separately
  temp_corr_ID <- list(map(idfList, ~cor.test(.x$Enjoyment,.x$Anger, method= "spearman", exact = FALSE)),  map(idfList, ~.x$Participant.Private.ID[1]), map(idfList, ~.x$Participant.Public.ID[1]))
  
  corr_and_ID <- temp_corr_ID %>% transpose() 
  
  for (iParticipant in corr_and_ID ){
    
    # Print the result
    message("Spearman correlation coefficient of Enjoyment - Anger is:", iParticipant[[1]]$estimate)
    # Print the result 
    message("Spearman correlation p-value is: ", iParticipant[[1]]$p.value)
    
    if (is.na(iParticipant[[1]]$p.value)){
      message("Participant ",iParticipant[[2]],  "has no p value (NA).")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      
    } 
    else if (iParticipant[[1]]$p.value < 0.05) {  
      message("Participant ",iParticipant[[2]],  " looks OK.")
      message("Participant ",iParticipant[[3]],  " looks OK.")
      approvedList <- append(approvedList, iParticipant[[3]])
    }
    else{message("Participant ", iParticipant[[2]],  " does not have a significant Enjoyment - Anger correlation.")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      
      # Print the unapproved participant's plot. 
      
      unapprovedPData<- iDF %>% group_by(Participant.Private.ID) %>% 
        filter(Participant.Private.ID == iParticipant[[2]]) 
      
      plot.ts(unapprovedPData[,c( 7:ncol(unapprovedPData))], main = paste("Unapproved_", unapprovedPData$Participant.Private.ID[1], unapprovedPData$VideoZoneSubset[1], sep="_"))
      
      }
  }
}

## Correlations of Emotions 2 
dataListEmo2 <- ls(pattern = "^Emotions 2")
for (iDF in dataListEmo2){
  
  iDF<-as_tibble(get(iDF))
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iDF <- iDF %>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )
 
  # Make a list of tibbles containing each participants data separately
  idfList<- iDF %>% group_by(Participant.Private.ID) %>% group_split()
  
  #Give out number of participants 
  iDF %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  # Plot each participants tibble separately
  temp_corr_ID <- list(map(idfList, ~cor.test(.x$Boredom,.x$Interest, method= "spearman", exact = FALSE)),  map(idfList, ~.x$Participant.Private.ID[1]), map(idfList, ~.x$Participant.Public.ID[1]))
  
  corr_and_ID <- temp_corr_ID %>% transpose() 
  
  for (iParticipant in corr_and_ID ){
    
    # Print the result
    message("Spearman correlation coefficient of Boredom - Interest is:", iParticipant[[1]]$estimate)
    # Print the result 
    message("Spearman correlation p-value is: ", iParticipant[[1]]$p.value)
    
    if (iParticipant[[1]]$p.value < 0.05){
      message("Participant ",iParticipant[[2]],  " looks OK.")
      message("Participant ",iParticipant[[3]],  " looks OK.")
      approvedList <- append(approvedList, iParticipant[[3]])
      
      
    }
    else{message("Participant ", iParticipant[[2]],  " does not have a significant Boredom - Interest correlation.")
      unapprovedList <- append(unapprovedList, iParticipant[[3]])
      
      # Print the unapproved participant's plot. 
      
      unapprovedPData<- iDF %>% group_by(Participant.Private.ID) %>% 
        filter(Participant.Private.ID == iParticipant[[2]]) 
      
      plot.ts(unapprovedPData[,c( 7:ncol(unapprovedPData))], main = paste("Unapproved_", unapprovedPData$Participant.Private.ID[1], unapprovedPData$VideoZoneSubset[1], sep="_"))
      
      }
  }
}
print(approvedList)

lapply(approvedList, write, "Approved.txt", append=TRUE, ncolumns=1000)
print(unapprovedList)
lapply(unapprovedList, write, "unApproved.txt", append=TRUE, ncolumns=1000)

## Body maps 

dataListBodyMaps <- ls(pattern = "^BodyMaps")

for (dfModified in dataListBodyMaps){
  
  #Get name of the task node
  taskName <- dfModified 
  
  dfModified <- get(dfModified)
  
  dfModified <- dfModified %>% select(
    Participant.Private.ID,
    Participant.Public.ID,
    randomiser.2jld,
    VideoZoneSubset,
    dfModified$VideoZoneSubset[1],
    Trial.Number,
    Response
  )
  

  dfModified <- dfModified %>% filter(!grepl('https', Response))

  
  # Name your dataset with only relevant rows and slider as columns to its task name (with which videoset it is)
  assign(taskName, dfModified)
}



## time-series of each measured dimension per video across participants per videoset per task
dataListAppr1 <- ls(pattern = "^Appraisals | ^Emotions")

for (myiDF in dataListAppr1){
  
  myiDF<-as_tibble(get(myiDF))
  
  myiDF$Trial.Number<- as.numeric(myiDF$Trial.Number)
  
  myLongIDF<- pivot_longer(myiDF, cols =(7:ncol(myiDF)), names_to = "Dimension", values_to = "Value")
  
  GroupedIDF<- iDF %>% group_by(Participant.Private.ID) %>% tally()

  # points plot and range
  pointPlot <- myLongIDF %>% group_by(Participant.Private.ID, VideoZoneSubset) %>%
    ggplot( aes(x = factor(Trial.Number), y = Value)) +
    facet_wrap(~Dimension) +
    geom_line() +
    geom_point(aes(Trial.Number, Value))   
  
  print(pointPlot+ggtitle(myLongIDF$VideoZoneSubset[1]))
  
  
  #violin plots 
  violinPlot <- myLongIDF %>% group_by(Participant.Private.ID, VideoZoneSubset) %>%
    ggplot( aes(x = factor(Trial.Number), y = Value)) +
    facet_wrap(~Dimension) +
    geom_line() +
    geom_violin(scale="width")
  
  print(violinPlot+ggtitle(myLongIDF$VideoZoneSubset[1]))
  
  #boxplots 
  boxPlot <- myLongIDF %>% group_by(Participant.Private.ID, VideoZoneSubset) %>%
    ggplot( aes(x = factor(Trial.Number), y = Value)) +
    facet_wrap(~Dimension) +
    geom_boxplot() 
  
  print (boxPlot +ggtitle(myLongIDF$VideoZoneSubset[1]))
  
  #lineplots 
  linePlot <- myLongIDF %>% group_by(Participant.Private.ID, VideoZoneSubset) %>%
    ggplot( aes(x = Trial.Number, y = Value, color = factor(Participant.Private.ID))) +
    facet_wrap(~Dimension) +
    geom_line() 
  
  print(linePlot+ggtitle(myLongIDF$VideoZoneSubset[1]))
  

# Let's plot the mean (= geom_line) across all subjects of each dimension (= facet) for 
# each video (x axis) and add the standard error around that mean (geom_ribbon)
  myMeanAndSE <- myLongIDF %>% group_by( Trial.Number, Dimension) %>% summarise(mymean = mean(Value), stdError = sd(Value/sqrt(n())) ) 
  
    MeanSePlot <- myMeanAndSE %>% 
    ggplot( aes(x = Trial.Number, y = mymean)) +
      facet_wrap(~Dimension) +
    geom_line( aes(y = mymean)) +
    geom_ribbon(aes(ymin = mymean - stdError, ymax = mymean + stdError), fill = "blue", alpha = 0.5)
  
  print(MeanSePlot +ggtitle(myLongIDF$VideoZoneSubset[1])) 


## let's create ICC (interclass correlations) to determine intersubject agreement
# between subjects for various videos. 

 
 Dims_and_IDs<- myiDF %>% select( Participant.Private.ID, starts_with("VideoZoneSubset"), -VideoZoneSubset, Pleasant, Unpleasant, Complex, Unexpected, Coping, Comprehensible)
 
 Dims_and_IDs_Long<- myLongIDF %>% select( Participant.Private.ID,starts_with("VideoZoneSubset"), -VideoZoneSubset, Dimension, Value)
 
 Dims_and_IDs_Long <- Dims_and_IDs_Long %>% rename(MyVideos = starts_with("VideoZoneSubset") )
 
 Dims_and_IDs_Long$Value<- as.numeric(Dims_and_IDs_Long$Value)
 
  byGroups<- Dims_and_IDs_Long %>% spread( key = Participant.Private.ID, value = Value) %>% group_by(Dimension)%>% group_split()
   
 
# 
#  Alist <-  list(map(byGroups, ~ICC(subset(.x, select = -c(Dimension, MyVideos)))), map(byGroups, ~print(.x$Dimension[[1]])))
#  ICC_LIst <- Alist %>% transpose()
#  print(ICC_LIst)

  
  Alist <- list(map(byGroups, ~icc(as.data.frame(subset(.x, select = -c(Dimension, MyVideos ))), model="twoway", type="agreement", unit = "single")), map(byGroups, ~print(.x$Dimension[[1]])))

  ICC_LIst <- Alist %>% transpose() 
  print(ICC_LIst) 
  
  Alist <-  list(map(byGroups, ~icc(subset(.x, select = -c(Dimension, VideoZoneSubset3)), model = "twoway",
                                    type = "agreement", unit = "single")), map(byGroups, ~print(.x$Dimension[[1]])))
  ICC_LIst <- Alist %>% transpose() 
  print(ICC_LIst) 
}





############################ Make sure we have all the data from Gorilla ############################

datalist <- ls(pattern = "^Appraisals |^Emotions|^BodyMaps_*")
for (myDF in datalist){
  task_name <- myDF
  myDF<-as_tibble(get(myDF))
  
  myDF_t_byP<- myDF %>% group_by(Participant.Private.ID)
  
  nb_of_people<- as.numeric(n_groups(myDF_t_byP))
  
  tally(myDF_t_byP)
  
  message(task_name, " has ", nb_of_people, " people.")
  
}

##### Get only Body maps list of completion code. 
datalist <- ls(pattern = "^BodyMaps_*")
for (myDF in datalist){
  task_name <- myDF
  myDF<-as_tibble(get(myDF))
  
  myDF_t_byP<- myDF %>% group_by(Participant.Private.ID)
  myDF_t_byCode<- myDF %>% group_by(Participant.Private.ID)
  
  nb_of_people<- as.numeric(n_groups(myDF_t_byP))
  
  tally(myDF_t_byP)
  
  BodyMapCodes <- tally(myDF_t_byCode)[1]
  
  lapply(t(BodyMapCodes), write, "BodyMaps.txt", append=TRUE, ncolumns=1000)
  
  
  message(task_name, " has ", nb_of_people, " people.")
  
}


#### Get worker Ids for approved people, selection by completion code. 

MyWorkersDataTibble <- read_csv("C:/Users/erdemli/Documents/LeProject/VideosParticipantsRecapProlific.csv")

# MyWorkersDataTibble<- make.names(names(MyWorkersDataTibble))
MyapprovedWorker <- MyWorkersDataTibble %>% filter( `Participant id` %in% approvedList)# For MTurk: `Actual Completion Code` %in% approvedList)

MyAbsentApprovedWorker <- MyWorkersDataTibble %>% filter(`Actual Completion Code` %in% approvedList)

lapply(t(MyapprovedWorker$`Participant id`), write, "ApprovedAmazonIDs.txt", append=TRUE, ncolumns=1000)# AmazonIdentifier), write, "ApprovedAmazonIDs.txt", append=TRUE, ncolumns=1000)




BodyMapCodeList <- as.list(t(BodyMapCodes))
MyBodyMapsWorker <- MyWorkersDataTibble %>% filter(`Actual Completion Code` %in% BodyMapCodeList)

lapply(t(MyBodyMapsWorker$AmazonIdentifier), write, "BodyMapsAmazonIDs.txt", append=TRUE, ncolumns=1000)

### Do timeseries of videos with x = participants, each panel = video, y= ??






#### Code that allows me to look one by one the unapproved people's data
`Emotions 2_VideoZoneSubset3`%>% mutate (Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )%>%  
  group_by(Participant.Private.ID) %>% filter(Participant.Public.ID == '62aa3e37751b77e6b2ac96d3' )%>% view() # Participant.Private.ID == 'cexkcj' )%>% view()




#############################Quality check of BodyMaps #########################

library(png)
img <- readPNG(system.file("img", "Rlogo.png", package="png"))
grid::grid.raster(img)

BodyMapsList<- ls(pattern = "BodyMaps_Video")
for (iBodyMapsVS in BodyMapsList){
  iBodyMapsVS <- get(iBodyMapsVS)
  
  # Sort thing data by Trial Number (so that time series make temporal sense) 
  iBodyMapsVS <- iBodyMapsVS %>% mutate(Trial.Number = as.double(Trial.Number)) %>% arrange(Participant.Private.ID, Trial.Number )

  # Add full path Name of the png file to display as an additional column
  iBodyMapsVS <- iBodyMapsVS %>%  rowwise() %>% mutate(myPNGFilePath = str_c('C:/Users/erdemli/Documents/LeProject/Data_pilots_/Datav24/uploads', Response, sep='/'))
  
  # Make a list of tibbles containing each participants data separately
  BMList<- iBodyMapsVS  %>% group_by(Participant.Private.ID) %>% group_split() 
  
  #Give out number of participants 
  iBodyMapsVS %>% group_by(Participant.Private.ID) %>% n_groups() %>% print()
  
  library(grid)
  library(gridExtra)
  
  show_body_maps <- function(yourTibble){
    # Print the png body map 
    plots <- list()  # new empty list
    for (iTrial in 1:32){ # Loop for all trials
      img<- readPNG(yourTibble$myPNGFilePath[iTrial])
      p1 <- rasterGrob(img, interpolate=TRUE)
      plots[iTrial]<-list(p1)
    }
    p1<- arrangeGrob(plots)
    grid.arrange(grobs = plots, top = paste(as.character( yourTibble$Participant.Private.ID[1] ), as.character( yourTibble$Participant.Public.ID[1]) ))
  } 

  BMList %>% map(show_body_maps)
  
  }








