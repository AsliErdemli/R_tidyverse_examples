# Analysis2_VideoAppraisalsVisualExplorationFullData

################################   OWNERSHIP   #################################
# This script is written by Asli Erdemli.
# Last updated in Aug 2025, adapted from a real project which was scripted in Nov 2022. 
# The real project's data and information was hidden or replaced.
# However, this script has kept specifics to what one might expect from raw data collected and downloaded from Gorilla and Prolific, the online  experiment buider.   

##############################   PURPOSE   #####################################
# To import, tidy and visually explore *FULL* data collected through Gorilla for LeProject. This is useful to have visual exploration of data, while considering all participants as interchangeable. (the data collection design is partial, meaning not every participant saw every video nor assessed every feature)

##############################   Structure   #####################################
# - merge all the lists of data in one full data, while extracting only relevant rows. 
# - get means across all participants for each video and each feature. (the data collection design is partial, meaning not every participant saw every video nor assessed every feature)
# - For each feature we plot the timeseries of means per video, with SE.
# - We do heatmaps, correlation plots, violin plot, box plots etc.

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
# Set main working directory to folder within all your separate CSV files
setwd("C:/Users/erdemli/Documents/LeProject")

# Set Library for this project
.libPaths("VideosProjectLib")

# Install the package in that library and activate it
#install.packages("tidyverse")
#install.packages("vioplot")
#install.packages("png")
#install.packages("ggfortify")
# install.packages("psych")
# install.packages ("lme4")
# update.packages(ask = FALSE)
# install.packages ("irrNA")
# install.packages("GGally")


library(png)
library(tidyverse)
library(magrittr)
library(vioplot)
library(ggfortify)
library(psych)
library(irrNA)
#library(lme4)
library(lmerTest)
library(GGally)
library(ggcorrplot)
library(superheat)




########################   IMPORT DATA   #######################################

# Load the content taken from Analysis1_MegalocalizersAppraisalsBindProlificAndMTurkData.R
df <- readRDS('data/MegalocalizerFullDataDFs.Rds')

### Load the list content to the Global environment. This is not recommended 
# because it untidies and overloads the environment. But it is a great aid to
# build the script because it makes accessing the list contents ( = dataframes)
# very easy. I keep it commented.
# list2env(df, .GlobalEnv)

# Turn the content into a list of tibbles (instead of keeping it as a list of dataframes)
tibble_List <- df %>% map(as_tibble)

### Investigation about number of subjects (why some people are missing)
# Ok so we have 540 number of people here. (before we do the rest)
CompleteDatalolo <- reduce(tibble_List, full_join) 
CompleteDatalolo %>% group_by (Experiment.Version) %>% count(Participant.Private.ID)%>% tally() 
CompleteDatalolo %>% group_by (randomiser.d39u) %>% count(Participant.Private.ID)%>% tally() 


# Make a sublist of Emotions and Appraisals. Leave body maps
Appr_Emo_TbList <- keep(tibble_List, str_detect(names(tibble_List),
                                                '^Appraisal|^Emotions'))


#####################   EXTRACT ONLY RELEVANT ROWS   ###########################


### For appraisal and emotion tasks, extract only response rows.

# Get only rows with responses to the sliders, not initial slider values etc
Appr_Emo<- Appr_Emo_TbList %>% map(~filter(.x, Zone.Type == "response_slider_endValue"))

# Count the participants you have. And how many observations per participant you have. 
# Since we have 32 videos, this should be a 32 * nb of sliders in that task. 
measuresPerParticipant <-
  Appr_Emo %>% map(~group_by(.x, Participant.Private.ID)) %>%  map(count)

# Exclude participants from the dataset (some cause datadoubling, giving errors
# for the next parts)
Appr_Emo<-
  Appr_Emo %>% map(~filter(.x, !(Participant.Private.ID == 7319126)))

# Select relevant variables and extract them to another dataframe:
Appr_Emo <- Appr_Emo %>%
  map(~dplyr::select(.x,
   Participant.Private.ID,
    Participant.Public.ID, # For Amazon M Turk data: Participant.Private.ID,
    randomiser.d39u,
    VideoZoneSubset,
   Experiment.Version,
    VideoID = .x$VideoZoneSubset[1],
    Trial.Number,
    Zone.Name,
    Response
  ))

# Spread sliders 1 2 3 4 5 6 and 7 to different columns instead of being stacked in rows.
Appr_Emo <- Appr_Emo %>% map(~spread(.x, Zone.Name, Response))

# Make sure Trial Number and the sliders are all doubles. This is important for 
# time-series and correlations you may create on this data. 

# Sort thing data by Trial Number (so that time series make temporal sense) 
# (but make sure participant order is protected)

Appr_Emo <- Appr_Emo %>% 
  map(~mutate(.x, across(Trial.Number:ncol(.x), as.numeric))) %>% 
  map(~ arrange(.x, Participant.Private.ID, Trial.Number ))



#########################   Rename the sliders   ###############################
## For Appraisals 1 dataframes: (remember, we have 3 subsets of videos) 

# Make a sublist of Appraisals1.  
Appr1_TbList <- Appr_Emo %>% keep(str_detect(names(Appr_Emo), '^Appraisals1'))

Appr1_TbList <- Appr1_TbList %>% map(~rename(.x, 
    Pleasant = slider1,
    Unpleasant = slider2,
    Familiar = slider3,
    Unexpected = slider4,
    Comprehensible = slider5,
    Complex = slider6,
    Coping = slider7
  ))

Appr1_Binded <- Appr1_TbList %>% map_df(bind_rows) 
  
# Make a sublist of Appraisals2.  
Appr2_TbList <- Appr_Emo %>% keep(str_detect(names(Appr_Emo), '^Appraisals2'))

Appr2_TbList <- Appr2_TbList %>% map(~rename(.x, 
  Acceptable = slider1,
  Chance = slider2,
  PeopleCaused = slider3,
  Intentional = slider4,
  Relevant = slider5,
  Engaged = slider6,
  Wonder = slider7
  ))

Appr2_Binded <- Appr2_TbList %>% map_df(bind_rows) 

# Make a sublist of Appraisals3.  
Appr3_TbList <- Appr_Emo %>% keep(str_detect(names(Appr_Emo), '^Appraisals3'))


Appr3_TbList<- Appr3_TbList %>%  map(~rename(.x, 
  Shorten = slider1,
  Mask = slider2,
  Awake = slider3,
  PhysiolA = slider4,
  Calm = slider5,
  SexualA = slider6
  ))

Appr3_Binded <- Appr3_TbList %>% map_df(bind_rows) 


# Make a sublist of Emotions1
Emo1_TbList <- Appr_Emo %>% keep(str_detect(names(Appr_Emo), '^Emotions1'))

Emo1_TbList<- Emo1_TbList %>% map(~rename(.x, 
  Sadness = slider1,
  Anger = slider2,
  Fear = slider3,
  Disgust = slider4,
  Surprise = slider5,
  Embarassment = slider6,
  Enjoyment = slider7
))

Emo1_Binded <- Emo1_TbList %>% map_df(bind_rows) 


# Make a sublist of Emotions2
Emo2_TbList <- Appr_Emo %>% keep(str_detect(names(Appr_Emo), '^Emotions2'))

Emo2_TbList<- Emo2_TbList  %>% map(~rename(.x, 
  Curiosity = slider1,
  Awe = slider2,
  Confusion = slider3,
  Excitement = slider4,
  Boredom = slider5,
  Interest = slider6
))

Emo2_Binded <- Emo2_TbList %>% map_df(bind_rows) 


######################   GET FULL DATA IN ONE PLACE   ##########################

# Put each list of each Task_videoZoneSubsetX dataframe together (Each videoset separate) 

Appr_Emo <- append(Appr1_TbList, c(Appr2_TbList, 
                                   Appr3_TbList, 
                                   Emo1_TbList, 
                                   Emo2_TbList))

Appr_Emo_Long <- Appr_Emo %>% map(~pivot_longer(.x, cols= (!('Participant.Private.ID':'Trial.Number')), # 7:ncol(.x)), 
                                                names_to = "Dimension", 
                                                values_to = "Value" ))

# Put all task dataframes in one list. The dataframes have all video subsets. 

Appr_Emo_BindedVS <- list(Appr1_Binded, 
                          Appr2_Binded,
                          Appr3_Binded, 
                          Emo1_Binded, 
                          Emo2_Binded)

Appr_Emo_BindedVS_Long <- Appr_Emo_BindedVS %>% map(~pivot_longer(.x, cols=  (!('Participant.Private.ID':'Trial.Number')),
                    names_to = "Dimension", values_to = "Value" ))



### Create a full data file 

# FullData <- Appr_Emo_BindedVS[[1]] %>% reduce(full_join, by ="VideoID") ## GIVES AN UNSOLVABLE ISSUE

FullData <- reduce(Appr_Emo_BindedVS, full_join) 

FullData_Long <- FullData %>% pivot_longer( cols= !('Participant.Private.ID':'Trial.Number'), 
                                  names_to = "Dimension", 
                                  values_to = "Value" )

write_csv(FullData, "data/megalocalizer_full_data_wide.csv")

write_csv(FullData_Long, "data/megalocalizer_full_data_long.csv")

#########################   COUNT THE NUMBER OF PARTICIPANTS BY DATA VERSION  #################

FullData %>% group_by (Experiment.Version) %>% count(Participant.Private.ID)%>% tally() # WHY DO I HAVE 444 PARTICIPANTS INSTEAD OF 450 ? (body maps not included)



###########################    GET MEANS PER VIDEO OF EACH RATING #############


Means_Per_video <- FullData %>% group_by(VideoID) %>% 
  select(VideoID, Pleasant:ncol(FullData)) %>%
  summarize_all(~mean(.x,  na.rm = TRUE))

Means_Per_video_Long<- Means_Per_video %>% 
  pivot_longer( cols= (2:ncol(Means_Per_video)), 
                names_to = "Dimension", 
                values_to = "Value" )

write_csv(Means_Per_video, "data/Means_per_video.csv")


##########################    Participant Count   ##############################
FullData %>% count(Participant.Private.ID)%>% tally() # WHY DO I HAVE 444 PARTICIPANTS INSTEAD OF 450 ? (body maps not included)

## HOw many people we have in each task. (grouping across all videosets)
FullData %>% group_by(randomiser.d39u, Participant.Private.ID) %>% tally() %>% group_data()


###################   Quality Check   ##########################################


## Times-series plots
# Make visuals of each task across all participants + Standard Errors. 

##CAREFULL! VIDEOIDS ARE NOT SHOWN IN THE SAME ORDER ACROSS PARTICIPANTS SO DO 
# NOT USE TRIAL NUMBER FOR PLOTTING OR CORRELATIONS ETC (THEY DONT REFER TO THE SAME VIDEO)

# Let's plot the mean (= geom_line) across all subjects of each dimension (= facet) for 
# each video (x axis) and add the standard error around that mean (geom_ribbon)

myBinded <-Appr_Emo_BindedVS_Long %>% map(~group_by(.x, 
                                                    VideoID, 
                                                    Dimension, 
                                                    VideoZoneSubset)) 

FullDataMeanBinded <- myBinded %>%  map(~summarise(.x, 
                                              my_mean = mean(Value), 
                                              my_sd =sd(Value),  
                                              my_n = n(), 
                                              stdError = sd(Value)/sqrt(n()) ))



MeanSePlot <- FullDataMeanBinded %>% map(  ~ggplot(.x, aes(x = VideoID, y = my_mean, group=1)) +
  ylim(0,100)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2, angle=90)) +
  facet_wrap(~ Dimension) +
  geom_line() +
  geom_ribbon(aes(ymin = my_mean - stdError, ymax = my_mean + stdError), fill = "blue", alpha = 0.5))

print(MeanSePlot)


#### Attempting to do a function 


plot_dens_fun <- function(listed_df) {
  MeanSePlot  <-ggplot(listed_df, aes(x = VideoID, y = my_mean, group=1)) +
    ylim(0,100)+
    ylab("Mean Rating") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE,n.dodge = 1, angle=90)) +
    facet_wrap(~ Dimension) +
    geom_line() + 
    geom_ribbon(aes(ymin = my_mean - stdError, ymax = my_mean + stdError), fill = "blue", alpha = 0.5)+
    ggtitle(paste( "Time-series of Mean ratings of 95 videos. (3 sets of participants combined) "), subtitle = "Everybody sees the videos in a different order \n We have combined 3 subset of videos & participants in one timeseries. \nThe ribbon = +- 1 se ");
  
  print(MeanSePlot)
  print(FullDataMeanBinded)
}

pdf(file = "figures/TimeSeriesForEachTask.pdf",width = 11,height=8)

map(FullDataMeanBinded, ~plot_dens_fun(.x)) # Print it for all tasks

dev.off()

########################   HEAT MAPS   ########################################
# # install devtools
# install.packages("devtools")
# # use devtools to install superheat
# devtools::install_github("rlbarter/superheat")

heatFullData<- Means_Per_video %>%  data.matrix(rownames.force	=NA) 

row.names(heatFullData) <- Means_Per_video$VideoID

heatFullData<-heatFullData%>% t()

pdf(file = "figures/SuperHeatMap.pdf",width = 11,height=8)

superheat(heatFullData,
          # retain original order of rows/cols
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          
          # change the size of the labels
          left.label.text.size = 5,
          bottom.label.text.size = 4,
          
          bottom.label.text.angle = 90,

        
          clustering.method = "hierarchical",
          dist.method = "euclidean",
          bottom.label = "variable",

          title = "Superheat Map for All 95 Videos: Hierchical clustering with Euclidian Distance")

dev.off()


############################ Correlation matrix ###############################

# Used : https://rpkgs.datanovia.com/ggcorrplot/


# install.packages("ggcorrplot")


corr <- round(cor(select(Means_Per_video, -VideoID)), 1) # Correlation Matrix
head(corr[, 1:6])


# Compute a matrix of correlation p-values
p.mat <- cor_pmat(select(Means_Per_video, -VideoID)) # P-value Matrix of the corrs.
head(p.mat[, 1:4])


pdf(file = "figures/CorrPlot.pdf",width = 8,height=11)

# Leave blank on no significant coefficient
ggcorrplot(
  corr, 
  ggtheme = ggplot2::theme_classic,
  p.mat = p.mat,
  hc.order = TRUE,  # 
  sig.level = 0.05, # alpha of significance
  type = "full",    # Show all of the matrix instead of half
  insig = "blank",  # if insignificant result, draw in white
  lab = FALSE,      # Do not write the p. values on the matrix
  lab_col = "black", # If you write the p-values, write them in this color
  outline.color = "white",   # The borders of the correlation matrix
  lab_size = 1,  # Size of the p values
  title = "Correlation matrix", # Title of the Correlation Matrix
  tl.cex = 10 # The size of the variable names
)

dev.off()


######################   Scatterplot Matrix   #################################


ggpairs(select(Means_Per_video, Pleasant:8))

ggpairs(select(Means_Per_video, 9:15))

ggpairs(select(Means_Per_video, 15:21))

ggpairs(select(Means_Per_video, 22:28))

ggpairs(select(Means_Per_video, 29:34))


##############################   BOXPLOT   #####################################

pdf(file = "figures/Boxplot.pdf",width = 8,height=11)

ggplot(Means_Per_video_Long, aes(x= reorder(Dimension, Value, mean), y = Value)) + #Ok this looks useful
  coord_flip() +
  xlab("Dimensions")+ 
  ggtitle("Boxplots of each dimension ordered by their means" )+
  geom_boxplot()

dev.off()


####################   COWPLOT PAGE , DENSITY PLOT   ###########################
library(ggplot2)
# install.packages("cowplot")
library(cowplot)
# minimal horizontal grid theme

ggplot(FullData_Long, aes(Value, fill = Dimension)) + 
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggtitle("DensityPlot", subtitle = "Everybody sees the videos in a different order") + 
  theme_minimal_hgrid(12)



#####violinplot


ggplot(Means_Per_video_Long, aes(x=reorder(Dimension, Value, mean), y=Value)) + 
  geom_violin(trim=TRUE)+ 
  coord_flip()+ 
  stat_summary(fun=mean, geom="point", shape=23, size=2) +
  stat_summary(fun=median, geom="point", size=2, color="red") 



### Density plots according to dimensions.

ggplot(Means_Per_video_Long, aes(fill= Dimension, y=Value)) + 
  geom_density(alpha = 0.5) +
  facet_wrap(~ Dimension) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggtitle(paste("The Density Plots of each dimension, means per video"), subtitle = "Everybody sees the videos in a different order") + 
  theme_minimal_hgrid(12)


