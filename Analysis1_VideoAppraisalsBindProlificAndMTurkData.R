# Analysis1_VideosAppraisalsBindProlificAndMTurkData.R 

################################   OWNERSHIP   #################################
# This script is written by Asli Erdemli.
# Last updated in Aug 2025, adapted from a real project which was scripted in Nov 2022. 
# The real project's data and information was hidden or replaced.
# However, this script has kept specifics to what one might expect from raw data collected and downloaded from Gorilla and Prolific, the online  experiment buider.    

##############################   DATASET   #####################################
# Among other variables, the dataset contains the following variables of interest: 
# - appraisals (3 different sets)
# - emotions (2 different sets)
# - bodymaps

# There are different versions of the dataset in Gorilla which need to be merged. 
# Some participants are collected via Prolific while others from Amazon MTurk. 

##############################   PURPOSE   #####################################

# Videos Appraisals Study: Join Prolific and Amazon MTurk data. 

# This study was partly collected on Amazon M Turk and partly on Prolific. 
# This script's purpose is to bind the two dataset together so as to have all 540
# participants' data together. 

# The data was collected in different batches. 
# In MTurk + Cloud Research : 18 + 162 + 289 + 22 = 491 /540. All of them are in 
# Gorilla data version 22
# In Prolific : 54 participants more = 545/540 (I don't know yet why the surplus
# but we have 540 people on Gorilla). All of these are in Gorilla data version 24. 

# The goal here is to create on big dataset from all these data collections.

################################################################################

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() # Will empty memory



# Set main working directory to folder within all your separate CSV files
setwd("C:/Users/erdemli/Documents/LeProject")


# Set Library for this project
.libPaths("VideosProjectLib")

library(tidyverse)
library(magrittr)


########################   IMPORT DATA   #######################################

# Where is all your Gorilla data? Should contain all task files, starting with "data_"
DataFileList <- list("data/raw_data/Datav24", "data/raw_data/Datav22", "data/raw_data/Datav21")

# Let's load all tasks from both versions. 
for (iVersion in DataFileList){
  
  DataFile <- iVersion
  
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
    
    # Read csv file as dataframe
    rawData <- read.csv(iFile)
    
    # If the file isn't empty, then a node name can be get from the "Tree Node Key"
    # column of our rawData.
    tryCatch(
      expr = {
        # Get name of the task node
        nodeName <- rawData[1, "Task.Name"]
        VSversion <- rawData[1, "VideoZoneSubset"]
        GorillaExperVersion <- as.character(rawData[1, "Experiment.Version"])
        nodeName <- paste(nodeName, VSversion, GorillaExperVersion, sep="_", collapse= NULL)
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
}


##########################   JOIN THE DIFFERENT VERSIONS OF EACH TASK ################

# Get all appraisals and body maps and emotions in a list. (Emotions are under appraisal task 4 and 5) 
relevantfiles <- lapply(ls(pattern = "^appraisal|^Video"), get)

# Remove all except the relevant files for clarity and memory usage.
# rm(list = grep(pattern = "relevantfiles|^appraisal|^Video", ls(), value = TRUE, invert = TRUE))
rm(list = grep(pattern = "relevantfiles", ls(), value = TRUE, invert = TRUE))

### Create a function to extract Task.Name, randomizer.d39u (which contains real 
# task content), Experiment version, and tree node key of each relevantfiles element. 
myfunction <- function(x){
  x %>% select( Task.Name, randomiser.d39u, VideoZoneSubset, Experiment.Version, Tree.Node.Key) %>% slice(1)
}

task_file_info <- relevantfiles %>% map_df( myfunction)


### Write all relevant files with all different versions in a file in csv format. 

#Let's write the files in csv according to the task_file_info names in a file. 
renameFileAndWrite_function <- function (x, filelocation){
  file_path <- paste(filelocation, x$randomiser.d39u[1], x$VideoZoneSubset[1], x$Experiment.Version[1], ".csv", sep = "")
  file_path <- gsub(" ", "", file_path)
  write_csv(x, file=file_path)
}

dir.create("data/subfiles_eachTask_each_VideoSubset_eachDataVersion/")
FileLocation = "data/subfiles_eachTask_each_VideoSubset_eachDataVersion/"

relevantfiles %>% walk(~ renameFileAndWrite_function( .x ,  filelocation = FileLocation))



### Now, let us create sublists in relevant files, according to Tree.Node.Key

# Rename each dataframe in relevantfiles list by their Tree.Node.Key
names(relevantfiles) <- relevantfiles %>% map(pluck(~.$Tree.Node.Key[1])) 

# Bind together (by rows) all dataframes in relevantfiles list having the same name.  
relevantfilesBinded <- map(split(relevantfiles, names(relevantfiles)), bind_rows)



### Let's rename the sublists more explicitly 

# function to rename dfs by getting their randomiser.d39u and VideoZoneSubset together and getting rid of spaces
rename_dfs <- function(x){
  file_name <- paste( x$randomiser.d39u[1], x$VideoZoneSubset[1], sep = "_")
  file_name <- gsub(" ", "", file_name)
}

# rename the Binded relevant files with these names. This way we have something 
# like Appraisal1_VideoZoneSubset instead of task-9zddv as names of dataframes
# listed in relevantfilesBinded . 
names(relevantfilesBinded) <- relevantfilesBinded %>% map(rename_dfs) 


### Let's try to get these merged dataframes out 

# Remove from your environment all variables that don't contain relevantfiles in their name
rm(list = grep(pattern = "relevantfiles", ls(), value = TRUE, invert = TRUE))
# Get your list elements out in your environment. This is usually unrecommended 
# because it untidies and overloads your environment but it allows you to access
# the dataframes easily therefore useful for script building. I keep it commented. 
# list2env(relevantfilesBinded, .GlobalEnv)

### Write all relevant files with both versions in a file in csv format. 

#Let's write the files in csv according to the task_file_info names in a file. 
renameFileAndWrite_function <- function (x, filelocation){
  file_path <- paste(filelocation, x$randomiser.d39u[1], x$VideoZoneSubset[1], ".csv", sep = "")
  file_path <- gsub(" ", "", file_path)
  write_csv(x, file=file_path)
}

dir.create("data/Mergedfiles_byDataVersion/")
FileLocation = "data/Mergedfiles_byDataVersion/"

relevantfilesBinded %>% walk(~ renameFileAndWrite_function( .x ,  filelocation = FileLocation))

### Save this list of all dataframes we are interested in so that it can be used in other scripts.  
saveRDS(relevantfilesBinded, file="data/VideosFullDataDFs.Rds")



#########################   COUNT THE NUMBER OF PARTICIPANTS BY DATA VERSION  #################

### Now, let us create sublists in relevant files, according to Experiment Version

# Create copy of relevantfiles to study Dataversions
relevantfiles_byDataVersion <- relevantfiles

# Rename each dataframe in relevantfiles_byDataVersion list by their Experiment.Version
names(relevantfiles_byDataVersion) <- relevantfiles_byDataVersion %>% map(pluck(~.$Experiment.Version[1])) 

# Bind together (by rows) all dataframes in relevantfiles_byDataVersion list having the same name (= DataVersion).  
relevantfilesBinded_byDataVersion <- map(split(relevantfiles_byDataVersion, names(relevantfiles_byDataVersion)), bind_rows)

# For some reason this gives one more person in each data version (fixed!)
lolo <- relevantfilesBinded_byDataVersion %>% map(~group_by(., Participant.Private.ID)) %>% map(~tally(.)) %>% map(~group_data(.))