# FP Signal Charachterization Automation Application Script

# PREPARE ENVIRONMENT -----
# Load libraries
library(caTools)
library(randomForest)
library(glmnet)
library(pdp)
library(caret)
library(pROC)
library(tidyr)
library(rsample)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(emmeans)
library(patchwork)
library(miceadds)
library(ggprism)
library(tibble)
library(cowplot)

# Set up paths
rootgithubpath <- c('C:/Users/rmdon/Desktop/GitHub_MyRepositories/') # Path to github repositories
rootboxpath <- c('C:/Users/rmdon/Box/') # Path to Box drive

outputpath <- c(paste(rootboxpath,'Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/',
                      'FPSignalInclusionOutput_Project_Dataset.csv', sep='')) # Path and file name to save the output of the classification model application
# Prepare file paths

# Model file path - GitHub repository plus model file name
filepath_model <-  paste(rootgithubpath,'FPSignalCharacterizationAutomation/Model Output/FPSignalCharachterizationAutomation_ElasticNetModel.rds',sep='')

# MATLAB Quantification Output containing the variables used by the model
filepath_quantificationvariables <- paste(rootboxpath,'Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/',
                                          'FPSignalCharacterizationAutomation_AllQuantificationVariables.csv', sep='')

# MATLAB output of experimental variables including Sensor and FiberPlacement
filepath_experimentalvariables <- paste(rootboxpath,'Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/',
                                        'FPSignalCharacterizationAutomation_ExperimentalVariables.csv', sep='')



# PREPARE DATA -----
# Read in quantification data and experimental variables
data_raw <- read_csv(filepath_quantificationvariables)
subjectvariables <- read_csv(filepath_experimentalvariables)

# Combine and format data into wide frame
data <- preprocessFPdata(data_raw, subjectvariables)
  

# Apply model and generate output
modeloutput <- predictFPsignal(data, filepath_model, threshold = 0.5, return_full = TRUE)
  
view(modeloutput)

write.csv(modeloutput, outputpath, row.names = FALSE)
