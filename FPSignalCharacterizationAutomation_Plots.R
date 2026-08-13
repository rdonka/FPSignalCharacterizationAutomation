#################### ADOLESCENT MORPHINE EXPOSURE RF ICSS TRANSIENSTS ANALYSIS #########################

rootgithubpath <- c('C:/Users/rmdon/Desktop/GitHub_MyRepositories/') # Path for Rachel's laptop to github repositories
rootboxpath <- c('C:/Users/rmdon/Box/') # Path for Rachel's laptop to Box drive

# Load libraries we will need
source(paste(rootgithubpath,'Analysis-FP/Functions_R/Functions_LoadPackages.R',sep=''))
library(caTools)
library(randomForest)
library(glmnet)
library(pdp)
library(caret)


# Load plot settings
source(paste(rootgithubpath,"Analysis-FP/Functions_R/Functions_SetPlotVariables.R",sep=''))

# Set working directory to where the data is saved
setwd(paste(rootboxpath, 'Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/',sep=''))

# PREPARE DATA -----

## Read in data and prep variables -----
subjectvariables <- read_csv('FPSignalCharacterizationAutomation_ExperimentalVariables.csv')
data_raw <- read_csv('FPSignalCharacterizationAutomation_AllQuantificationVariables.csv')

# Merge subject variables with data
data <- left_join(data_raw, subjectvariables, by = "SubjectID")


## Split data into train and test - for each subject, randomly assign split 1 and 2 to test and train
# 50-50 to train and test by Split
#data <- data %>% 
#  group_by(SubjectID) %>%
#  mutate(train = sample(Split, 1)) %>%  # randomly choose one session per subject for training
#  ungroup() %>%
#  mutate(Set = ifelse(Split == train, "Train", "Test")) %>%
#  select(-train)

# 30-70 to train and test by subject and virus
set.seed(123)

# Step 1: Create subject-level summary for stratification
subject_summary <- data %>%
  group_by(SubjectID, Sensor) %>%
  summarise(
    Signal = first(Signal_RDScored),  # assumes both sessions have same Signal
    .groups = "drop"
  )

# Step 2: Stratified sample of SubjectIDs
train_subjects <- subject_summary %>%
  group_by(Sensor, Signal) %>%
  sample_frac(0.5) %>%
  pull(SubjectID)

# Step 3: Assign Set based on SubjectID
data_split <- data %>%
  mutate(Set = ifelse(SubjectID %in% train_subjects, "Train", "Test"))

# Pivot data to wide
data_wide <- data_split %>%
  pivot_wider(
    id_cols = c(SubjectID, Split, Set, Sex, FiberPlacement, Sensor, Rig, Power, Signal_RDScored, Signal_RDQuality, baqscalingfactor),  # keep these constant
    names_from = Variable,
    values_from = c(sig, baq, baqscaled, sigsub, sigfilt, sigtobaq, sigtobaqscaled)
  )

# Factor categorical variables
data_wide$Split <- factor(data_wide$Split)
data_wide$Sex <- factor(data_wide$Sex)
data_wide$FiberPlacement <- factor(data_wide$FiberPlacement, levels=c('VTA','NAcLS'))
data_wide$Sensor <- factor(data_wide$Sensor, levels=c('GCaMP6f','GRABDA2H','GRABDA3M','dLight1.3b'))
data_wide$Rig <- factor(data_wide$Rig)
data_wide$Signal <- factor(data_wide$Signal_RDScored, levels=c('N','Y'))
data_wide$SignalQuality <- factor(data_wide$Signal_RDQuality)


## Analysis Variables -----
IDvariables <- c('SubjectID','Set','Split','Signal','SignalQuality')
includevariables <- c('baqscalingfactor', 'sig_range', 'sig_range_bins', 'sig_rms_bins', 'sig_MagSum_Band1', 'sig_MagSum_Band2','sig_MagSum_Band3',
                      'baq_range', 'baq_range_bins', 'baq_rms_bins', 'baq_MagSum_Band1', 'baq_MagSum_Band2','baq_MagSum_Band3',
                      'baqscaled_range', 'baqscaled_range_bins', 'baqscaled_rms_bins', 'baqscaled_MagSum_Band1', 'baqscaled_MagSum_Band2','baqscaled_MagSum_Band3',
                      'sigsub_range', 'sigsub_range_bins', 'sigsub_rms_bins', 'sigsub_MagSum_Band1', 'sigsub_MagSum_Band2','sigsub_MagSum_Band3',
                      'sigfilt_range', 'sigfilt_range_bins', 'sigfilt_rms_bins', 'sigfilt_MagSum_Band1',
                      'sigtobaq_mean_bins', 'sigtobaq_MagSum_Band1', 'sigtobaq_MagSum_Band2', 'sigtobaq_MagSum_Band3',
                      'sigtobaqscaled_mean_bins', 'sigtobaqscaled_MagSum_Band1', 'sigtobaqscaled_MagSum_Band2', 'sigtobaqscaled_MagSum_Band3')


modeldata <- data_wide %>% select(all_of(IDvariables), all_of(includevariables))

# RANDOM FOREST -----
## Prepate train and test data -----
# Remove extra columns
RFdata <- modeldata %>%
  select(-SignalQuality)

# Create training and testing sets
# RFdata_train <- RFdata %>% filter(Set == "Train") %>% select(-Set)
# RFdata_test  <- RFdata %>% filter(Set == "Test")%>% select(-Set)
RFdata_train <- RFdata %>% filter(Split == 1) %>% select(-Set, -Split)
RFdata_test  <- RFdata %>% filter(Split == 2)%>% select(-Set, -Split)




# MEANS -----




# PLOT -----
