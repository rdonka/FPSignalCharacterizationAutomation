#################### ADOLESCENT MORPHINE EXPOSURE RF ICSS TRANSIENSTS ANALYSIS #########################

rootgithubpath <- c('C:/Users/rmdon/Desktop/GitHub_MyRepositories/') # Path for Rachel's laptop to github repositories
rootboxpath <- c('C:/Users/rmdon/Box/') # Path for Rachel's laptop to Box drive

# Load libraries we will need
library(caTools)
library(randomForest)
library(glmnet)
library(pdp)
library(caret)
library(pROC)
library(tidyr)
library(rsample)

# Load plot settings
#source(paste(rootgithubpath,"Analysis-FP/Functions_R/Functions_SetPlotVariables.R",sep=''))

# Set working directory to where the data is saved
setwd(paste(rootboxpath, 'Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/',sep=''))

# PREPARE DATA -----

## Read in data and prep variables -----
subjectvariables <- read_csv('FPSignalCharacterizationAutomation_ExperimentalVariables.csv')
data_raw <- read_csv('FPSignalCharacterizationAutomation_AllQuantificationVariables.csv')

# Merge subject variables with data
data <- left_join(data_raw, subjectvariables, by = "SubjectID")  %>% filter(Sensor != 'GRABDA3M')


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
data_wide$Sensor <- factor(data_wide$Sensor, levels=c('GCaMP6f','GRABDA2H','dLight1.3b'))
data_wide$Rig <- factor(data_wide$Rig)
data_wide$Signal <- factor(data_wide$Signal_RDScored, levels=c('N','Y'))
data_wide$SignalQuality <- factor(data_wide$Signal_RDQuality)


# Scale ratios for plots
data_wide$sigtobaqscaled_mean_bins_scaled <- data_wide$sigtobaqscaled_mean_bins-1
data_wide$sigtobaq_MagSum_Band1_scaled <- data_wide$sigtobaq_MagSum_Band1-1
data_wide$sigtobaq_MagSum_Band2_scaled <- data_wide$sigtobaq_MagSum_Band2-1
data_wide$sigtobaq_MagSum_Band3_scaled <- data_wide$sigtobaq_MagSum_Band3-1
data_wide$sigtobaqscaled_MagSum_Band1_scaled <- data_wide$sigtobaqscaled_MagSum_Band1-1
data_wide$sigtobaqscaled_MagSum_Band2_scaled <- data_wide$sigtobaqscaled_MagSum_Band2-1
data_wide$sigtobaqscaled_MagSum_Band3_scaled <- data_wide$sigtobaqscaled_MagSum_Band3-1

## Analysis Variables -----
IDvariables <- c('SubjectID','Sensor','Set','Split','Signal','SignalQuality')
# includevariables <- c('sig_range', 'sig_range_bins', 'sig_rms_bins', 'sig_MagSum_Band1', 'sig_MagSum_Band2','sig_MagSum_Band3',
#                       'baq_range', 'baq_range_bins', 'baq_rms_bins', 'baq_MagSum_Band1', 'baq_MagSum_Band2','baq_MagSum_Band3',
#                       'baqscaled_range', 'baqscaled_range_bins', 'baqscaled_rms_bins', 'baqscaled_MagSum_Band1', 'baqscaled_MagSum_Band2','baqscaled_MagSum_Band3',
#                       'sigsub_range', 'sigsub_range_bins', 'sigsub_rms_bins', 'sigsub_MagSum_Band1', 'sigsub_MagSum_Band2','sigsub_MagSum_Band3',
#                       'sigfilt_range', 'sigfilt_range_bins', 'sigfilt_rms_bins', 'sigfilt_MagSum_Band1',
#                       'sigtobaq_mean_bins', 'sigtobaq_MagSum_Band1', 'sigtobaq_MagSum_Band2', 'sigtobaq_MagSum_Band3',
#                       'sigtobaqscaled_mean_bins', 'sigtobaqscaled_MagSum_Band1', 'sigtobaqscaled_MagSum_Band2', 'sigtobaqscaled_MagSum_Band3')

includevariables <- c('sig_range_bins', 'sig_rms_bins', 'sig_MagSum_Band1', 'sig_MagSum_Band2','sig_MagSum_Band3',
                      'baq_range_bins', 'baq_rms_bins', 'baq_MagSum_Band1', 'baq_MagSum_Band2','baq_MagSum_Band3',
                      'baqscaled_range_bins', 'baqscaled_rms_bins', 'baqscaled_MagSum_Band1', 'baqscaled_MagSum_Band2','baqscaled_MagSum_Band3',
                      'sigsub_range_bins', 'sigsub_rms_bins', 'sigsub_MagSum_Band1', 'sigsub_MagSum_Band2','sigsub_MagSum_Band3',
                      'sigfilt_range_bins', 'sigfilt_rms_bins', 'sigfilt_MagSum_Band1',
                      'sigtobaq_mean_bins', 'sigtobaq_MagSum_Band1', 'sigtobaq_MagSum_Band2', 'sigtobaq_MagSum_Band3',
                      'sigtobaqscaled_mean_bins_scaled', 'sigtobaq_MagSum_Band1_scaled', 'sigtobaq_MagSum_Band2_scaled', 'sigtobaq_MagSum_Band3_scaled',
                      'sigtobaqscaled_MagSum_Band1_scaled', 'sigtobaqscaled_MagSum_Band2_scaled', 'sigtobaqscaled_MagSum_Band3_scaled')


modeldata <- data_wide %>% select(all_of(IDvariables), all_of(includevariables))

# Raw Variable Plots -----

modeldata_signalmeans <- modeldata %>%
  group_by(Signal) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop" # Drops the grouping structure afterwards
  )
    
modeldata_signalmeans   
  

modeldata_sensorsignalmeans <- modeldata %>%
  group_by(Sensor, Signal) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop" # Drops the grouping structure afterwards
  )

modeldata_sensorsignalmeans   


modeldata_signalqualitymeans <- modeldata %>%
  group_by(Signal, SignalQuality) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop" # Drops the grouping structure afterwards
  )

modeldata_signalqualitymeans   


modeldata_sensorsignalqualitymeans <- modeldata %>%
  group_by(Sensor, Signal, SignalQuality) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop" # Drops the grouping structure afterwards
  )

modeldata_sensorsignalqualitymeans   


## Plots
## Signal Range
dv_sig_range_bins <- 'sig_range_bins'
iv_sig_range_bins <- 'Signal'

ymin_sig_range_bins <- 0
ymax_sig_range_bins <- 170
yticks_sig_range_bins <- 40
ybreaks_sig_range_bins <- seq(ymin_sig_range_bins,ymax_sig_range_bins,yticks_sig_range_bins)

xbreaklabels_sig_range_bins <- c('N','Y')

ylabel_sig_range_bins <- 'Raw Signal Range'
xlabel_sig_range_bins <- 'Signal (Y/N)'

plottitle_sig_range_bins <- 'Mean Raw Signal Stream Range'
plotsubtitle_sig_range_bins <- 'Raw signal range is greater in rats with signal.'

signalbar_sig_range_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sig_range_bins, iv_sig_range_bins, 
                                     ymin_sig_range_bins, ymax_sig_range_bins, ybreaks_sig_range_bins, 
                                     xbreaklabels_sig_range_bins, ylabel_sig_range_bins, xlabel_sig_range_bins, 
                                     plottitle_sig_range_bins, plotsubtitle_sig_range_bins)
  
signalbar_sig_range_bins
  

## Signal RMS
dv_sig_rms_bins <- 'sig_rms_bins'
iv_sig_rms_bins <- 'Signal'

ymin_sig_rms_bins <- 0
ymax_sig_rms_bins <- 1000
yticks_sig_rms_bins <- 250
ybreaks_sig_rms_bins <- seq(ymin_sig_rms_bins,ymax_sig_rms_bins,yticks_sig_rms_bins)

xbreaklabels_sig_rms_bins <- c('N','Y')

ylabel_sig_rms_bins <- 'Raw Signal RMS'
xlabel_sig_rms_bins <- 'Signal (Y/N)'

plottitle_sig_rms_bins <- 'Mean Raw Signal Stream Root Mean Squared'
plotsubtitle_sig_rms_bins <- 'Raw signal RMS is greater in rats with signal.'

signalbar_sig_rms_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sig_rms_bins, iv_sig_rms_bins, 
                                                        ymin_sig_rms_bins, ymax_sig_rms_bins, ybreaks_sig_rms_bins, 
                                                        xbreaklabels_sig_rms_bins, ylabel_sig_rms_bins, xlabel_sig_rms_bins, 
                                                        plottitle_sig_rms_bins, plotsubtitle_sig_rms_bins)

signalbar_sig_rms_bins



## Scaled Background Range
dv_baqscaled_range_bins <- 'baqscaled_range_bins'
iv_baqscaled_range_bins <- 'Signal'

ymin_baqscaled_range_bins <- 0
ymax_baqscaled_range_bins <- 150
yticks_baqscaled_range_bins <- 30
ybreaks_baqscaled_range_bins <- seq(ymin_baqscaled_range_bins,ymax_baqscaled_range_bins,yticks_baqscaled_range_bins)

xbreaklabels_baqscaled_range_bins <- c('N','Y')

ylabel_baqscaled_range_bins <- 'Scaled Background Range'
xlabel_baqscaled_range_bins <- 'Signal (Y/N)'

plottitle_baqscaled_range_bins <- 'Mean Scaled Background Range'
plotsubtitle_baqscaled_range_bins <- 'Scaled background range is greater in rats with signal.'

signalbar_baqscaled_range_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_baqscaled_range_bins, iv_baqscaled_range_bins, 
                                                        ymin_baqscaled_range_bins, ymax_baqscaled_range_bins, ybreaks_baqscaled_range_bins, 
                                                        xbreaklabels_baqscaled_range_bins, ylabel_baqscaled_range_bins, xlabel_baqscaled_range_bins, 
                                                        plottitle_baqscaled_range_bins, plotsubtitle_baqscaled_range_bins)

signalbar_baqscaled_range_bins


## Scaled Background RMS
dv_baqscaled_rms_bins <- 'baqscaled_rms_bins'
iv_baqscaled_rms_bins <- 'Signal'

ymin_baqscaled_rms_bins <- 0
ymax_baqscaled_rms_bins <- 1000
yticks_baqscaled_rms_bins <- 250
ybreaks_baqscaled_rms_bins <- seq(ymin_baqscaled_rms_bins,ymax_baqscaled_rms_bins,yticks_baqscaled_rms_bins)

xbreaklabels_baqscaled_rms_bins <- c('N','Y')

ylabel_baqscaled_rms_bins <- 'Scaled Background RMS'
xlabel_baqscaled_rms_bins <- 'Signal (Y/N)'

plottitle_baqscaled_rms_bins <- 'Mean Scaled Background Stream Root Mean Squared'
plotsubtitle_baqscaled_rms_bins <- 'Scaled background RMS is greater in rats with signal.'

signalbar_baqscaled_rms_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_baqscaled_rms_bins, iv_baqscaled_rms_bins, 
                                                        ymin_baqscaled_rms_bins, ymax_baqscaled_rms_bins, ybreaks_baqscaled_rms_bins, 
                                                        xbreaklabels_baqscaled_rms_bins, ylabel_baqscaled_rms_bins, xlabel_baqscaled_rms_bins, 
                                                        plottitle_baqscaled_rms_bins, plotsubtitle_baqscaled_rms_bins)

signalbar_baqscaled_rms_bins


## Subtracted Signal RMS
dv_sigsub_rms_bins <- 'sigsub_rms_bins'
iv_sigsub_rms_bins <- 'Signal'

ymin_sigsub_rms_bins <- 0
ymax_sigsub_rms_bins <- 21
yticks_sigsub_rms_bins <- 5
ybreaks_sigsub_rms_bins <- seq(ymin_sigsub_rms_bins,ymax_sigsub_rms_bins,yticks_sigsub_rms_bins)

xbreaklabels_sigsub_rms_bins <- c('N','Y')

ylabel_sigsub_rms_bins <- 'Subtracted Signal RMS'
xlabel_sigsub_rms_bins <- 'Signal (Y/N)'

plottitle_sigsub_rms_bins <- 'Mean Subtracted Signal Stream Root Mean Squared'
plotsubtitle_sigsub_rms_bins <- 'Subtracted signal RMS is greater in rats with signal.'

signalbar_sigsub_rms_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigsub_rms_bins, iv_sigsub_rms_bins, 
                                                      ymin_sigsub_rms_bins, ymax_sigsub_rms_bins, ybreaks_sigsub_rms_bins, 
                                                      xbreaklabels_sigsub_rms_bins, ylabel_sigsub_rms_bins, xlabel_sigsub_rms_bins, 
                                                      plottitle_sigsub_rms_bins, plotsubtitle_sigsub_rms_bins)

signalbar_sigsub_rms_bins

## Subtracted Signal Range
dv_sigsub_range_bins <- 'sigsub_range_bins'
iv_sigsub_range_bins <- 'Signal'

ymin_sigsub_range_bins <- 0
ymax_sigsub_range_bins <- 110
yticks_sigsub_range_bins <- 25
ybreaks_sigsub_range_bins <- seq(ymin_sigsub_range_bins,ymax_sigsub_range_bins,yticks_sigsub_range_bins)

xbreaklabels_sigsub_range_bins <- c('N','Y')

ylabel_sigsub_range_bins <- 'Subtracted Signal Range'
xlabel_sigsub_range_bins <- 'Signal (Y/N)'

plottitle_sigsub_range_bins <- 'Mean Subtracted Signal Stream Range'
plotsubtitle_sigsub_range_bins <- 'Subtracted signal range is greater in rats with signal.'

signalbar_sigsub_range_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigsub_range_bins, iv_sigsub_range_bins, 
                                                           ymin_sigsub_range_bins, ymax_sigsub_range_bins, ybreaks_sigsub_range_bins, 
                                                           xbreaklabels_sigsub_range_bins, ylabel_sigsub_range_bins, xlabel_sigsub_range_bins, 
                                                           plottitle_sigsub_range_bins, plotsubtitle_sigsub_range_bins)

signalbar_sigsub_range_bins



## Subtracted Signal Magnitude - Band 1
dv_sigsub_MagSum_Band1_bins <- 'sigsub_MagSum_Band1'
iv_sigsub_MagSum_Band1_bins <- 'Signal'

ymin_sigsub_MagSum_Band1_bins <- 0
ymax_sigsub_MagSum_Band1_bins <- 530
yticks_sigsub_MagSum_Band1_bins <- 125
ybreaks_sigsub_MagSum_Band1_bins <- seq(ymin_sigsub_MagSum_Band1_bins,ymax_sigsub_MagSum_Band1_bins,yticks_sigsub_MagSum_Band1_bins)

xbreaklabels_sigsub_MagSum_Band1_bins <- c('N','Y')

ylabel_sigsub_MagSum_Band1_bins <- 'Total Magnitude (0.0051 to 2.5 Hz)'
xlabel_sigsub_MagSum_Band1_bins <- 'Signal (Y/N)'

plottitle_sigsub_MagSum_Band1_bins <- 'Mean Subtracted Signal Stream Magnitude - 0.0051 to 2.5 Hz'
plotsubtitle_sigsub_MagSum_Band1_bins <- 'Subtracted signal magnitude in the low frequency band is greater in rats with signal.'

signalbar_sigsub_MagSum_Band1_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigsub_MagSum_Band1_bins, iv_sigsub_MagSum_Band1_bins, 
                                                           ymin_sigsub_MagSum_Band1_bins, ymax_sigsub_MagSum_Band1_bins, ybreaks_sigsub_MagSum_Band1_bins, 
                                                           xbreaklabels_sigsub_MagSum_Band1_bins, ylabel_sigsub_MagSum_Band1_bins, xlabel_sigsub_MagSum_Band1_bins, 
                                                           plottitle_sigsub_MagSum_Band1_bins, plotsubtitle_sigsub_MagSum_Band1_bins)

signalbar_sigsub_MagSum_Band1_bins

## Subtracted Signal Magnitude - Band 2
dv_sigsub_MagSum_Band2_bins <- 'sigsub_MagSum_Band2'
iv_sigsub_MagSum_Band2_bins <- 'Signal'

ymin_sigsub_MagSum_Band2_bins <- 0
ymax_sigsub_MagSum_Band2_bins <- 175
yticks_sigsub_MagSum_Band2_bins <- 40
ybreaks_sigsub_MagSum_Band2_bins <- seq(ymin_sigsub_MagSum_Band2_bins,ymax_sigsub_MagSum_Band2_bins,yticks_sigsub_MagSum_Band2_bins)

xbreaklabels_sigsub_MagSum_Band2_bins <- c('N','Y')

ylabel_sigsub_MagSum_Band2_bins <- 'Total Magnitude (2.5 to 10 Hz)'
xlabel_sigsub_MagSum_Band2_bins <- 'Signal (Y/N)'

plottitle_sigsub_MagSum_Band2_bins <- 'Mean Subtracted Signal Stream Magnitude - 2.5 to 10 Hz'
plotsubtitle_sigsub_MagSum_Band2_bins <- 'Subtracted signal magnitude in the medium frequency band is greater in rats with signal.'

signalbar_sigsub_MagSum_Band2_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigsub_MagSum_Band2_bins, iv_sigsub_MagSum_Band2_bins, 
                                                                  ymin_sigsub_MagSum_Band2_bins, ymax_sigsub_MagSum_Band2_bins, ybreaks_sigsub_MagSum_Band2_bins, 
                                                                  xbreaklabels_sigsub_MagSum_Band2_bins, ylabel_sigsub_MagSum_Band2_bins, xlabel_sigsub_MagSum_Band2_bins, 
                                                                  plottitle_sigsub_MagSum_Band2_bins, plotsubtitle_sigsub_MagSum_Band2_bins)

signalbar_sigsub_MagSum_Band2_bins



## Subtracted Signal Magnitude - Band 3
dv_sigsub_MagSum_Band3_bins <- 'sigsub_MagSum_Band3'
iv_sigsub_MagSum_Band3_bins <- 'Signal'

ymin_sigsub_MagSum_Band3_bins <- 0
ymax_sigsub_MagSum_Band3_bins <- 80
yticks_sigsub_MagSum_Band3_bins <- 20
ybreaks_sigsub_MagSum_Band3_bins <- seq(ymin_sigsub_MagSum_Band3_bins,ymax_sigsub_MagSum_Band3_bins,yticks_sigsub_MagSum_Band3_bins)

xbreaklabels_sigsub_MagSum_Band3_bins <- c('N','Y')

ylabel_sigsub_MagSum_Band3_bins <- 'Total Magnitude (10 to 100 Hz)'
xlabel_sigsub_MagSum_Band3_bins <- 'Signal (Y/N)'

plottitle_sigsub_MagSum_Band3_bins <- 'Mean Subtracted Signal Stream Magnitude - 10 to 100 Hz'
plotsubtitle_sigsub_MagSum_Band3_bins <- 'Subtracted signal magnitude in the high frequency band is greater in rats with signal.'

signalbar_sigsub_MagSum_Band3_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigsub_MagSum_Band3_bins, iv_sigsub_MagSum_Band3_bins, 
                                                                  ymin_sigsub_MagSum_Band3_bins, ymax_sigsub_MagSum_Band3_bins, ybreaks_sigsub_MagSum_Band3_bins, 
                                                                  xbreaklabels_sigsub_MagSum_Band3_bins, ylabel_sigsub_MagSum_Band3_bins, xlabel_sigsub_MagSum_Band3_bins, 
                                                                  plottitle_sigsub_MagSum_Band3_bins, plotsubtitle_sigsub_MagSum_Band3_bins)

signalbar_sigsub_MagSum_Band3_bins


## Raw signal to scaled background mean
dv_sigtobaqscaled_mean_bins_scaled <- 'sigtobaqscaled_mean_bins_scaled'
iv_sigtobaqscaled_mean_bins_scaled <- 'Signal'

ymin_sigtobaqscaled_mean_bins_scaled <- -.08
ymax_sigtobaqscaled_mean_bins_scaled <- .08
yticks_sigtobaqscaled_mean_bins_scaled <- .04
ybreaks_sigtobaqscaled_mean_bins_scaled <- seq(ymin_sigtobaqscaled_mean_bins_scaled,ymax_sigtobaqscaled_mean_bins_scaled,yticks_sigtobaqscaled_mean_bins_scaled)

xbreaklabels_sigtobaqscaled_mean_bins_scaled <- c('N','Y')

ylabel_sigtobaqscaled_mean_bins_scaled <- 'Ratio (Mean Signal : Mean Scaled Background)'
xlabel_sigtobaqscaled_mean_bins_scaled <- 'Signal (Y/N)'

plottitle_sigtobaqscaled_mean_bins_scaled <- 'Ratio of Signal Stream to Scaled Background Stream Mean'
plotsubtitle_sigtobaqscaled_mean_bins_scaled <- 'Ratio is higher in rats with signal.'

signalbar_sigtobaqscaled_mean_bins_scaled <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigtobaqscaled_mean_bins_scaled, iv_sigtobaqscaled_mean_bins_scaled, 
                                                         ymin_sigtobaqscaled_mean_bins_scaled, ymax_sigtobaqscaled_mean_bins_scaled, ybreaks_sigtobaqscaled_mean_bins_scaled, 
                                                         xbreaklabels_sigtobaqscaled_mean_bins_scaled, ylabel_sigtobaqscaled_mean_bins_scaled, xlabel_sigtobaqscaled_mean_bins_scaled, 
                                                         plottitle_sigtobaqscaled_mean_bins_scaled, plotsubtitle_sigtobaqscaled_mean_bins_scaled)

signalbar_sigtobaqscaled_mean_bins_scaled



## Signal to Scaled Background Magnitude - Band 1
dv_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'sigtobaqscaled_MagSum_Band1_scaled'
iv_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'Signal'

ymin_sigtobaqscaled_MagSum_Band1_scaled_bins <- -.16
ymax_sigtobaqscaled_MagSum_Band1_scaled_bins <- .16
yticks_sigtobaqscaled_MagSum_Band1_scaled_bins <- .08
ybreaks_sigtobaqscaled_MagSum_Band1_scaled_bins <- seq(ymin_sigtobaqscaled_MagSum_Band1_scaled_bins,ymax_sigtobaqscaled_MagSum_Band1_scaled_bins,yticks_sigtobaqscaled_MagSum_Band1_scaled_bins)

xbreaklabels_sigtobaqscaled_MagSum_Band1_scaled_bins <- c('N','Y')

ylabel_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'Magnitude Ratio (0.0051 to 2.5 Hz)'
xlabel_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'Signal (Y/N)'

plottitle_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'Ratio of Signal to Scaled Background Stream Magnitude - 0.0051 to 2.5 Hz'
plotsubtitle_sigtobaqscaled_MagSum_Band1_scaled_bins <- 'Ratio in the low frequency band is higher in rats with signal.'

signalbar_sigtobaqscaled_MagSum_Band1_scaled_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigtobaqscaled_MagSum_Band1_scaled_bins, iv_sigtobaqscaled_MagSum_Band1_scaled_bins, 
                                                                  ymin_sigtobaqscaled_MagSum_Band1_scaled_bins, ymax_sigtobaqscaled_MagSum_Band1_scaled_bins, ybreaks_sigtobaqscaled_MagSum_Band1_scaled_bins, 
                                                                  xbreaklabels_sigtobaqscaled_MagSum_Band1_scaled_bins, ylabel_sigtobaqscaled_MagSum_Band1_scaled_bins, xlabel_sigtobaqscaled_MagSum_Band1_scaled_bins, 
                                                                  plottitle_sigtobaqscaled_MagSum_Band1_scaled_bins, plotsubtitle_sigtobaqscaled_MagSum_Band1_scaled_bins)

signalbar_sigtobaqscaled_MagSum_Band1_scaled_bins



## Signal to Scaled Background Magnitude - Band 2
dv_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'sigtobaqscaled_MagSum_Band2_scaled'
iv_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'Signal'

ymin_sigtobaqscaled_MagSum_Band2_scaled_bins <- -.3
ymax_sigtobaqscaled_MagSum_Band2_scaled_bins <- .3
yticks_sigtobaqscaled_MagSum_Band2_scaled_bins <- .15
ybreaks_sigtobaqscaled_MagSum_Band2_scaled_bins <- seq(ymin_sigtobaqscaled_MagSum_Band2_scaled_bins,ymax_sigtobaqscaled_MagSum_Band2_scaled_bins,yticks_sigtobaqscaled_MagSum_Band2_scaled_bins)

xbreaklabels_sigtobaqscaled_MagSum_Band2_scaled_bins <- c('N','Y')

ylabel_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'Magnitude Ratio (2.5 to 10 Hz)'
xlabel_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'Signal (Y/N)'

plottitle_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'Ratio of Signal to Scaled Background Stream Magnitude - 2.5 to 10 Hz'
plotsubtitle_sigtobaqscaled_MagSum_Band2_scaled_bins <- 'Ratio in the medium frequency band is higher in rats with signal.'

signalbar_sigtobaqscaled_MagSum_Band2_scaled_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigtobaqscaled_MagSum_Band2_scaled_bins, iv_sigtobaqscaled_MagSum_Band2_scaled_bins, 
                                                                          ymin_sigtobaqscaled_MagSum_Band2_scaled_bins, ymax_sigtobaqscaled_MagSum_Band2_scaled_bins, ybreaks_sigtobaqscaled_MagSum_Band2_scaled_bins, 
                                                                          xbreaklabels_sigtobaqscaled_MagSum_Band2_scaled_bins, ylabel_sigtobaqscaled_MagSum_Band2_scaled_bins, xlabel_sigtobaqscaled_MagSum_Band2_scaled_bins, 
                                                                          plottitle_sigtobaqscaled_MagSum_Band2_scaled_bins, plotsubtitle_sigtobaqscaled_MagSum_Band2_scaled_bins)

signalbar_sigtobaqscaled_MagSum_Band2_scaled_bins


## Signal to Scaled Background Magnitude - Band 3
dv_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'sigtobaqscaled_MagSum_Band3_scaled'
iv_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Signal'

ymin_sigtobaqscaled_MagSum_Band3_scaled_bins <- -.1
ymax_sigtobaqscaled_MagSum_Band3_scaled_bins <- .36
yticks_sigtobaqscaled_MagSum_Band3_scaled_bins <- .1
ybreaks_sigtobaqscaled_MagSum_Band3_scaled_bins <- seq(ymin_sigtobaqscaled_MagSum_Band3_scaled_bins,ymax_sigtobaqscaled_MagSum_Band3_scaled_bins,yticks_sigtobaqscaled_MagSum_Band3_scaled_bins)

xbreaklabels_sigtobaqscaled_MagSum_Band3_scaled_bins <- c('N','Y')

ylabel_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Magnitude Ratio (10 to 100 Hz)'
xlabel_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Signal (Y/N)'

plottitle_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Ratio of Signal to Scaled Background Stream Magnitude - 10 to 100 Hz'
plotsubtitle_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Ratio in the high frequency band is higher in rats with signal.'

signalbar_sigtobaqscaled_MagSum_Band3_scaled_bins <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigtobaqscaled_MagSum_Band3_scaled_bins, iv_sigtobaqscaled_MagSum_Band3_scaled_bins, 
                                                                          ymin_sigtobaqscaled_MagSum_Band3_scaled_bins, ymax_sigtobaqscaled_MagSum_Band3_scaled_bins, ybreaks_sigtobaqscaled_MagSum_Band3_scaled_bins, 
                                                                          xbreaklabels_sigtobaqscaled_MagSum_Band3_scaled_bins, ylabel_sigtobaqscaled_MagSum_Band3_scaled_bins, xlabel_sigtobaqscaled_MagSum_Band3_scaled_bins, 
                                                                          plottitle_sigtobaqscaled_MagSum_Band3_scaled_bins, plotsubtitle_sigtobaqscaled_MagSum_Band3_scaled_bins)

signalbar_sigtobaqscaled_MagSum_Band3_scaled_bins


# ELASTIC NET - SIGNAL -----
## Prepare train and test data -----
# Remove extra columns
ENdata <- modeldata

# Create training and testing sets
ENdata_train <- ENdata %>% filter(Set == "Train") %>% select(-Set, -Split)
ENdata_test  <- ENdata %>% filter(Set == "Test")%>% select(-Set, -Split)


# Create design matrix of predictors (excluding SubjectID)
EN_x_train <- model.matrix(~ .-Signal -SignalQuality -SubjectID -Sensor, data = ENdata_train)
EN_y_train <- ENdata_train$Signal

# Prepare test
EN_x_test <- model.matrix(~ .-Signal -SignalQuality -SubjectID-Sensor, data = ENdata_test)
EN_y_test <- ENdata_test$Signal

## Fit elastic net -----
set.seed(123)

EN_cv <- cv.glmnet(
  x = EN_x_train,
  y = EN_y_train,
  family = "binomial",
  alpha = 0.5,             # Elastic Net
  type.measure = "class",  # or "auc" if you want AUC
  nfolds = 10              # 10-fold cross-validation
)

# Set lambda 
EN_minlambda <- EN_cv$lambda.min

# Refit model at this lambda
EN_finalmodel <- glmnet(EN_x_train, EN_y_train, family = "binomial", alpha = 0.5, lambda = EN_minlambda)


# Plot CV error curve
plot(EN_cv)

# Coefficients at best lambda
coef(EN_cv, s = "lambda.min")

## Predict test set -----
# Predict probabilities (for class = 1/"Y")
EN_test_probs <- predict(EN_finalmodel, newx = EN_x_test, type = "response")

# Convert to binary predictions using threshold (e.g., 0.5)
EN_test_preds <- ifelse(EN_test_probs > 0.5, "Y", "N") %>% factor(levels = c("N", "Y"))

# Predict class
EN_pred_class <- predict(EN_cv, newx = EN_x_test, s = "lambda.min", type = "class")

# Predict probability
EN_pred_prob <- predict(EN_cv, newx = EN_x_test, s = "lambda.min", type = "response")

# Confusion matrix
EN_confusion <- table(Predicted = EN_test_preds, Actual = EN_y_test)
print(EN_confusion)

accuracy <- mean(EN_test_preds == EN_y_test)
accuracy


# Extract coefficients
EN_coef <- coef(EN_finalmodel) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(coefficient = `s0`) %>%
  filter(coefficient != 0 & variable != "(Intercept)")


EN_coef_cleaned <- EN_coef %>%
  mutate(abs_coef = abs(coefficient),
         Direction = ifelse(coefficient > 0, "Predicts Signal", "Predicts No Signal"),
         Signal = ifelse(coefficient > 0, "Y", "N")) %>%
  arrange(desc(abs_coef)) %>%
  slice(1:30) # Select top 30 predictors

EN_coef_cleaned

# Rank by absolute value
EN_toppredictors <- EN_coef %>%
  mutate(abs_coef = abs(coefficient)) %>%
  arrange(desc(abs_coef)) #%>%
  #slice(1:20)  # Top 20 predictors

EN_toppredictors



## ADD BACK CATEGORICAL ID VARIABLES -----
catvars <- data_wide %>% filter(Split == 1)

EN_data_test_full <- ENdata_test %>%
  left_join(catvars %>% select(SubjectID, FiberPlacement, Power, Rig),
            by = c("SubjectID"))

EN_test_results <- EN_data_test_full %>%
  select(SubjectID, Signal, SignalQuality, Sensor, FiberPlacement, SignalQuality, Power, Rig) %>%
  mutate(
    Predicted = EN_test_preds,
    Prob_Y = as.numeric(EN_test_probs),
    Correct = (Signal == EN_test_preds)
  )

## SUMMARIZE -----
EN_test_results_summary_quality <- EN_test_results %>%
  group_by(Sensor, Signal, SignalQuality) %>%
  summarise(
    Accuracy = mean(Correct),
    AccuracyPerc = scales::percent(mean(Correct), accuracy = 0.1),
    n = n(),
    n_correct = sum(Correct),
    n_incorrect = n - n_correct,
    .groups = "drop"
  )

EN_test_results_summary_quality

EN_test_results %>%
  group_by(Sensor, SignalQuality) %>%
  summarise(Accuracy = mean(Correct), .groups = "drop") %>%
  ggplot(aes(x = SignalQuality, y = Accuracy, fill = Sensor)) +
  geom_col(position = "dodge") +
  labs(title = "Classification Accuracy by Sensor and SignalQuality",
       y = "Accuracy", x = "Signal Quality") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()



EN_test_results_summary <- EN_test_results %>%
  group_by(Sensor, Signal) %>%
  summarise(
    Accuracy = mean(Correct),
    AccuracyPerc = scales::percent(mean(Correct), accuracy = 0.1),
    n = n(),
    n_correct = sum(Correct),
    n_incorrect = n - n_correct,
    .groups = "drop"
  )

EN_test_results_summary


EN_test_results %>%
  group_by(Sensor, Signal) %>%
  summarise(Accuracy = mean(Correct), .groups = "drop") %>%
  ggplot(aes(x = Signal, y = Accuracy, fill = Sensor)) +
  geom_col(position = "dodge") +
  labs(title = "Classification Accuracy by Sensor",
       y = "Accuracy", x = "Signal") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()




# Generate Plot
fig_m1_variable_importance <- EN_coef_cleaned %>%
  # Reorder the y-axis (variable) by the coefficient value for smooth bars
  ggplot(aes(x = coefficient, y = fct_reorder(variable, coefficient), fill = Signal)) +
  geom_col() +
  scale_fill_manual(values = colors_signal, labels = c("Predicts Signal","Predicts No Signal")) + # Noise Color
  scale_x_continuous(labels = scales::label_comma()) + # Format numbers if large
  labs(title = "Top Variables (Signal vs. No Signal)",
       subtitle = paste("Elastic Net (\u03B1=0.5)"),
       x = "Elastic Net Coefficient", y = "") +
  mytheme_legend
 # theme(legend.position = "top")

fig_m1_variable_importance


# FIGURE 2: Model 1 ROC Curve
# This plot answers: "How well is the model separating the two classes?"
# We use the pROC object (must be generated if not already)

# Generate the roc object if it doesn't exist
roc_obj <- roc(EN_y_test, as.numeric(EN_test_probs))
auc_value <- round(auc(roc_obj), 3)

fig_m1_roc <- ggroc(roc_obj, color = colors_signal_Y, size = 1.2) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "black") + # Reference line
  labs(title = paste("Signal Model Performance: AUC =", auc_value),
       subtitle = "Receiver Operating Characteristic (ROC)",
       x = "Specificity (Correct 'No Signal' classification)", y = "Sensitivity (Correct 'Yes Signal' Classification)") +
  mytheme

fig_m1_roc


# FIGURE 3: Confusion Matrix Heatmap
# This plot answers: "Where are the actual errors occurring?"
# It displays True Positives, False Positives, False Negatives, and True Negatives.

# Data preparation: Convert your table `EN_confusion` to a data frame for plotting
# en_cm_data <- as.data.frame(as.table(EN_confusion)) # assuming EN_confusion exists

# If `EN_confusion` is a matrix/table, we convert to tidy format:
en_cm_tidy <- EN_confusion %>%
  as.data.frame() %>% # Convert table to dataframe
  mutate(Actual = factor(Actual, levels=rev(levels(Actual))), # Reorder to get TN top-left, TP bottom-right
         Predicted = factor(Predicted, levels=levels(Predicted)))

fig_m1_confusion_matrix <- en_cm_tidy %>%
  ggplot(aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 8, color = "black") + # Add counts
  scale_fill_gradient(low = colors_signal_N, high = colors_signal_Y, name = "Count") +
  scale_x_discrete(position = "top") + # Standard for matrices
  labs(title = "Confusion Matrix",
       x = "Predicted", y = "Actual") +
  mytheme_legend + theme(legend.position = 'right')

fig_m1_confusion_matrix


# FIGURE 4: Probability Density (Calibration Check)
# This plot answers: "Does the model separate probabilities robustly?"
# It is excellent for identifying "fuzzy" signals where probabilities are near 0.5.
fig_m1_probability_density <- EN_test_results %>%
  ggplot(aes(x = Prob_Y, fill = Signal)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 1, color = "black") + # Classification threshold
  scale_fill_manual(values = c("N" = colors_signal_N, "Y" = colors_signal_Y), name = "Actual Signal") +
  labs(title = "Predicted Probability vs. Actual Classification",
       x = "Predicted Probability of Signal (Prob Y)", y = "Density") +
  mytheme_legend +
  theme(legend.position = "right")

fig_m1_probability_density



# FIGURE 5: Correct vs. Incorrect Counts by Signal Quality
## Compare performance by Signal Quality Score
EN_test_results$Correct <- as.integer(EN_test_results$Correct)  # ensure numeric 0/1

logit_model <- glm(Correct ~ factor(SignalQuality), data = EN_test_results, family = "binomial")
summary(logit_model)
anova(logit_model)

model_emmeans <- emmeans(logit_model, ~ SignalQuality)
pairwise_results <- pairs(model_emmeans, adjust = "tukey")

pairwise_results



fig_m1_quality_counts <- EN_test_results %>%
  # Filter out any NAs in SignalQuality (e.g., if "No Signal" recordings lack a score)
  filter(!is.na(SignalQuality)) %>% 
  mutate(Outcome = ifelse(Correct == TRUE, "Correct", "Incorrect")) %>%
  ggplot(aes(x = factor(SignalQuality), fill = Outcome)) +
  geom_bar(position = "stack", alpha = 1) +
  scale_fill_manual(values = c("Correct" = colors_signal_Y, "Incorrect" = colors_signal_N)) +
  labs(title = "Classification Outcomes by Signal Quality",
       x = "Signal Quality Score", 
       y = "Test n",
       fill = "Model Prediction") +
  theme_minimal(base_size = 14) +
  mytheme_legend + 
  theme(legend.position = "right")

fig_m1_quality_counts


# FIGURE 6: Accuracy Percentage by Signal Quality
fig_m1_quality_accuracy <- EN_test_results %>%
  filter(!is.na(SignalQuality)) %>%
  group_by(SignalQuality) %>%
  summarise(
    Accuracy = mean(Correct),
    Total = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = factor(SignalQuality), y = Accuracy, fill = SignalQuality)) +
  geom_col(color = "white", alpha = 1) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 1)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
  scale_fill_manual(values = colors_signalquality) +
  labs(title = "Model Accuracy by Signal Quality",
       subtitle = "Percentage of recordings correctly classified as Signal/No Signal",
       x = "Signal Quality Score", 
       y = "Accuracy") +
  mytheme_legend + theme(legend.position = 'right')

fig_m1_quality_accuracy



# FIGURE 7: Model Confidence (Probability) by Signal Quality and Actual Signal
fig_m1_quality_probs <- EN_test_results %>%
  filter(!is.na(SignalQuality)) %>%
  ggplot(aes(x = factor(SignalQuality), y = Prob_Y, fill = Signal)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 1) + # The 0.5 decision boundary
  scale_fill_manual(values = c("N" = colors_signal_N, "Y" = colors_signal_Y), name = "Actual Signal Label") +
  labs(title = "Model Confidence by Signal Quality",
       x = "Signal Quality Score", 
       y = "Predicted Probability of Signal (Prob Y)") +
  theme_minimal(base_size = 14) +
  mytheme_legend + 
  theme(legend.position = "right")

print(fig_m1_quality_probs)



## Sensor comparisons
sensor_table <- table(EN_test_results$Sensor, EN_test_results$Correct)
print("Contingency Table (Counts):")
print(sensor_table)

fisher_result <- fisher.test(sensor_table)
print("Fisher's Exact Test Result:")
print(fisher_result)


# FIGURE 8: Sensor Accuracy Count Matrix
sensor_df <- as.data.frame(sensor_table)

# Rename columns for clarity (assuming your table outputs Var1 and Var2)
colnames(sensor_df) <- c("Sensor", "Correct", "Count")

fig_sensor_accuracytable <- ggplot(sensor_df, aes(x = Correct, y = Sensor, fill = Count)) +
  geom_tile(color = "white", size = 1) +
  # Add the text counts in the middle of each tile
  geom_text(aes(label = Count), color = "black", size = 6) +
  # Use a nice color gradient (e.g., light blue to dark blue)
  scale_fill_gradient(low = colors_signal_N, high = colors_signal_Y) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
  labs(
    title = "Sensor Accuracy Count Matrix",
    x = "Was the Classification Correct?",
    y = "Sensor Type"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
fig_sensor_accuracytable

# FIGURE 8: Accuracy Rate by Sensor
fig_sensor_accuracy <- EN_test_results %>%
  group_by(Sensor) %>%
  summarise(
    Accuracy = mean(Correct),
    Total = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(Sensor, -Accuracy), y = Accuracy, fill = Sensor)) +
  geom_col(color = "white", alpha = 1) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values=colors_sensor) + 
  labs(title = "Signal Classification Accuracy by Sensor",
       x = "Sensor", 
       y = "Accuracy") +
  mytheme_legend + 
  theme(legend.position = "none") 

fig_sensor_accuracy


# FIGURE 9: Model Confidence (Predicted Probability) by Sensor
# This is crucial: it shows if the model is systematically over- or under-predicting
# for specific sensors, even if it gets the final binary class "correct".

fig_sensor_probs <- EN_test_results %>%
  ggplot(aes(x = Sensor, y = Prob_Y, fill = Signal)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = c("N" = colors_signal_N, "Y" = colors_signal_Y), name = "Actual Signal Label") +
  labs(title = "Model Confidence by Sensor",
       x = "Sensor", 
       y = "Predicted Probability of Signal (Prob Y)") +
  mytheme_legend +
  theme(legend.position = "right")

fig_sensor_probs






# ELASTIC NET - SIGNAL QUALITY -----
## Prepare train and test data -----
ENSQdata_prep <- modeldata %>%
  select(-Signal) %>%
  filter(!is.na(SignalQuality)) %>% 
  # Create an interaction column to balance BOTH variables simultaneously
  mutate(Stratify_Col = interaction(Sensor, SignalQuality))

# Set a seed for reproducibility
set.seed(123) 

# Create the split object (e.g., 80% train, 20% test)
# Stratifying by our new combined column guarantees equal representation
data_split <- initial_split(ENSQdata_prep, prop = 0.80, strata = Stratify_Col)

# Extract the sets and clean up the extra columns
ENSQdata_train <- training(data_split) %>% 
  select(-any_of(c("Set", "Split", "Stratify_Col")))

ENSQdata_test <- testing(data_split) %>% 
  select(-any_of(c("Set", "Split", "Stratify_Col")))

# Create design matrix of predictors (excluding SubjectID)
ENSQ_x_train <- model.matrix(~ .-SignalQuality -SubjectID, data = ENSQdata_train)[, -1]
ENSQ_y_train <- ENSQdata_train$SignalQuality
ENSQ_y_train <- droplevels(ENSQ_y_train)

# Prepare test
ENSQ_x_test <- model.matrix(~ .-SignalQuality -SubjectID, data = ENSQdata_test)[, -1]
ENSQ_y_test <- ENSQdata_test$SignalQuality
ENSQ_y_test <- droplevels(ENSQ_y_test)




## Fit elastic net -----
set.seed(123)

ENSQ_finalmodel <- cv.glmnet(
  x = ENSQ_x_train,
  y = ENSQ_y_train,
  family = "multinomial",
  alpha = 0.5,             # Elastic Net
  type.measure = "class",  # or "auc" if you want AUC
  nfolds = 10              # 10-fold cross-validation
)


print(ENSQ_finalmodel$lambda.min)
print(ENSQ_finalmodel$lambda.1se)

# 4. Extract the coefficients for the best model directly from the CV object
# No need to use glmnet() again!
best_coefs <- coef(ENSQ_finalmodel, s = "lambda.min")
print(best_coefs)

# 5. Make predictions on your test set
test_predictions <- predict(ENSQ_finalmodel, 
                            newx = ENSQ_x_test, 
                            s = "lambda.min", 
                            type = "class")

# View a confusion matrix of how it performed
table(Predicted = test_predictions, Actual = ENSQ_y_test)

## Predict test set -----
# Predict probabilities (for class = 1/"Y")
ENSQ_test_probs <- predict(ENSQ_finalmodel, newx = ENSQ_x_test, type = "response")[,,1]

ENSQ_test_preds <- predict(ENSQ_finalmodel, newx = ENSQ_x_test, type = "class") %>% as.vector()

# Accuracy
ENSQ_accuracy <- mean(ENSQ_test_preds == ENSQ_y_test)
cat("Overall accuracy:", round(ENSQ_accuracy, 3), "\n")

# Confusion matrix
table(Predicted = ENSQ_test_preds, Actual = ENSQ_y_test)

# Pull coefficients
ENSQ_coef_list <- coef(ENSQ_finalmodel)

# Tidy up non-zero coefficients across all classes
ENSQ_coefs <- map_df(names(ENSQ_coef_list), function(class_label) {
  coef_matrix <- as.matrix(ENSQ_coef_list[[class_label]])
  tibble(
    variable = rownames(coef_matrix),
    coefficient = coef_matrix[, 1]
  ) %>%
    filter(variable != "(Intercept)", variable !="", coefficient != 0) %>%
    mutate(Class = class_label)
})


# Top predictors across all classes by absolute coefficient
ENSQ_coefs %>%
  mutate(abs_coef = abs(coefficient)) %>%
  arrange(desc(abs_coef)) %>%
  slice(1:20)


## ADD BACK CATEGORICAL ID VARIABLES -----
ENSQdata_test_full <- ENSQdata_test %>%
  left_join(data_wide %>% filter(Split==1) %>% select(SubjectID, FiberPlacement, Signal, Power, Rig),
            by = c("SubjectID"))

ENSQ_test_results <- ENSQdata_test_full %>%
  select(SubjectID, Signal, SignalQuality, Sensor, FiberPlacement, Power, Rig) %>%
  bind_cols(ENSQ_test_probs) %>%
  mutate(
    Predicted = ENSQ_test_preds,
    Correct = (SignalQuality == ENSQ_test_preds)
  )

## Compare performance by Sensor and Signal Quality Score -----
ENSQ_test_results %>%
  group_by(Sensor, Signal, SignalQuality) %>%
  summarise(
    Accuracy = mean(Correct),
    AccuracyPerc = scales::percent(mean(Correct), accuracy = 0.1),
    n = n(),
    n_correct = sum(Correct),
    n_incorrect = n - n_correct,
    .groups = "drop"
  )

ENSQ_test_results %>%
  group_by(Sensor, SignalQuality) %>%
  summarise(Accuracy = mean(Correct), .groups = "drop") %>%
  ggplot(aes(x = SignalQuality, y = Accuracy, fill = Sensor)) +
  geom_col(position = "dodge") +
  labs(title = "Classification Accuracy by Sensor and Signal Quality",
       y = "Accuracy", x = "Signal Quality") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()









