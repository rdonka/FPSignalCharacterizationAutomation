#################### FP Signal Characterization Automation #########################

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

set.seed(123)

# Create subject-level summary for stratification
subject_summary <- data %>%
  group_by(SubjectID, Sensor) %>%
  summarise(
    Signal = first(Signal_RDScored),
    .groups = "drop"
  )

# Stratified sample of Subjects
train_subjects <- subject_summary %>%
  group_by(Sensor, Signal) %>%
  sample_frac(0.5) %>%
  pull(SubjectID)

# Assign Set based on SubjectID
data_split <- data %>%
  mutate(Set = ifelse(SubjectID %in% train_subjects, "Train", "Test"))

# Pivot data to wide
data_wide <- data_split %>%
  pivot_wider(
    id_cols = c(SubjectID, Split, Set, Sex, FiberPlacement, Sensor, Rig, Power, Signal_RDScored, Signal_RDQuality, baqscalingfactor),
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


# Scale ratios
data_wide$sigtobaq_mean_bins_scaled <- data_wide$sigtobaq_mean_bins-1
data_wide$sigtobaq_range_bins_scaled <- data_wide$sigtobaq_range_bins-1
data_wide$sigtobaq_rms_bins_scaled <- data_wide$sigtobaq_rms_bins-1

data_wide$sigtobaqscaled_mean_bins_scaled <- data_wide$sigtobaqscaled_mean_bins-1
data_wide$sigtobaqscaled_range_bins_scaled <- data_wide$sigtobaqscaled_range_bins-1
data_wide$sigtobaqscaled_rms_bins_scaled <- data_wide$sigtobaqscaled_rms_bins-1

data_wide$sigtobaq_MagSum_Band1_scaled <- data_wide$sigtobaq_MagSum_Band1-1
data_wide$sigtobaq_MagSum_Band2_scaled <- data_wide$sigtobaq_MagSum_Band2-1
data_wide$sigtobaq_MagSum_Band3_scaled <- data_wide$sigtobaq_MagSum_Band3-1

data_wide$sigtobaqscaled_MagSum_Band1_scaled <- data_wide$sigtobaqscaled_MagSum_Band1-1
data_wide$sigtobaqscaled_MagSum_Band2_scaled <- data_wide$sigtobaqscaled_MagSum_Band2-1
data_wide$sigtobaqscaled_MagSum_Band3_scaled <- data_wide$sigtobaqscaled_MagSum_Band3-1

## Analysis Variables -----
IDvariables <- c('SubjectID','Sensor','Set','Split','Signal','SignalQuality')

includevariables <- c('sig_range_bins', 'sig_rms_bins', 'sig_MagSum_Band1', 'sig_MagSum_Band2','sig_MagSum_Band3',
                      'baq_range_bins', 'baq_rms_bins', 'baq_MagSum_Band1', 'baq_MagSum_Band2','baq_MagSum_Band3',
                      'baqscaled_range_bins', 'baqscaled_rms_bins', 'baqscaled_MagSum_Band1', 'baqscaled_MagSum_Band2','baqscaled_MagSum_Band3',
                      'sigsub_range_bins', 'sigsub_rms_bins', 'sigsub_MagSum_Band1', 'sigsub_MagSum_Band2','sigsub_MagSum_Band3',
                      'sigfilt_range_bins', 'sigfilt_rms_bins', 'sigfilt_MagSum_Band1', 'sigfilt_MagSum_Band2',
                      'sigtobaq_rms_bins_scaled','sigtobaq_MagSum_Band1_scaled', 'sigtobaq_MagSum_Band2_scaled', 'sigtobaq_MagSum_Band3_scaled',
                      'sigtobaqscaled_rms_bins_scaled','sigtobaqscaled_MagSum_Band1_scaled', 'sigtobaqscaled_MagSum_Band2_scaled', 'sigtobaqscaled_MagSum_Band3_scaled')

includevariables_labels <- c('Raw Signal Range', 'Raw Signal RMS', 'Raw Signal Low Freq FFT Mag', 'Raw Signal Medium Freq FFT Mag','Raw Signal High Freq FFT Mag',
                             'Raw Background Range', 'Raw Background RMS', 'Raw Background Low Freq FFT Mag', 'Raw Background Medium Freq FFT Mag','Raw Background High Freq FFT Mag',
                             'Scaled Background Range', 'Scaled Background RMS', 'Scaled Background Low Freq FFT Mag', 'Scaled Background Medium Freq FFT Mag','Scaled Background High Freq FFT Mag',                      
                             'Subtracted Signal Range', 'Subtracted Signal RMS', 'Subtracted Signal Low Freq FFT Mag', 'Subtracted Signal Medium Freq FFT Mag','Subtracted Signal High Freq FFT Mag',
                             'Filtered Signal Range', 'Filtered Signal RMS', 'Filtered Signal Low Freq FFT Mag', 'Filtered Signal Medium Freq FFT Mag',
                             'Raw Signal:Raw Background RMS', 'Raw Signal:Raw Background Low Freq FFT Mag', 'Raw Signal:Raw Background Medium Freq FFT Mag','Raw Signal:Raw Background High Freq FFT Mag',
                             'Raw Signal:Scaled Background RMS', 'Raw Signal:Scaled Background Low Freq FFT Mag', 'Raw Signal:Scaled Background Medium Freq FFT Mag','Raw Signal:Scaled Background High Freq FFT Mag')


modeldata <- data_wide %>% select(all_of(IDvariables), all_of(includevariables))

# Raw Variable Plots -----
## Prepare means
### Overall signal means
modeldata_signalmeans <- modeldata %>%
  group_by(Signal) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop"
  )
    
modeldata_signalmeans   
  
### Overall signal means by sensor
modeldata_sensorsignalmeans <- modeldata %>%
  group_by(Sensor, Signal) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop" 
  )

modeldata_sensorsignalmeans   

### Signal Quality means
modeldata_signalqualitymeans <- modeldata %>%
  group_by(Signal, SignalQuality) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop"
  )

modeldata_signalqualitymeans   

### Signal Quality means by sensor
modeldata_sensorsignalqualitymeans <- modeldata %>%
  group_by(Sensor, Signal, SignalQuality) %>% 
  summarize(
    n_count = n(),
    across(all_of(includevariables), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
    .groups = "drop"
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
plotsubtitle_baqscaled_rms_bins <- 'Scaled background RMS is similar in rats with and without signal.'

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



## Raw signal to scaled background range
dv_sigtobaqscaled_range_bins_scaled <- 'sigtobaqscaled_rms_bins_scaled'
iv_sigtobaqscaled_range_bins_scaled <- 'Signal'

ymin_sigtobaqscaled_range_bins_scaled <- -.08
ymax_sigtobaqscaled_range_bins_scaled <- .08
yticks_sigtobaqscaled_range_bins_scaled <- .4
ybreaks_sigtobaqscaled_range_bins_scaled <- seq(ymin_sigtobaqscaled_range_bins_scaled,ymax_sigtobaqscaled_range_bins_scaled,yticks_sigtobaqscaled_range_bins_scaled)

xbreaklabels_sigtobaqscaled_range_bins_scaled <- c('N','Y')

ylabel_sigtobaqscaled_range_bins_scaled <- 'Ratio (Raw Signal : Scaled Background RMS)'
xlabel_sigtobaqscaled_range_bins_scaled <- 'Signal (Y/N)'

plottitle_sigtobaqscaled_range_bins_scaled <- 'Ratio of Signal Stream to Scaled Background Stream RMS'
plotsubtitle_sigtobaqscaled_range_bins_scaled <- 'Ratio is comparable in rats with and without signal.'

signalbar_sigtobaqscaled_range_bins_scaled <- plotdescriptives_signal_bar(modeldata_signalmeans, modeldata, dv_sigtobaqscaled_range_bins_scaled, iv_sigtobaqscaled_range_bins_scaled, 
                                                                         ymin_sigtobaqscaled_range_bins_scaled, ymax_sigtobaqscaled_range_bins_scaled, ybreaks_sigtobaqscaled_range_bins_scaled, 
                                                                         xbreaklabels_sigtobaqscaled_range_bins_scaled, ylabel_sigtobaqscaled_range_bins_scaled, xlabel_sigtobaqscaled_range_bins_scaled, 
                                                                         plottitle_sigtobaqscaled_range_bins_scaled, plotsubtitle_sigtobaqscaled_range_bins_scaled)

signalbar_sigtobaqscaled_range_bins_scaled


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
plotsubtitle_sigtobaqscaled_MagSum_Band3_scaled_bins <- 'Ratio in the high frequency band is similar in rats with and without signal.'

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
  alpha = 0.5,             
  type.measure = "class", # AUC grading metric
  nfolds = 10 # 10-fold cross-validation
)

# Set lambda 
EN_minlambda <- EN_cv$lambda.min

# Refit model at this lambda
EN_finalmodel <- glmnet(EN_x_train, EN_y_train, family = "binomial", alpha = 0.5, lambda = EN_minlambda)

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


variable_lookup <- setNames(includevariables_labels, includevariables)

EN_coef_cleaned <- EN_coef %>% filter(variable != "X.Intercept.") %>%
  mutate(abs_coef = abs(coefficient),
         Direction = ifelse(coefficient > 0, "Predicts Signal", "Predicts No Signal"),
         Signal = ifelse(coefficient > 0, "Y", "N")) %>%
  mutate(variablename = coalesce(variable_lookup[variable], variable)) %>% 
  arrange(desc(abs_coef)) %>%
  slice(1:30)


# Rank byVariableNames2String()# Rank by absolute value
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

EN_test_results$SensorSignalQuality <- paste(EN_test_results$Sensor, EN_test_results$SignalQuality, sep='_')

## SUMMARIZE -----
EN_test_results_summary_quality <- EN_test_results %>%
  group_by(Sensor, Signal, SignalQuality, SensorSignalQuality) %>%
  summarise(
    Accuracy = mean(Correct),
    AccuracyPerc = scales::percent(mean(Correct), accuracy = 0.1),
    n = n(),
    n_correct = sum(Correct),
    n_incorrect = n - n_correct,
    .groups = "drop"
  )

EN_test_results_summary_quality

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

## FIGURE 1: CV error curve
cv_data <- tibble(
  lambda = EN_cv$lambda,
  log_lambda = log(EN_cv$lambda),
  cvm = EN_cv$cvm, # Cross-validation mean error
  cvsd = EN_cv$cvsd, # Cross-validation standard deviation
  cvup = EN_cv$cvup, # Upper bound of error curve
  cvlo = EN_cv$cvlo, # Lower bound of error curve
  nzero = EN_cv$nzero # Number of non-zero coefficients
)

log_lambda_min <- log(EN_cv$lambda.min)

fig_cv_error <- ggplot(cv_data, aes(x = log_lambda, y = cvm)) +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), color = "#707070", width = 0.06) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = log_lambda_min, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = log_lambda_min, y = max(cv_data$cvup), 
           label = "Lambda", angle = 90, vjust = -0.5, hjust=.8, color = "red", size=6) +
  labs(
    title = "Elastic Net Cross-Validation Error",
    subtitle = "Performance across regularization penalties",
    x = "Log(Lambda)",
    y = "Misclassification Error") +
  mytheme +
  theme(
    panel.grid.minor = element_blank()
  )

fig_cv_error


# FIGURE 2: Variable Importance
fig_m1_variable_importance <- EN_coef_cleaned %>%
  ggplot(aes(x = coefficient, y = fct_reorder(variablename, coefficient), fill = Signal)) +
  geom_col() +
  scale_fill_manual(values = colors_signal, labels = c("Predicts No Signal","Predicts Signal")) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(title = "Top Variables (Signal vs. No Signal)",
       subtitle = paste("Elastic Net (\u03B1=0.5)"),
       x = "Elastic Net Coefficient", y = "") +
  mytheme_legend +
  geom_vline(xintercept=0, linewidth = myaxislinewidth, linetype = 'dashed',color='black')

fig_m1_variable_importance


# FIGURE 3: ROC Curve
roc_obj <- roc(EN_y_test, as.numeric(EN_test_probs))
auc_value <- round(auc(roc_obj), 3)

fig_m1_roc <- ggroc(roc_obj, color = colors_signal_Y, size = 1.2) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "black", linewidth=myaxislinewidth) +
  labs(title = paste("Signal Model Performance: AUC =", auc_value),
       subtitle = "Receiver Operating Characteristic (ROC)",
       x = "Specificity (Correct 'No Signal' classification)", y = "Sensitivity (Correct 'Yes Signal' Classification)") +
  mytheme

fig_m1_roc


# FIGURE 3: Confusion Matrix Heatmap
en_cm_tidy <- EN_confusion %>%
  as.data.frame() %>% 
  mutate(Actual = factor(Actual, levels=rev(levels(Actual))),
         Predicted = factor(Predicted, levels=levels(Predicted)))

fig_m1_confusion_matrix <- en_cm_tidy %>%
  ggplot(aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 8, color = "black")+
  scale_fill_gradient(low = colors_signal_N, high = colors_signal_Y, name = "Count") +
  scale_x_discrete(position = "top") +
  labs(title = "Confusion Matrix",
       x = "Predicted", y = "Actual") +
  mytheme_legend + theme(legend.position = 'right')

fig_m1_confusion_matrix


# FIGURE 4: Probability Density (Calibration Check)
fig_m1_probability_density <- EN_test_results %>%
  ggplot(aes(x = Prob_Y, fill = Signal)) +
  geom_density(alpha = 1, color=NA) +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = myaxislinewidth, color = "black") + # Classification threshold
  scale_fill_manual(values = c("N" = colors_signal_N, "Y" = colors_signal_Y), name = "Actual Signal") +
  scale_y_continuous(expand=c(0,0), limits = c(0,10), breaks=seq(0,10,2.5)) +
  labs(title = "Predicted Probability vs. Actual Classification",
       x = "Predicted Probability of Signal (Prob Y)", y = "Density") +
  mytheme_legend +
  theme(legend.position = "right")

fig_m1_probability_density



# FIGURE 5: Correct vs. Incorrect Counts by Signal Quality
EN_test_results$Correct <- as.integer(EN_test_results$Correct)

logit_model <- glm(Correct ~ factor(SignalQuality), data = EN_test_results, family = "binomial")
summary(logit_model)
anova(logit_model)

model_emmeans <- emmeans(logit_model, ~ SignalQuality)
pairwise_results <- pairs(model_emmeans, adjust = "tukey")

pairwise_results


fig_m1_quality_counts <- EN_test_results %>%
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
  scale_y_continuous(expand=c(0,0),limits=c(0,1.1)) +
  labs(title = "Model Accuracy by Signal Quality",
       x = "Signal Quality Score", 
       y = "Accuracy") +
  mytheme_legend + theme(legend.position = 'right') +
  geom_hline(yintercept=.5, linewidth=myaxislinewidth, linetype='dashed', color='black')

fig_m1_quality_accuracy



# FIGURE 7: Model Confidence (Probability) by Signal Quality and Actual Signal
fig_m1_quality_probs <- EN_test_results %>%
  filter(!is.na(SignalQuality)) %>%
  ggplot(aes(x = factor(SignalQuality), y = Prob_Y, fill = Signal)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = myaxislinewidth) + # The 0.5 decision boundary
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
sensor_df <- as.data.frame(sensor_table) # Convert to data frame
colnames(sensor_df) <- c("Sensor", "Correct", "Count") # Rename columns


fig_sensor_accuracytable <- ggplot(sensor_df, aes(x = Correct, y = Sensor, fill = Count)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Count), color = "black", size = 6) +
  scale_fill_gradient(low = colors_signal_N, high = colors_signal_Y) +
  scale_x_discrete(labels = c('Incorrect','Correct')) +
  labs(title = "Signal Classification Accuracy Count Matrix",
    x = "Classification",
    y = "Sensor"
  ) +
  mytheme_legend + 
  theme(
    panel.grid = element_blank(),
    legend.position = "right")

fig_sensor_accuracytable

# FIGURE 9: Accuracy Rate by Sensor
fig_sensor_accuracy <- EN_test_results %>%
  group_by(Sensor) %>%
  summarise(
    Accuracy = mean(Correct),
    Total = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(Sensor, -Accuracy), y = Accuracy, fill = Sensor)) +
  geom_col(color = "white", alpha = 1) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.4, size = 5) +
  scale_y_continuous(expand=c(0,0), labels = scales::percent_format(), limits = c(0, 1.02)) +
  scale_fill_manual(values=colors_sensor) + 
  labs(title = "Signal Classification Accuracy",
       x = "Sensor", 
       y = "Accuracy") +
  mytheme_legend + 
  theme(legend.position = "none")  +
  geom_hline(yintercept=.5, linewidth=myaxislinewidth, linetype='dashed', color='black')

fig_sensor_accuracy


# FIGURE 10: Accuracy Rate by Sensor and Signal Quality
fig_sensor_accuracy_signalquality <- EN_test_results %>%
  group_by(Sensor, SignalQuality, SensorSignalQuality) %>%
  summarise(Accuracy = mean(Correct), .groups = "drop") %>%
  ggplot(aes(x = SignalQuality, y = Accuracy, fill = SensorSignalQuality)) +
  geom_col(position = position_dodge(width = mydodgewidth), linewidth = mycollinewidth, width=mycolwidth, show.legend=TRUE) +
  scale_fill_manual(values = colors_sensorsignalquality, breaks = c("GCaMP6f_3", "GRABDA2H_3", "dLight1.3b_3"), labels = c("GCaMP6f", "GRABDA2H", "dLight1.3b"),
  name = "Sensor")+
  labs(title = "Signal Classification Accuracy by Signal Quality",
       y = "Accuracy", x = "Signal Quality") +
  scale_y_continuous(expand=c(0,0),labels = scales::percent_format()) +
  mytheme_legend+
  geom_hline(yintercept=.5, linewidth=myaxislinewidth, linetype='dashed', color='black')

fig_sensor_accuracy_signalquality



# FIGURE 11: Model Confidence (Predicted Probability) by Sensor
# This is crucial: it shows if the model is systematically over- or under-predicting
# for specific sensors, even if it gets the final binary class "correct".

fig_sensor_probs <- EN_test_results %>%
  ggplot(aes(x = Sensor, y = Prob_Y, fill = Signal)) +
  geom_boxplot(alpha = 1, outlier.shape = 21, outlier.size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = myaxislinewidth) +
  scale_fill_manual(values = c("N" = colors_signal_N, "Y" = colors_signal_Y), name = "Actual Signal Label") +
  labs(title = "Model Confidence by Sensor",
       x = "Sensor", 
       y = "Predicted Probability of Signal (Prob Y)") +
  mytheme_legend +
  theme(legend.position = "right")

fig_sensor_probs


