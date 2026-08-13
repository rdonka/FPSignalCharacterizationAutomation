%% EXAMPLE ANALYSIS
% PROJECT SUMMARY: Testing of AJP problem files.

%% Set up paths and analysis keys
% Set up user path inputs
rootdirectory =  'C:\Users\rmdon\'; % Computer user unique portion of file path
parentfolder = 'Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\'; % Folder to output analysis csv files to

analysisfolder = 'Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\Analysis\'; % Folder to output analysis csv files to
subjectfigurefolder = 'Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\Analysis\Figures\Subject Figures\'; % Folder to output figures to
overallfigurefolder = 'Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\Analysis\Figures\Overall Figures\'; % Folder to output figures to

% Create full paths with rootdirectory appended
analysispath = append(rootdirectory,analysisfolder); 
subjectfigurepath = append(rootdirectory,subjectfigurefolder);
overallfigurepath = append(rootdirectory,overallfigurefolder);

% Add GitHub Repositories and data folders to MATLAB path
addpath(genpath(append(rootdirectory,'\Desktop\GitHub_MyRepositories\PASTa\'))); % Path for GitHub repository
addpath(genpath(append(rootdirectory,parentfolder))); % Path for analysis files - this is where the keys are saved\\

% Load in experiment key names - Subject Key and File Key
subjectkeyname = ''; % Name of csv file containing subject information; set to '' if not using a Subject Key
filekeyname = 'FileKey_FPSignalCharacterizationAutomation.csv'; % Name of csv file containing session information and paths


skipexistingfigs = 1;

%% Load keys
% Load subject key and file key into a data structure and append rootdirectory to RawFolderPath and ExtractedFolderPaths
[experimentkey_raw] = createExperimentKey(rootdirectory, subjectkeyname, filekeyname);

% Remove Session Exclude
sessionincludeidxs = find(~strcmp({experimentkey_raw.SessionExclude},'EXCLUDE'));
[experimentkey] = experimentkey_raw(sessionincludeidxs);


%% Extract data
% Extract raw data from blocks using the function 'extractTDTdata' to extract raw data blocks.
% Set up inputs
sigstreamnames = {'x465A', 'x65A', '465A'}; % All names of signal streams across files
baqstreamnames = {'x405A', 'x05A', '405A'}; % All names of background streams across files
rawfolderpaths = string({experimentkey.RawFolderPath})'; % Create string array of raw folder paths
extractedfolderpaths = string({experimentkey.ExtractedFolderPath})'; % Create string array of extracted folder paths

extractTDTdata(rawfolderpaths,extractedfolderpaths,sigstreamnames,baqstreamnames,'skipexisting',1,'trim',0); % extract data with no trimming

%% Load data
% Load previously extracted data blocks and tie to experiment key. 
% Each block is loaded as a row in the data structure.
[rawdata] = loadKeydata(experimentkey); % Load data based on the experiment key into the structure 'rawdata'


%% Check Sensor and Signal ns 
n_GCaMP6f_Y = sum(strcmp({rawdata.Sensor},'GCaMP6f')&strcmp({rawdata.Signal_RDScored},'Y'))   % 18/50
n_GRABDA2H_Y = sum(strcmp({rawdata.Sensor},'GRABDA2H')&strcmp({rawdata.Signal_RDScored},'Y')) % 72/50 - DONE
n_GRABDA3m_Y = sum(strcmp({rawdata.Sensor},'GRABDA3M')&strcmp({rawdata.Signal_RDScored},'Y')) % 7/50
n_dLight_Y = sum(strcmp({rawdata.Sensor},'dLight1.3b')&strcmp({rawdata.Signal_RDScored},'Y')) % 51/50 - DONE

n_GCaMP6f_N = sum(strcmp({rawdata.Sensor},'GCaMP6f')&strcmp({rawdata.Signal_RDScored},'N'))   % 25/50
n_GRABDA2H_N = sum(strcmp({rawdata.Sensor},'GRABDA2H')&strcmp({rawdata.Signal_RDScored},'N')) % 17/50 - DONE
n_GRABDA3m_N = sum(strcmp({rawdata.Sensor},'GRABDA3M')&strcmp({rawdata.Signal_RDScored},'N')) % 1/50 - DONE
n_dLight_N = sum(strcmp({rawdata.Sensor},'dLight1.3b')&strcmp({rawdata.Signal_RDScored},'N')) % 12/50

%% Crop data
% Prepare start and stop
startcropsecs = 5;
endcropsecs = 5*60 + 5;

for eachfile = 1:length(rawdata)
    rawdata(eachfile).sessionstart = floor(startcropsecs*rawdata(eachfile).fs);
    
    if rawdata(eachfile).sessionstart + ceil(endcropsecs*rawdata(eachfile).fs) < length(rawdata(eachfile).sig)
        rawdata(eachfile).sessionend = ceil(endcropsecs*rawdata(eachfile).fs);
    else
        rawdata(eachfile).sessionend = length(rawdata(eachfile).sig);
        sessionlengthmins = (rawdata(eachfile).sessionend - rawdata(eachfile).sessionstart)/rawdata(eachfile).fs/60;
        disp(['WARNING: Session short - file ', num2str(eachfile)])
        disp(['   Cropped length will be ', num2str(sessionlengthmins), ' minutes.'])
    end
end

% Manually adjust cropping for files with issues
idx_ML3M018 = find(strcmp({rawdata.SubjectID},'ML-3M018'));
rawdata(idx_ML3M018).sessionstart = floor(30*rawdata(idx_ML3M018).fs);
rawdata(idx_ML3M018).sessionend = ceil(rawdata(idx_ML3M018).sessionstart+endcropsecs*rawdata(idx_ML3M018).fs);

idx_SC016 = find(strcmp({rawdata.SubjectID},'ML-SC016'));
rawdata(idx_SC016).sessionstart = floor(120*rawdata(idx_SC016).fs);
rawdata(idx_SC016).sessionend = ceil(rawdata(idx_SC016).sessionstart+endcropsecs*rawdata(idx_SC016).fs);

idx_SC024 = find(strcmp({rawdata.SubjectID},'ML-SC024'));
rawdata(idx_SC024).sessionstart = floor(150*rawdata(idx_SC024).fs);
rawdata(idx_SC024).sessionend = ceil(rawdata(idx_SC024).sessionstart+endcropsecs*rawdata(idx_SC024).fs);

idx_437 = find(strcmp({rawdata.SubjectID},'RD-437'));
rawdata(idx_437).sessionstart = floor(20*rawdata(idx_437).fs);
rawdata(idx_437).sessionend = ceil(rawdata(idx_437).sessionstart+endcropsecs*rawdata(idx_437).fs);

idx_1022 = find(strcmp({rawdata.SubjectID},'MK-1022'));
rawdata(idx_1022).sessionstart = floor(65*rawdata(idx_1022).fs);
rawdata(idx_1022).sessionend = ceil(rawdata(idx_1022).sessionstart+endcropsecs*rawdata(idx_1022).fs);

% Crop streams
cropstartfieldname = 'sessionstart'; % name of field with session start index
cropendfieldname = 'sessionend'; % name of field with session end index
streamfieldnames = {'sig', 'baq'}; % which streams to crop

[croppeddata] = cropFPdata(rawdata,cropstartfieldname,cropendfieldname, streamfieldnames); % Output cropped data into new structure called data


%% Process data
% Subset data by sensor
[croppeddata_dLight] = croppeddata(strcmp({croppeddata.Sensor},'dLight1.3b'));
[croppeddata_GCaMP6f] = croppeddata(strcmp({croppeddata.Sensor},'GCaMP6f'));
[croppeddata_GRABDA2h] = croppeddata(strcmp({croppeddata.Sensor},'GRABDA2H'));
[croppeddata_GRABDA3m] = croppeddata(strcmp({croppeddata.Sensor},'GRABDA3M'));

% Subtract and filter data with default settings
% Subtract and filter data
sigfieldname = 'sig';
baqfieldname = 'baq';
fsfieldname = 'fs';

[subtracteddata_GCaMP6f] = subtractFPdata(croppeddata_GCaMP6f,sigfieldname,baqfieldname,fsfieldname,'baqscalingfreqmin',10,'baqscalingfreqmax',80); % adds sigsub (subtracted stream) and sigfilt (subtracted and filtered stream) to data frame
[subtracteddata_dLight] = subtractFPdata(croppeddata_dLight,sigfieldname,baqfieldname,fsfieldname,'baqscalingfreqmin',10,'baqscalingfreqmax',80); % adds sigsub (subtracted stream) and sigfilt (subtracted and filtered stream) to data frame
[subtracteddata_GRABDA2h] = subtractFPdata(croppeddata_GRABDA2h,sigfieldname,baqfieldname,fsfieldname,'baqscalingfreqmin',8,'baqscalingfreqmax',50); % adds sigsub (subtracted stream) and sigfilt (subtracted and filtered stream) to data frame
[subtracteddata_GRABDA3m] = subtractFPdata(croppeddata_GRABDA3m,sigfieldname,baqfieldname,fsfieldname,'baqscalingfreqmin',8,'baqscalingfreqmax',50); % adds sigsub (subtracted stream) and sigfilt (subtracted and filtered stream) to data frame

subtracteddata = [subtracteddata_GCaMP6f; subtracteddata_dLight; subtracteddata_GRABDA2h; subtracteddata_GRABDA3m];

% Remove unneeded fields
includefields = [fieldnames(experimentkey); 'date'; 'sessionduration'; 'starttime'; 'stoptime'; 'fs';...
    'sig';'baq';'baqscaled';'sigsub';'sigfilt';'baqscalingfactor';'sessionstart';'sessionend'];

fieldstoremove_dLight = setdiff(fieldnames(subtracteddata_dLight),includefields);
fieldstoremove_GCaMP6f = setdiff(fieldnames(subtracteddata_GCaMP6f),includefields);
fieldstoremove_GRABDA2h = setdiff(fieldnames(subtracteddata_GRABDA2h),includefields);
fieldstoremove_GRABDA3m = setdiff(fieldnames(subtracteddata_GRABDA3m),includefields);

[data_dLight] = rmfield(subtracteddata_dLight, fieldstoremove_dLight);
[data_GCaMP6f] = rmfield(subtracteddata_GCaMP6f, fieldstoremove_GCaMP6f);
[data_GRABDA2h] = rmfield(subtracteddata_GRABDA2h, fieldstoremove_GRABDA2h);
[data_GRABDA3m] = rmfield(subtracteddata_GRABDA3m, fieldstoremove_GRABDA3m);

[data] = [data_dLight; data_GCaMP6f; data_GRABDA2h; data_GRABDA3m];

clear rawdata
clear croppeddata
clear subtracteddata_dLight
clear subtracteddata_GCaMP6f
clear subtracteddata_GRABDA2h
clear subtracteddata_GRABDA3m
clear data_dLight
clear data_GCaMP6f
clear data_GRABDA2h
clear data_GRABDA3m

%% Plot whole session streams for each file
% Use plotTraces to plot all raw traces - data needs to contain sig, baq, baq_scaled, sigsub, and sigfilt.
for eachfile = 1:length(data)
    maintitle = append(data(eachfile).SubjectID,' - Sensor: ',data(eachfile).Sensor,' - Signal (RD): ',data(eachfile).Signal_RDScored,' - Quality Score: ',num2str(data(eachfile).Signal_RDQuality)); % Create title string for current plot
    plotfilepath = append(subjectfigurepath,'\Session Traces\SessionTraces_',data(eachfile).SubjectID,'_',data(eachfile).Sensor,'_Signal',data(eachfile).Signal_RDScored,'_Quality',num2str(data(eachfile).Signal_RDQuality), '.png');

    if isfile(plotfilepath) == 0 | skipexistingfigs == 0
        alltraces = plotTraces(data,eachfile,maintitle);

        set(gcf, 'Units', 'inches', 'Position', [0, 0, 8, 9]);
        exportgraphics(gcf,plotfilepath,'Resolution',300)
    else
        continue;
    end
end


%% Plot whole session FFT magnitude plots for each file
% Plot FFT power
fsfieldname = 'fs'; % Prepare field names for function inputs 

for eachfile = 1:length(data) % Plot each file
    fileindex = eachfile;
    maintitle = append(data(eachfile).SubjectID,' - Sensor: ',data(eachfile).Sensor,' - Signal (RD): ',data(eachfile).Signal_RDScored,' - Quality Score: ',num2str(data(eachfile).Signal_RDQuality)); % Create title string for current plot
    plotfilepath = append(subjectfigurepath,'\FFTs\SessionFFTpower_',data(eachfile).SubjectID,'_',data(eachfile).Sensor,'_Signal',data(eachfile).Signal_RDScored,'_Quality',num2str(data(eachfile).Signal_RDQuality), '.png');

    if isfile(plotfilepath) == 0 | skipexistingfigs == 0
        plotFFTpower(data,fileindex,maintitle,fsfieldname);
        set(gcf, 'Units', 'inches', 'Position', [0, 0, 8, 9]);
        exportgraphics(gcf,plotfilepath,'Resolution',300)
    else
        continue;
    end
end

%% Split streams into two halves
streamnames = {'sig','baq','baqscaled','sigsub','sigfilt'};

for eachfile = 1:length(data)
    for eachstream = 1:length(streamnames)
        currstream = char(streamnames(eachstream));
        currstreamlength = length(data(eachfile).(currstream));
        halfstreamsamples = floor(currstreamlength/2);

        data(eachfile).(append(currstream,'_split'))([1,2],1:halfstreamsamples) = NaN;
        data(eachfile).(append(currstream,'_split'))(1,1:halfstreamsamples) = data(eachfile).(currstream)(1:halfstreamsamples);
        data(eachfile).(append(currstream,'_split'))(2,1:halfstreamsamples) = data(eachfile).(currstream)(halfstreamsamples+1:halfstreamsamples*2);
    end
end
        
%% Quantify Streams in Time Domain
splitstreamnames = {'sig_split','baq_split','baqscaled_split','sigsub_split','sigfilt_split'};
splitn = 2;

binsizesecs = 30;

variablenames = {'Split','Variable','sig','baq','baqscaled','sigsub','sigfilt','sigtobaq','sigtobaqscaled'};
quantificationvariables = ["mean","range","var","sd","rms",...
                           "mean_bins","range_bins","var_bins","sd_bins","rms_bins"];

emptystreamvariablearray = nan(length(quantificationvariables), length(variablenames));
emptysplitstreamtable = array2table(emptystreamvariablearray, 'VariableNames', variablenames);

for eachfile = 1:length(data)
    % Prep for quantification
    binsizesamples = floor(binsizesecs*data(eachfile).fs);
    currvariables = table();
   
    for eachsplit = 1:splitn
        currsplitstreamtable = emptysplitstreamtable;
        currsplitstreamtable.Split(1:length(quantificationvariables)) = eachsplit;
        currsplitstreamtable.Variable = quantificationvariables';

        for eachsplitstream = 1:length(splitstreamnames)
            currsplitstreamname = char(splitstreamnames(eachsplitstream)); % Current stream name
            currstreamcol = extractBefore(currsplitstreamname, '_'); % Column in table

            % Prepare data stream (remove NaNs)
            currstream_raw = data(eachfile).(currsplitstreamname)(eachsplit,:);
            currstream = currstream_raw(~isnan(currstream_raw)); % Removed NaNs

            % Overall stream variables
            currsplitstreamtable.(currstreamcol)(1) = mean(currstream); % Mean
            currsplitstreamtable.(currstreamcol)(2) = max(currstream)-min(currstream);
            currsplitstreamtable.(currstreamcol)(3) = var(currstream);
            currsplitstreamtable.(currstreamcol)(4) = std(currstream);
            currsplitstreamtable.(currstreamcol)(5) = rms(currstream);

            % Stream variables by 30s time bin
            currsplitnbins = floor(length(currstream) / binsizesamples);
            binend = 0;
            for eachbin = 1:currsplitnbins % Find all values by bins
                binstart = binend+1;

                if binstart+binsizesamples<length(currstream)
                    binend = binstart+binsizesamples;
                else
                    binend = length(currstream);
                end

                binmean(eachbin) = mean(currstream(binstart:binend));
                binrange(eachbin) = max(currstream(binstart:binend))-min(currstream(binstart:binend));
                binvar(eachbin) = var(currstream(binstart:binend));
                binsd(eachbin) = std(currstream(binstart:binend));
                binrms(eachbin) = rms(currstream(binstart:binend));
            end
            % Add means of bin values to table
            currsplitstreamtable.(currstreamcol)(6) = mean(binmean); % Mean
            currsplitstreamtable.(currstreamcol)(7) = mean(binrange); % Mean
            currsplitstreamtable.(currstreamcol)(8) = mean(binvar); % Mean
            currsplitstreamtable.(currstreamcol)(9) = mean(binsd); % Mean
            currsplitstreamtable.(currstreamcol)(10) = mean(binrms); % Mean
        end

        % Find ratio of raw signal to raw and scaled background
        currsplitstreamtable.sigtobaq = currsplitstreamtable.sig./currsplitstreamtable.baq;
        currsplitstreamtable.sigtobaqscaled = currsplitstreamtable.sig./currsplitstreamtable.baqscaled;

        % Combine tables for each split
        currvariables = [currvariables;currsplitstreamtable];
    end
    % Add quantification table to data structure
    data(eachfile).quantification_streams = currvariables;
end
            
%% Quantify Frequency Domain
splitstreamnames = {'sig_split','baq_split','baqscaled_split','sigsub_split','sigfilt_split'};
splitn = 2;

FFTbandstartfreqs = [0.0051, 2.5, 10, 0.0051];
FFTbandstopfreqs = [2.5, 10, 100, 100];

FFTbands.Band = [1,2,3,4]';
FFTbands.start = FFTbandstartfreqs';
FFTbands.stop = FFTbandstopfreqs';

minfreq = 0.0051;
maxfreq = 100; % Max frequency to include

FFTvariablenames = {'Split','Variable','sig','baq','baqscaled','sigsub','sigfilt','sigtobaq','sigtobaqscaled'};

FFTquantificationvariables = ["PowerMean_Band1","PowerMean_Band2","PowerMean_Band3","PowerMean_Band4",...
                              "PowerSum_Band1","PowerSum_Band2","PowerSum_Band3","PowerSum_Band4",...
                              "MagMean_Band1","MagMean_Band2","MagMean_Band3","MagMean_Band4",...
                              "MagSum_Band1","MagSum_Band2","MagSum_Band3","MagSum_Band4"];

FFTemptyvariablearray = nan(length(FFTquantificationvariables), length(FFTvariablenames));
FFTemptysplittable = array2table(FFTemptyvariablearray, 'VariableNames', FFTvariablenames);

for eachfile = 1:length(data)
    currFFTvariables = table();
    for eachsplit = 1:splitn
        currsplitFFTtable = FFTemptysplittable;
        currsplitFFTtable.Split(1:length(FFTquantificationvariables)) = eachsplit;
        currsplitFFTtable.Variable = FFTquantificationvariables';

        for eachsplitstream = 1:length(splitstreamnames)
            currsplitstreamname = char(splitstreamnames(eachsplitstream)); % Current stream name
            currstreamcol = extractBefore(currsplitstreamname, '_'); % Column in table

            % Prepare data stream (remove NaNs)
            currstream_raw = data(eachfile).(currsplitstreamname)(eachsplit,:);
            currstream = currstream_raw(~isnan(currstream_raw)); % Removed NaNs
            
            % Prepare data stream FFT
            [currstreamFFT, currstreamF] = preparestreamFFT(currstream,data(eachfile).fs);
            
            currstreamidxs = currstreamF > minfreq & currstreamF < maxfreq; % Find indices of frequencies above the set min threshold and below the set max threshold
            
            currstreamF = currstreamF(currstreamidxs);
            currstreamFFTmag = currstreamFFT(currstreamidxs);
            currstreamFFTpower = currstreamFFTmag.^2;

            % Band values
            currFFTpowermean = [];
            currFFTpowersum = [];
            currFFTmagmean = [];
            currFFTmagsum = [];
            for eachband = 1:length(FFTbands.start)
                currbandidxs = currstreamF > FFTbands.start(eachband) & currstreamF < FFTbands.stop(eachband);

                currFFTpowermean(eachband) = mean(currstreamFFTpower(currbandidxs));
                currFFTpowersum(eachband) = sum(currstreamFFTpower(currbandidxs));

                currFFTmagmean(eachband) = mean(currstreamFFTmag(currbandidxs));
                currFFTmagsum(eachband) = sum(currstreamFFTmag(currbandidxs));
            end 

            currsplitFFTtable.(currstreamcol)(1:4) = currFFTpowermean;
            currsplitFFTtable.(currstreamcol)(5:8) = currFFTpowersum;
            currsplitFFTtable.(currstreamcol)(9:12) = currFFTmagmean;
            currsplitFFTtable.(currstreamcol)(13:16) = currFFTmagsum;
        end

        % Find ratio of raw signal to raw and scaled background
        currsplitFFTtable.sigtobaq = currsplitFFTtable.sig./currsplitFFTtable.baq;
        currsplitFFTtable.sigtobaqscaled = currsplitFFTtable.sig./currsplitFFTtable.baqscaled;

        % Combine tables for each split
        currFFTvariables = [currFFTvariables;currsplitFFTtable];
    end
    % Add quantification table to data structure
    data(eachfile).quantification_FFT = currFFTvariables;
end


%% Combine all variables into one output table
allvariables = table();

for eachfile = 1:length(data)
    % Merge time and frequency domain varaibles into one table
    currfilevariables = [data(eachfile).quantification_streams; data(eachfile).quantification_FFT];
    % Prepare overall table with SubjectID
    currfiletable = table();
    currfiletable.SubjectID(1:height(currfilevariables)) = {data(eachfile).SubjectID};
    currfiletable = [currfiletable, currfilevariables];

    % Combine into overall table
    allvariables = [allvariables; currfiletable];
end

% Output variables table to csv
exportfilepath_quantificationvariables = append(analysispath,'FPSignalCharacterizationAutomation_AllQuantificationVariables.csv');
writetable(allvariables,exportfilepath_quantificationvariables);

% Output FFT Bands
FFTbandstable = struct2table(FFTbands);
exportfilepath_FFTbands = append(analysispath,'FPSignalCharacterizationAutomation_FFTBands.csv');
writetable(FFTbandstable,exportfilepath_FFTbands);

% Output Experiment Variables
experimentvariablestable = struct2table(experimentkey);
experimentvariablestable.baqscalingfactor = [data.baqscalingfactor]';

exportfilepath_experimentvariables = append(analysispath,'FPSignalCharacterizationAutomation_ExperimentalVariables.csv');
writetable(experimentvariablestable,exportfilepath_experimentvariables);

%% Center sig and baqscaled
centerstreamnames = {'sig','baqscaled'};

for eachfile = 1:length(data)
    for eachcenterstream = 1:length(centerstreamnames)
        currcenterstream = centerstreamnames{eachcenterstream};
        data(eachfile).(append(currcenterstream,'_centered')) = data(eachfile).(currcenterstream) - mean(data(eachfile).(currcenterstream));
    end
end

%% Downsample streams
% Downsample Group Mean Trial Traces and Output to Table
fs = data(1).fs; % original sampling rate
targetFs = 20; % downsampled rate
block = round(fs / targetFs); % samples per output point

streamnames = {'sig','baq','baqscaled','sigsub','sigfilt','sig_centered','baqscaled_centered'};

for eachfile = 1:length(data)
    disp(['Downsampling file: ',num2str(eachfile)]);
    for eachstream = 1:length(streamnames)
        currstreamname = streamnames{eachstream};
        currfieldstream = data(eachfile).(currstreamname);

        % Downsample and add to data struct
        currstreamds = movmedian(currfieldstream, block, 'Endpoints','discard'); % Find block medians
        currstreamds = currstreamds(1:block:end);
        data(eachfile).([currstreamname,'_ds']) = currstreamds; % take one per block
    end
end


%% Correlation Analysis
dsfs = 20;
windowsec = .5;       % Window size in seconds (adjust based on dynamics)
w = round(windowsec*dsfs); % Window size in samples

corrstreamfieldnames = {'sig_ds','baqscaled_ds','sigsub_ds','sig_centered_ds','baqscaled_centered_ds'};

% Find moving mean and sd for each stream
for eachfile = 1:length(data)
    for eachcorrstream = 1:length(corrstreamfieldnames)
        currcorrstream = corrstreamfieldnames{eachcorrstream};
        currcorrstreamdata = data(eachfile).(currcorrstream);
        data(eachfile).(['t_',currcorrstream]) = 1:1:length(currcorrstreamdata); % Time vector
        data(eachfile).(['mu_',currcorrstream]) = movmean(currcorrstreamdata, w);
        data(eachfile).(['sd_',currcorrstream]) = movstd(currcorrstreamdata, w);
    end
end

% Calculate correlations
corrxfields = {'sig_ds','sigsub_ds','sigsub_ds'};
corryfields = {'baqscaled_ds','sig_centered_ds','baqscaled_centered_ds'};
corrlabels = {'sigbaqscaled','sigsubsig','sigsubbaqscaled'};
        
for eachfile = 1:length(data)
    for eachcorr = 1:length(corrxfields)
        currcorrxfield = corrxfields{eachcorr};
        currcorryfield = corryfields{eachcorr};
        currcorrlabel = corrlabels{eachcorr};

        currxdata = data(eachfile).(currcorrxfield);
        currydata = data(eachfile).(currcorryfield);
        currxmudata = data(eachfile).(['mu_',currcorrxfield]);
        currymudata = data(eachfile).(['mu_',currcorryfield]);
        currxsddata = data(eachfile).(['sd_',currcorrxfield]);
        currysddata = data(eachfile).(['sd_',currcorryfield]);

        currcov = movmean(currxdata .* currydata, w) - currxmudata .* currymudata;
        currrho = currcov ./ (currxsddata .* currysddata);
        currrhotime = ((0:length(currxdata)-1)' / dsfs)'; 
    
        currp = polyfit(currxdata, currydata, 1); 
        currslope = currp(1);
        currint = currp(2);

        currxline = linspace(min(currxdata), max(currxdata), 100);
        curryline = polyval(currp, currxline);
        
        currRmatrix = corrcoef(currxdata, currydata);
        currglobalR = currRmatrix(1,2);

        % Output to data
        data(eachfile).(['rho_',currcorrlabel]) = currrho;
        data(eachfile).(['rhotime_',currcorrlabel]) = currrhotime;
        data(eachfile).(['xline_',currcorrlabel]) = currxline;
        data(eachfile).(['yline_',currcorrlabel]) = curryline;
        data(eachfile).(['globalR_',currcorrlabel]) = currglobalR;
    end
end


%% Plot Session Corrs
% tracetitles = {'Raw 465 with Scaled 405', 'Subtracted 465 with Raw 465 (Centered)','Subtracted 465 with Scaled 405 (Centered)'};
% corrtitles = {'Raw 465 vs Scaled 405: Moving Correlation Coefficient', 'Subtracted 465 vs Raw 465: Moving Correlation Coefficient', 'Subtracted 465 vs Scaled 405: Moving Correlation Coefficient'};
% scattertitles = {'Raw 465 vs Scaled 405 (Global R = ', 'Subtracted 465 vs Raw 465 (Global R = ', 'Subtracted 465 vs Scaled 405 (Global R = '};
% scatterxlabels = {'Raw 465 F','Subtracted 465 F','Subracted 465 F'};
% scatterylabels = {'Scaled 405 F','Raw 465 F (Centered)','Scaled 405 F (Centered)'};
plotcorrxfields = {'sig_ds'};
plotcorryfields = {'baqscaled_ds'};
plotcorrlabels = {'sigbaqscaled'};
      
tracetitles = {'Raw 465 with Scaled 405'};
corrtitles = {'Raw 465 vs Scaled 405: Moving Correlation Coefficient'};
scattertitles = {'Raw 465 vs Scaled 405 (Global R = '};
scatterxlabels = {'Raw 465 F'};
scatterylabels = {'Scaled 405 F'};

for eachfile = 1:length(data)
    maintitle = append(data(eachfile).SubjectID,' - ',data(eachfile).Sensor); % Create title string for current plot
    plotfilepath = append(subjectfigurepath,'\Stream Correlations\StreamCorrelations_',data(eachfile).SubjectID,'_',data(eachfile).Sensor, '.png');

    if isfile(plotfilepath) == 0 
        close all
        corrplots = tiledlayout(3, length(plotcorrxfields), 'Padding','compact', 'TileSpacing','compact')

        for eachcorr = 1:length(plotcorrxfields)
            currcorrxfield = plotcorrxfields{eachcorr};
            currcorryfield = plotcorryfields{eachcorr};
            currcorrlabel = plotcorrlabels{eachcorr};
        
            currxdata = data(eachfile).(currcorrxfield);
            currydata = data(eachfile).(currcorryfield);
            currtdata = data(eachfile).(['t_',currcorrxfield]);
        
            currrho = data(eachfile).(['rho_',currcorrlabel]);
            currxline = data(eachfile).(['xline_',currcorrlabel]);
            curryline = data(eachfile).(['yline_',currcorrlabel]);
            currglobalR = data(eachfile).(['globalR_',currcorrlabel]);
        
            % plot1tile = eachcorr;
            % plot2tile = eachcorr+3;
            % plot3tile = eachcorr+6;

            plot1tile = eachcorr;
            plot2tile = eachcorr+1;
            plot3tile = eachcorr+2;

            plotxticks = 0:floor(60*dsfs):length(currtdata);
            plotxlabels = 0:1:5;

            % Plot 1: Traces - Raw 465 and Scaled 405
            nexttile(plot1tile)
            hold on
            plot(currtdata, currxdata, 'LineWidth', 1);
            plot(currtdata, currydata, 'LineWidth', 1);
            title(tracetitles{eachcorr});
            xlim([0, length(currtdata)])
            xticks(plotxticks)
            xticklabels(plotxlabels)
            ylabel('Fluorescence (A.U.)');
            xlabel('Time (mins)');
            hold off
            
            % Plot 2: Correlation over Time - Raw 465 and Scaled 405
            nexttile(plot2tile)
            hold on
            plot(currtdata, currrho, 'LineWidth', 1);
            yline(0, '--k'); % Zero correlation line
            title(corrtitles{eachcorr});
            xlim([0, length(currrho)])
            xticks(plotxticks)
            xticklabels(plotxlabels)
            ylabel('Correlation (\rho)');
            xlabel('Time (mins)');
            ylim([-1.5 1.5]);
            hold off
            
            % Plot 3: Scatter with Corr line - Raw 465 and Scaled 405
            nexttile(plot3tile)
            hold on;
            scatter(currxdata, currydata,1); 
            xlabel('Raw 465');
            ylabel('Scaled 405');
            plot(currxline, curryline, 'r', 'LineWidth', 3); % Plot the Regression Line
            title([scattertitles{eachcorr} num2str(currglobalR, '%.2f') ')']);
            xlabel(scatterxlabels{eachcorr});
            ylabel(scatterylabels{eachcorr});
            hold off;
        end
        title(corrplots, maintitle, 'Interpreter', 'none');

        set(gcf, 'Units', 'inches', 'Position', [0, 0, 18, 12]);
        exportgraphics(gcf,plotfilepath,'Resolution',300)
    else
        continue;
    end
end

%% Export to R - Moving Window Correlations - Rho
addfieldnames = fieldnames(experimentkey);
% Export table for each corr
for eachcorr = 1:length(corrxfields)
    currcorrtable = table();
    currcorrlabel = corrlabels{eachcorr};

    for eachfile = 1:length(data)
        currfilerhotable = table();
        currfilerhotable.Time = data(eachfile).(['rhotime_',currcorrlabel])';
        currfilerhotable.Rho = data(eachfile).(['rho_',currcorrlabel])';
        currfilerhotable.Corr(1:height(currfilerhotable)) = {currcorrlabel};

        for eachaddfield = 1:length(addfieldnames)
            curraddfieldname = addfieldnames{eachaddfield};
            currfilerhotable.(curraddfieldname)(1:height(currfilerhotable)) = {data(eachfile).(curraddfieldname)};
        end
        currcorrtable = [currcorrtable; currfilerhotable];
    end
    writetable(currcorrtable,append(analysispath,'MATLAB Outputs\StreamCorrelations_Rho_allsensors_',currcorrlabel,'.csv'));
end

%% Export to R - Scatter Data
% Export table for each corr
corrstreamfieldnames = {'sig_ds','baqscaled_ds','sigsub_ds','sig_centered_ds','baqscaled_centered_ds'};
streamtable = table();
for eachfile = 1:length(data)
    currfilestreamtable = table();
    for eachcorrstream = 1:length(corrstreamfieldnames)
        currcorrstreamname = corrstreamfieldnames{eachcorrstream};
        currfilestreamtable.(currcorrstreamname) = data(eachfile).(currcorrstreamname)';
    end
    for eachaddfield = 1:length(addfieldnames)
        curraddfieldname = addfieldnames{eachaddfield};
        currfilestreamtable.(curraddfieldname)(1:height(currfilestreamtable)) = {data(eachfile).(curraddfieldname)};
    end
    streamtable = [streamtable; currfilestreamtable];
end

writetable(streamtable,append(analysispath,'MATLAB Outputs\StreamValues_allstreams_allsensors.csv'));



%% EXAMPLE PLOT: Traces
exampletraceidx_GCaMP6f = find(strcmp({data.SubjectID},'RD455'));
exampletraceidx_dLight = find(strcmp({data.SubjectID},'RD479'));
exampletraceidx_GRABDA2h = find(strcmp({data.SubjectID},'RD1001'));

exampletracecolor_GCaMP6f = '#008121';
exampletracecolor_dLight = '#003FD1';
exampletracecolor_GRABDA2h = '#EA007D';

exampletracecolor_baqscaled = '#8200C8';

tracexticks = 0:ceil(dsfs*60):ceil(5*dsfs*60);
tracexlabels = 0:1:5;

ymin_GCaMP6f = 165;
ymax_GCaMP6f = 196.5;
yticks_GCaMP6f = ymin_GCaMP6f:10:ymax_GCaMP6f;

ymin_dLight = 72;
ymax_dLight = 94.5;
yticks_dLight = ymin_dLight:7:ymax_dLight;

ymin_GRABDA2h = 135;
ymax_GRABDA2h = 210;
yticks_GRABDA2h = ymin_GRABDA2h:25:ymax_GRABDA2h;

close all
exampletraces = tiledlayout(1, 3, 'Padding','compact', 'TileSpacing','compact')

% GCaMP6f Trace
nexttile(1)
hold on
plot(data(exampletraceidx_GCaMP6f).t_sig_ds,data(exampletraceidx_GCaMP6f).sig_ds,'LineWidth', .75,'Color',exampletracecolor_GCaMP6f);
plot(data(exampletraceidx_GCaMP6f).t_baqscaled_ds,data(exampletraceidx_GCaMP6f).baqscaled_ds,'LineWidth', .75,'Color',exampletracecolor_baqscaled);
xticks(tracexticks)
xticklabels(tracexlabels)
ylim([ymin_GCaMP6f, ymax_GCaMP6f])
yticks(yticks_GCaMP6f)
ylabel('Fluorescence (A.U.)');
xlabel('Time (mins)');
hold off
           
% dLight1.3b Trace
nexttile(2)
hold on
plot(data(exampletraceidx_dLight).t_sig_ds,data(exampletraceidx_dLight).sig_ds,'LineWidth', .75, 'Color',exampletracecolor_dLight);
plot(data(exampletraceidx_dLight).t_baqscaled_ds,data(exampletraceidx_dLight).baqscaled_ds,'LineWidth', .75,'Color',exampletracecolor_baqscaled);
xticks(tracexticks)
xticklabels(tracexlabels)
ylim([ymin_dLight, ymax_dLight])
yticks(yticks_dLight)
ylabel('Fluorescence (A.U.)');
xlabel('Time (mins)');
hold off

% GRABDA2h Trace
nexttile(3)
hold on
plot(data(exampletraceidx_GRABDA2h).t_sig_ds,data(exampletraceidx_GRABDA2h).sig_ds,'LineWidth', .75,'Color',exampletracecolor_GRABDA2h);
plot(data(exampletraceidx_GRABDA2h).t_baqscaled_ds,data(exampletraceidx_GRABDA2h).baqscaled_ds,'LineWidth', .75,'Color',exampletracecolor_baqscaled);
xticks(tracexticks)
xticklabels(tracexlabels)
ylim([ymin_GRABDA2h, ymax_GRABDA2h])
yticks(yticks_GRABDA2h)
ylabel('Fluorescence (A.U.)');
xlabel('Time (mins)');
hold off

allaxes = findobj(exampletraces, 'Type', 'axes');
set(allaxes, 'TickDir','out','Box','off')
set(gcf, 'Units', 'inches', 'Position', [0, 0, 12, 2]);
exportgraphics(gcf,append(overallfigurepath,'Figure_ExampleTracesbySensor.eps'),'Resolution',300)
