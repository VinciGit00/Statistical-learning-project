%% TRAIN AND TEST DATASETS (20% TEST)

clear all
clc

data = readtable('./Sleep_health_and_lifestyle_dataset.csv');

% Validation (train: 80%, test: 20%)
cv = cvpartition(size(data,1),'HoldOut',0.30);
idx = cv.test;


% ATTENZIONE !!! Rimuovo colonna bloood pressure
data(:, "BloodPressure") = [];
data(:, "PersonID") = [];
%data(:, "QualityOfSleep") = [];



% ANALISI DEI DATI
sleepduration = table2array(data(:,"SleepDuration"));
histfit(sleepduration)
mean(sleepduration)
std(sleepduration)

%{
sleepQuality = table2array(data(:,"QualityOfSleep"));
histfit(sleepQuality)
mean(sleepQuality)
std(sleepQuality)
%}


scatter(table2array(data(:,"Age")), table2array(data(:,"SleepDuration")))
scatter(table2array(data(:,"DailySteps")), table2array(data(:,"SleepDuration")))
scatter(table2array(data(:,"QualityOfSleep")), table2array(data(:,"SleepDuration")))
scatter(table2array(data(:,"QualityOfSleep")), table2array(data(:,"SleepDuration")))


corr(table2array(data(:,"Age")), table2array(data(:,"SleepDuration")))
corr(table2array(data(:,"DailySteps")), table2array(data(:,"SleepDuration")))
corr(table2array(data(:,"QualityOfSleep")), table2array(data(:,"SleepDuration")))


% Separate to training and test data
dataTrain = data(~idx,:);
dataTest  = data(idx,:);


ytrain = dataTrain(:,"SleepDuration");
dataTrain(:,"SleepDuration") = [];
xtrain = dataTrain;

ytest = dataTest(:,"SleepDuration");
dataTest(:,"SleepDuration") = [];
xtest = dataTest;


dataready = [xtrain ytrain];


res = fitlm(dataready);
[ypred yci] = predict(res, xtest);

aa = not(isnan(ypred));

ypred = ypred(aa,:);
ytest = ytest(aa,:);

mse = sqrt(mean((ytest-ypred).^2))
r2 = res.Rsquared

% ANALISI RESIDUI
residui = table2array(ytest-ypred);
histfit(residui)
mean(residui)

% info residui: quando uso 90% test, la maggiorparte delle previsioni ha
% residui piccoli, mentre alcune residui molto alti (7+). Quindi maggior
% parte delle volte la previsione è giusta, ma altre volte sbaglia
% completamente (infatti la media residui è più alta rispetto a quando uso
% 30% test)
adf = jbtest(residui)


%% LOOCV
clear all
clc
data = readtable('./Sleep_health_and_lifestyle_dataset.csv');

mselist = [];
for i=1:size(data, 1)
    dataTrain = data;
    dataTest  = data(i,:);    
    
    ytrain = dataTrain(:,"SleepDuration");
    dataTrain(:,"SleepDuration") = [];
    xtrain = dataTrain;
    
    ytest = dataTest(:,"SleepDuration");
    dataTest(:,"SleepDuration") = [];
    xtest = dataTest;
    
    dataready = [xtrain ytrain];

    res = fitlm(dataready);
    ypred = predict(res, xtest);
        
    mse = sqrt(mean((ytest-ypred).^2));
    mselist = [mselist table2cell(mse)];
    i
end

mean(cell2mat(mselist))



%% K FOLD (K = 10)
clear all
clc
data = readtable('./Sleep_health_and_lifestyle_dataset.csv');

kfolds = 10;

cvobj = cvpartition(size(data,1),"KFold",kfolds)

mselist = [];
for i=1:kfolds
    idx = cvobj.test(i);
    % Separate to training and test data
    dataTrain = data(~idx,:);
    dataTest  = data(idx,:);   
    
    ytrain = dataTrain(:,"SleepDuration");
    dataTrain(:,"SleepDuration") = [];
    xtrain = dataTrain;
    
    ytest = dataTest(:,"SleepDuration");
    dataTest(:,"SleepDuration") = [];
    xtest = dataTest;
    
    dataready = [xtrain ytrain];

    res = fitlm(dataready);
    ypred = predict(res, xtest);
        
    mse = sqrt(mean((ytest-ypred).^2));
    mselist = [mselist table2cell(mse)];
    i
end

% ATTENZIONE! CI SONO DEI NAN DENTRO MSELIST, DATO CHE IL COMANDO PREDICT()
% PRODUCE DEI VALORI NAN QUANDO INCONTRAM, DENTRO XTEST, DELLE VARIABILI CATEGORICHE CHE
% NEL DATASET DI TRAIN NON HA MAI INCONTRATO!!
nanmean(cell2mat(mselist))


%% BOOTSTRAP

















