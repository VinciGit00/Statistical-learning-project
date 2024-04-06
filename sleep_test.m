clear all
clc

data = readtable('./Sleep_health_and_lifestyle_dataset.csv');

% Validation (train: 70%, test: 30%)
cv = cvpartition(size(data,1),'HoldOut',0.30);
idx = cv.test;

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


res = fitlm(dataready)
ypred = predict(res, xtest);

aa = not(isnan(ypred));

ypred = ypred(aa,:);
ytest = ytest(aa,:);

mse = sqrt(mean((ytest-ypred).^2))



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

















