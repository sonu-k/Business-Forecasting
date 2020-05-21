library(fpp)
library(fpp2)
library(data.table)
library(ggplot2)

data=copy(USD_INR_2_Finaldataset) #copy dataset into a new data container
View(data)
setDT(data) #set as data table
str(data) #checking for any mismatch in data types
data[,Date:=as.Date(Date,'%m/%d/%Y')] #changing 'date' column from char to Date format

ts=ts(data$Price,frequency = 365) #make the price column of the data as a time series (yearly)
autoplot(ts)+ylab("Rupee")+xlab("Year") #visualize
plot(stl(ts,s.window=5)) #visualize seasonality and trend seperately
trainset=data[1:3652] #training set (first 3652 rows)
testset=data[3653:3957] #test set (last 305 rows)

tdata=ts(trainset$Price,frequency=365,start=c(2009,1,1)) #converting the price column of the training data into a time series
ggseasonplot(tdata,year.labels = TRUE, year.labels.left = TRUE) #plot the training price data

tesdata=ts(testset$Price,frequency=365,start=c(2019,1,1)) #repeat for test set
autoplot(tdata)+
  autolayer(tesdata,color='black')+
  ylab("Rupees") #visualize
View(tdata)
View(tesdata)

#checking for seasonality in the price column data

Pricedata=ts(data$Price,frequency=365,start=c(2009,1,1))
plot(Pricedata)
plot(stl(Pricedata,s.window = 10)) #it is seasonal


#NAIVE METHOD

nfit=naive(tdata,h=305) #fit the model on training data
nfc=forecast(nfit,h=305) #forecast 305 future values

autoplot(tdata)+ #visualize fitted values and predictions
  autolayer(tesdata,series = "Test data")+
  autolayer(fitted(nfit),series="Fitted values")+
  autolayer(nfc,series="Naive forecasts",PI=FALSE)+
    ggtitle("NAIVE FORECASTING METHOD")

#SEASONAL NAIVE METHOD

sfit=snaive(tdata,h=305) #fit the model on training data
sfc=forecast(sfit,h=305) #forecast 305 future values

autoplot(tdata)+ #visualize fitted values and predictions
  autolayer(tesdata,series = "Test data")+
  autolayer(fitted(sfit),series="Fitted values")+
  autolayer(sfc,series="S-Naive forecasts",PI=FALSE)+
  ggtitle("SEASONAL NAIVE FORECASTING METHOD")

#MEAN METHOD

mfit=meanf(tdata,h=305)#fit the model on training data
mfc=forecast(mfit,h=305)#forecast 305 future values

autoplot(tdata)+ #visualize fitted values and predictions
  autolayer(tesdata,series = "Test data")+
  autolayer(fitted(mfit),series="Fitted values")+
  autolayer(mfc,series="Simple Avg forecasts",PI=FALSE)+
  ggtitle("SIMPLE AVERAGE FORECASTING METHOD")

#ARIMA METHOD

ndiffs(tdata) #checking number of times data needs to be differenced
afit=auto.arima(tdata)#fit the model on training data
afit #checking ar and ma values(p=4,q=0) 
afc=forecast(afit,h=305)#forecast 305 future values

autoplot(tdata)+ #visualize fitted values and predictions
  autolayer(tesdata,series = "Test data")+
  autolayer(fitted(afit),series="Fitted values")+
  autolayer(afc,series="Arima forecasts",PI=FALSE)+
  ggtitle("ARIMA MODEL")

#ACCURACY COMPARISONS

fit1=accuracy(nfc,x=tesdata) #checking for accuracy on test data
fit2=accuracy(sfc,x=tesdata)
fit3=accuracy(mfc,x=tesdata)
fit4=accuracy(afc,x=tesdata)
fit1 #checking the different error values to choose best model
fit2
fit3
fit4
#naive works the best since conversion rates are strongly dependent on its previous day's rate

autoplot(tdata)+ #visualizing all the model predictions
  autolayer(tesdata,series = "Test data")+
  autolayer(nfc,series="Naive forecasts",PI=FALSE)+
  autolayer(sfc,series="SNaive forecasts",PI=FALSE)+
  autolayer(mfc,series="Simple Average forecasts",PI=FALSE)+
  autolayer(afc,series="Arima forecasts",PI=FALSE)+
  ggtitle("MODEL COMPARISONS")
