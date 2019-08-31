#time series forecasting for different time series datasets in R with different methods.
#Relevenat plots are generated and the code is commented with the results wherever appropriate

rm(list=ls())
# cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
# setwd(cdir)

library(forecast)
library(tseries)
library(GauPro)
library(kernlab)
library(dplyr)
library(lubridate)
library(prophet)
library(plotly)
library(Metrics)
library(htmlwidgets)

library(reticulate)

# Accept parameters, args[6] is the R package repo url
args <- commandArgs()

# All installed packages
pkgs <- installed.packages()

# List of required packages for this project

reqs <- c("htmlwidgets","forecast","tseries","GauPro","kernlab","dplyr","plotly","lubridate","prophet","Metrics","reticulate")

# Try to install the dependencies if not installed
sapply(reqs, function(x){
  if (!x %in% rownames(pkgs)) {
    install.packages(x,dependencies = T)
  }
})


mlflow<-import("mlflow")


##################### Foreasting function #####################

forecasting_plot_func<-function(data1,testno,data1_train,data1_test){
  
  rmse_df<-data.frame(ARIMA=numeric(1),GPSQE=numeric(1),GPV=numeric(1),Pr=numeric(1))
  data1<-data
  data1_train<-data_train
  data1_test<-data_test
  
  train_df<-data.frame(Time=as.numeric(time(data1_train)),TrainData = as.numeric(data1_train))
  test_df<-data.frame(Time=as.numeric(time(data1_test)),TestActualData = as.numeric(data1_test))
  
  
  ##################### ARIMA model #################################
  
  
  fit_ARIMA = auto.arima(data1_train)
  
  fcast_ARIMA <- forecast(data1_train,testno)
  lower_ARIMA<-as.data.frame(fcast_ARIMA$lower)
  upper_ARIMA<-as.data.frame(fcast_ARIMA$upper)
  test_df$Predicted=fcast_ARIMA$mean
  test_df$Lower=lower_ARIMA$`80%`
  test_df$Upper=upper_ARIMA$`80%`
  df<-full_join(train_df,test_df)
  p1<-plot_func(df,"Auto ARIMA")
  rmse_df$ARIMA<-rmse((test_df$Predicted),test_df$TestActualData)
  
  
  ##################### Gaussian process regression using squared-exponential kernel function #####################
  
  gp <- GauPro(X=1:length(data1_train), Z=data1_train, parallel=FALSE)
  
  x<-1:length(data1)
  xlim_Vals<-as.numeric(time(data1))
  
  gp_prediction<-as.numeric(gp$predict(x))
  gp_test<-tail(gp_prediction,testno)
  gp_var_prediction<-as.numeric(gp$predict(x, se=T)$se)
  gp_var<-tail(gp_var_prediction,testno)
  
  test_df$Predicted=gp_test
  test_df$Lower=gp_test-0.5*gp_var
  test_df$Upper=gp_test+0.5*gp_var
  df<-full_join(train_df,test_df)
  p2<-plot_func(df,"GP with squared exponential kernel")
  rmse_df$GPSQE<-rmse((test_df$Predicted),test_df$TestActualData)
  
  
  
  
  ##################### Gaussian process regression using vanilladot kernel function #####################
  
  gp2 <- gausspr(1:length(data1_train), data1_train,kernel="vanilladot",variance.model="TRUE")
  
  gp_prediction<-as.numeric(predict(gp2,x))
  gp_test<-tail(gp_prediction,testno)
  gp_var_prediction<-as.numeric(predict(gp2,x,type="sdeviation"))
  gp_var<-tail(gp_var_prediction,testno)
  
  test_df$Predicted=gp_test
  test_df$Lower=gp_test-0.03*gp_var
  test_df$Upper=gp_test+0.03*gp_var
  df<-full_join(train_df,test_df)
  p3<-plot_func(df,"GP with Vanilladot kernel")
  rmse_df$GPV<-rmse((test_df$Predicted),test_df$TestActualData)
  
  
  
  ##################### Using prophet library #####################
  
  history <- data.frame(
    ds = seq(as.Date('2015-01-01'), as.Date('2015-01-01')+length(data1_train)-1, by = 'd'),
    y=as.numeric(data1_train)
  )
  m <- prophet(history)
  future <- make_future_dataframe(m, periods = testno)
  forecast <- predict(m, future)
  
  forecast2<-head(forecast,(length(data1)-testno))
  forecast3<-tail(forecast,testno)
  
  test_df$Predicted=forecast3$yhat
  test_df$Lower=forecast3$yhat_lower
  test_df$Upper=forecast3$yhat_upper
  df<-full_join(train_df,test_df)
  p4<-plot_func(df,"Using Prophet library")
  rmse_df$Pr<-rmse((test_df$Predicted),test_df$TestActualData)
  
  
  p<-list(p1,p2,p3,p4)
  return(list(p,rmse_df))
  
}


plot_func<-function(df,tit){
  p <- plot_ly(df) %>%
    add_trace(x = ~Time, y = ~TrainData, name = 'Training Data',type = 'scatter',mode = 'lines') %>%
    add_trace(x = ~Time, y = ~TestActualData, name = 'Actual Test Data',type = 'scatter',mode = 'markers') %>%
    add_trace(x = ~Time, y = ~Predicted,name = 'Predicted',type = 'scatter',mode = 'lines') %>%
    add_trace(x = ~Time, y = ~Upper,name = 'Upper confidence bound',type = 'scatter',mode = 'lines') %>% 
    add_trace(x = ~Time, y = ~Lower,name = 'Lower confidence bound',type = 'scatter',mode = 'lines') %>%
    layout(title=tit, yaxis = list(title = "Time Series Data"))
  return(p)
}


###################### Dataset #########################
dataset_no<-ifelse(is.na(args[6]),1,as.numeric(args[6]))    #datset to be used
testno<-ifelse(is.na(args[7]),20,as.numeric(args[7]))    #number of data points to be used for test


if (dataset_no==1) {
  data=austres
  data_char<-"austres"
}
if (dataset_no==2) {
  data=BJsales
  data_char<-"BJsales"
}

data_train<-head(data,(length(data)-testno))
data_test<-tail(data,testno)

func_out<-forecasting_plot_func(data,testno,data_train,data_test)

RMSE_Vals<-func_out[[2]]
print(RMSE_Vals)

mlflow$log_param("Dataset", data_char)
mlflow$log_param("Test points", testno)

mlflow$log_metric("rmse_ARIMA", RMSE_Vals$ARIMA)
mlflow$log_metric("rmse_GPSQE", RMSE_Vals$GPSQE)
mlflow$log_metric("rmse_GPV", RMSE_Vals$GPV)
mlflow$log_metric("rmse_Prophet", RMSE_Vals$Pr)

saveWidget(as_widget(func_out[[1]][1][[1]]), "AutoARIMA_plot.html")
saveWidget(as_widget(func_out[[1]][2][[1]]), "GaussianProcessRegression_SE_plot.html")
saveWidget(as_widget(func_out[[1]][3][[1]]), "GaussianProcessRegression_Vanilladot_plot.html")
saveWidget(as_widget(func_out[[1]][4][[1]]), "Prophet_plot.html")
mlflow$log_artifact("AutoARIMA_plot.html")
mlflow$log_artifact("GaussianProcessRegression_SE_plot.html")
mlflow$log_artifact("GaussianProcessRegression_Vanilladot_plot.html")
mlflow$log_artifact("Prophet_plot.html")



#It can be seen that from the plots that for both the data sets, ARIMA model and the 
#Prophet library are giving the best agreement with the original data. 
#Gaussian process regression with vanilladot function shows a good agreement with
#the original data. However, Guassian process regression with squared exponential 
#kernel is the worst performer. However, this is very good model for nonlinear regression
#(as opposed to forecasting). RMSE values reflect a similar pattern. 



