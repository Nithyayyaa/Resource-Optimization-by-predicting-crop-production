holt_model<-1
##==import excel============================
method1<-function(x)
{
  data<<-read_excel("project_database1.xlsx", sheet=x)
  View(data)
}
#===========================================
##=====Production===========================
methodprod<-function(data)
{
  data1<<-data %>% select(Year,production)
  View(data1)
  train <<- data1[1:7,1:2]
  View (train)
  test <<-  data1[5:7,1:2]
  View(test)
  nrow(train); nrow(test)
  dat_ts <<- ts(train[,2], start = c(2013, 1), end = c(2019, 1), frequency = 1) 
  holt_model <<- holt(dat_ts,h = 3)
  summary(holt_model)
}
inside1<-function()
{
  mape <<- function(actual,pred)
  {
    mape <<- mean(abs((actual - pred)/actual))*100
    return (mape)
  }
  
  df_holt <<- as.data.frame(holt_model)
  test$holt <<- df_holt$`Point Forecast`
  mape(test$production,test$holt) 
}
##==========LabourCost===========================

methodlab<-function(data)
{
  data1<<-data %>% select(Year,`Labour cost`)
  View(data1)
  train <<- data1[1:7,1:2]
  test <<-  data1[5:7,1:2]
  nrow(train); nrow(test)
  dat_ts <<- ts(train[,2], start = c(2013, 1), end = c(2019, 1), frequency = 1)
  
  holt_model <<- holt(dat_ts,h = 3)
  summary(holt_model)
}
inside2<-function()
{
  colnames(data1)<-"labour_cost"
  mape <<- function(actual,pred)
  {
    mape <<- mean(abs((actual - pred)/actual))*100
    return (mape)
  }
  
  df_holt <<- as.data.frame(holt_model)
  test$holt <<- df_holt$`Point Forecast`
  mape(test$labour_cost,test$holt) 
}
#=============================================
##=============SeedCost===============
methodseed<-function(data)
{
  data1<-data %>% select(Year,`Seed Cost`)
  View(data1)
  train <- data1[1:7,1:2]
  test <-  data1[5:7,1:2]
  nrow(train); nrow(test)
  dat_ts <- ts(train[,2], start = c(2013, 1), end = c(2019, 1), frequency = 1)
  
  holt_model <- holt(dat_ts, h = 3)
  summary(holt_model)
}
#==============Fertiliser Cost=========
methodfert<-function(data)
{
  data1<-data %>% select(Year,`Fertiliser Cost`)
  View(data1)
  train <- data1[1:7,1:2]
  test <-  data1[5:7,1:2]
  nrow(train); nrow(test)
  dat_ts <- ts(train[,2], start = c(2013, 1), end = c(2019, 1), frequency = 1)
  
  holt_model <- holt(dat_ts, h = 3)
  summary(holt_model)
}
##==========FixedCost========================
methodfixed<-function(data)
{
  data1<-data %>% select(Year,`Fixed Cost`)
  View(data1)
  train <- data1[1:7,1:2]
  test <-  data1[5:7,1:2]
  nrow(train); nrow(test)
  dat_ts <- ts(train[,2], start = c(2013, 1), end = c(2019, 1), frequency = 1)
  
  holt_model <- holt(dat_ts, h = 3)
  summary(holt_model)
}

inside<-function()
{
  mape <<- function(actual,pred)
  {
    mape <<- mean(abs((actual - pred)/actual))*100
    return (mape)
  }
  
  df_holt <<- as.data.frame(holt_model)
  test$holt <<- df_holt$`Point Forecast`
  mape(test$production,test$holt) 
}

method1("KKM Farm")
methodprod(data)
inside1()
methodlab(data)
inside2()
methodseed(data)
inside3()
methodfert(data)
inside4()
methodfixed(data)
inside5()


