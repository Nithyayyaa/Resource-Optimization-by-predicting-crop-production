data<-project_database1
ggplot(data=data)+geom_point(mapping = aes(x=Area,y=Size,color=Size))
View(data)
colnames(data)[2]<-"Area"
View(data)
colnames(data)[1]<-"Farm_name"
ggplot(data=data)+geom_point(mapping = aes(x=Farm_name,y=Area,color=Farm_name))
data1<-data %>% select(`Year`:`Fixed Cost`)
View(data1)
glimpse(data2)
ggplot(data2)+geom_line(mapping=aes(x=Year,y=production))+geom
data_1
data2_1<-data2[1:7,1:2]
##=====================================================
train <- data1[1:4,1:6]
View (train)
test <-  data1[5:7,1:6]
View(test)
nrow(train); nrow(test)
dat_ts <- ts(train[,6], start = c(2013, 1), end = c(2016, 1), frequency = 1)
##==================================================
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
##=====NAIVE==========================================
naive_mod <- naive(dat_ts, h = 1)
summary(naive_mod)
test$naive = 1457
View(test)
mape(test$production,test$naive) 
##====================================================
data<-project_database1
plot(x = data$`Size(hectares)`,y = data$`Avg production`,
     xlab = "Area",
     ylab = "Average Production",
     main = "Area vs Average Production"
)
View(data_ts)
##========HOLTMODEL========================
holt_model <- holt(dat_ts, h = 3)
summary(holt_model)

View(data2)

df_holt = as.data.frame(holt_model)
test$holt = df_holt$`Point Forecast`
mape(dat_test$production,test$holt) 

#============writin algo=====================
method1<-function(x)
{
  data<-read_excel("project_database1.xlsx", sheet=x)
  View(data)
}

method2<-function(data,coln,cn)
{
  data1<-data %>% select(`Year`,coln)
  View(data1)
  train <- data1[1:4,1:2]
  View (train)
  test <-  data1[5:7,1:2]
  View(test)
  nrow(train); nrow(test)
  dat_ts <- ts(train[,2], start = c(2013, 1), end = c(2016, 1), frequency = 1)
  
  mape <- function(actual,pred)
  {
    mape <- mean(abs((actual - pred)/actual))*100
    return (mape)
  }
  
  holt_model <- holt(dat_ts, h = 3)
  summary(holt_model)
  
  df_holt = as.data.frame(holt_model)
  test$holt = df_holt$`Point Forecast`
  mape(test$cn,test$holt) 
  
}

method1("Vasant Farm")
method2(data,'production',production)




View(data1)
d1<-data %>% select(production,Yearp)
View(d1)



