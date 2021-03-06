require(jsonlite)
require(httr)
require(data.table)
require(ellipsis)
require(forecast)
require(stats)
require(xts)
require(zoo)
require(ggplot2)
# DONT CHANGE FUNCTIONS
get_token <- function(username, password, url_site){
  11
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}
get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}
check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}
send_one_forecast <- function(product_id, forecast ,token, url_site, submit_now=F){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/submission/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  
  output = content(result)
  data = data.table::rbindlist(output)
  data[,date:=timestamp(date,quiet = TRUE)]
  aaa = tail(data[order(date)][,"submission"],1)
  
  print("Ex-submission:")
  print(aaa)
  
  split1 = strsplit(aaa$submission,"}")
  split2 = unlist(strsplit(unlist(split1),"'"))
  
  ps = c()
  nos = c()
  
  for(a in 1:7){
    i = 3
    ps = c(ps,split2[2*a])
    tmp = split2[2*a+1]
    l = nchar(tmp)
    tx = as.double(substr(tmp,i,l-2))
    nos = c(nos,tx)
  }
  
  i = 3
  ps = c(ps,split2[16])
  tmp = split2[17]
  l = nchar(tmp)
  tx = as.double(substr(tmp,i,l))
  nos = c(nos,tx)
  
  predictions = data.table("product_content_id" = ps,"forecast" = nos)
  
  print("Ex-Predictions:")
  print(predictions)
  
  idx = (predictions$product_content_id == product_id)
  predictions[idx,]$forecast = forecast
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print("")
  print("")
  print("New submission:")
  print(submission)
  print("New Predictions:")
  print(predictions)
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("")
    print("SUCCESSFULLY SUBMİTTED. Below you can see the details of your submission")
    print("")
    print(content(result))
  } else {
    print("Could NOT submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print("")
  print("")
  print(content(result))
  
}

# DONT CHANGE 
subm_url = 'http://167.172.183.67'
username = "Group3"
password = "cy6DmxzXZEsqDcnF"
#token = get_token(username=username, password=password, url=subm_url)
token = "9cd307d666be12b6aa909786ad3c002815f1f8ce"
data = get_data(token=token,url=subm_url)
mydata = data[data$product_content_id == "31515569" ,]
mydata2=data[data$product_content_id == "6676673" ,]
mydata3= data[data$product_content_id == "4066298" ,]
mydata4=data[data$product_content_id == "7061886" ,]
mydata5=data[data$product_content_id == "32939029" ,]
kulakl�k<-mydata2[54:427]
tayt<-mydata[146:427]
pamukluhavlu<-mydata3[133:427]
es<-mydata4[89:427]
di�f<-mydata5[208:427]
product.data= data[data$product_content_id == "85004" ,]
tayt$meanthree<-0
for(i in 0:279){
  for(j in 0:2){
    tayt$meanthree[3+i]<- tayt$meanthree[3+i]+tayt$sold_count[1+i+j]
  }
  tayt$meanthree[3+i]<- tayt$meanthree[3+i]/3
}
tayt$meanthreetwo[5:282]<-tayt$meanthree[3:280]
tayt$meanweek<-0
for(i in 0:275){
  for(j in 0:6){
    tayt$meanweek[7+i]<- tayt$meanweek[7+i]+tayt$sold_count[1+i+j]
  }
  tayt$meanweek[7+i]<- tayt$meanweek[7+i]/7
}
tayt$meanweektwo[9:282]<-tayt$meanweek[7:280]

tayt$one[2:282]<-tayt$sold_count[1:281]
tayt$two[3:282]<-tayt$sold_count[1:280]
tayt$three[4:282]<-tayt$sold_count[1:279]
tayt$four[5:282]<-tayt$sold_count[1:278]
tayt$five[6:282]<-tayt$sold_count[1:277]
tayt$six[7:282]<-tayt$sold_count[1:276]
tayt$seven[8:282]<-tayt$sold_count[1:275]

tayt$visit[3:282]<-tayt$visit_count[1:280]
tayt$favored[3:282]<-tayt$favored_count[1:280]
tayt$basket[3:282]<-tayt$basket_count[1:280]
tayt$category[3:282]<-tayt$category_sold[1:280]
tayt$category_brand[3:282]<-tayt$category_brand_sold[1:280]
tayt$category_visit2[3:282]<-tayt$category_visits[1:280]
tayt$ty[3:282]<-tayt$ty_visits[1:280]
tayt$pricetwo[3:282]<-tayt$price[1:280]
tayt$priceone [2:282]<-tayt$price[1:281]
tayt[,event_date:=as.POSIXct(tayt$event_date,format='%Y-%m-%d' ) ]
tayt$date<- weekdays(as.Date(tayt$event_date))
test_data=tail(tayt,50)
data=tayt[1:(nrow(tayt)-50)]
acf(tayt$sold_count)
lmodel=lm(formula=sold_count ~ two,data=tayt[126:232])
summary(lmodel)
lmodel=lm(formula=sold_count ~ two+visit+favored+basket+category+category_brand+category_visit2+ty,data=tayt[126:232])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+meanweektwo,data)
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo,data=tayt[126:232])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo+priceone+pricetwo,data=tayt[126:232])
summary(lmodel)
checkresiduals(lmodel)
test_data[,predicted:=predict(lmodel,test_data)] 
for(i in 1:50){
  data_ts=ts(tayt $sold_count[1:(i+232-2)],frequency = 1)
  arima_model=auto.arima(data_ts)
  predictions=forecast(arima_model,h=2)
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_arima[i]<-predicted_arima[2]
}
for(i in 1:50){
  xreg = basket=tayt$basket[1:(i+232-2)]
  data_ts=ts(tayt $sold_count[1:(i+232-2)],frequency = 1)
  arima_model=auto.arima(data_ts,xreg=xreg)
  predictions=forecast(arima_model,h=2,xreg= tayt$basket[(i+232-1):(i+232)])
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_lr_arima[i]<-predicted_arima[2]
}
ts.plot(test_data$sold_count)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted_lr_arima,col="green")
lines(test_data$predicted,col="blue")
pred_datatable=test_data[,.(sold_count,predicted,predicted_arima,predicted_lr_arima)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
test_data2 <- test_data[-c(7), ]
pred_datatable=test_data2[,.(sold_count,predicted,predicted_arima,predicted_lr_arima)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result

kulakl�k$meanthree<-0
for(i in 0:371){
  for(j in 0:2){
    kulakl�k$meanthree[3+i]<- kulakl�k$meanthree[3+i]+kulakl�k$sold_count[1+i+j]
  }
  kulakl�k$meanthree[3+i]<- kulakl�k$meanthree[3+i]/3
}
kulakl�k$meanthreetwo[5:374]<-kulakl�k$meanthree[3:372]
kulakl�k$meanweek<-0
for(i in 0:367){
  for(j in 0:6){
    kulakl�k$meanweek[7+i]<- kulakl�k$meanweek[7+i]+ kulakl�k$sold_count[1+i+j]
  }
  kulakl�k$meanweek[7+i]<- kulakl�k$meanweek[7+i]/7
}
kulakl�k$meanweektwo<-0
kulakl�k$meanweektwo[9:374]<- kulakl�k$meanweek[7:372]

kulakl�k$one[2:374]<- kulakl�k$sold_count[1:373]
kulakl�k$two[3:374]<- kulakl�k$sold_count[1:372]
kulakl�k$three[4:374]<- kulakl�k$sold_count[1:371]
kulakl�k$four[5:374]<- kulakl�k$sold_count[1:370]
kulakl�k$five[6:374]<- kulakl�k$sold_count[1:369]
kulakl�k$six[7:374]<- kulakl�k$sold_count[1:368]
kulakl�k$seven[8:374]<- kulakl�k$sold_count[1:367]

kulakl�k$visit[3:374]<- kulakl�k$visit_count[1:372]
kulakl�k$favored[3:374]<- kulakl�k$favored_count[1:372]
kulakl�k$basket[3:374]<- kulakl�k$basket_count[1:372]
kulakl�k$category[3:374]<- kulakl�k$category_sold[1:372]
kulakl�k$category_brand[3:374]<- kulakl�k$category_brand_sold[1:372]
kulakl�k$category_visit2[3:374]<- kulakl�k$category_visits[1:372]
kulakl�k$ty[3:374]<- kulakl�k$ty_visits[1:372]
kulakl�k$pricetwo[3:374]<- kulakl�k$price[1:372]
kulakl�k$priceone[2:374]<- kulakl�k$price[1:373]
kulakl�k[,event_date:=as.POSIXct(kulakl�k$event_date,format='%Y-%m-%d' ) ]
kulakl�k$date<- weekdays(as.Date(kulakl�k$event_date))
test_data=tail(kulakl�k,50)
data=kulakl�k[1:(nrow(kulakl�k)-50)]
acf(kulakl�k$sold_count)
lmodel=lm(formula=sold_count ~ two,data=kulakl�k[218:374])
summary(lmodel)
lmodel=lm(formula=sold_count ~ two+visit+favored+basket+category+category_brand+category_visit2+ty,data=kulakl�k [218:374])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+meanweektwo,data)
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo,data=kulakl�k[218:374])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo+priceone+pricetwo,data=kulakl�k[218:374])
summary(lmodel)
checkresiduals(lmodel)
test_data[,predicted:=predict(lmodel,test_data)]
for(i in 1:50){
  data_ts=ts(kulakl�k $sold_count[1:(i+324-2)],frequency = 1)
  arima_model=auto.arima(data_ts)
  predictions=forecast(arima_model,h=2)
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_arima[i]<-predicted_arima[2]
}
for(i in 1:50){
  xreg = basket=kulakl�k$basket[1:(i+324-2)]
  data_ts=ts(kulakl�k $sold_count[1:(i+324-2)],frequency = 1)
  arima_model=auto.arima(data_ts,xreg=xreg)
  predictions=forecast(arima_model,h=2,xreg= kulakl�k$basket[(i+324-1):(i+324)])
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_lr_arima[i]<-predicted_arima[2]
}
ts.plot(test_data$sold_count)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted_lr_arima,col="green")
lines(test_data$predicted,col="blue")
test_data2<-test_data[1:49]
pred_datatable=test_data2[,.(sold_count,predicted,predicted_arima,predicted_lr_arima)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
pamukluhavlu$meanthree<-0
for(i in 0:292){
  for(j in 0:2){
    pamukluhavlu$meanthree[3+i]<-pamukluhavlu$meanthree[3+i]+pamukluhavlu$sold_count[1+i+j]
  }
  pamukluhavlu$meanthree[3+i]<-pamukluhavlu$meanthree[3+i]/3
}
pamukluhavlu$meanthreetwo[5:295]<-pamukluhavlu$meanthree[3:293]
pamukluhavlu$meanweek<-0
for(i in 0:288){
  for(j in 0:6){
    pamukluhavlu$meanweek[7+i]<- pamukluhavlu$meanweek[7+i]+pamukluhavlu$sold_count[1+i+j]
  }
  pamukluhavlu$meanweek[7+i]<- pamukluhavlu$meanweek[7+i]/7
}
pamukluhavlu$meanweektwo[9:295]<- pamukluhavlu$meanweek[7:293]

pamukluhavlu$one[2:295]<- pamukluhavlu$sold_count[1:294]
pamukluhavlu$two[3:295]<- pamukluhavlu$sold_count[1:293]
pamukluhavlu$three[4:295]<- pamukluhavlu$sold_count[1:292]
pamukluhavlu$four[5:295]<- pamukluhavlu$sold_count[1:291]
pamukluhavlu$five[6:295]<- pamukluhavlu$sold_count[1:290]
pamukluhavlu$six[7:295]<- pamukluhavlu$sold_count[1:289]
pamukluhavlu$seven[8:295]<- pamukluhavlu$sold_count[1:288]

pamukluhavlu$visit[3:295]<- pamukluhavlu$visit_count[1:293]
pamukluhavlu$favored[3:295]<- pamukluhavlu$favored_count[1:293]
pamukluhavlu$basket[3:295]<- pamukluhavlu$basket_count[1:293]
pamukluhavlu$category[3:295]<- pamukluhavlu$category_sold[1:293]
pamukluhavlu$category_brand[3:295]<- pamukluhavlu$category_brand_sold[1:293]
pamukluhavlu$category_visit2[3:295]<- pamukluhavlu$category_visits[1:293]
pamukluhavlu$ty[3:295]<- pamukluhavlu$ty_visits[1:293]
pamukluhavlu$pricetwo[3:295]<- pamukluhavlu$price[1:293]
pamukluhavlu$priceone [2:295]<- pamukluhavlu$price[1:294]
pamukluhavlu [,event_date:=as.POSIXct(pamukluhavlu$event_date,format='%Y-%m-%d' ) ]
pamukluhavlu$date<- weekdays(as.Date(pamukluhavlu$event_date))
test_data=tail(pamukluhavlu,50)
data=pamukluhavlu[1:(nrow(pamukluhavlu)-50)]
acf(pamukluhavlu$sold_count) 
lmodel=lm(formula=sold_count ~ two,data= pamukluhavlu [139:245])
summary(lmodel)
lmodel=lm(formula=sold_count ~ two+visit+favored+basket+category+category_brand+category_visit2+ty,data= pamukluhavlu [139:245])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+meanweektwo,data)
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo,data= pamukluhavlu [139:245])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo +pricetwo,data= pamukluhavlu [139:245])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+visit+favored+basket+meanweektwo +pricetwo,data= pamukluhavlu [139:245])
summary(lmodel)
checkresiduals(lmodel)
test_data[,predicted:=predict(lmodel,test_data)]
for(i in 1:50){
  data_ts=ts(pamukluhavlu$sold_count[1:(i+245-2)],frequency =1)
  arima_model=auto.arima(data_ts)
  predictions=forecast(arima_model,h=2)
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_arima[i]<-predicted_arima[2]
}
for(i in 1:50){
  data_ts=ts(pamukluhavlu$sold_count[139:(i+245-2)],frequency = 1)
  arima_model=auto.arima(data_ts,xreg=pamukluhavlu$basket[139:(i+245-2)])
  predictions=forecast(arima_model,h=2,xreg= pamukluhavlu$basket [(i+245-1):(i+245)])
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_lr_arima[i]<-predicted_arima[2]
}
ts.plot(test_data$sold_count)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted_lr_arima,col="green")
lines(test_data$predicted,col="blue")
Ma3<-pamukluhavlu$meanthreetwo[246:295]
pred_datatable=test_data[,.(sold_count,predicted,predicted_arima,predicted_lr_arima,Ma3)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
es$meanthree<-0
for(i in 0:336){
  for(j in 0:2){
    es$meanthree[3+i]<-es$meanthree[3+i]+es$sold_count[1+i+j]
  }
  es$meanthree[3+i]<-es$meanthree[3+i]/3
}
es$meanthreetwo[5:339]<-es$meanthree[3:337]
es$meanweek<-0
for(i in 0:332){
  for(j in 0:6){
    es$meanweek[7+i]<- es$meanweek[7+i]+es$sold_count[1+i+j]
  }
  es$meanweek[7+i]<- es$meanweek[7+i]/7
}
es$meanweektwo[9:339]<- es$meanweek[7:337]

es$one[2:339]<- es$sold_count[1:338]
es$two[3:339]<- es$sold_count[1:337]
es$three[4:339]<- es$sold_count[1:336]
es$four[5:339]<- es$sold_count[1:335]
es$five[6:339]<- es$sold_count[1:334]
es$six[7:339]<- es$sold_count[1:333]
es$seven[8:339]<- es$sold_count[1:332]

es$visit[3:339]<- es$visit_count[1:337]
es$favored[3:339]<- es$favored_count[1:337]
es$basket[3:339]<- es$basket_count[1:337]
es$category[3:339]<- es$category_sold[1:337]
es$category_brand[3:339]<- es$category_brand_sold[1:337]
es$category_visit2[3:339]<- es$category_visits[1:337]
es$ty[3:339]<- es$ty_visits[1:337]
es$pricetwo[3:339]<- es$price[1:337]
es$priceone [2:339]<- es$price[1:338]
es [,event_date:=as.POSIXct(es$event_date,format='%Y-%m-%d' ) ]
es$date<- weekdays(as.Date(es$event_date))
test_data=tail(es,50)
data=es[1:(nrow(es)-50)]
acf(es$sold_count)
lmodel=lm(formula=sold_count ~ two,data= es[183:289])
summary(lmodel)
lmodel=lm(formula=sold_count ~ two+visit+favored+basket+category+category_brand+category_visit2+ty,data= es[183:289])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+meanweektwo,data)
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo,data= es[183:289])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+three+four+five+six+seven +visit+favored+basket+category+category_brand+category_visit2+ty+meanweektwo +pricetwo,data= es[183:289])
summary(lmodel)
lmodel=lm(formula=sold_count ~ as.factor(date)+two+visit+favored+basket+meanweektwo +pricetwo,data= es[183:289])
summary(lmodel)
checkresiduals(lmodel)
test_data[,predicted:=predict(lmodel,test_data)]
for(i in 1:50){
  data_ts=ts(es$sold_count[1:(i+289-2)],frequency =1)
  arima_model=auto.arima(data_ts)
  predictions=forecast(arima_model,h=2)
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_arima[i]<-predicted_arima[2]
}
for(i in 1:50){
  data_ts=ts(es$sold_count[183:(i+289-2)],frequency = 1)
  arima_model=auto.arima(data_ts,xreg=cbind(es$basket[183:(i+289-2)], es$two [183:(i+289-2)]))
  predictions=forecast(arima_model,h=2,xreg=cbind(es$basket [(i+289-1):(i+289)],es$two[(i+289-1):(i+289)]))
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_lr_arima[i]<-predicted_arima[2]
}
```{r }
ts.plot(test_data$sold_count)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted_lr_arima,col="green")
lines(test_data$predicted,col="blue")
Ma3<-es$meanthreetwo[290:339]
pred_datatable=test_data[,.(sold_count,predicted,predicted_arima,predicted_lr_arima,Ma3)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
di�f$meanthree<-0
for(i in 0:217){
  for(j in 0:2){
    di�f$meanthree[3+i]<-di�f$meanthree[3+i]+di�f$sold_count[1+i+j]
  }
  di�f$meanthree[3+i]<-di�f$meanthree[3+i]/3
}
di�f$meanthreetwo[5:220]<-di�f$meanthree[3:218]
di�f$meanweek<-0
for(i in 0:213){
  for(j in 0:6){
    di�f$meanweek[7+i]<- di�f$meanweek[7+i]+di�f$sold_count[1+i+j]
  }
  di�f$meanweek[7+i]<- di�f$meanweek[7+i]/7
}
di�f$meanweektwo[9:220]<- di�f$meanweek[7:218]

di�f$one[2:220]<- di�f$sold_count[1:219]
di�f$two[3:220]<- di�f$sold_count[1:218]
di�f$three[4:220]<- di�f$sold_count[1:217]
di�f$four[5:220]<- di�f$sold_count[1:216]
di�f$five[6:220]<- di�f$sold_count[1:215]
di�f$six[7:220]<- di�f$sold_count[1:214]
di�f$seven[8:220]<- di�f$sold_count[1:213]

di�f$visit[3:220]<- di�f$visit_count[1:218]
di�f$favored[3:220]<- di�f$favored_count[1:218]
di�f$basket[3:220]<- di�f$basket_count[1:218]
di�f$category[3:220]<- di�f$category_sold[1:218]
di�f$category_brand[3:220]<- di�f$category_brand_sold[1:218]
di�f$category_visit2[3:220]<- di�f$category_visits[1:218]
di�f$ty[3:220]<- di�f$ty_visits[1:218]
di�f$pricetwo[3:220]<- di�f$price[1:218]
di�f$priceone [2:220]<- di�f$price[1:219]
di�f[,event_date:=as.POSIXct(di�f$event_date,format='%Y-%m-%d' ) ]
di�f$date<- weekdays(as.Date(di�f$event_date))
test_data=tail(di�f,50)
data=di�f[1:(nrow(di�f)-50)]
acf(di�f$sold_count)
lmodel=lm(formula=sold_count~as.factor(date)+two+three+four+five+six+seven+visit+favored+basket+meanweektwo,data= data[64:220])
summary(lmodel)
checkresiduals(lmodel)
test_data[,predicted:=predict(lmodel,test_data)]
for(i in 1:50){
  data_ts=ts(di�f$sold_count[1:(i+170-2)],frequency =1)
  arima_model=auto.arima(data_ts)
  predictions=forecast(arima_model,h=2)
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_arima[i]<-predicted_arima[2]
}
for(i in 1:50){
  data_ts=ts(di�f$sold_count[64:(i+170-2)],frequency = 1)
  arima_model=auto.arima(data_ts,xreg=cbind(di�f$basket[64:(i+170-2)], di�f$two [64:(i+170-2)]))
  predictions=forecast(arima_model,h=2,xreg=cbind(di�f$basket [(i+170-1):(i+170)],di�f$two[(i+170-1):(i+170)]))
  predicted_arima<-as.vector(predictions$mean)
  test_data$predicted_lr_arima[i]<-predicted_arima[2]
}
ts.plot(test_data$sold_count)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted_lr_arima,col="green")
lines(test_data$predicted,col="blue")
Ma3<-di�f$meanthreetwo[171:220]
pred_datatable=test_data[,.(sold_count,predicted,predicted_arima,predicted_lr_arima,Ma3)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
test_data2 <- test_data[-c(43,44,45,46,50), ]
Ma3<-test_data2$meanthreetwo
pred_datatable=test_data2[,.(sold_count,predicted,predicted_arima,predicted_lr_arima,Ma3)]
melted=melt(pred_datatable,id.vars=c(1))
summary_result=melted[,list(se=(value-sold_count)^2,
                            ad=abs(value-sold_count),
                            ape=abs(value-sold_count)/sold_count,sold_count),by=list(variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result
plot(product.data$event_date,product.data$sold_count,type="l", xlab="event_date", ylab="sold_count")
product.data[,lag2_forecast:=shift(sold_count,2)]
product.data[,lag7_forecast:=shift(sold_count,7)]
product.data[,lag1_forecast:=shift(sold_count,1)]
lr_model = lm(sold_count~lag2_forecast+lag7_forecast+lag1_forecast, product.data)
product.data[,lr_forecast:=predict(lr_model,product.data)]
product.data[,list(sold_count,lr_forecast)]
plot(product.data$event_date,product.data$sold_count,type="l",col="red",xlab="event_date",ylab="sold_count",main="Comparison of Sold Count and LM Model")
lines(product.data$event_date,product.data$lr_forecast,type="l",col="blue")

may = product.data[which(product.data$event_date >= "2020-04-27"),]
may_ts=ts(may[,-2],start=c(2020,4),frequency=365)
may_ts=may_ts[,3]
data_ts=ts(may[,c("sold_count")])
arima_model= auto.arima(data_ts)
summary(arima_model)
model_fitted = data_ts - residuals(arima_model)
ts.plot(data_ts, xlab = "Time", ylab = "Daily Sales",main="Arima Model1")
points(model_fitted, type = "l", col = 2, lty = 2)
forecast = forecast (arima_model, h=5)


fc = ses(may_ts, h=5)
summary(fc)
autoplot(fc)+autolayer(fitted(fc))












          




