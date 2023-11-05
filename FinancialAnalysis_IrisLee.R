# Financial Data Analysis -- Final Report
# MIS Junior 108306078 Iris Lee

rm(list = ls(all = TRUE))

# Import function
source("/Users/irislee/Financial Analysis/final/function_FDA.R")

# Import data: 8 stocks
data_1301 = read.table("/Users/irislee/Financial Analysis/final/1301.TW.csv", sep= ",", na.strings = "null", header = T)
data_2317 = read.table("/Users/irislee/Financial Analysis/final/2317.TW.csv", sep= ",", na.strings = "null", header = T)
data_2330 = read.table("/Users/irislee/Financial Analysis/final/2330.TW.csv", sep= ",", na.strings = "null", header = T)
data_2412 = read.table("/Users/irislee/Financial Analysis/final/2412.TW.csv", sep= ",", na.strings = "null", header = T)
data_2610 = read.table("/Users/irislee/Financial Analysis/final/2610.TW.csv", sep= ",", na.strings = "null", header = T)
data_2881 = read.table("/Users/irislee/Financial Analysis/final/2881.TW.csv", sep= ",", na.strings = "null", header = T)
data_2912 = read.table("/Users/irislee/Financial Analysis/final/2912.TW.csv", sep= ",", na.strings = "null", header = T)
data_6446 = read.table("/Users/irislee/Financial Analysis/final/6446.TWO.csv", sep= ",", na.strings = "null", header = T)

# Transform Date to type "Date"
data_1301$Date<-paste(substr(data_1301$Date, 1, 4),"-",
                       substr(data_1301$Date, 6, 7),"-",
                       substr(data_1301$Date, 9, 10),
                       sep="")
data_1301$Date<-as.Date(data_1301$Date)
class(data_1301$Date)

data_2317$Date<-paste(substr(data_2317$Date, 1, 4),"-",
                      substr(data_2317$Date, 6, 7),"-",
                      substr(data_2317$Date, 9, 10),
                      sep="")
data_2317$Date<-as.Date(data_2317$Date)
class(data_2317$Date)

data_2330$Date<-paste(substr(data_2330$Date, 1, 4),"-",
                      substr(data_2330$Date, 6, 7),"-",
                      substr(data_2330$Date, 9, 10),
                      sep="")
data_2330$Date<-as.Date(data_2330$Date)
class(data_2330$Date)

data_2412$Date<-paste(substr(data_2412$Date, 1, 4),"-",
                      substr(data_2412$Date, 6, 7),"-",
                      substr(data_2412$Date, 9, 10),
                      sep="")
data_2412$Date<-as.Date(data_2412$Date)
class(data_2412$Date)

data_2610$Date<-paste(substr(data_2610$Date, 1, 4),"-",
                      substr(data_2610$Date, 6, 7),"-",
                      substr(data_2610$Date, 9, 10),
                      sep="")
data_2610$Date<-as.Date(data_2610$Date)
class(data_2610$Date)

data_2881$Date<-paste(substr(data_2881$Date, 1, 4),"-",
                      substr(data_2881$Date, 6, 7),"-",
                      substr(data_2881$Date, 9, 10),
                      sep="")
data_2881$Date<-as.Date(data_2881$Date)
class(data_2881$Date)

data_2912$Date<-paste(substr(data_2912$Date, 1, 4),"-",
                      substr(data_2912$Date, 6, 7),"-",
                      substr(data_2912$Date, 9, 10),
                      sep="")
data_2912$Date<-as.Date(data_2912$Date)
class(data_2912$Date)

data_6446$Date<-paste(substr(data_6446$Date, 1, 4),"-",
                      substr(data_6446$Date, 6, 7),"-",
                      substr(data_6446$Date, 9, 10),
                      sep="")
data_6446$Date<-as.Date(data_6446$Date)
class(data_6446$Date)


# Replace missing value with function NA_rep
NA_rep<-function(x, colx){   ## x: a data frame, colx: which column of the data frame
  
  ind_miss<-which(is.na(x[, colx])) ## where the missing data is  
  
  for(i in ind_miss){ ## use a "for" loop to replace the missing value with 
    ## its previous one
    x[i, colx]<-x[i-1, colx]
    
  }
  
  return(x)
  
}

# Replace all of the missing Adj.Close Price using function NA_rep 
data_1301<-NA_rep(data_1301, 6)  
data_2317<-NA_rep(data_2317, 6)   
data_2330<-NA_rep(data_2330, 6) 
data_2412<-NA_rep(data_2412, 6) 
data_2610<-NA_rep(data_2610, 6)
data_2881<-NA_rep(data_2881, 6)
data_2912<-NA_rep(data_2912, 6)
data_6446<-NA_rep(data_6446, 6)

# Check whether there is still any missing value in the data
sum(is.na(data_1301[, 6]))
sum(is.na(data_2317[, 6]))
sum(is.na(data_2330[, 6]))
sum(is.na(data_2412[, 6]))
sum(is.na(data_2610[, 6]))
sum(is.na(data_2881[, 6]))
sum(is.na(data_2912[, 6]))
sum(is.na(data_6446[, 6]))

# Individual Asset
## 8 Assets: 
### Formosa Plastics(1301),Hon Hai(2317),TSMC(2330),Chunghwa Telecom(2412),
### China Airlines(2610),Fubon Financial Holding(2881),President Chain Store(2912),PharmaEssentia(6446)

## Plot time series plots of assets' Adj.Price
par(mfrow = c(1,1))
rangex<-range(c(data_1301[, 6],data_2317[,6],data_2330[, 6],data_2412[, 6],data_2610[, 6],data_2881[, 6],data_2912[, 6], data_6446[, 6]))
plot(x = data_1301$Date, y = data_1301$Adj.Close, ylim= rangex,
     type = "l", xlab = "Date", ylab = "Adj.Close Price", 
     col = "coral1",lwd =2, main = "Time Series Plots of Assets' Prices")
lines(x = data_1301$Date, y = data_2317$Adj.Close,col = "lightblue2",lwd =2)
lines(x = data_1301$Date, y = data_2330$Adj.Close,col = "lightsalmon",lwd =2)
lines(x = data_1301$Date, y = data_2412$Adj.Close,col = "burlywood3",lwd =2)
lines(x = data_1301$Date, y = data_2610$Adj.Close,col = "lightpink1",lwd =2)
lines(x = data_1301$Date, y = data_2881$Adj.Close,col = "yellowgreen",lwd =2)
lines(x = data_1301$Date, y = data_2912$Adj.Close,col = "khaki2",lwd =2)
lines(x = data_1301$Date, y = data_6446$Adj.Close,col = "thistle",lwd =2)
legend("topleft", 
       legend = c("Formosa Plastics","Hon Hai","TSMC","Chunghwa Telecom", "China Airlines", "Fubon Financial Holding","President Chain Store","PharmaEssentia"), 
       lwd =c(2,2,2,2,2,2,2,2),
       col = c("coral1","lightblue2","lightsalmon","burlywood3", "lightpink1", "yellowgreen","khaki2","thistle"), cex= 0.8)

## Plot time series plots of daily returns
### Adjust Volume's and Adj.Close's position
data_1301<-cbind(data_1301[,1:5], data_1301[,7], data_1301[,6]);colnames(data_1301)[6:7]<-c("Volume","Adj.Close")
data_2317<-cbind(data_2317[,1:5], data_2317[,7], data_2317[,6]);colnames(data_2317)[6:7]<-c("Volume","Adj.Close")
data_2330<-cbind(data_2330[,1:5], data_2330[,7], data_2330[,6]);colnames(data_2330)[6:7]<-c("Volume","Adj.Close")
data_2412<-cbind(data_2412[,1:5], data_2412[,7], data_2412[,6]);colnames(data_2412)[6:7]<-c("Volume","Adj.Close")
data_2610<-cbind(data_2610[,1:5], data_2610[,7], data_2610[,6]);colnames(data_2610)[6:7]<-c("Volume","Adj.Close")
data_2881<-cbind(data_2881[,1:5], data_2881[,7], data_2881[,6]);colnames(data_2881)[6:7]<-c("Volume","Adj.Close")
data_2912<-cbind(data_2912[,1:5], data_2912[,7], data_2912[,6]);colnames(data_2912)[6:7]<-c("Volume","Adj.Close")
data_6446<-cbind(data_6446[,1:5], data_6446[,7], data_6446[,6]);colnames(data_6446)[6:7]<-c("Volume","Adj.Close")

### Calculate each stock's daily returns, turn them into percentage, and round them to third decimal place
data_1301$ret = c(NA, round(retx(data_1301$Adj.Close)*100,3))
data_2317$ret = c(NA, round(retx(data_2317$Adj.Close)*100,3))
data_2330$ret = c(NA, round(retx(data_2330$Adj.Close)*100,3))
data_2412$ret = c(NA, round(retx(data_2412$Adj.Close)*100,3))
data_2610$ret = c(NA, round(retx(data_2610$Adj.Close)*100,3))
data_2881$ret = c(NA, round(retx(data_2881$Adj.Close)*100,3)) 
data_2912$ret = c(NA, round(retx(data_2912$Adj.Close)*100,3)) 
data_6446$ret = c(NA, round(retx(data_6446$Adj.Close)*100,3)) 

head(data_1301);tail(data_1301)
head(data_2317);tail(data_2317)
head(data_2330);tail(data_2330)
head(data_2412);tail(data_2412)
head(data_2610);tail(data_2610)
head(data_2881);tail(data_2881)
head(data_2912);tail(data_2912)
head(data_6446);tail(data_6446)


### Store each stock's return to data_return
data_return<-data.frame(matrix(0, nrow(data_1301), 9))
data_return[,1]<-data_1301$Date
data_return[,2:ncol(data_return)]<-cbind(data_1301$ret, data_2317$ret, 
                             data_2330$ret, data_2412$ret, data_2610$ret,data_2881$ret,data_2912$ret,data_6446$ret)  

colnames(data_return)<-c("Date","Formosa Plastics","Hon Hai","TSMC","Chunghwa Telecom","China Airlines","Fubon Financial Holding","President Chain Store","PharmaEssentia")
head(data_return)

#### Remove first row of the return data frame
data_return = data_return[-1,]

### Plots each stock's time series plot of returns
rangex = range(data_return[2:nrow(data_return),2:9])
par(mfrow = c(1,1))
plot(x = data_return$Date, y = data_return$`Formosa Plastics`, type="l", main = "Times series plot of returns(Formosa Plastics,1301)",
     col = "coral1",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$`Hon Hai`, type="l", main = "Times series plot of returns(Hon Hai,2317)",
     col = "lightblue2",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$TSMC, type="l", main = "Times series plot of returns(TSMC,2330)",
     col = "lightsalmon",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$`Chunghwa Telecom`, type="l", main = "Times series plot of returns(Chunghwa Telecom,2412)",
     col = "burlywood3",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$`China Airlines`, type="l", main = "Times series plot of returns(China Airlines,2610)",
     col = "lightpink1",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$`Fubon Financial Holding`, type="l", main = "Times series plot of returns(Fubon Financial Holding,2881)",
     col = "yellowgreen",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$`President Chain Store`, type="l", main = "Times series plot of returns(President Chain Store,2912)",
     col = "khaki2",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

plot(x = data_return$Date, y = data_return$PharmaEssentia, type="l", main = "Times series plot of returns(PharmaEssentia,6446)",
     col = "thistle",xlab = "Date", ylab = "Return (%)",lwd=2)
abline(h=0,lty=2)

## Descriptive statistics of returns (annualized and percentage)
sum(is.na(data_return))
summary_return<-rbind(nrow(data_1301),
                    apply(data_return[,2:ncol(data_return)], 2, summary)*252,
                    apply(data_return[,2:ncol(data_return)], 2, sd)*sqrt(252),
                    apply(data_return[,2:ncol(data_return)], 2, my_skewness)/sqrt(252),
                    apply(data_return[,2:ncol(data_return)], 2, my_kurtosis)/252,
                    apply(data_return[,2:ncol(data_return)], 2, my_acf1)
)
rownames(summary_return)[1] <-"No. of Obs."

rownames(summary_return)[8:nrow(summary_return)]<-c("Std.","SKew.","Kurt.","ACF1")
summary_return<-round(summary_return,3)
summary_return <- t(summary_return) 
summary_return <- summary_return[,-3];summary_return <- summary_return[,-5]
summary_return

## Correlation matrix of the returns
corx<-cor(data_return[,2:ncol(data_return)])
round(corx,3)

#------------------------------------------------------------------------------------------------

# Portfolios
## 7 strategies:
### mvp, nsgmvp, fixed, gmvp, rfmvp, nsmvp, nsrfmvp

## Calculate expected return
consx<-ABC_mvp(data_return[2:ncol(data_return)])
gmvp_mr<-consx$B/consx$C # 全局最小變異數投資組合的期望報酬率
gmvp_mr # 0.036% 不用再*100因為本來資料報酬率就是百分比

## Change the return data into decimal 統一以小數點顯示，not百分比
data_return[, 2:ncol(data_return)]<-data_return[, 2:ncol(data_return)]/100 ## scaled by 1/100, not in percentage
head(data_return)

## Basic settings
kx<-252*2                ## set rolling window length to 2 years
hx<-nrow(data_return)-kx ## length of out-of-sample period 估計要多少次

## Date Storage 儲存資料：先產生一些空白向量
### portfolio weights, starting from period t-1
wx_mat_mvp<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_nsgmvp<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_fixed<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_gmvp<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_rfmvp<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_nsmvp<-matrix(0, hx+1, ncol(data_return)-1)
wx_mat_nsrfmvp<-matrix(0, hx+1, ncol(data_return)-1)

### portfolio return 樣本外報酬率
por_ret_mvp<-numeric(hx)
por_ret_nsgmvp<-numeric(hx)
por_ret_fixed<-numeric(hx)
por_ret_gmvp<-numeric(hx)
por_ret_rfmvp<-numeric(hx)
por_ret_nsmvp<-numeric(hx)
por_ret_nsrfmvp<-numeric(hx)

### turn over rate
tor_mvp<-numeric(hx)
tor_nsgmvp<-numeric(hx)
tor_fixed<-numeric(hx)
tor_gmvp<-numeric(hx)
tor_rfmvp<-numeric(hx)
tor_nsmvp<-numeric(hx)
tor_nsrfmvp<-numeric(hx)

### HHI
hhi_mvp<-numeric(hx)
hhi_nsgmvp<-numeric(hx)
hhi_fixed<-numeric(hx)
hhi_gmvp<-numeric(hx)
hhi_rfmvp<-numeric(hx)
hhi_nsmvp<-numeric(hx)
hhi_nsrfmvp<-numeric(hx)

### SLR：做空作多比率
slr_mvp<-numeric(hx)
slr_nsgmvp<-numeric(hx)
slr_fixed<-numeric(hx)
slr_gmvp<-numeric(hx)
slr_rfmvp<-numeric(hx)
slr_nsmvp<-numeric(hx)
slr_nsrfmvp<-numeric(hx)


### target return, and annualized risk free return 1%
mu_targ<-0.036/100 ## daily return 0.036%
rf = 0.01/252

## Calculate data for each portfolio, rolling window
for(i in 1:hx){
  
  ### data in the window
  datax<-data_return[i:(i+kx-1), 2:ncol(data_return)]
  
  ### porfolios' weights
  wx_mvp<-as.vector(mvp_wx(datax, mu_targ = mu_targ)) # mvp
  wx_nsgmvp<-nsgmvp_wx_quad(datax)$solution # nsgmvp
  wx_fixed <- 1/8 # fixed weight
  wx_gmvp <- as.vector(gmvp_wx(datax)) 
  wx_rfmvp <- as.vector(rf_mvp_wx(datax,mu_targ = mu_targ,rf = rf)) 
  wx_nsmvp <- nsmvp_wx_quad(datax,mu_targ=mu_targ)$solution
  wx_nsrfmvp <- rf_nsmvp_wx_quad(datax,mu_targ=mu_targ,rf = rf)$solution

  
  ### data used to calculate turnover rate (independent of portfolios)
  rx<-data_return[i+kx,2:ncol(data_return)]  ## return at period i+kx (period t+1)
  rx_lag<-datax[nrow(datax),]                ## return at period i+kx-1 (period t)
  
  ### individual assets' turnover rate
  tor_ind_mvp<-wx_mvp-wx_mat_mvp[i,]*(1+rx_lag)/(1+sum(wx_mat_mvp[i,]*rx_lag))
  tor_ind_nsgmvp<-wx_nsgmvp-wx_mat_nsgmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_nsgmvp[i,]*rx_lag))
  tor_ind_fixed <-wx_fixed -wx_mat_fixed[i,]*(1+rx_lag)/(1+sum(wx_mat_fixed[i,]*rx_lag))
  tor_ind_gmvp<-wx_gmvp -wx_mat_gmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_gmvp[i,]*rx_lag))
  tor_ind_rfmvp<-wx_rfmvp -wx_mat_rfmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_rfmvp[i,]*rx_lag))
  tor_ind_nsmvp<-wx_nsmvp -wx_mat_nsmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_nsmvp[i,]*rx_lag))
  tor_ind_nsrfmvp<-wx_nsrfmvp -wx_mat_nsrfmvp[i,]*(1+rx_lag)/(1+sum(wx_mat_nsrfmvp[i,]*rx_lag))
  
  ### portfolio turn over rate
  tor_mvp[i]<-sum(abs(tor_ind_mvp))
  tor_nsgmvp[i]<-sum(abs(tor_ind_nsgmvp))
  tor_fixed[i]<-sum(abs(tor_ind_fixed))
  tor_gmvp[i]<-sum(abs(tor_ind_gmvp))
  tor_rfmvp[i]<-sum(abs(tor_ind_rfmvp))
  tor_nsmvp[i]<-sum(abs(tor_ind_nsmvp))
  tor_nsrfmvp[i]<-sum(abs(tor_ind_nsrfmvp))
  
  ### portfolio return 樣本外投資組合報酬 -> gross return 無扣除手續費
  por_ret_mvp[i]<-sum(wx_mvp*rx)
  por_ret_nsgmvp[i]<-sum(wx_nsgmvp*rx)
  por_ret_fixed[i]<-sum(wx_fixed*rx)
  por_ret_gmvp[i]<-sum(wx_gmvp*rx)
  por_ret_rfmvp[i]<-sum(wx_rfmvp*rx)
  por_ret_nsmvp[i]<-sum(wx_nsmvp*rx)
  por_ret_nsrfmvp[i]<-sum(wx_nsrfmvp*rx)
  
  ### HHI
  hhi_mvp[i]<-sum(wx_mvp^2)/(sum(abs(wx_mvp))^2)
  hhi_nsgmvp[i]<-sum(wx_nsgmvp^2)/(sum(abs(wx_nsgmvp))^2)
  hhi_fixed[i]<-sum(wx_fixed^2)/(sum(abs(wx_fixed))^2)
  hhi_gmvp[i]<-sum(wx_gmvp^2)/(sum(abs(wx_gmvp))^2)
  hhi_rfmvp[i]<-sum(wx_rfmvp^2)/(sum(abs(wx_rfmvp))^2)
  hhi_nsmvp[i]<-sum(wx_nsmvp^2)/(sum(abs(wx_nsmvp))^2)
  hhi_nsrfmvp[i]<-sum(wx_nsrfmvp^2)/(sum(abs(wx_nsrfmvp))^2)
  
  ### SLR
  slr_mvp[i]<-sum(abs(wx_mvp[wx_mvp<0]))/sum(abs(wx_mvp[wx_mvp>0]))
  slr_nsgmvp[i]<-sum(abs(wx_nsgmvp[wx_nsgmvp<0]))/sum(abs(wx_nsgmvp[wx_nsgmvp>0]))
  slr_fixed[i]<-sum(abs(wx_fixed[wx_fixed<0]))/sum(abs(wx_fixed[wx_fixed>0]))
  slr_gmvp[i]<-sum(abs(wx_gmvp[wx_gmvp<0]))/sum(abs(wx_gmvp[wx_gmvp>0]))
  slr_rfmvp[i]<-sum(abs(wx_rfmvp[wx_rfmvp<0]))/sum(abs(wx_rfmvp[wx_rfmvp>0]))
  slr_nsmvp[i]<-sum(abs(wx_nsmvp[wx_nsmvp<0]))/sum(abs(wx_nsmvp[wx_nsmvp>0]))
  slr_nsrfmvp[i]<-sum(abs(wx_nsrfmvp[wx_nsrfmvp<0]))/sum(abs(wx_nsrfmvp[wx_nsrfmvp>0]))
  
  ### store portfolio weight vector at this period
  wx_mat_mvp[i+1,]<-wx_mvp
  wx_mat_nsgmvp[i+1,]<-wx_nsgmvp
  wx_mat_fixed[i+1,]<-wx_fixed
  wx_mat_gmvp[i+1,]<-wx_gmvp
  wx_mat_rfmvp[i+1,]<-wx_rfmvp
  wx_mat_nsmvp[i+1,]<-wx_nsmvp
  wx_mat_nsrfmvp[i+1,]<-wx_nsrfmvp
  
  print(i) 
}

## Examine average weight of each asset in each portfolio
mean_wx_mat_mvp <- round(apply(wx_mat_mvp,2,mean),3)
mean_wx_mat_nsgmvp <- round(apply(wx_mat_nsgmvp,2,mean),3)
mean_wx_mat_fixed <- round(apply(wx_mat_fixed,2,mean),3)
mean_wx_mat_gmvp <- round(apply(wx_mat_gmvp,2,mean),3)
mean_wx_mat_rfmvp <- round(apply(wx_mat_rfmvp,2,mean),3)
1-sum(mean_wx_mat_rfmvp)
mean_wx_mat_nsmvp <- round(apply(wx_mat_nsmvp,2,mean),3)
mean_wx_mat_nsrfmvp <- round(apply(wx_mat_nsrfmvp,2,mean),3)

## Plot portfolios' returns and cumulative gross returns
### mvp
par(mfrow = c(1,1))
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_mvp*100, type = "l",lwd=1.5, 
     main = "Time series plot of oos portfolio returns (mvp)",
     xlab = "Date", ylab = "Return(%)",col = "tomato1"
)
abline(h=0,lty=2)
cumr_mvp<-cumprod(1+por_ret_mvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_mvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (mvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "tomato1"
)
abline(h=1,lty=2)

### nsgmvp
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_nsgmvp*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (nsgmvp)",
     xlab = "Date", ylab = "Return(%)",col = "steelblue1"
)
abline(h=0,lty=2)
cumr_nsgmvp<-cumprod(1+por_ret_nsgmvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_nsgmvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (nsgmvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "steelblue1"
)
abline(h=1,lty=2)

### fixed weight
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_fixed*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (fixed)",
     xlab = "Date", ylab = "Return(%)",col = "darkorange"
)
abline(h=0,lty=2)
cumr_fixed<-cumprod(1+por_ret_fixed)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_fixed, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (fixed)",
     xlab = "Date", ylab = "Cumulative gross return",col = "darkorange"
)
abline(h=1,lty=2)

### gmvp
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_gmvp*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (gmvp)",
     xlab = "Date", ylab = "Return(%)",col = "dodgerblue3"
)
abline(h=0,lty=2,col="white")
cumr_gmvp<-cumprod(1+por_ret_gmvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_gmvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (gmvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "dodgerblue3"
)
abline(h=1,lty=2)

### rfmvp
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_rfmvp*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (rfmvp)",
     xlab = "Date", ylab = "Return(%)",col = "seagreen2"
)
abline(h=0,lty=2)
cumr_rfmvp<-cumprod(1+por_ret_rfmvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_rfmvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (rfmvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "seagreen2"
)
abline(h=1,lty=2)

### nsmvp
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_nsmvp*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (nsmvp)",
     xlab = "Date", ylab = "Return(%)",col = "mediumpurple3"
)
abline(h=0,lty=2,col="white")
cumr_nsmvp<-cumprod(1+por_ret_nsmvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_nsmvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (nsmvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "mediumpurple3"
)
abline(h=1,lty=2)

### nsrfmvp
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = por_ret_nsrfmvp*100, type = "l", lwd =1.5,
     main = "Time series plot of oos portfolio returns (nsrfmvp)",
     xlab = "Date", ylab = "Return(%)",col = "goldenrod1"
)
abline(h=0,lty=2)
cumr_nsrfmvp<-cumprod(1+por_ret_nsmvp)                                   
plot(x = data_return[(kx+1):nrow(data_return),]$Date,
     y = cumr_nsrfmvp, type = "l", lwd =1.5,
     main = "Time series plot of oos cumulative gross returns of the portfolio (nsrfmvp)",
     xlab = "Date", ylab = "Cumulative gross return",col = "goldenrod1"
)
abline(h=1,lty=2)


## Descriptive statistics of the oos portfolio returns
### mvp
por_ret_mvp <- por_ret_mvp*100 # change to percentage
summary_por_mvp<-c(hx,summary(por_ret_mvp)*252, sd(por_ret_mvp)*sqrt(252),
                   my_skewness(por_ret_mvp)/sqrt(252),
                   my_kurtosis(por_ret_mvp)/252,
                   my_acf1(por_ret_mvp))
names(summary_por_mvp)[1] <- "No. of Obs."
names(summary_por_mvp)[8:length(summary_por_mvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_mvp[2:length(summary_por_mvp)] <- round(summary_por_mvp[2:length(summary_por_mvp)],3)
summary_por_mvp <- summary_por_mvp[-3]
summary_por_mvp <- summary_por_mvp[-5]
summary_por_mvp
#write.table(summary_por_mvp, "/Users/irislee/Financial Analysis/final/summary_por_mvp.csv", sep = ",")

### nsgmvp
por_ret_nsgmvp <- por_ret_nsgmvp*100 # change to percentage
summary_por_nsgmvp<-c(hx,summary(por_ret_nsgmvp)*252, sd(por_ret_nsgmvp)*sqrt(252),
                   my_skewness(por_ret_nsgmvp)/sqrt(252),
                   my_kurtosis(por_ret_nsgmvp)/252,
                   my_acf1(por_ret_nsgmvp))
names(summary_por_nsgmvp)[1] <- "No. of Obs."
names(summary_por_nsgmvp)[8:length(summary_por_nsgmvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_nsgmvp[2:length(summary_por_nsgmvp)] <- round(summary_por_nsgmvp[2:length(summary_por_nsgmvp)],3)
summary_por_nsgmvp <- summary_por_nsgmvp[-3]
summary_por_nsgmvp <- summary_por_nsgmvp[-5]
summary_por_nsgmvp

### fixed weight
por_ret_fixed <- por_ret_fixed*100 # change to percentage
summary_por_fixed<-c(hx,summary(por_ret_fixed)*252, sd(por_ret_fixed)*sqrt(252),
                      my_skewness(por_ret_fixed)/sqrt(252),
                      my_kurtosis(por_ret_fixed)/252,
                      my_acf1(por_ret_fixed))
names(summary_por_fixed)[1] <- "No. of Obs."
names(summary_por_fixed)[8:length(summary_por_fixed)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_fixed[2:length(summary_por_fixed)] <- round(summary_por_fixed[2:length(summary_por_fixed)],3)
summary_por_fixed <- summary_por_fixed[-3]
summary_por_fixed <- summary_por_fixed[-5]
summary_por_fixed

### gmvp
por_ret_gmvp <- por_ret_gmvp*100 # change to percentage
summary_por_gmvp<-c(hx,summary(por_ret_gmvp)*252, sd(por_ret_gmvp)*sqrt(252),
                    my_skewness(por_ret_gmvp)/sqrt(252),
                    my_kurtosis(por_ret_gmvp)/252,
                    my_acf1(por_ret_gmvp))
names(summary_por_gmvp)[1] <- "No. of Obs."
names(summary_por_gmvp)[8:length(summary_por_gmvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_gmvp[2:length(summary_por_gmvp)] <- round(summary_por_gmvp[2:length(summary_por_gmvp)],3)
summary_por_gmvp <- summary_por_gmvp[-3]
summary_por_gmvp <- summary_por_gmvp[-5]
summary_por_gmvp

### rfmvp
por_ret_rfmvp <- por_ret_rfmvp*100 # change to percentage
summary_por_rfmvp<-c(hx,summary(por_ret_rfmvp)*252, sd(por_ret_rfmvp)*sqrt(252),
                    my_skewness(por_ret_rfmvp)/sqrt(252),
                    my_kurtosis(por_ret_rfmvp)/252,
                    my_acf1(por_ret_rfmvp))
names(summary_por_rfmvp)[1] <- "No. of Obs."
names(summary_por_rfmvp)[8:length(summary_por_rfmvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_rfmvp[2:length(summary_por_rfmvp)] <- round(summary_por_rfmvp[2:length(summary_por_rfmvp)],3)
summary_por_rfmvp <- summary_por_rfmvp[-3]
summary_por_rfmvp <- summary_por_rfmvp[-5]
summary_por_rfmvp

### nsmvp
por_ret_nsmvp <- por_ret_nsmvp*100 # change to percentage
summary_por_nsmvp<-c(hx,summary(por_ret_nsmvp)*252, sd(por_ret_nsmvp)*sqrt(252),
                     my_skewness(por_ret_nsmvp)/sqrt(252),
                     my_kurtosis(por_ret_nsmvp)/252,
                     my_acf1(por_ret_nsmvp))
names(summary_por_nsmvp)[1] <- "No. of Obs."
names(summary_por_nsmvp)[8:length(summary_por_nsmvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_nsmvp[2:length(summary_por_nsmvp)] <- round(summary_por_nsmvp[2:length(summary_por_nsmvp)],3)
summary_por_nsmvp <- summary_por_nsmvp[-3]
summary_por_nsmvp <- summary_por_nsmvp[-5]
summary_por_nsmvp

### nsrfmvp
por_ret_nsrfmvp <- por_ret_nsrfmvp*100 # change to percentage
summary_por_nsrfmvp<-c(hx,summary(por_ret_nsrfmvp)*252, sd(por_ret_nsrfmvp)*sqrt(252),
                     my_skewness(por_ret_nsrfmvp)/sqrt(252),
                     my_kurtosis(por_ret_nsrfmvp)/252,
                     my_acf1(por_ret_nsrfmvp))
names(summary_por_nsrfmvp)[1] <- "No. of Obs."
names(summary_por_nsrfmvp)[8:length(summary_por_nsrfmvp)]<-c("Std.","SKewness","Kurtosis","ACF1")                  
summary_por_nsrfmvp[2:length(summary_por_nsrfmvp)] <- round(summary_por_nsrfmvp[2:length(summary_por_nsrfmvp)],3)
summary_por_nsrfmvp <- summary_por_nsrfmvp[-3]
summary_por_nsrfmvp <- summary_por_nsrfmvp[-5]
summary_por_nsrfmvp

## Sharpe ratio, set annualized risk free rate to 1%
rfx <- 0.01/252

### mvp
por_ret_mvp <- por_ret_mvp/100 # change to decimal
sr_mvp <- (mean(por_ret_mvp)-rfx)/(sd(por_ret_mvp))*sqrt(252)
sr_mvp <- round(sr_mvp,3) 
sr_mvp # 0.874

### nsgmvp
por_ret_nsgmvp <- por_ret_nsgmvp/100 # change to decimal
sr_nsgmvp <- (mean(por_ret_nsgmvp)-rfx)/(sd(por_ret_nsgmvp))*sqrt(252)
sr_nsgmvp <- round(sr_nsgmvp,3)
sr_nsgmvp # 0.684

### fixed
por_ret_fixed <- por_ret_fixed/100 # change to decimal
sr_fixed <- (mean(por_ret_fixed)-rfx)/(sd(por_ret_fixed))*sqrt(252)
sr_fixed <- round(sr_fixed,3) # 1.060
sr_fixed # 1.060

### gmvp
por_ret_gmvp <- por_ret_gmvp/100 # change to decimal
sr_gmvp <- (mean(por_ret_gmvp)-rfx)/(sd(por_ret_gmvp))*sqrt(252)
sr_gmvp <- round(sr_gmvp,3) 
sr_gmvp # 0.710

### rfmvp
por_ret_rfmvp <- por_ret_rfmvp/100 # change to decimal
sr_rfmvp <- (mean(por_ret_rfmvp)-rfx)/(sd(por_ret_rfmvp))*sqrt(252)
sr_rfmvp <- round(sr_rfmvp,3) 
sr_rfmvp # 0.454

### nsmvp
por_ret_nsmvp <- por_ret_nsmvp/100 # change to decimal
sr_nsmvp <- (mean(por_ret_nsmvp)-rfx)/(sd(por_ret_nsmvp))*sqrt(252)
sr_nsmvp <- round(sr_nsmvp,3) 
sr_nsmvp # 0.858

### nsrfmvp
por_ret_nsrfmvp <- por_ret_nsrfmvp/100 # change to decimal
sr_nsrfmvp <- (mean(por_ret_nsrfmvp)-rfx)/(sd(por_ret_nsrfmvp))*sqrt(252)
sr_nsrfmvp <- round(sr_nsrfmvp,3)
sr_nsrfmvp # 0.639

## Summary of turnover rate, HHI, and SLR
### mvp
summary_tor_mvp <- c(summary(tor_mvp),sd(tor_mvp))
names(summary_tor_mvp)[length(summary_tor_mvp)] <-"Std."
summary_tor_mvp <-summary_tor_mvp[-2];summary_tor_mvp <-summary_tor_mvp[-4]
summary_tor_mvp <-round(summary_tor_mvp,3)
summary_tor_mvp

summary_hhi_mvp <- c(summary(hhi_mvp),sd(hhi_mvp))
names(summary_hhi_mvp)[length(summary_hhi_mvp)] <-"Std."
summary_hhi_mvp <-summary_hhi_mvp[-2];summary_hhi_mvp <-summary_hhi_mvp[-4]
summary_hhi_mvp <-round(summary_hhi_mvp,3)
summary_hhi_mvp

summary_slr_mvp <- c(summary(slr_mvp),sd(slr_mvp))
names(summary_slr_mvp)[length(summary_slr_mvp)] <-"Std."
summary_slr_mvp <-summary_slr_mvp[-2];summary_slr_mvp <-summary_slr_mvp[-4]
summary_slr_mvp <-round(summary_slr_mvp,3)
summary_slr_mvp

### nsgmvp
summary_tor_nsgmvp <- c(summary(tor_nsgmvp),sd(tor_nsgmvp))
names(summary_tor_nsgmvp)[length(summary_tor_nsgmvp)] <-"Std."
summary_tor_nsgmvp <-summary_tor_nsgmvp[-2];summary_tor_nsgmvp <-summary_tor_nsgmvp[-4]
summary_tor_nsgmvp <-round(summary_tor_nsgmvp,3)
summary_tor_nsgmvp

summary_hhi_nsgmvp <- c(summary(hhi_nsgmvp),sd(hhi_nsgmvp))
names(summary_hhi_nsgmvp)[length(summary_hhi_nsgmvp)] <-"Std."
summary_hhi_nsgmvp <-summary_hhi_nsgmvp[-2];summary_hhi_nsgmvp <-summary_hhi_nsgmvp[-4]
summary_hhi_nsgmvp <-round(summary_hhi_nsgmvp,3)
summary_hhi_nsgmvp

summary_slr_nsgmvp <- c(summary(slr_nsgmvp),sd(slr_nsgmvp))
names(summary_slr_nsgmvp)[length(summary_slr_nsgmvp)] <-"Std."
summary_slr_nsgmvp <-summary_slr_nsgmvp[-2];summary_slr_nsgmvp <-summary_slr_nsgmvp[-4]
summary_slr_nsgmvp <-round(summary_slr_nsgmvp,3)
summary_slr_nsgmvp

### fixed weight
summary_tor_fixed <- c(summary(tor_fixed),sd(tor_fixed))
names(summary_tor_fixed)[length(summary_tor_fixed)] <-"Std."
summary_tor_fixed <-summary_tor_fixed[-2];summary_tor_fixed <-summary_tor_fixed[-4]
summary_tor_fixed <-round(summary_tor_fixed,3)
summary_tor_fixed

summary_hhi_fixed <- c(summary(hhi_fixed),sd(hhi_fixed))
names(summary_hhi_fixed)[length(summary_hhi_fixed)] <-"Std."
summary_hhi_fixed <-summary_hhi_fixed[-2];summary_hhi_fixed <-summary_hhi_fixed[-4]
summary_hhi_fixed <-round(summary_hhi_fixed,3)
summary_hhi_fixed 

summary_slr_fixed <- c(summary(slr_fixed),sd(slr_fixed))
names(summary_slr_fixed)[length(summary_slr_fixed)] <-"Std."
summary_slr_fixed <-summary_slr_fixed[-2];summary_slr_fixed <-summary_slr_fixed[-4]
summary_slr_fixed <-round(summary_slr_fixed,3)
summary_slr_fixed

### gmvp
summary_tor_gmvp <- c(summary(tor_gmvp),sd(tor_gmvp))
names(summary_tor_gmvp)[length(summary_tor_gmvp)] <-"Std."
summary_tor_gmvp <-summary_tor_gmvp[-2];summary_tor_gmvp <-summary_tor_gmvp[-4]
summary_tor_gmvp <-round(summary_tor_gmvp,3)
summary_tor_gmvp

summary_hhi_gmvp <- c(summary(hhi_gmvp),sd(hhi_gmvp))
names(summary_hhi_gmvp)[length(summary_hhi_gmvp)] <-"Std."
summary_hhi_gmvp <-summary_hhi_gmvp[-2];summary_hhi_gmvp <-summary_hhi_gmvp[-4]
summary_hhi_gmvp <-round(summary_hhi_gmvp,3)
summary_hhi_gmvp

summary_slr_gmvp <- c(summary(slr_gmvp),sd(slr_gmvp))
names(summary_slr_gmvp)[length(summary_slr_gmvp)] <-"Std."
summary_slr_gmvp <-summary_slr_gmvp[-2];summary_slr_gmvp <-summary_slr_gmvp[-4]
summary_slr_gmvp <-round(summary_slr_gmvp,3)
summary_slr_gmvp 

### rfmvp
summary_tor_rfmvp <- c(summary(tor_rfmvp),sd(tor_rfmvp))
names(summary_tor_rfmvp)[length(summary_tor_rfmvp)] <-"Std."
summary_tor_rfmvp <-summary_tor_rfmvp[-2];summary_tor_rfmvp <-summary_tor_rfmvp[-4]
summary_tor_rfmvp <-round(summary_tor_rfmvp,3)
summary_tor_rfmvp

summary_hhi_rfmvp <- c(summary(hhi_rfmvp),sd(hhi_rfmvp))
names(summary_hhi_rfmvp)[length(summary_hhi_rfmvp)] <-"Std."
summary_hhi_rfmvp <-summary_hhi_rfmvp[-2];summary_hhi_rfmvp <-summary_hhi_rfmvp[-4]
summary_hhi_rfmvp <-round(summary_hhi_rfmvp,3)
summary_hhi_rfmvp

summary_slr_rfmvp <- c(summary(slr_rfmvp),sd(slr_rfmvp))
names(summary_slr_rfmvp)[length(summary_slr_rfmvp)] <-"Std."
summary_slr_rfmvp <-summary_slr_rfmvp[-2];summary_slr_rfmvp <-summary_slr_rfmvp[-4]
summary_slr_rfmvp <-round(summary_slr_rfmvp,3)
summary_slr_rfmvp

### nsmvp
summary_tor_nsmvp <- c(summary(tor_nsmvp),sd(tor_nsmvp))
names(summary_tor_nsmvp)[length(summary_tor_nsmvp)] <-"Std."
summary_tor_nsmvp <-summary_tor_nsmvp[-2];summary_tor_nsmvp <-summary_tor_nsmvp[-4]
summary_tor_nsmvp <-round(summary_tor_nsmvp,3)
summary_tor_nsmvp

summary_hhi_nsmvp <- c(summary(hhi_nsmvp),sd(hhi_nsmvp))
names(summary_hhi_nsmvp)[length(summary_hhi_nsmvp)] <-"Std."
summary_hhi_nsmvp <-summary_hhi_nsmvp[-2];summary_hhi_nsmvp <-summary_hhi_nsmvp[-4]
summary_hhi_nsmvp <-round(summary_hhi_nsmvp,3)
summary_hhi_nsmvp

summary_slr_nsmvp <- c(summary(slr_nsmvp),sd(slr_nsmvp))
names(summary_slr_nsmvp)[length(summary_slr_nsmvp)] <-"Std."
summary_slr_nsmvp <-summary_slr_nsmvp[-2];summary_slr_nsmvp <-summary_slr_nsmvp[-4]
summary_slr_nsmvp <-round(summary_slr_nsmvp,3)
summary_slr_nsmvp

### nsrfmvp
summary_tor_nsrfmvp <- c(summary(tor_nsrfmvp),sd(tor_nsrfmvp))
names(summary_tor_nsrfmvp)[length(summary_tor_nsrfmvp)] <-"Std."
summary_tor_nsrfmvp <-summary_tor_nsrfmvp[-2];summary_tor_nsrfmvp <-summary_tor_nsrfmvp[-4]
summary_tor_nsrfmvp <-round(summary_tor_nsrfmvp,3)
summary_tor_nsrfmvp

summary_hhi_nsrfmvp <- c(summary(hhi_nsrfmvp),sd(hhi_nsrfmvp))
names(summary_hhi_nsrfmvp)[length(summary_hhi_nsrfmvp)] <-"Std."
summary_hhi_nsrfmvp <-summary_hhi_nsrfmvp[-2];summary_hhi_nsrfmvp <-summary_hhi_nsrfmvp[-4]
summary_hhi_nsrfmvp <-round(summary_hhi_nsrfmvp,3)
summary_hhi_nsrfmvp

summary_slr_nsrfmvp <- c(summary(slr_nsrfmvp),sd(slr_nsrfmvp))
names(summary_slr_nsrfmvp)[length(summary_slr_nsrfmvp)] <-"Std."
summary_slr_nsrfmvp <-summary_slr_nsrfmvp[-2];summary_slr_nsrfmvp <-summary_slr_nsrfmvp[-4]
summary_slr_nsrfmvp <-round(summary_slr_nsrfmvp,3)
summary_slr_nsrfmvp

## VaR, ES, LPSD of the oos portfolio returns
### mvp
por_ret_mvp <- por_ret_mvp*100 # in percentage
var_mvp <- round(VaR_samplex(por_ret_mvp,1,alphax=0.05),3);var_mvp
es_mvp <- round(ES_samplex(por_ret_mvp,1,alphax=0.05),3);es_mvp
lpsd_mvp <- round(LPSDx(por_ret_mvp,rfx=1/252),3);lpsd_mvp

### nsgmvp
por_ret_nsgmvp <- por_ret_nsgmvp*100 # in percentage
var_nsgmvp <- round(VaR_samplex(por_ret_nsgmvp,1,alphax=0.05),3);var_nsgmvp
es_nsgmvp <- round(ES_samplex(por_ret_nsgmvp,1,alphax=0.05),3);es_nsgmvp
lpsd_nsgmvp <- round(LPSDx(por_ret_nsgmvp,rfx=1/252),3);lpsd_nsgmvp

### fixed
por_ret_fixed <- por_ret_fixed*100 # in percentage
var_fixed <- round(VaR_samplex(por_ret_fixed,1,alphax=0.05),3);var_fixed
es_fixed <- round(ES_samplex(por_ret_fixed,1,alphax=0.05),3);es_fixed
lpsd_fixed <- round(LPSDx(por_ret_fixed,rfx=1/252),3);lpsd_fixed

### gmvp
por_ret_gmvp <- por_ret_gmvp*100 # in percentage
var_gmvp <- round(VaR_samplex(por_ret_gmvp,1,alphax=0.05),3);var_gmvp
es_gmvp <- round(ES_samplex(por_ret_gmvp,1,alphax=0.05),3);es_gmvp
lpsd_gmvp <- round(LPSDx(por_ret_gmvp,rfx=1/252),3);lpsd_gmvp

### rfmvp
por_ret_rfmvp <- por_ret_rfmvp*100 # in percentage
var_rfmvp <- round(VaR_samplex(por_ret_rfmvp,1,alphax=0.05),3);var_rfmvp
es_rfmvp <- round(ES_samplex(por_ret_rfmvp,1,alphax=0.05),3);es_rfmvp
lpsd_rfmvp <- round(LPSDx(por_ret_rfmvp,rfx=1/252),3);lpsd_rfmvp

### nsmvp
por_ret_nsmvp <- por_ret_nsmvp*100 # in percentage
var_nsmvp <- round(VaR_samplex(por_ret_nsmvp,1,alphax=0.05),3);var_nsmvp
es_nsmvp <- round(ES_samplex(por_ret_nsmvp,1,alphax=0.05),3);es_nsmvp
lpsd_nsmvp <- round(LPSDx(por_ret_nsmvp,rfx=1/252),3);lpsd_nsmvp

### nsrfmvp
por_ret_nsrfmvp <- por_ret_nsrfmvp*100 # in percentage
var_nsrfmvp <- round(VaR_samplex(por_ret_nsrfmvp,1,alphax=0.05),3);var_nsrfmvp
es_nsrfmvp <- round(ES_samplex(por_ret_nsrfmvp,1,alphax=0.05),3);es_nsrfmvp 
lpsd_nsrfmvp <- round(LPSDx(por_ret_nsrfmvp,rfx=1/252),3);lpsd_nsrfmvp
