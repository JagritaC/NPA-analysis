library(forecast)
library(tseries)
library(urca)
library(lmtest)
library(strucchange)
library(ggplot2)
#Read Dataset
GNPA <- read.csv("C:/Users/Dell/Desktop/My projects/GNPA.csv")
#Preprocessing Data
head(GNPA)
summary(GNPA)
sum(is.na(GNPA))
GNPA<-na.omit(GNPA)
GNPA
#Correlations between Gross Non-Performing Assets(GNPA)and other variables.
cor(GNPA[,2:8])
#Correlations between rate of change of GNPA and rate of change in other variables
cor(GNPA[,9:15])
#T test between public and private sector. 
#H0: Both public and private sector banks produce same amount of NPAs.
t1=t.test(GNPA[,24], GNPA[,25], paired = TRUE, conf.level=0.95)
t1
#H0: H0: Both priority sector and non-priority sector banks produce same amount of NPAs.
t2=t.test(GNPA[,26], GNPA[,27], paired = TRUE, conf.level=0.95)
t2
#ANOVA for comparing various recovery methods
#H0: All the recovery channels work with same efficiency and recovers equal amount of NPAs.
Lok.Adalats=GNPA[,16]
DRT=GNPA[,17]
SARFAESI=GNPA[,18]
cg=data.frame(cbind(Lok.Adalats ,DRT,SARFAESI))
sg<-stack(cg)
sg
AV=aov(values~ind,data=sg)
summary(AV)
#Time series analysis of GNPA in public sector banks.
k<-GNPA[,24]
k
time<-ts(k,start=2005,end=2019,frequency=1)
time
time1<-log(time)
plot(time1)
#auto-correlation function
acf(time)
pacf(time)
#testing stationarity
kpss.test(time,null='Trend')
adf.test(time)
#detrending
t1=diff(time)
#rechecking stationarity
adf.test(t1)
kpss.test(t1,null='Trend')
plot.ts(t1)
#checking structure change
model1=Fstats(t1~1,from=0.01)
sctest(model1)
strucchange::breakpoints(t1~1)
#modelling
acf(t1)
pacf(t1)

