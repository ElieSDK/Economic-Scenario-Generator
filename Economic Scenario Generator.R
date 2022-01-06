install.packages('quantmod')
install.packages('fBasics')
install.packages('corrplot')
install.packages('xts')
install.packages('vars')
install.packages('xlsx')
install.packages('TTR')
install.packages('fGarch')
install.packages('zoo')
library(quantmod)
library(fBasics)
library(corrplot)
library(xts)
library(vars)
library(xlsx)
library(TTR)
library(fGarch)
library(zoo)

#-------------------------------------------------------------------------------


#Import equity csv (contains seven worldwide (MSCI) Indexes)
Equity<-read.csv("C:/Users/sdk/Desktop/Data/Equity.csv", stringsAsFactors=FALSE)


#Remove first row
Equity<-Equity[-1,]


#Convert to numeric and replace NA by 0
#MSCI.Daily.TR.Emerging.Markets
#MSCI.Daily.TR.Gross.EMU.Local
Equity$MSCI.Daily.TR.Emerging.Markets<-suppressWarnings(as.numeric(Equity$MSCI.Daily.TR.Emerging.Markets))
Equity$MSCI.Daily.TR.Gross.EMU.Local<-suppressWarnings(as.numeric(Equity$MSCI.Daily.TR.Gross.EMU.Local))
Equity$MSCI.Daily.TR.Gross.World.Loca<-as.numeric(Equity$MSCI.Daily.TR.Gross.World.Loca)
Equity$MSCI.Daily.TR.Gross.Japan.Loca<-as.numeric(Equity$MSCI.Daily.TR.Gross.Japan.Loca)

Equity[is.na(Equity)] <- 0


#Convert to date format
Equity$Date<-as.Date(Equity$Date,"%d/%m/%Y")


#Daily dataset: From 01/01/1999 to 31/03/2014 
Daily<-Equity[Equity$Date >= as.Date("010199", "%d%m%y") & Equity$Date <= as.Date("310314", "%d%m%y"), ]


#Compound Annual Growth Rate - CAGR
#Describes the rate at which an investment would have grown if it grew at a steady rate.
#More info: investopedia.com/terms/c/cagr.asp
beginning_value<-head(Daily$MSCI.Daily.TR.Gross.World.Loca, n=1)
ending_value<-tail(Daily$MSCI.Daily.TR.Gross.World.Loca, n=1)

beginning_date<-head(Daily$Date, n=1)
beginning_year<-as.numeric(format(beginning_date,"%Y"))

ending_date<-tail(Daily$Date, n=1)
ending_year<-as.numeric(format(ending_date,"%Y"))


#CAGR Formula
CAGR<-(-1)+(ending_value/beginning_value)^(1/(ending_year-beginning_year))
CAGR


#Periodic Returns
#quarterlyReturn, weeklyReturn, allReturns
#More info: quantmod.com/documentation/periodReturn.html

ts <- xts(Daily$MSCI.Daily.TR.Gross.World.Loca,Daily$Date)

DailyReturn = data.frame(Daily$Date,dailyReturn(ts)) 
MonthlyReturn = data.frame(monthlyReturn(ts)) 
AnnualReturn = data.frame(annualReturn(ts))
QuarterlyReturn = data.frame(quarterlyReturn(ts)) 


#Plots
hist(DailyReturn$daily.returns,
     main="Histogram of DailyReturn",
     col=c("red"),
     ylim=c(0,600),
     xlim=c(-0.05,0.05),
     breaks=100)
lines(density(DailyReturn$daily.returns),col="blue",lwd=2)

hist(MonthlyReturn$monthly.returns,
     main="Histogram of MonthlyReturn",
     col=c("red"),
     ylim=c(0,20),
     xlim=c(-0.2,0.2),
     breaks=50)
lines(density(MonthlyReturn$monthly.returns),col="blue",lwd=2)

hist(AnnualReturn$yearly.returns,
     main="Histogram of AnnualReturn",
     col=c("red"),
     ylim=c(0,5),
     xlim=c(-0.5,0.5),
     breaks=25)
lines(density(AnnualReturn$yearly.returns),col="blue",lwd=2)


#Indexes Rebase

#Convert to numeric
Daily$MSCI.Daily.TR.Gross.World.Loca<-as.numeric(Daily$MSCI.Daily.TR.Gross.World.Loca)
Daily$Daily$MSCI.Daily.TR.Gross.Japan.Loca<-as.numeric(Daily$Daily$MSCI.Daily.TR.Gross.Japan.Loca)
Daily$MSCI.Daily.TR.Gross.UK.Local<-as.numeric(Daily$MSCI.Daily.TR.Gross.UK.Local)
Daily$MSCI.Daily.TR.Gross.EMU.Local<-as.numeric(Daily$MSCI.Daily.TR.Gross.EMU.Local)
Daily$MSCI.Daily.TR.Gross.North.Amer<-as.numeric(Daily$MSCI.Daily.TR.Gross.North.Amer)
Daily$MSCI.Daily.TR.Gross.Pacific.Lo<-as.numeric(Daily$MSCI.Daily.TR.Gross.Pacific.Lo)
Daily$MSCI.Daily.TR.Emerging.Markets<-as.numeric(Daily$MSCI.Daily.TR.Emerging.Markets)


I <- data.frame(Daily[[1]])

for(i in 1:nrow(Daily)){
  
  
  I[i,2]=(Daily$MSCI.Daily.TR.Gross.World.Loca[i]/Daily$MSCI.Daily.TR.Gross.World.Loca[1])*100
  I[i,3]=(Daily$MSCI.Daily.TR.Gross.Japan.Loca[i]/Daily$MSCI.Daily.TR.Gross.Japan.Loca[1])*100
  I[i,4]=(Daily$MSCI.Daily.TR.Gross.UK.Local[i]/Daily$MSCI.Daily.TR.Gross.UK.Local[1])*100
  I[i,5]=(Daily$MSCI.Daily.TR.Gross.EMU.Local[i]/Daily$MSCI.Daily.TR.Gross.EMU.Local[1])*100
  I[i,6]=(Daily$MSCI.Daily.TR.Gross.North.Amer[i]/Daily$MSCI.Daily.TR.Gross.North.Amer[1])*100
  I[i,7]=(Daily$MSCI.Daily.TR.Gross.Pacific.Lo[i]/Daily$MSCI.Daily.TR.Gross.Pacific.Lo[1])*100
  I[i,8]=(Daily$MSCI.Daily.TR.Emerging.Markets[i]/Daily$MSCI.Daily.TR.Emerging.Markets[1])*100  
  
  i<- i + 1
}


#Variables rename
names(I)[2]<-"World"
names(I)[3]<-"JPN"
names(I)[4]<-"UK"
names(I)[5]<-"EMU"
names(I)[6]<-"NAm"
names(I)[7]<-"PACFIC"
names(I)[8]<-"EM"


#Rebased indexes plots
plot(I$Daily..1.., I$World, col=c("Red"), type="l",lwd=1, ylim=c(0,750))
lines(I$Daily..1.., I$JPN, col=c("Blue"), lwd=1)
lines(I$Daily..1.., I$UK, col=c("Yellow"), lwd=1)
lines(I$Daily..1.., I$EMU, col=c("Orange"), lwd=1)
lines(I$Daily..1.., I$NAm, col=c("Violet"), lwd=1)
lines(I$Daily..1.., I$PACFIC, col=c("Black"), lwd=1)
lines(I$Daily..1.., I$EM, col=c("Green"), lwd=1)
legend("topleft", title="Indexes", c("World","JPN","UK","EMU","NAm","PACIFIC","EM"), 
       fill=c("Red","Blue","Yellow","Orange","Violet","Black","Green"), 
       horiz=FALSE,
       cex=0.5)


#Skewness
#More info: https://www.investopedia.com/terms/s/skewness.asp

#skewness(DailyReturn$daily.returns)
basicStats(DailyReturn$daily.returns)  
basicStats(MonthlyReturn$monthly.returns)  
basicStats(AnnualReturn$yearly.returns)  



#Annualized Standard Deviation
#More info: cran.r-project.org/web/packages/caTools/caTools.pdf

#Import Date
Annualized = data.frame(Daily$Date) 
#Compute annualized return
Annualized$Ann.rt = runSD(DailyReturn$daily.returns, 253)
#Replace NA by 0
Annualized[is.na(Annualized)] <- 0

#Plots of annualized SD
plot(Annualized, type='l')
plot(density(Annualized$Ann.rt), type='l')

hist(Annualized$Ann.rt,
     col=c("blue"),
     ylim=c(0,300),
     xlim=c(0,0.025),
     breaks=100)



#Creation of a New data.frame for performance analysis
#Time-Series Objects (stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html)
RPtest <- ts(DailyReturn$daily.returns, start=c(2009, 1), end=c(2014, 12), frequency=7)
RPtest2 <- window(RPtest, start=c(2013, 1), end=c(2014, 1)) 



#Indexes Correlation
#cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

IC <- I[,-1]
cor(IC)
corrplot(cor(IC), method = "number")
corrplot.mixed(cor(IC))



#Rolling annualized return of Indexes

#Copy Daily vector & remove useless columns
TS2<-Daily
TS2<-TS2[,-9]


#p.30 - cran.r-project.org/web/packages/TTR/TTR.pdf

#Log daily return

TS2.An.R<-data.frame(Daily$Date)

TS2.An.R$World <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.World.Loca,Daily$Date)))
TS2.An.R$JPN <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.Japan.Loca,Daily$Date)))
TS2.An.R$UK <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.UK.Local,Daily$Date)))
TS2.An.R$EMU <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.EMU.Local,Daily$Date)))
TS2.An.R$NAm <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.North.Amer,Daily$Date)))
TS2.An.R$PACFIC <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Gross.Pacific.Lo,Daily$Date)))
TS2.An.R$EM <-log(1+dailyReturn(xts(Daily$MSCI.Daily.TR.Emerging.Markets,Daily$Date)))



#UK nominal & real interest rates
UKRFINAL<-read.csv("C:/Users/sdk/Desktop/Data/UKRFINAL.csv", stringsAsFactors=FALSE)

#Convert to date format
UKRFINAL$Date<-as.Date(UKRFINAL$Date,"%d/%m/%Y")


#Remove weekends rows
UKRFINAL<-UKRFINAL[complete.cases(UKRFINAL$NOM.1),]


#Import Hedge Funds, Property, Commodities, Inflation csv
HF<-read.csv("C:/Users/sdk/Desktop/Data/HF.csv", stringsAsFactors=FALSE)
PPT<-read.csv("C:/Users/sdk/Desktop/Data/Property.csv", stringsAsFactors=FALSE)
CM<-read.csv("C:/Users/sdk/Desktop/Data/CM.csv", stringsAsFactors=FALSE)
Infla3<-read.csv("C:/Users/sdk/Desktop/Data/Infla.csv", stringsAsFactors=FALSE)

#Convert to date format
HF$Date<-as.Date(HF$Date,"%d/%m/%Y")
PPT$Date<-as.Date(PPT$Date,"%d/%m/%Y")
CM$Date<-as.Date(CM$Date,"%d/%m/%Y")
Infla3$Date<-as.Date(Infla3$Date,"%d/%m/%Y")

#Convert to time series
HF.ts<-xts(HF$HFRI.Fund.Weighted.Composite.I,HF$Date)
PPT.ts<-xts(PPT$UK.IPD.Total.Return.All.Proper,PPT$Date)
CM.ts<-xts(CM$DJUBS,CM$Date)
Inf2.ts<-xts(Infla3[,2:3],Infla3$Date)

Infla3<-Infla3[complete.cases(Infla3),]

#Daily Returns
DailyHF = data.frame(HF$Date,dailyReturn(HF.ts)) 
DailyPPT = data.frame(PPT$Date,dailyReturn(PPT.ts)) 
DailyCM = data.frame(CM$Date,dailyReturn(CM.ts)) 
DailyCPI = data.frame(Infla3$Date,dailyReturn(Inf2.ts$CPI)) 
DailyRPI = data.frame(Infla3$Date,dailyReturn(Inf2.ts$RPI)) 


#--------------------------#Merging---------------------------------------------


#Equity returns (Rolling annual log return : TS2.An.R)
#Commodities, Property and Hedge funds returns (DailyCM, DailyPPT, DailyHF)
#Nominal an Real interest rates (UKRFINAL)
#Rates of inflation (CPI, RPI)


#Equity returns
ts.all <- xts(Daily[,2:8], Daily$Date)

DailyAll = data.frame(Daily$Date,dailyReturn(ts.all[,4])) 

DailyAll <- merge(dailyReturn(ts.all[,1]),dailyReturn(ts.all[,2]),
                  dailyReturn(ts.all[,3]),dailyReturn(ts.all[,4]),
                  dailyReturn(ts.all[,5]),dailyReturn(ts.all[,6]),
                  dailyReturn(ts.all[,7]))

names(DailyAll)[1]<-"World"
names(DailyAll)[2]<-"JPN"
names(DailyAll)[3]<-"UK"
names(DailyAll)[4]<-"EMU"
names(DailyAll)[5]<-"NAm"
names(DailyAll)[6]<-"PACFIC"
names(DailyAll)[7]<-"EM"

DailyAll2 = data.frame(Daily$Date, DailyAll) 


#Commodities, Property and Hedge funds returns

Daily.CHFP <- merge(merge(DailyCM, DailyHF, by.x="CM.Date", by.y="HF.Date"), DailyPPT,
                    by.x="CM.Date", by.y="PPT.Date")

names(Daily.CHFP)[1]<-"Date"
names(Daily.CHFP)[2]<-"CM"
names(Daily.CHFP)[3]<-"HF"
names(Daily.CHFP)[4]<-"PPT"



#Inflation rate (DailyCPI, DailyRPI)
Daily.UKif <- merge(DailyCPI, DailyRPI, by.x="Infla3.Date", by.y="Infla3.Date")

names(Daily.UKif)[2]<-"CPI"
names(Daily.UKif)[3]<-"RPI"



#Final Merging

Daily.Merge1 <- merge(merge(DailyAll2, Daily.CHFP, by.x="Daily.Date", by.y="Date"), UKRFINAL,
                      by.x="Daily.Date", by.y="Date")

Daily.Final <- merge(Daily.Merge1, Daily.UKif, by.x="Daily.Date", by.y="Infla3.Date")


#--------------------------#Interpolation---------------------------------------


#Create data frame
DailyHF.i = data.frame(Daily.Final$HF) 
DailyPPT.i = data.frame(Daily.Final$PPT)  
CPI.i = data.frame(Daily.Final$CPI)
RPI.i = data.frame(Daily.Final$RPI)

#Replace 0 by NA
DailyHF.i[DailyHF.i == 0] <- NA
DailyPPT.i[DailyPPT.i == 0] <- NA
CPI.i[CPI.i == 0] <- NA
RPI.i[RPI.i == 0] <- NA

#Initialisation of first and last obs.
DailyHF.i[1,1]<-0.0450010734

DailyPPT.i[1,1]<-0.011003217
DailyPPT.i[3789,1]<-0.01112791

CPI.i[3789,1]<-0.0007880221

RPI.i[3789,1]<-0.0007939659


#Interpolation
DailyHF.i$Int<-na.approx(DailyHF.i$Daily.Final.HF, method = "linear")
DailyPPT.i$Int<-na.approx(DailyPPT.i$Daily.Final.PPT, method = "linear")
CPI.i$Int<-na.approx(CPI.i$Daily.Final.CPI, method = "linear")
RPI.i$Int<-na.approx(RPI.i$Daily.Final.RPI, method = "linear")

Daily.Final$HF<-DailyHF.i$Int
Daily.Final$PPT<-DailyPPT.i$Int
Daily.Final$CPI<-CPI.i$Int
Daily.Final$RPI<-RPI.i$Int



#--------------------------#Monthly Data----------------------------------------


#Indexes
TS2.M$World <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.World.Loca,Daily$Date)))
TS2.M$JPN <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.Japan.Loca,Daily$Date)))
TS2.M$UK <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.UK.Local,Daily$Date)))
TS2.M$EMU <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.EMU.Local,Daily$Date)))
TS2.M$NAm <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.North.Amer,Daily$Date)))
TS2.M$PACFIC <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Gross.Pacific.Lo,Daily$Date)))
TS2.M$EM <-log(1+monthlyReturn(xts(Daily$MSCI.Daily.TR.Emerging.Markets,Daily$Date)))

TS2.M<-data.frame(TS2.M)

names(TS2.M)[1]<-"World"
names(TS2.M)[2]<-"JPN"
names(TS2.M)[3]<-"UK"
names(TS2.M)[4]<-"EMU"
names(TS2.M)[5]<-"NAm"
names(TS2.M)[6]<-"PACFIC"
names(TS2.M)[7]<-"EM"

TS2.M$Date <-row.names(TS2.M)

#HF, PPT, CM, CPI, RPI Returns
TS2M.HF = monthlyReturn(HF.ts)
TS2M.PPT = monthlyReturn(PPT.ts)
TS2M.CM = monthlyReturn(CM.ts)
TS2M.CPI = monthlyReturn(Inf2.ts$CPI)
TS2M.RPI = monthlyReturn(Inf2.ts$RPI)


#Merging

MonthlyAll <- merge(TS2M.HF, TS2M.PPT, TS2M.CM, TS2M.CPI, TS2M.RPI) 

names(MonthlyAll)[1]<-"HF"
names(MonthlyAll)[2]<-"PPT"
names(MonthlyAll)[3]<-"CM"
names(MonthlyAll)[4]<-"CPI"
names(MonthlyAll)[5]<-"RPI"


#Replace NA by previous value for CM
MonthlyAll$CM<-na.locf(MonthlyAll$CM)

MonthlyAll<-MonthlyAll[complete.cases(MonthlyAll),]


write.csv(MonthlyAll, file = "C:/Users/sdk/Desktop/Data/MonthlyAll.csv")


write.csv(TS2.M, file = "C:/Users/sdk/Desktop/Data/TS2.M.csv")

#Read
MonthlyAllFi<-read.csv("C:/Users/Asus/QA/FiNaL/MonthlyAllFi.csv", stringsAsFactors=FALSE)

MonthlyAllFi$Date<-as.Date(MonthlyAllFi$Date,"%d/%m/%Y")

capture.output(MonthlyAllFi, file = "C:/Monthly.txt")



write.csv(Daily.Final, file = "C:/Users/sdk/Desktop/Data/Daily.Final.csv")



#--------------------------#STATS-----------------------------------------------

#--------------------------#Vector Autoregression Processes---------------------

#More info: christophj.github.io/replicating/r/vector-autoregression-var-in-r/


vardata<-VAR(Daily.Final[,18:23], p = 30, type = c("const"), season = NULL, exogen = NULL, lag.max = NULL)


#Summary
capture.output(summary(vardata), file = "C:/Users/sdk/Desktop/Data/output30.txt")


#Coeff (stats.stackexchange.com/a/46742)
capture.output(coef(vardata), file = "C:/Users/sdk/Desktop/Data/coef30.txt")


#Errors
capture.output(VARselect(Daily.FinalR[,2:23], type = c("const"), lag.max = 18), 
               file = "C:/Users/sdk/Desktop/Data/error30.txt")


#Forecasting VAR with CI
capture.output(predict(vardata, n.ahead = 30, ci = 0.95, dumvar = NULL), 
               file = "C:/Users/sdk/Desktop/Data/predict30.txt")


#Residuals
residuals(vardata)


#Plot
plot(vardata)
plot(predict(vardata, n.ahead = 100, ci = 0.95, dumvar = NULL))


#vars
vardata2<-VAR(Daily.FinalR[,15:23], p = 10, type = c("const"), season = NULL, exogen = NULL, lag.max = NULL)
plot(predict(vardata2, n.ahead = 100, ci = 0.95, dumvar = NULL))
stability(vardata)
fanchart(predict(vardata, n.ahead = 100, ci = 0.95, dumvar = NULL))


#(VAR(p) for n.ahead steps)
fevd(vardata, n.ahead = 100)



#--------------------------#Monthly VAR(1)--------------------------------------


vardataM<-VAR(MonthlyAllFi[,2:23], p = 1, type = c("const"), season = NULL, exogen = NULL, lag.max = NULL)


#Summary
capture.output(summary(vardataM), file = "C:/output1.txt")


#Coeff (stats.stackexchange.com/a/46742, pdf p161)
capture.output(coef(vardataM), file = "C:/coef1.txt")


#Errors
capture.output(VARselect(MonthlyAllFi[,2:23], type = c("const"), lag.max = 5), 
               file = "C:/error5.txt")


#Forecasting VAR with CI
capture.output(predict(vardataM, n.ahead = 240, ci = 0.95, dumvar = NULL), 
               file = "C:/Users/sdk/Desktop/Data/predict5.txt")


#Correlation matrix
covM2 <- cov(MonthlyAllFi[,2:23], y = NULL, use = "everything")


#Cholesky Matrix
cholM <- chol(covM2)

write.csv(cholM, file = "C:/Users/sdk/Desktop/Data/cholM2.csv")


#Plot
plot(vardataM)

pdf(file="myplot.pdf", width=120, height=120)
png("100kHighRes300dpi.png", units="px", width=1600, height=1600, res=300)
plot(predict(vardataM, n.ahead = 240, ci = 0.95, dumvar = NULL))
dev.off()


#--------------------------#Cholesky Matrix-------------------------------------


#Mat Cov Res
Chol<-read.xlsx("C:/Users/Asus/Desktop/Chol.xlsx",1)


#Correlation matrix
covM <- cov(Daily.FinalR[,2:23], y = NULL, use = "everything")


#Cholesky Matrix
cholM <- chol(Chol[,2:23])
write.xlsx(cholM, file = "C:/Users/Asus/Desktop/cholM.xlsx")

Chol<-NULL