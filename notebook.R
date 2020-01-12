good<-read.csv("/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 3/good.csv")
good<-good[ ,c("WICpreg","mathraw97","AGE97","faminc97","HOME97","CHRACE")]
good$RACE <- ifelse(good$CHRACE== 9, NA, ifelse(good$CHRACE== 1, .5, ifelse(good$CHRACE== 2, -.5, NA)))
goodR<-good[ ,c("WICpreg","mathraw97","AGE97","faminc97","HOME97","RACE")]

# descriptive statistics
N<-c(length(which(!is.na(goodR$WICpreg))),length(which(!is.na(goodR$mathraw97))),length(which(!is.na(goodR$AGE97))),length(which(!is.na(goodR$faminc97))),length(which(!is.na(goodR$HOME97))),length(which(!is.na(goodR$RACE))))
mean<-sapply(goodR,mean,na.rm=TRUE)
median<-sapply(goodR,median,na.rm=TRUE)
quantile<-sapply(goodR,quantile,na.rm=TRUE)
sd<-sapply(goodR,sd,na.rm=TRUE)
range<-sapply(goodR,range,na.rm=TRUE)
descriptive<-rbind(N,mean,median,quantile,sd,range)
write.csv(descriptive,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 3/descriptive.csv")
tableWICpreg<-table(goodR$WICpreg)
frequency<-tableWICpreg
write.csv(frequency,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 3/frequency.csv")
cor<-cor(goodR,use = "complete.obs")
correlation<-round(cor,2)
write.csv(correlation,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 3/correlation.csv")

goodR2<-na.omit(goodR[ ,c("WICpreg","mathraw97","AGE97","faminc97","RACE","HOME97")])
# assumptions
# linearity
pairs(goodR2,panel=panel.smooth)
library(ggplot2)

ggplot(goodR2,aes(x=AGE97, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("AGE97") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")
ggplot(goodR2,aes(x=faminc97, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("faminc97") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")
ggplot(goodR2,aes(x=HOME97, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("HOME97") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")

#homoscedasticity
lm<-lm(mathraw97~WICpreg+AGE97+faminc97+HOME97+RACE,data=goodR2)
goodR2.res<-resid(lm)
fitted.res<-fitted(lm)
plot(fitted.res,goodR2.res)
abline(0,0,col="red")
lines(lowess(goodR2.res~fitted.res),col="green")

#normality of residuals
hist(goodR2.res,15)

#outliers
lm2<-lm(mathraw97~WICpreg+AGE97+faminc97+HOME97+RACE,data=goodR2)
outliers<-goodR2[,c("mathraw97","WICpreg","AGE97","faminc97","RACE","HOME97")]
outliers$r<-rstudent(lm2)
outliers$lev<-hatvalues(lm2)
outliers$cd<-cooks.distance(lm2)
outliers$dffit<-dffits(lm2)
dfbetaR<-dfbetas(lm2)
colnames(dfbetaR)
colnames(dfbetaR)<-c("int_dfb","WICpreg_dfb","AGE97_dfb","faminc97_dfb","HOME97_dfb","RACE_dfb")
outliers<-cbind(outliers,dfbetaR)
head(outliers)
# discrepancy
plot(outliers$r,
     xlab="Index",
     ylab="studentized residuals",
     pch=19)
abline(h = 0, col = "red")
abline(h = -3, col = "green")
abline(h = 3, col = "green")
abline(h = -2, col = "blue")
abline(h = 2, col = "blue")
rstudent1<- subset(outliers,abs(r)>3)
# leverage
# (2*5+2)/2042=0.0059
plot(outliers$lev, 
     xlab="Index",
     ylab="leverage",
     pch=19)
abline(h=0.0059, col="blue")
leverage1<- subset(outliers,lev >0.01)
View(leverage1)
# influence - Cook's D
plot(outliers$cd, 
     xlab="Index",
     ylab="cook’s d",
     pch=19)
abline(h=4/1848, col="blue")
large_cd<-subset(outliers, cd > (4/1848))
quantile(large_cd$cd, probs = seq(0, 1, 0.025))
abline(h= .0057, col="green")
to_omit <- subset(outliers, cd >= .0057)
to_omit

#multicolinearity 
library(car)
multicolinearity<-vif(lm2)
multicolinearity
#no multicolinearity

#remove outliers
no_outliers<-subset(outliers, cd < .0057)

#transform faminc97
min(no_outliers$faminc97)
no_outliers$logfaminc<-ifelse(no_outliers$faminc97<= 1,0,ifelse(no_outliers$faminc97>1,log(no_outliers$faminc97), NA))
#transform AGE97
no_outliers$AGE97c<-no_outliers$AGE97-mean(no_outliers$AGE97)
no_outliers$AGE97c2<-(no_outliers$AGE97c)^2
no_outliers$AGE97c3<-(no_outliers$AGE97c)^3

#examine homoscedasticity again
lm<-lm(mathraw97~WICpreg+AGE97c+AGE97c2+AGE97c3+logfaminc+HOME97+RACE,data=no_outliers)
no_outliers.res<-resid(lm)
fitted.res<-fitted(lm)
plot(fitted.res,no_outliers.res)
abline(0,0,col="red")
lines(lowess(no_outliers.res~fitted.res),col="green")

#examine normality of residuals again 
hist(no_outliers.res,15)



#data transformation 

#new centered variables
no_outliers$chome<-no_outliers$HOME97 - mean(no_outliers$HOME97)
no_outliers$cincome<-no_outliers$logfaminc - mean(no_outliers$logfaminc)
#interaction variables
no_outliers$cincomeWIC<-no_outliers$cincome * no_outliers$WICpreg
no_outliers$RACEWIC<-no_outliers$RACE * no_outliers$WICpreg
no_outliers$AGE97cWIC<-no_outliers$AGE97c * no_outliers$WICpreg
no_outliers$AGE97c2WIC<-no_outliers$AGE97c2 * no_outliers$WICpreg
no_outliers$AGE97c3WIC<-no_outliers$AGE97c3 * no_outliers$WICpreg

library(lmSupport)
library(sjPlot)
#main effects model
lm_main<-lm(mathraw97 ~ WICpreg + AGE97c + AGE97c2 + AGE97c3+ cincome + RACE + chome, data=no_outliers)
summary(lm_main)
lm_main<-tab_model(lm_main)
lm_main

#interaction between WIC and income
lm_inter_1<-lm(mathraw97 ~ WICpreg + AGE97c + AGE97c2 + AGE97c3 + cincome + RACE + chome + cincomeWIC, data=no_outliers)
summary(lm_inter_1)
lm_inter_1<-tab_model(lm_inter_1)
lm_inter_1

#interaction between WIC and race
lm_inter_2<-lm(mathraw97 ~ WICpreg + AGE97c + AGE97c2 + +AGE97c3 + cincome + RACE + chome + RACEWIC, data=no_outliers)
summary(lm_inter_2)
lm_inter_2<-tab_model(lm_inter_2)
lm_inter_2

#interaction between WIC and age
lm_inter_3<-lm(mathraw97 ~ WICpreg + AGE97c + AGE97c2 + AGE97c3 + cincome + RACE + chome + AGE97cWIC +AGE97c2WIC + AGE97c3WIC, data=no_outliers)
summary(lm_inter_3)
lm_inter_3<-tab_model(lm_inter_3)
lm_inter_3
