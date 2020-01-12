good<-read.csv("/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 2/good.csv")
goodR<-good[ ,c("WICpreg","mathraw97","AGE97","faminc97","bthwht")]

# descriptive statistics
N<-c(length(which(!is.na(goodR$WICpreg))),length(which(!is.na(goodR$mathraw97))),length(which(!is.na(goodR$AGE97))),length(which(!is.na(goodR$faminc97))),length(which(!is.na(goodR$bthwht))))
mean<-sapply(goodR,mean,na.rm=TRUE)
median<-sapply(goodR,median,na.rm=TRUE)
quantile<-sapply(goodR,quantile,na.rm=TRUE)
sd<-sapply(goodR,sd,na.rm=TRUE)
range<-sapply(goodR,range,na.rm=TRUE)
descriptive<-rbind(N,mean,median,quantile,sd,range)
write.csv(descriptive,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 2/descriptive.csv")
tableWICpreg<-table(goodR$WICpreg)
frequency<-tableWICpreg
write.csv(frequency,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 2/frequency.csv")
cor<-cor(goodR,use = "complete.obs")
correlation<-round(cor,2)
write.csv(correlation,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 2/correlation.csv")

# assumptions
# linearity
pairs(goodR,panel=panel.smooth)
library(ggplot2)
ggplot(goodR,aes(x=WICpreg, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("WICpreg") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")
ggplot(goodR,aes(x=AGE97, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("AGE97") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")
ggplot(goodR,aes(x=faminc97, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("faminc97") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")
ggplot(goodR,aes(x=bthwht, y = mathraw97)) +
  geom_point(size = 0.6) +
  xlab("bthwht") +
  ylab("mathraw97") +
  theme_bw() +
  geom_smooth(method = "loess")

#homoscedasticity
lm<-lm(mathraw97~WICpreg+AGE97+faminc97+bthwht,data=goodR)
goodR.res<-resid(lm)
fitted.res<-fitted(lm)
plot(fitted.res,goodR.res)
abline(0,0,col="red")
lines(lowess(goodR.res~fitted.res),col="green")

#normality of residuals
hist(goodR.res,15)

#omitted variable HOME97
goodR2<-na.omit(good[ ,c("WICpreg","mathraw97","AGE97","faminc97","bthwht","HOME97")])
m1<-lm(mathraw97~WICpreg+AGE97+faminc97+bthwht,data=goodR2)
m2<-lm(HOME97~WICpreg+AGE97+faminc97+bthwht,data=goodR2)
m1_resid<-m1$residuals
m2_resid<-m2$residuals
plot(m2_resid,m1_resid)
abline(lm(m1_resid~m2_resid),col="red")
lines(lowess(m1_resid~m2_resid),col="blue")

#outliers
lm2<-lm(mathraw97~WICpreg+AGE97+faminc97+bthwht+HOME97,data=goodR2)
outliers<-goodR2[,c("mathraw97","WICpreg","AGE97","faminc97","bthwht","HOME97")]
outliers$r<-rstudent(lm2)
outliers$lev<-hatvalues(lm2)
outliers$cd<-cooks.distance(lm2)
outliers$dffit<-dffits(lm2)
dfbetaR<-dfbetas(lm2)
colnames(dfbetaR)
colnames(dfbetaR)<-c("int_dfb","WICpreg_dfb","AGE97_dfb","famin97_dfb","bthwht_dfb","HOME97_dfb")
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
abline(h = 2, col = "blue")
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
abline(h=4/2042, col="blue")
large_cd<-subset(outliers, cd > (4/2042))
quantile(large_cd$cd, probs = seq(0, 1, 0.025))
abline(h= .0066, col="green")
to_omit <- subset(outliers, cd >= .0066)
to_omit

#multicolinearity 
multicolinearity<-vif(lm2)
write.csv(multicolinearity,"/Users/yunxin/Desktop/PENN/Courses/Linear Modeling/Assignment 2/multicolinearity.csv")
#no multicolinearity

#remove outliers
no_outliers<-subset(outliers, cd < .0066)

#data transformation 
hist(no_outliers$faminc97,freq=FALSE)
lines(density(no_outliers$faminc97))
hist(no_outliers$AGE97,freq=FALSE)
lines(density(no_outliers$AGE97))
#transform faminc97
min(no_outliers$faminc97)
no_outliers$logfaminc<-ifelse(no_outliers$faminc97<= 1,0,ifelse(no_outliers$faminc97>1,log(no_outliers$faminc97), NA))
#transform AGE97
no_outliers$AGE97c<-no_outliers$AGE97-mean(no_outliers$AGE97)
no_outliers$AGE97c2<-(no_outliers$AGE97c)^2
summary(lm)
lm_1<-tab_model(lm)


lm3<-lm(mathraw97~WICpreg+logfaminc+AGE97c+AGE97c2+bthwht+HOME97,data=no_outliers)
summary(lm3)
lm_3<-tab_model(lm3)
