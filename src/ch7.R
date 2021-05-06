####################################
# Chapter 7 Lab: Non-linear Modeling
####################################

# packages
library(ISLR)
library(akima)
library(gam)
library(splines)

# data
attach(Wage)
View(Wage)

# 1. Polynomial Regression and Step Functions -----------------------

# create a 4th degree orthogonal polynomial: x^0, x^1, x^2, x^3
polynom <- poly(age, degree = 4) 

# fit by orthogonal polynomials
fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# fit by raw polynomials
fit2 = lm(wage ~ poly(age, 4, raw = T), data = Wage) 
coef(summary(fit2))

fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage) # same as above, just manual notation
coef(summary(fit2a))

fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage) # yet another notation for the same
coef(summary(fit2b))

# predict wages from age 18 to 80
agelims = range(age)
age.grid = seq(from=agelims[1],to=agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# plot
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# difference in estimate in orthogonal vs raw polynomials
preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit)) # basically zero

# decide on the right degree of polynomial to use
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5) # either a cubic or a quadratic model seems to be reasonable

# a more convenient way for doing the same comparison
coef(summary(fit.5))
(-11.983)^2 # the square of the t-statistics are equal to the F-statistics from the anova() function

# however, anova works with other terms
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

#  we fit a logistic regression model (weather wage > 250k)
fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

# predict
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)

# plot
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

# 2. Splines --------------------------------------------------------

# create a set of basis functions: 3 knots + 2 boundary knots result in 6 basis functions
basis_functions <- bs(age, knots = c(25,40,60))
basis_functions

# fit: regression spline
fit = lm(wage ~ bs(age, knots = c(25,40,60)), data = Wage)

# predict
pred = predict(fit,newdata=list(age=age.grid),se=T)

# plot
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# instead of knots, we can define the degrees of freedom (df) as well
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# fit: natural spline
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

# fit: smoothing spline
fit = smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df

# plot
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# fit: local regression
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)

# plot
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# 3. GAMs -----------------------------------------------------------

# fit: gam with natural splines
gam1 = lm(wage ~ ns(year,4) + ns(age,5) + education, data = Wage)

# fit: gam with smoothing splines
gam.m3 = gam(wage ~ s(year,4) + s(age,5) + education, data = Wage)

# plot
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.Gam(gam1, se=TRUE, col="red")

# compare models
gam.m1=gam(wage ~ s(age,5) + education, data = Wage)
gam.m2=gam(wage ~ year + s(age,5) + education, data = Wage)
anova(gam.m1,gam.m2,gam.m3,test="F") # gam with a linear function of year is better than gam without year. No need to have non-linearity for year though.
summary(gam.m3)

# predict 
preds=predict(gam.m2,newdata=Wage)

# fit: gam with local regression (loess)
gam.lo = gam(wage ~ s(year,df=4) + lo(age,span=0.7) + education, data = Wage)

# plot
plot.Gam(gam.lo, se=TRUE, col="green")

# fit: gam with local regression and interaction
gam.lo.i=gam(wage ~ lo(year, age,span=0.5)+education,data=Wage)
plot(gam.lo.i)

# fit: logistic case 
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

# frequency of response by categories: no high earners in less than high schools
table(education,I(wage>250))

# fit logistic case without "1. < HS Grad" category
gam.lr.s = gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
