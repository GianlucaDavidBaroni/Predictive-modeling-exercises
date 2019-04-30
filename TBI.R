library(rms)
TBIdata <- spss.get(file.choose("TBI.sav"), use.value.labels = FALSE)

dd <- datadist(TBIdata)
options(datadist='dd')


data.USA <- TBIdata[TBIdata$trial==75,]
data.INT <- TBIdata[TBIdata$trial==74,]

fit <- lrm(d.mort~age+d.motor+pupil.i, data = data.USA)
summary(fit)
print(fit)
fit$linear.predictors

#linear predictor and prediction probability according to the model for each patient in both dataset
lp.USA <- predict(fit, newdata = data.USA)
p.USA <- plogis(lp.USA)
y.USA <- data.USA$d.mort

lp.INT <- predict(fit, newdata = data.INT)
p.INT <- plogis(lp.INT)
y.INT <- data.INT$d.mort

#-----calibration in the large-------
mean(p.INT)
mean(y.INT)

mean(p.USA)
mean(y.USA)

#------fit with lp as only covariate - --
fit.lp <- lrm(d.mort~lp.USA, data = data.USA)
fit.lp #the coefficient is obviously 1 
