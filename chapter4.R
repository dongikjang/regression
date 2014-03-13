

library(gdata)
library(Hmisc)

desplot <- function(data.set, legend.xy, main=NULL){
	hist.fit <- hist(data.set, plot=FALSE)
	dens.fit <- density(data.set)
	xlim <- range(range(dens.fit$x), range(hist.fit$breaks))
	xlim <- c(min(xlim[1],-4), max(xlim[2],4))
	xseq <- seq(min(xlim[1],-4), max(xlim[2],4), , 100)
	yseq <- dnorm(xseq)
	ylim <- c(0, max(max(hist.fit$density), max(yseq)))*1.1
	par(mfrow=c(1,2), cex.main=2)
	hist.fit$counts <- hist.fit$density
	plot(hist.fit, xlim = xlim, ylim = ylim, xlab="", ylab="Density", main="", border="grey", col="grey")
	lines(xseq, yseq, lwd=2, col=2)
	legend(legend.xy, 'Normal',lty=1, lwd=2, col=2)
	qqnorm(data.set, main='')
	qqline(data.set, col=3, lty=1, lwd=2)
	title(main=main, line=-2, outer=TRUE)
}

#pdf(paste(path2, "qqplot_1.pdf", sep=""), width=12, height=6.75)
set.seed(324)
x <- rnorm(1000)
desplot(x, "topleft", "Gaussian Distribution")
#dev.off()

#pdf(paste(path2, "qqplot_2.pdf", sep=""), width=12, height=6.75)
x <-rnorm(1100,0,.95)
x <-  sort(x)[51:1050]
desplot(x, "topleft", "Light Tailed Distribution")
#dev.off()

#pdf(paste(path2, "qqplot_3.pdf", sep=""), width=12, height=6.75)
x <- rt(1000,4)
desplot(x, "topleft", "Heavy Tailed Distribution")
#dev.off()

#pdf(paste(path2, "qqplot_4.pdf", sep=""), width=12, height=6.75)
x <- rchisq(1000,2)-2
desplot(x, "topleft", "Positive Skewed Distribution")
#dev.off()

#pdf(paste(path2, "qqplot_5.pdf", sep=""), width=12, height=6.75)
x<- -(rchisq(1000,2)-2)
desplot(x, "topleft", "Negative Skewed Distribution")
#dev.off()



#pdf(paste(path2, "residual_1.pdf", sep=""), width=13, height=6.75)
#non independent error(nagative autocorrelation)
set.seed(324)
a.t <- rnorm(30,0,3)
rho <- -.7
e.0 <- 0
e.t<-c()
for(i in 1:30){
	if(i==1) e.t[i]<-rho*e.0+a.t[i] else e.t[i]<-rho*e.t[i-1]+a.t[i]
}
x <- seq(-10,10,,30)
y <- 2+3*x+e.t
fit <- lm(y~x)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)

plot(x,y)
abline(fit)
plot(fit$fitt,fit$resi,xlab='Fitted value',ylab='Residual',type='b')
abline(h=0)
title(main='Non independent errors(negative autocorrelation)',line=-2,outer=T)
#dev.off()




#pdf(paste(path2, "residual_2.pdf", sep=""), width=13, height=6.75)
set.seed(324)
a.t<-rnorm(30,0,3)
rho<-.9
e.0<-0
e.t<-c()
for(i in 1:30){
	if(i==1) e.t[i]<-rho*e.0+a.t[i] else e.t[i]<-rho*e.t[i-1]+a.t[i]
}
x<-seq(-10,10,,30)
y<-2+3*x+e.t
fit<-lm(y~x)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)

plot(x,y)
abline(fit)
plot(fit$fitt,fit$resi,xlab='Fitted value',ylab='Residual',type='b')
abline(h=0)
title(main='Non independent errors(positive autocorrelation)',line=-2,outer=T)
#dev.off()


#pdf(paste(path2, "residual_3.pdf", sep=""), width=13, height=6.75)
#non constant variance
set.seed(324)
e.t<-c()
for(i in 1:30){
	e.t[i]<-rnorm(1,0,i/5)
}
x<-seq(-10,10,,30)
y<-2+3*x+e.t
fit<-lm(y~x)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
plot(x,y)
abline(fit)
plot(fit$fitt,fit$resi,xlab='Fitted value',ylab='Residual',type='n')
title(main='Non constant variance(funnel)',line=-2,outer=T)
polygon(c(-30,35,35,-30),c(0,11.9,-11.3,0), col = "gray")
par(new=T)
plot(fit$fitt,fit$resi,xlab='',ylab='')
abline(h=0)

#dev.off()


#pdf(paste(path2, "residual_4.pdf", sep=""), width=13, height=6.75)
e.t<-c()
set.seed(324)
for(i in 1:50){
	if(i<25) e.t[i]<-rnorm(1,0,i/5) else e.t[i]<-rnorm(1,0,abs(i-51)/5)
}
x<-seq(-10,10,,50)
y<-2+3*x+e.t
fit<-lm(y~x)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
plot(x,y)
abline(fit)
plot(fit$fitt,fit$resi,xlab='Fitted value',ylab='Residual',type='n')
abline(h=0)
title(main='Non constant variance(double bow)',line=-2,outer=T)
a<-seq(-30,35)
b<--(a-35)*(a+30)/130
c<--b
polygon(c(a,rev(a)),c(b,rev(c)), col = "gray")
par(new=T)
plot(fit$fitt,fit$resi,xlab='',ylab='')
abline(h=0)
#dev.off()

#pdf(paste(path2, "residual_5.pdf", sep=""), width=13, height=6.75)
#nonlinear
set.seed(324)
e.t<-rnorm(30,0,5)
x<-sample(seq(-10,10,,50),30)
y<-2+3*x+.5*x^2+e.t
fit<-lm(y~x)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
plot(x,y)
abline(fit)
plot(fit$fitt,fit$resi,xlab='Fitted value',ylab='Residual',type='n')
abline(h=0)
title(main='Non linear',line=-2,outer=T)
a<-sort(fit$fitt)
b <- -16-.5*x+.5*x^2+10
b<-b[order(fit$fitt)]
c <- b-17
polygon(c(a,rev(a)),c(b,rev(c)), col = "gray",lty=0)
par(new=T)
plot(fit$fitt,fit$resi,xlab='',ylab='')
abline(h=0)
#dev.off()

#scaled <- function(model, type="standardized")
#UseMethod("scaled")
#
#scaled.lm <- function(model, type="standardized"){
#	switch(type,
#		studentized = rstandard(model),
#		
#		rstudent = rstudent(model),
#		
#		standardized = residuals(model)/summary(model)$sigma
#	)
#}

url <- "https://raw.github.com/dongikjang/regression/master/"
rfun <- getURL(paste(http, "scaled.R",sep=""))
eval(parse(text=rfun))
rfun <- getURL(paste(http, "read.xls2.r",sep=""))
eval(parse(text=rfun))
# If OS is Windows then install "xlsReadWrite" package
# If OS is Mac or Linux then install "gdata" package

library(RCurl)
tf <- paste(tempfile(), "xls", sep = ".")
download.file(paste(url, "Dataset/data-ex-3-1.xls", sep=""), tf, method="curl")
data_3.1 <- read.xls2(tf, header=TRUE)
View(data_3.1)
colnames(data_3.1) <- c("obs", "d_time", "n_case", "dista")

lmfit <- lm(d_time~n_case+dista)
scaled(lmfit)

residual_mat <- cbind(residuals(lmfit), scaled(lmfit), scaled(lmfit, "studentized"), scaled(lmfit, "rstudent"))
colnames(residual_mat) <- c("residual", "stadardized", "studentized", "rstudent")
head(residual_mat)

#pdf(paste(path2, "ex4_2_1.pdf", sep=""), width=13, height=6.75)
#qqplot:  Figure4.2
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
qqnorm(residuals(lmfit), main='Ordinary least-squares residuals')
qqline(residuals(lmfit), col=2, lwd=2)
qqnorm(scaled(lmfit, "studentized"), main='Studentized residuals')
qqline(scaled(lmfit, "studentized"), col=2, lwd=2)
title(main='Q-Q plots of residuals',line=-1,outer=T)
#dev.off()

#pdf(paste(path2, "ex4_2_2.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
fit_val <- fitted(lmfit)
plot(fit_val, residuals(lmfit), xlab="Fitted value", ylab="Residual", main="Original residuals")
abline(h=0, lty=1, col="grey")
plot(fit_val, scaled(lmfit, "rstudent"), xlab="Fitted value", ylab="Studentized residual", main="Studentized residuals")
abline(h=c(0,-2,2), lty=c(1,2,2), col="grey")
title(main='Residuals vs predicted for the delivery time data',line=-1,outer=T)
#dev.off()



#pdf(paste(path2, "ex4_2_3.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
fit_val <- fitted(lmfit)
plot(n_case, residuals(lmfit), xlab="Cases", ylab="Residual", main="Residuals vs cases")
abline(h=0, lty=1, col="grey")
plot(dista, residuals(lmfit), xlab="Distance", ylab="Residual", main="Residuals vs distance")
abline(h=0, lty=1, col="grey")
title(main='Residuals vs regressors for the delivery time data',line=-1,outer=T)
#dev.off()

#pdf(paste(path2, "ex4_2_4.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)#partial regression plot : Figure 4.7
plot(lm(n_case~dista)$resi,lm(d_time~dista)$resi,xlab='Cases',ylab='Time',
main='Time vs Cases',pch=16,cex=1.3)
plot(lm(dista~n_case)$resi,lm(d_time~n_case)$resi,xlab='Distance',ylab='Time',
main='Time vs Distance',pch=16,cex=1.3)
title(main='Partial regression plots for the delivery time data',line=-1,outer=T)
#dev.off()

partial <- function(model, part)
UseMethod("partial")

partial <- function(model, part){
	x <- model$model[, part]
	coeff <- model$coefficients[part]
	resi <- c(residuals(model) + x*coeff)
	return(resi)
}

#pdf(paste(path2, "ex4_2_5.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
plot(n_case, partial(lmfit, "n_case"), pch=16,cex=1.3, xlab='Cases',ylab='Time',main='Time vs Cases')
plot(dista, partial(lmfit, "dista"),pch=16,cex=1.3,
xlab='Distance',ylab='Time',main='Time vs Distance')
title(main='Partial residual plots for the delivery time data',line=-1,outer=T)
#dev.off()

#pdf(paste(path2, "ex4_2_6.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,1),pch=16,cex=1.4, mar=c(5,14,4,12)+0.1)
plot(dista,n_case,xlab='Distance',ylab='Cases',pch=16,main='Cases vs Distance')
#identify(dista ,n_case, 1:length(n_case))
text(dista[c(9,22)], n_case[c(9,22)], (1:length(n_case))[c(9,22)], pos=1)
title(main='Regressor vs regressor for the delivery time data',line=-1,outer=T)
#dev.off()

#pdf(paste(path2, "ex4_2_7.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,1),pch=16,cex=1.4, mar=c(5,14,4,12)+0.1)
i <- 1:length(d_time)
site <- ifelse(i<=7, "SD", ifelse(i<=17, "B", ifelse(i<=23, "A", "M")))
site <- factor(site)
stripchart(scaled(lmfit, "rstudent")~site, pch=16, vertical=T, cex=1.5, xlab='Site(city)',
ylab="R-student values")
title(main='R-student values by site(city) for the delivery time data')
#dev.off()



press <- function (obj) 
{
    sum((resid(obj)/(1 - hatvalues(obj)))^2)
}

press(lm(d_time ~ n_case))
press(lm(d_time ~ n_case + dista))



tf <- paste(tempfile(), "xls", sep = ".")
download.file(paste(url, "Dataset/data-ex-2-1.xls", sep=""), tf, method="curl")
data_2.1 <- read.xls2(tf, header=TRUE)
colnames(data_2.1) <- c("obs", "yi", "xi")
attach(data_2.1)

#pdf(paste(path2, "ex4_7_1.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,1),pch=16,cex=1.4, mar=c(5,14,4,12)+0.1)
plot(xi, yi, pch=19, xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)")
#dev.off()

lmfit <- lm(yi~xi)

#pdf(paste(path2, "ex4_7_2.pdf", sep=""), width=13, height=6.75)
#qqplot:  Figure4.2
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
qqnorm(residuals(lmfit), datax=TRUE, main='Q-Q plot')
qqline(residuals(lmfit), datax=TRUE, col=2, lwd=2)
#identify(sort(residuals(lmfit)), qnorm(1:length(xi)/length(xi)),  (1:length(xi))[order(residuals(lmfit))])
text(residuals(lmfit)[c(5,6)], qnorm(c(0.5,1.5)/length(xi)), c(5,6), pos=4)
fit_val <- fitted(lmfit)
plot(fit_val, scaled(lmfit, "rstudent"), xlab="Fitted value", ylab="Residual", main="Residuals vs Fitted values")
#identify(fit_val, scaled(lmfit, "rstudent"), 1:length(xi))
text(fit_val[5:6], scaled(lmfit, "rstudent")[5:6], 5:6, pos=3)
abline(h=0, lty=1, col="grey")

title(main='Residual plots for the rocket propellant data',line=-1,outer=T)
#dev.off()



lmfit <- lm(yi[-c(5,6)]~xi[-c(5,6)])

#pdf(paste(path2, "ex4_7_3.pdf", sep=""), width=13, height=6.75)
#qqplot:  Figure4.2
par(mfrow=c(1,2), cex.main=1.2, pch=19, cex=1.5)
qqnorm(residuals(lmfit), datax=TRUE, main='Q-Q plot')
qqline(residuals(lmfit), datax=TRUE, col=2, lwd=2)
#identify(sort(residuals(lmfit)), qnorm(1:length(xi)/length(xi)),  (1:length(xi))[order(residuals(lmfit))])
#text(residuals(lmfit)[c(5,6)], qnorm(c(0.5,1.5)/length(xi)), c(5,6), pos=4)
fit_val <- fitted(lmfit)
plot(fit_val, scaled(lmfit, "rstudent"), xlab="Fitted value", ylab="Residual", main="Residuals vs Fitted values")
abline(h=0, lty=1, col="grey")

title(main='Residual plots for the rocket propellant data',line=-1,outer=T)
#dev.off()

#pdf(paste(path2, "ex4_7_4.pdf", sep=""), width=13, height=6.75)
par(mfrow=c(1,1),pch=16,cex=1.4, mar=c(5,14,4,12)+0.1, cex.main=1.4)
lmfit <- lm(yi~xi)
plot(xi, yi, xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)")
abline(lmfit, col=2, lwd=2)
points(xi[5:6], yi[5:6], col="grey", cex=1.5, pch=19)
lmfit <- lm(yi[-c(5,6)]~xi[-c(5,6)])
abline(lmfit, col=2, lwd=2, lty=2)
legend("topright", legend=c("Full", "Obs 5, 6th are removed"), col=2, lty=1:2, lwd=2)
title(main="Treatment of outliers")
#dev.off()



x <- c(1,1,2,3.3,3.3,4,4,4,4.7,5,5.6,5.6,5.6,6,6,6.5,6.9)
y <- c(10.84,9.30,16.35,22.88,24.35,24.56,25.86,29.16,24.59,22.25,25.90,27.2,25.61,25.45,
26.56,21.03,21.46)

SSpe <- function(model, lof){  #SSpe function
	lmfit <- lm(model)
	y <- model.response(lmfit$model)
	x <- factor(lof)
	SSpe <- sum(xtabs(y^2~x)-xtabs(y~x)^2/table(x))
	SSres <- sum(residuals(lmfit)^2)
	SSlof <- SSres- SSpe
	out <- matrix(NA, 3, 5)
	colnames(out) <- c("Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)")
	rownames(out) <- c("SSlof", "SSpe", "SSres")
	out[,1] <- c(SSlof, SSpe, SSres)
	out[,2] <- c(length(levels(x))-2, length(x)-length(levels(x)), length(x)-2)
	out[1:2,3] <- out[1:2,1]/out[1:2,2]
	out[1,4] <- out[1,3]/out[2,3]
	out[1,5] <- pf(out[1,4], out[1,2], out[2,2], lower.tail=F)
	printCoefmat(out, digits=4, na.print="")
}

SSpe(y~x, x)

f1 <- lm(y ~ x) 
f2 <- lm( y ~ factor( x ) ) 
anova( f1, f2 )
