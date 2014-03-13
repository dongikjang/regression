read.xls2 <- function(file, header=TRUE){
	os <- .Platform$OS.type
	if(os=="windows"){
		if(any(loadedNamespaces()=="gdata")) detach(package:gdata)
		require(xlsReadWrite)
		tf <- paste(tempfile(), "xls", sep = ".")
		download.file(file, tf, mode = "wb")
		out <- read.xls(tf, colNames=header)
	}else if(os=="unix"){
		if(any(loadedNamespaces()=="xlsReadWrite")) detach(package:xlsReadWrite)
		require(gdata)
		out <- read.xls(file, header=header)
	}else{
		warning("Only Mac and Windows")	
	}
	return(out)
}

url <- "https://raw.github.com/dongikjang/regression/master/"
rfun <- getURL(paste(http, "read.xls2.r",sep=""))
eval(parse(text=rfun))
# If OS is Windows then install "xlsReadWrite" package
# If OS is Mac or Linux then install "gdata" package

library(RCurl)
tf <- paste(tempfile(), "xls", sep = ".")
download.file(paste(url, "Dataset/data-ex-2-1.xls", sep=""), tf, method="curl")
data_2.1 <- read.xls2(tf, header=TRUE)


#library(gdata)
#data_2.1 <- read.xls(tf, header=TRUE)

#For Windows
#library(xlsReadWrite)
#data_2.1 <- read.xls(tf)

#library(RODBC)
#channel <- odbcConnectExcel(tf)
#data_2.1 <- sqlFetch(channel, "Sheet1")

#data_2.1 <- read.table("clipboard", header=TRUE, sep="\t")

library(Hmisc)
latex(data_2.1, file="data_2.1.tex")

#pdf(paste(path2, "figure_2_1.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(data_2.1[,3:2], xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)", pch=19, cex=1.5)
title(main="Figure 2.1", line=3)
par(cex.main=1.5)
title(main="Scatter diagram of shear shrength vs prepellant age", line=1)
abline(lm(data_2.1[,2:3]), col=2, lty=2, lwd=2)
legend("topright", legend="linear regression line", lty=2, col=2, lwd=2)
#dev.off()


colnames(data_2.1) <- c("obs", "yi", "xi")
attach(data_2.1)
lmfit <- lm(yi~xi)
lmfit$coefficients
lmfit$residuals
lmfit$fitted.valus

sfit <-summary(lmfit)
names(sfit)

fit<-fitted(lmfit)
res <-residuals(lmfit)
ta2.2 <- cbind(xi,fit,res)

#pdf(paste(path2, "figure_2_1_res.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(xi, res,  xlab="Age of Propellant(weeks)", ylab="Residuals", main="Residuals plot", pch=19, cex=2)
abline(h=0, col=2, lty=2, lwd=2)
#dev.off()

#pdf(paste(path2, "figure_2_1_res2.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(yi, res,  xlab="Shear Strength(psi)", ylab="Residuals", main="Residuals plot", pch=19, cex=2)
abline(h=0, col=2, lty=2, lwd=2)
#dev.off()

anova(lmfit)

x <- model.matrix( ~ xi) #design matrix
y <- yi
xtxi <- solve(t(x) %*% x)
xtxi %*% t(x) %*% y
solve(crossprod(x,x),crossprod(x,y))
lmfit$coefficients
coefficients(lmfit)


sqrt(sum(lmfit$res^2)/df.residual(lmfit))
sqrt(deviance(lmfit)/df.residual(lmfit))
sfit$sigma

xtxi <- sfit$cov.unscaled  #solve(t(x) %*% x)
sqrt(diag(xtxi))*sfit$sigma
sfit$coef[,2]

1-deviance(lmfit)/sum((y-mean(y))^2)  # deviance is Sum of Square Residual
sfit$r.squared
num <- (deviance(lmfit)/df.residual(lmfit))
den <-(sum((y-mean(y))^2)/(length(y)-1))
1- num/den
sfit$adj.r.squared


t.025<-qt(0.975,df.residual(lmfit))
l.beta1<-coefficients(lmfit)[2]-t.025*sfit$coef[2,2]
u.beta1<-coefficients(lmfit)[2]+t.025*sfit$coef[2,2]
c(l.beta1, u.beta1)

sigma2<-anova(lmfit)[2,3]
q.025<-qchisq(.975,df.residual(lmfit))
q.975<-qchisq(.025,df.residual(lmfit))
l.sigma2<-df.residual(lmfit)*sigma2/q.025
u.sigma2<-df.residual(lmfit)*sigma2/q.975
c(l.sigma2,u.sigma2)

x0<-13.3625
x0<- model.matrix( ~x0)
err<-t.025*sfit$sigma*sqrt(x0%*%xtxi%*%t(x0))  #t.025<-qt(0.975,df.residual(lmfit))
c(coefficients(lmfit)%*%t(x0)-err,coefficients(lmfit)%*%t(x0)+err)
predict(lmfit, data.frame(xi=13.3625), interval="confidence")
x1<-seq(0, 25, length=100)
interval.est<-predict(lmfit, data.frame(xi=x1), interval="confidence")
head(interval.est)


#pdf(paste(path2, "figure_2_5.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(xi, yi ,pch=19,xlim=c(0,25),ylim=c(1500,3000),xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)", cex=1.5)
title(main="Figure 2.5", line=3)
par(cex.main=1.5)
title(main="Interval estimation of mean response", line=1)
abline(lmfit)
lines(x1,interval.est[,2],lty=2,col=2,lwd=2)
lines(x1,interval.est[,3],lty=2,col=2,lwd=2)
#dev.off()


err1 <- t.025*sfit$sigma*sqrt(1+x0%*%xtxi%*%t(x0))
c(coefficients(lmfit)%*%t(x0)-err1,coefficients(lmfit)%*%t(x0)+err1)
predict(lmfit,data.frame(xi=13.3625),interval="prediction")

x1<-seq(0, 25, length=100)
pred <- predict(lmfit, data.frame(xi=x1), interval="prediction")

#pdf(paste(path2, "figure_2_5_2.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(xi, yi ,pch=19,xlim=c(0,25),ylim=c(1500,3000),xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)", cex=1.5)
title(main="Figure 2.5", line=3)
par(cex.main=1.5)
title(main="Prediction of new observation", line=1)
abline(lmfit)
lines(x1,pred[,2], lty=3, col=4, lwd=3)
lines(x1,pred[,3], lty=3, col=4, lwd=3)
#dev.off()


#pdf(paste(path2, "figure_2_5_3.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(xi, yi ,pch=19,xlim=c(0,25),ylim=c(1500,3000),xlab="Age of Propellant(weeks)", ylab="Shear Strength(psi)", cex=1.5)
title(main="Figure 2.5", line=3)
par(cex.main=1.5)
title(main="Interval vs Prediction", line=1)
abline(lmfit)

lines(x1,interval.est[,2],lty=2,col=2,lwd=3)
lines(x1,interval.est[,3],lty=2,col=2,lwd=3)
lines(x1,pred[,2], lty=3, col=4, lwd=3)
lines(x1,pred[,3], lty=3, col=4, lwd=3)
legend("bottomleft", legend=c('interval','prediction') ,lty=c(2,3),col=c(2,4),lwd=3, cex=1.5)
#dev.off()




detach(data_2.1)

