url <- "https://raw.github.com/dongikjang/regression/master/"
rfun <- getURL(paste(http, "read.xls2.r",sep=""))
eval(parse(text=rfun))
# If OS is Windows then install "xlsReadWrite" package
# If OS is Mac or Linux then install "gdata" package

library(RCurl)
tf <- paste(tempfile(), "xls", sep = ".")
download.file(paste(url, "Dataset/data-ex-3-1.xls", sep=""), tf, method="curl")
data_3.1 <- read.xls2(tf, header=TRUE)
View(data_3.1)
colnames(data_3.1) <- c("Observation", "Delivery Time(y)", "Number of Cases(x1)", "Distanc(x2)")



library(Hmisc)
latex(data_3.1[,-1], file="data_3.1.tex")

pdf(paste(path2, "figure_3_1.pdf", sep=""), width=9.4, height=6.75)
par(mar=c(4.5,5,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
pairs(data_3.1[,2:4], pch=19, cex=1.5)
dev.off()


pdf(paste(path2, "figure_3_1_2.pdf", sep=""), width=9.4, height=6.75)
library(lattice)	#need lattice package 
trellis.par.get('background')$col
trellis.par.set(theme=col.whitebg())
par(mar=c(4.5,7,5,2),cex.main=2, cex.lab=1.5, cex.axis=1.5)
cloud(data_3.1[,2]~data_3.1[,3]*data_3.1[,4], cex=1.5,
	  scales=list(col='blue', lty=2, cex=2),
	  screen=list(x=-90,y=-50,z=0),pch=16,
	  xlab=colnames(data_3.1)[3], 
	  ylab=colnames(data_3.1)[4],
	  zlab=colnames(data_3.1)[2])
dev.off()


library(rgl)
plot3d(x=data_2.1[,3], y=data_2.1[,4], z=data_2.1[,2], 
	   radius=20, type="s", col=2,
	   xlab=colnames(data_3.1)[3], 
	   ylab=colnames(data_3.1)[4],
	   zlab=colnames(data_3.1)[2])
rgl.postscript(paste(path2, "figure_3_1_3.pdf", sep=""),"pdf",drawText=TRUE)

nl <- colnames(data_3.1)
colnames(data_3.1) <- c("obs", "d_time", "n_case", "dista")
attach(data_3.1)
lmfit <- lm(d_time~n_case+dista)
(sfit <- summary(lmfit))
anova(lmfit)

anova2 <- function(x){
	fit <- anova(x)
	nrows <- nrow(fit)
	fit[1,1:2] <- apply(fit[1:(nrows-1),1:2], 2, sum)
	fit <- fit[-(2:(nrows-1)), ]
	fit[1,3] <- fit[1,2]/fit[1,1]
	fit[1,4] <- fit[1,3]/fit[2,3]
	rownames(fit)[1] <- "Regression"
	fit[1,5] <- pf(fit[1,4], fit[1,1], fit[2,1], lower.tail=FALSE)
	return(fit)
}


ta3.3 <- cbind(d_time, fitted(lmfit), residuals(lmfit))
colnames(ta3.3) <- c("y", "y_hat", "residual")
head(round(ta3.3, digits=2))


(summary(lmfit)$sigma)^2


(tss <- sum((d_time-mean(d_time))^2))	#sum of square total 
(sse <- deviance(lmfit)) #sum of square erro
(df.r <- df.residual(lmfit)) #n-p-1
p<-2 
(fstat <- ((tss-sse)/p)/(sse/df.r))  #F-statistics
pf(fstat, p, df.residual(lmfit), lower.tail=FALSE) #p-value
######################
redfit <- lm(d_time ~ n_case)	#Reduced Model (H0 : coefficient of dist =0) 
(sse1 <- deviance(redfit))	#SSE of Reduced Model 
(fstat <- (deviance(redfit)-deviance(lmfit))/(deviance(lmfit)/df.residual(lmfit)))
pf(fstat, 1, df.residual(lmfit), lower.tail=FALSE)
sqrt(fstat)
summary(lmfit)$coef
(tstat <- summary(lmfit)$coef[3,3]) 
2*pt(sqrt(fstat), df.residual(lmfit), lower.tail=FALSE)

anova(redfit, lmfit)

# (Ho: coefficient of n.case = coefficient of dist)
redfit2 <- lm(d_time ~ I(n_case + dista)) 
anova(redfit2, lmfit)


# (Ho : coefficient of dist =0.5)
redfit3 <- lm(d_time ~ n_case + offset(0.5*dista))
anova(redfit3, lmfit)
(tstat <- (summary(lmfit)$coef[3,1] - 0.5)/summary(lmfit)$coef[3,2])
2*pt(abs(tstat), df.residual(lmfit), lower.tail=FALSE)
tstat^2


summary(lmfit)$coef
sfit <- summary(lmfit) 
t.025 <- qt(0.975, df.residual(lmfit))
c(sfit$coef[2,1]-t.025*sfit$coef[2,2], sfit$coef[2,1]+t.025*sfit$coef[2,2])
confint(lmfit)
confint(lmfit, parm='dista', level = 0.95)


x0 <- c(1, 8, 275) 
(y0 <- sum(x0*coef(lmfit)))
t.025<-qt(0.975,df.residual(lmfit)) 
x <- model.matrix(lmfit) 
xtxi <- solve(t(x) %*% x) 
bm <- sqrt(x0 %*% xtxi %*% x0) *t.025 * summary(lmfit)$sigma 
c(y0-bm, y0+bm)
predict(lmfit, data.frame(n_case=8,dista=275), interval="confidence")


bm <- sqrt(1+x0 %*% xtxi %*% x0) *t.025 * summary(lmfit)$sigma 
c(y0-bm, y0+bm)
x0 <- data.frame(n_case=8, dista=275) 
str(predict(lmfit, x0, se=TRUE))
predict(lmfit, x0, interval="confidence")
predict(lmfit, x0, interval="prediction")

pdf(paste(path2, "figure_3_4.pdf", sep=""), width=9.4, height=6.75)
library(ellipse) #need ellipse package 
par(cex.lab=1.5)
plot(ellipse(lmfit, c(2,3)), type="l", lwd=2, xlab="Number of Cases(x1)", ylab="Distanc(x2)") 
points(coef(lmfit)[2], coef(lmfit)[3], pch=19, col=2, cex=2) 
abline(v=confint(lmfit)[2,], lty=2, col=4, lwd=2) 
abline(h=confint(lmfit)[3,], lty=2, col=4, lwd=2)
dev.off()



persp3d(regx, regy, regz, normal_x=regx, normal_y=regy, normal_z=regz, col="grey")
points3d(n_case, dista, d_time, col="red")

x <- n_case/diff(range(n_case)) 
y <- dista/diff(range(dista)) 
z <- d_time/diff(range(d_time)) 
regx  <- seq(-0.1, 1.1, len=100)
regy  <- seq(-0.1, 1.1, len=100)
xy <- expand.grid(regx, regy)
x0 <- data.frame(x=xy[,1], y=xy[,2]) 
lmfit0 <- lm(z ~ x+y)
regz <- predict(lmfit0, x0, interval="prediction")[,1]
lx <- c(-0.1, 1.1, 1.1, -0.1)
ly <- c(-0.1, -0.1, 1.1, 1.1)
lz <- predict(lmfit0, data.frame(x=lx, y=ly) , interval="prediction")[,1]
rgl.bg(sphere = FALSE, color = c("white", "green"), lit = FALSE, size=2, alpha=0.2, back = "lines")
rgl.light()
rgl.bbox()
rgl.spheres(x, y, z, col="red", color="gray",radius=0.05,
			specular="green",texmipmap=T, texminfilter="linear.mipmap.linear",
			axes=FALSE)
rgl.surface(regx, regy, regz, col="grey",color="blue",alpha=0.5,shininess=128)
rgl.quads(lx, lz, ly,color="red",size=5,front="lines",back="lines",lit=F)

detach(data_3.1)