pc50boxes <- function(data) {
	vp1 <- viewport(width=1, height=0.5, y=1, just="top")
	q <- qplot(Level, PC50, data=data, geom="blank", colour=Factor, xlab="", ylab="PC50 (hours)")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q1 <- q + stat_summary(fun.data="median_hilow", conf.int=0.5, geom="crossbar", width=0.5)
	q1 <- q1 + opts(title="Median and quartiles")
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.5, y=0, just="bottom")
	q2 <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	q2 <- q2 + opts(title="95% normal confidence interval for mean")
	print(q2, vp=vp2)

#	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
#	q3 <- q + stat_summary(fun.data="mean_cl_boot", geom="crossbar", width=0.5)
#	q3 <- q3 + opts(title="Non-parametric bootstrap")
#	q3 <- q3 + opts(legend.position="none")
#	print(q3, vp=vp3)
}

pc50both <- function(data1, data2) {
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q <- qplot(Level, PC50, data=data1, geom="blank", colour=Factor, xlab="95% normal confidence interval for mean", ylab="PC50 (hours)")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q1 <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	print(q1, vp=vp1)

	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(Treatment, PC50, data=data2, geom="blank", colour=Treatment, xlab="Treatment:Centre", ylab="PC50 (hours)")
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q2 <- q2 + opts( legend.position="none")
	q2 <- q2 + facet_grid(.~Centre)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(Treatment, PC50, data=data2, geom="blank", colour=Treatment, xlab="Treatment:Sex", ylab="PC50 (hours)")
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~Sex)
	print(q3, vp=vp3)
}

pc99both <- function(data1, data2) {
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q <- qplot(Level, PC99, data=data1, geom="blank", colour=Factor, xlab="95% normal confidence interval for mean", ylab="PC99 (hours)")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q1 <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	print(q1, vp=vp1)

	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(Treatment, PC99, data=data2, geom="blank", colour=Treatment, xlab="Treatment:Centre", ylab="PC99 (hours)")
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q2 <- q2 + opts( legend.position="none")
	q2 <- q2 + facet_grid(.~Centre)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(Treatment, PC99, data=data2, geom="blank", colour=Treatment, xlab="Treatment:Sex", ylab="PC99 (hours)")
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~Sex)
	print(q3, vp=vp3)
}

pc50interaction <- function(data) {
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q1 <- qplot(Sex, PC50, data=data, geom="blank", colour=Sex, xlab="Sex:Centre", ylab="PC50 (hours)")
	q1 <- q1 + scale_colour_discrete(h.start=120)
	q1 <- q1 + geom_point(position=position_jitter(w=0.1))
	q1 <- q1 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q1 <- q1 + opts( legend.position="none")
	q1 <- q1 + facet_grid(.~Centre)
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(Treatment, PC50, data=data, geom="blank", colour=Treatment, xlab="Treatment:Centre", ylab="PC50 (hours)")
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q2 <- q2 + opts( legend.position="none")
	q2 <- q2 + facet_grid(.~Centre)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(Treatment, PC50, data=data, geom="blank", colour=Treatment, xlab="Treatment:Sex", ylab="PC50 (hours)")
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~Sex)
	print(q3, vp=vp3)
}

pc99boxes <- function(data) {
	vp1 <- viewport(width=1, height=0.5, y=1, just="top")
	q <- qplot(Level, PC99, data=data, geom="blank", colour=Factor, xlab="", ylab="PC99 (hours)")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q1 <- q + stat_summary(fun.data="median_hilow", conf.int=0.5, geom="crossbar", width=0.5)
	q1 <- q1 + opts(title="Median and quartiles")
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.5, y=0, just="bottom")
	q2 <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	q2 <- q2 + opts(title="95% normal confidence interval for mean")
	print(q2, vp=vp2)

#	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
#	q3 <- q + stat_summary(fun.data="mean_cl_boot", geom="crossbar", width=0.5)
#	q3 <- q3 + opts(title="Non-parametric bootstrap")
#	q3 <- q3 + opts(legend.position="none")
#	print(q3, vp=vp3)
}

pc99interaction <- function(data) {
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q1 <- qplot(Sex, PC99, data=data, geom="blank", colour=Sex, xlab="Sex:Centre", ylab="PC99 (hours)")
	q1 <- q1 + scale_colour_discrete(h.start=120)
	q1 <- q1 + geom_point(position=position_jitter(w=0.1))
	q1 <- q1 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q1 <- q1 + opts( legend.position="none")
	q1 <- q1 + facet_grid(.~Centre)
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(Treatment, PC99, data=data, geom="blank", colour=Treatment, xlab="Treatment:Centre", ylab="PC99 (hours)")
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q2 <- q2 + opts( legend.position="none")
	q2 <- q2 + facet_grid(.~Centre)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(Treatment, PC99, data=data, geom="blank", colour=Treatment, xlab="Treatment:Sex", ylab="PC99 (hours)")
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~Sex)
	print(q3, vp=vp3)
}

prr24.plot <- function(data) {
	q <- qplot(Treatment, log(PRR24), data=data, geom="blank", colour=Treatment, xlab="95% normal confidence interval for mean", ylab="log PRR24")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	q + facet_grid(.~Sex)
}

prr12.plot <- function(data) {
	q <- qplot(Treatment, log(PRR12), data=data, geom="blank", colour=Treatment, xlab="95% normal confidence interval for mean", ylab="log PRR12")
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + opts(legend.position="none")
	q <- q + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.5)
	q + facet_grid(.~Sex)
}


getBasis <- function(knots, norder=4) {
	nbasis <- length(knots) + norder - 2
	create.bspline.basis(range(knots), nbasis, norder, knots)
}

getFdsmooth <- function(x, y, smoothing=10) {
	# x <- malaria.fda.df$pt
	cubic.basis <- getBasis(x)
	cubic.fdPar <- fdPar(cubic.basis, 2, smoothing)
	# y <- as.matrix(malaria.fda.df$lparct)
	smooth.basis(x, y, cubic.fdPar)
}

misc <- function() {
	predose <- malaria.fda.df$prr[1,]
	malaria.fda.df$prr <- sweep(malaria.fda.df$prr, 2, predose, "/")
}

getxfdlist <- function() {
	pt.cbasis <- create.constant.basis(range(malaria.fda.df$pt))
	constfd=fd(matrix(1, 1, 43), pt.cbasis)
	Sexfd=fd(matrix(as.numeric(malaria.fda.df$Sex) - 1, 1, 43), pt.cbasis)
	Treatmentfd=fd(matrix(as.numeric(malaria.fda.df$Treatment) - 1, 1, 43), pt.cbasis)
	Interactionfd=fd(matrix((as.numeric(malaria.fda.df$Sex) - 1)*(as.numeric(malaria.fda.df$Treatment) -1), 1, 43), pt.cbasis)
	list(constfd, Sexfd, Treatmentfd, Interactionfd)
}

getbetalist <- function(fd, smoothing=10) {
	betaifdPar <- fdPar(fd$basis, 2, smoothing)
	list(betaifdPar, betaifdPar, betaifdPar, betaifdPar)
}

getfRegress <- function(data.smooth, smoothing=10) {
	fd <- data.smooth$fd
	x <- data.smooth$argvals
	y <- data.smooth$y
	xfdlist <- getxfdlist()
	betalist <- getbetalist(fd, smoothing)
	fr <- fRegress(fd, xfdlist, betalist, y2cMap=data.smooth$y2cMap)
	fr$SigmaE <- getSigmaE(fr, x, y)
	fr$betastderrlist <- getfStderr(fr, x, y)$betastderrlist
	fr
}

getSigmaE <- function(fit, x, y) {
	errmat <- y - eval.fd(x, fit$yhatfdobj$fd)
	errmat %*% t(errmat) / dim(y)[2]
}

getfStderr <- function(fit, x, y) {
	sigmaE <- getSigmaE(fit, x, y)
	fRegress.stderr(fit, fit$y2cMap, sigmaE)
}

getFperm <- function(data.smooth, smoothing=10) {
	fd <- data.smooth$fd
	xfdlist <- getxfdlist()
	betalist <- getbetalist(fd, smoothing)
	Fperm.fd(fd, xfdlist, betalist)
}

getfResid <- function(fit, data.smooth) {
	x <- data.smooth$argvals
	n <- dim(data.smooth$y)[2]
	errmat <- data.smooth$y - eval.fd(x, fit$yhatfdobj$fd)
	resids.df <- data.frame(pt=x, as.data.frame(errmat))
	p <- n + 1
	resids.long <- reshape(resids.df, direction='long', varying=list(2:p))
	names(resids.long)[2:3] <- c("Subject","e")
	resids.long$s <- rep(sqrt(diag(fit$SigmaE)), n)
	resids.long$sr <- with(resids.long, e/s)
	resids.long$Sex <- PC90.df$Sex[1]
	resids.long$Treatment <- PC90.df$Treatment[1]
	for (i in 1:n) {
		resids.long$Sex[resids.long$Subject==i] <- PC90.df$Sex[i]
		resids.long$Treatment[resids.long$Subject==i] <- PC90.df$Treatment[i]
	}
	resids.long
}

plotfSmooth <- function() {
	qplot(pt, y, data=lprr2.fdSmooth.long, colour=Treatment, linetype=Sex, group=Subject, geom='line', stat='smooth', xlab="Time (hours)", ylab="log ratio of parasite count to pre-dose", main="Cubic spline smoothing")
}

plotfresids <- function(resids.df) {
	q <- qplot(pt, sr, data=resids.df, colour=Treatment, linetype=Sex, group=Subject, geom='line', stat='smooth', xlab="Time (hours)", ylab="Standardized residual", main="Residual functions from ANOVA")
	q + geom_hline(yintercept=0, lty=2)
}

plotbetas <- function(fit) {
	plot(fit$betaestlist[[1]]$fd,col=1,lwd=2, ylim=c(-10,6), xlab="Time (hours)", ylab="log ratio of parasite count to pre-dose", main="Fitted coefficients for ANOVA model")
	lines(fit$betaestlist[[1]]$fd+2*fit$betastderrlist[[1]],col=1,lwd=1,lty=2)
	lines(fit$betaestlist[[1]]$fd-2*fit$betastderrlist[[1]],col=1,lwd=1,lty=2)
	lines(fit$betaestlist[[2]]$fd,col=2,lwd=2)
	lines(fit$betaestlist[[2]]$fd+2*fit$betastderrlist[[2]],col=2,lwd=1,lty=2)
	lines(fit$betaestlist[[2]]$fd-2*fit$betastderrlist[[2]],col=2,lwd=1,lty=2)
	lines(fit$betaestlist[[3]]$fd,col=4,lwd=2)
	lines(fit$betaestlist[[3]]$fd+2*fit$betastderrlist[[3]],col=4,lwd=1,lty=2)
	lines(fit$betaestlist[[3]]$fd-2*fit$betastderrlist[[3]],col=4,lwd=1,lty=2)
	legend(0, -6, lty=c(1,1,1,2), col=c(1,2,4,1), legend=c("Mean (Male, single)","Female","Combined treatment","95% CIs"))
}

plotfdcoefs <- function(fit, nx=201) {
	rngx <- fit$yfdPar$fd$basis$range
	x <- seq(rngx[1], rngx[2], length=nx)
	coef.names <- c("Mean (male, single)", "Female, single", "Male, combi", "Female, combi")
	plot.df <- data.frame()
	for (i in 1:4) {
		y <- eval.fd(x, fit$betaestlist[[i]]$fd)
		s <- eval.fd(x, fit$betastderrlist[[i]])[,1]
		plot.df <- rbind(plot.df, data.frame(x=x, y=y, upper=y+1.96*s, lower=y-1.96*s, coef=coef.names[i]))
	}
	q <- qplot(x, y, data=plot.df, geom='line', colour=coef, size=1, xlab="Time (hours)", ylab="log ratio of parasite count to pre-dose", main="Fitted coefficient functions with 95% confidence intervals")
	q <- q + geom_line(aes(y=upper), linetype=2) + geom_line(aes(y=lower), linetype=2)
	q <- q + geom_hline(yintercept=0, linetype=2)
	q + facet_wrap(~coef, ncol=2) + opts(legend.position='none')
}

plotFperm <- function(fperm, x, y1, y2) {
	time <- fperm$argvals
	q <- qplot(time, fperm$Fvals, colour='red', size=1, geom='line', xlab='Time (hours)', ylab='F-statistic', main='Permutation F-test')
	q <- q + geom_line(aes(y=fperm$qvals.pts), linetype=2, colour='blue', size=0.5)
	q <- q + geom_hline(yintercept=fperm$qval, linetype=2, colour='blue', size=1)
	q <- q + geom_text(aes(x=x, y=y1, label="maximum 0.05 critical value"), size=4, colour='blue')
	q <- q + geom_text(aes(x=x, y=y2, label="pointwise 0.05 critical value"), size=4, colour='blue')
	q + opts(legend.position='none')
}
