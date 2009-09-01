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

getFdsmooth <- function(data, smoothing=10) {
	cubic.basis <- getBasis(malaria.fda.df$pt)
	cubic.fdPar <- fdPar(cubic.basis, 2, smoothing)
	# data <- as.matrix(malaria.fda.df$lparct)
	smooth.basis(malaria.fda.df$pt, data, cubic.fdPar)
}

misc <- function() {
	predose <- malaria.fda.df$prr[1,]
	malaria.fda.df$prr <- sweep(malaria.fda.df$prr, 2, predose, "/")
}

getxfdlist <- function() {
	pt.cbasis <- create.constant.basis(range(malaria.fda.df$pt))
	constfd=fd(matrix(1,1,43),pt.cbasis)
	Sexfd=fd(matrix(as.numeric(malaria.fda.df$Sex),1,43),pt.cbasis)
	Treatmentfd=fd(matrix(as.numeric(malaria.fda.df$Treatment),1,43),pt.cbasis)
	list(constfd, Sexfd, Treatmentfd)
}

getbetalist <- function(fd, smoothing=10) {
	 list(fdPar(fd$basis, 2, smoothing), fdPar(fd$basis, 2, smoothing), fdPar(fd$basis, 2, smoothing))
}

getfRegress <- function(data.smooth, smoothing=10) {
	fd <- data.smooth$fd
	xfdlist <- getxfdlist()
	betalist <- getbetalist(fd, smoothing)
	fRegress(fd, xfdlist, betalist, y2cMap=data.smooth$y2cMap)
}

getSigmaE <- function(fit, data) {
	errmat <- data - eval.fd(malaria.fda.df$pt, fit$yhatfdobj$fd)
	errmat %*% t(errmat) / 43
}

getfStderr <- function(fit, data) {
	sigmaE <- getSigmaE(fit, data)
	fRegress.stderr(fit, fit$y2cMap, sigmaE)
}

getFperm <- function(data.smooth, smoothing=10) {
	fd <- data.smooth$fd
	xfdlist <- getxfdlist()
	betalist <- getbetalist(fd, smoothing)
	Fperm.fd(fd, xfdlist, betalist)
}

getfResid <- function(fit, data) {
	errmat <- data - eval.fd(malaria.fda.df$pt, fit$yhatfdobj$fd)
	errmat.df <- data.frame(pt=malaria.fda.df$pt, as.data.frame(errmat))
	errmat.long <- reshape(errmat.df, direction='long', varying=list(2:44))
	names(errmat.long)[2:3] <- c("Subject","e")
	errmat.long
}

plotfSmooth <- function() {
	qplot(pt, y, data=lprr2.fdSmooth.long, colour=Treatment, linetype=Sex, group=Subject, geom='line', stat='smooth', xlab="Time (hours)", ylab="log ratio of parasite count to pre-dose", main="Cubic spline smoothing")
}

plotfresids <- function() {
	q <- qplot(pt, sr, data=lprr2.stderr.long, colour=Treatment, linetype=Sex, group=Subject, geom='line', stat='smooth', xlab="Time (hours)", ylab="Standardized residual", main="Residual functions from ANOVA")
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
