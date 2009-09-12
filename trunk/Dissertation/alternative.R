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

extrapplot <- function() {
	data <- with(malaria, malaria[SUBJID==98 | SUBJID==285,])
	extrap.df <- data.frame(SUBJID=c(98,98,285,285))
	for (s in unique(data$SUBJID)) {
		dat <- data[data$SUBJID==s,]
		l <- nrow(dat)
		fit <- lm(log(1 + parct) ~ acttm, data=dat, subset=c(rep(F, l-2), c(T, T)))
		newdat <- data.frame(acttm=c(dat$acttm[l], ifelse(s==98, 55, 85)))
		extrap.df$acttm[extrap.df$SUBJID==s] <- newdat$acttm
		extrap.df$parct[extrap.df$SUBJID==s] <- predict(fit, newdata=newdat)
		pc99 <- log(1 + (0.01 * dat$pre[1]))
		print((pc99 - coef(fit)[1])/coef(fit)[2])
	}
	minlim.df <- data.frame(SUBJID=c(98,285), x=c(48.0833, 48.1))
	q <- rawggplot(data)
	q <- getlogplot(q)
	q <- addPClines(q, percentage=99, logplot=T, xpos=10)
	q <- q + geom_line(data=extrap.df, linetype=2, colour="blue")
	q <- q + geom_vline(aes(xintercept=x), data=minlim.df, linetype=2, colour="blue")
	q + scale_y_continuous(limits=c(4,12)) + opts(legend.position="none") + facet_wrap(~SUBJID, scales="free") + opts(title="PC99 estimation beyond last datum")
}

plot477 <- function() {
	q <- rawggplot(malaria[malaria$SUBJID==477,])
	q <- getlogplot(q)
	q <- addPClines(q, percentage=99, logplot=T, xpos=10)
	q + opts(legend.position="none", title="Estimation of PC99 for subject 477")
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


# Get spline basis, cubic by default
getBasis <- function(knots, norder=4) {
	require(fda)
	nbasis <- length(knots) + norder - 2
	create.bspline.basis(range(knots), nbasis, norder, knots)
}

# Get spline smoothed functional data
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

# Get factor fdas for regression
getxfdlist <- function() {
	pt.cbasis <- create.constant.basis(range(malaria.fda.df$pt))
	constfd=fd(matrix(1, 1, 43), pt.cbasis)
	Sexfd=fd(matrix(as.numeric(malaria.fda.df$Sex) - 1, 1, 43), pt.cbasis)
	Treatmentfd=fd(matrix(as.numeric(malaria.fda.df$Treatment) - 1, 1, 43), pt.cbasis)
	Interactionfd=fd(matrix((as.numeric(malaria.fda.df$Sex) - 1)*(as.numeric(malaria.fda.df$Treatment) -1), 1, 43), pt.cbasis)
	list(constfd, Sexfd, Treatmentfd, Interactionfd)
}

# Get paramater fdas for regression
getbetalist <- function(fd, smoothing=10) {
	betaifdPar <- fdPar(fd$basis, 2, smoothing)
	list(betaifdPar, betaifdPar, betaifdPar, betaifdPar)
}

# Perform regression
getfRegress <- function(data.smooth, smoothing=10) {
	require(fda)

	fd <- data.smooth$fd
	x <- data.smooth$argvals
	y <- data.smooth$y

	# Get the factors as a list of fda objects
	xfdlist <- getxfdlist()
	# Get the coefficient fdas to be fitted
	betalist <- getbetalist(fd, smoothing)

	# Do the regression and add the standard errors for the coefficients
	fr <- fRegress(fd, xfdlist, betalist, y2cMap=data.smooth$y2cMap)
	fr$SigmaE <- getSigmaE(fr, x, y)
	fr$betastderrlist <- getfStderr(fr, x, y)$betastderrlist
	fr
}

getfRegress2 <- function(fd, x, smoothing=10) {
	y <- eval.fd(x, fd)
	xfdlist <- getxfdlist()
	betalist <- getbetalist(fd, smoothing)
	fr <- fRegress(fd, xfdlist, betalist)
	#fr$SigmaE <- getSigmaE(fr, x, y)
	#fr$betastderrlist <- getfStderr(fr, x, y)$betastderrlist
	fr
}

# Get var-covar matrix
getSigmaE <- function(fit, x, y) {
	yobs <- eval.fd(x, fit$yhatfdobj$fd)
	e <- y - yobs
	e %*% t(e) / dim(y)[2]
}

# Get the standard error of the coefficients
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

# Get the standardized residuals for fda regression
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
	resids.long$Sex <- factor("Male","Female")
	resids.long$Treatment <- factor("alone","combi")
	for (i in 1:n) {
		resids.long$Sex[resids.long$Subject==i] <- PC90.df$Sex[PC90.df$Subject==i]
		resids.long$Treatment[resids.long$Subject==i] <- PC90.df$Treatment[PC90.df$Subject==i]
	}
	resids.long
}

plotfSmooth <- function(data.long=lprr2.fdSmooth.long) { 
	qplot(pt, y, data=data.long, colour=Treatment, linetype=Sex, group=Subject, geom='line', stat='smooth', xlab="Time (hours)", ylab="log ratio of parasite count to pre-dose", main="Cubic spline smoothing")
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

# Plot coefficients from fda regression with CIs
plotfdcoefs <- function(fit, nx=201) {
	require(fda)
	require(ggplot2)
	rngx <- fit$yfdPar$fd$basis$range
	x <- seq(rngx[1], rngx[2], length=nx)
	coef.names <- c("Mean (male, alone)", "Sex (female)", "Treatment (combi)", "Sex:Treatment (female, combi)")
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

# Plot F statistic fda
plotFperm <- function(fperm, x, y1, y2) {
	require(ggplot2)
	time <- fperm$argvals
	q <- qplot(time, fperm$Fvals, colour='red', size=1, geom='line', xlab='Time (hours)', ylab='F-statistic', main='Permutation F-test')
	q <- q + geom_line(aes(y=fperm$qvals.pts), linetype=3, colour='blue')
	q <- q + geom_hline(yintercept=fperm$qval, linetype=2, colour='blue')
	q <- q + geom_text(aes(x=x, y=y1, label="maximum 0.05 critical value"), size=4, colour='blue')
	q <- q + geom_text(aes(x=x, y=y2, label="pointwise 0.05 critical value"), size=4, colour='blue')
	q + opts(legend.position='none')
}

getPredxfdlist <- function() {
	pt.cbasis <- create.constant.basis(range(malaria.fda.df$pt))
	constfd=fd(matrix(1, 1, 4), pt.cbasis)
	Sexfd=fd(matrix(c(0,1,0,1), 1, 4), pt.cbasis)
	Treatmentfd=fd(matrix(c(0,0,1,1), 1, 4), pt.cbasis)
	Interactionfd=fd(matrix(c(0,0,0,1), 1, 4), pt.cbasis)
	list(constfd, Sexfd, Treatmentfd, Interactionfd)
}

model.matrix.fRegress <- function(obj) {
	sapply(obj$xfdlist, function(x) x$coefs)	
	
}

# Get data for plotting predictions and CIs over time range
getfdpreddata <- function(fit, nt=201, level.names=c("Male, alone", "Female, alone", "Male, combi", "Female, combi")) {
	rngt <- fit$yfdPar$fd$basis$range
	t <- seq(rngt[1], rngt[2], length=nt)
	data.df <- data.frame(getfdpred(t[1], fit))
	for (i in 2:nt)
		data.df <- rbind(data.df, getfdpred(t[i], fit))
	data.df$level <- rep(level.names, nt)
	data.df
}

# Calculate confidence intervals for fitted fda mean function
getfdpred <- function(t, fit, conf=0.95) {
	require(fda)

	# Get matrix of factor levels for each sex/treatment group
	x <- t(sapply(getPredxfdlist(), function(x) x$coef))
	# Get vector of coefficients
	Beta <- as.matrix(sapply(fit$betaestlist, function(x) eval.fd(t, x$fd)))
	# Get fitted values
	pred <- t(x) %*% Beta

	# Calculate residuals and residual variance
	yobs <- eval.fd(fit$yfdPar$fd, t)
	yfit <- eval.fd(fit$yhatfdobj$fd, t)
	e <- yobs - yfit
	n <- length(e)
	p <- length(Beta)
	s2 <- e %*% t(e) / (n - p)

	# Calculate variance of predictions
	X <- model.matrix(fit)
	varpred <- as.numeric(s2) * t(x) %*% solve(t(X) %*% X) %*% x

	# Return predictions at times specified with CIs specified
	pval <- (1 - conf) / 2
	limit <- sqrt(diag(varpred)) * abs(qt(pval, n - p))
	matrix(c(rep(t, p), pred, pred-limit, pred+limit), p, p,
		dimnames=list(NULL, c("t", "pred", "lwr", "upr")))
}

# Plot fitted function with CIs
plotfdpred <- function(data) {
	require(ggplot2)
	q <- qplot(t, pred, data=data, ymin=lwr, ymax=upr, geom='errorbar', colour=Treatment, xlab="Time (hours)", ylab="log ratio of parasite count to predose", main="95% confidence intervals for functional model")
	#q + geom_line(aes(y=lwr), linetype=2) + geom_line(aes(y=upr), linetype=2)
	#q + geom_errorbar(aes(ymin=lwr, ymax=upr, colour=Treatment))
	q <- q + geom_line(size=1)
	q + facet_grid(~Sex)
}
