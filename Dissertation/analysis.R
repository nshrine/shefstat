pc90aov <- function(data, title="", ...) {
	q <- qplot(Treatment, PC90.logistic, data=data, xlab="", ylab="PC90 (hours)", main=title, geom="blank", facets=Centre~Sex, margins=T)
	q + stat_boxplot(aes(fill=Treatment), width=0.3)
}

pc90aov.fun <- function(data, title="", fun="mean_cl_boot", ...) {
	q <- qplot(Treatment, PC90.logistic, data=data, xlab="", ylab="PC90 (hours)", main=title, geom="point", position=position_jitter(w=0.1), facets=Centre~Sex, margins=T, colour=Treatment)
	q + stat_summary(fun.data=fun, geom="crossbar", width=0.3)
}

pc90ancova <- function(data, title="", ...) {
	q <- qplot(acttm, PC90.loglin, data=data, xlab="", ylab="PC90 (hours)", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q + facet_grid(CENTREID~SEX, margins=T, scales="free")
}

pc90boxes <- function(data) {
	vp1 <- viewport(width=1, height=0.5, y=1, just="top")
	q <- qplot(Level, PC90, data=data, geom="blank", colour=Factor, xlab="", ylab="PC90 (hours)")
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

pc90interaction <- function(data) {
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q1 <- qplot(Sex, PC90, data=data, geom="blank", colour=Sex, xlab="Sex:Centre", ylab="PC90 (hours)")
	q1 <- q1 + scale_colour_discrete(h.start=120)
	q1 <- q1 + geom_point(position=position_jitter(w=0.1))
	q1 <- q1 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q1 <- q1 + opts( legend.position="none")
	q1 <- q1 + facet_grid(.~Centre)
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(Treatment, PC90, data=data, geom="blank", colour=Treatment, xlab="Treatment:Centre", ylab="PC90 (hours)")
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q2 <- q2 + opts( legend.position="none")
	q2 <- q2 + facet_grid(.~Centre)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(Treatment, PC90, data=data, geom="blank", colour=Treatment, xlab="Treatment:Sex", ylab="PC90 (hours)")
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + stat_summary(fun.data="mean_cl_normal", geom="crossbar", width=0.3)
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~Sex)
	print(q3, vp=vp3)
}

ancova.interactions <- function(pretime=F) {
	vp0 <- viewport(width=0.5, height=0.25, y=1, just="top")
	q <- qplot(Predose, PC90.loglin, data=PC90.df, xlab="Pre-dose parasite count", ylab="PC90 (hours)")
	q <- q + scale_x_continuous(limits=c(0,100000), formatter="comma")
	if (pretime) {
		q <- qplot(Pretime, PC90.loglin, data=PC90.df, xlab="Time of pre-dose count before first dose (hours)", ylab="PC90 (hours)")
		#q <- q + scale_x_continuous(limits=c(0,100000), formatter="comma")
	}
	q <- q + opts( legend.position="none")
	print(q, vp=vp0)
	
	vp1 <- viewport(width=1, height=0.25, y=0.75, just="top")
	q1 <- q + facet_grid(.~Centre)
	print(q1, vp=vp1)
	
	vp2 <- viewport(width=1, height=0.25, y=0.5, just="top")
	q2 <- q + facet_grid(.~Sex)
	print(q2, vp=vp2)

	vp3 <- viewport(width=1, height=0.25, y=0, just="bottom")
	q3 <- q + facet_grid(.~Treatment)
	print(q3, vp=vp3)
}

plotresids.lme <- function(model, binwidth=0.5) {
	plotresids.lm(model, resid(model, type='pearson'), model$data, binwidth)
}

plotresids.lm <- function(model, resids, data, binwidth=0.5, trans=F, weighted=F) { # Need to modify for lme
	if (missing(resids)) {
		resids <- stdres(model)
	}
	if (missing(data)) {
		data <- model$model
	}
	lab.txt <- "Standardized residuals"

	#
	# Histogram
	#
	vp1 <- viewport(width=0.5, height=0.5, x=0.25, y=0.75)
	q <- qplot(resids, xlab=lab.txt, geom="blank") + geom_histogram(colour="black", fill="white", binwidth=binwidth) 
	print(q, vp=vp1)

	#
	# QQ normal
	#
	vp2 <- viewport(width=0.5, height=0.5, x=0.75, y=0.75)
	q <- qplot(sample=resids)
	y <- quantile(resids, c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	q <- q + geom_abline(intercept=int, slope=slope, linetype=2)
	print(q, vp=vp2)

	#
	# vs factors
	#
	vp3 <- viewport(width=0.5, height=0.5, x=0.25, y=0.25)
	p <- dim(data)[2] - weighted
	data.l <- reshape(data, direction="long", varying=list(2:p), v.names="Factor")
	if (p > 2)
		q <- qplot(Factor, rep(resids, p-1), data=data.l, geom="blank", ylab=lab.txt, colour=time)
	else
		q <- qplot(Factor, resids, data=data.l, geom="blank", ylab=lab.txt)
	q <- q + geom_hline(aes(yintercept=0), linetype=2)
#	q <- q + stat_boxplot(width=0.5, aes(outlier.size=0))
	q <- q + geom_point(position=position_jitter(w=0.1))
	q <- q + stat_summary(fun.dat="mean_sdl", mult=1, geom="crossbar", width=0.5)
	q <- q + opts(legend.position="none")
	print(q, vp=vp3)

	#
	# vs fitted
	#
	vp4 <- viewport(width=0.5, height=0.5, x=0.75, y=0.25)
	q <- qplot(fitted(model)^(1+1*trans), resids, data=data, xlab="Fitted PC90 (hours)", ylab=lab.txt)
	q <- q + geom_hline(aes(yintercept=0), linetype=2)
	print(q, vp=vp4)
}

resample.pc90 <- function(data, fit, n=1000, bootstrap=F) {
	model.terms <- attr(terms(fit), "term.labels")
	coef.samples <- matrix(nrow=n, ncol=length(model.terms) + 1)
	pc90.sample <- data$PC90.loglin

	for (i in 1:n) {
		data$PC90.loglin <- sample(pc90.sample, length(pc90.sample), replace=bootstrap)
		fit.resample <- update(fit, data=data)
		coef.samples[i,] <- coef(fit.resample)
	}

	dimnames(coef.samples) <- list(sample=1:n, coefficient=names(coef(fit)))
	coef.samples	
}

resample.aov <- function(fit, n=1000, bootstrap=F) {
	model.terms <- attr(terms(fit), "term.labels")
	p <- length(model.terms)
	F.samples <- matrix(nrow=n, ncol=p, dimnames=list(sample=1:n, term=model.terms))
	data <- fit$model
	response.sample <- data[,1]

	F.values <- summary(fit)[[1]][4]
	F.samples[1,] <- F.values[1:p,1]
	for (i in 2:n) {
		data[,1] <- sample(response.sample, length(response.sample), replace=bootstrap)
		fit.resample <- update(fit, data=data)
		F.values <- summary(fit.resample)[[1]][4]
		F.samples[i,] <- F.values[1:p,1]
	}

	F.samples	
}

restricted.resample.aov <-  function(fit, strata, n=1000, bootstrap=F) {
	model.terms <- attr(terms(fit), "term.labels")
	p <- length(model.terms)
	F.samples <- matrix(nrow=n, ncol=p, dimnames=list(sample=1:n, term=model.terms))
	data <- fit$model
	randomize.within <- lapply(strata, function(x) data[,x])

	F.values <- summary(fit)[[1]][4]
	F.samples[1,] <- F.values[1:p,1]
	for (i in 2:n) {
		resample.list <- as.list(by(data, randomize.within, function(x) {
			x[,1] <- sample(x[,1], length(x[,1]), replace=bootstrap)
			x
		}))
		for (strata.sample in resample.list) {
			data[dimnames(strata.sample)[[1]],] <- strata.sample
		}
		# Checksum on strata for code verification
		# by(data, randomize.within, function(x) print(sum(x$PC90.loglin)))
		fit.resample <- update(fit, data=data)
		F.values <- summary(fit.resample)[[1]][4]
		F.samples[i,] <- F.values[1:p,1]
	}

	F.samples	
}


pt.resample <- function(data, fit, n=1000, bootstrap=F) {
	coef.samples <- resample.pc90(data, fit, n, bootstrap)
	indicators <- apply(coef.samples, 1, function(x) abs(x) > abs(coef(fit)))
	t(t(rowMeans(indicators)))
}

pf.resample <- function(fit, n=1000, bootstrap=F) {
	F.samples <- resample.aov(fit, n, bootstrap)
	p <- dim(F.samples)[2]
	F.actual <- summary(fit)[[1]][4][1:p,1]
	indicators <- apply(F.samples, 1, function(x) x > F.actual)
	t(t(rowMeans(indicators)))
}

pf.restricted.resample <- function(fit, strata, n=1000, bootstrap=F) {
	F.samples <- restricted.resample.aov(fit, strata, n, bootstrap)
	p <- dim(F.samples)[2]
	F.actual <- summary(fit)[[1]][4][1:p,1]
	indicators <- apply(F.samples, 1, function(x) x > F.actual)
	t(t(rowMeans(indicators)))
}

summary.pc90 <- function(data, digits=2) {
	result <- array(c(mean(data), median(data), sd(data)))
	rownames(result) <- c("Mean", "Median", "sd")
	round(result, digits)
}

compare.ancova <- function(data) {
	q <- qplot(Predose, PC90, data=data, colour=Treatment, facets=.~Sex, xlab="Pre-dose parasite count", ylab="PC90 (hours)", main="Comparison of ANOVA and ANCOVA models") + scale_x_continuous(limits=c(0,100000), formatter="comma")
	q + geom_line(aes(y=fit, linetype=Model, colour=Treatment), size=1)
}

compare.ancova2 <- function(data) {
	q <- qplot(Pretime, PC90, data=data, colour=Treatment, facets=.~Sex, xlab="Time of pre-dose parasite count before first dose (hours)", ylab="PC90 (hours)", main="Comparison of ANOVA and ANCOVA models")# + scale_x_continuous(limits=c(0,100000), formatter="comma")
	q + geom_line(aes(y=fit, linetype=Model, colour=Treatment), size=1)
}

sample.pc90 <- function(data, n=1000, bootstrap=F) {
	
}

stillwhite.resid <- function(row, data) {
	centre <- row['Centre'][1,1]
	sex <- row['Sex'][1,1]
	trt <- row['Treatment'][1,1]
	Xcst <- row['PC90'][1,1]
	Xstar <- with(data, Xcst - mean(PC90[Centre==centre]) - mean(PC90[Sex==sex]) - mean(PC90[Treatment==trt]) + mean(PC90))
	Xstar
}

stillwhite2.resid <- function(row, data) {
	sex <- row['Sex'][1,1]
	trt <- row['Treatment'][1,1]
	Xcst <- row['PC90'][1,1]
	Xstar <- with(data, Xcst - mean(PC90[Sex==sex]) - mean(PC90[Treatment==trt]) + mean(PC90))
	Xstar
}


t.resample <- function(x1, x2, R=1000, bootstrap=F) {
	Tobs <- mean(x1) - mean(x2)
	n1 <- length(x1)
	n2 <- length(x2)
	values <- c(x1, x2)
	n <- length(values)
	values.resampled <- replicate(R - 1, sample(values, n, replace=bootstrap))
	T <- c(Tobs, apply(values.resampled, 2, function(x) mean(x[1:n1]) - mean(x[n1+1:n2])))
	mean(abs(T) > abs(Tobs))
}

permute.f.ci <- function(data, k, R=1000, bootstrap=F) {
	data$PC90.loglin[data$Treatment=='alone'] <- data$PC90.loglin[data$Treatment=='alone'] - k
	fit <- aov(PC90.loglin ~ Centre*Sex*Treatment, data)
	pf.restricted.resample(fit, c("Centre", "Sex"), R, bootstrap)
}
