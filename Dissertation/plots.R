# Lattice plot of raw data for each subject
plot1 <- function() {
	xyplot(parct ~ acttm | SUBJID, data=malaria.df, panel=pf1, par.strip.text=list(cex=0.5), ylim=c(0:15), strip=strip.custom(strip.levels=c(T, T)), xlab="Time in hours from first dose", ylab="Log(1+parasite count)")
}

# Panel function for plot1
pf1 <- function(x, y) {
	panel.xyplot(x, log(1+y), type='p')
#	j=x[c(TRUE,y>0)]
#	z=log(1+y[c(TRUE,y>0)])
#	fit<-lm(z~j+I(j^2)+I(j^3))
#	panel.xyplot(j,predict(fit),type='l')
	panel.abline(h=log(1 + (0.1 * y[1])), lty=2)
}


# Lattice plot of log parasite count with logistic fit
plot2<-function() {
	xyplot(log(parct + 1) ~ acttm | SUBJID, data=malaria.df, panel=pf2, par.strip.text=list(cex=0.5))
}

# Panel function for plot2
pf2 <- function(x, y) {
	panel.xyplot(x, y, type='p')
	fit <- nls(y ~A + (L / (1 + exp(-B * (x - U)))), start=list(A=min(y), L=max(y), B=-0.1, U=15))
	panel.xyplot(x, predict(fit), type='l')
}


# Plots raw data for a subject with PC90 level shown
rawplot90 <- function(subjid) {
	with(malaria.df, plot(acttm[SUBJID==subjid], parct[SUBJID==subjid], xlab="Time", ylab="Parasite count", type='o', main=subjid))
	with(malaria.df, abline(h=0.1 * parct[SUBJID==subjid][1], lty=2))
}

# Plots log count with logistic fit and linear interpolation
logplot90 <- function(s) {
	# Plot the log count
	x <- malaria$acttm[malaria$SUBJID==s]
	y <- log(1 + malaria$parct[malaria$SUBJID==s])
	plot(x, y, xlab="Time", ylab="Log(1 + Parasite count)", type='p', main=s)

	parct90 <- log(1 + (0.1 * malaria$parct[malaria$SUBJID==s][1]))
	fit <- PC90logit.fit[[s]]
	if (!is.null(fit)) {
		range=min(x):max(x)
		lines(range,predict(fit, data.frame(x=range)), lty=2)
	}
	i <- which(y < parct90)[1]
	lines(c(x[i-1],x[i]), c(y[i-1],y[i]), lty=2)
	abline(h=parct90, lty=2)
}


# Plots a number of counts with vertical PC90 line
plotPC90 <- function(range) {
	for (s in PC90.df$SUBJID[range]) {
	    plot(malaria$acttm[malaria$SUBJID==s], malaria$parct[malaria$SUBJID==s], xlab="Time", ylab="Parasite count", type='l', main=s)
	    abline(v=PC90.df$PC90[PC90.df$SUBJID==s], lty=2)
	}
}


# Set up the basic plot with data, scales and panels by subject
getrawplot <- function(data) {
	q <- ggplot(data, aes(x=acttm, y=parct))
	q <- q + scale_x_continuous(name="Time (hours)")
	q <- q + scale_y_continuous(formatter=function(x) return(x/1000), name=expression("Parasite Count / "*mu*"L  (1000s)"))
	l <- length(unique(data$SUBJID))
	q + facet_wrap(~SUBJID, ncol=min(3,l), scales="free_y")
}

# Convert the plot q to log-linear
getlogplot <- function(q) {
	q <- q %+% transform(q$data, parct=log(1+parct))
	q <- q + scale_y_continuous(name="Log (1 + parasite count)")
	l <- length(unique(q$data$SUBJID))
	q + facet_wrap(~SUBJID, ncol=min(3,l), scales="fixed")
}

# Change point shape and line colour to reflect treatment group
addtrtgeoms <- function(q, points=T, lines=T) {
	# Reorder the patient factor so that "alone" treatment patients are first
	for(subj in as.character(unique(q$data$SUBJID[q$data$trt=='A']))) {
		q$data$SUBJID <- relevel(q$data$SUBJID, subj)
	}
	if (points) {
		q <- q + geom_point(aes(shape=trttxt, colour=trttxt))
	}
	if (lines) {
		q <- q + geom_line(aes(colour=trttxt))
	}
	q + scale_shape(name="Treatment") + scale_colour_discrete("Treatment")
}

# Initial raw plot of data take 2
rawggplot <- function(data, title="", points=T, lines=T) {
	
#	q <- qplot(acttm, parct, data=data, aes(acttm, parct), xlab="Time (hours)", ylab="Parasite Count (1000s)", main=title, geom="blank")
	q <- getrawplot(data)
	q <- addtrtgeoms(q, points, lines)
	q + opts(title=title)
	#    q <- q + scale_y_continuous(formatter=function(x) return(x/1000))
#	l <- length(unique(data$SUBJID))
#	ncol <- ifelse(l < 3, l, 3)
#	q + facet_wrap(~SUBJID, ncol=ncol)
#	q + geom_line(aes(colour=trttxt))
}

# Plot raw data for male and female subjects side-by-side
rawggplot3 <- function(data, title="", centre="", w1=0.45, w2=0.55, r1=4, r2=4, points=T, lines=T) {
	require(ggplot2)

	vp1 <- viewport(width=w1, x=0, just="left")
	q <- getrawplot(data[data$SEX=='Male',])
	q <- addtrtgeoms(q, points, lines)
	q <- q + opts(title=paste(centre, "Male"), legend.position='none')
	q <- q + facet_wrap(~SUBJID, nrow=r1, scales="free_y")
	print(q, vp=vp1)

	vp2 <- viewport(width=w2, x=1, just="right")
	q <- getrawplot(data[data$SEX=='Female',])
	q <- addtrtgeoms(q, points, lines)
	q <- q + opts(title=paste(centre, "Female"))
	q <- q + facet_wrap(~SUBJID, nrow=r2, scales="free_y")
	print(q, vp=vp2)
}

predoseaov <- function(data, title="") {
	q <- qplot(trttxt, parct, data=data, xlab="", ylab="Parasite Count (1000s)", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q <- q + scale_y_continuous(formatter=function(x) return(x/1000))
	q <- q + stat_summary(fun.data="mean_cl_boot", size=3, geom="point", solid=T)
	q + facet_grid(CENTREID~SEX, margins=T)
}

# Plot pre-dose counts as boxplots by centre, sex and treatment, in pairs of factors
predoseaov2 <- function(data) {
	require(ggplot2)

	# Boxplots of pre-dose count by sex for each centre
	vp1 <- viewport(width=1, height=0.33, y=1, just="top")
	q1 <- qplot(SEX, parct, data=data, geom="blank", colour=SEX, xlab="Sex:Centre", ylab="")
	q1 <- q1 + scale_colour_discrete(h.start=120)
	q1 <- q1 + scale_y_continuous(limits=c(0,100000), formatter="comma")
	q1 <- q1 + geom_boxplot(width=0.3, outlier.size=0)
	q1 <- q1 + geom_point(position=position_jitter(w=0.1))
	q1 <- q1 + opts(legend.position="none")
	q1 <- q1 + facet_grid(.~CENTREID)
	print(q1, vp=vp1)

	# Boxplots of pre-dose count by treatment for each centre
	vp2 <- viewport(width=1, height=0.33, y=0.5, just="centre")
	q2 <- qplot(trttxt, parct, data=data, geom="blank", colour=trttxt, xlab="Treatment:Centre", ylab=expression("Parasite Count / "*mu*"L"))
	q2 <- q2 + scale_y_continuous(limits=c(0,100000), formatter="comma")
	q2 <- q2 + geom_boxplot(width=0.3, outlier.size=0)
	q2 <- q2 + geom_point(position=position_jitter(w=0.1))
	q2 <- q2 + opts(legend.position="none")
	q2 <- q2 + facet_grid(.~CENTREID)
	print(q2, vp=vp2)

	# Boxplots of pre-dose count by treatment for each sex
	vp3 <- viewport(width=1, height=0.33, y=0, just="bottom")
	q3 <- qplot(trttxt, parct, data=data, geom="blank", colour=trttxt, xlab="Treatment:Sex", ylab="")
	q3 <- q3 + scale_y_continuous(limits=c(0,100000), formatter="comma")
	q3 <- q3 + geom_boxplot(width=0.3, outlier.size=0)
	q3 <- q3 + geom_point(position=position_jitter(w=0.1))
	q3 <- q3 + opts(legend.position="none")
	q3 <- q3 + facet_grid(.~SEX)
	print(q3, vp=vp3)
}

allaov <- function(data, title="") {
	q <- qplot(pt, log((1 + parct)/pre), data=data, xlab="Time from first dose (hours)", ylab="Log fraction of pre-dose count", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt, shape=trttxt)) + scale_colour_discrete("Treatment") + scale_shape("Treatment")
#	q <- q + scale_y_continuous(formatter=function(x) return(x/1000)) #+ opts(axis.text.x = theme_text(angle=90))
	q <- q + stat_summary(fun.y="mean", geom="line", aes(colour=trttxt))
	q + facet_grid(CENTREID~SEX, margins=T)
}

# Plot log count ratio with mean level for each treatment shown
allaov2 <- function(data, title="Parasite reduction averaged over subjects with time from first treatment") {
	# All data
	vp1 <- viewport(width=0.7, height=0.33, x=0.5, y=1, just="top")
	q <- qplot(pt, log((1 + parct)/pre), data=data, xlab="", ylab="", main="", geom="blank")
	q <- q + geom_point(aes(colour=trttxt, shape=trttxt)) + scale_colour_discrete("Treatment") + scale_shape("Treatment")
	q <- q + stat_summary(fun.y="mean", geom="line", aes(colour=trttxt)) 
	print(q, vp=vp1)

	# Split by centre
	vp2 <- viewport(height=0.33, y=0.5, just="centre")
	q <- q + facet_grid(.~CENTREID) + opts(title="", legend.position="none") + scale_y_continuous(name="Log fraction of pre-dose count")
	print(q, vp=vp2)

	# Split by sex
	vp3 <- viewport(height=0.33, y=0, just="bottom")
	q <- q + facet_grid(.~SEX) + opts(title="", legend.position="none") + scale_x_continuous(name="Time from first dose (hours)") + scale_y_continuous(name="")
	print(q, vp=vp3)
}

predose.resid <- function(residuals, title="Residuals from pre-dose count ANOVA", b=20000, limits) {
	plot1 <- qplot(residuals, main=title, xlab="Residual", geom="blank")
	plot1 <- plot1 + geom_histogram(fill='white', colour='black', binwidth=b)
	if (!missing(limits)) {
		plot1 <- plot1 + scale_x_continuous(limits=limits)
	}
	print(plot1)
	subplot1 <- qplot(sample=residuals, stat="qq")
	y <- quantile(residuals, c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	subplot1 <- subplot1 + geom_abline(intercept=int, slope=slope, linetype=2)
	vp2 <- viewport(x=1, y=0.90, width=0.5, height=0.5, just=c("right", "top"))
	print(subplot1, vp=vp2)
}

histandqq <- function(data, ..., xlab="") {
	vp1 <- viewport(width=0.5, x=0.25)
	print(qplot(data, geom='blank', xlab=xlab) + geom_histogram(fill='white', colour='black', ...) + geom_vline(xintercept=0, linetype=2, size=1), vp=vp1)
	vp2 <- viewport(width=0.5, x=0.75)
	print(qplot(sample=data, geom='blank') + geom_point(stat='qq', shape=16), vp=vp2)
}
