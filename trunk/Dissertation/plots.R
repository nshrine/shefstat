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


getrawplot <- function(data) {
    q <- ggplot(data, aes(x=acttm, y=parct))
    q <- q + scale_x_continuous(name="Time (hours)")
    q <- q + scale_y_continuous(formatter=function(x) return(x/1000), name="Parasite Count (1000s)")
    l <- length(unique(data$SUBJID))
    q + facet_wrap(~SUBJID, ncol=min(3,l), scales="free_y")
}

getlogplot <- function(q) {
    q <- q %+% transform(q$data, parct=log(1+parct))
    q <- q + scale_y_continuous(name="Log (1 + parasite count)")
    l <- length(unique(q$data$SUBJID))
    q + facet_wrap(~SUBJID, ncol=min(3,l), scales="fixed")
}

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
    
#    q <- qplot(acttm, parct, data=data, aes(acttm, parct), xlab="Time (hours)", ylab="Parasite Count (1000s)", main=title, geom="blank")
    q <- getrawplot(data)
    q <- addtrtgeoms(q, points, lines)
    q + opts(title=title)
    #    q <- q + scale_y_continuous(formatter=function(x) return(x/1000))
#    l <- length(unique(data$SUBJID))
#    ncol <- ifelse(l < 3, l, 3)
#    q + facet_wrap(~SUBJID, ncol=ncol)
#	q + geom_line(aes(colour=trttxt))
}

predoseaov <- function(data, title="") {
	q <- qplot(trttxt, parct, data=data, xlab="", ylab="Parasite Count (1000s)", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q <- q + scale_y_continuous(formatter=function(x) return(x/1000))
	q <- q + stat_summary(fun.data="mean_cl_boot", size=3, geom="point", solid=T)
	q + facet_grid(CENTREID~SEX, margins=T)
}

allaov <- function(data, title="") {
	q <- qplot(pt, log((1 + parct)/pre), data=data, xlab="Time from first dose (hours)", ylab="Log fraction of pre-dose count", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt, shape=trttxt)) + scale_colour_discrete("Treatment") + scale_shape("Treatment")
#	q <- q + scale_y_continuous(formatter=function(x) return(x/1000)) #+ opts(axis.text.x = theme_text(angle=90))
	q <- q + stat_summary(fun.y="mean", geom="line", aes(colour=trttxt))
	q + facet_grid(CENTREID~SEX, margins=T)
}

predose.resid <- function(residuals, title="Residuals from pre-dose count ANOVA", b=20000, limits) {
	plot1 <- qplot(residuals, main=title, xlab="Residual", geom="blank")
	plot1 <- plot1 + geom_histogram(fill='white', colour='black', binwidth=b)
	if (!missing(limits)) {
		plot1 <- plot1 + scale_x_continuous(limits=limits)
	}
	print(plot1)
	subplot1 <- qplot(sample=residuals, stat="qq")
	vp2 <- viewport(x=1, y=0.90, width=0.5, height=0.5, just=c("right", "top"))
	print(subplot1, vp=vp2)
}
