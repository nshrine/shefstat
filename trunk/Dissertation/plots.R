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


# Initial raw plot of data take 2
rawggplot <- function(data, title="") {
	# Reorder the patient factor so that "alone" treatment patients are first
	data$SUBJID <- relevel(data$SUBJID, as.character(unique(data$SUBJID[data$trt=='A'])))

	p <- qplot(acttm, parct, data=data, aes(acttm, parct), xlab="Time (hours)", ylab="Parasite Count (1000s)", main=title)
	p + geom_point(aes(colour=trttxt, shape=trttxt)) + geom_line(aes(colour=trttxt)) + scale_colour_discrete("Treatment") + scale_shape_discrete("Treatment") + facet_wrap(~SUBJID, scales='free_y') + scale_y_continuous(formatter=function(x) return(x/1000)) 
}
