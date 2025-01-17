interp90 <- function(acttm, parct, loglinear = FALSE) {
	parct90 <- 0.1 * parct[1]	
	pos90 <- length(parct[parct > parct90])
	pos91 <- pos90 + 1
	if (loglinear) {
		parct90 <- log(parct90)
		parct <- log(parct+0.000000000000001)
	}
	fit <- lm(acttm ~ parct, data = data.frame(acttm, parct), subset = pos90:pos91)
	PC90 <- predict(fit, newdata = data.frame(parct = parct90))
	PC90
}

fitLogit90 <- function(x, y, b=-0.1, u=20) {
	y <- log(1 + y)
	fit <- nls(y~A+(L/(1+exp(-B*(x-U)))),start=list(A=min(y),L=max(y),B=b,U=u))
	return(fit)
}

optimPC90 <- function(fit, y, range=0:50) {
	parct90 <- log(1 + (0.1 * y[1]))
	assign("y", parct90, pos=1)
	assign("fit", fit, pos=1)
	opt <- optimize(minlogit, range)
	PC90 <- opt$minimum
	PC90
}

minlogit <- function(t) {
	return((y - predict(fit, data.frame(x=t)))^2)
}

# Create a list of cubic fits to data up to first zero
cubic.fits <- function(data) {
	require(nlme)
	lmList(log(1 + parct) ~ acttm + I(acttm^2) + I(acttm^3) | SUBJID, data=uptofirstzero(data))
}

# Create a new data frame containing data up to first zero count
uptofirstzero <- function(data) {
	new.df <- data.frame()
	for (s in unique(data$SUBJID)) {
		rows <- data[data$SUBJID==s,]
		n <- match(0, rows$parct)
		if (!is.na(n)) {
			rows <- rows[1:n,]
		}
		new.df <- rbind(new.df, rows)
	}
	new.df
}

# Find the PC90 time from a cubic fit
pc90cubic <- function(fit, data) {
	coefs <- coef(fit)
	parct90 <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * 0.1))
	preds <- predict(fit)
	
	# Find the times at which counts are above and below PC90 
	upper <- which.min(preds > parct90)
	lower <- upper - 1
	interval <- c(data$acttm[lower], data$acttm[upper])

	# Search for PC90 between these times
	soln <- uniroot(function(x) coefs[1] + coefs[2]*x + coefs[3]*x^2 + coefs[4]*x^3 - parct90, interval=interval)
	soln$root
}

# Perform logistic fitting after selecting starting parameters
logistic.fit <- function(dat) {
	# Set alpha and lambda to max and min counts
	initA <- max(log(1 + dat$parct))
	initL <- min(log(1 + dat$parct))

	# Set mu to time closest to half max count
	halfway <- (initA + initL)/2
	initU <- dat$acttm[which.min(abs(log(1 + dat$parct) - halfway))]

	# Set beta to -0.5 i.e. 2 in SSfpl formulation
	initB <- 2
	
	tryCatch(fit <- nls(log(1 + parct) ~ SSfpl(acttm, A, L, U, B), data=dat,
				start=list(A=initA, L=initL, U=initU, B=initB)),
		error=function(e) e)
	fit
}

logistic.fits <- function(data) {
	subjects <- levels(data$SUBJID)
	n <- length(subjects)
	fits.df <- data.frame(subset(data, select=c(SUBJID, CENTREID, SEX, trttxt), subset=plantm=='PRE-DOSE'), A=numeric(n), L=numeric(n), U=numeric(n), B=numeric(n))
	for (i in 1:n) {
		fit <- NULL
		dat <- subset(data, SUBJID==fits.df$SUBJID[i])
		fit <- logistic.fit(dat)
		if(!is.null(fit)) {
			fits.df[i, 5:8] <- coef(fit)
		}
	}
	return(fits.df)
}

# Find PC90 time from logistic fit
getPC90.logistic <- function(fit, data) {
	parct90 <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * 0.1))
	preds <- predict(fit)

	# Find the times at which counts are above and below PC90
	upper <- which.min(preds > parct90)
	lower <- upper - 1
	interval <- c(data$acttm[lower], data$acttm[upper])

	# Search for PC90 between these times
	soln <- uniroot(function(x) predict(fit, data.frame(acttm=x)) - parct90, interval=interval)
	soln$root
}

# sapply(PC90.df$SUBJID, function(s) pc90cubic(cubics.lmlist[[s]], malaria.0[malaria.0$SUBJID==s,]))
#

# Get indices of counts immediately above and below PCx level
getAboveBelow <- function(data, PC=90) {
	pc90 <- data$parct[data$plantm=='PRE-DOSE'] * (100 - PC)/100
	above.pc90 <- which(data$parct > pc90)
	upper <- above.pc90[length(above.pc90)]
	lower <- upper + 1
	c(upper, lower)
}

getabsAboveBelow <- function(data, pc) {
	above.pc <- which(data$parct > pc)
	upper <- above.pc[length(above.pc)]
	lower <- upper + 1
	c(upper, lower)
}

# Perform linear fit between two points either side of PCx
lmloglin <- function(data, PC=90) {
	lm(log(1 + parct) ~ acttm, data=data, subset=getAboveBelow(data, PC))
}

getPC90.loglin <- function(fit, data) {
	pc90 <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * 0.1))
	B0 <- coef(fit)[1]
	B1 <- coef(fit)[2]
	(pc90 - B0) / B1
}

# Get PCx time from log-linear interpolation
getPC.loglin <- function(data, PC=90) {
	pc <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * (100 - PC)/100))
	fit <- lmloglin(data, PC)
	B0 <- coef(fit)[1]
	B1 <- coef(fit)[2]
	(pc - B0) / B1
}

getPC.3000 <- function(data, pc=3000) {
	fit <- lm(log(1 + parct) ~ acttm, data=data, subset=getabsAboveBelow(data, pc))
	B0 <- coef(fit)[1]
	B1 <- coef(fit)[2]
	(log(1 + pc) - B0) / B1
}
