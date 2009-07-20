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

pc90cubic <- function(fit, data) {
	coefs <- coef(fit)
	parct90 <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * 0.1))
	preds <- predict(fit)
	upper <- which.min(preds > parct90)
	lower <- upper - 1
	interval <- c(data$acttm[lower], data$acttm[upper])
	soln <- uniroot(function(x) coefs[1] + coefs[2]*x + coefs[3]*x^2 + coefs[4]*x^3 - parct90, interval=interval)
	soln$root
}

logistic.fit <- function(dat) {
	initA <- max(log(1 + dat$parct))
	initL <- min(log(1 + dat$parct))
	halfway <- (initA + initL)/2
	initU <- dat$acttm[which.min(abs(log(1 + dat$parct) - halfway))]
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
	return(new.df)
}

getPC90.logistic <- function(fit, data) {
	parct90 <- log(1 + (data$parct[data$plantm=='PRE-DOSE'] * 0.1))
	preds <- predict(fit)
	upper <- which.min(preds > parct90)
	lower <- upper - 1
	interval <- c(data$acttm[lower], data$acttm[upper])
	soln <- uniroot(function(x) predict(fit, data.frame(acttm=x)) - parct90, interval=interval)
	soln$root
}

# sapply(PC90.df$SUBJID, function(s) pc90cubic(cubics.lmlist[[s]], malaria.0[malaria.0$SUBJID==s,]))
#
