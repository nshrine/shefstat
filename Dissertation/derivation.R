ggplot90 <- function(data, title="", pc90lines, xpos=45, am=1, vjust=-0.1) {
	q <- rawggplot(data, title)
	addPC90lines(q, xpos=xpos, am=am, vjust=vjust)
#	if (missing(pc90lines)) {
#		pc90lines <- getPC90lines(data)
#	}
#	pc90lines <- data.frame(pc90lines, xpos=xpos, vjust=vjust)
#	q <- q + geom_hline(aes(yintercept=pc90), data=pc90lines, linetype=2)
#	q + geom_text(aes(x=xpos, y=pc90, label="PC90", vjust=vjust), data=pc90lines[am,])
}

gglog90 <- function(data, title="", xpos=45, am=1, vjust=-0.1) {
	q <- rawggplot(data, title)
	q <- getlogplot(q)
	addPC90lines(q, logplot=T, xpos=xpos, am=am, vjust=-0.1)
}

addPC90lines <- function(q, logplot=F, xpos=45, am=1, vjust=-0.1) {
	data <- q$data
	if (logplot) {
		data$parct <- exp(data$parct) - 1
	}
	pc90.df <- subset(data, select=c(SUBJID, parct), subset=plantm=='PRE-DOSE')
	pc90 <- ifelse(rep(logplot, nrow(pc90.df)), log((pc90.df$parct + 1) * 0.1), pc90.df$parct * 0.1)
	pc90.df <- data.frame(pc90.df, pc90=pc90, xpos=xpos, vjust=vjust)
	q <- q + geom_hline(aes(yintercept=pc90), data=pc90.df, linetype=2)
	q + geom_text(aes(x=xpos, y=pc90, label="PC90", vjust=vjust), data=pc90.df[am,])
}

plotraw90 <- function() {
	ggplot90(malaria.1M, am=8, vjust=-0.1, title="Parasite counts for Centre 1 Males with PC90 level shown")
}

logistic.fit <- function(data) {
	subjects <- levels(data$SUBJID)
	n <- length(subjects)
	fits.df <- data.frame(subset(data, select=c(SUBJID, CENTREID, SEX, trttxt), subset=plantm=='PRE-DOSE'), A=numeric(n), L=numeric(n), U=numeric(n), B=numeric(n))
	for (i in 1:n) {
		fit <- NULL
		dat <- subset(data, SUBJID==fits.df$SUBJID[i])
		initA <- max(log(1 + dat$parct))
		initL <- min(log(1 + dat$parct))
		halfway <- (initA + initL)/2
		initU <- dat$acttm[which.min(abs(log(1 + dat$parct) - halfway))]
		initB <- 2
		tryCatch(fit <- nls(log(1 + parct) ~ SSfpl(acttm, A, L, U, B), data=dat,
					start=list(A=initA, L=initL, U=initU, B=initB)),
			error=function(e) e)
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

addcubicfit <- function(q) {
	q + stat_smooth(method="lm", formula=y~x+I(x^2)+I(x^3), data=uptofirstzero(q$data), fullrange=F, se=F) 
}

plotcubic <- function(subjs = c('54','80','96','98','140','150','176','182','185','187','197','203')) {
    q <- rawggplot(subset(malaria, subset=SUBJID %in% subjs), title="Cubic fit to log parasite count up to first 0 reading", lines=F)
#	q <- q + opts(legend.position="bottom")
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T, am=10)
	addcubicfit(q)
}

addlogfit <- function(q) {
	q + stat_smooth(method="nls", formula="y ~ SSfpl(x, A, L, U, B)",
		start="list(A=max(y), L=min(y), U=x[which.min(abs(y-((max(y)+min(y))/2)))], B=2)", se=F)
}

addlogparms <- function(q, fits) {
	subjs <- unique(q$data$SUBJID)
	fits <- subset(fits, subset=SUBJID %in% subjs)
	q <- q + geom_segment(data=fits, aes(x=min(x), y=A, xend=U, yend=A), colour='red', size=1)
	q <- q + geom_segment(data=fits, aes(x=U, y=L, xend=max(x), yend=L), colour='red', size=1)
	q <- q + geom_segment(data=fits, aes(x=U, y=L, xend=U, yend=A), colour='red', size=1)
	q + geom_text(data=fits, x=40, y=4, aes(label=round(-1/B, 1)), colour='blue')
}

gglogistic <- function(data, fits, title="", xpos=45, am=1, vjust=0) {
	q <- getrawplot(data)
	q <- getlogplot(q)
#	q <- addPC90lines(q, data, logplot=T)
	q <- q + geom_point(size=3, shape=1)
	q <- q + stat_smooth(method="nls", formula="y ~ SSfpl(x, A, L, U, B)", se=F)
}

plotlogistic <- function(subjs = c('54','80','96','98','140','150','176','182','185','187','197','203')) {
	q <- rawggplot(subset(malaria, subset=SUBJID %in% subjs), title="Logistic fit to log parasite count", lines=F)
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T, am=10)
	addlogfit(q)
}

showlogparms <- function(fits, subjs = c('96','150','197')) {
	q <- rawggplot(subset(malaria, subset=SUBJID %in% subjs), title="Logistic fit to log parasite count", lines=F)
	q <- getlogplot(q)
	q <- addlogfit(q)
	q <- addlogparms(q, fits)
	q + opts(legend.position="none")
}

plotfailures <- function(subjs) {
	q <- rawggplot(subset(malaria, subset=SUBJID %in% subjs), title="Subjects for which logistic fitting fails")
	q <- getlogplot(q)
	addPC90lines(q, logplot=T, am=8)
}

cubicfailures <- function(q) {
	addcubicfit(q) + opts(title="Cubic fit to data where logistic fitting fails")
}
