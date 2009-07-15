ggplot90 <- function(data, title="", pc90lines, xpos=45, am=1, vjust=0) {
	q <- rawggplot(data, title)
	if (missing(pc90lines)) {
		pc90lines <- getPC90lines(data)
	}
	pc90lines <- data.frame(pc90lines, xpos=xpos, vjust=vjust)
	q <- q + geom_hline(aes(yintercept=pc90), data=pc90lines, linetype=2)
	q + geom_text(aes(x=xpos, y=pc90, label="PC90", vjust=vjust), data=pc90lines[am,])
}

gglog90 <- function(data, title="", xpos=45, am=1, vjust=0) {
	data$parct <- data$parct + 1
	pc90lines <- getPC90lines(data)
	data$parct <- log(data$parct)
	pc90lines$pc90 <- log(pc90lines$pc90)
	q <- ggplot90(data, title, pc90lines, xpos, am, vjust)
#	q <- q + geom_line(aes(colour=trttxt))
	q + scale_y_continuous("log(1 + parasite count)") 
}

addPC90lines <- function(q, data, logplot=F, xpos=45, am=1, vjust=-0.1) {
	if (missing(data)) {
		data <- q$data
	}
	pc90.df <- subset(data, select=c(SUBJID, parct), subset=plantm=='PRE-DOSE')
	pc90 <- ifelse(logplot, log((pc90.df$parct + 1) * 0.1), pc90.df$parct * 0.1)
	pc90.df <- data.frame(pc90.df, pc90=pc90, xpos=xpos, vjust=vjust)
	q <- q + geom_hline(aes(yintercept=pc90), data=pc90.df, linetype=2)
	q + geom_text(aes(x=xpos, y=pc90, label="PC90", vjust=vjust), data=pc90.df[am,])
}

plotraw90 <- function() {
	ggplot90(malaria.1M, am=8, vjust=-0.1, title="Parasite counts for Centre 1 Males with PC90 level shown")
}

cubic90 <- function(data, title="", pc90lines=NA, xpos=45, am=1, vjust=0) {
    q <- gglog90(data, title, xpos, am, vjust)
    fitdat <- malaria.01M
    fitdat$parct <- log(1 + fitdat$parct)
    q + stat_smooth(data=fitdat, method="lm", formula=y~x+I(x^2)+I(x^3)) 
}

plotcubic <- function() {
    cubic90(malaria.1M, am=8, vjust=-0.1, title="Cubic fit to log parasite count up to first 0 reading") + opts(legend_position="bottom")
}

logistic.fit <- function(data) {
	subjects <- levels(data$SUBJID)
	n <- length(subjects)
	fits.df <- data.frame(subset(data, select=c(SUBJID, CENTREID, SEX, trttxt), subset=plantm=='PRE-DOSE'), A=numeric(n), L=numeric(n), U=numeric(n), B=numeric(n))
	for (i in 1:n) {
		fit <- NULL
		tryCatch(fit <- nls(log(1 + parct) ~ SSfpl(acttm, A, L, U, B), data=data, subset=SUBJID==fits.df$SUBJID[i]), error=function(e) e)
		if(!is.null(fit)) {
			fits.df[i, 5:8] <- coef(fit)
		}
	}
	return(fits.df)
}

addlogfit <- function(q) {
	q + stat_smooth(method="nls", formula="y ~ SSfpl(x, A, L, U, B)", se=F)
}

addlogparms <- function(q, fits) {
	subjs <- unique(q$data$SUBJID)
	fits <- subset(fits, subset=SUBJID %in% subjs)
	q <- q + geom_segment(data=fits, aes(x=min(x),y=A,xend=U,yend=A), colour='red', size=1)
	q <- q + geom_segment(data=fits, aes(x=U,y=L,xend=max(x),yend=L), colour='red', size=1)
	q <- q + geom_segment(data=fits, aes(x=U, y=L, xend=U, yend=A), colour='red', size=1)
	q + geom_text(data=fits, x=40, y=4, aes(label=round(B,1)), colour='blue')
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
	data <- q$data
	q <- getlogplot(q)
	q <- addPC90lines(q, data, logplot=T)
	addlogfit(q)
}

showlogparms <- function(fits, subjs = c('54','80','96','98','140','150','176','182','185','187','197','203')) {
	q <- rawggplot(subset(malaria, subset=SUBJID %in% subjs), title="Logistic fit to log parasite count", lines=F)
	q <- getlogplot(q)
	q <- addlogfit(q)
	q <- addlogparms(q, fits)
	q + opts(legend.position="none")
}

