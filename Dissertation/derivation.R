ggplot90 <- function(data, title="", pc90lines=NA, xpos=45, am=1, vjust=0) {
	q <- rawggplot(data, title)
	if (is.null(dim(pc90lines))) {
		pc90lines <- getPC90lines(data)
	}
	pc90lines <- data.frame(pc90lines, xpos=xpos, vjust=vjust)
	l <- length(unique(data$SUBJID))
    ncol <- ifelse(l < 3, l, 3)
#    q <- q + geom_line(aes(colour=trttxt))
	q + geom_hline(aes(yintercept=pc90), data=pc90lines, linetype=2) + facet_wrap(~SUBJID, ncol=ncol, scales="free_y") + geom_text(aes(x=xpos, y=pc90, label="PC90", vjust=vjust), data=pc90lines[am,])
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

getPC90lines <- function(data) {
	predose <- subset(data, select=c(SUBJID, parct), subset=plantm=='PRE-DOSE')
	pc90 <-data.frame(predose, pc90=predose$parct * 0.1)
	return(pc90)
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

gglog <- function(data, title="", xpos=45, am=1, vjust=0) {
#    data$parct <- log(1 + data$parct)
#    q <- rawggplot(data, title)
#    q <- q + scale_y_continuous("log(1 + parasite count)")
    q <- gglog90(data, title, xpos, am, vjust) 
    q + stat_smooth(method="nls", formula="y ~ SSfpl(x, A, B, xmid, scal)", se=F)
}

plotlogistic <- function() {
    gglog(subset(malaria, subset=SUBJID=='54' | SUBJID=='80' | SUBJID=='96' | SUBJID=='98' | SUBJID=='140' | SUBJID=='150' | SUBJID=='176' | SUBJID=='182' | SUBJID=='185' | SUBJID=='187' | SUBJID=='197' | SUBJID=='203'), am=10, vjust=-0.2, title="Logistic fit to log parasite count")
}

logistic.fit <- function(data) {
	subjects <- levels(data$SUBJID)
	n <- length(subjects)
	fits.df <- data.frame(SUBJID=array(dim=43), SEX=array(dim=43), CENTREID=array(dim=43), trttxt=array(dim=43), A=numeric(n), L=numeric(n), U=numeric(n), B=numeric(n))
	i = 1
	for (s in subjects) {
		fits.df[i, 1:4] <- data[data$SUBJID==s & data$plantm=='PRE-DOSE', c('SUBJID', 'SEX', 'CENTREID', 'trttxt')]
		fits.df[i, 5:8] <- NA
		fit <- NULL
		tryCatch(fit <- nls(log(1 + parct) ~ SSfpl(acttm, A, L, U, B), data=data, subset=SUBJID==s), error=function(e) e)
		if(!is.null(fit)) {
			fits.df[i, 5:8] <- coef(fit)
		}
		i <- i + 1
	}
	return(fits.df)
}

gglogistic <- function(data, fits, title="", xpos=45, am=1, vjust=0) {
	q <- gglog90(data, title, xpos, am, vjust)
	q + geom_hline(data=fits, aes(yintercept=A))
}
