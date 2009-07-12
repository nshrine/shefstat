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
