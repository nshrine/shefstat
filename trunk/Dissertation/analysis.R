pc90aov <- function(data, title="", ...) {
	q <- qplot(trttxt, PC90.loglin, data=data, xlab="", ylab="PC90 (hours)", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q <- q + stat_summary(fun.data="mean_cl_boot", width=0.3, geom="crossbar", aes(colour=trttxt), ...)
	q + facet_grid(CENTREID~SEX, margins=T)
}

pc90ancova <- function(data, title="", ...) {
	q <- qplot(acttm, PC90.loglin, data=data, xlab="", ylab="PC90 (hours)", main=title, geom="blank")
	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q + facet_grid(CENTREID~SEX, margins=T, scales="free")
}

