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

gglog90 <- function(data, title="", xpos=45, am=1, vjust=-0.1, ...) {
	q <- rawggplot(data, title, ...)
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

addPClines <- function(q, percentage=90, logplot=F, xpos=45, am=1, vjust=-0.1) {
	data <- q$data
	if (logplot) {
		data$parct <- exp(data$parct) - 1
	}
	frac <- 1 - (percentage/100)
	pc.df <- subset(data, select=c(SUBJID, parct), subset=plantm=='PRE-DOSE')
	pc <- ifelse(rep(logplot, nrow(pc.df)), log((pc.df$parct + 1) * frac), pc.df$parct * frac)
	pc.df <- data.frame(pc.df, pc=pc, xpos=xpos, vjust=vjust)
	q <- q + geom_hline(aes(yintercept=pc), data=pc.df, linetype=2)
	label <- paste("PC", percentage)
	q + geom_text(aes(x=xpos, y=pc, vjust=vjust), label=label, data=pc.df[am,])
}

plotraw90 <- function() {
	ggplot90(malaria.1M, am=8, vjust=-0.1, title="Parasite counts for Centre 1 Males with PC90 level shown")
}

# Add cubic fit to plot q
addcubicfit <- function(q, ...) {
	q + stat_smooth(method="lm", formula=y~x+I(x^2)+I(x^3), data=uptofirstzero(q$data), fullrange=F, se=F, ...) 
}


# Add logistic fit to plot q
addlogfit <- function(q, ...) {
	q + stat_smooth(method="nls", formula="y ~ SSfpl(x, A, L, U, B)", se=F, ...)
		# start="list(A=max(y), L=min(y), U=x[which.min(abs(y-((max(y)+min(y))/2)))], B=2)", se=F)
}

addlogparms <- function(q, fits) {
	subjs <- unique(q$data$SUBJID)
	fits <- subset(fits, subset=SUBJID %in% subjs)
	q <- q + geom_segment(data=fits, aes(x=min(acttm), y=A, xend=U, yend=A), colour='red', linetype=2)
	q <- q + geom_segment(data=fits, aes(x=U, y=L, xend=max(acttm), yend=L), colour='red', linetype=2)
	q <- q + geom_segment(data=fits, aes(x=U, y=L, xend=U, yend=A), colour='red', linetype=2)
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

cubicsresid <- function() {
	predose.resid(stdres(cubics.lmlist), b=0.5, limits=c(-2,5), title="Standardized residuals from cubic fits")
}

plotresids <- function(data, model, resids, fits, ...) {

	if (missing(resids)) {
		if (inherits(model, "lmList")) 
			resids <- unlist(lapply(model, stdres))
		if (inherits(model, "list")) 
			resids <- unlist(lapply(model, residuals, type='pearson'))
	}

	if (missing(fits)) {
		fits <- unlist(lapply(model, fitted))
	}

	vp1 <- viewport(width=0.5, height=0.5, x=0.75, y=0.75)
	q1 <- qplot(sample=resids, stat="qq")
	y <- quantile(resids, c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	q1 <- q1 + geom_abline(intercept=int, slope=slope, linetype=2)
	print(q1, vp=vp1)

	vp2 <- viewport(width=0.5, height=0.5, x=.25, y=0.25)
	q2 <- qplot(data$acttm, resids, xlab="Time (hours)", ylab="Standardized residuals") + geom_hline(yintercept=0, linetype=2)
	print(q2, vp=vp2)

	vp3 <- viewport(width=0.5, height=0.5, x=.75, y=.25)
	q3 <- qplot(fits, resids, xlab="Fitted", ylab="Standardized residuals")  + geom_hline(yintercept=0, linetype=2)
	print(q3, vp=vp3)
	
	vp4 <- viewport(width=0.5, height=0.5, x=.25, y=.75)
#	q4 <- qplot(fitted(model), log(1+data$parct), xlab="Fitted", ylab="Actual") + geom_abline(intercept=0, slope=1, linetype=2)
	q4 <- qplot(resids, xlab="Standardized residuals", geom="blank") + geom_histogram(fill="white", colour="black", ...)
	print(q4, vp=vp4)
}

comparefits <- function(subjs) {
	q <- getrawplot(subset(malaria, subset=SUBJID %in% subjs)) + geom_point(shape=1)
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T)
	q <- addcubicfit(q, colour='blue')
	q <- q + stat_function(fun=function(x) SSfpl(x, 8.3909, -0.1511, 25.0694, 2.0991))
	q <- q + geom_vline(xintercept=4.5474043, lty=3, colour=4)
	q <- q + geom_vline(xintercept=17.212508, lty=3, colour=1)
	q <- q + geom_text(aes(x=5,y=4,label="cubic", angle=90))
	q + geom_text(aes(x=17.6,y=4,label="logistic", angle=90))
}

# Add log-linear interpolation to plot q
addloglin <- function(q, ...) {
	dat <- q$data
	dat$parct <- exp(q$dat$parct) - 1
	dat <- by(dat, dat$SUBJID, getAboveBelow)
	data <- data.frame()
	for (s in unique(q$data$SUBJID)) {
		data <- rbind(data, q$data[q$data$SUBJID==s,][dat[[s]],])
	}
	q + geom_line(data=data, ...)
}

# Add vertical lines showing 3 PC90 estimates
addPC90vlines <- function(q, dat, colours=c("blue", "red", "green")) {
	dat <- subset(dat, subset=SUBJID %in% unique(q$data$SUBJID))
#	q <- q + geom_vline(data=dat, aes(xintercept=c(PC90.cubic, PC90.logistic, PC90.loglin), linetype=..xintercept..)) 
	q <- q + geom_vline(data=dat, aes(xintercept=PC90, colour=method), linetype=2) + scale_colour_manual(values=colours)
#	q <- q + geom_text(data=dat, aes(x=PC90.cubic, y=2, label="cubic", angle=90, hjust=-1))
#	q + geom_text(data=dat, aes(x=PC90.logistic, y=2, label="logistic", angle=90, hjust=1))
	q
}

# All 3 PC90 estimation methods on same plot
comparePC90 <- function(dat, pc90, logfit=T, ...) {
	q <- getrawplot(dat) + geom_point(shape=1)
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T)
	q <- addcubicfit(q, colour="blue")
	if (logfit) {
		tryCatch(q <- addlogfit(q, colour="red", ...), error=function(e) {})
	}
	q <- addloglin(q, colour="green")
	addPC90vlines(q, pc90) + facet_wrap(~SUBJID, ncol=2)
}

plotresids.PC90 <- function(model, outliers, ...) {
	residuals <- stdres(model)
	data <- data.frame(model$model, outlier="")
	class(data$outlier) <- "character"
	data$outlier <- ""
	if (!missing(outliers)) {
		for (s in outliers) {
			data$outlier[data$SUBJID==s] <- s
		}
	}

	vp1 <- viewport(width=0.5, height=0.5, x=0.75, y=0.75)
	q1 <- qplot(sample=residuals, stat="qq") #+ geom_text(label=data$outlier, stat="qq", colour="red", size=4)
	print(q1, vp=vp1)

	vp2 <- viewport(width=0.5, height=0.5, x=.25, y=0.25)
	q2 <- qplot(model$model$method, residuals, xlab="Method", ylab="Standardized residuals", geom="blank") + geom_boxplot(outlier.shape=NA) + geom_jitter() + geom_text(label=data$outlier, hjust=-0.1, colour="red", size=4) + geom_hline(yintercept=0, linetype=2)
	print(q2, vp=vp2)

	vp3 <- viewport(width=0.5, height=0.5, x=.75, y=.25)
	q3 <- qplot(fitted(model), residuals, xlab="Fitted", ylab="Standardized residuals")  + geom_hline(yintercept=0, linetype=2) + geom_text(label=data$outlier, hjust=-0.1, colour="red", size=4)
	print(q3, vp=vp3)
	
	vp4 <- viewport(width=0.5, height=0.5, x=.25, y=.75)
#	q4 <- qplot(fitted(model), log(1+data$parct), xlab="Fitted", ylab="Actual") + geom_abline(intercept=0, slope=1, linetype=2)
	q4 <- qplot(residuals, xlab="Standardized residuals", geom="blank") + geom_histogram(fill="white", colour="black", ...)
	print(q4, vp=vp4)
}

plotcorrs <- function() {
	qplot(PC90.mean, PC90.diff, data=subset(PC90.diffs, subset=SUBJID!='183' & SUBJID!='509'), xlab="Mean PC90 estimate (hours)", ylab="Difference in PC90 estimates (hours)", facets=.~methods)
}

spread.acttm <- function(data) {
	plantms <- as.character(unique(data$plantm[data$plantm!="PRE-DOSE"]))
	sapply(plantms, acttm.resid, data=data)
}

acttm.resid <- function(plantm, acttm) {
	pt <- as.numeric(strsplit(plantm, " ")[[1]][1])
	acttm - pt
}

plotlogisticresidualswithout0data <- function(...) {
	plotresids(malaria.fit[malaria.fit$parct>0,], logistic.lmlist, unlist(lapply(logistic.lmlist, residuals, type='pearson'))[malaria.fit$parct>0], unlist(lapply(logistic.lmlist, fitted))[malaria.fit$parct>0], ...)
}

splitresids <- function(data, residuals, ...) {
	q <- qplot(trttxt, residuals, data=data, geom="blank")
#	q <- q + geom_point(aes(colour=trttxt), position=position_jitter(w=0.05)) + scale_colour_discrete("Treatment")
	q <- q + geom_boxplot(aes(colour=trttxt)) + scale_colour_discrete("Treatment")
#	q <- q + stat_summary(fun.data="mean_cl_boot", size=3, geom="point", solid=T)
	q + facet_grid(CENTREID~SEX, margins=T)
}

pc90.multilevel <- function() {
	qplot(Method, PC90, data=PC90.reshape, geom="jitter", position=position_jitter(w=0.5), facets=Centre~Sex, margins=T) + stat_summary(fun.data="mean_cl_boot", width=0.5, geom="crossbar", size=1, aes(colour=Treatment), position="dodge")
	qplot(Method, PC90, data=PC90.reshape, geom="boxplot", facets=Centre~Sex, margins=T, fill=Treatment)
}

PC90.by.subjects <- function(...) {
	vp1 <- viewport(width=0.5, x=0.25)
	q <- PC90.split.methods(...)
	print(q, vp=vp1)
	vp2 <- viewport(width=0.5, x=0.75)
	q <- PC90.split.methods(between=F, ...)
	print(q, vp=vp2)
}

PC90.split.methods <- function(between=T, facets) {
	q <- NULL
	if (between) {
		q <- qplot(Method, resid(PC90methods.between.aov), data=PC90.reshape[!is.na(PC90.reshape$PC90),], main="Between Subjects", ylab="PC90 - stratum mean (hours)", geom="blank", ylim=c(-4,4)) 
	} else {
		q <- qplot(Method, PC90.s, data=PC90.reshape, main="Within Subjects", ylab="PC90 - subject mean (hours)", geom="blank", ylim=c(-4,4))
	}
	if (missing(facets)) {
		q <- q + stat_boxplot(width=0.3)
	} else {
		q <- q + stat_boxplot(width=0.3,) #+ geom_point(aes(colour=Treatment), position=position_dodge(width=0.5)) # + stat_boxplot(width=0.5, aes(colour=Treatment, outlier.colour="grey"))
#		q <- q + stat_summary(fun.data="median_hilow", conf.int=0.5, geom="crossbar", width=0.4, aes(colour=Treatment), position="dodge")
		q <- q + facet_grid(facets)
	}
	q
}

comparelog <- function(data, title="", centre="", r1=4, r2=4, points=T, lines=T) {
    vp1 <- viewport(width=0.47, x=0, just="left")
    q <- getrawplot(data)
    q <- addtrtgeoms(q, points, lines)
    q <- addPC90lines(q, xpos=40)
    q <- q + opts(title=paste(centre, "Untransformed counts"), legend.position='none')
    q <- q + facet_wrap(~SUBJID, nrow=r1, scales="free_y")
    print(q, vp=vp1)

    vp2 <- viewport(width=0.53, x=1, just="right")
    q <- getrawplot(data)
    q <- addtrtgeoms(q, points, lines)
    q <- getlogplot(q)
    q <- addPC90lines(q, logplot=T, xpos=40)
    q <- q + opts(title=paste(centre, "Logarithmic transformation"))
    q <- q + facet_wrap(~SUBJID, nrow=r2, scales="free_y")
    print(q, vp=vp2)
}

# q1 <- comparePC90(subset(malaria, subset=SUBJID %in% subjs.fit & CENTREID=='Centre 1' & SEX=='Female'), PC90.reshape)
# q1 <- q1 + opts(title="Centre 1, Female")
# q2 <- comparePC90(subset(malaria, subset=SUBJID %in% c(subjs.nofit,183) & CENTREID=='Centre 1' & SEX=='Female'), PC90.reshape, F)
#rror in data.frame(pc90.df, pc90 = pc90, xpos = xpos, vjust = vjust) : 
# arguments imply differing number of rows: 0, 1
# q2 <- comparePC90(subset(malaria, subset=SUBJID %in% subjs.nofit & CENTREID=='Centre 1' & SEX=='Female'), PC90.reshape, F)
# q2 <- q2 + opts(legend.position="none") + facet_wrap(~SUBJID, ncol=1)
# q2 <- q2 + opts(title="No logistic fit")
# print(q1, vp=vp1)
# q1 <- q1 + facet_wrap(~SUBJID, ncol=3)
# print(q1, vp=vp1)
# print(q2, vp=vp2)
# export.eps("Afits1F.eps")

compdiffs <- function(data) {
	title <- 'cubic - logistic'
	vp1 <- viewport(width=0.5, x=0, height=0.33, y=1, just=c("left","top"))
	dat <- data[data$methods==title,]
	q <- gethist(dat, title)
	print(q, vp=vp1)

	vp2 <- viewport(width=0.5, x=1, height=0.33, y=1, just=c("right","top"))
	q <- getqq(dat, title)
	print(q, vp=vp2)

	title <- 'logistic - loglin'
	vp3 <- viewport(width=0.5, x=0, height=0.33, y=0.5, just=c("left","centre"))
	dat <- data[data$methods==title,]
	q <- gethist(dat, title)
	print(q, vp=vp3)

	vp4 <- viewport(width=0.5, x=1, height=0.33, y=0.5, just=c("right","centre"))
	q <- getqq(dat, title)
	print(q, vp=vp4)

	title <- 'cubic - loglin'
	vp5 <- viewport(width=0.5, x=0, height=0.33, y=0, just=c("left","bottom"))
	dat <- data[data$methods==title,]
	q <- gethist(dat, title)
	print(q, vp=vp5)

	vp6 <- viewport(width=0.5, x=1, height=0.33, y=0, just=c("right","bottom"))
	q <- getqq(dat, title)
	print(q, vp=vp6)
}

gethist <- function(dat, title) {
	q <- qplot(PC90.diff, geom='blank', data=dat, xlab="Difference in PC90 estimate (hours)", main=title)
	q <- q + geom_histogram(colour="black", fill="white", binwidth=1) 
	q + geom_vline(xintercept=0, linetype=2)
}

getqq <- function(dat, title) {
	q <- qplot(sample=PC90.diff, data=dat, stat="qq", main=title)
	y <- quantile(dat$PC90.diff, c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	q + geom_abline(intercept=int, slope=slope, linetype=2)
}
