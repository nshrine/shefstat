SetTheme <- function() {
	theme_set(theme_bw())
	theme_update(legend.key=theme_rect(colour=NA))
	theme_update(strip.background=theme_rect(fill=NA, colour=NA))
}

#
# Data
#

raw1 <- function() {
	rawggplot3(malaria2[malaria2$CENTREID=='Centre 1',], w1=0.525, w2=0.475)
}

raw2 <- function() {
	rawggplot3(malaria2[malaria2$CENTREID=='Centre 2',], w1=0.4, w2=0.6)
}

preaov <- function() {
	predoseaov2(malaria[malaria$plantm=='PRE-DOSE',])
}	

preaovres <- function() {
	fit <- aov(parct ~ CENTREID * SEX * trttxt, data=malaria, subset=plantm=="PRE-DOSE")
	plotresids.lm(fit, xlab="Fitted pre-dose count (hours)")	
}

logpreaovres <- function() {
	fit <- aov(log(parct) ~ CENTREID * SEX * trttxt, data=malaria, subset=plantm=="PRE-DOSE")
	plotresids.lm(fit, xlab="Fitted log pre-dose count")	
}

allaov <- function() {
	allaov2(malaria)
}


#
# Derivation
#

comprawlog <- function() {
	subjs <- c("Subject 295", "Subject 224", "Subject 183", "Subject 162", "Subject 80", "Subject 54", "Subject 140", "Subject 176", "Subject 185", "Subject 218", "Subject 262", "Subject 294")
	comparelog(malaria2[malaria2$SUBJID %in% subjs,])
}

cubics <- function(subjs = c('Subject 54','Subject 80','Subject 96','Subject 98','Subject 140','Subject 150','Subject 176','Subject 182','Subject 185','Subject 187','Subject 197','Subject 203')) {
    q <- rawggplot(subset(malaria2, subset=SUBJID %in% subjs), title="Cubic fit to log parasite count up to first 0 reading", lines=F)
#	q <- q + opts(legend.position="bottom")
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T, am=10)
	addcubicfit(q)
}

cubicsresid <- function() {
	plotresids(malaria.0, cubics.lmlist, resid(cubics.lmlist,'pearson'), binwidth=0.5)
}

logistics <- function(subjs = c('Subject 54','Subject 80','Subject 96','Subject 98','Subject 140','Subject 150','Subject 176','Subject 182','Subject 185','Subject 187','Subject 197','Subject 203')) {
	q <- rawggplot(subset(malaria2, subset=SUBJID %in% subjs), title="Logistic fit to log parasite count", lines=F)
	q <- getlogplot(q)
	q <- addPC90lines(q, logplot=T, am=10)
	addlogfit(q)
}

logisticresid <- function() {
	plotresids(malaria[malaria$SUBJID %in% subjs.fit,], logistic.lmlist, binwidth=0.5)
}

logparms <- function(fits=malaria2, subjs = c('Subject 96','Subject 150','Subject 197')) {
	q <- rawggplot(subset(malaria2, subset=SUBJID %in% subjs), title="Logistic fit to log parasite count", lines=F)
	q <- getlogplot(q)
	q <- addlogfit(q)
	q <- addlogparms(q, fits)
	q + opts(legend.position="none")
}

failures <- function(subjs=paste("Subject", subjs.nofit)) {
	q <- rawggplot(subset(malaria2, subset=SUBJID %in% subjs), title="Subjects for which logistic fitting fails")
	q <- getlogplot(q)
	addPC90lines(q, logplot=T, am=8)
}

pc90agree <- function() {
	subjs.agree <- paste("Subject", subjs.agree)
	comparePC90(malaria2[malaria2$SUBJID %in% subjs.agree,], PC90.reshape2)
}

pc90bad <- function() {
	subjs.bad <- paste("Subject", subjs.bad)
	comparePC90(malaria2[malaria2$SUBJID %in% subjs.bad,], PC90.reshape2, start=list(A=10, L=0, U=20, B=2))
}

pc90nofit <- function() {
	subjs.nofit <- paste("Subject", subjs.nofit)
	comparePC90(malaria2[malaria2$SUBJID %in% subjs.nofit,], PC90.reshape2, logfit=F)
}

methodsbysubject <- function() {
	PC90.by.subjects()
}

comparediffs <- function() {
	compdiffs(PC90.diffs)
}

pc90estcor <- function() {
	qplot(PC90.mean, PC90.diff, data=PC90.diffs, facets=.~methods, xlab="Mean PC90 estimate (hours)", ylab="Difference in PC90 estimates (hours)", ylim=c(-4,4))
}


#
# Analysis
#

pc90boxes.f <- function() {
	pc90boxes(PC90.loglin.long)
}

pc90interaction.f <- function() {
	pc90interaction(PC90.loglin.df)
}

aovloglinres <- function() {
	plotresids.lm(PC90.loglin.aov)
}

aovsqrtres <- function() {
	plotresids.lm(PC90.loglin2.aov)
}

aovresw <-function() {
	plotresids.lm(PC90.loglinwt.aov, weighted=T)
}

aovsqrtresw <- function() {
	plotresids.lm(PC90.loglin2wt.aov, weighted=T, trans=T)
}

aov2rwt <- function() {
	plotresids.lm(PC90.loglin2rwt.aov, weighted=T, trans=T)
}

predose.ancova <- function() {
	ancova.interactions()
}

pretime.ancova <- function() {
	ancova.interactions(T)
}

#compancova <- function() {
#	compare.ancova(compare.ancova.df)
#}

#compancova2 <- function() {
#	compare.ancova2(compare.ancova.df)
#}


#
# Alternative
#

pc50anova <- function() {
	pc50both(PC50.long, PC50.df)
}

pc99extrap <- function() {
	extrapplot()
}

f.477 <- function() {
	plot477()
}

cubicspline <- function() {
	plotfSmooth()
}

fdaresids <- function() {
	plotfresids(lprr2i.resids)
}

fdahistqq <- function() {
	predose.resid(lprr2i.resids$sr, title="Standardized residuals", b=0.5, limits=c(-4,7))
}

fdapermF <- function() {
	plotFperm(lprr2i.Fperm, 20, 0.31, 0.21)
}

fdcoef <- function() {
	plotfdcoefs(lprr2i.fRegress)
}

fdfitted <- function() {
	plotfdpred(lprr2i.plot.df)
}

fdspeed <- function() {
	plotfdpred(lprr2i.speed.pred) + opts(title="1st derivative with 95% confidence intervals")
}

#
# Appendices
#

Araw1M <- function() {
	q <- with(malaria2, gglog90(malaria2[CENTREID=='Centre 1' & SEX=='Male',])) + facet_wrap(~SUBJID, ncol=4)
	q + opts(title="Centre 1, Male")
}

Araw1F <- function() {
	q <- with(malaria2, gglog90(malaria2[CENTREID=='Centre 1' & SEX=='Female',]))
	q + opts(title="Centre 1, Female")
}

Araw2M <- function() {
	q <- with(malaria2, gglog90(malaria2[CENTREID=='Centre 2' & SEX=='Male',]))
	q + opts(title="Centre 2, Male")
}

Araw2F <- function() {
	q <- with(malaria2, gglog90(malaria2[CENTREID=='Centre 2' & SEX=='Female',]))
	q + opts(title="Centre 2, Female")
}

Afits1M <- function() {
	allfits('Centre 1', 'Male', ncol=4, width=0.78, height=0.33, start=list(A=10, L=0, U=20, B=2))
}

Afits1F <- function() {
	allfits('Centre 1', 'Female', width=0.75, height=0.67)
}

Afits2M <- function() {
	allfits('Centre 2', 'Male', ncol=2, width=0.67, height=0.67)
}

Afits2F <- function() {
	allfits('Centre 2', 'Female', width=0.75, height=1)
}
