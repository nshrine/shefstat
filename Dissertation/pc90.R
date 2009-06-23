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