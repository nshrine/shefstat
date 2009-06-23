getbwdata<-function(x) {
	d<-data.frame()
	count<-1
	for (i in x) {
		 r<-resid(i)
		 l<-length(r)
		 t<-names(x)[count]
		 d<-rbind(d,data.frame(pt=rep(t,l),resid=r))
		 count<-count+1
	}
	d
}

mint<-function(t) {
	return((y-predict(fit,data.frame(acttm=t)))^2)
}

find90<-function() {
	subjs<-levels(factor(SUBJID))
	l<-length(subjs)
	i<-1
	result<-1:l
	for (fit in malaria3.lmlist) {
		pre<-parct[SUBJID==subjs[i]][1]
		y<-log(1+(0.1*pre))
		assign("y",y,pos=1)
		assign("fit",fit,pos=1)
		opt<-optimize(mint,0:50)
		result[i]<-opt$minimum
		i<-i+1
	}
	result
}