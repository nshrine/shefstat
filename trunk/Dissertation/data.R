malaria.1F <- subset(malaria, subset=CENTREID=='001' & SEX=='F')
malaria.1M <- subset(malaria, subset=CENTREID=='001' & SEX=='M')
malaria.2F <- subset(malaria, subset=CENTREID=='002' & SEX=='F')
malaria.2M <- subset(malaria, subset=CENTREID=='002' & SEX=='M')
malaria.1F <- malaria.1F[order(malaria.1F$trt),]
malaria.1M <- malaria.1M[order(malaria.1M$trt),]
malaria.2F <- malaria.2F[order(malaria.2F$trt),]
malaria.2M <- malaria.2M[order(malaria.2M$trt),]