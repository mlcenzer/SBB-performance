rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/")

data_all<-read.csv("data/flight_summary_latest2.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data_all<-data_all[data_all$flew!="",]

data_all$flew_b<-0
data_all$flew_b[data_all$flew=="Y"]<-1

data_all$host_c[data_all$host_plant=="K. elegans"]<-1
data_all$host_c[data_all$host_plant=="C. corindum" | data_all$host_plant=="C. corindum "]<- -1

data_all$sex_c<--1
data_all$sex_c[data_all$sex=="F"]<-1

data_all$lat_c<-data_all$latitude-mean(data_all$latitude)

data_all$sym_dist<-abs(data_all$latitude-25.49197)

data_all$eggs_b<-0
data_all$eggs_b[data_all$eggs=="Y"]<-1

data_all$ID<-as.factor(data_all$ID)

data_all$min_from_start<-0

for(row in 1:length(data_all$days_from_start)){
	minute<-as.numeric(strsplit(strsplit(data_all$time_start[row], ":")[[1]][2], '[ap]m'))
	hour<-as.numeric(strsplit(data_all$time_start[row], ":")[[1]][1])
	if(hour>=7){
		data_all$min_from_start[row]<-60*(hour-8)+minute
	}	
	if(hour<7){
		data_all$min_from_start[row]<-60*(hour+4)+minute
	}
}

data_all$min_from_start_c<-data_all$min_from_start-mean(data_all$min_from_start)

data_flew<-data_all[data_all$distance!=0 & data_all$flew_b==1,]


library(lme4)
####Check for convergence without splitting by trial type, as speeds look normally distributed and may be more cooperative
test_model<-lmer(average_speed~host_c*sex_c*sym_dist+ (1|trial_type) + (1|ID), data=data_flew) #it converges! Run one set of models with trial type as a random factor.


#######testing some covariates:
model_test<-lmer(average_speed~chamber + (1|ID) + (1|trial_type), data=data_flew)
summary(model_test) ###Possibly a reduction in speed in chamber B-1! Same chamber with possible distance issues - we should check this one for levelness, I have seen bugs struggling to move even when they are flying in this one.

#######No effect of test date
summary(lmer(average_speed~days_from_start + (1|ID) + (1|trial_type), data=data_flew))

#######No effect of test time
summary(lmer(average_speed~min_from_start + (1|ID) + (1|trial_type), data=data_flew))


data<-data.frame(R=data_flew$average_speed, A=data_flew$host_c, B=data_flew$sex_c, C=data_flew$sym_dist, X=data_flew$ID, Y=data_flew$trial_type, Z=data_flew$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glmer 2-RF + 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###The top three models (holding a combined 97% probability) contain only the random effects of trial type and ID; no effects of host, sex, or distance from the sympatric zone on speed, hands down.

m36<-lmer(R~(1|X) + (1|Y), data=data)

###Try adding chamber to see if that improves fit
chamber_model<-lmer(R~(1|X) + (1|Y) + (1|Z), data=data) 
AIC(chamber_model)###It does! Not something necessarily to report in the paper, as it is not biologically interesting, but we should definitely look at chamber B-1



