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



###Break up by trial type
data_short<-data_flew[data_flew$trial_type=="T15" | data_flew$trial_type=="T25" | data_flew$trial_type=="T30",]

#######testing link functions
model_test<-glm(distance~chamber, data=data_short, family=Gamma(link="log")) #equivalent to using log link function in Gamma is to log transform distance, except it won't fuss about non-0 values
summary(model_test)
plot(model_test)

###30 minutes trials:

#######Possible effect of chamber? B-1 looks weird anyway.
summary(glm(distance~chamber, data=data_short, family=Gamma(link="log")))

#######No effect of test date
summary(glm(distance~days_from_start, data=data_short, family=Gamma(link="log")))

#######No effect of test time
summary(glm(distance~min_from_start, data=data_short, family=Gamma(link="log")))


data<-data.frame(R=data_short$distance, A=data_short$host_c, B=data_short$sex_c, C=data_short$sym_dist, X=data_short$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glmer 3-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#4 models did not converge: m11, m14, m33, m34
#top models: m16, m9, m17, m12, m14

anova(m9, m12, test="Chisq") #No effect of adding B
anova(m12, m16, test="Chisq") #No effect of B*C interaction
anova(m16, m17, test="Chisq") #No effect of adding A*B interaction
anova(m12, m14, test="Chisq") #No effect of A*B interaction
####So, let's use m9! No model with chamber was in the top 5.

model30<-glm(distance~host_c*sym_dist, family=Gamma(link="log"), data=data_short)
###Strong negative effect of sym_dist: longer dispersal distances closer to the sympatric zone
###Strong positive interaction between host and sym_dist: the effect of distance from the sympatric zone on dispersal was weaker on GRT bugs

summary30<-aggregate(distance~host_plant*sym_dist, data=data_short, FUN=mean)
#summary30BV<-aggregate(distance~sym_dist, data=data_short[data_short$host_c==-1,], FUN=mean)
plot(summary30$distance~summary30$sym_dist, pch=c(19,22)[as.factor(summary30$host_plant)])
#plot(summary30BV$distance~summary30BV$sym_dist)








###60 minutes trials:
data_hour<-data_flew[data_flew$trial_type=="T60",]

#######No chamber effects
summary(glm(distance~chamber, data=data_hour, family=Gamma(link="log")))

#######No effect of test date
summary(glm(distance~days_from_start, data=data_hour, family=Gamma(link="log")))

#######No effect of test time
summary(glm(distance~min_from_start, data=data_hour, family=Gamma(link="log")))


data<-data.frame(R=data_hour$distance, A=data_hour$host_c, B=data_hour$sex_c, C=data_hour$sym_dist, X=data_hour$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glm 3-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#5 models did not converge: m8, m10, m11, m13, m15; this link function may not be the best. Trying again with identity and inverse.
source("generic models-Gamma glm 3-FF inverse link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###All converged with inverse - but the results don't make any sense.
#top models: m8, m14, m9, m11, m17, m1
source("generic models-Gamma glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#####Not converging


anova(m8, m11, test="Chisq") #No effect of adding C
anova(m14, m11, test="Chisq") #No effect of A*C interaction
anova(m9, m12, test="Chisq") #No effect of adding B
anova(m14, m17, test="Chisq") #No effect of B*C interaction
###So, let's use m8

model60<-glm(distance~host_c*sex_c, family=Gamma(link="inverse"), data=data_hour)
summary(model60)
###Marginal effect of host plant: bugs from GRT disperse farther
###Marginal effect of sex: males disperse farther
###Marginal interaction between host and sex: the effect of host is weaker on males

#######I am not sure how to interpret these estimates, but the above interpretation is based on logical direction rather than coef(model60), which makes no sense to me.

summary60<-aggregate(distance~host_plant*sex, data=data_hour, FUN=mean)
#summary30BV<-aggregate(distance~sym_dist, data=data_hour[data_hour$host_c==-1,], FUN=mean)
plot(summary60$distance~c(1,2,1,2), col=c(1,2)[as.factor(summary60$sex)])
#plot(summary30BV$distance~summary30BV$sym_dist)













###90 minutes trials:
data_ninety<-data_flew[data_flew$trial_type=="T90",]

#######No chamber effects
summary(glm(distance~chamber, data=data_ninety, family=Gamma(link="inverse")))

#######No effect of test date
summary(glm(distance~days_from_start, data=data_ninety, family=Gamma(link="inverse")))

#######No effect of test time
summary(glm(distance~min_from_start, data=data_ninety, family=Gamma(link="inverse")))


data<-data.frame(R=data_ninety$distance, A=data_ninety$host_c, B=data_ninety$sex_c, C=data_ninety$sym_dist, X=data_ninety$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glm 3-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#5 models did not converge: m9, m12, m14, m17; this link function may not be the best. Trying again with identity and inverse.
source("generic models-Gamma glm 3-FF inverse link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###All converged with inverse
#top models: m14, m17, m9, m12, m16
source("generic models-Gamma glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#####Not converging, use inverse link.


anova(m14, m17, test="Chisq") #No effect of adding B*C interaction
anova(m14, m12, test="Chisq") #Strong effect of adding A*B interaction
anova(m9, m12, test="Chisq") #No effect of adding B alone
anova(m12, m16, test="Chisq") #No effect of B*C interaction
###So let's use m14

model90<-glm(distance~host_c*sex_c + host_c*sym_dist, family=Gamma(link="inverse"), data=data_ninety)
summary(model90)
###Moderate effect of sym_dist: Being close to the sympatric zone makes you disperse farther
###Moderate interaction between host and sym_dist: Being from BV --> stronger effect of being close to the sympatric zone

#######I am not sure how to interpret these estimates, but the above interpretation is based on logical direction rather than coef(model90), which makes no sense to me.

summary90<-aggregate(distance~host_plant*sym_dist, data=data_ninety, FUN=mean)
#summary30BV<-aggregate(distance~sym_dist, data=data_ninety[data_ninety$host_c==-1,], FUN=mean)
plot(summary90$distance~summary90$sym_dist, pch=c(19,22)[as.factor(summary90$host_plant)])
#plot(summary30BV$distance~summary30BV$sym_dist)














#####Keep ID number in here
######Distance will not converge, or shows nothing interesting, within this subset. Look here at flight type instead...
data_long<-data_flew[data_flew$trial_type=="Tlong",]

data_long$flight_type_b<-1
data_long$flight_type_b[data_long$flight_type=="B"]<-0


#######No chamber effects
summary(glmer(flight_type_b~chamber + (1|ID), data=data_long, family=binomial))

#######No effect of test date
summary(glmer(flight_type_b~days_from_start + (1|ID), data=data_long, family=binomial))

#######Marginal effect of test time: later starting bugs don't get as long to fly
summary(glmer(flight_type_b~min_from_start_c + (1|ID), data=data_long, family=binomial))


data<-data.frame(R=data_long$flight_type_b, A=data_long$host_c, B=data_long$sex_c, C=data_long$sym_dist, D=data_long$min_from_start_c, X=data_long$ID) ##Won't converge with D, drop for now

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###Not converging: m25, m28, m31, m32, m34
###Really nothing here either.







#######No chamber effects
summary(glmer(distance~chamber + (1|ID), data=data_long1, family=Gamma(link="log")))

#######No effect of test date
summary(glmer(distance~days_from_start + (1|ID), data=data_long, family=Gamma(link="log")))

#######YES effect of test time: later starting bugs don't get as long to fly
summary(glm(distance~min_from_start_c, data=data_long, family=Gamma(link="log")))

summarylong<-aggregate(distance~ID, FUN=mean, data=data_long)
summarylong$n<-aggregate(distance~ID, FUN=length, data=data_long)$distance
summarylong


data<-data.frame(R=data_long$distance, A=data_long$host_c, B=data_long$sex_c, C=data_long$sym_dist, D=data_long$min_from_start_c, X=data_long$ID)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glmer 4-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)

########Serious issues of non-convergence when attempting to include ID as a random factor. Try, for now, looking only at the first measurement for each individual. 
###This ended up being unhelpful.
data_long1<-data_long[1,]
for(row in 1:length(data_long$ID)){
	new_row<-length(data_long1$ID)+1
	if(data_long$ID[row] %in% data_long1$ID) print("Hooray!")
	else{
		data_long1[new_row,]<-data_long[row,]
	}
}
data<-data.frame(R=data_long1$distance, A=data_long1$host_c, B=data_long1$sex_c, C=data_long1$sym_dist, D=data_long1$min_from_start_c)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glm 4-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)




###Try subsetting by those that spent less than 30 minutes on the mill vs. those that did not; this is actually not possible with the current data format, so split by burst vs. not (which is essentially the same):

data_cont<-data_long[data_long$flight_type!="B",]

summarycont<-aggregate(distance~ID, FUN=mean, data=data_cont)
summarycont$n<-aggregate(distance~ID, FUN=length, data=data_cont)$distance
summarycont
#######No continuous fliers were measured more than once. Hmmm....

#######No chamber effects
summary(glm(distance~chamber, data=data_cont, family=Gamma(link="log")))

#######No effect of test date
summary(glm(distance~days_from_start, data=data_cont, family=Gamma(link="log")))

#######No effect of start time
summary(glm(distance~min_from_start_c, data=data_cont, family=Gamma(link="log")))

data<-data.frame(R=data_cont$distance, A=data_cont$host_c, B=data_cont$sex_c, C=data_cont$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glm 3-FF inverse link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#Top  model is m0

###Within this subset, there is no effect of anything. But also, there is very little power to detect anything, given the limited number of bugs that made it to this point.

summarycont<-aggregate(distance~sex, data=data_cont, FUN=mean)
summarycont$n<-aggregate(distance~sex, FUN=length, data=data_cont)$distance
summarycont





data_burst<-data_long[data_long$flight_type=="B",]

summaryburst<-aggregate(distance~ID, FUN=mean, data=data_burst)
summaryburst$n<-aggregate(distance~ID, FUN=length, data=data_burst)$distance
summaryburst

#######No chamber effects
summary(glm(distance~chamber, data=data_burst, family=Gamma(link="log")))

#######Appears to be an effect of test date, but only when accounting for ID
summary(glm(distance~days_from_start, data=data_burst, family=Gamma(link="log")))

#######Appears to be an effect of start time, but unable to run this while controlling for ID
summary(glm(distance~min_from_start_c, data=data_burst, family=Gamma(link="log")))

###Take a look:
summary_min<-aggregate(distance~min_from_start_c, data=data_burst, FUN=mean)
summary_min

####I suspect both of these effects are real. However, we do not have the power to include them in the overall model structure; because our factors of interest were randomized with respect to these, they should not alter our predictions for effect size. But, we should be aware of them.

data<-data.frame(R=data_burst$distance, A=data_burst$host_c, B=data_burst$sex_c, C=data_burst$sym_dist, X=data_burst$ID)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glmer 3-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
##Except for m20, m30, and m32, any model containing the random effect of individual did not work. Which is sadly not okay.






