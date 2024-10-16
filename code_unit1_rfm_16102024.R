load("ecomm_data.rda")

# How many observations and variables are in the e-commerce data set?
dim(ecomm_data)

#Check first rows
head(ecomm_data)

#Check structure
str(ecomm_data)

#Examine descriptives
library(psych)
describe(ecomm_data)
#or: summary(ecomm_data)

#Plotting some data
hist(ecomm_data$purchaseExpectInNextMonth ,
     main="Histogram of expected purchase intention in the next month",
     xlab="purchase intention",
 ylab="frequency" )

plot(ecomm_data$whenSiteUsed, main="Distribution of recent visits")

#Adjusting the order of the levels and save it as a new variable
ecomm_data$CwhenSiteUsed <- factor(ecomm_data$whenSiteUsed,
                                         levels(ecomm_data$whenSiteUsed)[c(2,1,3:5)])

plot(ecomm_data$CwhenSiteUsed, main="Distribution of recent visits")

aggregate(purchaseExpectInNextMonth~CwhenSiteUsed, data=ecomm_data, FUN=mean)

#is there a relationship between recency and purchase intention?
recency1 <- lm(purchaseExpectInNextMonth~relevel(CwhenSiteUsed,ref="Never. This is my first visit"), data=ecomm_data)
summary(recency1)

#look at frequency

hist(ecomm_data$behavNumVisits, main="Distribution of number of  visits", nclass=400)


aggregate(purchaseExpectInNextMonth~behavNumVisits,data=ecomm_data, FUN=mean)

#is there a relationship between frequency and purchase intention

frequency1 <- lm(purchaseExpectInNextMonth~behavNumVisits, data=ecomm_data)
summary(frequency1)

#use a transformation
frequency1a <- lm(purchaseExpectInNextMonth~log(behavNumVisits), data=ecomm_data)
summary(frequency1a)

#often companies create classes based on quantiles

breaks <- quantile(ecomm_data$behavNumVisits, probs = seq(0, 1, 0.2))
ecomm_data$FSCORE  <- as.numeric(as.character(cut(ecomm_data$behavNumVisits, 
                                                        breaks = unique(breaks), # we want our breaks as quantiles
                                                        right = FALSE, # ntervals should be open on the right (and closed on the left)
                                                        include.lowest = TRUE, # include our lowest interval (1)
                                                        labels = 1:(length(unique(breaks))-1)))) # give lables from 1 to 5
                                                 
frequency1c <- lm(purchaseExpectInNextMonth~as.factor(FSCORE), data=ecomm_data)
summary(frequency1c)

#recency and frequency

#Expected purchase intentions
aggr.pIntention.RF <- aggregate(purchaseExpectInNextMonth~CwhenSiteUsed+FSCORE,data=ecomm_data, FUN=mean)
table.pIntention.RF <- cbind(aggr.pIntention.RF[1:5,3],aggr.pIntention.RF[6:10,3])
colnames(table.pIntention.RF) <-c("FSCORE1","FSCORE2")
rownames(table.pIntention.RF) <-aggr.pIntention.RF[1:5,1]
table.pIntention.RF

##########################################################
##### RFM data set

load("rfm_data.rda")

#--------------------------------------------------#
# 5er Quantile Plots for RECENCY, FREQUEN and MONETARY                         
#--------------------------------------------------#


breaks <- quantile(rfm_data$RECENCY, probs = seq(0, 1, 0.2))

#create scores/segments
rfm_data$NRECENCY_5 <- cut(rfm_data$RECENCY, 
                       breaks = breaks, # we want our breaks as quantiles
                       right = FALSE, # ntervals should be open on the right (and closed on the left)
                       include.lowest = TRUE, # include our lowest interval (1)
                       labels = 1:(length(breaks)-1)) # give lables from 1 to 5

# REPEAT for FREQUEN 
breaks <- unique(quantile(rfm_data$FREQUEN, probs = seq(0, 1, 0.2)))

rfm_data$NFREQUEN_5 <- cut(rfm_data$FREQUEN, 
                       breaks = breaks, # we want our breaks as quantiles
                       right = FALSE, # ntervals should be open on the right (and closed on the left)
                       include.lowest = TRUE, # include our lowest interval (1)
                       labels = (length(breaks)-1):1) # give lables from 5 to 1 (we have to switch these, because we want the highest Frequencies to be in quantile 1)

# REPEAT for MONETARY
breaks <- unique(quantile(rfm_data$MONETARY, probs = seq(0, 1, 0.2)))

rfm_data$NMONETARY_5 <- cut(rfm_data$MONETARY, 
                        breaks = breaks, # we want our breaks as quantiles
                        right = FALSE, # ntervals should be open on the right (and closed on the left)
                        include.lowest = TRUE, # include our lowest interval (1)
                        labels = (length(breaks)-1):1) # give lables from 5 to 1 (we have to switch these, because we want the highest Frequencies to be in quantile 1)


#bar charts

par(mfrow=c(1,3))

# NRECENCY

aggr.rec <- aggregate(rfm_data$RESPONSE~factor(rfm_data$NRECENCY_5),FUN=mean)
colnames(aggr.rec)<-c("Classes","Response")


barplot(aggr.rec[,2], names.arg = aggr.rec$Classes,main="5er Quantiles for RECENCY and their Response Rate",xlab="Classes")
abline(h=mean(rfm_data$RESPONSE))


# NFREQUEN

aggr.fre <- aggregate(rfm_data$RESPONSE~factor(rfm_data$NFREQUEN_5,levels=rev(levels(rfm_data$NFREQUEN_5))),FUN=mean)
colnames(aggr.fre)<-c("Classes","Response")


barplot(aggr.fre[,2], names.arg = aggr.fre$Classes,main="5er Quantiles for FREQUENCY and their Response Rate",xlab="Classes")
abline(h=mean(rfm_data$RESPONSE))

# NMONETARY

aggr.mon <- aggregate(rfm_data$RESPONSE~factor(rfm_data$NMONETARY_5,levels=rev(levels(rfm_data$NMONETARY_5))),FUN=mean)
colnames(aggr.mon)<-c("Classes","Response")


barplot(aggr.mon[,2], names.arg = aggr.mon$Classes,main="5er Quantiles for MONETARY and their Response Rate",xlab="Classes")
abline(h=mean(rfm_data$RESPONSE))

# Creating a RFM Score ####
#================================================#
#================================================#

rfm_data$RFMScore <- as.numeric(as.character(rfm_data$NRECENCY_5))    +as.numeric(as.character(rfm_data$NFREQUEN_5))   +as.numeric(as.character(rfm_data$NMONETARY_5))
table(rfm_data$RFMScore)

agg_rfm <- aggregate(rfm_data$RESPONSE~as.factor(rfm_data$RFMScore),FUN=mean)
agg_rfm
colnames(agg_rfm) <-c("RFMScore","ResponseRate")

#create plots
par(mfrow=c(1,2))

barplot(agg_rfm[,2], names.arg = agg_rfm$RFMScore, ,xlab="RFM Score",main="Response rate across segments")
abline(h=mean(rfm_data$RESPONSE))

#add lift
agg_rfm$Lift <- agg_rfm[,2]/mean(rfm_data$RESPONSE)
plot(agg_rfm[,3],type="l", main="Lift across segments", ylab="",xaxt="n",xlab="RFM Score")
axis(side=1,at=c(2,4,6,8,10,12),labels=agg_rfm[as.numeric(as.character(agg_rfm[,1]))%%2==0,1])
abline(h=1)
