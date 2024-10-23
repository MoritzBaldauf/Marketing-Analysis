##### INITIALIZATION #####

# Load Data ---------------------------------------------------------------

load(rfm_data.rda)
load()


##### SECTION II #####

load("rfm_data.rda")

library(ggplot2)

ggplot(data = rfm_data, 
       mapping = aes(x = MONETARY)) +
  geom_histogram()

#or

ggplot(data = rfm_data) +
  geom_histogram(mapping = aes(x = MONETARY))



ggplot(data = rfm_clean, mapping = aes(x = MONETARY)) +  
  geom_histogram(binwidth = 1, fill = "#00a000", color = "#000000") +  
  ggtitle("Histogram of Age") +  
  scale_x_continuous(n.breaks = 20) +  
  scale_y_continuous(n.breaks = 10) +  
  theme_bw()




# Exercise I --------------------------------------------------------------
data(mtcars)

ggplot(mtcars, aes(x = mpg, y= hp, color=cyl))+ 
  geom_point() +
  stat_smooth(method = "lm", se= FALSE) +
  labs(title = "HP per MPG", x = "MPG", y = "HP") +
  theme_classic()


# Exercise II -------------------------------------------------------------

summary(ecomm_data)
library(dplyr)
library("ggpubr")

ecom_filter <- ecomm_data[!(is.na(ecomm_data$age) | ecomm_data$age==""), ]

Exit <- ecom_filter %>% 
  filter(surveyType == "At Exit") %>% 
  ggplot(aes(x= age)) +
  geom_bar() +
  scale_x_discrete(limits = c("Less than 18", "18-24","25-34", "35-44", "45-54", "55-65", "Prefer not to answer"))

At_Arrival_Only <- ecom_filter %>% 
  filter(surveyType == "At Arrival Only") %>% 
  ggplot(aes(x= age)) +
  geom_bar() + 
  scale_x_discrete(limits = c("Less than 18", "18-24","25-34", "35-44", "45-54", "55-65", "Prefer not to answer"))

At_Arrival_and_Exit <- ecom_filter %>% 
  filter(surveyType == "At Arrival and Exit") %>% 
  ggplot(aes(x= age)) +
  geom_bar() + 
  scale_x_discrete(limits = c("Less than 18", "18-24","25-34", "35-44", "45-54", "55-65", "Prefer not to answer"))

ggarrange(Exit, At_Arrival_Only, At_Arrival_and_Exit + rremove("x.text"), 
          labels = c("At Exit", "At Arrival Only", "At Arrival and Exit"),font.label = list(size= 7), 
          ncol = 3, nrow = 1)

# SECTION II - CLUSTERING -------------------------------------------------


library(scales)

load("rfm_data.rda")

dim(rfm_data)
head(rfm_data)

rfm_data$REC <- rescale(rfm_data$RECENCY, to=c(0,1))
rfm_data$FRE <- rescale(rfm_data$FREQUEN, to=c(0,1))
rfm_data$MON <- rescale(rfm_data$MONETARY, to=c(0,1))

head(rfm_data)

rfm_km <- rfm_data[, 6:8]

set.seed(42)
km_rfm1 <- kmeans(rfm_km, centers = 3, nstart = 20)
km_rfm1


library(dplyr)
library(ggplot2)

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(42)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km_rfm <- kmeans(rfm_km, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km_rfm$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',3),'#FF0000', rep('#000000', 6))
  )

km_rfm2 <- kmeans(rfm_km, centers = 4, nstart = 20)
km_rfm2

km_rfm2$cluster

rfm_data$cluster <- km_rfm2$cluster

head(rfm_data)

# static
library(scatterplot3d)
scatterplot3d(rfm_data[,2:4], pch=20, color=rainbow(4)[rfm_data$cluster])

# interactive
library(rgl)
plot3d(rfm_data[,2:4], col=rainbow(4)[rfm_data$cluster])


# Section III - models and eval -------------------------------------------

load("ecomm_data.rda")

dim(ecomm_data)

head(ecomm_data)

summary(ecomm_data)

str(ecomm_data)
#zoom in on some potentially interesting vars
#demographics
table(ecomm_data$age)

#make one "unknown" category
ecomm_data$Cage <- ecomm_data$age
levels(ecomm_data$Cage)[levels(ecomm_data$age)==""] <- "unknown"
levels(ecomm_data$Cage)[levels(ecomm_data$Cage)=="Prefer not to answer"] <- "unknown"
levels(ecomm_data$Cage)[is.na(levels(ecomm_data$Cage))==T] <- "unknown"

#check
table(ecomm_data$Cage)

#reorder factor levels

ecomm_data$Cage <- factor(ecomm_data$Cage, levels(ecomm_data$Cage)[c(1,8,2:7)])
plot(ecomm_data$Cage, main="Age distribution of the customers")

#do the same for gender: merge unknowns
ecomm_data$Cgender <- ecomm_data$gender
levels(ecomm_data$Cgender)[levels(ecomm_data$gender)==""] <- "unknown"
levels(ecomm_data$Cgender)[levels(ecomm_data$gender)=="Prefer not to answer"] <- "unknown"

plot(ecomm_data$Cgender)

plot(ecomm_data$whenSiteUsed)
#rearrange levels
ecomm_data$whenSiteUsed <- factor(ecomm_data$whenSiteUsed, levels(ecomm_data$whenSiteUsed)[c(2,1,3:5)])

#test two types of models
#prepare data set
set.seed(2017)
intrain<- runif(dim(ecomm_data)[1],0,1)>0.3

training<- ecomm_data[intrain,]
testing<- ecomm_data[intrain==F,]


glm1 <- glm(as.factor(behavAnySale)==1 ~  Cgender + Cage + relevel(whenSiteUsed,ref="Never. This is my first visit") 
            +(behavNumVisits) , data=testing,family = binomial)
summary(glm1)



install.packages("party")
library(party)
tree1<- ctree(as.factor(behavAnySale)==1 ~  Cgender+ Cage + 
                relevel(whenSiteUsed,ref="Never. This is my first visit") +(behavNumVisits), 
              training)
plot(tree1)

#make prediction
#logistic
pred_glm <- predict(glm1, testing,type="response")

print("Confusion Matrix for glm1")
table(pred_glm>0, Actual = testing$behavAnySale)
table(pred_glm>0.2, Actual = testing$behavAnySale)
table(pred_glm>0.5, Actual = testing$behavAnySale)

install.packages("ROCR")
library(ROCR)


#get accuracy
predL  <- prediction(pred_glm,testing$behavAnySale)
perfL_acc <-performance(predL, measure = "acc")
summary(perfL_acc@y.values[[1]])

#true positive rate
perfL_tpr <-performance(predL, measure = "tpr")
summary(perfL_tpr@y.values[[1]])


pred_tree <- predict(tree1, testing)

print("Confusion Matrix for Decision Tree")
table(pred_tree>0, Actual = testing$behavAnySale)
table(pred_tree>0.2, Actual = testing$behavAnySale)
table(pred_tree>0.5, Actual = testing$behavAnySale)

#get accuracy
predT  <- prediction(pred_tree,testing$behavAnySale)
perfT_acc <-performance(predT, measure = "acc")
summary(perfT_acc@y.values[[1]])

#true positive rate
perfT_tpr <-performance(predT, measure = "tpr")
summary(perfT_tpr@y.values[[1]])

#combine tpr and fpr for both models

perfL <- performance(predL,"tpr","fpr")
perfT <- performance(predT,"tpr","fpr")

plot( perfL, main="ROC", col="blue") 
plot(perfT, add = TRUE, col="green")
abline(coef = c(0,1),col="red")
legend("bottomright",c("logit","tree"),fill=c("blue","green"))

#get area under the curve

aucL = performance(predL, measure = "auc")
aucL@y.values

aucT = performance(predT, measure = "auc")
aucT@y.values


#lift chart

#logistic
breaks <- quantile(pred_glm, probs = seq(0, 1, 0.1))
testing$GlmScore  <- as.numeric(as.character(cut(pred_glm, 
                                                 breaks = unique(breaks), # we want our breaks as quantiles
                                                 right = FALSE, # ntervals should be open on the right (and closed on the left)
                                                 include.lowest = TRUE, # include our lowest interval (1)
                                                 labels = (length(unique(breaks))-1):1))) # give lables from 1 to 5

aggr.glm <- aggregate(testing$behavAnySale~as.factor(testing$GlmScore),FUN=mean)
colnames(aggr.glm)<-c("Classes","Response")


par(mfrow=c(2,1))
barplot(aggr.glm[,2], names.arg = aggr.glm$Classes,main="Response rates",xlab="Classes")
abline(h=mean(testing$behavAnySale)*1)

#add lift
aggr.glm$Lift <- aggr.glm[,2]/mean(testing$behavAnySale)*1
plot(aggr.glm[,3],type="l",main="Lift chart", xlab="Classes",ylab="",ylim=c(0,2.5))


#look at sizes
aggr.sizes <- aggregate(testing$behavAnySale~as.factor(testing$GlmScore),FUN=length)
barplot(aggr.sizes[,2], names.arg = aggr.glm$Classes,main="Customer segments",xlab="Classes")

#repeat for tree
breaks <- quantile(pred_tree, probs = seq(0, 1, 0.1))
testing$TreeScore  <- as.numeric(as.character(cut(pred_tree, 
                                                  breaks = unique(breaks), # we want our breaks as quantiles
                                                  right = FALSE, # ntervals should be open on the right (and closed on the left)
                                                  include.lowest = TRUE, # include our lowest interval (1)
                                                  labels = (length(unique(breaks))-1):1))) # give lables from 1 to 5

aggr.tree <- aggregate(testing$behavAnySale~as.factor(testing$TreeScore),FUN=mean)
colnames(aggr.tree)<-c("Classes","Response")

par(mfrow=c(2,1))
barplot(aggr.tree[,2], names.arg = aggr.tree$Classes,main="Response rates",xlab="Classes")
abline(h=mean(testing$behavAnySale)*1)

#add lift
aggr.tree$Lift <- aggr.tree[,2]/mean(testing$behavAnySale)*1
plot(aggr.tree[,3],type="l",main="Lift chart", xlab="Classes",ylab="",ylim=c(0,2.5))


aggrt.sizes <- aggregate(testing$behavAnySale~as.factor(testing$TreeScore),FUN=length)
barplot(aggrt.sizes[,2], names.arg = aggr.tree$Classes,main="Customer segments",xlab="Classes")

#look at sizes
aggregate(testing$behavAnySale~as.factor(testing$TreeScore),FUN=length)


#why should we care about type of model?

table(testing$TreeScore<2,testing$GlmScore<5)

#select top 200 according to each model

testing$topGlm <-0
testing$topGlm[rank(-pred_glm)<=200]<- 1

testing$toptree <-0
testing$toptree[rank(-pred_tree)<=200]<- 1

table(testing$topGlm,testing$toptree)

#compare modeling approach to "rfm" segmentation

testing$recency <- 5
testing$recency[testing$whenSiteUsed=="In the past week"] <-1
testing$recency[testing$whenSiteUsed=="In the past month"] <-2
testing$recency[testing$whenSiteUsed=="In the past year"] <-3
testing$recency[testing$whenSiteUsed=="More than a year ago"] <-4

table(testing$recency)

breaks <- quantile(testing$behavNumVisits, probs = seq(0, 1, 0.1))
testing$frequency  <- as.numeric(as.character(cut(testing$behavNumVisits, 
                                                  breaks = unique(breaks), # we want our breaks as quantiles
                                                  right = FALSE, # ntervals should be open on the right (and closed on the left)
                                                  include.lowest = TRUE, # include our lowest interval (1)
                                                  labels = (length(unique(breaks))-1):1))) # give lables from 10 to 1, the lower frequency the higher numvisits



par(mfrow=c(2,1))
plot(table(testing$recency,testing$frequency),
     main="'RM'-segmentation", ylab="FSCORE", xlab="RSCORE")

testing$RF <- testing$recency+testing$frequency
plot(table(testing$RF))


agg_rf <- aggregate(testing$behavAnySale~as.factor(testing$RF),FUN=mean)
colnames(agg_rf) <-c("RFMScore","ResponseRate")


par(mfrow=c(2,1))
barplot(agg_rf[,2], names.arg = agg_rf$RFMScore, main="Response rates across segments")
abline(h=mean(testing$behavAnySale))

#add lift
agg_rf$Lift <- agg_rf[,2]/mean(testing$behavAnySale)
plot(agg_rf[,3],type="l", main="Lift", ylab="")
abline(h=1)

plot(pred_glm,testing$RF,
     main="modeling vs segmentation", ylab="RSCORE+FSCORE", xlab="model prediction")



#how does targeted customer differ from overall?

#make a summary of the top   customers

summary(testing[,c(37,38,24,33,36)])
summary(subset(testing[,33],testing$topGlm==1))
table(subset(testing[,37],testing$topGlm==1))/dim(subset(testing,testing$topGlm==1))[1] 
table(subset(testing[,38],testing$topGlm==1))/dim(subset(testing,testing$topGlm==1))[1] 
table(subset(testing[,24],testing$topGlm==1))/dim(subset(testing,testing$topGlm==1))[1] 

apply((testing)[,c(1,3,4,7)],2,mean)
table(testing[,37])/dim(testing)[1]
table(testing[,38])/dim(testing)[1]
table(testing[,24])/dim(testing)[1]

#above average segments
summary(subset(testing[,33],testing$GlmScore<5))
table(subset(testing[,37],testing$GlmScore<5))/dim(subset(testing,testing$GlmScore<5))[1] 
table(subset(testing[,38],testing$GlmScore<5))/dim(subset(testing,testing$GlmScore<5))[1] 
table(subset(testing[,24],testing$GlmScore<5))/dim(subset(testing,testing$GlmScore<5))[1] 




# Exercise III ------------------------------------------------------------









