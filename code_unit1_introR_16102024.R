###############################
# Libraries
###############################

library(readr)

#+++++++++++++++++++++++++++
# SESSION 2
#+++++++++++++++++++++++++++


# Load data
#-----------------------------------

rfm <- read_csv("rfm_raw.csv") 
load("rfm_raw.rda")


# Having a first look at your data
#-----------------------------------
View(rfm)  # the data viewer, comparable to what you see in Excel or SPSS


# Getting to know our data piece-wise
#-----------------------------------
dim(rfm)   		# shows the shape of the dataframe as ROWS COLS
nrow(rfm)		# shows number of rows, i.e. observations
ncol(rfm) 		# shows number of columns, i.e. No. of variables
names(rfm) 		# shows names of the variables
head(rfm)  		# OR head(rfm_clean, 20), shows first six rows of the dataframe OR a number you specify
tail(rfm)  		# OR tail(rfm_clean, 20), shows first six rows of the dataframe OR a number you specify
str(rfm)   		# shows the dataset structure – variable type and first values
summary(rfm$Age) 	# shows the variable summary statistics
unique(rfm$Class) 	# shows the distinct values of a variables


# Data cleaning after 1st exploration
#-----------------------------------


dim(rfm)
names(rfm)
str(rfm)
head(rfm, 5)
tail(rfm, 5)     # there are 3 rows of almost empty data 


rfm <- rfm[-c(25977:25979),]    #this removes these empty rows
tail(rfm, 5)

rfm <- rfm[ , -1] # first column provides no information in this case, can be deleted, too
rfm

summary(rfm) 		# provides a short overview (descriptives) of the variables

# there are 83 NAs in ArrivalDelayMinutes, with overall >29,000 obs, i.e. less then 5% --> can be deleted
# you might consider saving the shrunken dataset into a new object, e.g. rfm_noNA to retrace separate cleaning steps

# exclude the missings
(missing <- which(is.na(rfm$ArrivalDelayMinutes)))
rfm <- rfm[-missing,]   # row-wise deletion of observations with missing values


# and rounding monetary values for better comprehension :)
rfm$Monetary <- round(rfm$Monetary, 2)
rfm$Monetary

# Calculating Satisfaction
rfm$OverallSatisfaction <- rowMeans(rfm[,13:26])
rfm$Satisfaction <- ifelse(rfm$OverallSatisfaction > 3, "satisfied", "neutral or dissatisfied")
rfm$OverallSatisfaction
rfm$Satisfaction

#rename data

rfm_clean <- rfm
# save the cleaned data
write_csv(rfm_clean, "rfm_clean.csv")
save(rfm_clean, file="rfm_clean.rda")

#---------------------------------------------------------------

rfm_clean <- read_csv("rfm_clean.csv")
rfm_clean <- load("rfm_clean.rda")

#---------------------------------------------------------------
# Exercise: extend your logic checks
#
# This is quarterly data, is there a recency value > 90 days?

rfm_clean$Recency > 90
any(rfm_clean$Recency > 90)


# Is there a frequency value > 90 days?

any(rfm_clean$Frequency>90)
# Are there people older than 100 (highly unlikely age for flying)?

any(rfm_clean$Age > 80)
over80 <- which(rfm_clean$Age > 80)
length(over80)
rfm_clean[over80, ]





# Let's look at the data visually
#-----------------------------------



# Plotting a single, continuous variable
#-----------------------------------

# Histogram and boxplots look at the distribution of a variable
# You can learn where values are clustered, if there are outliers, or how the data distribution is shaped


# Histogram
hist(rfm_clean$Monetary, main = "Customer Spending", xlab = "Spending", breaks = 20) 


#Boxplot
boxplot(rfm_clean$Monetary, main = "Boxplot of Prices", ylab = "Price")


# Plotting a single, categorical variable
#-----------------------------------

# Bar or pie charts help to illustrate how groups are distribute
# You can learn which groups are more or less prominent, if one outperfroms the others, or or if unreasonable data is included

# Bar chart 
barplot(table(rfm_clean$Class),         
        main = "Class Flown",         
        ylab = "Count", xlab = "Class")

 


# Plotting two continuous variables 
#-----------------------------------

# Scatter plot
plot(rfm_clean$DepartureDelayMinutes, rfm_clean$ArrivalDelayMinutes,
     main = "Plot of Departure Delay vs Arrival Delay (in minutes)",
     xlab = "Departure Delay (minutes)",
     ylab = "Arrival Delay (minutes)")




# Preparing some variables for later usage: Age groups

rfm_clean$AgeBin <- NA
rfm_clean$AgeBin[rfm_clean$Age < 25] <- "< 25"
rfm_clean$AgeBin[rfm_clean$Age >= 25 & rfm_clean$Age <= 45] <- "25-45"
rfm_clean$AgeBin[rfm_clean$Age > 45] <- "> 45"

agebin_tb <- table(rfm_clean$AgeBin)
agebin_tb <- sort(agebin_tb)
print(agebin_tb)
barplot(agebin_tb, main = "Customers by Age Group",
        xlab = "Age Group", ylab = "Count", 
        ylim = c(0, 10000))


# Do age groups differ in distance flown, their spending behavior, or perceived onboard service satisfaction?

boxplot(FlightDistance ~ AgeBin, data = rfm_clean, 
        main = "Flight Distance by Age Group", 
        xlab = "Age Group", ylab = "Flight Distance")

boxplot(Monetary ~ AgeBin, data = rfm_clean, 
        main = "Spending by Age Group", 
        xlab = "Age Group", ylab = "Spending")

boxplot(SeatComfort ~ AgeBin, data = rfm_clean, 
        main = "Seat Comfort Satisfaction by Age Group", 
        xlab = "Age Group", ylab = "Seat Comfort Satisfaction")


# Is there a relationship between age and distance flown?

plot(rfm_clean$Age, rfm_clean$FlightDistance, main = "Relationship between Age and Flight Distance",
     xlab = "Age", 
     ylab = "Flight Distance")




