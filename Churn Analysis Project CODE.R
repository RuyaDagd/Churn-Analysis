
library(readxl)
sales <- read_excel("Stat 493 - Project 1 - Data.xlsx", 
                    sheet = "SALES")
products <- read_excel("Stat 493 - Project 1 - Data.xlsx", 
                       sheet = "PRODUCTS")
customers <- read_excel("Stat 493 - Project 1 - Data.xlsx", 
                        sheet = "CUSTOMERS")

date<-sales
date$DateTime <- as.Date(date$DateTime, format = "%d%b%Y:%H:%M:%S")


### SPLIT (FIRST 9 MONTH)
library(dplyr)
first<-date%>%filter(DateTime<as.Date("2017-10-01"))
head(first)
tail(first)
# SPLIT (LAST MONTH)
last<-date%>%filter(DateTime>=as.Date("2017-10-01"))
head(last)
tail(last)
##### FIRST 6 MONTHS
active_six<-first%>%filter(DateTime<as.Date("2017-07-01"))
head(active_six)
tail(active_six)
unique(date$UserID)
unique(active_six$UserID)
df<-as.data.frame(unique(active_six$UserID))

yedinciay<-date%>%filter(DateTime<as.Date("2017-08-01"))%>%filter(DateTime>=as.Date("2017-07-01"))
yedisekiz<-date%>%filter(DateTime<as.Date("2017-09-01"))%>%filter(DateTime>=as.Date("2017-07-01"))
yedisekizdokuz<-date%>%filter(DateTime<as.Date("2017-10-01"))%>%filter(DateTime>=as.Date("2017-07-01"))

##########
df$churn1 <- ifelse(df$`unique(active_six$UserID)` %in% yedinciay$UserID,0,1)
df$churn1
df$churn2 <- ifelse(df$`unique(active_six$UserID)` %in% yedisekiz$UserID,0,1)
df$churn3 <- ifelse(df$`unique(active_six$UserID)` %in% yedisekizdokuz$UserID,0,1)
head(df)
tail(df)
##################
str(sales)
str(customers)
str(products)
sapply(sales, function(x) sum(is.na(x)))
sapply(customers, function(x) sum(is.na(x)))
sapply(products, function(x) sum(is.na(x)))
df<-as.data.frame(unique(active_six$UserID)) 

head(df)
extractfirstcol_act_six<- active_six[,-1]
birlesik<-merge(df, customers, by=1)
head(birlesik)
head(extractfirstcol_act_six)
extractfirst<-extractfirstcol_act_six[,c(-2,-3)]
head(extractfirst)
######

birl<-merge(birlesik, extractfirst, by=1)
head(birl)
data1<-birl[!duplicated(birl$`unique(active_six$UserID)`), ]
head(data1)
dim(data1)
str(data1)
sapply(data1, function(x) sum(is.na(x)))
#####

numeric_var <- sapply(data1, is.numeric)
corr_matrix <- cor(data1[, numeric_var])
library(corrplot)
corrplot(corr_matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")

### LOGISTIC REGRESSION
head(data1)
firstchurn<-data1[,c(-1,-3,-4,-5)]
secondchurn<-data1[,c(-1,-2,-4,-5)]
thirdchurn<-data1[,c(-1,-2,-3,-5)]
LogModel1 <- glm(as.factor(firstchurn$churn1) ~ .,family=binomial(link="logit"),data=firstchurn)
LogModel2 <- glm(as.factor(secondchurn$churn2) ~ .,family=binomial(link="logit"),data=secondchurn)
LogModel3 <- glm(as.factor(thirdchurn$churn3) ~ .,family=binomial(link="logit"),data=thirdchurn)
print(summary(LogModel1))
print(summary(LogModel2))
print(summary(LogModel3))
formula(LogModel1)
anova(LogModel1, test="Chisq")
anova(LogModel2, test="Chisq")
anova(LogModel3, test="Chisq")


table(data1$churn1)
str(data1)
prop.table(table(data1$churn1,data1$PaymentType))

#LAST 6 MONTH
head(last)
lastactive_six<-last%>%filter(DateTime<as.Date("2018-04-01"))%>%filter(DateTime>=as.Date("2017-10-01"))
head(lastactive_six)
tail(lastactive_six)
unique(date$UserID)
unique(active_six$UserID)
dflast<-as.data.frame(unique(lastactive_six$UserID))


lastyedinciay<-date%>%filter(DateTime<as.Date("2018-05-01"))%>%filter(DateTime>=as.Date("2018-04-01"))
lastyedisekiz<-date%>%filter(DateTime<as.Date("2018-06-01"))%>%filter(DateTime>=as.Date("2018-04-01"))
lastyedisekizdokuz<-date%>%filter(DateTime<=as.Date("2018-06-30"))%>%filter(DateTime>=as.Date("2018-04-01"))

##########

dflast$churn1 <- ifelse(dflast$`unique(lastactive_six$UserID)` %in% lastyedinciay$UserID,0,1)
dflast$churn1
dflast$churn2 <- ifelse(dflast$`unique(lastactive_six$UserID)` %in% lastyedisekiz$UserID,0,1)
dflast$churn3 <- ifelse(dflast$`unique(lastactive_six$UserID)` %in% lastyedisekizdokuz$UserID,0,1)
head(dflast)
tail(dflast)

lastextractfirstcol_act_six<- lastactive_six[,-1]

lastbirlesik<-merge(dflast, customers, by=1)
head(lastbirlesik)


head(lastextractfirstcol_act_six)
lastextractfirst<-lastextractfirstcol_act_six[,c(-2,-3)]
head(lastextractfirst)


lastbirl<-merge(lastbirlesik, lastextractfirst, by=1)
head(lastbirl)
lastdata<-lastbirl[!duplicated(lastbirl$`unique(lastactive_six$UserID)`), ]
head(lastdata)

lastfirstchurn<-lastdata[,c(-1,-3,-4,-5)]
lastsecondchurn<-lastdata[,c(-1,-2,-4,-5)]
lastthirdchurn<-lastdata[,c(-1,-2,-3,-5)]

#Assessing the predictive ability of the Logistic Regression model

fitted.results <- predict(LogModel1,newdata=lastfirstchurn,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != lastfirstchurn$churn1)
print(paste('Logistic Regression Accuracy',1-misClasificError))

#Logistic Regression Confusion Matrix

print("Confusion Matrix for Logistic Regression") 
table(lastfirstchurn$churn1, fitted.results > 0.5)
#Odds Ratio
library(MASS)
exp(cbind(OR=coef(LogModel1), confint(LogModel1)))


#Decision Tree

class(firstchurn$churn1)
firstchurn$churn1<- as.factor(firstchurn$churn1)
DT <- rpart(churn1 ~ Gender + Channel + PaymentType , firstchurn, method = "class", cp=0)
summary(DT)
#Variable importance says sex is the most important variable
#If we have cont. var. we should have tresholds, but for categorical var. should have dummies with respect to the categories
printcp(DT)
rpart.plot(DT, type=1, extra = 102)

class(secondchurn$churn2)
secondchurn$churn2<- as.factor(secondchurn$churn2)
D <- rpart(churn2 ~ Gender + Channel + PaymentType , secondchurn, method = "class", cp=0)
summary(D)

printcp(D)
rpart.plot(D, type=1, extra = 102)

class(thirdchurn$churn3)
thirdchurn$churn3<- as.factor(thirdchurn$churn3)
D3 <- rpart(churn3 ~ Gender + Channel + PaymentType , thirdchurn, method = "class", cp=0)
summary(D3)
printcp(D3)
rpart.plot(D3, type=1, extra = 102)


predict_unseen <-predict(DT, lastfirstchurn, type = 'class')
table_mat <- table(lastfirstchurn$churn1, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

predict_unseen2 <-predict(D, lastsecondchurn, type = 'class')
table_mat2 <- table(lastsecondchurn$churn2, predict_unseen2)
table_mat2
accuracy_Test2 <- sum(diag(table_mat2)) / sum(table_mat2)
print(paste('Accuracy for test', accuracy_Test2))

predict_unseen3 <-predict(D3, lastthirdchurn, type = 'class')
table_mat3 <- table(lastfirstchurn$churn1, predict_unseen3)
table_mat3
accuracy_Test3 <- sum(diag(table_mat3)) / sum(table_mat3)
print(paste('Accuracy for test', accuracy_Test3))

###################################
head(sales)
head(customers)
head(products)
tail(products)
#####

c<-merge(sales,products)
head(c)
head(lastdata)


#Explaratory Data Analysis
head(data)
dim
head(date)
eda<-merge(sales,customers)
head(eda)
eda1<-merge(eda,products)
head(eda1)
dim(eda1)
#table
table(eda1$Channel)
table(eda1$Gender)
table(eda1$Location)
table(eda1$Discount)

table(eda1$PaymentType)

table(eda1$Category)
str(eda1)
library(ggplot2)
Gender<- ggplot(eda1,aes(Gender,fill=Gender))+geom_bar(fill=c("rosybrown3","rosybrown4"))+theme(legend.position="none")
Gender
Channel<-ggplot(eda1,aes(Channel,fill=Channel))+geom_bar(fill=c("rosybrown3","rosybrown4"))+theme(legend.position="none")
Channel
Location<-ggplot(eda1,aes(Location,fill=Location))+geom_bar(fill=c("pink","pink1","pink2","pink3","rosybrown","rosybrown1","rosybrown2","rosybrown3","rosybrown4"))+theme(legend.position="none")
Location
PaymentType<-ggplot(eda1,aes(PaymentType,fill=PaymentType))+geom_bar(fill=c("rosybrown3","rosybrown4","rosybrown2"))+theme(legend.position="none")
PaymentType

# After Churn
Payment <- table(data1$PaymentType, data1$churn1)
Paymentprob <- round(prop.table(table(data1$PaymentType, data1$churn1), margin = 1), 2)
Paymentprob
# Create data.
data <- data.frame(
  category = c("Churned", "Not churned"),
  fraction=c(0.54, 0.46)
)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, " : ", data$fraction*100, "%")

# Make the plot
Cashplot <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=3, aes(y=labelPosition, label=label, color=category), size=5) + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("rosybrown2", "rosybrown3")) +
  scale_color_manual(values = c("black", "black")) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none") + theme(plot.caption = element_text(size = 15, 
                                                                      face = "bold", colour = "black", hjust = 0.5)) +labs(caption = "Cash")
Cashplot


# Create data.
data <- data.frame(
  category = c("Churned", "Not churned"),
  fraction=c(0.56, 0.44)
)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, " : ", data$fraction*100, "%")

# Make the plot
Mobilepayment <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=3, aes(y=labelPosition, label=label, color=category), size=5) + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("rosybrown2", "rosybrown3")) +
  scale_color_manual(values = c("black", "black")) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none") + theme(plot.caption = element_text(size = 15, 
                                                                      face = "bold", colour = "black", hjust = 0.5)) +labs(caption = "Mobile Payment")
Mobilepayment


# load library
library(ggplot2)

# Create data.
data <- data.frame(
  category = c("Churned", "Not churned"),
  fraction=c(0.61, 0.39)
)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, " : ", data$fraction*100, "%")

# Make the plot

Onlinecreditcard <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=3, aes(y=labelPosition, label=label, color=category), size=5) + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("rosybrown2", "rosybrown3")) +
  scale_color_manual(values = c("black", "black")) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none") + theme(plot.caption = element_text(size = 15, 
                                                                      face = "bold", colour = "black", hjust = 0.5)) +labs(caption = "Online Credit Card")
Onlinecreditcard


Cashplot; Mobilepayment; Onlinecreditcard
####################################
#Location
LocationTable <- table(data1$Location, data1$churn1)
LocationProbTable <- round(prop.table(table(data1$Location, data1$churn1), margin = 1),2)

locationdata <- data.frame(t(LocationProbTable))
colnames(locationdata) <- c( "Churn","Location", "Freq")
locationdata$Churn <- ifelse(locationdata$Churn == 1 , yes = "Churn", no = "Not churn")
locationdata$Churn <- factor(locationdata$Churn, levels = c("Churn", "Not churn"))
locationdata$label <- paste(locationdata$Freq*100, paste("%",sep=""),sep="")

# 0 means they rejected the subscription; we will talk about the ones accepted it.
locationdata$label <- ifelse(locationdata$Churn == "Churn", yes = locationdata$label, no = NA) # bunu sadece bikez cal??st??r

head(locationdata)

locationdata$Location <- as.character(locationdata$Location)
locationdata$Location <- factor(locationdata$Location, levels = as.character(rev(arrange(locationdata[is.na(locationdata$label) == F,], Freq)[,2])))
#as.character(rev(arrange(dayData[is.na(dayData$label) == F,], Freq)[,2]))



locationPlot<-ggplot(locationdata, aes(x = Location, y = Freq, fill = Churn, label = label)) + 
  geom_bar(stat = "identity") +
  geom_label(size = 3, position = position_stack(vjust = c(0, 1)),colour = "black",fontface = "bold.italic")+
  scale_fill_manual(values=c("rosybrown2", "rosybrown3")) + 
  theme_minimal() + 
  xlab(" ")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, face = "italic"), 
                                                      legend.title = element_text(size = 10, face = "italic")) + 
  labs(title = "", fill = "Churn Status") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(vjust = -5))+
  theme(legend.position = "bottom") +
  theme(axis.text.y = element_text(colour = "white")) + 
  coord_polar() + theme(axis.text.x = element_text(vjust = 1.25))
locationPlot

# Gender ve churn1
tablegender <- table(data1$churn1, data1$Gender)
tablegender <- round(prop.table(tablegender, margin = 2),2)
tablegender
plotgender <- data.frame(tablegender)



plotgender$label<- paste(plotgender$Freq*100, paste("%",sep=""),sep="")


library(ggplot2)

gender_plot <- ggplot(plotgender, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "black",fontface = "bold")+
  scale_fill_manual(values=c("rosybrown2", "rosybrown3")) + 
  theme_minimal() + 
  xlab("Churn situation")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Churn") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
gender_plot
### Channel
channelTable <- table(data1$Channel, data1$churn1)
channelprobtable <- round(prop.table(table(data1$Channel, data1$churn1), margin = 1), 2)



channeldata <- data.frame(channelprobtable)
colnames(channeldata) <- c("Channel", "Churn", "Value")
channeldata$Churn <- as.factor(ifelse(channeldata$Churn == 1 , yes = "Churn", no = "Not churn"))
channeldata$Channel <- as.factor(ifelse(channeldata$Channel == "MOBILE" , yes = "Mobile", no = "Web"))
channeldata$label <- paste(channeldata$Value*100, "%", sep = "")
channeldata




library(ggplot2)
channelPlot <- ggplot(channeldata, aes(Channel, Churn, label = label)) +
  geom_point(aes(size = Value), alpha=0.8, color = c("rosybrown2", "rosybrown2","rosybrown3", "rosybrown3"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("black", "black","black", "black"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "gray95"))+labs(x = "Channel Types")
channelPlot

#Customer Segmentation
CS<-merge(sales,products, by="ProductID")
all<-merge(CS,customers, by="UserID")

#Choose 2 category
prop.table(table(all$Category))
#Category1->Female Fashion more likely with 22% and 
#Category2->TVs and TV Sets 2nd more likely with 18%

all$category1bought<-ifelse(all$Category=="Female Fashion",1,0)
all$category1bought<-as.factor(all$category1bought)
class(all$category1bought)

all$category2bought<-ifelse(all$Category=="TVs and TV Sets",1,0)
all$category2bought<-as.factor(all$category2bought)
class(all$category2bought)
all<- all%>%
  mutate(Age = ifelse(is.na(Age), mean(all$Age, na.rm= T), Age),
         'Age_Group' = case_when(
           Age >= 19 & Age<30 ~ "19-29",
           Age >= 30 & Age<40 ~ "30-39",
           Age >= 40 & Age<=50 ~ "40-50"))
sum(is.na(all))
categ1<-all[,c(-2,-3,-4,-9,-10,-15)]
categ1$category1bought<-as.factor(categ1$category1bought)
categ1$Channel<-as.factor(categ1$Channel)
categ1$PaymentType<-as.factor(categ1$PaymentType)
categ1$Discount<-as.factor(categ1$Discount)
categ1$Category<-as.factor(categ1$Category)
categ1$Gender<-as.factor(categ1$Gender)
categ1$Age_Group<-as.factor(categ1$Age_Group)
str(categ1)

categ2<-all[,c(-1,-2,-3,-4,-10,-14)]
categ2$category2bought<-as.factor(categ2$category2bought)
categ2$Channel<-as.factor(categ2$Channel)
categ2$PaymentType<-as.factor(categ2$PaymentType)
categ2$Discount<-as.factor(categ2$Discount)
categ2$Category<-as.factor(categ2$Category)
categ2$Gender<-as.factor(categ2$Gender)
# LIBRARIES ----
library(parsnip)    # machine learning
library(rsample)  # testing/training splits
library(recipes)     # preprocessing
library(yardstick)   #  ROC/AUC
library(xgboost)    # xgboost
library(Ckmeans.1d.dp)  # feature importance plot
library(rpart)      # decision tree
library(rpart.plot) # decision tree plot
library(ggplot2) # Graphic
library(tidyverse) 

###
library(caTools)
set.seed(123)
s<-categ1[-sample(1:nrow(categ1), 65000), ]
sample_data = sample.split(s, SplitRatio = 0.5)
train_data <- subset(s, sample_data == TRUE)
test_data <- subset(s, sample_data == FALSE)

train_data$category1bought<-as.factor(train_data$category1bought)
train_data$Channel<-as.factor(train_data$Channel)
train_data$PaymentType<-as.factor(train_data$PaymentType)
train_data$Discount<-as.factor(train_data$Discount)
train_data$Category<-as.factor(train_data$Category)
train_data$Age_Group<-as.factor(train_data$Age_Group)
str(train_data)

prop.table(table(train_data$category1bought))
rtree <- rpart(category1bought ~ PaymentType+Gender+Age_Group , categ1, method = "class", cp=0)
summary(rtree)
printcp(rtree)
plotcp(rtree)
rpart.plot(rtree)
ctree_ <- ctree(category1bought ~ ., train_data)
plot(ctree_)

library(randomForest)
fit <- randomForest(category1bought ~ . , train_data)
print(fit) # view results
importance(dt_fit1)

library(tree)
tree.fit <- tree(,method="anova", data=categ1)
summary(tree.fit)
cv.trees <- cv.tree(tree.fit)
plot(cv.trees)

str(train_data)
dt_fit1 <- rpart(formula = category1bought ~ Price+ Age_Group+ PaymentType,
                 data = train_data,
                 method = "class", 
                 control = rpart.control(minsplit = 20, 
                                         cp = 0, 
                                         xval = 0),
                 parms = list(split = "gini"))
summary(dt_fit1)
rpart.plot(dt_fit1)

buyingcateg1<-categ1%>%filter(category1bought==1)

summary(buyingcateg1)
summary(categ1)

buyingcateg2<-categ2%>%filter(category2bought==1)
summary(buyingcateg2)

tablegender <- table(categ2$category2bought,categ2$Gender)
tablegender <- round(prop.table(tablegender, margin = 2),2)
tablegender
plotgender <- data.frame(tablegender)

plotgender$label<- paste(plotgender$Freq*100, paste("%",sep=""),sep="")

gender_plot <- ggplot(plotgender, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "black",fontface = "bold")+
  scale_fill_manual(values=c("rosybrown2", "rosybrown3")) + 
  theme_minimal() + 
  xlab("TVs and TV Sets")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Buying TVs and TV Sets") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
gender_plot

tablegender <- table(categ1$category1bought,categ1$Gender)
tablegender <- round(prop.table(tablegender, margin = 2),2)
tablegender
plotgender <- data.frame(tablegender)

plotgender$label<- paste(plotgender$Freq*100, paste("%",sep=""),sep="")

gender_plot <- ggplot(plotgender, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "black",fontface = "bold")+
  scale_fill_manual(values=c("rosybrown2", "rosybrown3")) + 
  theme_minimal() + 
  xlab("Female Fashion")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Buying Female Fashion") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
gender_plot

PaymentType<-ggplot(categ1,aes(x=Gender, y=category1bought))+geom_bar(fill=c("rosybrown3","rosybrown4"))+theme(legend.position="none")
PaymentType
head(categ1)

tableed <- table(categ1$category1bought, categ1$Location)
tableed <- round(prop.table(tableed, margin = 2),2)
tableed
detach(bank_full)

ploted <- data.frame(tableed)
ploted$label<- paste(ploted$Freq*100, paste("%",sep=""),sep="")


library(ggplot2)

ed_plot <- ggplot(ploted, aes(x = education, y = Freq, fill = subscription, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "white",fontface = "bold")+
  scale_fill_manual(values=c("#FF4C50", "#228B22")) + 
  theme_minimal() + 
  xlab("education")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "",fill = "Subsription") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
ed_plot

lastdata<-lastbirl[!duplicated(lastbirl$`unique(lastactive_six$UserID)`), ]
buyingcateg1$UserID<-as.factor(buyingcateg1$UserID)                                                                                                                                                       

a<-customers[,-2]
a<-a%>%group_by(UserID)
CS<-merge(sales,products, by=1)
cs<-CS[,c(-1,-3)]
cs<-cs%>%group_by(UserID)
all<-merge(cs,customers, by=1)

estimated_hist <- ggplot(buyingcateg1, aes(x =Price, fill = Discount)) +
  geom_histogram() +
  theme_minimal() +
  #scale_x_continuous(breaks = seq(0,255000,by=30000), labels = comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1))

estimated_box <- ggplot(buyingcateg1, aes(x = Gender, y = Price, fill = Gender)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')

estimated_hist | estimated_box

ggplot(buyingcateg2, aes(x =Price, fill = PaymentType)) +
  geom_histogram() +
  theme_minimal() +
  #scale_x_continuous(breaks = seq(0,255000,by=30000), labels = comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1))

plot(prop.table(buyingcateg1$Channel))
barplot(buyingcateg1$Channel, col = "white", border = "steelblue")
barplot(buyingcateg1$Channel, col = "white",
        border = c("#999999", "#E69F00", "#56B4E9"))
ggplot(buyingcateg1, aes(Channel, fill = Channel)) +
  geom_bar(fill=c("rosybrown3","rosybrown4") +
             theme(legend.position = 'none')