rm(list = ls())

if (!require(MASS)) install.packages('MASS'); require(MASS)

if (!require(dplyr)) install.packages('dplyr'); require(dplyr) 

if (!require(forcats)) install.packages('forcats'); require(forcats)

if (!require(vcdExtra)) install.packages('vcdExtra'); require(vcdExtra)

if (!require(pROC)) install.packages('pROC'); require(pROC)

if (!require(RColorBrewer)) install.packages('RColorBrewer'); require(RColorBrewer)

if (!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)

if (!require(ggcorrplot)) install.packages('ggcorrplot'); require(ggcorrplot)

if (!require(car)) install.packages('car'); require(car)


data<-read.csv("E-Commerce.csv",header = T)
head(data)
colnames(data)<-c("ID","Warehouse","Shipment","care_calls","rating","cost",
                  "prior_purchases","importance","gender","discount","grams","Y")
head(data)
data<-data %>% select(-ID,-Warehouse)
data$rating<-as.factor(data$rating)
str(data)
summary(data)

data_chr <- data %>% select(Shipment,importance,gender,rating)
data_num <- data %>% select(-c(Shipment,importance,gender,rating))
data_num <- sapply(data_num, as.numeric) %>% as.data.frame()

E_commerce <- cbind(data_chr, data_num)
E_commerce$discount <- E_commerce$discount/100
str(E_commerce)
head(E_commerce)
# ----------------------------------------------
# preprocessing
# check missing values
sum(is.na(data))

# ----------------------------------------------
# EDA
pastel<-brewer.pal(8,"Pastel1")
table(E_commerce$importance,E_commerce$Y)
# drawing bar plot using function
drawbarplot<-function(x,title,xlab){
                barplot(table(E_commerce$Y,x),main=title,xlab=xlab,ylab='count',
                legend=c('reached','not reached'),col=pastel[c(2,1)])
}

str(E_commerce)
# data visualization for categorical varibles
x11()
drawbarplot(E_commerce$Shipment,'배송방법에 따른 도착여부','mode of shipment')
drawbarplot(E_commerce$importance,'제품의 중요성에 따른 도착여부','product importance')
drawbarplot(E_commerce$gender,'구매자의 성별에 따른 도착여부','gender')
drawbarplot(E_commerce$care_calls,'발송 문의 전화 수에 따른 도착여부','calling')
drawbarplot(E_commerce$rating,'고객등급에 따른 도착여부','rating')

# ----------------------------------------------
# check multicollinearity for numeric variables
x11(); ggcorrplot(data %>% select_if(is.numeric) %>% cor(),
           lab = T,
           hc.order = T)
# ----------------------------------------------
# ----------------------------------------------
# Testing independent
# for nominal variable
attach(E_commerce)
str(E_commerce)
chisq.test(Shipment,Y) #Y와 독립
chisq.test(gender,Y) #Y와 독립

# for ordinal variable
attach(E_commerce)
table(importance,Y)%>%CMHtest(rscores=c(1,2,3))
table(care_calls,Y)%>%CMHtest(rscores=c(2,3,4,5,6,7))
table(rating,Y)%>%CMHtest(rscores=c(1,2,3,4,5)) #Y와 독립
table(prior_purchases,Y)%>%CMHtest(rscores=c(2,3,4,5,6,7,8,10))
detach(E_commerce)

# T-test for continuous variable
attach(E_commerce)
leveneTest(cost,as.factor(Y))
t.test(cost~as.factor(Y),var.equal =T)
leveneTest(discount,as.factor(Y))
t.test(discount~as.factor(Y),var.equal =F)
leveneTest(grams,as.factor(Y))
t.test(grams~as.factor(Y),var.equal=F)
detach(E_commerce)

# ----------------------------------------------
# fit glm
fit <- glm(Y~importance+care_calls+prior_purchases+cost+discount+grams,family = binomial,data = E_commerce)
summary(fit)
1-pchisq(14834-12012,10998-10991)
# multicollinearity
vif(fit)

# ----------------------------------------------
# stepwise variable selection: AIC algorithm
stepAIC(fit)
# ----------------------------------------------
prop <- sum(as.numeric(as.character(E_commerce$Y)))/nrow(E_commerce)
predicted <- as.numeric(fitted(fit) > prop)
c_t <- xtabs(~E_commerce$Y+predicted)
sensitivity<-c_t[1,1]/(c_t[1,1]+c_t[1,2])
sensitivity
specificity<-c_t[2,2]/(c_t[2,1]+c_t[2,2])
specificity
(c_t[1,1]+c_t[2,2])/sum(c_t)

# ----------------------------------------------
# ROC curve
rocplot <- roc(Y~fitted(fit),data=E_commerce)
x11(); plot.roc(rocplot,legacy.axes=T)
auc(rocplot)
