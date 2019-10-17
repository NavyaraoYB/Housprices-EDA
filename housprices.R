################### QUESTION1


data<-read.csv("/Users/navyarao/Downloads/houseprices.csv")
mean1<-mean(data$Price)
sd1<-sd(data$Price)
n<-length(data$Price)
ss<-sd1/sqrt(length(data$Price))
x_bar<-(mean1-150000)/ss
df<-n-1
1-pt(x_bar,df)

mean2<-mean(data$Living.Area)
sd2<-sd(data$Living.Area)

ss<-sd2/sqrt(length(data$Living.Area))
x_bar1<-(mean2-1800)/ss
1-pt(x_bar1,df)





str(data)
data_Nofireplace<-subset(data,Fireplace==0,select = c(Price))
data_fireplace<-subset(data,Fireplace==1,select = c(Price))

boxplot(data_fireplace$Price,main="Fireplace")
boxplot(data_Nofireplace$Price,main="No Fireplace")

# Using base R graphics
lmts <- range(data_Nofireplace,data_fireplace)

par(mfrow = c(1, 2))
boxplot(data_Nofireplace,ylim=lmts)
boxplot(data_fireplace,ylim=lmts)
options(scipen = 1000)
mean_1<-mean(data_Nofireplace$Price)
mean_2<-mean(data_fireplace$Price)


sp=(nrow(data_fireplace)-1) * (sd(data_fireplace$Price)^2)+ (nrow(data_Nofireplace)-1) * (sd(data_Nofireplace$Price)^2)
df_fp = nrow(data_fireplace) + nrow(data_Nofireplace) -2
sdpooled= sqrt(sp/df_fp)
p_value <- pt((mean(data_Nofireplace$Price) - mean(data_fireplace$Price))/(sdpooled * sqrt((1/nrow(data_fireplace)) + (1/nrow(data_fireplace)) )), df_fp)




House_old <- subset(data,as.numeric(data$Age) <= 30)
House_new <- subset(data,as.numeric(data$Age) > 30)

mean_oldhouse <- mean(House_old$Lot.Size)
mean_newhouse <- mean(House_new$Lot.Size)
sd_oldhouse <- sd(House_old$Lot.Size)
sd_newhouse <- sd(House_new$Lot.Size)


SE <- sd_oldhouse^2 / nrow(House_old) + sd_newhouse^2/ nrow(House_new)
SE_sqr <-  SE^2   
denom <- (((sd_oldhouse^2)/nrow(House_old))^2 )/(nrow(House_old)-1) + (((sd_newhouse^2)/nrow(House_new))^2)/(nrow(House_new)-1)
df <- SE_sqr/denom
p_value <- pt((mean_newhouse-mean_oldhouse)/sqrt(sd_oldhouse^2/nrow(House_old) + sd_newhouse^2/nrow(House_new)),df)


housefireplace <-   subset(House_old,as.numeric(House_old$Fireplace) == 1)
prop_fireplace<-nrow(housefireplace)/nrow(House_old)
houseNofireplace <- subset(House_new,as.numeric(House_new$Fireplace) == 1)
prop_Nofireplace<-nrow(houseNofireplace)/nrow(House_new)
total_proportion <- (prop_fireplace * nrow(House_old) + prop_Nofireplace*nrow(House_new))/ (nrow(House_new) +nrow(House_old))
z = (prop_fireplace-prop_Nofireplace)/sqrt(total_proportion* (1-total_proportion)*((1/nrow(House_old)) + ((1/nrow(House_new)))))
p_value <- pnorm(z)

data$HouseType[(data$Bedrooms<3)] <- "Small"
data$HouseType[(data$Bedrooms>2& data$Bedrooms <= 4)] <- "Med"
data$HouseType[(data$Bedrooms > 4 &  data$Bedrooms <= 6)] <- "Large"
anova <- aov(data$Price~data$HouseType)
summary(anova)









