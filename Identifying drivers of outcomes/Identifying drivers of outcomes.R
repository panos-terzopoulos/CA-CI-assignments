#Assignment 2
#rm(list = ls())

#Set my Working Directory
setwd("C:/Users/panos/OneDrive/Desktop/Assignments of CA-CI/Identifying drivers of outcomes")

#Upload the csv file
df<-read.csv("AirBnB_TravelerData.csv", sep=";")


#Task 1 

#Creating dummy variables for each categorcial variable  (1.3)
install.packages('fastDummies')
library("fastDummies")

df <- dummy_cols(df, select_columns = c('AlaskaFF', 'Promotion', 'CustomerEmail', "Address"))
df


#Logistic regression with all the variables 
res.airbnb <- glm( Choice ~ Age + Tickets + RoundTrip + AlaskaFF_1 + AlaskaFF_2 
                    + Promotion_1  + Promotion_2 + CustomerEmail_1 + CustomerEmail_3 + CustomerEmail_4
                    + Address_1 + Address_2 , data=df, family = binomial())
summary(res.airbnb)
#For interpretation 
round(exp(coef(res.airbnb)), 3)


#Logistic regression excluding the insignificant variables 
res.airbnb2=glm( Choice ~ Age +  + RoundTrip  + Promotion_1  + CustomerEmail_1 + CustomerEmail_3 
                + Address_1 + Address_2  
                ,data=df, family=binomial)
summary(res.airbnb2)
#For estimation 
round(exp(coef(res.airbnb2)), 3)


#Logistic regression with interaction terms 
res.airbnb3=glm( Choice ~ Age + Tickets + RoundTrip + AlaskaFF_1 + AlaskaFF_2 
                 + Promotion_1  + Promotion_2 + CustomerEmail_1 + CustomerEmail_3 + CustomerEmail_4
                 + Address_1 + Address_2  + Age*CustomerEmail_1 + Age*Address_2 + Promotion_1*CustomerEmail_1
                 + Age*Address_1 + Age*Address_2 + Promotion_1*CustomerEmail_1 + 
                   Promotion_1*CustomerEmail_3 + Promotion_1*Address_1 + Promotion_1*Address_2 
                 + CustomerEmail_1*Address_2 + CustomerEmail_3*Address_1 
                 + CustomerEmail_3*Address_2
                 ,data=df, family=binomial)
summary(res.airbnb3)
#For estimation 
round(exp(coef(res.airbnb3)), 3)


#Task 2

#likelihood ratio test
install.packages('lmtest')
library(lmtest)
lrtest(res.airbnb, res.airbnb2)

#McFadden’s Pseudo-R^2
install.packages('DescTools')
library('DescTools')
PseudoR2(res.airbnb, which = 'McFadden')
PseudoR2(res.airbnb2, which = 'McFadden')
#Adjusted McFadden’s Pseudo-R2
PseudoR2(res.airbnb, which = 'McFaddenAdj')
PseudoR2(res.airbnb2, which = 'McFaddenAdj')

#BIC
install.packages('flexmix')
library('flexmix')
BIC(res.airbnb)
BIC(res.airbnb2)


#Task 3
#Create the probability of responding to the email, with the same dataset 
df$prob <- predict(res.airbnb, df, type="response")
hist(df$pred_choice)
summary(df$pred_choice)
df$Choice1 <- ifelse(df$pred_choice>=0.5, 1,0)
table(df$Choice1)
table(df$Choice)

#Changing the variables values
df1 <- df
df1$Promotion<- ifelse(df$Promotion==1, "Promotion 1",
                       ifelse(df$Promotion==2, "Promotion 2","Promotion 3"))
df1$RoundTrip<- ifelse(df$RoundTrip==1, "one way Ticket","two ways Ticket")

df1$Address<- ifelse(df$Address==1, "Oregon State",
                       ifelse(df$Address==2, "Eugene or Springfield","Out of State"))


#Plotting the probability and the targeting                        
library(ggplot2)
ggplot(data = df1, aes(x = CustomerEmail, y = prob, fill = factor(RoundTrip))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_grid(Promotion~Address) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Address", y = "Choice Probability") +
  theme_classic() +
  theme(legend.position = "bottom")





#Creating a simulated dataset 
Ageee=c (20,38,56)

segment1 = data.frame(segment = 1:108,
                     Address = c("Oregon", "Eugene/Springfield", "Out of state"),
                     Address_1 = c(1, 0, 0),
                     Address_2 = c(0, 1, 0),
                     round_trip = rep(c("No", "Yes"), each = 3),
                     RoundTrip = rep(c(0, 1), each = 3),
                    CustomerEmail=rep(c("Gmail", "edu","Yahoo","other")), 
                    CustomerEmail_1=c(1,0,0,0),
                    CustomerEmail_3=c(0,0,1,0),
                    CustomerEmail_4=c(0,0,0,1),
                    AlaskaFF=seq(c("not Member","Member","MVP")),
                    AlaskaFF_1=c(1,0,0),
                    AlaskaFF_2=c(0,1,0),
                    Promotion_1=1,
                    Promotion_2=0)

segment1$Age=mean(df$Age[df$Promotion==1])
segment1$Tickets=mean(df$Tickets[df$Promotion==1])


segment2 = data.frame(segment = 1:108,
                      Address = c("Oregon", "Eugene/Springfield", "Out of state"),
                      Address_1 = c(1, 0, 0),
                      Address_2 = c(0, 1, 0),
                      round_trip = rep(c("No", "Yes"), each = 3),
                      RoundTrip = rep(c(0, 1), each = 3),
                      CustomerEmail=rep(c("Gmail", "edu","Yahoo","other")), 
                      CustomerEmail_1=c(1,0,0,0),
                      CustomerEmail_3=c(0,0,1,0),
                      CustomerEmail_4=c(0,0,0,1),
                      AlaskaFF=seq(c("not Member","Member","MVP")),
                      AlaskaFF_1=c(1,0,0),
                      AlaskaFF_2=c(0,1,0),
                      Promotion_1=0,
                      Promotion_2=1)

segment2$Age=mean(df$Age[df$Promotion==2])
segment2$Tickets=mean(df$Tickets[df$Promotion==2])

segment3 = data.frame(segment = 1:108,
                      Address = c("Oregon", "Eugene/Springfield", "Out of state"),
                      Address_1 = c(1, 0, 0),
                      Address_2 = c(0, 1, 0),
                      round_trip = rep(c("No", "Yes"), each = 3),
                      RoundTrip = rep(c(0, 1), each = 3),
                      CustomerEmail=rep(c("Gmail", "edu","Yahoo","other")), 
                      CustomerEmail_1=c(1,0,0,0),
                      CustomerEmail_3=c(0,0,1,0),
                      CustomerEmail_4=c(0,0,0,1),
                      AlaskaFF=seq(c("not Member","Member","MVP")),
                      AlaskaFF_1=c(1,0,0),
                      AlaskaFF_2=c(0,1,0),
                      Promotion_1=0,
                      Promotion_2=0)

segment3$Age=mean(df$Age[df$Promotion==3])
segment3$Tickets=mean(df$Tickets[df$Promotion==3])

segment_final = rbind(segment1, segment2, segment3)
segment_final$prob=predict(res.airbnb, segment_final, type = "response")

hist(segment_final$prob)


#---------------------------------------


hist(Segmento$prob)
hist(Segmento$exp)


give_a_try = data.frame(segment = 1:72,
                      age = rep(c("Young", "Middle Age", "Old"), each=6),
                      Age=rep(agee, each=6),
                      CustomerEmail=rep(c("Gmail", "edu","Yahoo","other")), 
                      CustomerEmail_1=c(1,0,0,0),
                      CustomerEmail_3=c(0,0,1,0),
                      CustomerEmail_4=c(0,0,0,1),
                      Address=seq(c("Oregon","Eugene & Springfield","Outside the state")),
                      Address_1=c(1, 0, 0),
                      Address_2=c(0, 1, 0))

#segments$prob_acquire = predict(res.acquisition, segments, type = "response")
give_a_try$Choice=predict(res.try, give_a_try, type = "response")

table(df$Choice)

