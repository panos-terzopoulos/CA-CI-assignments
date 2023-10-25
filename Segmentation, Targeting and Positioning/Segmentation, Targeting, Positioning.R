#Assignment 4 
setwd("C:/Users/panos/OneDrive/Desktop/Assignments of CA-CI/Segmentation, Targeting and Positioning")
pacman::p_load(ggplot2, dplyr, reshape2, 
               psych, corrplot, fpc, cluster,
               nnet, mclust, e1071, randomForest,
               ape, FactoMineR, factoextra, gridExtra, ggrepel)
#READ THE CSV
smartwatch <- read.csv("smartwatch_survey.csv")

head(smartwatch)
str(smartwatch)
summary(smartwatch)

#Creating dummy variables for each categorcial variable
install.packages('fastDummies')
library("fastDummies")

df <- dummy_cols(smartwatch, select_columns = c('Degree','Income'))
df
df <- df %>% 
  rename(Imp_WTP = "WTP")

#Colonne
ncol(smartwatch)
columns<-colnames(smartwatch)
columns<-columns[14:38]
columns


#Task 1--------------------------------------------------------------------------------------
#First step:Investigate the correlation structure and distribution of variables
which(colnames(df) == 'WTP')
which(colnames(df) == 'Income_1')



pairs.panels(df[,2:14], 
             method = "pearson",
             hist.col = "grey60",
             lm = TRUE, 
             ellipses = FALSE,
             cex.cor = 1.3,
             cex.labels = 0.5)#show low correlations?

#standardize all variables and save as a separate object
data = data.frame(scale(df[, 2:45]))
head(data)

summary(data)

#PCA: Only importance ratings variables starting with Imp
head(select(data, starts_with("Imp_")))

# note, I am using select function from dplyr package here, 
#to take only the variables that start with "Imp_"
res.pca_1 = PCA(select(data, starts_with("Imp_")))

eig.val_1 = get_eigenvalue(res.pca_1) # get eigenvalues
eig.val_1

fviz_eig(res.pca_1, addlabels = TRUE) # scree plot
#five dimensions?

# save the PCA scores
scores_1 = get_pca_ind(res.pca_1)

# retain only 5 dimensions
scores_1 = scores_1$coord[, 1:4]
rownames(scores_1) = df$id # use ids as rownames


head(scores_1)

# compute euclidean distances for hierarchical clustering
dist.eucl_1 <- dist(scores_1, method = "euclidean")
cl.ward_1 <- hclust(dist.eucl_1, method = "ward.D2")

#Only importance ratings as basis of segmentation:
# plot dendrogram
plot(as.dendrogram(cl.ward_1), 
     leaflab = "none", main = "HC-Ward: Importance Ratings") 

# How many clusters?
VRC.ward = sapply(c(2:10), function(x){
  cluster.stats(d = dist.eucl_1, clustering = cutree(cl.ward_1, x))$ch
})

# plot CH index
plot(x = 2:10, y = VRC.ward, type = "b", 
     xlab = "Number of Clusters",
     ylab = "Calinski-Harabasz index")


plot(as.dendrogram(cl.ward_1), 
     leaflab = "none", main = "HC-Ward: Importance Ratings")    
rect.hclust(cl.ward_1, k = 4, border = "darkred")#4 clusters?


# Visualize 5 clusters
fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cutree(cl.ward_1, k = 5)), 
             main = "HC_Ward: Importance ratings", 
             geom = "point", pointsize = 1)

# Visualize 4 clusters
                  # note: input is the standardize data here not the PCAs, 
fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  #       as this function conducts PCA already.
                  cluster = cutree(cl.ward_1, k = 4)), 
             main = "HC_Ward: Importance ratings", 
             geom = "point", pointsize = 1)
# Visualize 3 cluster
fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cutree(cl.ward_1, k = 3)), 
             main = "HC_Ward: Importance ratings", 
             geom = "point", pointsize = 1)
# Visualize 3 cluster
gg5<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cutree(cl.ward_1, k = 3)), 
             main = "HC_Ward: Importance ratings", 
             geom = "point", pointsize = 1)

# Visualize 2 cluster
gg6<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cutree(cl.ward_1, k = 2)), 
             main = "HC_Ward: Importance ratings", 
             geom = "point", pointsize = 1)






set.seed(185) 
cl.kmeans_1 <- kmeans(scores_1, centers = 3)
cl.kmeans_2 <- kmeans(scores_1, centers = 4)
cl.kmeans_3 <- kmeans(scores_1, centers = 5)

# cluster sizes:
table(cl.kmeans_1$cluster)
table(cl.kmeans_2$cluster)
table(cl.kmeans_3$cluster)

# visualize the 3 clusters
fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cl.kmeans_1$cluster), 
             main = "Kmeans: Importance ratings", 
             geom = "point", pointsize = 1)

# visualize the 4 clusters
fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                  # note: input is the standardize data here not the PCAs, 
                  #       as this function conducts PCA already.
                  cluster = cl.kmeans_2$cluster), 
             main = "Kmeans: Importance ratings", 
             geom = "point", pointsize = 1)


p1<- plot(x = 2:10, y = VRC.ward, type = "b", 
         xlab = "Number of Clusters",
         ylab = "Calinski-Harabasz index")


x = 2:10
y = VRC.ward

plot_data = data.frame(x = x, y = y)

p = ggplot(data = plot_data, aes(x = x, y = y)) + 
  geom_line(color = "blue") +
  xlab("Number of Clusters") +
  ylab("Calinski-Harabasz index")

# save the plot to a variable
plot_variable <- p
#plot 2 graphs sequentially 


gg1<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                       # note: input is the standardize data here not the PCAs, 
                       #       as this function conducts PCA already.
                       cluster = cutree(cl.ward_1, k = 2)), 
                  main = "HC_Ward: 2 Clusters", 
                  geom = "point", pointsize = 1)
gg2<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                       # note: input is the standardize data here not the PCAs, 
                       #       as this function conducts PCA already.
                       cluster = cutree(cl.ward_1, k = 3)), 
                  main = "HC_Ward: 3 Clusters", 
                  geom = "point", pointsize = 1)

gg3<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                       # note: input is the standardize data here not the PCAs, 
                       #       as this function conducts PCA already.
                       cluster = cl.kmeans_1$cluster), 
                  main = "Kmeans: 3 Clusters", 
                  geom = "point", pointsize = 1)
gg4<-fviz_cluster(list(data = select(data, starts_with("Imp_")), 
                       # note: input is the standardize data here not the PCAs, 
                       #       as this function conducts PCA already.
                       cluster = cl.kmeans_2$cluster), 
                  main = "Kmeans: 4 Clusters", 
                  geom = "point", pointsize = 1)
gridExtra::grid.arrange(arrangeGrob(gg1,gg2,gg3,gg4,ncol=2))

fviz_nbclust(scores_1, kmeans, method = 'wss')

# Graph for K-means 
f1<-fviz_nbclust(scores_1, kmeans, method = 'silhouette')
# WSS (Elbow criterion)
f2<-fviz_nbclust(scores_1, kmeans, method = 'wss')

gridExtra::grid.arrange(arrangeGrob(gg3,gg4,f1,f2,ncol=2))

# Graph for HC-Ward  


gridExtra::grid.arrange(arrangeGrob(gg5,gg6,ncol=2),plot_variable)
#CLUSTER EXPLANATION 
smartwatch$seg1<- cl.kmeans_1$cluster
smartwatch$seg2<- cl.kmeans_2$cluster
seg1
freq = smartwatch %>% 
  group_by(seg1) %>%
  summarise(size = n()/nrow(smartwatch), 
            Imp_Innov = mean(Imp_Innov), 
            Imp_ConstCom = mean(Imp_ConstCom),
            Imp_CreatCom = mean(Imp_CreatCom), 
            Imp_TimelyInf = mean(Imp_TimelyInf),
            Imp_SaveMT = mean(Imp_SaveMT),
            Imp_SaveML = mean(Imp_SaveML), 
            Imp_TaskMgm = mean(Imp_TaskMgm),
            Imp_DeviceSt = mean(Imp_DeviceSt),
            Imp_Photo = mean(Imp_Photo),
            Imp_Wellness = mean(Imp_Wellness),
            Imp_Athlete = mean(Imp_Athlete),
            Imp_Style = mean(Imp_Style),
            WTP = mean(WTP),
            iPhone = mean(iPhone),
            CompBuy = mean(CompBuy),
            Occup_Health = mean(Occup_Health),
            Occup_Finc = mean(Occup_Finc),
            Occup_Sales = mean(Occup_Sales),
            Occup_Cons = mean(Occup_Cons),
            Occup_Advt = mean(Occup_Advt),
            Occup_Finc = mean(Occup_Finc),
            Occup_Edu = mean(Occup_Edu),
            Occup_Eng = mean(Occup_Eng),
            Occup_Tech = mean(Occup_Tech),
            Occup_Retail = mean(Occup_Retail),
            Occup_SMB = mean(Occup_SMB),
            FB_Insta = mean(FB_Insta),
            Twit = mean(Twit),
            Snap = mean(Snap),
            YouTube = mean(YouTube),
            Pod_radio = mean(Pod_radio),
            TV = mean(TV),
            NewsP = mean(NewsP),
            AmznP = mean(AmznP),
            Age = mean(Age),
            Female = mean(Female),
            Degree = mean(Degree),
            Income = mean(Income),
            seg1 = mean(seg1)) %>%
  mutate_at(2:20, round, 2) # round columns except for 1st cluster allocation

freq = t(freq)[-1,]
freq
write.table(freq, file = "freq.txt", sep = ",", quote = FALSE, row.names = F)

#------------------------------------------------------------------------
#1.3 Rating 

freq=freq[c(1,14,38),]
freq
freq=t(freq)
freq
colMeans(freq)

size=freq[,1]
WTP=freq[,2]
Income=freq[,3]

size_st=0.4*5*(size -min(size))/(max(size)-min(size))
WTP_st=0.4*5*(WTP -min(WTP))/(max(WTP)-min(WTP))
Income_st=0.2*5*(Income -min(Income))/(max(Income)-min(Income))
size_st
WTP_st
rating=cbind(size_st,WTP_st,Income_st)
rating=t(rating)
rating
rating=colSums(rating)
rating

#------------------------------------------------------------------------

#DIFFERENCES BETWEEN the 3 CLUSTERS 
smartwatch_imp_long = melt(select(smartwatch, "Id", "seg1", starts_with("Imp_")), 
                        id.vars = c("Id", "seg1")) 

ggplot(data = smartwatch_imp_long, aes(x = value, fill = as.factor(seg1))) +
  geom_histogram(position = "dodge", 
                 breaks = seq(1, 7, by = 1)) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  facet_wrap(.~variable, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(x = "Rating", y = "Frequency", fill = "Segment")

#distribution 
ggplot(data = smartwatch_imp_long, aes(x = value, fill = as.factor(seg1))) +
  geom_boxplot(position = "dodge", 
                 breaks = seq(1, 7, by = 1)) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  facet_wrap(.~variable, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(x = "Rating", y = "Frequency", fill = "Segment")

#DIFFERENCES BETWEEN the 4 CLUSTERS 
smartwatch_imp_long = melt(select(smartwatch, "Id", "seg2", starts_with("Imp_")), 
                           id.vars = c("Id", "seg2")) 

ggplot(data = smartwatch_imp_long, aes(x = value, fill = as.factor(seg2))) +
  geom_histogram(position = "dodge", 
                 breaks = seq(1, 7, by = 1)) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  facet_wrap(.~variable, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(x = "Rating", y = "Frequency", fill = "Segment")


#-------------------------------------------------------------
#Task 2--------------------------------------------------------------------
  #CLASSIFICATION 
  #Training and test sets

smartwatch <- df %>% 
  rename(smartwatch = "Imp_WTP")

  data = smartwatch
set.seed(5)   # fix the seed for reproducibility
train.pop <- 0.65 # we will use 65-35% split
N <- nrow(data) # total sample size

# randomly sample 65% of observations
train.cases <- sample(N, N*train.pop)

# assign the randomly sampled 65% of obs. to training set
data.train <- data[train.cases, ] 
nrow(data.train)

# assign the rest to test set
data.test <- data[-train.cases, ]
nrow(data.test)
seg1

#--------------Variables Attibute---------------------------------------------
#Trial 1: Multinomial logistic regression-----------
unique(data$seg1)

# last level will be treated as reference
res_multinom <- multinom(seg1 ~ CompBuy+AmznP+Female+Degree+Age, 
                         data = data.train)
output = summary(res_multinom)
seg = paste(rownames(output$coefficients), "vs. segment 1")

# Coefficients
t(round(output$coefficients, 2))

# standard errors
t(round(output$standard.errors, 2))

# z-test
z <- output$coefficients/output$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test

# let combine for each segment in a separate table
tabPrint = lapply(c(1:length(seg)), function(x){
  temp = round(cbind(output$coefficients[x,], output$standard.errors[x,], z[x,], p[x,]), 2)
  colnames(temp) = c("Coefficient","Std. Errors","z stat","p value")
  return(temp)
})

# name the list elements
names(tabPrint) = seg
tabPrint

# predict for test data
data.test$seg_log <- predict(res_multinom, newdata = data.test)
head(data.test)

# hitrate
round(mean(data.test$seg1 == data.test$seg_log), 4) * 100

# confusion matrix
table(data.test$seg_log, data.test$seg1)

#Trial 2:Naive Bayes-------------------------------
# Naive Bayes
nb <- naiveBayes(seg1 ~ CompBuy+AmznP+Female+Degree+Age, 
                 data = data.train)
(nb)

# predict segment allocation for test data
data.test$seg_nb <- predict(nb, data.test)
head(data.test)

# How well did the model perform?
round(mean(data.test$seg1 == data.test$seg_nb), 4) * 100

# rows: predicted, columns: actual
table(data.test$seg_nb, data.test$seg1)

#Trial 3: Random Forest------------------------------
# first need to coerce the segment column to factor
data.train$seg1 <- factor(data.train$seg1)
head(data.train)

set.seed(5) # for reproducibility
rf <- randomForest(seg1 ~ CompBuy+AmznP+Female+Degree+Age, 
                   data = data.train, ntree = 10000)

# output the results
(rf)

# predict segment allocation for test data
data.test$seg_rf <- predict(rf, data.test)
head(data.test)

# How well did the model perform?
round(mean(data.test$seg1 == data.test$seg_rf), 4) * 100

# rows: predicted, columns: actual
table(data.test$seg_rf, data.test$seg1)

#Comparison of methods-Importance---------------------------
cbind(logistic = round(mean(data.test$seg1 == data.test$seg_log), 4) * 100, 
      nb = round(mean(data.test$seg1 == data.test$seg_nb), 4) * 100,
      rf = round(mean(data.test$seg1 == data.test$seg_rf), 4) * 100)

