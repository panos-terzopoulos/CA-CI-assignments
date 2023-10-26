setwd("C:/Users/panos/OneDrive/Desktop/Assignments of CA-CI/Perceptual Mapping (MDS & FA)")

#rm(list = ls())
#libraries
install.packages(c("ggplot2","TTR","dplyr","tidyr","zoo","smacof","reshape","corrplot"))
#library("ggplot2","TTR","dplyr","tidyr","zoo")
install.packages("ggrepel")
pacman::p_load(reshape2, ggplot2, gplots, ggrepel, gridExtra, corrplot, 
               scatterplot3d, EFAtools, psych, RColorBrewer, lavaan, semPlot, 
               semTools)
install.packages("smacof")
library(smacof)

library("ggplot2")
library(TTR)
library(dplyr)
library(tidyr)
library(zoo)
library(smacof)
library(reshape)
library(corrplot)
library(ggrepel)


df <- read.csv("QuestionaireData_CityTrips.csv")
RAW <- read.csv("QuestionaireData_CityTrips.csv")
head(df)
summary(df)

df <- RAW[,-c(457:466)]
df <- df[,-c(2:56)]
data_base_names<- c("df1","df2","df3","df4","df5","df6","df7","df8","df9","df10")
n_variables<-ncol(df)     #number of variables in df
df_names<-names(df)       #extract all the variables name from df 
new<-data.frame()



city_name<-colnames(RAW[,4:23])    #all the city name 
city_name
new<-data.frame()
#rm(attributes)
attributes<-c()

for (i in 1:20){
  
  attributes[i]<-paste("Att_",i,sep="")  #name of all the attributes 
}

attributes[21]<-"City"
attributes
cities_preferences<-data.frame(t(rep(0,21)))
names(cities_preferences)<-attributes

dataset<-data.frame()

for (i in 1:10){  #create 10 different dataset based on the questionary sample 
  
  data_set<-df[df$Sample==i,]
  N_A_sum<-data.frame(colSums(is.na(data_set)))  #sum of the N/A value per each row
  n_row<-nrow(data_set)   #number of rows in the new dataset
  x<-c()
  z<-c()
  y<-1
  k<-1
  
  for (j in 1:n_variables) 
  {
    if(N_A_sum[j,]==n_row){
      x[y]<-df_names[j]   #save the names of the variables which need to be eliminated 
      y<-y+1
      
    }
    if(N_A_sum[i,]!=0)
    {
      z[k]<-df_names[j] #save the names of the variables who has random missing values 
    }
  }
  data_set<- data_set%>%select(-one_of(x))
  #dataset %>% mutate_at(z, ~replace_na(.,mean(., na.rm = TRUE)))   #replace N/A value with the mean 
  assign(paste("df",i,sep=""),data_set) 
  
  
  
  for (i in 1:length(city_name))
  {
    new<- (data_set[ , grepl(city_name[i], colnames(data_set))] )
    
    if(ncol(new)!=0)
    {
      new$City<- city_name[i]
      cities_preferences<-rbind(cities_preferences,setNames(new, names(cities_preferences)))
      
    }
    
  }
}

cities_preferences <-cities_preferences[-c(1),] #delete the first row filled by zeros 
40*6+25*6*4+24*6*2+23*6+22*6+33*6 #control if the creation of the dataset is right 
x #index of columns which have only N/A values 

#-----------------------------------------------------
#Treat missing values 

#1. Option 
#eliminate all the row with N/A (113 rows out of 1596)
#calcolate the number of rows with N/A values 
# nrow(cities_preferences[is.na(cities_preferences), ])/nrow(cities_preferences)
# ct <- cities_preferences %>% drop_na()
# cities_preferences<-ct


#2 Option
#find the random N/A values and replace them with the mean of each column 
#number of missing values per each variable
colSums(is.na(cities_preferences))

no_missing_value <-cities_preferences
no_missing_value

for(i in 1:ncol(no_missing_value)) {
  no_missing_value[ , i][is.na(no_missing_value[ , i])] <- mean(no_missing_value[ , i], na.rm=TRUE)
}

#control if the missing values have been replaced 
colSums(is.na(no_missing_value))

cities_preferences<-no_missing_value
#-----------------------------------------------

#Aggregate of the rating per city and attribute
aggregate_City<-aggregate(x=cities_preferences,by=list(cities_preferences$City),FUN=mean,na.rm=TRUE)
Cities<- aggregate_City$Group.1
Cities
aggregate_City<-aggregate_City[,-c(22)]
aggregate_City$City<- Cities


rownames(aggregate_City) <- aggregate_City$Group.1
round(aggregate_City[, -1], 2)
#aggregate_City<- aggregate_City[,-c(21)] #how to delete column 

#----------------------------------------------------------------------------------------------
#MDS
data.dist = dist(aggregate_City[, -1], method = "euclidean") #[,-1] dropping the first column with the TV names 
round(data.dist, 2)
data.dist

#Transform the distance into intervals
data.mds <- smacofSym(data.dist, ndim = 2, type = "interval")
data.mds$conf # output coordinates

data.mds.df <- data.frame(data.mds$conf)     
data.mds.df$City <- rownames(data.mds$conf)
data.mds.df




plot = ggplot(data = data.mds.df, aes(x = D1, y = D2)) +
  # add lines through origin
  geom_vline(xintercept = 0, color = "grey60") +
  geom_hline(yintercept = 0, color = "grey60") +
  # add cities as points
  geom_point() + 
  # add city names as labels
   geom_text_repel(aes(label = data.mds.df$City)) +
  coord_fixed() +
  theme_classic()
plot

#just to have a nicer representation 
plot = plot + 
  scale_x_continuous(limits = c(-1.2, 1.2)) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  labs(x = "Dimension 1", y = "Dimension 2")
plot

#save a image in our directory 
ggsave(plot, path = , filename = "City_rating.png", device = png)


data.mds.df = merge(data.mds.df, aggregate_City[,-c(1)], by = "City")
data.mds.df[, 1:6]

#each attribute can be run in a regression as a dependent variable , for example entertaining 
#with -1 we suppress the intercept 
reg = lm(Att_1 ~ -1 + D1 + D2, data = data.mds.df)
#summary(reg)

DV = names(data.mds.df)[-c(1:3)] # Dependent variables
DV

#running the regression with all the attributes as dependent variable ( same as running a loop)
fo <- sprintf("cbind(%s) ~ %s", toString(DV), "-1 + D1 + D2")
reg = do.call("lm", list(fo, quote(data.mds.df)))

# save coefficients as an object
attrVector = reg$coefficients
attrVector[, 1:6]
attrVector<-data.frame(attrVector)
data.mds.df[, 1:6]
#colnames(attrVector)<-c(b)
config = data.mds.df[, c("D1", "D2", "City")]
config


config$type = "point"
config

#transpone the regression coefficients 
attrVector <- as.data.frame(t(attrVector))
attrVector

attrVector$City <- rownames(attrVector)

# add plot type
attrVector$type = "vector"
head(attrVector)

config = rbind(config, attrVector)
config

options(ggrepel.max.overlaps = Inf)
#take as values only a subset which has a variable type="point" 
plot = ggplot(data = subset(config, type == "point"), aes(x = D1, y = D2)) +
  # add lines through origin
  geom_vline(xintercept = 0, color = "grey80") +
  geom_hline(yintercept = 0, color = "grey80") +
  # add brands as points
  geom_point() + 
  # add brand names as labels
  geom_text_repel(aes(label = City)) +
  # plot attribute vectors
  geom_segment(data = subset(config, type == "vector"),
               aes(x = 0, y = 0, xend = D1, yend = D2),
               alpha = 0.4, color = "green",
               arrow = arrow(length = unit(0.5, "cm"))) +
  # add attribute vector labels
  geom_text_repel(data = subset(config, type == "vector"),
                  alpha = 0.4, color = "green",
                  aes(label = City)) +
  # fix scale
  coord_fixed() +
  # extend the x- and y-axis limits
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  # label the x- and y-axis
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_classic()
plot


#--------------------------------------------------------------------
#deleting the non random missing values 
city_name<-colnames(RAW[,4:23])
#Preferences data set 
pref <- RAW[,-c(57:462)]
pref <- pref[,-c(2:36)]
city_name

names(pref)<- c(city_name)
n_variables<-ncol(pref)
a<-c("Prague","Geneva", "Paris","Stockholm","Brussels")

#------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------

city_name<-colnames(RAW[,4:23])
#Preferences data set 
pref <- RAW[,-c(57:462)]
pref <- pref[,-c(2:36)]



#------------------------------------------
#------------------------------
#for i in 1:10{}
PROVAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

pref <- RAW[,-c(57:462)]
pref <- pref[,-c(2:36)]
a<-c("Sample","Prague","Geneva", "Paris","Stockholm","Brussels","London" ,"Vienna" ,"Istanbul" ,"StPetersburg", "Barcelona" , "Dublin" ,"Amsterdam" ,"Athens","Riga", "Lisbon", "Rome","Madrid" ,"Berlin"   ,"Krakow")
colnames(pref)<-a
pref<-pref[which(pref$Sample==1),]

pref<-pref[which(pref$Sample==2),]
pref<-pref[which(pref$Sample==3),]
pref<-pref[which(pref$Sample==4),]
pref<-pref[which(pref$Sample==5),]
pref<-pref[which(pref$Sample==6),]
pref<-pref[which(pref$Sample==7),]
pref<-pref[which(pref$Sample==8),]
pref<-pref[which(pref$Sample==9),]
pref<-pref[which(pref$Sample==10),]

#-------------------------------------------
#Preferences data set 
pref <- RAW[,-c(57:462)]
pref <- pref[,-c(2:36)]
pref1<-pref[which(pref$Sample==1),]
pref2<-pref[which(pref$Sample==2),]
pref3<-pref[which(pref$Sample==3),]
pref4<-pref[which(pref$Sample==4),]
pref5<-pref[which(pref$Sample==5),]
pref6<-pref[which(pref$Sample==6),]
pref7<-pref[which(pref$Sample==7),]
pref8<-pref[which(pref$Sample==8),]
pref9<-pref[which(pref$Sample==9),]
pref10<-pref[which(pref$Sample==10),]

for
#to delete 
pref<-pref[,-c(7:21)]
a<-c("Sample","Prague","Geneva", "Paris","Stockholm","Brussels")
names(pref)<- c(a)

pref<-t(pref)
colnames(pref) = paste0("id", 1:40)
pref<-as.data.frame(pref)
pref$City<- a

#colnames(pref)[42]<-c("City")

pref[is.na(pref)]=3
pref.data = merge(data.mds.df[, c("City", "D1", "D2")], 
                  pref, by = "City")
head(pref.data)

DV = paste0("id", 1:40) # Dependent variables
DV

fi <- sprintf("cbind(%s) ~ %s", toString(DV), "-1 + D1 + D2")
reg = do.call("lm", list(fi, quote(pref.data)))
reg
# save coefficients as an object
idVector = reg$coefficients
idVector


# transpose and save as a data frame
idVector <- as.data.frame(t(idVector))

# add ids under column brand
idVector$City <- rownames(idVector)

# add plot type
idVector$type = "idvector"
idVector

config = rbind(config, idVector)
config


plot = ggplot(data = subset(config, type == "point"), 
              aes(x = D1, y = D2)) +
  # add lines through origin
  geom_vline(xintercept = 0, color = "grey60") +
  geom_hline(yintercept = 0, color = "grey60") +
  # add brands as points
  geom_point() + 
  # add brand names as labels
  geom_text_repel(aes(label = City)) +
  # plot attribute vectors
  geom_segment(data = subset(config, type == "vector"),
               aes(x = 0, y = 0, xend = D1, yend = D2),
               alpha = 0.4, color = "green",
               arrow = arrow(length = unit(0.5, "cm"))) +
  # add vector labels
  geom_text_repel(data = subset(config, type == "vector"),
                  alpha = 0.4, color = "green",
                  aes(label = City)) +
  # plot preference vectors
  geom_segment(data = subset(config, type == "idvector"),
               aes(x = 0, y = 0, xend = D1, yend = D2),
               alpha = 0.2, color = "darkred") +
  # fix scale
  coord_fixed() +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Dimension 1", y = "Dimensions 2") +
  theme_classic()
plot

#-----------------------------------------------------------------------
names(pref)<- c(city_name)
#transpone pref
pref<-t(pref)
head(pref)

rownames(pref)
a<-rownames(pref)
a
#pref<-cbind(pref,a)
#colnames(pref)[267]<-c("City")


colnames(pref) = paste0("id", 1:266)
pref<-as.data.frame(pref)
pref$City<- a

colnames(pref)[267]<-c("City")
pref[is.na(pref)]=0
pref.data = merge(data.mds.df[, c("City", "D1", "D2")], 
                  pref, by = "City")
head(pref.data)

DV = paste0("id", 1:266) # Dependent variables
DV

fi <- sprintf("cbind(%s) ~ %s", toString(DV), "-1 + D1 + D2")
reg = do.call("lm", list(fi, quote(pref.data)))
reg
# save coefficients as an object
idVector = reg$coefficients
idVector


# transpose and save as a data frame
idVector <- as.data.frame(t(idVector))

# add ids under column brand
idVector$City <- rownames(idVector)

# add plot type
idVector$type = "idvector"
idVector

config = rbind(config, idVector)
config


plot = ggplot(data = subset(config, type == "point"), 
              aes(x = D1, y = D2)) +
  # add lines through origin
  geom_vline(xintercept = 0, color = "grey60") +
  geom_hline(yintercept = 0, color = "grey60") +
  # add brands as points
  geom_point() + 
  # add brand names as labels
  geom_text_repel(aes(label = City)) +
  # plot attribute vectors
  geom_segment(data = subset(config, type == "vector"),
               aes(x = 0, y = 0, xend = D1, yend = D2),
               alpha = 0.4, color = "green",
               arrow = arrow(length = unit(0.5, "cm"))) +
  # add vector labels
  geom_text_repel(data = subset(config, type == "vector"),
                  alpha = 0.4, color = "green",
                  aes(label = City)) +
  # plot preference vectors
  geom_segment(data = subset(config, type == "idvector"),
               aes(x = 0, y = 0, xend = D1, yend = D2),
               alpha = 0.2, color = "darkred") +
  # fix scale
  coord_fixed() +
  scale_x_continuous(limits = c(-1.1, 1.1)) +
  scale_y_continuous(limits = c(-1.1, 1.1)) +
  labs(x = "Dimension 1", y = "Dimensions 2") +
  theme_classic()
plot
