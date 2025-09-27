install.packages("ggplot2") #charts package 
library(ggplot2) #charts package 
install.packages("ISLR") #dataset package 
library(ISLR) #charts package 
install.packages("corrplot") #correlation package 
library(corrplot) #charts package 
install.packages("era")
library(era)
install.packages("purrr")
library(purrrr)

sabor <- read.csv("") #indicate correct path to the dataset csv file

#1.a, 1.b
str(sabor)


#--------------------------------------------
#1.c
summary(sabor)

blank_rows <- sabor[sabor$Name == "" | is.na(sabor$Name), ]
count_blank_rows <- nrow(blank_rows)
print(count_blank_rows)

blank_rows <- sabor[sabor$Education == "" | is.na(sabor$Education), ]
count_blank_rows <- nrow(blank_rows)
print(count_blank_rows)

blank_rows <- sabor[sabor$Marital_Status == "" | is.na(sabor$Marital_Status), ]
count_blank_rows <- nrow(blank_rows)
print(count_blank_rows)

#--------------------------------------------
#2.a
curr_year <- as.integer(this_year())
sabor$age <- curr_year-sabor$Birthyear

summary(sabor$age)

boxplot(main="Boxplot of Age", ylab = 'Age',sabor$age)

hist(
  sabor$age, 
  main="Histogram of Age", 
  xlab="Age", 
  ylab="Amount of clients",
  col="lightblue", 
  border="black") 
#--------------------------------------------
#2.b
sabor$TotalChildren <- sabor$Kid_Younger6 + sabor$Children_6to18
str(sabor$TotalChildren)
#--------------------------------------------
#2.c
sabor$MntTotal <- sabor$MntAdditionalRequests + sabor$MntDesserts + sabor$MntDrinks + sabor$MntEntries + sabor$MntMeat.Fish + sabor$MntVegan.Vegetarian
str(sabor$MntTotal)
#--------------------------------------------
#3
summary(sabor$age)
table(sabor$Marital_Status)
prop.table(table(sabor$Marital_Status))
table(sabor$Education)
table(sabor$TotalChildren)
prop.table(table(sabor$TotalChildren))
prop.table(table(sabor$Education))
summary(sabor$Income)

boxplot(main="Boxplot of Income", ylab = 'Income',sabor$Income)

hist(
  sabor$Income, 
  main="Histogram of Income", 
  xlab="Income", 
  ylab="Amount of clients",
  col="lightblue", 
  border="black") 
#--------------------------------------------
#4.a
summary(sabor$MntAdditionalRequests)
summary(sabor$MntDesserts)
summary(sabor$MntDrinks) 
summary(sabor$MntEntries)
summary(sabor$MntMeat.Fish)
summary(sabor$MntVegan.Vegetarian)      

#--------------------------------------------
#4.b
summary(sabor$NumOfferPurchases)
summary(sabor$NumAppPurchases)
summary(sabor$NumTakeAwayPurchases) 
summary(sabor$NumStorePurchases)

#--------------------------------------------
#4.c
str(subset(sabor, Time_Adherence > 30)) #or >=30

#--------------------------------------------
#4.d
summary(sabor$NumAppVisitsMonth)

#--------------------------------------------
#4.e
prop.table(table(sabor$Complain))

#--------------------------------------------
#4.f
prop.table(table(sabor$Response_Cmp1))
prop.table(table(sabor$Response_Cmp2))
prop.table(table(sabor$Response_Cmp3))
prop.table(table(sabor$Response_Cmp4))
prop.table(table(sabor$Response_Cmp5))
#--------------------------------------------
#5.a
dados_numericos <- sabor[11:16]
matriz_corr <- cor(dados_numericos, use = 'complete.obs')
corrplot(method = 'number', matriz_corr) 
#--------------------------------------------
#5.b
plot(
  sabor$Income, sabor$MntVegan.Vegetarian, 
  main="Income vs MntVegan.Vegetarian", 
  xlab="Income", 
  ylab=" MntVegan.Vegetarian", 
  col="darkblue",  
  pch=19)                                 #scatterplot, graph with points at the intersection of two variables

abline(
  lm(sabor$MntVegan.Vegetarian ~ sabor$Income), 
  col="red")    
#--------------------------------------------
#5.c
sabor_upd <- subset(sabor, Income <= 200000)
#--------------------------------------------
#5.c.i
boxplot(
  Income ~ Marital_Status, data=sabor_upd,
  main="Income by Marital Status", 
  xlab="Status",
  ylab="Income", 
  col="lightblue") 

boxplot(
  Income ~ TotalChildren, data=sabor_upd,
  main="Income by Total Children", 
  xlab="Amount of Children",
  ylab="Income", 
  col="lightblue") 


#--------------------------------------------#--------------------------------------------#--------------------------------------------

#ANALISE INFERENCIAL

#1.
sabor_upd$WithChildren <- ifelse(sabor_upd$TotalChildren > 0, "Yes", "No")
#--------------------------------------------
#1.a
table(sabor_upd$WithChildren)
t.test(MntDesserts ~ WithChildren, data = sabor_upd)
boxplot(
  MntDesserts ~ WithChildren, data=sabor_upd,
  main="Amount Spent on Desserts by With Children", 
  xlab="With Children",
  ylab="Amount Spent on Desserts", 
  col="lightblue") 
#--------------------------------------------
#1.b
table(sabor_upd$WithChildren)
t.test(Recency ~ WithChildren, data = sabor_upd)
boxplot(
  Recency ~ WithChildren, data=sabor_upd,
  main="Recency by With Children", 
  xlab="With Children",
  ylab="Days from last visit", 
  col="lightblue") 
#--------------------------------------------
#1.c
table_marital_children <- table(sabor_upd$Marital_Status, sabor_upd$WithChildren)
chisq.test(table_marital_children)
prop.table(table(sabor_upd$Marital_Status, sabor_upd$WithChildren))
#--------------------------------------------
#2
t.test(sabor_upd$MntMeat.Fish, sabor_upd$MntVegan.Vegetarian, alternative = "two.sided")
#--------------------------------------------
#3
t.test(sabor_upd$NumOfferPurchases, mu = 1)
str(sabor_upd$NumOfferPurchases)

#--------------------------------------------
#4
sabor_upd$MntTotal <- sabor_upd$MntAdditionalRequests + sabor_upd$MntDesserts + sabor_upd$MntDrinks + sabor_upd$MntEntries + sabor_upd$MntMeat.Fish + sabor_upd$MntVegan.Vegetarian
str(sabor_upd$MntTotal)
t.test(MntTotal ~ WithChildren, data = sabor_upd)
boxplot(
  MntTotal ~ WithChildren, data=sabor_upd,
  main="MntTotal by With Children", 
  xlab="With Children",
  ylab="Total Amount Spent", 
  col="lightblue") 

#--------------------------------------------
#5.a
shapiro_results <- lapply(split(sabor_upd$MntAdditionalRequests, sabor_upd$Marital_Status), shapiro.test)
shapiro_results
#5.b
kruskal_test <- kruskal.test(MntAdditionalRequests ~ Marital_Status, data = sabor_upd)
kruskal_test














