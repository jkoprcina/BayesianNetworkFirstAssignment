# Authors: Bruno Jerkovic, Josip Koprcina, Christos Mavrikis

# If you are running this script for the first time
# please uncomment and install the packages below


#install.packages(c("remotes","pROC","naivebayes"))
#remotes::install_github("jtextor/bayesianNetworks")
#install.packages("lavaan", repos="http://cran.at.r-project.org/")
#install.packages("dummies")
#install.packages("bnlearn")
#install.packages("dagitty")

# Include used libs for assignment

#library(dummies)
#library(tibble)
#library(naivebayes)
library(lavaan)
library(bnlearn)
library(dagitty)
library(bayesianNetworks)
library(pROC)

# Pass our dataset into a variable 
obese <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
# Print some instances to be sure everything is OK
head(obese[1:10,])

obese<-obese[sample(nrow(obese)),]

###Create 10 equally size folds
folds <- cut(seq(1,nrow(obese)),breaks=10,labels=FALSE)


#DAG - plot BN
g <- dagitty('
dag {
bb="0,0,1,1"
Age [pos="0.050,0.037"]
CAEC [pos="0.258,0.368"]
CALC [pos="0.948,0.314"]
CH2O [pos="0.250,0.277"]
FAF [pos="0.791,0.234"]
FAVC [pos="0.394,0.364"]
FCVC [pos="0.502,0.348"]
Gender [pos="0.249,0.037"]
Height [pos="0.140,0.685"]
MTRANS   [pos="0.653,0.475"]
NCP [pos="0.381,0.523"]
NObeyesdad [pos="0.258,0.935"]
SCC [pos="0.817,0.119"]
SMOKE [pos="0.027,0.326"]
TUE [pos="0.121,0.277"]
Weight [pos="0.511,0.726"]
family_history_with_overweight [pos="0.547,0.038"]
Age -> CALC
Age -> CAEC
Age -> MTRANS
Age -> FAF
Age -> FAVC
Age -> FCVC
Age -> Height
Age -> SCC
Age -> SMOKE
Age -> TUE
Age -> Weight
CAEC -> Weight
CALC-> Weight
CH2O -> Weight
FAF -> CH2O
FAF -> FCVC
FAF -> NCP
FAF -> Weight
FAVC -> Weight
FCVC -> Weight
Gender -> FAF
Gender -> SCC
Gender -> SMOKE
Gender -> TUE
Gender -> Weight
Gender -> Height
Height -> NObeyesdad
MTRANS -> Weight
NCP -> FAVC
NCP -> Weight
SCC -> CH2O
SCC -> FAF
SCC -> FAVC
SCC -> FCVC
SCC -> NCP
SCC -> Weight
SMOKE -> Weight
TUE -> Weight
Weight -> NObeyesdad
family_history_with_overweight -> FAVC
family_history_with_overweight -> FCVC
family_history_with_overweight -> NCP
family_history_with_overweight -> SCC
family_history_with_overweight -> Weight
}
')

plot(g)
# If you want a clearer or larger picture of the plot
# you can export the plot using the Export button


# Analytics of the data
mean(obese$Age)
sd(obese$Age)
max(obese$Age)
min(obese$Age)


table(obese$family_history_with_overweight)
table(obese$SMOKE)
table(obese$SCC)
table(obese$CAEC)
table(obese$Gender)
table(obese$FAVC)
table(obese$MTRANS)
table(obese$NObeyesdad)


# Check conditional Independencies - Uncomment bellow to see
impliedConditionalIndependencies(g)
localTests(g, obese, type="cis.chisq")

#Start of preprocessing 

# Round off columns - as we go through the instances we get variables Age = 24.13125124124
# or variables that are binary or ternary (ex. 1,2 or 1,2,3) become 2.621321312 etc.
# so we start the data pre-processing by rounding of these values
obese$Age    <- round(obese$Age)
obese$Height <- round(obese$Height,digits=2)
obese$Weight <- round(obese$Weight)
obese$FCVC   <- round(obese$FCVC)
obese$NCP    <- round(obese$NCP)
obese$FAF    <- round(obese$FAF)
obese$TUE    <- round(obese$TUE)
obese$CH2O   <- round(obese$CH2O)

#Dividing into Age groups
s <-rep("<20",nrow(obese))
s[obese$Age>=20 &obese$Age<35] <- "20-34"
s[obese$Age>=35 &obese$Age<50] <- "35-49"
s[obese$Age>=50] <- "50>="
obese$Age <-as.numeric(ordered(s)) # <20 = 1 , 20-34 = 2 , 35-49 = 3 , 50>= = 4
print(obese$Age)

table(obese$NObeyesdad)
#Dividing into Height groups
s <-rep("<1.60",nrow(obese))
s[obese$Height>=1.60 &obese$Height<1.75] <- "1.60-1.74"
s[obese$Height>=1.75 &obese$Height<1.85] <- "1.75-1.84"
s[obese$Height>=1.85] <- "1.85>="
obese$Height <-as.numeric(ordered(s)) # <1.60 = 1 , 1.60-1.74 = 2 , 1.75-1.84 = 3 , 1.85>= = 4

#Dividing into Weight groups
s <-rep("<50",nrow(obese))
s[obese$Weight>=50 &obese$Weight<70] <- "50-69"
s[obese$Weight>=70 &obese$Weight<90] <- "70-89"
s[obese$Weight>=90] <- "90>="
obese$Weight <-as.numeric(ordered(s))  # <50 = 1 , 50-69 = 2 , 70-89 = 3 ,90>= = 4
#print column $Weight
#obese$Weight

#Breakdown Weight levels as well
obese$NObeyesdad <- as.character(obese$NObeyesdad)
obese$NObeyesdad[obese$NObeyesdad %in% c("Insufficient_Weight","Normal_Weight")] <- "Normal_Weight"
obese$NObeyesdad[obese$NObeyesdad %in% c("Overweight_Level_I","Overweight_Level_II")] <- "Overweight"
obese$NObeyesdad[obese$NObeyesdad %in% c("Obesity_Type_I","Obesity_Type_II","Obesity_Type_III")] <- "Obese"
obese$NObeyesdad <- as.numeric(ordered(obese$NObeyesdad))#Normal_Weight = 1 Overweight = 2 Obese= 3
#print column $NObeyesdad
#obese$NObeyesdad

#as.numeric ordered
obese$family_history_with_overweight <- as.numeric(ordered(obese$family_history_with_overweight,c("no","yes"))) #no = 1 &  yes = 2
obese$FAVC                           <- as.numeric(ordered(obese$FAVC,c("no","yes"))) #no = 1 &  yes = 2
obese$SCC                            <- as.numeric(ordered(obese$SCC,c("no","yes"))) #no = 1 &  yes = 2
obese$SMOKE                          <- as.numeric(ordered(obese$SMOKE,c("no","yes"))) #no = 1 &  yes = 2
obese$Gender                         <- as.numeric(ordered(obese$Gender,c("Female","Male"))) #Female = 1 &  Male = 2
obese$CAEC                           <- as.numeric(ordered(obese$CAEC, levels=c("no","Sometimes","Frequently","Always"))) #no = 1 Sometimes = 2 Frequently = 3 Always = 4
obese$CALC                           <- as.numeric(ordered(obese$CALC, levels=c("no","Sometimes","Frequently","Always"))) #no = 1 Sometimes = 2 Frequently = 3 Always = 4
obese$MTRANS                         <- as.numeric(ordered(obese$MTRANS, c("Walking","Bike","Public_Transportation","Motorbike","Automobile")))

#End of preprocessing
#If you want to test the results without the preprocessing
#remove all code from line 112 - 169 (or put it in comments)



localTests(g, obese, type="cis.chisq")
#Split into train and test sets
#table(obese$family_history_with_overweight)

#Perform 10 fold cross validation - random sampling

testIndexes <- which(folds==10,arr.ind=TRUE)
train.set <- obese[-testIndexes, ]
test.set <- obese[testIndexes, ]
#nrow(test.set)



#train.set <- obese[1:1000,]
#table(train.set$NObeyesdad)
#head(train.set)
#summary(train.set)
#localTests(g, train.set, type="cis.chisq")
#test.set <- obese[1001:2111,]
#head(train.set)
#summary(test.set)
#table(test.set$NObeyesdad)


#Correlation Matrix - check for path coefficients
M <- lavCor(obese)
varTable(obese)
M
localTests(g,sample.cov = M, sample.nobs = nrow(obese))

cg <- coordinates(g)
cg
fit <- sem(toString(g,"lavaan"),sample.cov = M , sample.nobs = nrow(obese))
summary(fit)

fg <- lavaanToGraph(fit,digits=2)
#fg
coordinates(fg) <- cg

plot(fg,show.coefficients = TRUE)

#PREDICTIONS

#fitting the model
net <- model2network(toString(g,"bnlearn"))
net

fit1 <- bn.fit(net,train.set,method="mle") #change
fit1
#summary(fit1)



#NObeyesdadpredicted = predict(fit, node="NObeyesdad_Insufficient_Weight", data=d[1:num,], method="bayes-lw")


#Prediction for train - Expect good predictions 
#table(obese$NObeyesdad[1:800]) # how many it actually has
p <-  predict(fit1, node="NObeyesdad", data=train.set[c("Height","Weight")], method="bayes-lw")
p
#table(p) #how many our prediction has

auc(train.set$NObeyesdad,p)
roc1<-roc(train.set$NObeyesdad,p) 

#Prediction for test - Expect relatevly good  predictions 
#table(obese$NObeyesdad[501:2111])
p1 <-  predict(fit1, node="NObeyesdad",data=test.set[c("Height","Weight")], method="bayes-lw")
#p1 <- round(p1)
#table(p1) #how many our prediction has
auc(test.set$NObeyesdad,p1)
roc2<-roc(test.set$NObeyesdad,p1)

plot(roc1,col='red') #train
plot(roc2, add=TRUE, col='blue') #test

#cbind(predicted = p1, actual = obese[1001:2111,"NObeyesdad"])
#plot(test.set[,"Age"],p1)

#-- MORE PREDICTIONS --#
#p <-  predict(fit1, node="family_history_with_overweight", data=test.set["NObeyesdad"], method="bayes-lw")
#p <- round(p)
#table(p) #how many our prediction has

#auc(test.set$family_history_with_overweight,p)
#roc1<-roc(test.set$family_history_with_overweight,p) 

