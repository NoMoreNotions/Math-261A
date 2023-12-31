
setwd("~/Desktop/261A-project")

# Import xlsx library
#install.packages("readxl")
#library("readxl")
library(MPV) # for PRESS

################### 1. READ DATA
# read the data
#school <- data.frame(read_excel("final_dataset.xlsx", sheet = "Sheet1"))
school <- read.csv("final_dataset_update.csv")
colnames(school)[16] <- "FRPM"

# Factor TestId and CountyName
school$Test.ID <- as.factor(school$Test.ID)
contrasts(school$Test.ID) = contr.treatment(2, base = 1)
school$County.Name <- as.factor(school$County.Name)

# Checking if a predictor is factor and constrast
#print(attributes(school$TestID))
#is.factor(school$County.Name)

################### 2.  CHECK DATA and VIF for quantitative variables
####### a. Plot data to check linear relationship

school.temp = data.frame(school$Percentage.Standard.Not.Met,
                         school$Test.ID,
                         school$ChronicAbsenteeismRate,
                         school$Students.Enrolled,school$GiveUpRate,
                         school$FRPM,school$Diversity.Index,
                         school$Violent.Crime,
                         school$Property.Crime)
# --> school.temp will only quantitave predictors and Test.ID (No County Name)

plot(school.temp)
colnames(school.temp)[1] <- "Percentage.Standard.Not.Met"
colnames(school.temp)[2] <-"Test.ID"
colnames(school.temp)[3] <-"ChronicAbsenteeismRate"
colnames(school.temp)[4]<- "Students.Enrolled"
colnames(school.temp)[5] <- "GiveUpRate"
colnames(school.temp)[6] <- "FRPM"
colnames(school.temp)[7]<- "Diversity.Index"
colnames(school.temp)[8]<- "Violent.Crime"
colnames(school.temp)[9]<- "Property.Crime"

# --> There seems to be the relationship between 
# some of the predictors with the response.

######## b. CHECK if multicolliearity exists

str(school.temp) #check types for each column
solve(cor(school.temp[,3:length(school.temp)]))
# --> ViolentCrime and PropertyCrime are highly correlated 
# --> Choose one only (with variable selection)

par(mfrow = c(1, 1))
######### c. CHECK for the need of X TRANSFORMATION
plot(school.temp[,2],school.temp[,1], main = "TestID")
plot(school.temp[,3],school.temp[,1], main ="ChronicAbsenteeismRate")
plot(school.temp[,4],school.temp[,1], main = "Students.Enrolled")
plot(school.temp[,5],school.temp[,1], main = "GiveUpRate")
plot(school.temp[,6],school.temp[,1], main = "FRPM")
plot(school.temp[,7],school.temp[,1], main = "Diversity.Index")
plot(school.temp[,8],school.temp[,1], main = "Violent.Crime")
plot(school.temp[,9],school.temp[,1], main = "Property.Crime")

# --> Student Enrolled seems not to be linear, 
# but no need to transform predictors (already checked)
# --> Give up rate seems not to have a clear relationship

################### 3. FIT MODEL
####### fit general lm
fit.0 <- lm(Percentage.Standard.Not.Met ~ Test.ID + County.Name + Students.Enrolled + 
     ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index +
     Violent.Crime + Property.Crime, data = school)
summary(fit.0)

# DiversityIndex = NA --> The variable is linearly related to 
# the other variables (CountyName) 
# --> Weird, its VIF is normal, it is because County Name is not in VIF matrix
# --> Becomes normal after dropping either CountyName or diversity index


###### Check for dropping either Diversity Index or County Name
par(mfrow = c(2, 2))
#fit with Diversity Index but without County Name
fit.0 <- lm(Percentage.Standard.Not.Met ~ Test.ID  + Students.Enrolled + 
              ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index +
              Property.Crime, data = school)
summary(fit.0) # R^2 = 0.6518, MS_RES = 11.25^2

plot(fit.0)
title(main = "Model With Diversity Index, Without County Name")


#fit with County Name but without Diversity Index
fit.0 <- lm(Percentage.Standard.Not.Met ~ Test.ID  + Students.Enrolled + 
              ChronicAbsenteeismRate + GiveUpRate + FRPM + County.Name +
              Property.Crime, data = school)
summary(fit.0) # R^2 = 0.6913, MS_RES = 10.62^2
plot(fit.0)
title(main = "Model with County Name,Without Diversity Index")

# --> Model with County Name yields higher R^2 value than with Diversity Index
# --> Model with County Name seems to produce larger leverage and influential points (Cook's Distance)
# --> The residual plots look about the same, but with Divervisty Index included, the 
# residual plot looks less bending straight line indicating a more constant variance despite higher number of outliers.

# --> USE MODEL fit.0 (after dropping County Name)


###################  4. Y-TRANSFORMATION

# What is the value ˆλ that maximizes the log-likelihood?
# Can't because y must be positive but we have 0 in y

# Temporarily get rid of 0
school.boxcox <- school[!(school$Percentage.Standard.Not.Met == 0),]
# --> school.boxcox will eliminate observations 0
length(school.boxcox$Percentage.Standard.Not.Met)
length(school$Percentage.Standard.Not.Met)
# --> only 20 observations is taken out --> not serious

fit.temp <- lm(Percentage.Standard.Not.Met ~ Test.ID  + Students.Enrolled + 
              ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index +
              Violent.Crime + Property.Crime, data = school.temp)

BC <- boxcox(fit.y,lambda = (-8:8)) 
power_BC <- BC$x[BC$y==max(BC$y)] #lamda ˆλ
power_BC # 0.5

# TRANSFORMING Y

fit.0.y <- lm(I(sqrt(Percentage.Standard.Not.Met)) ~ Test.ID  + Students.Enrolled + 
                ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index +
                 Property.Crime, data = school)

summary(fit.0.y) # R_square = 0.676, MS_RES = 0.9838
par(mfrow = c(2, 2))
plot(fit.0)
title(main="Before Transformating Y")
plot(fit.0.y)
title(main="After Transformating Y")
# --> Higher R^2, 0.676, Residual plot does not change much, normality seems to improve a bit.

# --> USE MODEL fit.0.y

################### 5. VARIABLE SELECTION
library(leaps)
attach(school)

forward = regsubsets(x=cbind(school$Test.ID,school$Students.Enrolled,
                   school$ChronicAbsenteeismRate,
                   school$GiveUpRate,school$FRPM,school$Violent.Crime,
                   school$Diversity.Index,
                   school$Property.Crime), y=school$Percentage.Standard.Not.Met, method = "forward",nbest = 4)
forward$intercept

# Variable Selection By-hand - Original Model
drop1(fit.0.y, I(sqrt(Percentage.Standard.Not.Met)) ~ Test.ID  + Students.Enrolled + 
              ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index +
              Property.Crime, data = school, test="F")
#drop violent crime

# refit
fit.1.y <-lm(I(sqrt(Percentage.Standard.Not.Met)) ~ Test.ID  + Students.Enrolled + 
             ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index + 
             Property.Crime, data = school)
summary(fit.1.y)
anova(fit.1.y,fit.0.y)
# p-value > 0.05 --> Adding Violent Crime does not significantly improve the model
# --> USE MODEL fit.1.y

#step wise --> Maybe no need, just wanted to check
fit.1 <- lm(I(sqrt(Percentage.Standard.Not.Met))~1,data =school)
add1(fit.1,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index, test = "F")

#add Diversity.Index
fit.2 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index)
drop1(fit.2,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index ,test = "F")

add1(fit.2,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index, test = "F")


#add Test ID
fit.3 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~school$Diversity.Index+ school$Test.ID)
drop1(fit.3,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ school$Test.ID,test = "F")


add1(fit.3,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index,test = "F")

#add ChronicAbsenteeismRate
fit.4 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~school$Diversity.Index+ school$Test.ID + school$ChronicAbsenteeismRate)
drop1(fit.4,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ school$Test.ID + school$ChronicAbsenteeismRate,test = "F")


add1(fit.4,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$ChronicAbsenteeismRate +    
       school$Students.Enrolled +
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index, test = "F")

#add FRPM
fit.5 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
              school$Test.ID + school$ChronicAbsenteeismRate + school$FRPM )
drop1(fit.5,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
        school$Test.ID + school$ChronicAbsenteeismRate+school$FRPM,test = "F")


add1(fit.5,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index , test = "F")

# add Property Crime
fit.6 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
              school$Test.ID + school$ChronicAbsenteeismRate +
              school$FRPM + school$Property.Crime)
drop1(fit.6,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
        school$Test.ID + school$ChronicAbsenteeismRate+school$FRPM +
        school$Property.Crime,test = "F")


add1(fit.6,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index,test = "F")

#add StudentsEnrolled
fit.7 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
              school$Test.ID + school$ChronicAbsenteeismRate + 
              school$Students.Enrolled +school$FRPM)
drop1(fit.7,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
        school$Test.ID + school$ChronicAbsenteeismRate+school$Students.Enrolled +
        school$FRPM,test = "F")


add1(fit.7,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index + 
       school$Violent.Crime, test = "F")

#addd give up rate
fit.8 <- lm(I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
              school$Test.ID + school$ChronicAbsenteeismRate +school$FRPM
            + school$Property.Crime +school$GiveUpRate)
drop1(fit.8,I(sqrt(school$Percentage.Standard.Not.Met)) ~ school$Diversity.Index+ 
        school$Test.ID + school$ChronicAbsenteeismRate +
        school$FRPM + school$Property.Crime + school$GiveUpRate,test = "F")


add1(fit.8,I(sqrt(school$Percentage.Standard.Not.Met))~ 
       school$Test.ID + 
       school$Students.Enrolled +
       school$ChronicAbsenteeismRate + 
       school$GiveUpRate + 
       school$FRPM + 
       school$Property.Crime +
       school$Diversity.Index, test = "F")


################### 6. CHECK FOR INTERACTIVE TERMS

fit.interaction  <-lm(I(sqrt(Percentage.Standard.Not.Met)) ~ Test.ID  * Students.Enrolled * 
               ChronicAbsenteeismRate * GiveUpRate * FRPM * Diversity.Index * 
               Property.Crime, data = school)

summary(fit.interaction) # R_square is 0.7161, MS_RES =  0.9262
anova(fit.interaction,fit.1.y)

plot(fit.1.y)
title(main = "Without interaction")

plot(fit.interaction)
title(main = "With interaction")


title(main = "With interaction")
# --> interaction terms are important (p_value is very small)
# USE MODEL fit.interaction
# but the residual assumptions are heavily violated (Cook's distance, Normality)

# FINAL MODEL fit.1.y

################## 6. Computing hat matrix - INFLUENTIAL, LEVERAGE PTS & OUTLIERS
# Computing hat matrix - Leverage pts

# 3 observations with the largest absolute standardized residuals - influential pts
head(sort(abs(rstandard(fit.1.y)), decreasing = TRUE),10) # 9371 > 6271 > 3101 > 6684

X <- as.matrix(cbind(1, school$Students.Enrolled, school$ChronicAbsenteeismRate,
                      school$Diversity.Index,
                       school$GiveUpRate, school$FRPM, school$Property.Crime))
H <- X%*%solve(t(X)%*%X)%*%t(X) 
H.order <- cbind(c(1:10423), diag(H))
order <- order(H.order[,2])
H.order <- H.order[order,]  # increasing order
tail(H.order,10)

x = tail(H.order[,1],1000)
y1 = head(H.order[,1],10403)
y2 = head(H.order[,1],10223)
y3 = head(H.order[,1],8423)
which(cooks.distance(fit.1.y) == max(cooks.distance(fit.1.y)))
school[6270,]
temp = school[y1,]
temp = school[y2,]
temp = school[y3,]
model <-lm(I(sqrt(Percentage.Standard.Not.Met)) ~ Test.ID  + Students.Enrolled + 
                        ChronicAbsenteeismRate + GiveUpRate + FRPM + Diversity.Index + 
                        Property.Crime, data = temp)
summary(model)
summary(fit.1.y)
#droping 20 outliers --> orginial R^2 = 0.676, now = 0.6782
#droping 200 outliers --> orginial R^2 now = 0.681
#droping 1000 outliers --> orginial R^2 now = 0.6906

# Improve R_2, but not much

############### 7.TRAINING AND TESTING DATA FOR THE MODEL

R_prediction = NULL
for (i in c(1:100)) {
  samp = sample(seq(1,10423,1),8000)
  train <- school[samp,]
  
  deleted = NULL
  for (i in seq(1,10423,1)) {
    if (!i %in% samp)
      deleted = append(deleted,i)
  }

  train$Test.ID <- as.factor(train$Test.ID)
  contrasts(train$Test.ID) = contr.treatment(2, base = 1)
  
  fit <- lm(
    I(sqrt(Percentage.Standard.Not.Met)) ~ ChronicAbsenteeismRate + 
      Test.ID + FRPM + Property.Crime + Students.Enrolled +
      GiveUpRate + Diversity.Index,data = train)
  
  test <- school[deleted,]
  test <- data.frame(test$Percentage.Standard.Not.Met,test$Property.Crime,
                    test$ChronicAbsenteeismRate,
                   test$FRPM,test$Test.ID,test$Students.Enrolled,
                   test$GiveUpRate,test$Diversity.Index)

  colnames(test)[1] = c("Percentage.Standard.Not.Met")
  colnames(test)[2] = c("Property.Crime")
  colnames(test)[3] = c("ChronicAbsenteeismRate")
  colnames(test)[4] = c("FRPM")
  colnames(test)[5] = c("Test.ID")
  colnames(test)[6] = c("Students.Enrolled")
  colnames(test)[7] = c("GiveUpRate")
  colnames(test)[8] = c("Diversity.Index")
  
  test$Test.ID <- as.factor(test$Test.ID)
  contrasts(test$Test.ID) = contr.treatment(2, base = 1)
  
  pred = predict(fit, test[,2:8])
  pred = pred^2
  result.1 = data.frame(pred, test[,1])
  colnames(result.1)[1] <- "predicted_response"
  colnames(result.1)[2] <- "true_response"
  result.1 = cbind(result.1, result.1[2] - result.1[1])
  colnames(result.1)[3] <- "deviation"
  result.1 = cbind(result.1, result.1[,2] - mean(result.1[,2]))
  colnames(result.1)[4] <- "(true - mean)"
  R <- 1 - (sum(result.1[,3]^2)/sum(result.1[,4]^2))
  #R <- sum(result.1[3]^2)
  R_prediction = append(R_prediction,R)
}

hist(R_prediction)
quantile(R_prediction, 0.025)
quantile(R_prediction,0.925)


# Making random prediction 
school[4000:4010,]
pred = data.frame(Diversity.Index = c(0.1),
                  Test.ID = c(1),
                  ChronicAbsenteeismRate = c(22.9),
                  FRPM = c(0.3),
                  GiveUpRate = c(1.9),
                  Students.Enrolled = c(1000),
                  Property.Crime = c(300))
pred = predict(fit.1.y, pred)
pred = pred^2
pred





