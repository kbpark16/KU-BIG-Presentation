#Data 불러들이기
install.packages("titanic")
library(titanic)

#Age, Fare, Survived variable만 남기기- x1,x2,y 예시들기 좋게
titanic_data<-titanic_train[,c(2,6,10)]

#결측치 제거
titanic_data2<-na.omit(titanic_data) 

titanic_data2$Survived<-as.factor(titanic_data2$Survived)

# 범주별 요약통계량
by(titanic_data2[,-1],titanic_data2[,1],summary)

#Box'm test for homogeneity of covariance matrix
install.packages("biotools")
library(biotools)
# formula method

#X , Y
boxM(titanic_data2[,-1],titanic_data2[,1])
#p-value<2.2e-16 : QDA

install.packages("ggplot2")
library(ggplot2)
#visualizing data

ggplot(data = titanic_data2, aes(x = Age, y = Fare)) +
  geom_point(aes(color=Survived, shape=Survived)) +
  xlab("Age(yrs)") + 
  ylab("Fare(in pound)") +
  ggtitle("Age vs Fare")

#Training/test set split(7:3) 
set.seed(1)
sub<-sample(nrow(titanic_data2),nrow(titanic_data2)*0.7)
train<-titanic_data2[sub,]
test<-titanic_data2[-sub,]


str(train) #499 obs. of 3 variables  
str(test)  #215 obs. of 3 variables

#1. QDA: BOX'M test 결과, 모 공분산행렬이 유의하게 다르므로, QDA
model_qda= qda(train[,-1],train[,1],data=train,CV=F)

p <- predict(model_qda, test[,-1], type = class)

pred_qda<-p$class

actual<-test$Survived

#2. LDA
model_lda= lda(train[,-1],train[,1],data=train,CV=F)

p <- predict(model_lda, test[,-1], type = class)

pred_lda<-p$class

# Confusion matrix
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
caret::confusionMatrix(pred_lda,actual,positive="1")

# 3. Logistic discriminant Function
model3 = glm(Survived~., family=binomial(),data=train)
predicted=predict(model3,newdata=test[,-1],type="response")
str(predicted)

ls(model3)

model3$coefficients

predicted.classes <- ifelse(predicted > 0.5, 1, 0)
predicted.classes<-as.factor(predicted.classes)
str(predicted.classes)
caret::confusionMatrix(predicted.classes,actual,positive="1")

#for visualization: compute 'logistic' discrimninat function
slope <- coef(model3)[2]/(-coef(model3)[3])
intercept <- coef(model3)[1]/(-coef(model3)[3]) 

#logistic decision boundary plotting, y=1(positive/Survived) above/ y=0(negative/Not-survived) below
xyplot(test$Fare~test$Age, xlab="Age",ylab="Fare",data = test, groups =factor(test$Survived2,labels=c("NO","YES")),
       ,auto.key=list(columns=2),type=c("p","g"),
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })



#2019.1.13--Trying to plot decison boundary for "LDA/QDA"

#method1
install.packages("klaR")
library(klaR)

titanic_data2
titanic_data2
train
?partimat
partimat(x=train[,c(3,2)],grouping=train[,1],method="lda",name=c("Fare(in pound)","Age(yrs)"),main="LDA(training set)")

partimat(x=train[,c(3,2)],grouping=train[,1],method="qda",name=c("Fare(in pound)","Age(yrs)"),main="QDA(training set)")

classscatter(Survived~., data=titanic_data2, method=lda, col.correct = "black",
             col.wrong = "red", gs = NULL)

species <- factor(rep(c("s","c","v"), each=50))
partimat(x=iris[,1:4], grouping=species, method="lda")


#method2
mu <- matrix(0,nrow=2,ncol=2)

ownldahyperplane <- function(sigmainv,mu1,mu2,prior1,prior2) {
  J <- nrow(mu)                                                                                  # number of classes
  b <- sigmainv%*%(mu1 - mu2)
  c <- -(1/2)*t(mu1 + mu2)%*%sigmainv%*%(mu1 - mu2) + log(prior1/prior2) 
  return(list(b=b,c=c))
}

##
## Returns linear betas (intersect and slopes) for the given hyperplane structure. 
## The structure is a list that matches the output of the function defined above. 
##
ownlinearize <- function(sephyp) {
  return(list(beta0=-sephyp$c/sephyp$b[2],                                   # line slope and intersect
              beta1=-sephyp$b[1]/sephyp$b[2]))
}


fit= lda(train[,-1],train[,1],data=train,CV=F)

A <- fit$scaling                                                                                    # extract A matrix 
sigmahatinv <- A%*%t(A)                                                                    # calculate sigma hat inverse
priorhat <- fit$prior                                                                              # get prior hat probabilities
muhat <- fit$means                                                                             # get mu hat

# =========================================================================================
# (5) Calculate the separating hyperplanes which can be added using abline
#     or create the class boundaries using lines by fixing six points. 
#     Run the abline one at the time after running step 2 anew  
# =========================================================================================


xyplot(test$Fare~test$Age, xlab="Age",ylab="Fare",data = test, groups =factor(test$Survived2,labels=c("NO","YES")),
       ,auto.key=list(columns=2),type=c("p","g"),
       panel=function(...){
         panel.xyplot(...)
         panel.abline(line12$beta0,line12$beta1)
         panel.grid(...)
       })

abline(line12$beta0,line12$beta1)

sephyp12 <- ownldahyperplane(sigmahatinv,muhat[1,],muhat[2,],     # calculate dec. boundary 1-2
                             priorhat[1],priorhat[2])
line12 <- ownlinearize(sephyp12)                                                       # get line for 1-2
abline(line12$beta0,line12$beta1)

iris
##fit LDA
iris
iris.data<-iris[,c(3,4,5)]

iris.lda <- lda(x=iris.data[,-3],grouping=iris.data[,3])
iris.qda <- qda(x=iris.data[,-3],grouping=iris.data[,3])
##create a grid for our plotting surface
x <- seq(-4,4,0.02)
y <- seq(-4,4,0.02)
z <- as.matrix(expand.grid(x,y),0)
m <- length(x)
n <- length(y)

iris.qdp <- predict(iris.qda,iris.data[,-3])$class
contour(iris.data[,1],iris.data[,2],matrix(iris.qdp,length(iris.data[,1]),length(iris.data[,2])),
        levels=c(1.5,2.5), add=TRUE, d=FALSE, lty=2)





# create a grid corresponding to the scales of Age and EstimatedSalary
# and fill this grid with lot of points
X1 = seq(min(iris.data[, 1]) - 1, max(iris.data[, 1]) + 1, by = 0.01)
X2 = seq(min(iris.data[, 2]) - 1, max(iris.data[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(iris.data[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time

iris.lda <- lda(x=iris.data[,-3],grouping=iris.data[,3])

pred_grid = predict(iris.lda, newdata =iris.data[,-3])$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)
