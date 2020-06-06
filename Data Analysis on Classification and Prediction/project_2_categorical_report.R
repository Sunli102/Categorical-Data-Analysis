# read in the raw datasets
data_training = read.table('/Users/lisun/GSU/Categorical Analysis/Project2/project.1.data.2.train.txt',
                           sep = ',', na.strings = c('?', 'l'))    

data_testing = read.table('/Users/lisun/GSU/Categorical Analysis/Project2/project.1.data.2.test.txt',
                          sep = ',',na.strings = 'l')  # there is no missing data in the file

# remove the variables with missing values and set up a new training
id_no_miss = which(apply(is.na(data_training),1,sum)==0)
data_train= data_training[id_no_miss,]

id_no_miss = which(apply(is.na(data_testing),1,sum)==0)
data_test= data_testing[id_no_miss,]

### plot the histogram of the each variables
## plot continous variables
# training data 
library(ggplot2)
for (i in c(2,3,8,11,14,15))
{
  print(qplot(data_train[,i], geom = 'histogram', xlab = colnames(data_train)[i]))
}

# test data 
library(ggplot2)
for (i in c(2,3,8,11,14,15))
{
  print(qplot(data_test[,i], geom = 'histogram', xlab = colnames(data_test)[i]))
}

## plot categorical variable
# training data
for (i in c(1,4,5,6,7,9,10,12,13,16))
{
  ggbar = ggplot(data.frame(data_train[,i]), aes(x=data_train[,i]))+ geom_bar()
  ggbar = ggbar + labs(x = colnames(data_train)[i])
  print(ggbar)
}

# test data
for (i in c(1,4,5,6,7,9,10,12,13,16))
{
  ggbar = ggplot(data.frame(data_test[,i]), aes(x=data_test[,i]))+ geom_bar()
  ggbar = ggbar + labs(x = colnames(data_test)[i])
  print(ggbar)
}


# plot histogram of subsamples
# plot histogram of subsamples of continous variables
op = par(mfrow = c(2,3))
for (i in c(2,3,8,11,14,15))
{
  hist(data_train[,i][which(data_train$V16=='-')],  border = 'blue', main = 
         paste('Histogram of ', names(data_train[i])), xlab = names(data_train[i]),
       ylab = 'V16 + / V16 -',xlim = range(data_train[i]))
  hist(data_train[,i][which(data_train$V16=='+')], border = 'red',
       ylab = 'presence', add=T)
  if (i == 2)
  {legend('topright', legend=c("V16 -", "V16 +"),
          col=c("blue", "red"), lty=1:1.5, cex=0.9)
  }
}







# plot the histogram of subsamples of categorical variables
for (i in c(1,4,5,6,7,9,10,12,13,16))
{
  lengdText = F
  if (i == 1){lengdText = T}
  roll1 = data_train[,i][which(data_train$V16=='-')]
  roll2 = data_train[,i][which(data_train$V16=='+')]
  rollAll = rbind(table(roll1), table(roll2))
  rownames(rollAll) = c('V16 -', 'V16 +')
  
  barplot(rollAll, beside = T, col = c('blue', 'red'), legend.text = lengdText, args.legend = list(x = "topleft"),
          main = paste('Histogram of ', names(data_train[i])), xlab = names(data_train[i]),
          ylab = 'V16 + / V16 -')
}


#################################################
####### without considering regularity ##########
#################################################
## using forward method to do regression model selection
fit.full = glm(V16 ~., data = data_train, family = binomial)
fit.null = glm(V16 ~1, data = data_train, family = binomial)
select = step(fit.null, scope = list(lower = fit.null, upper = fit.full), direction = 'forward')


### Calculate the ROC curves and AUCs  
##  leave one out method is used  
nn = 100
pi0=seq(1,0, length.out = nn)
nsample = nrow(data_train)
roc.1=NULL

initial.time = proc.time()

for(k in 1:length(pi0))
{
  # nsample = nrow(data_train)
  n1_11=n1_10=n1_01=n1_00=0
  
  for (i in 1:nsample)
  {
    training = data_train[-i,]
    test = data_train[i,]
    
    fit.1.training = glm(V16 ~ V9 + V15 + V11 + V6 + V14 + V4 + V8, data = training, family = binomial)
    
    if (predict(fit.1.training, test, type = 'response') >= pi0[k])
    {V16.pred.1 = '+'}else 
    {V16.pred.1 = '-'}
    
    if((test$V16=='+')&(V16.pred.1=='+'))
    {n1_11=n1_11+1}
    if((test$V16=='+')&(V16.pred.1=='-'))
    {n1_10=n1_10+1}
    if((test$V16=='-')&(V16.pred.1=='+'))
    {n1_01=n1_01+1}
    if((test$V16=='-')&(V16.pred.1=='-'))
    {n1_00=n1_00+1}
  }
  sensitivity_1 = n1_11/(n1_11 + n1_10)
  specificity_1 = n1_00/(n1_01 + n1_00)
  roc.1=rbind(roc.1, c(1-specificity_1, sensitivity_1))
}

prop_1 = sum(data_train[,16]=='+')/nsample; prop_1
prop_0 = 1-prop_1; prop_0  


# running time
time =  proc.time() - initial.time; time      

## plot ROC curve and calculate AUC
plot(roc.1, type="s",xlim=c(0,1), ylim=c(0,1), col="red", lwd=3,
     main="ROC curve without regularity", xlab="1-Specificity", ylab="Sensitivity")
auc.1=sum(roc.1[-nn, 2]*(roc.1[-1,1]-roc.1[-nn,1]))
auc.1    

### Identify the best cut-off points and the best prediction accuracy
pred.accuracy.1=roc.1[,2]*prop_1 +(1-roc.1[,1])*prop_0; 
best_point.1 = which.max(pred.accuracy.1);  best_point.1   
best_pi0_1=pi0[best_point.1]; best_pi0_1      
pred.accuracy.1[best_point.1]      


###################################################
## apply classification rule to the test dataset ##
fit.1 = glm(V16 ~ V9 + V15 + V11 + V6 + V14 + V4 + V8, data = data_train, family = binomial)

ntest = nrow(data_test)
test_predict1 = NULL

test_predict = predict(fit.1, data_test, type = 'response')
test_predict1[test_predict >= best_pi0_1]  = '+'
test_predict1[test_predict < best_pi0_1]  = '-'
test_pred.accuracy = mean(test_predict1 == data_test$V16)
test_pred.accuracy    





##############################################
############ consider regularity #############
##############################################
### use Ridge penalty to build logistic regression model
y = data_train$V16
x = data.matrix(data_train[, -16])


library(glmnet)
fit.0=glmnet(x, y, family="binomial", alpha=0)
lambda.seq=fit.0$lambda
pi0=seq(0,1, length.out = nn)
correct.num=matrix(0, length(lambda.seq), length(pi0))
nsample=nrow(x)

initial.time = proc.time()

for(i in 1:nsample)
{
  x.train=x[-i,]
  x.test=matrix(x[i,],1,ncol(x))
  y.train=y[-i]
  y.test=y[i]
  fit.1.training=glmnet(x.train, y.train, family="binomial", alpha=0)
  for(j in 1:length(lambda.seq))
  {
    pred.prob=predict(fit.1.training, newx=x.test, s=lambda.seq[j], type="response")
    for(k in 1:length(pi0))
    {
      if(pred.prob>=pi0[k])
      {Y.pred.1='+'}else
      {Y.pred.1='-'}
      
      if((y.test=='+')&(Y.pred.1=='+'))
      {correct.num[j,k]=correct.num[j,k]+1}
      if((y.test=='-')&(Y.pred.1=='-'))
      {correct.num[j,k]=correct.num[j,k]+1}
    }
  }
}

# running time
time1 = proc.time()-initial.time; time1    

# implement the optimal lambda and optimal cut-off point pi0
accuracy.1=correct.num/nsample     
max(accuracy.1)       
jk.1 = which(accuracy.1==max(accuracy.1), arr.ind=TRUE); jk.1
j.1=jk.1[1,1]; j.1          
k.1=jk.1[1,2]; k.1          
lambda.opt.1 = lambda.seq[j.1]; lambda.opt.1       
pi0.opt.1 = pi0[k.1]; pi0.opt.1                     


###################################################
## apply classification rule to the test dataset ##
ntest = nrow(data_test)
test_predict.1 = NULL

y_test = data_test$V16
x_test = data.matrix(data_test[, -16])

fit.ridge =glmnet(x, y, family="binomial",  alpha = 0)
pred.prob=predict(fit.ridge, newx=x_test,s = lambda.opt.1,type="response")
test_predict.1[pred.prob >= pi0.opt.1]  = '+'
test_predict.1[pred.prob < pi0.opt.1]  = '-'
accuracy.ridge = mean(test_predict.1 == y_test)
accuracy.ridge         






### use Lasso penalty to build logistic regression model
nn = 100
x = data.matrix(data_train[, -16])
y = data_train$V16

library(glmnet)
fit.lasso=glmnet(x, y, family="binomial")

lambda.seq=fit.lasso$lambda
pi0=seq(0,1, length.out = nn)
correct.num.2=matrix(0, length(lambda.seq), length(pi0))
nsample=nrow(x)

initial.time = proc.time()

for(i in 1:nsample)
{
  x.train=x[-i,]
  x.test=matrix(x[i,],1,ncol(x))
  y.train=y[-i]
  y.test=y[i]
  fit.lasso.training=glmnet(x.train, y.train, family="binomial",alpha = 1)
  for(j in 1:length(lambda.seq))
  {
    pred.prob=predict(fit.lasso.training, newx=x.test, s=lambda.seq[j], type="response")
    for(k in 1:length(pi0))
    {
      if(pred.prob>=pi0[k])
      {Y.pred.1='+'}else
      {Y.pred.1='-'}
      
      if((y.test=='+')&(Y.pred.1=='+'))
      {correct.num.2[j,k]=correct.num.2[j,k]+1}
      if((y.test=='-')&(Y.pred.1=='-'))
      {correct.num.2[j,k]=correct.num.2[j,k]+1}
    }
  }
}

# running time
time2 = proc.time()-initial.time; time2       

# implement the optimal lambda and optimal cut-off point pi0
accuracy.2=correct.num.2/nsample
max(accuracy.2)       
jk = which(accuracy.2==max(accuracy.2), arr.ind=TRUE); jk
j=jk[1,1]; j
k=jk[1,2]; k
lambda.opt.2= lambda.seq[j]; lambda.opt.2   
pi0.opt.2 = pi0[k]; pi0.opt.2       



###################################################
## apply classification rule to the test dataset ##
ntest = nrow(data_test)
test_predict.2 = NULL

y_test = data_test$V16
x_test = data.matrix(data_test[, -16])

fit.lasso =glmnet(x, y, family="binomial",alpha = 1)
pred.prob.2=predict(fit.lasso, newx=x_test, type="response", s = lambda.opt.2)
test_predict.2[pred.prob.2 >= pi0.opt.2] = '+'
test_predict.2[pred.prob.2 < pi0.opt.2] = '-'
accuracy.lasso =mean(test_predict.2 == y_test)
accuracy.lasso  



# peek the variable selection
lambda.opt=lambda.seq[j]
fit.lasso=glmnet(x, y, family="binomial")
coef(fit.lasso, s=lambda.opt)