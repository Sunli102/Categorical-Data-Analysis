###############
### Project 1
###############

### Data1
data1 = read.csv('~/GSU/Categorical Analysis/Project1/project.1.data.1.csv', header = T)
str(data1)

sum(is.na(data1))  # check if there are missing values

# print the histogram of each variables separately
library(ggplot2)
for (i in (1:14))
{print(qplot(data1[,i], geom = 'histogram', xlab = names(data1[i])))}


# plot histogram of subsamples
op = par(mfrow = c(2,3))
for(i in 1:6)
{if(i == 6) {yy = 140} else{if (i== 4) {yy = 40} else {yy =120}}
  hist(data1[,i][data1$target==0],  border = 'blue', main = 
         paste('Histogram of ', names(data1[i])), xlab = names(data1[i]),
       ylab = 'absence / presence',ylim = c(0,yy))
  hist(data1[,i][data1$target>0], border = 'red',
       ylab = 'presence', add=T)
  if (i == 1)
  {legend(30,105, legend=c("absence", "presence"),
          col=c("blue", "red"), lty=1:1.5, cex=0.9)
  }
}

op = par(mfrow = c(2,3))
for(i in 7:12)
{if(i==9 | i == 12) {yy = 140} else{if (i == 8) {yy = 40} else {yy =100}}
  hist(data1[,i][data1$target==0],  border = 'blue', main = 
         paste('Histogram of ', names(data1[i])), xlab = names(data1[i]),
       ylab = 'absence / presence',ylim = c(0,yy))
  hist(data1[,i][data1$target>0], border = 'red',
       ylab = 'presence', add=T)
}



op = par(mfrow = c(2,3))
for(i in 13:14)
{ if (i == 14) {xx = c(-1,1)} else {xx = c(3,7)}
  hist(data1[,i][data1$target==0],  border = 'blue', main = 
         paste('Histogram of ', names(data1[i])), xlab = names(data1[i]),
       ylab = 'absence/present', xlim = xx, ylim = c(0,160))
  hist(data1[,i][data1$target>0], border = 'red',
       ylab = 'presence', add=T)
}


# Data processing
data1$sex[data1$sex == 0] = 'Female'
data1$sex[data1$sex == 1] = 'Male'
data1$sex = as.factor(data1$sex)

data1$cp[data1$cp == 1] = 'Typical Angina'
data1$cp[data1$cp == 2] = 'Atypical Angina'
data1$cp[data1$cp == 3] = 'Non-Angina pain'
data1$cp[data1$cp == 4] = 'Asymptomatic'
data1$cp = as.factor(data1$cp)

data1$fbs[data1$fbs == 0] = 'False'
data1$fbs[data1$fbs == 1] = 'True'
data1$fbs = as.factor(data1$fbs)

data1$restecg[data1$restecg == 0] = 'Normal'
data1$restecg[data1$restecg == 1] = 'ST-T Abnormal'
data1$restecg[data1$restecg == 2] = 'LV Hypertrophy'
data1$restecg = as.factor(data1$restecg)

data1$exang[data1$exang == 0] = 'No'
data1$exang[data1$exang == 1] = 'Yes'
data1$exang = as.factor(data1$exang)

data1$slope[data1$slope == 1] = 'Up'
data1$slope[data1$slope == 2] = 'Flat'
data1$slope[data1$slope == 3] = 'Down'
data1$slope = as.factor(data1$slope)

data1$thal[data1$thal == 3] = 'Normal'
data1$thal[data1$thal == 6] = 'Fixed defect'
data1$thal[data1$thal == 7] = 'Reversible defect'
data1$thal = as.factor(data1$thal)

# Relevel reference level for each categorical variable
data1$sex = relevel(data1$sex, 'Female')
data1$cp = relevel(data1$cp, 'Typical Angina')
data1$fbs = relevel(data1$fbs,'False')
data1$restecg = relevel(data1$restecg,'Normal')
data1$exang= relevel(data1$exang, 'No')
data1$slope = relevel(data1$slope, 'Up')
data1$thal = relevel(data1$thal,'Normal')

str(data1)
names(data1)

fit.full = glm(target~., data = data1, family = binomial)
fit.null = glm(target~1, data = data1, family = binomial)
select1.1 = step(fit.null, scope = list(lower = fit.null, upper = fit.full),
                direction = 'forward')



## interaction model
data1.1 = (select1.1$model)
colnames(data1.1)

pred = colnames(data1.1)[-1]
m = length(pred)


pred.names = NULL
for (i in 1:m)
{ pred.names = c(pred.names, pred[i])}

#pred.names = pred
for (i in 1:(m-1)){
  for (j in (i+1):m) {
    pred.names = c(pred.names, paste(pred[i], ':', pred[j]))
  }
}
pred.names

Formula = formula(paste('target ~ ', paste(pred.names, collapse = '+')))
Formula

fit.full.1 = glm(Formula, data = data1.1, family = binomial)
fit.null.1 = glm(target ~ 1, data = data1.1, family = binomial )

select1.2 = step(fit.null.1, scope = list(lower=fit.null.1, upper = fit.full.1),
                direction = 'forward')



# using data = data1, the original one
fit.final = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps +
                   thalach + thal:ca + slope:trestbps + thal:oldpeak + oldpeak:slope +
                   ca:thalach, data = data1, family = binomial)
summary(fit.final)
coef(fit.final)
confint(fit.final)


fit.final = glm(thal + ca + cp + oldpeak + slope + sex + trestbps + 
                  thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                  thal:ca + ca:thalach + ca:trestbps, data = data1, family = binomial)
summary(fit.final)
coef(fit.final)



fit.final = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
                  thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                  thal:ca + ca:thalach + ca:trestbps, data = data1, family = binomial)
summary(fit.final)



# using the data = data1.1, which is the data model with selected main effects
fit.final1 = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
                   thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                   thal:ca + ca:thalach + ca:trestbps, data = data1.1, family = binomial)
summary(fit.final1)









