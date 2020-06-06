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



# model
# Step:  AIC=223.98
# target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
#   exang + thalach


## interaction model
#str(select.1)
data1.1 = (select1.1$model)
colnames(data1.1)

pred = colnames(data1.1)[-1]
m = length(pred)

# Command+Shift + C keyboard shortcut)
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


# Step:  AIC=213.73
# target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
#   thalach + thal:ca + slope:trestbps + thal:oldpeak + oldpeak:slope + 
#   ca:thalach




# using data = data1, the original one
fit.final = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps +
                   thalach + thal:ca + slope:trestbps + thal:oldpeak + oldpeak:slope +
                   ca:thalach, data = data1, family = binomial)
summary(fit.final)

coef(fit.final)

confint(fit.final)

##########
# the coefficients with the number factors
# (Intercept)           thal6           thal7              ca             cp2 
# -3.39240904     -6.14884037      0.83283464     -1.42748231      1.66398718 
# cp3             cp4         oldpeak          slope2          slope3 
# 0.44442205      3.06994689     -0.73480834     -3.45545726     14.79102276 
# sex1        trestbps         thalach        thal6:ca        thal7:ca 
# 1.65494196      0.02694373     -0.03435759     20.18277870     -0.75611840 
# slope2:trestbps slope3:trestbps   thal6:oldpeak   thal7:oldpeak  oldpeak:slope2 
# 0.02866177     -0.12392157      2.50750261      1.26651496      1.36200638 
# oldpeak:slope3      ca:thalach 
# 1.57370447          0.01862994 

##########
# the coefficients with the variable names
# (Intercept)              thalFixed defect 
# -3.39240904                   -6.14884037 
# thalReversible defect                            ca 
# 0.83283464                   -1.42748231 
# cpAsymptomatic             cpAtypical Angina 
# 3.06994689                    1.66398718 
# cpNon-Angina pain                       oldpeak 
# 0.44442205                   -0.73480834 
# slopeDown                     slopeFlat 
# 14.79102276                   -3.45545726 
# sexMale                      trestbps 
# 1.65494196                    0.02694373 
# thalach           thalFixed defect:ca 
# -0.03435759                   20.18277870 
# thalReversible defect:ca            slopeDown:trestbps 
# -0.75611840                   -0.12392157 
# slopeFlat:trestbps      thalFixed defect:oldpeak 
# 0.02866177                    2.50750261 
# thalReversible defect:oldpeak             oldpeak:slopeDown 
# 1.26651496                    1.57370447 
# oldpeak:slopeFlat                    ca:thalach 
# 1.36200638                    0.01862944 


    
  




























# data1$ca[data1$ca == 0] = 'Null'
# data1$ca[data1$ca == 1] = 'Single Vessel'
# data1$ca[data1$ca == 0] = 'Double Vessel'
# data1$ca[data1$ca == 0] = 'Triple Vessel'
# data1$ca = as.factor(data1$ca)
#############################################################
#############################################################
### factor Ca

#### factor Ca
# Step:  AIC=219.74
# target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
#   exang + thalach


# Step:  AIC=203.33
# target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
#   thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
#   thal:ca + ca:thalach + ca:trestbps


fit.final = glm(thal + ca + cp + oldpeak + slope + sex + trestbps + 
                  thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                  thal:ca + ca:thalach + ca:trestbps, data = data1, family = binomial)
summary(fit.final)

coef(fit.final)


# Step:  AIC=203.33
# target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
#   thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
#   thal:ca + ca:thalach + ca:trestbps



fit.final = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
                  thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                  thal:ca + ca:thalach + ca:trestbps, data = data1, family = binomial)
summary(fit.final)

# coef(fit.final)
# > coef(fit.final)
# (Intercept)           thal6           thal7             ca1             ca2 
# -3.58296195     -7.20382375      0.49396969     -2.86758813     -3.69873010 
# ca3             cp2             cp3             cp4         oldpeak 
# 1592.50087658      1.57793101      0.16132330      2.96379136     -1.56100810 
# slope2          slope3            sex1        trestbps         thalach 
# -2.49759471     31.92396979      1.91542445      0.04481535     -0.04956423 
# slope2:trestbps slope3:trestbps  oldpeak:slope2  oldpeak:slope3   thal6:oldpeak 
# 0.02003521     -0.27585088      2.30432611      3.67220310      3.06574607 
# thal7:oldpeak       thal6:ca1       thal7:ca1       thal6:ca2       thal7:ca2 
# 1.83254181     28.71459544     -0.32401190     26.69212215     27.78101090 
# thal6:ca3       thal7:ca3     ca1:thalach     ca2:thalach     ca3:thalach 
# -276.14232025   -327.08538798      0.06970520      0.07874153    -15.47392799 
# ca1:trestbps    ca2:trestbps    ca3:trestbps 
# -0.04127979     -0.04386037      7.49855970 


# using the data = data1.1, which is the data model with selected main effects
fit.final1 = glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + 
                   thalach + slope:trestbps + oldpeak:slope + thal:oldpeak + 
                   thal:ca + ca:thalach + ca:trestbps, data = data1.1, family = binomial)
summary(fit.final1)



#par('mar')
#par(mar=c(1,1,1,1))







