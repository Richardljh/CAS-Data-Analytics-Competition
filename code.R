##########################################
#########  CAS 数据分析比赛
#########  数据预处理
#########  张艺凡 李嘉弘
#########  April 1, 2022
##########################################


##########################################
#########  导入数据及包
##########################################
rm(list = ls())
library(tidyverse)
library(readxl)
library(gridExtra)
library(gbm)
library(xgboost)
library(cplm)
library(sampling)

##setwd('C://Users//HP//Desktop')
#setwd('C://Richard//cas competition//2nd round')

dat <- read_excel("History data.xlsx")
test_set <- read.csv("pricing data.csv")
str(dat)

table(dat$ClaimNb)
(dat$freq<-dat$ClaimNb/dat$Exposure)
#table(dat$freq)
#发生四次索赔以上的保比较异常，考虑去掉
dat = dat[-which(dat$ClaimNb>4),]
#dat$ClaimNb <- pmin(dat$ClaimNb, 4)
table(dat$ClaimNb)
sum(dat$ClaimNb)/sum(dat$Exposure)
#boxplot(dat$Exposure)
#hist(dat$Exposure)
# 大部分保单的风险暴露低于一年，所以考虑把所有大于一年的风险暴露保单取为一年
#dat$Exposure <- pmin(dat$Exposure, 1)

## 直方图
ggplot(dat, aes(x = ClaimNb)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), col = 'black') + 
  labs(y = 'Proportion') + scale_y_continuous(labels = scales::percent) + 
  labs(x = 'The number of claims')
gridExtra::grid.arrange(
  ggplot(dat, aes(x = Exposure)) +
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.05, col = 'black') +
    labs(y = 'Proportion') + scale_y_continuous(labels = scales::percent) + 
    labs(x = 'The number of policy years'),
  ggplot(test_set, aes(x = Exposure)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.05, col = 'black') +
    labs(y = 'Proportion') + scale_y_continuous(labels = scales::percent) + 
    labs(x = 'The number of policy years'),
  ncol = 2
)

# 定义Poisson偏差损失函数
Poisson.Deviance <- function(pred, obs){2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}
# 平均索赔频率
#sum(dat$freq)/count(dat)
(lambda_bar <- sum(dat$ClaimNb)/sum(dat$Exposure))
# 对应的Poisson偏差损失
(Poisson.Deviance(lambda_bar*dat$Exposure, dat$ClaimNb))

##########################################
#########  广义线性模型数据预处理
##########################################

dat2 <- dat
str(dat2)


testset<- test_set
str(testset)

###### Area

table(dat2$Area)
dat2$Area <- as.factor(dat2$Area)
testset$Area <- as.factor(testset$Area)

dat2$AreaGLM <- dat2$Area
feq_by_Area <- dat2 %>%
  group_by(AreaGLM) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))
testset$AreaGLM <- testset$Area

feq_by_Area
ggplot(feq_by_Area, aes(x = AreaGLM)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')
dat2$AreaGLM <- relevel(dat2$AreaGLM, 'C')# 把基准放到数量最多的level
testset$AreaGLM <- relevel(testset$AreaGLM, 'C')

###### VehPower 
dat2$VehPowerGLM <- as.factor(dat2$VehPower)
table(dat2$VehPowerGLM)
dat2$VehPowerGLM <- as.factor(pmin(dat2$VehPower,11))
table(dat2$VehPowerGLM)
testset$VehPowerGLM <- as.factor(pmin(testset$VehPower,11))

feq_by_VehPowerGLM<- dat2 %>%
  group_by(VehPowerGLM) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))

feq_by_VehPowerGLM
ggplot(feq_by_VehPowerGLM, aes(x = VehPowerGLM)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')

dat2$VehPowerGLM <- relevel(dat2$VehPowerGLM, '6')# 把基准放到数量最多的level
testset$VehPowerGLM <- relevel(testset$VehPowerGLM, '6')
table(testset$VehPowerGLM )
###### VehAge
summary(dat2$VehAge)
ggplot(dat2, aes(x = VehAge)) + 
  geom_density(adjust = 2, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'VehAge')

VehAgeGLM <- cbind(c(0:110), c(1, rep(2,5),rep(3,5), rep(4,100)))
## 将VehAge按照1年内，2-5年，6-10年，11-100年变为因子型
dat2$VehAgeGLM <- as.factor(VehAgeGLM[dat2$VehAge+1,2]) 
testset$VehAgeGLM <- as.factor(VehAgeGLM[testset$VehAge+1,2]) 
table(dat2$VehAgeGLM)


dat2$VehAgeGLM <-relevel(dat2$VehAgeGLM, '2')

testset$VehAgeGLM <-relevel(testset$VehAgeGLM, '2')


feq_by_VehAgeGLM<- dat2 %>%
  group_by(VehAgeGLM) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))

feq_by_VehAgeGLM
ggplot(feq_by_VehAgeGLM, aes(x = VehAgeGLM)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')


######DriveAge
summary(dat2$DrivAge)
ggplot(dat2, aes(x = DrivAge)) + 
  geom_density(adjust = 2, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'VehAge')

DrivAgeGLM <- cbind(c(18:100), c(rep(1,3), rep(2,5), rep(3,5), rep(4,10), rep(5,10), rep(6,20), rep(7,30)))
dat2$DrivAgeGLM <- as.factor(DrivAgeGLM[dat2$DrivAge-17,2])
testset$DrivAgeGLM <- as.factor(DrivAgeGLM[testset$DrivAge-17,2])

feq_by_DrivAgeGLM<- dat2 %>%
  group_by(DrivAgeGLM) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))

feq_by_DrivAgeGLM
ggplot(feq_by_DrivAgeGLM, aes(x = DrivAgeGLM)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')
dat2$DrivAgeGLM <-relevel(dat2$DrivAgeGLM, ref='6')
testset$DrivAgeGLM <-relevel(testset$DrivAgeGLM, ref='6')
######BonusMalus
summary(dat2$BonusMalus)
ggplot(dat2, aes(x = BonusMalus)) + 
  geom_density(adjust = 5, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'BonusMalus')

dat2$BonusMalusGLM <- pmin(dat2$BonusMalus, 150)
testset$BonusMalusGLM <- pmin(testset$BonusMalus, 150)
ggplot(dat2, aes(x = BonusMalusGLM)) + 
  geom_density(adjust = 5, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'BonusMalus')

cor.test(dat2$freq, dat2$BonusMalusGLM)

#####VehBrand
dat2$VehBrand <- as.factor(dat2$VehBrand)
testset$VehBrand <- as.factor(testset$VehBrand)

table(dat2$VehBrand)

feq_by_VehBrand<- dat2 %>%
  group_by(VehBrand) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))

feq_by_VehBrand
ggplot(feq_by_VehBrand, aes(x = VehBrand)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')

dat2$VehBrand <-relevel(dat2$VehBrand, ref='B12')
testset$VehBrand <-relevel(testset$VehBrand, ref='B12')
#####VehGas
dat2$VehGas <- as.factor(dat2$VehGas)
testset$VehGas <- as.factor(testset$VehGas)
table(dat2$VehGas)

feq_by_VehGas<- dat2 %>%
  group_by(VehGas) %>%
  summarise(Count = n(),
            Claim_freq = sum(ClaimNb)/sum(Exposure))

feq_by_VehGas
ggplot(feq_by_VehGas, aes(x = VehGas)) + 
  geom_bar(mapping = aes(y = Claim_freq), col = 'black',stat= 'identity') + 
  labs(y = 'Claim Frequency')

dat2$VehGas <-relevel(dat2$VehGas, ref='Regular')
testset$VehGas <-relevel(testset$VehGas, ref='Regular')
######Density
summary(dat2$Density)
ggplot(dat2, aes(x = Density)) + 
  geom_density(adjust = 10, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'Density')
## Density成厚尾特称，考虑取对数
dat2$DensityGLM <- as.numeric(log(dat2$Density))
testset$DensityGLM <- as.numeric(log(testset$Density))
ggplot(dat2, aes(x = DensityGLM)) + 
  geom_density(adjust = 3, col = 'black', fill = 'gray') +
  labs(y = 'Density') +
  labs(x = 'logDensity')

cor.test(dat2$freq, dat2$DensityGLM)

######Region
table(dat2$Region)
dat2$Region <- relevel(as.factor(dat2$Region), 'R24')
testset$Region<-relevel(as.factor(testset$Region), 'R24')

#####划分训练集和测试集

prop_train = 0.8

dat2 <- arrange(dat2, Exposure)
dat2$stra <- as.factor(rep(1:10, each=round(nrow(dat2)/10)))
set.seed(1010)
sub_train = strata(dat2, stratanames=("stra"),
                   size=rep(round(prop_train*nrow(dat2)/10),10),method="srswor")
learn.GLM <- dat2[sub_train$ID_unit,]
test.GLM <- dat2[-sub_train$ID_unit,]
learn.GLM$Claimfreq = learn.GLM$ClaimNb/learn.GLM$Exposure
test.GLM$Claimfreq = test.GLM$ClaimNb/test.GLM$Exposure
n_l <- nrow(learn.GLM)
n_t <- nrow(test.GLM)

## 同质保单建模
learn.GLM$logexpo <- log(learn.GLM$Exposure)
test.GLM$logexpo <- log(test.GLM$Exposure)
testset$logexpo <- log(testset$Exposure)

(homo_lambda<- sum(learn.GLM$ClaimNb)/sum(learn.GLM$Exposure))
learn.GLM$homofit <- homo_lambda*learn.GLM$Exposure
test.GLM$homofit <- homo_lambda*test.GLM$Exposure

Poisson.Deviance(learn.GLM$homofit, learn.GLM$ClaimNb)
Poisson.Deviance(test.GLM$homofit, test.GLM$ClaimNb)

### 广义线性模型
{t1 <- proc.time()
  d.glm1 <- glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM
                + VehBrand + VehGas + DensityGLM + Region + AreaGLM + offset(log(Exposure)), 
                data=learn.GLM, family=poisson())
  (proc.time()-t1)[3]}

summary(d.glm1)  

learn.GLM$GLM1fit <- fitted(d.glm1)
learn.GLM$GLM1link <- log(learn.GLM$GLM1fit/learn.GLM$Exposure)
test.GLM$GLM1fit <- predict(d.glm1, newdata=test.GLM, type="response")
test.GLM$GLM1link <- log(test.GLM$GLM1fit/test.GLM$Exposure)
testset$GLM1fit <- predict(d.glm1, newdata = testset, type = 'response')
testset$GLM1link <- log(testset$GLM1fit/testset$Exposure)

Poisson.Deviance(learn.GLM$GLM1fit, learn.GLM$ClaimNb)
(D.glm1<-Poisson.Deviance(test.GLM$GLM1fit, test.GLM$ClaimNb))


### 将log(exposure)作为解释变量
{t1 <- proc.time()
  d.glm5 <- glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM
                + VehBrand + VehGas + DensityGLM + Region + AreaGLM + log(Exposure), 
                data=learn.GLM, family=poisson())
  (proc.time()-t1)[3]}

summary(d.glm5)  

learn.GLM$GLM5fit <- fitted(d.glm5)
learn.GLM$GLM5link <- log(learn.GLM$GLM5fit)
test.GLM$GLM5fit <- predict(d.glm5, newdata=test.GLM, type="response")
test.GLM$GLM5link <- log(test.GLM$GLM5fit)
testset$GLM5fit <- predict(d.glm5, newdata = testset, type = 'response')
testset$GLM5link <- log(testset$GLM5fit)

Poisson.Deviance(learn.GLM$GLM5fit, learn.GLM$ClaimNb)
(D.glm5<-Poisson.Deviance(test.GLM$GLM5fit, test.GLM$ClaimNb))


## GBM（梯度提升算法）

set.seed(123)
d.gbm3<- gbm(
  formula = ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
  + VehBrand + VehGas + Density + Region + Area + offset(log(Exposure)),
  data = learn.GLM,
  distribution = "poisson",
  n.trees = 800,
  shrinkage = 0.25,
  interaction.depth =2,
  bag.fraction = 1,
  cv.folds = 5,
  n.minobsinnode = 1000,
  verbose = F
)

best.iter.3<-gbm.perf(d.gbm3, method="cv")

learn.GLM$GBM3fit<-predict(d.gbm3, newdata = learn.GLM, n.tress = best.iter.3,
                           type = "response")*learn.GLM$Exposure
learn.GLM$GBM3link <- log(learn.GLM$GBM3fit/learn.GLM$Exposure)
test.GLM$GBM3fit<-predict(d.gbm3, newdata = test.GLM, n.tress = best.iter.3,
                          type = "response")*test.GLM$Exposure
test.GLM$GBM3link <- log(test.GLM$GBM3fit/test.GLM$Exposure)
testset$GBM3fit<-predict(d.gbm3, newdata = testset, n.tress = best.iter.3,
                         type = "response")*testset$Exposure
testset$GBM3link <- log(testset$GBM3fit/testset$Exposure)
Poisson.Deviance(learn.GLM$GBM3fit, learn.GLM$ClaimNb)
(D.gbm3 <- Poisson.Deviance(test.GLM$GBM3fit, test.GLM$ClaimNb))

set.seed(123)
d.gbm4<- gbm(
  formula = ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
  + VehBrand + VehGas + Density + Region + Area + logexpo,
  data = learn.GLM,
  distribution = "poisson",
  n.trees = 800,
  shrinkage = 0.25,
  interaction.depth =2,
  bag.fraction = 1,
  #train.fraction = 0.9,
  cv.folds = 5,
  n.minobsinnode = 1000,
  verbose = F
)

best.iter.4<-gbm.perf(d.gbm4, method="cv")

learn.GLM$GBM4fit<-predict(d.gbm4, newdata = learn.GLM, n.tress = best.iter.4,
                           type = "response")
learn.GLM$GBM4link <- log(learn.GLM$GBM4fit)
test.GLM$GBM4fit<-predict(d.gbm4, newdata = test.GLM, n.tress = best.iter.4,
                          type = "response")
test.GLM$GBM4link <- log(test.GLM$GBM4fit)
testset$GBM4fit<-predict(d.gbm4, newdata = testset, n.tress = best.iter.4,
                         type = "response")
testset$GBM4link <- log(testset$GBM4fit)
Poisson.Deviance(learn.GLM$GBM4fit, learn.GLM$ClaimNb)
(D.gbm4 <- Poisson.Deviance(test.GLM$GBM4fit, test.GLM$ClaimNb))

## xgboost

xgb_formula.1 <- as.formula('ClaimNb ~ VehPower + VehAge + DrivAge + 
                            BonusMalus + VehBrand + VehGas + 
                            Density + Region + Area + 
                            offset(log(Exposure))')
data.training.mm.1 <- model.matrix(xgb_formula.1, data = learn.GLM)
data.training.dm.1 <- xgb.DMatrix(data.training.mm.1, 
                                  label = learn.GLM$ClaimNb)
data.testing.mm.1 <- model.matrix(xgb_formula.1, data = test.GLM)
data.testing.dm.1 <- xgb.DMatrix(data.testing.mm.1, label = test.GLM$ClaimNb)
testset$ClaimNb <- rep(0,nrow(testset))
testset$Claimfreq <- rep(0,nrow(testset))
data.testing.mm.t <- model.matrix(xgb_formula.1, data = testset)
data.testing.dm.t <- xgb.DMatrix(data.testing.mm.t,label = testset$ClaimNb)

par.3 <- list(
  "booster" = "gbtree", # We are using a decision tree - alternatively we could use a GLM (gblinear).
  "objective" = "count:poisson", # Poisson损失
  #"eval_metric" = "poisson-nloglik", # Poisson负对数似然函数
  "eta" = 0.5, # Learning rate.
  "max_depth" = 3,
  "min_child_weight" = 0,
  "subsample" = 0.7, # Proportion of observations.
  "colsample_bytree" = 0.4, # Proportion of features.
  "gamma" = 5,
  "lambda" = 1,
  "alpha" = 1
)


set.seed(1010)
d.xgb3 <- xgboost(
  params = par.3,
  data = data.training.dm.1,
  nrounds =  1772,
  #1113,#1022
  print_every_n = 100,
  prediction = FALSE
)

learn.GLM$XGB3fit <- predict(d.xgb3, newdata = data.training.dm.1, type = 'response')
learn.GLM$XGB3link <- log(learn.GLM$XGB3fit/learn.GLM$Exposure)
test.GLM$XGB3fit <- predict(d.xgb3, newdata = data.testing.dm.1, type = 'response')
test.GLM$XGB3link <- log(test.GLM$XGB3fit/test.GLM$Exposure)
testset$XGB3fit <- predict(d.xgb3, newdata = data.testing.dm.t, type = 'response')
testset$XGB3link <- log(testset$XGB3fit/testset$Exposure)
Poisson.Deviance(learn.GLM$XGB3fit, learn.GLM$ClaimNb)
Poisson.Deviance(test.GLM$XGB3fit, test.GLM$ClaimNb)

xgb_formula.4 <- as.formula('ClaimNb ~ VehPower + VehAge + DrivAge + 
                            BonusMalus + VehBrand + VehGas + 
                            Density + Region + Area + 
                            log(Exposure)')
data.training.mm.4 <- model.matrix(xgb_formula.4, data = learn.GLM)
data.training.dm.4 <- xgb.DMatrix(data.training.mm.4, 
                                  label = learn.GLM$ClaimNb)
data.testing.mm.4 <- model.matrix(xgb_formula.4, data = test.GLM)
data.testing.dm.4 <- xgb.DMatrix(data.testing.mm.4, label = test.GLM$ClaimNb)

par.4 <- list(
  "booster" = "gbtree", # We are using a decision tree - alternatively we could use a GLM (gblinear).
  "objective" = "count:poisson", # Poisson损失
  #"eval_metric" = "poisson-nloglik", # Poisson负对数似然函数
  "eta" = 0.25,#0.5, # Learning rate.
  "max_depth" = 3,
  "min_child_weight" = 0,
  "subsample" = 0.7,#0.7, # Proportion of observations.
  "colsample_bytree" = 0.4,#0.4, # Proportion of features.
  "gamma" = 3,
  "lambda" = 1,
  "alpha" = 1
)


set.seed(1010)
d.xgb4 <- xgboost(
  params = par.4,
  data = data.training.dm.4,
  nrounds = 2283,
  #1113,#1022
  print_every_n = 100,
  prediction = FALSE
)

learn.GLM$XGB4fit <- predict(d.xgb4, newdata = data.training.dm.4, type = 'response')
learn.GLM$XGB4link <- log(learn.GLM$XGB4fit)
test.GLM$XGB4fit <- predict(d.xgb4, newdata = data.testing.dm.4, type = 'response')
test.GLM$XGB4link <- log(test.GLM$XGB4fit)
xgb_formula.2 <- as.formula('ClaimNb ~ VehPower + VehAge + DrivAge + 
                            BonusMalus + VehBrand + VehGas + 
                            Density + Region + Area + 
                            log(Exposure)')
data.testing.mm.t2 <- model.matrix(xgb_formula.2, data = testset)
data.testing.dm.t2 <- xgb.DMatrix(data.testing.mm.t2,label = testset$ClaimNb)
testset$XGB4fit <- predict(d.xgb4, newdata = data.testing.dm.t2, type = 'response')
testset$XGB4link <- log(testset$XGB4fit)

Poisson.Deviance(learn.GLM$XGB4fit, learn.GLM$ClaimNb)
Poisson.Deviance(test.GLM$XGB4fit, test.GLM$ClaimNb)

par.6 <- list(
  "booster" = "gbtree", # We are using a decision tree - alternatively we could use a GLM (gblinear).
  "objective" = "count:poisson", # Poisson损失
  #"eval_metric" = "poisson-nloglik", # Poisson负对数似然函数
  "eta" = 0.15, # Learning rate.
  "max_depth" = 7,
  "min_child_weight" = 0,
  "subsample" = 0.7, # Proportion of observations.
  "colsample_bytree" = 0.4, # Proportion of features.
  "gamma" = 3,
  "lambda" = 1,
  "alpha" = 1
)

set.seed(1010)
d.xgb6 <- xgboost(
  params = par.6,
  data = data.training.dm.4,
  nrounds = 566,
  #1113,#1022
  print_every_n = 100,
  prediction = FALSE
)

learn.GLM$XGB6fit <- predict(d.xgb6, newdata = data.training.dm.4, type = 'response')
learn.GLM$XGB6link <- log(learn.GLM$XGB6fit)
test.GLM$XGB6fit <- predict(d.xgb6, newdata = data.testing.dm.4, type = 'response')
test.GLM$XGB6link <- log(test.GLM$XGB6fit)
testset$XGB6fit <- predict(d.xgb6, newdata = data.testing.dm.t2, type = 'response')
testset$XGB6link <- log(testset$XGB6fit)
Poisson.Deviance(learn.GLM$XGB6fit, learn.GLM$ClaimNb)
Poisson.Deviance(test.GLM$XGB6fit, test.GLM$ClaimNb)

### Meta GLM

meta.glm2 <- glm(ClaimNb ~ GLM1link + GBM3link + XGB3link+ offset(log(Exposure)),
                 data = learn.GLM, family = poisson())
summary(meta.glm2)
learn.GLM$metafit2 <- predict(meta.glm2, newdata = learn.GLM, type='response')
test.GLM$metafit2 <- predict(meta.glm2, newdata = test.GLM, type='response')
testset$metafit2 <- predict(meta.glm2, newdata = testset, type='response')
Poisson.Deviance(learn.GLM$metafit2, learn.GLM$ClaimNb)
(d.mt2<-Poisson.Deviance(test.GLM$metafit2, test.GLM$ClaimNb))

meta.glm4 <- glm(ClaimNb ~ GLM5link + XGB6link + log(Exposure),
                 data = learn.GLM, family = poisson())
summary(meta.glm4)
learn.GLM$metafit4 <- predict(meta.glm4, newdata = learn.GLM, type='response')
test.GLM$metafit4 <- predict(meta.glm4, newdata = test.GLM, type='response')
testset$metafit4 <- predict(meta.glm4, newdata = testset, type='response')
Poisson.Deviance(learn.GLM$metafit4, learn.GLM$ClaimNb)
(d.mt4<-Poisson.Deviance(test.GLM$metafit4, test.GLM$ClaimNb))

### 折扣分析
##建立等10组等保单数组合
no.group <- 10
target.lr <- 1
quantile_l <- rep(0, no.group)
for(i in 1:no.group){
  quantile_l[i] <- quantile(learn.GLM[['XGB6fit']], i/no.group)
}
quantile_l

###按照lambda的阈值给learn.GLM分组

learn.GLM$risk_group <- 1
for(i in 1:(no.group-1)){
  lll <- learn.GLM[['XGB6fit']] > quantile_l[i]
  learn.GLM$risk_group[lll] <- i+1
}
learn.GLM_byrisk <- learn.GLM %>%
  group_by(risk_group)%>%
  summarise(Count = n(),
            Total_Exposure = sum(Exposure),
            Claim_freq = sum(ClaimNb)/n(),
            loss_ratio = sum(ClaimNb)/sum(XGB6fit),
            discount = loss_ratio/target.lr
  )

learn.GLM_byrisk

### 计算折扣和折扣后保费

discount <- learn.GLM_byrisk$discount
test.GLM$risk_group <- 1
testset$risk_group <- 1

for(i in 1:(no.group-1)){
  lll <- test.GLM[['XGB6fit']] > quantile_l[i]
  test.GLM$risk_group[lll] <- i+1
  testset$risk_group[testset[['XGB6fit']] > quantile_l[i]]<- i+1
}

test.GLM$discount = 1
learn.GLM$discount = 1
testset$discount = 1
for(i in 1:no.group){
  test.GLM$discount[test.GLM$risk_group == i] = discount[i]
  learn.GLM$discount[learn.GLM$risk_group == i] = discount[i]
  testset$discount[testset$risk_group == i] = discount[i]
}

#test.GLM$risk_group <- as.factor(test.GLM$risk_group)
test.GLM$prem <- test.GLM[['XGB6fit']]*test.GLM$discount
learn.GLM$prem <- learn.GLM[['XGB6fit']]*learn.GLM$discount
testset$premium <- testset[['XGB6fit']]*testset$discount

test.GLM_byrisk <- test.GLM %>%
  group_by(risk_group) %>%
  summarise(Count = n(),
            Total_Exposure = sum(Exposure),
            Claim_freq = sum(ClaimNb)/n(),
            loss_ratio = sum(ClaimNb)/sum(prem),
            Discount = mean(discount)
  )

test.GLM_byrisk

##提升度计算函数

tishengdu <- function(obs, pred, num_group){
  rank_test <- data.frame(ClaimNb = obs, Model.fit = pred)
  rank_test <- arrange(rank_test, Model.fit)
  rank_test$group <- rep(1:num_group, each=round(nrow(rank_test)/num_group))
  rank_test <- rank_test %>%
    group_by(group) %>%
    summarise(Total.loss = sum(ClaimNb))
  return(rank_test$Total.loss[num_group]/rank_test$Total.loss[1])
}

tishengdu(test.GLM$ClaimNb, test.GLM$XGB4fit, 50)
tishengdu(test.GLM$ClaimNb, test.GLM$XGB6fit, 50)
tishengdu(test.GLM$ClaimNb, test.GLM$metafit2, 50)
tishengdu(test.GLM$ClaimNb, test.GLM$metafit4, 50)

##打平函数（按照保单数分组打平）
daping <- function(model_name, no.group, target.lr){
  quantile_l <- rep(0, no.group)
  for(i in 1:no.group){
    quantile_l[i] <- quantile(learn.GLM[[model_name]], (i-1)/no.group)
  }
  learn.GLM$risk_group <- 1
  for(i in 1:(no.group-1)){
    lll <- learn.GLM[[model_name]] > quantile_l[i+1]
    learn.GLM$risk_group[lll] <- i+1
  }
  learn.GLM$model.netprem <- learn.GLM[[model_name]]
  learn.GLM_byrisk <- learn.GLM %>%
    group_by(risk_group) %>%
    summarise(loss_ratio = sum(ClaimNb)/sum(model.netprem),
              discount = loss_ratio/target.lr)
  discount <- learn.GLM_byrisk$discount
  test.GLM$risk_group <- 1
  for(i in 1:(no.group-1)){
    lll <- test.GLM[[model_name]] > quantile_l[i+1]
    test.GLM$risk_group[lll] <- i+1
  }
  test.GLM$discount = discount[1]
  learn.GLM$discount = discount[1]
  for(i in 1:no.group){
    learn.GLM$discount[learn.GLM$risk_group == i] = discount[i]
    test.GLM$discount[test.GLM$risk_group == i] = discount[i]
  }
  prem.test <- test.GLM[[model_name]]*test.GLM$discount
  prem.learn <- learn.GLM[[model_name]]*learn.GLM$discount
  prem <- list(prem.learn, prem.test)
  return(prem)
}

## 模拟分析
premium_test <- data.frame(IDpol = test.GLM$IDpol,
                           ClaimNb = test.GLM$ClaimNb,
                           Exposure = test.GLM$Exposure,
                           #GLM5fit = daping('GLM5fit',10, 1)[[2]],
                           XGB4fit = test.GLM$XGB4fit,
                           XGB6fit = test.GLM$XGB6fit/0.95)
                           #metafit2 = daping('metafit2',10, 1)[[2]],
                           #metafit4 = daping('metafit4',10, 1)[[2]])
model.list<-c('XGB4fit','XGB6fit')
n = length(model.list)

##竞争函数
competition <- function(cmlist,cmdata){
  ln=length(cmlist)
  kd=cmdata[,colnames(cmdata) %in% cmlist]
  com_data<-data.frame(ID=cmdata$IDpol,ClaimNb=cmdata$ClaimNb,kd)
  # dim(com_data)
  for (i in 1:nrow(com_data)){
    com_data$price[i]=ifelse(length(cmlist)>1,min(kd[i,]),kd[i])
    com_data$group[i]=cmlist[ifelse(length(cmlist)>1,which.min(kd[i,]),1)]
  }
  CMT <- matrix(rep(0,2*n), nrow = 2, ncol = n,
                dimnames=list(c("premium","loss"),model.list))
  for(j in 1:n){
    #计算竞争后每个模型的保费规模和赔付率
    CMT[1,model.list[j]]=sum(com_data[com_data$group==model.list[j],]$price)
    # pr[1,"id"]=1
    CMT[2,model.list[j]]=sum(com_data[com_data$group==model.list[j],]$ClaimNb)/sum(com_data[com_data$group==model.list[j],]$price)
  }
  return(CMT)
}

#模拟100次竞争
sample_time=100
success=matrix(rep(0,sample_time*n),nrow=sample_time,ncol=n)
colnames(success)=c(model.list)
pr=matrix(rep(0,sample_time*n),nrow=sample_time,ncol=n)
colnames(pr)=c(model.list)
loss=matrix(rep(0,sample_time*n),nrow=sample_time,ncol=n)
colnames(loss)=c(model.list)

set.seed(1050)
for(m in 1:sample_time){
  #每次随机抽取10000张保单参与竞价
  ll <- sample(c(1:nrow(premium_test)), 10000, replace = FALSE)
  newdata <- premium_test[ll,]
  F_ls=model.list
  S_ls=model.list
  while (length(F_ls)>0 ) {  #当失败集合不为空时，开始循环；当失败集合为空时结束循环
    if(length(S_ls)>=1){  #当成功集合至少有一个数时开始下一次竞价
      cmp=competition(S_ls,newdata)
      F_ls=model.list[which(cmp["loss",]>=1.5)]  #记录失败模型
      S_ls=model.list[which(cmp["loss",]<1.5)]   #记录成功模型
    }
    else {break}
  }
  #记录最终竞价的保费、赔付率、成功者
  if(length(S_ls)==0){cmp=matrix(nrow=nrow(cmp),ncol=ncol(cmp))}
  else{ pr[m,]=cmp[1,]  
  loss[m,]=cmp[2,]
  for(j in 1:n){
    success[m,model.list[j]]=ifelse(model.list[j] %in% S_ls,1,0)
  }
  }
  print(m)
}

simu_result <- matrix(rep(0,7*n), nrow = 7, ncol = n,
                      dimnames=list(c("successtime","meanpremium","mean_lr","sd_prem","sd_lr","ce_prem","ce_lr"),model.list))
for(j in 1:n){
  simu_result[1,model.list[j]]=sum(success[,model.list[j]])
  simu_result[2,model.list[j]]=sum(subset(pr[,model.list[j]],pr[,model.list[j]]>0))/sum(success[,model.list[j]])
  simu_result[3,model.list[j]]=sum(subset(loss[,model.list[j]],loss[,model.list[j]]>0))/sum(success[,model.list[j]])
  simu_result[4,model.list[j]]=sd(subset(pr[,model.list[j]],pr[,model.list[j]]>0))
  simu_result[5,model.list[j]]=sd(subset(loss[,model.list[j]],loss[,model.list[j]]>0))
  simu_result[6,model.list[j]]=sd(subset(pr[,model.list[j]],pr[,model.list[j]]>0))/sum(subset(pr[,model.list[j]],pr[,model.list[j]]>0))*sum(success[,model.list[j]])
  simu_result[7,model.list[j]]=sd(subset(loss[,model.list[j]],loss[,model.list[j]]>0))/sum(subset(loss[,model.list[j]],loss[,model.list[j]]>0))*sum(success[,model.list[j]])
}
simu_result

##比较几个定价结果的基尼系数
gini(loss = 'ClaimNb', score = c('XGB4fit','XGB6fit'), data = premium_test)

##比较几个定价结果的提升度
tishengdu(premium_test$ClaimNb, premium_test$XGB6fit_un, 50)
tishengdu(premium_test$ClaimNb, premium_test$XGB6fit, 50)
tishengdu(premium_test$ClaimNb, premium_test$metafit2, 50)
tishengdu(premium_test$ClaimNb, premium_test$metafit4, 50)

##综上所述，无论从样本外损失，gini系数，
##提升度和模拟竞争结果上来看XGB6都有着比较大的优势
##不进行打平的效果稍好于进行打平的结果
##因此我们选择最终的xgb6作为我们最终的定价模型，并且定价结果不进行打平处理

##采用全数据集训练xgb6
data.training.mm.all <- model.matrix(xgb_formula.4, data = dat2)
data.training.dm.all <- xgb.DMatrix(data.training.mm.all, 
                                    label = dat2$ClaimNb)
set.seed(1010)
d.xgb.all <- xgboost(
  params = par.6,
  data = data.training.dm.all,
  nrounds = 566,
  #1113,#1022
  print_every_n = 100,
  prediction = FALSE
)

##报价数据预测
testset$XGB6fit.all <- predict(d.xgb.all, newdata = data.testing.dm.t2, type = 'response')
write.csv(testset, '光速-43.csv')
dat2$XGB6fit <- predict(d.xgb.all, newdata = data.training.dm.all, 
                            type = 'response')

no.group <- 10
target.lr <- 1
quantile_l <- rep(0, no.group)
for(i in 1:no.group){
  quantile_l[i] <- quantile(dat2[['XGB6fit']], i/no.group)
}
quantile_l

###按照lambda的阈值给learn.GLM分组

dat2$risk_group <- 1
for(i in 1:(no.group-1)){
  lll <- dat2[['XGB6fit']] > quantile_l[i]
  dat2$risk_group[lll] <- i+1
}
dat2_byrisk <- dat2 %>%
  group_by(risk_group)%>%
  summarise(Count = n(),
            Total_Exposure = sum(Exposure),
            Claim_freq = sum(ClaimNb)/n(),
            loss_ratio = sum(ClaimNb)/sum(XGB6fit),
            discount = loss_ratio/target.lr
  )

dat2_byrisk

### 计算折扣和折扣后保费

discount <- dat2_byrisk$discount
testset$risk_group <- 1
for(i in 1:(no.group-1)){
  testset$risk_group[testset[['XGB6fit.all']] > quantile_l[i]]<- i+1
}
testset$discount = 1
for(i in 1:no.group){
  testset$discount[testset$risk_group == i] = discount[i]
}

testset$premium <- testset[['XGB6fit.all']]*testset$discount

