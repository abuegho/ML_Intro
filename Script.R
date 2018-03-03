library(boot)
library(tidyverse)


## k-fold
fold_cv = NULL

for(i in 1:5){
  model1 = glm(Male~poly(Alcohol + `Contributed To Accident` + `Personal Injury` + Belts, i), data = seg)
  fold_cv[i] = cv.glm(seg, model1, K = 5)$delta[1]
}
fold_cv

train_control = trainControl(method = "cv", number = 10)

model = train(Male~Alcohol + `Contributed To Accident` + `Personal Injury` + Belts, 
              data = seg2, metric = "Kappa", trControl = train_control, method = "spls")

pred = predict(model, seg2)

seg2 = cbind(seg2, pred)

confusionMatrix(seg2$pred, seg2$Male)

KM = kmeans(x = seg, centers = 10, iter.max = 1000)

Single2 = cbind(Single, cluster = KM$cluster)

ggplot(Single2, aes(factor(Alcohol), factor(Belts), col = factor(cluster))) +
  geom_jitter(alpha = .3, size = 4)
 

ggplot(Single2, aes(Race, factor(cluster), col = factor(cluster))) + geom_jitter()