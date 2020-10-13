df = Social_Network_Ads
df$Purchased = factor(df$Purchased, levels = c(0, 1))
sample = sample.int(n = nrow(df),
                    size = floor(0.75 * nrow(x)),
                    replace = F)
train = df[sample, ]
test = df[-sample, ]


train[-3] = scale(train[-3])
test[-3] = scale(test[-3])




library(class)
yPred = knn(
  train =  train[, 1:2],
  test = test[, -3],
  cl = train$Purchased,
  k = 5
)

table(test$Purchased, yPred)
mean(test$Purchased == yPred)
