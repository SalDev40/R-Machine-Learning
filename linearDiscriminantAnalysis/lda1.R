library(ISLR)
library(MASS)

split = sample.int(
  n = nrow(mtcars),
  size = floor(0.8 * nrow(mtcars)),
  replace = F
)
train = mtcars[split, ]
test = mtcars[-split,]

cylinders  = as.factor(train$cyl)

fitLda = lda(cylinders ~ mpg + hp, data = train)

yLdaPred = predict(fitLda, newdata = test)

table(test$cyl,yLdaPred$class)
