dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[2:5]

#encode categorical variable as a factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
dataset$Gender = ifelse(dataset$Gender == "Male", 1, 0)
dataset$Gender = factor(dataset$Gender, levels = c(0, 1))

#train/test split
sample = sample.int(n = nrow(dataset),
                    size = floor(.75 * nrow(dataset)),
                    replace =  F)
train  = dataset[sample,]
test = dataset[-sample,]

#logistic regression
logReg = glm(Purchased ~ ., data = train, family = 'binomial')
summary(logReg)

#only age and salary
logReg2 = glm(Purchased ~ Age + EstimatedSalary,
              data = train[, 2:4],
              family = 'binomial')
summary(logReg2)

#make prediction
prob = predict(logReg, type = 'response', newdata = test[-3])
yPred = prob > 0.5
cm = table(test$Purchased, yPred)
