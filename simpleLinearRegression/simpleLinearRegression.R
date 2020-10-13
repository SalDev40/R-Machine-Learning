dataset = read.csv('Salary_Data.csv')

#split into train/test
set.seed(101)
sample <- sample.int(n = nrow(dataset),
                     size = floor(0.66 * nrow(dataset)),
                     replace = F)

trainingSet <- dataset[sample,]
testSet <- dataset[-sample,]

# create linear model
regressor <-
  lm(formula = Salary ~ YearsExperience  , data = trainingSet)
summary(regressor)
par(mfrow = c(2,2))
plot(regressor)


# make prediction
yPred = predict(regressor, newdata = testSet)

# Visualize Training set
library(ggplot2)
ggplot() +
  geom_point(aes(x = trainingSet$YearsExperience, y = trainingSet$Salary),
             colour = 'red') +
  geom_line(aes(
    x = trainingSet$YearsExperience,
    y = predict(regressor, newdata = trainingSet)
  ),
  colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualizing the Test set
ggplot() +
  geom_point(aes(x = testSet$YearsExperience, y = testSet$Salary),
             colour = 'red') +
  geom_line(aes(
    x = trainingSet$YearsExperience,
    y = predict(regressor, newdata = trainingSet)
  ),
  colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

