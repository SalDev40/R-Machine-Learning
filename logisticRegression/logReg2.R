data(BreastCancer, package = "mlbench")

bc <- BreastCancer

# turn all factors to numeric and
# taking care of missing data with mean of column
for (i in 1:9) {
  bc[, i] = as.numeric(as.character(bc[, i]))
  bc[, i] = ifelse(is.na(bc[, i]),
                   ave(
                     bc[, i],
                     FUN = function(x) {
                       mean(x, na.rm = TRUE)
                     }
                   ), bc[, i])
}

#encode response variable
bc$Class = ifelse(bc$Class == 'malignant',1,0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

#split into train and test
set.seed(10)
split = sample.int(n = nrow(bc),
                   size = (0.75 * nrow(bc)),
                   replace = F)

train = bc[split, ]
testSet = bc[-split, ]

#logistic regression
bc.glm = glm(formula = Class ~ Cl.thickness + Cell.shape + Cell.size,
             data = train,
             family = 'binomial')

summary(bc.glm)

#make prediction/ confusion matrix
yHat1 = predict.glm(bc.glm, newdata = testSet, type="response")

table(testSet$Class, yHat1 > 0.5)
























































