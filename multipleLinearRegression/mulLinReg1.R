library(MASS)
b = Boston
#if we have NA in any rows, fill the NA with the mean
if (nrow(Boston[complete.cases(Boston), ]) != nrow(Boston)){
      for (i in 1:length(Boston)) {
        b[, i] = as.numeric(b[, i])
        b[, i] = ifelse(is.na(b[, i]),
                        ave(
                          b[, i],
                          FUN = function(x)
                            mean(x, na.rm = TRUE)
                        ),
                        b[, i])
      }
}


#train and test split
splitMlm = sample.int(n = nrow(Boston),
                      size = floor(0.75 * nrow(Boston)),
                      replace = F)

train = b[splitMlm,]
test = b[splitMlm,]


fit.mlm = lm(formula = crim ~ ., data = train)
summary(fit.mlm)
par(mfrow=c(2,2))
plot(fit.mlm)
