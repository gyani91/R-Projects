# Regression Trees
library(rpart)
data("algae")
algae = algae[-manyNAs(algae),]
rt_a1 = rpart(a1 ~ ., data = algae[, 1:12])
prettyTree(rt_a1)

# final_lm_a1
algae_clean = knnImputation(algae, k=10)
lm_predictions_a1 = predict(final_lm, algae_clean)
rt_predictions_a1 = predict(rt_a1, algae)
# Compute Mean Absolute Error

mae = mean(abs(lm_predictions_a1 - algae[, 'a1']))
msr = mean((lm_predictions_a1 - algae[, 'a1'])^2)

nmsr = mean((lm_predictions_a1 - algae[, 'a1'])^2) / mean((algae[, "a1"])) #incomplete

regr.eval(algae[, "a1"], rt_predictions_a1, train.y = algae[, "a1"])
par_old = par(mfrow = c(1,2))
plot(lm_predictions_a1, algae[, 'a1'], main = "Linear model", xlab = "Predicted", ylab="True Value")
abline(0,1,lty=2)
plot(rt_predictions_a1, algae[, 'a1'], main = "Regression Tree", xlab = "Predicted", ylab="True Value")
abline(0,1,lty=2)

sensible_lm_predictions_a1 = ifelse(lm_predictions_a1 < 0, 0, lm_predictions_a1)
regr.eval(algae[, "a1"], lm_predictions_a1, stats = c("mae", "mse"))

data(algae)
hist(algae$mxPH, probability = T)
boxplot(algae$oPO4, ylab = "oPO4")
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)
bwplot(size ~ a1, data = algae, ylab = "River Size", xlab="Algal A1")
cars <- c(1, 3, 6, 4, 9)
plot(cars, type="o", col="blue")
title(main="Cars", col.main="red", font.main=4)

