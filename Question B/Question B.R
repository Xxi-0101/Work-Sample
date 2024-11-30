# Data preparation
# Remember to download the required packages before running the code
load(file = "labourvote.Rda")
any(is.na(ess_gb)) # To check whether there are any missing values
library(pROC)
library(mfx)

# 1)
# Split the dataset into training and test data
set.seed(1)
training.rows <- sample(nrow(ess_gb),(nrow(ess_gb)/2))
training.data <- ess_gb[training.rows,]
test.data <- ess_gb[-training.rows,]

# Run three logit models containing different sets of variables
# Model 1
mod1 <- glm(labvote ~ gndr + yrbrn + eduyrs + lrscale + hinctnta, family = binomial(link = "logit"), data = training.data)
summary(mod1)

# Model 2
mod2 <- glm(labvote ~  yrbrn + eduyrs + lrscale + polintr + hinctnta, family = binomial(link = "logit"), data = training.data)
summary(mod2)

# Model 3
mod3 <- glm(labvote ~ gndr+ eduyrs + lrscale + polintr + hinctnta, family = binomial(link = "logit"), data = training.data)
summary(mod3)

# Produce predicted probabilities for the test data for 3 models
logit.probs.test1 <- predict(mod1,test.data, type = "response")
logit.probs.test2 <- predict(mod2,test.data, type = "response")
logit.probs.test3 <- predict(mod3,test.data, type = "response")

# Assign 1 to all observations with a predicted probability greater than 0.5 and vice versa
logit.preds.test1 <- ifelse(logit.probs.test1 > 0.5,1,0)
logit.preds.test2 <- ifelse(logit.probs.test2 > 0.5,1,0)
logit.preds.test3 <- ifelse(logit.probs.test3 > 0.5,1,0)

# Confusion matrix
# Compare the performance of three models
# Calculate the total error rate, sensitivity and specificity of the three model
x <- table(logit.preds.test1,test.data$labvote)
(x[1,2] + x[2,1]) / nrow(test.data) # Error rate
x[2,2] / (x[1,2] + x[2,2]) # Sensitivity
x[1,1] / (x[1,1] + x[2,1]) # Specificity

y <- table(logit.preds.test2,test.data$labvote)
(y[1,2] + y[2,1]) / nrow(test.data) # Error rate
y[2,2] / (y[1,2] + y[2,2]) # Sensitivity
y[1,1] / (y[1,1] + y[2,1]) # Specificity

z <- table(logit.preds.test3,test.data$labvote)
(z[1,2] + z[2,1]) / nrow(test.data) # Error rate
z[2,2] / (z[1,2] + z[2,2]) # Sensitivity
z[1,1] / (z[1,1] + z[2,1]) # Specificity

# Plot the ROC curve for three models
rocplot1 <- roc(test.data$labvote,logit.probs.test1)
rocplot1$auc # area under the curve
plot(rocplot1,legacy.axes = T)

rocplot2 <- roc(test.data$labvote,logit.probs.test2)
rocplot2$auc # area under the curve
plot(rocplot2,legacy.axes = T)

rocplot3 <- roc(test.data$labvote,logit.probs.test3)
rocplot3$auc # area under the curve
plot(rocplot3,legacy.axes = T)

# Plot in one graph
# Create a blank plot
plot(rocplot1, type = "n", legacy.axes = TRUE, main = "ROC Curves")

# Plot each ROC curve
plot(rocplot1, col = "red", add = TRUE) # mod1
plot(rocplot2, col = "blue", add = TRUE) # mod2
plot(rocplot3, col = "#228B22", add = TRUE) # mod3

# Add legend
legend("bottomright", legend = c("Model 1", "Model 2", "Model 3"), col = c("red", "blue", "#228B22"), lty = 1)

# Add text for the AUC values
text(0.3, 0.3, paste("AUC (Model 1):", round(auc(rocplot1), 3)), col = "red")
text(0.3, 0.25, paste("AUC (Model 2):", round(auc(rocplot2), 3)), col = "blue")
text(0.3, 0.2, paste("AUC (Model 3):", round(auc(rocplot3), 3)), col = "#228B22")

# 2)
# Interpret the model
summary(mod1)
# Average marginal effects 
logitmfx(mod1, data = ess_gb)

