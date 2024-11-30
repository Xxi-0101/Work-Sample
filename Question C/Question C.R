# Data preparation
load(file = "existing_estimates.Rda")
load(file = "eusurvey.Rda")
load(file = "eupoststrat.Rda")
library(lme4)
library(data.table)
library(boot)

# To check whether there are any missing values
any(is.na(e)) 
any(is.na(est))
any(is.na(post))

# Remove missing values
e <- na.omit(e)


# Estimate an appropriate logistic multilevel model
m1 <- glmer(leave ~ votecon + voteukip + female + age + highed + lowed + c_con15 + c_ukip15 + 
              c_unemployed + c_whitebritish + c_deprived + (1 + female + age | cname), 
            nAGQ = 0, family = binomial(link = "logit"), data = e)

summary(m1)
fixef(m1)
ranef(m1)

# Calculate the change in probability
# votecon
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# voteukip
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# age
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# highed
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# lowed
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# c_whitebritish 
inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*(mean(e$c_whitebritish) + 1) + fixef(m1)[12]*mean(e$c_deprived)) -
  inv.logit(fixef(m1)[1] + fixef(m1)[2] + fixef(m1)[3] + fixef(m1)[4] + fixef(m1)[5] + fixef(m1)[6] + fixef(m1)[7] + fixef(m1)[8]*mean(e$c_con15) + fixef(m1)[9]*mean(e$c_ukip15) + fixef(m1)[10]*mean(e$c_unemployed) + fixef(m1)[11]*mean(e$c_whitebritish) + fixef(m1)[12]*mean(e$c_deprived))

# Analyse random effects
# Create a random effects table
ranef(m1)
random_effects <- ranef(m1)$cname
view(random_effects)


# Produce post-stratified estimates
post$prediction <- predict(m1, newdata = post, type = "response", allow.new.levels = TRUE)
post$weight.pred <- post$prediction * post$percent*100
view(post)

# Add up the values of "weight.pred" for each state
results <- data.table(post)[ , .(final.est = sum(weight.pred)), by = .(cname)]
view(results)

# Estimates of Hanretty
est$estimate <- est$estimate * 100
view(est)

# Mean Absolute Error
MAE <- mean(abs(est$estimate - results$final.est))
print(MAE)

# ICC of the model
m0 <- lmer(leave ~ (1|cname),data = e)
summary(m0)

0.01196 / (0.01196 + 0.23011)
