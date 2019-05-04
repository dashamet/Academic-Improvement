library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)


parents <- parents %>%
  mutate(collaborative_teachers_score_2 = as.numeric(collaborative_teachers_score)^2,
         effective_school_leadership_score_2 = as.numeric(effective_school_leadership_score)^2,
         rigorous_instruction_score_2 = as.numeric(rigorous_instruction_score)^1.4,
         supportive_environment_score = as.numeric(supportive_environment_score),
         strong_family_community_ties_score = as.numeric(strong_family_community_ties_score),
         trust_score_2 = as.numeric(trust_score)^2,
         log_delta = log(delta),
         delta2 = delta ^ 0.4)

parents_vars = subset(parents, select = c(12,13,17,18,19,20))
parents_vars_2 = subset(parents, select = c(12,13,17,18,19))
parents_vars_2 = names(parents_vars_2)
parents_vars = names(parents_vars)

fit_parents_delta <- with(parents, lm(reformulate(parents_vars, "delta2")))
fit_parents_delta_2 <- with(parents, lm(reformulate(parents_vars_2, "delta2")))
summary(fit_parents_delta)

step_parents_delta <- stepAIC(fit_parents_delta)
step_parents_delta_2 <- stepAIC(fit_parents_delta_2)
step_parents_delta_back <- stepAIC(fit_parents_delta, direction = "backward")
step_parents_delta_both <- stepAIC(fit_parents_delta, direction = "both")

model.glm1 = glm(formula(step_parents_delta), data = parents)
model.glm2 = glm(formula(step_parents_delta_2), data = parents)

cv.glm(data = parents, glmfit = model.glm1, K = 10)$delta[2]
cv.glm(data = parents, glmfit = model.glm2, K = 10)$delta[2]

summary(step_parents_delta)

summary(powerTransform(with(step_parents_delta,cbind(parents$effective_school_leadership_score_2, parents$collaborative_teachers_score_2, parents$rigorous_instruction_score_2, parents$supportive_environment_score, parents$trust_score_2))))
transformTukey(parents$delta2)

vif(step_parents_delta)
resettest(step_parents_delta)
mmps(step_parents_delta)
ad.test(residuals(step_parents_delta))
allpossregs(delta ~ effective_school_leadership_score + supportive_environment_score + strong_family_community_ties_score + collaborative_teachers_score_2 + rigorous_instruction_score_2 + trust_score_2, data = parents)
ncvTest(step_parents_delta)
plot(step_parents_delta)
