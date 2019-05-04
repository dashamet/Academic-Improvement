library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)
library(MASS)

teachers <- teachers %>%
  mutate(collaborative_teachers_score = as.numeric(collaborative_teachers_score),
         effective_school_leadership_score = as.numeric(effective_school_leadership_score),
         rigorous_instruction_score = as.numeric(rigorous_instruction_score),
         supportive_environment_score = as.numeric(supportive_environment_score),
         strong_family_community_ties_score = as.numeric(strong_family_community_ties_score),
         trust_score = as.numeric(trust_score))
         #effective_school_leadership_score_2 = effective_school_leadership_score^2,
         #collaborative_teachers_score_2 = collaborative_teachers_score^2,
         #rigorous_instruction_score_2 = rigorous_instruction_score^1.4,
         #trust_score_2 = trust_score^2,
         #delta_2 = log(delta))

teachers_vars = names(subset(teachers, select = c(9, 10, 11, 12, 13, 14)))
#teachers_vars_2 = names(subset(teachers, select = c(17, 18, 19, 20, 12)))

fit_teachers_delta <- with(teachers, lm(reformulate(teachers_vars, "delta")))
#fit_teachers_delta_2 <- with(teachers, lm(reformulate(teachers_vars_2, "delta_2")))

summary(powerTransform(with(fit_teachers_delta,cbind(teachers$effective_school_leadership_score, 
                                                      teachers$collaborative_teachers_score, 
                                                      teachers$rigorous_instruction_score, 
                                                      teachers$supportive_environment_score, 
                                                      teachers$trust_score))))

step_teachers_delta_backward <- stepAIC(fit_teachers_delta, direction = "backward")
step_teachers_delta_forward <- stepAIC(fit_teachers_delta, direction = "forward")
step_teachers_delta_both <- stepAIC(fit_teachers_delta, direction = "both")
#allpossregs(delta ~ effective_school_leadership_score + supportive_environment_score + strong_family_community_ties_score + collaborative_teachers_score_2 + rigorous_instruction_score_2 + trust_score_2, 
            #data = teachers) 

summary(step_teachers_delta_backward)
summary(step_teachers_delta_forward)
summary(step_teachers_delta_both)

transformTukey(teachers$delta)

vif(step_teachers_delta_backward)
resettest(step_teachers_delta_backward)
mmps(step_teachers_delta_backward)
ad.test(residuals(step_teachers_delta_backward))
ncvTest(step_teachers_delta_backward) # fails --> need to take log of the y 
plot(step_teachers_delta_backward) 

#model.glm1 = glm(formula(step_parents_delta), data = parents)
#model.glm2 = glm(formula(step_parents_delta_2), data = parents)
#cv.glm(data = parents, glmfit = model.glm1, K = 10)$delta[2]
#cv.glm(data = parents, glmfit = model.glm2, K = 10)$delta[2]
