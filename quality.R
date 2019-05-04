library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)
library(boot)


qual <- quality_review %>%
  mutate(curriculum = as.numeric(quality_review_how_interesting_and_challenging_is_the_curriculum),
         effective = as.numeric(quality_review_how_effective_is_the_teaching_and_learning),
         assess = as.numeric(quality_review_how_well_does_the_school_assess_what_students_are_learning),
         expect = as.numeric(quality_review_how_clearly_are_high_expectations_communicated_to_students_and_staff),
         teach = as.numeric(quality_review_how_well_do_teachers_work_with_each_other))

qual_vars = names(subset(qual, select = c(34:38)))

fit_qual_delta <- with(qual, lm(reformulate(qual_vars, "delta")))
summary(fit_qual_delta)

summary(powerTransform(with(fit_qual_delta,cbind(qual$curriculum, qual$effective, qual$assess, qual$expect, qual$teach))))
transformTukey(qual$delta)

qual <- qual %>%
  mutate(curriculum = curriculum^0.5,
         assess = assess^0.5,
         expect = expect^-0.5,
         teach = log(teach),
         delta = delta^0.324)

fit_qual_delta <- with(qual, lm(reformulate(qual_vars, "delta")))
summary(fit_qual_delta)

step_qual_delta_forward <- step(fit_qual_delta, direction = "forward")
step_qual_delta_backward <- step(fit_qual_delta, direction = "backward")
step_qual_delta_both <- step(fit_qual_delta, direction = "both")
allpossregs(delta ~ effective_school_leadership_score + supportive_environment_score + strong_family_community_ties_score + collaborative_teachers_score + rigorous_instruction_score + trust_score, data = qual)

summary(step_qual_delta_backward)
summary(step_qual_delta_forward)
summary(step_qual_delta_both)

fit_qual_delta_2 <- lm(delta ~ supportive_environment_score + collaborative_teachers_score + rigorous_instruction_score + trust_score, data = qual)


model.glm1 = glm(formula(step_qual_delta_both), data = qual)
model.glm2 = glm(formula(fit_qual_delta_2), data = qual)

cv.glm(data = qual, glmfit = model.glm1, K = 10)$delta[2]
cv.glm(data = qual, glmfit = model.glm2, K = 10)$delta[2]

vif(fit_qual_delta_2)
resettest(fit_qual_delta_2)
mmps(fit_qual_delta_2)
ad.test(residuals(fit_qual_delta_2))
ncvTest(fit_qual_delta_2)
plot(fit_qual_delta_2)
