library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)
library(boot)


demo <- demoity_review %>%
  mutate(curriculum = as.numeric(demoity_review_how_interesting_and_challenging_is_the_curriculum),
         effective = as.numeric(demoity_review_how_effective_is_the_teaching_and_learning),
         assess = as.numeric(demoity_review_how_well_does_the_school_assess_what_students_are_learning),
         expect = as.numeric(demoity_review_how_clearly_are_high_expectations_communicated_to_students_and_staff),
         teach = as.numeric(demoity_review_how_well_do_teachers_work_with_each_other))

demo_vars = names(subset(demo, select = c(5:14)))

fit_demo_delta <- with(demo, lm(reformulate(demo_vars, "delta")))
summary(fit_demo_delta)

summary(powerTransform(with(fit_demo_delta,cbind(demo$total_enrollment, demo$percent_female, demo$percent_male, demo$percent_asian, demo$percent_black, demo$percent_hispanic, demo$percent_white, demo$percent_students_with_disabilities, demo$percent_english_language_learners, demo$percent_poverty))))
transformTukey(demo$delta)

demo <- demo %>%
  mutate(curriculum = curriculum^0.5,
         assess = assess^0.5,
         expect = expect^-0.5,
         teach = log(teach),
         delta = delta^0.35)

fit_demo_delta <- with(demo, lm(reformulate(demo_vars, "delta")))
summary(fit_demo_delta)

step_demo_delta_forward <- step(fit_demo_delta, direction = "forward")
step_demo_delta_backward <- step(fit_demo_delta, direction = "backward")
step_demo_delta_both <- step(fit_demo_delta, direction = "both")
allpossregs(delta ~ curriculum + effective + assess + expect + teach, data = demo)

summary(step_demo_delta_backward)
summary(step_demo_delta_forward)
summary(step_demo_delta_both)

fit_demo_delta_2 <- lm(delta ~ factor(curriculum) + factor(effective) + factor(expect) + factor(teach), data = demo)


model.glm1 = glm(formula(step_demo_delta_both), data = demo)
model.glm2 = glm(formula(fit_demo_delta_2), data = demo)

cv.glm(data = demo, glmfit = model.glm1, K = 10)$delta[2]
cv.glm(data = demo, glmfit = model.glm2, K = 10)$delta[2]

vif(fit_demo_delta_2)
resettest(fit_demo_delta_2)
mmps(fit_demo_delta_2)
ad.test(residuals(fit_demo_delta_2))
ncvTest(fit_demo_delta_2)
plot(fit_demo_delta_2)
