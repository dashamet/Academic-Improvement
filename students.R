library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)
library(boot)

students_vars = names(subset(survey, select = c(9:14)))


students <- survey %>%
  mutate(collaborative_teachers_score = as.numeric(collaborative_teachers_score),
         effective_school_leadership_score = as.numeric(effective_school_leadership_score),
         rigorous_instruction_score = as.numeric(rigorous_instruction_score),
         supportive_environment_score = as.numeric(supportive_environment_score),
         strong_family_community_ties_score = as.numeric(strong_family_community_ties_score),
         trust_score = as.numeric(trust_score),
         delta = delta + abs(min(delta)) + 1)

fit_students_delta <- with(students, lm(reformulate(students_vars, "delta")))
summary(fit_students_delta)

summary(powerTransform(with(fit_students_delta,cbind(students$collaborative_teachers_score, students$effective_school_leadership_score, students$rigorous_instruction_score, students$supportive_environment_score, students$strong_family_community_ties_score, students$trust_score))))
transformTukey(students$delta)

students <- students %>%
  mutate(collaborative_teachers_score = collaborative_teachers_score^2,
         effective_school_leadership_score = effective_school_leadership_score^2,
         rigorous_instruction_score = rigorous_instruction_score^1.4,
         trust_score = trust_score^2)

fit_students_delta <- with(students, lm(reformulate(students_vars, "delta")))
summary(fit_students_delta)

step_students_delta_forward <- step(fit_students_delta, direction = "forward")
step_students_delta_backward <- step(fit_students_delta)
step_students_delta_both <- step(fit_students_delta, direction = "both")
allpossregs(delta ~ effective_school_leadership_score + supportive_environment_score + strong_family_community_ties_score + collaborative_teachers_score + rigorous_instruction_score + trust_score, data = students)

summary(step_students_delta_backward)
summary(step_students_delta_forward)
summary(step_students_delta_both)

fit_students_delta_2 <- lm(delta ~ supportive_environment_score + collaborative_teachers_score + rigorous_instruction_score + trust_score, data = students)


model.glm1 = glm(formula(step_students_delta_both), data = students)
model.glm2 = glm(formula(fit_students_delta_2), data = students)
model.glm3 = glm(formula(step_students_delta_forward), data = students)

cv.glm(data = students, glmfit = model.glm1, K = 10)$delta[2]
cv.glm(data = students, glmfit = model.glm2, K = 10)$delta[2]
cv.glm(data = students, glmfit = model.glm3, K = 10)$delta[2]

resettest(step_students_delta_both)
mmps(step_students_delta_both)
ad.test(residuals(step_students_delta_both))
ncvTest(step_students_delta_both)
plot(step_students_delta_both)

