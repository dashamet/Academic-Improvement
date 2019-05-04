library(car)
library(lmtest)
library(nortest)
library(R330)
library(rcompanion)
library(tidyverse)
library(boot)

demo <- demo %>%
  mutate(delta = delta + abs(min(delta)) + 1,
         percent_female = percent_female + 0.0001,
         percent_male = percent_male + 0.0001,
         percent_asian = percent_asian + 0.0001,
         percent_black = percent_black + 0.0001,
         percent_white = percent_white + 0.0001,
         percent_english_language_learners = percent_english_language_learners + 0.0001)

demo_vars = names(subset(demo, select = c(5:14)))

fit_demo_delta <- with(demo, lm(reformulate(demo_vars, "delta")))
summary(fit_demo_delta)

summary(powerTransform(with(fit_demo_delta,cbind(demo$total_enrollment, demo$percent_female, demo$percent_male, demo$percent_asian, demo$percent_black, demo$percent_hispanic, demo$percent_white, demo$percent_students_with_disabilities, demo$percent_english_language_learners, demo$percent_poverty))))
transformTukey(demo$delta)

demo <- demo %>%
  mutate(total_enrollment = total_enrollment^-0.5,
         percent_asian = percent_asian^0.5,
         percent_black = percent_black^0.89,
         percent_hispanic = percent_hispanic^1.31,
         percent_white = percent_white^0.33,
         percent_students_with_disabilities = percent_students_with_disabilities^0.87,
         percent_english_language_learners = percent_english_language_learners^0.22,
         percent_poverty = percent_poverty^2.78)

fit_demo_delta <- with(demo, lm(reformulate(demo_vars, "delta")))
summary(fit_demo_delta)

step_demo_delta_forward <- step(fit_demo_delta, direction = "forward")
step_demo_delta_backward <- step(fit_demo_delta, direction = "backward")
step_demo_delta_both <- step(fit_demo_delta, direction = "both")
allpossregs(delta ~ total_enrollment + percent_female + percent_male + percent_asian + percent_hispanic + percent_white + percent_black + percent_english_language_learners + percent_students_with_disabilities + demo$percent_poverty, data = demo)

summary(step_demo_delta_backward)
summary(step_demo_delta_forward)
summary(step_demo_delta_both)

fit_demo_delta_2 <- lm(delta ~ total_enrollment + percent_female + percent_male + percent_asian + percent_black + percent_hispanic + percent_white + percent_students_with_disabilities + percent_english_language_learners + percent_poverty, data = demo)
fit_demo_delta_3 <- lm(delta ~ percent_female + percent_black + percent_hispanic + percent_students_with_disabilities, data = demo)
fit_demo_delta_4 <- lm(delta ~ percent_female + percent_black + percent_hispanic + percent_students_with_disabilities + percent_english_language_learners, data = demo)
fit_demo_delta_5 <- lm(delta ~ total_enrollment + percent_female + percent_black + percent_hispanic + percent_students_with_disabilities + percent_english_language_learners, data = demo)
fit_demo_delta_2 <- lm(delta ~ percent_female + percent_black + percent_hispanic + percent_english_language_learners, data = demo)

model.glm1 = glm(formula(fit_demo_delta_2), data = demo)
model.glm2 = glm(formula(fit_demo_delta_3), data = demo)
model.glm3 = glm(formula(fit_demo_delta_2), data = demo)
model.glm4 = glm(formula(fit_demo_delta_2), data = demo)
model.glm5 = glm(formula(fit_demo_delta_2), data = demo)
model.glm6 = glm(formula(fit_demo_delta_2), data = demo)

cv.glm(data = demo, glmfit = model.glm1, K = 10)$delta[2]
cv.glm(data = demo, glmfit = model.glm2, K = 10)$delta[2]

vif(fit_demo_delta_2)
resettest(fit_demo_delta_2)
mmps(fit_demo_delta_2)
ad.test(residuals(fit_demo_delta_2))
ncvTest(fit_demo_delta_2)
plot(fit_demo_delta_2)
