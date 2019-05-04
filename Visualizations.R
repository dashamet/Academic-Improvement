library(tidyverse)

plot(density(quality$total_sat), 
     main = "Average Total SAT Scores for NYC Public High Schools",
     xlab = "Total SAT Score",
     ylab = "Density")

plot(density(quality$total_grade_8_score), 
     main = "Average Incoming Grade 8 Test Scores for NYC Public High Schools",
     xlab = "Average Grade 8 Test Score",
     ylab = "Density")

plot(density(quality$delta),
     main = "Average Test Score Improvement for NYC Public Schools",
     xlab = "Difference between average SAT percentile and average Grade 8 percentile",
     ylab = "Density")

