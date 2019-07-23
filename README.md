# Predictors of Academic Improvement

* This project contains the code used to produce linear models for a paper evaluating various predictors of students’ academic improvement in high school: stakeholder assessments (by students, parents, and teachers), expert evaluations, and school demographics.

* We obtained our data from the New York City Department of Education, in particular their [stakeholder assessment](https://infohub.nyced.org/reports-and-policies/school-quality/nyc-school-survey/survey-archives) and [quality review](https://infohub.nyced.org/reports-and-policies/school-quality/school-quality-reports-and-resources/school-quality-report-citywide-data) data from the 2016-17 school year. We used this data to create 3 datasets: 

  * `demographics1.xlsx` is the dataset with demographic indicators.
  * `quality2.xlsx` is the dataset with school quality indicators. 
  * `survey2.xlsx` is the dataset with aggregated stakeholder survey responses.

* `MASTER.R` contains the code that loads the 3 datasets we use in our analysis. Run this file before working with any other files in this project. 

* `Visualizations.R` contains the code for three graphs: (1) average total SAT scores, (2) average incoming grade 8 test scores,(3) average test score improvement. 

* `demo.R`, `quality.R`, `parents.R`, `students.R`, and `teachers.R` contain the code for our models for demographics, expert evaluations, parent survey responses, student survey responses, and teacher survey responses respectively. 
