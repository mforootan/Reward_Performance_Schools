## Performance Task 
Accountability Data Analyst

Division of Strategy and Data

## Background : 
Historically, the Tennessee Department of Education recognized 10 percent of schools in the state as Reward schools. Reward Performance schools are the top 5 percent of schools in terms of achievement as measured by a one-year success rate. Reward Progress schools are the top 5 percent of schools in terms of growth as measured by the Tennessee Value-Added Assessment System (TVAAS).

## Task: 
Using the accompanying data, identify Reward Performance and Reward Progress schools among K-8 schools. The accompanying data only includes schools that are considered in the K-8 pool for accountability purposes.

Begin by creating 2015 one-year, 2014 one-year, and three-year success rates for the All Students group and by student group. Success rates are a percentage obtained using the following calculation: 



You will need to consider the following rules: 

    1. High school End of Course tests taken by students in grades less than 9 are reassigned in the following manner: Algebra I and II are reassigned to Math; English I, II, and III are reassigned to Reading; and Biology I is reassigned to Science.
    2. Only subjects with 30 or more tests are included in a school’s success rate. Test count is verified at the year-school-subject-student group level.
    3. Test records with missing grade are excluded from calculations. Since the “All Grades” records include missing grade, you should not use these records. Instead you should aggregate across grades after you reassign using the criteria outlined in step 1. 

Schools are exempt from reward status for having large student group gaps. Gaps are defined as the difference in success rates between historically underperforming student groups and their relevant comparison student group. We consider gaps between the following student groups:

|Historically Underserved Group|Comparison Group|
|------------------------------|----------------|
|Black/Hispanic/Native American|All Students
|Economically Disadvantaged|Non-Economically Disadvantaged
|Students with Disabilities	|Non-Students with Disabilities
|English Learners|Non-English Language Learners|

A school is exempt from reward status if it meets both of the following conditions for any of its student group gaps:
* Its current year gap has widened (is larger) compared to its prior year gap.
* Its three-year gap is larger than the median gap among schools with sufficient data to calculate a three-year gap.

If a school meets both criteria for any of their student group gaps, it is not eligible for reward status, even if its success rate or TVAAS index would otherwise qualify it as a reward school. Note that gaps are only evaluated if both groups have sufficient valid tests to create a success rate; i.e., if either the Historically Underperforming group or the Comparison group does not have 30 valid tests, the gap size is not evaluated and that particular gap cannot meet the reward exemption.

A note on English Language Learner (ELL) gap comparisons: there is data for English Language Learners and for English Language Learners with T1/T2. T1/T2 students are those who are one/two years removed from receiving ELL services. For accountability purposes, we treat the ELL group as follows: if there are 30 valid tests in the English Language Learners group, use the English Language Learners with T1/T2 group in the gap comparison. The relevant comparison group is always Non-English Language Learners/T1 or T2.

Using one- and three-year success rates, identify schools that are exempt from reward status. Then, identify 5 percent of non-exempt schools with the highest one-year All Students success rate as Reward Performance.

Next, identify 5 percent of non-exempt schools with the largest TVAAS index values as Reward Progress. If there is overlap between Reward Performance and Reward Progress schools, you will need to further identify Reward Progress schools such that there are 5 percent of schools identified uniquely for Reward Progress.

At a minimum, your output should indicate whether each school is Reward Exempt, Reward Performance, and Reward Progress, e,g,:

|system|school|reward_exempt|reward_performance|reward_progress|
|------|------|-------------|------------------|---------------|
|10|5|No|Yes|No

Please submit your code and final output to Alexander Poon within 72 hours of receipt of this task. Please contact Alexander Poon with any questions about the task.
