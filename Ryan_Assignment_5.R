library(ggplot2)
library(e1071)
library(dplyr)
library(plotly)
library(lsmeans)
library(magrittr)


# The data in this document gives the number of meals eaten that contain fish (per week) and mercury levels in head hair for 100 fisherman.  
# Save the data to a format that can be read into R.  Read the data in for analysis.  
# Use R to calculate the quantities and generate the visual summaries requested below.


# (1) Save the data to a file  (excel or CSV file) and read it into R memory for analysis. (Q1 -  2 points)

setwd("C:/Users/patry/OneDrive/Desktop")
df = read.csv("student_iq_test.csv", header=T, sep=",", fileEncoding="UTF-8-BOM", stringsAsFactors = F)
df

# How many students are in each group?  
# Summarize the data relating to both test score and age by the student group (separately).  
# Use appropriate numerical and/or graphical summaries.  (3 points)




# number of students in each group
physics.student.count <- subset(df, df$group == "Physics student")
nrow(physics.student.count)  # 15

math.student.count <- subset(df, df$group == "Math student")
nrow(math.student.count)  # 15

chem_student_count <- subset(df, df$group == "Chemistry student")
nrow(chem_student_count)  # 15


# Summarization of data
plot.1 <- plot_ly(data = df, x = ~iq, y = ~age, color = ~group)
plot.1


# (2)	
# Do the test scores vary by student group? 
# Perform a one way ANOVA using the aov or Anova function in R to assess.  
# Summarize the results using the 5 step procedure. 
# If the results of the overall model are significant, perform the appropriate pairwise 
# comparisons using Tukey's procedure to adjust for multiple comparisons and summarize these results. 
# (7 points) 


# Analysis of variance model
model <- aov(iq ~ group, data=df)
summary(model)

anova.table <- anova(model)
anova.table

# Analysis of Variance Table
# 
# Response: iq
#
#            Df  Sum Sq Mean Sq F value    Pr(>F)    
# group       2 1171.73  585.87  26.565 3.496e-08 ***
# Residuals  42  926.27   22.05 


# (assume alpha of 0.05)

# (1)
# H0: All underlying population means are equal
# H1: Not all of the underlying population means are equal


# (2) Select the appropriate test statistic
# F = MSB / MSW


# (3)
# State the decision rule:


k <- 3
n <- 45
qf(0.95, df1=  k - 1, df2= (n - k))  # 3.219942


# Decision rule: Reject H0 if f  >= 3.219942
# Otherwise reject H0:



# (4) Compute the test statistic
MSB <- anova.table$`Mean Sq`[1]
MSB

MSW <- anova.table$`Mean Sq`[2]
MSW


f = MSB / MSW
f  #26.56514

#(5) Conclusion

# Reject H0 since 26.56 is >= 3.2199
# Significance in findings confirmed


# Analysis via TukeyHSD
model <- aov(iq ~ group, data=df)
TukeyHSD(model)


# $`group`
#                                         diff        lwr        upr     p adj
# Math student-Chemistry student     -8.666667 -12.832756 -4.5005778 0.0000262
# Physics student-Chemistry student -12.133333 -16.299422 -7.9672445 0.0000000
# Physics student-Math student       -3.466667  -7.632756  0.6994222 0.1194835

model.1 <- model
model.1


# (3)	
# Create an appropriate number of dummy variables for student group and re-run the
# one-way ANOVA using the lm function with the newly created dummy variables.  
# Set chemistry students as the reference group.  Confirm if the results are the same.  
# What is the interpretation of the beta estimates from the regression model?  (4 points)


# Am i supposed to compare a lm with just 1 dummy variable and a seperate lm with dummy test with 3 variables? 

# create appropriate dummy variables
dummy.df <- df
dummy.df$is.chemistry.student <- ifelse(df$group == "Chemistry student", 1, 0)
dummy.df$is.math.student <- ifelse(df$group == "Math student", 1, 0)
dummy.df$is.physics.student <- ifelse(df$group == "Physics student", 1, 0)
dummy.df

model <- lm(dummy.df$iq ~  dummy.df$is.physics.student + dummy.df$is.math.student, data = dummy.df)
summary(model)


model.2 <- model


# Confirm if the results are the same:
anova(model.1)
anova(model.2)


# Proof:
# model.1's residual Sum Sq (926.27) == model.2's Residual Sum Sq (926.27) (same applies for Mean Sq)
# model.1's group Sum Sq (117.73) == model.2's variable Sum Sq (dummy.df$is.physics.student + dummy.df$is.math.student) 
# model.1's F value (26.565) is the average of the two F values in model.2, i.e. (27.587 + 25.543)/2
# model.1's Mean Sq is the average of model.2's variable Mean Sq coefficients (608.40 + 563.33)/2

summary(model.2)

# Visualize B1
x <- list(title = "Is Chemistry Student")
y <- list(title = "IQ Test Score")

plot.2 <- dummy.df %>% 
  plot_ly(x = dummy.df$is.chemistry.student) %>% 
  add_markers(y = dummy.df$iq, name="Observation") %>% 
  add_lines( y = fitted(model), name="Beta.1 Projection") %>%
  layout(xaxis = x, yaxis = y)
plot.2

summary(model)

# Interpretation:  

# The y intercept looks to be the mean of the students who are not chemistry students
# and it appears to slope to meet the mean of students who are chemistry students

# proof:
mean(subset(dummy.df, dummy.df$is.chemistry.student == 0)$iq)
mean(subset(dummy.df, dummy.df$is.chemistry.student == 1)$iq)

# This logically implies that a linear model assumes an increase of 10.4 iq points between chemistry and non chemistry students
# 46.26667 - 35.86667 = 10.4













# (4)	Re-do the one-way ANOVA adjusting for age.   Focus on the output relating to the
# comparisons of test score by student type.  
# Explain how this analysis differs from the analysis in step 2 above (not the results but how does this
# analysis differ in terms of the questions it answers as opposed to the one above).  
# Did you obtain different results?  Summarize briefly (no need to go through the 5 -step procedure here).   
# Present the least square means and interpret these. (6 points)


# plot means using plotly boxplot
plot.3 <- plot_ly(df, y = ~iq, color = ~group, type = "box")
plot.3

model <- aov(iq ~ group + age, data=df)
summary(model)


(Anova(lm(df$iq~df$group+df$age), type=3))


# Summary:
# There is a strong statistical 



# How is this analysis differnt?:
# This analysis accounted for adjustment of the age variable.  
# This time we are evaluationg whetier the means of a dependent categorical variable (IQ Test Score) are equal across levels of a 
# categorical independent variable (student type), while statistically controlling for the effects of a continuious variable (age).

options(contrasts=c("contr.treatment", "contr.poly"))

# Calculate the lsmeans 
lsmeans(lm(df$iq~df$group+df$age), pairwise~df$group)

#Interpretation: the ls means are the group means after having controlled for age."



     