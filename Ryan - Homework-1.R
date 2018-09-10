
library(ggplot2)
library(e1071)
setwd("C:/Users/patry/OneDrive/Desktop")


#1)
df = read.csv("hospital_admittance.csv", header=F, sep=",", fileEncoding="UTF-8-BOM", stringsAsFactors = F)
df

#Reassign the list as a vector
df <- as.vector(unlist(df))
df

mean(df)
median(df)
sd(df)
quantile(df)[1]
quantile(df)[3]
min(df)
max(df)


# Histogram
hist(df,  xlab="Patient Stay (days)", col="turquoise", main = ("Histogram of C. Difficile Patients; Bin size = 1"),  breaks=seq(max(df)))

# Outliers
boxplot(df, horizontal = T, col="turquoise")$out
skewness(df)

#4)

# a)
# Distribution of C. Difficle stays less than a week
pnorm(7, mean = 5, sd = 3, lower.tail = T)





# (b) 
# Recent publications have indicated that hypervirulent strains of C. Difficile are on the rise.  
# Such strains are associated with poor outcomes, including extended hospital stays.   
# An investigator is interested in showing that the average hospital stay durations have increased versus published literature.  
# He has a sample of 10 patients from his hospital. 
# If the published data are consistent with the truth, what is the probability that the sample mean in his sample will be greater than 7 days? (3 points)



# How do I incorporate the "size" information into this calculation?

#Could possibly be something like this

sample.pop = 10
sample.sd = 3

pnorm( q=7,  sd = 3, mean=5, lower.tail = F)

sample.var <- sample.sd/sqrt(sample.pop)
sample.var


diff <- (5 - 2) / sample.var  #(3.162278) sd above the mean
percentage <- 100 * pnorm(diff, lower.tail=F)
percentage

# https://www.khanacademy.org/math/ap-statistics/sampling-distribution-ap/sampling-distribution-mean/v/sampling-distribution-example-problem

#


1 - pnorm( 261, mean=266, sd=5, lower.tail=T) - pnorm( 271, mean=266, sd=5, lower.tail=F)


pnorm( 984, mean=1000, sd=300, lower.tail=T) 


median(c(8,4,10,4,14,26))






