# Load required libraries
library(ggplot2)

# Create vectors for quality scores with and without visual aids
no_visual_aids <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)
with_visual_aids <- c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65,70)

# Create a data frame
lecture_data <- data.frame(
  Student = 1:17,
  No_Visual_Aids = no_visual_aids,
  With_Visual_Aids = with_visual_aids
)

# Box plot
boxplot(lecture_data[,2:3], col=c("blue","red"), names=c("No Visual Aids", "With Visual Aids"), main="Quality Scores with and without Visual Aids")

# Shapiro-Wilk test for normality
shapiro_test_without <- shapiro.test(lecture_data$No_Visual_Aids)
shapiro_test_with <- shapiro.test(lecture_data$With_Visual_Aids)

# Print Shapiro-Wilk test results
print(shapiro_test_without)
print(shapiro_test_with)

# Calculate mean, standard deviation, and confidence interval for each condition
mean_no <- mean(lecture_data$No_Visual_Aids)
mean_with <- mean(lecture_data$With_Visual_Aids)
sd_no <- sd(lecture_data$No_Visual_Aids)
sd_with <- sd(lecture_data$With_Visual_Aids)
ci_no <- t.test(lecture_data$No_Visual_Aids)$conf.int
ci_with <- t.test(lecture_data$With_Visual_Aids)$conf.int

# Print descriptive statistics
cat("Descriptive Statistics for Quality Scores Without Visual Aids:\n")
cat("Mean:", mean_no, "\n")
cat("Standard Deviation:", sd_no, "\n")
cat("95% Confidence Interval:", ci_no, "\n\n")

cat("Descriptive Statistics for Quality Scores With Visual Aids:\n")
cat("Mean:", mean_with, "\n")
cat("Standard Deviation:", sd_with, "\n")
cat("95% Confidence Interval:", ci_with, "\n\n")

# Test of Difference Between Groups
t_test <- t.test(lecture_data$No_Visual_Aids, lecture_data$With_Visual_Aids, paired=TRUE)

# Print t-test results
print(t_test)
