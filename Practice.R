library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readxl)

anth <- read_excel("anthropometric.xlsx")
data <- anth %>%
  filter(gender != "both", gender != "other", gender !=" other", ideal != "NA", armspan != "NA", height != "NA", foot != "NA")
anthfemale <- data %>%
  filter(gender != "male")
anthmale <- data %>%
  filter(gender != "female")

#Descriptive Statistics

mean(data$ideal)
median(data$ideal)
mode(data$ideal)
sd(data$ideal)

#Inferencial Statistics

#Bivariate

cor.test(data$ideal, data$height, use = pairwise.complete.obs, method = c("pearson", "kendall", "spearman"))

#Multivariate

as.factor(data$gender)

cor(data, method = "pearson")

#Most Correlated

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
 mosthighlycorrelated(data[2:8], 15)

#ANOVA
 
#One-Way Anova (Between Subjects, does gender predict height overall?)
 
fit1 <- aov(height ~ gender, data = data)
summary(fit1)

#Three-Way Anova (Between Subjects, does gender, armspan, and leg length predict height overall?)

fit2 <- aov(height ~ gender * armspan * forearm, data = data)
summary(fit2)

#One-Way Anova (Within Subjects, does gender predict height for each participant?)

subject <- c(1:531)

data2 <- cbind(data, subject)

fit3 <- aov(height ~ as.factor(gender) + Error(subject/as.factor(gender)), data = data2)
summary(fit3)

#Three-Way Anove (Within Subjects, does gender, armspan, and leg length predict height for each participant?)

fit4 <- aov(height ~ )

