read.csv("email_phishing_data (1).csv")

EPData <- read.csv("email_phishing_data (1).csv")

is.factor(EPData$label)

EPData$label <- as.factor(EPData$label)

summary(EPData)

#install.packages("skimr")

library(skimr)

skim(EPData)

#install.packages("gtsummary")

library(gtsummary)

tbl_summary(EPData)
