library(dplyr)
Amazon <- read.csv("Amazon.csv")
str(Amazon)
head(Amazon)

Amazon_Clean <- Amazon %>% 
  mutate(
    OrderDate = as.Date(OrderDate),
    Category = as.factor(Category),
    Brand         = as.factor(Brand),
    PaymentMethod = as.factor(PaymentMethod),
    OrderStatus   = as.factor(OrderStatus),
    Country       = as.factor(Country),
    State         = as.factor(State),
    City          = as.factor(City),
    Year          = lubridate::year(OrderDate),
    Month         = lubridate::month(OrderDate, label = TRUE, abbr = TRUE)
  )

colSums(is.na(Amazon_Clean))

saveRDS(Amazon_Clean, file = "Amazon_clean.rds")
