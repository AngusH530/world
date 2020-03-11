install.packages("rio")
require(rio)
data <- import("D:/final/final.csv")
str(data)
library(forcats)
data$Gender <- factor (data$Gender)
data$Gender <- fct_recode(data$Gender, "male" = "1",
                          "female" = "2")
ggplot(data = data, mapping = aes(x = CCSupport, colour = Gender)) + 
  geom_freqpoly(binwidth = 1)

data$FreqCity <- fct_recode(data$FreqCity, "More than thre days per week"="1",
                            "one or two days per week"="2")
ggplot(data = data, mapping = aes(x = CCSupport, colour = FreqCity)) + 
  geom_freqpoly(binwidth = 1)

data$Age <- factor (data$Age)
data$Age <- fct_recode(data$Age, "16-17 years"="1",
                       "18-30 years"="2", "31-40 years"="3","41-50 years"="4","51-60 years"="5","60+ years"="6")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Age)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Age)) + 
  geom_freqpoly(binwidth = 1)

data$Income <- factor (data$Income)
data$Income <- fct_recode(data$Income, "Nil or negative income"="1",
                          "Less than $20,000"="2", "$20,001 - $50,000"="3","$50,001 - $69,999"="4","$70,000 -$84,999"="5","$85,000 - $99,999"="6","$100,000 -$199,999"="7","$200,000 or more"="8")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Income)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Income)) + 
  geom_freqpoly(binwidth = 1)

data$Ethnicity <- factor (data$Ethnicity)
data$Ethnicity <- fct_recode(data$Ethnicity, "Australian or Pacific Tropical Islander"="1",
                             "North-West European"="2", "Southern or Eastern European"="3","North African or Middle Eastern "="4","South-East Asian"="5","North-East Asian"="6","Southern or Central Asian"="7","North or South American"="8","Sub-Saharan African"="9")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Ethnicity)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Ethnicity)) + 
  geom_freqpoly(binwidth = 1)
data$Education <- factor (data$Education)
data$Education <- fct_recode(data$Education, "Grade 10 or below"="1",
                             "Grade 11"="2", "Grade 12 / School Certificate"="3","Technical qualification/certificate "="4","Undergraduate university degree"="5","Post-graduate university degree"="6")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Education)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Education)) + 
  geom_freqpoly(binwidth = 1)
data$Employment <- factor (data$Employment)
data$Employment <- fct_recode(data$Employment, "Full-time"="1",
                              "Part-time"="2", "Self-employed"="3","Not in the work force"="4","Retired"="5","Student"="6")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Employment)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Employment)) + 
  geom_freqpoly(binwidth = 1)
data$HouseSize <- factor (data$HouseSize)
data$HouseSize <- fct_recode(data$HouseSize, "Single person household"="1",
                             "Couple (no child)"="2", "Couple (with child)"="3","Single parent family"="4","Living at home with parents"="5","Other"="6")
ggplot(data = data, mapping = aes(x = CCSupport, fill = HouseSize)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = HouseSize)) + 
  geom_freqpoly(binwidth = 1)
data$Mode <- factor (data$Mode)
data$Mode <- fct_recode(data$Mode, "Drive"="1",
                        "Carpooling"="2", "Public transport"="3","Riding bike & Walking "="4")
ggplot(data = data, mapping = aes(x = CCSupport, fill = Mode)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = Mode)) + 
  geom_freqpoly(binwidth = 1)
data$ForTransit <- factor (data$ForTransit)
data$ForTransit <- fct_recode(data$ForTransit, "Strongly disagree"="1",
                              "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ForTransit)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ForTransit)) + 
  geom_freqpoly(binwidth = 1)

data$ForEnv <- factor (data$ForEnv)
data$ForEnv <- fct_recode(data$ForEnv, "Strongly disagree"="1",
                          "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ForEnv)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ForEnv)) + 
  geom_freqpoly(binwidth = 1)

data$ReduceCong <- factor (data$ReduceCong)
data$ReduceCong <- fct_recode(data$ReduceCong, "Strongly disagree"="1",
                              "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ReduceCong)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ReduceCong)) + 
  geom_freqpoly(binwidth = 1)
data$ReduceEmission <- factor (data$ReduceEmission)
data$ReduceEmission <- fct_recode(data$ReduceEmission, "Strongly disagree"="1",
                                  "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ReduceEmission)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ReduceEmission)) + 
  geom_freqpoly(binwidth = 1)

data$UnfairPoor <- factor (data$UnfairPoor)
data$UnfairPoor <- fct_recode(data$UnfairPoor, "Strongly disagree"="1",
                              "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = UnfairPoor)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = UnfairPoor)) + 
  geom_freqpoly(binwidth = 1)
data$BadEconomy <- factor (data$BadEconomy)
data$BadEconomy <- fct_recode(data$BadEconomy, "Strongly disagree"="1",
                              "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = BadEconomy)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = BadEconomy)) + 
  geom_freqpoly(binwidth = 1)
data$ExistingTransitSufficient <- factor (data$ExistingTransitSufficient)
data$ExistingTransitSufficient <- fct_recode(data$ExistingTransitSufficient, "Strongly disagree"="1",
                                             "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ExistingTransitSufficient)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ExistingTransitSufficient)) + 
  geom_freqpoly(binwidth = 1)
data$UseTransitMore <- factor (data$UseTransitMore)
data$UseTransitMore <- fct_recode(data$UseTransitMore, "Strongly disagree"="1",
                                  "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = UseTransitMore)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = UseTransitMore)) + 
  geom_freqpoly(binwidth = 1)
data$CarpoolMore <- factor (data$CarpoolMore)
data$CarpoolMore <- fct_recode(data$CarpoolMore, "Strongly disagree"="1",
                               "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = CarpoolMore)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = CarpoolMore)) + 
  geom_freqpoly(binwidth = 1)
data$ToCityLess <- factor (data$ToCityLess)
data$ToCityLess <- fct_recode(data$ToCityLess, "Strongly disagree"="1",
                              "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = ToCityLess)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = ToCityLess)) + 
  geom_freqpoly(binwidth = 1)

data$LessCityWork <- factor (data$LessCityWork)
data$LessCityWork <- fct_recode(data$LessCityWork, "Strongly disagree"="1",
                                "Disagree"="2", "Neutral"="3","Agree"="4","Strongly agree"="5")
ggplot(data = data, mapping = aes(x = CCSupport, fill = LessCityWork)) + 
  geom_histogram(binwidth = 1)
ggplot(data = data, mapping = aes(x = CCSupport, colour = LessCityWork)) + 
  geom_freqpoly(binwidth = 1)
