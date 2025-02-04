data <- readRDS("Data/vcp-global-data.rds")

options(scipen = n)

data$Country <- as.factor(data$Country)

levels(data$Country)[levels(data$Country)%in%("Czech Republic")] <- "Czechia"

suppressWarnings(
data$Region <- countrycode(sourcevar = data[,"Country"],origin = "country.name",destination = "continent")
)
data$Region <- ifelse(data$Country == "Kosovo", "Europe", data$Region)

data <- data %>%
  filter(Region == "Europe")

observation_counts <- data %>% 
  group_by(Year,Country) %>% 
  summarise(Observations = n()) %>%
  group_by(Country) %>%
  summarise(sample = sum(Observations),surveys = n(), years = list(Year))

countries <- observation_counts %>%
  filter(surveys == 5)

data <- data %>%
  filter(Country %in% countries$Country)

# Sample of people who answered 'other' or 'prefer not to say' to the gender question is very small, and option only given in the 2022 survey.

gen_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(gender = list(sort(unique(Gender))), .groups = 'drop') %>%
  mutate(count = sapply(gender, length))

age_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(age = list(sort(unique(Age))), .groups = 'drop') %>%
  mutate(count = sapply(age, length))

inc_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(income = list(sort(unique(Income))), .groups = 'drop') %>%
  mutate(count = sapply(income, length))

inc_group_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(income_g = list(sort(unique(Income_grouped))), .groups = 'drop') %>%
  mutate(count = sapply(income_g, length))

edu_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(education = list(sort(unique(Education))), .groups = 'drop') %>%
  mutate(count = sapply(education, length))

rel_cats <- data %>%
  group_by(Country, Year) %>%
  summarise(religion = list(sort(unique(Religion))), .groups = 'drop') %>%
  mutate(count = sapply(religion, length))

# Exclude 2022

# Exclude income (collinearity)

data <- data %>%
  filter(Year != 2022) %>%
  select(-c(Income, Income_grouped))
####

data <- data %>%
  mutate_at(vars(Gender,Age, Education, Religion), as.factor)

###

data <- data %>% 
  filter(!Gender %in% c("Other", "Prefer not to say", "   Prefer not to say", "   Other"))

####

levels(data$Education)

levels(data$Education)[levels(data$Education) %in% c("Completed High level education (University)", "University")] <- "Undergraduate Degree"
levels(data$Education)[levels(data$Education) %in% c("Completed Higher level of education (Masters, PHD, etc.)", "Masters/PhD", "Postgraduate degree (PhD or higher)", "Complete higher education (bachelor's, master's or specialist's degree)")] <- "Postgraduate Degree"
levels(data$Education)[levels(data$Education) %in% c("Completed primary", "Primary")] <- "Primary School"
levels(data$Education)[levels(data$Education) %in% c("Completed secondary school", "Secondary", "Secondary/Vocational", "Complete secondary school (10, currently 11 years)", "Secondary school", "Secondary vocational education")] <- "Secondary School"
levels(data$Education)[levels(data$Education) %in% c("No education/only basic education", "None", "No qualifications")] <- "No Education"
levels(data$Education)[levels(data$Education) %in% c("Refused/Don't know/no answer", "No Answer", "Refused/DNK/DNA", "Don't know, refused")] <- "No Answer"
levels(data$Education)[levels(data$Education) %in% c("Other education level", "Other")] <- "Other"
levels(data$Education)[levels(data$Education) %in% c("Undergraduate Degree", "Postgraduate Degree", "University or above")] <- "University or Above"
levels(data$Education)[levels(data$Education) %in% c("Primary School", "Primary or below", "No Education", "Primary school or lower")] <- "Primary or below"

levels(data$Religion)

levels(data$Religion)[levels(data$Religion) %in% c("Agnostic/Atheist", "Agnostic/Atheist/no religion", "Atheist/agnostic")] <- "Atheist/Agnostic/No Religion"
levels(data$Religion)[levels(data$Religion) %in% c("Other", "Other religious affiliation")] <- "Other"
levels(data$Religion)[levels(data$Religion) %in% c("No Answer", "No response", "Refused/DNK/DNA", "Refused/Don't know/no answer")] <- "No Answer"
levels(data$Religion)[levels(data$Religion) %in% c("Russian or Eastern Orthodox", "Russian/Eastern-Orthodox")] <- "Russian / Eastern Orthodox"
levels(data$Religion)[levels(data$Religion) %in% c("Christian", "Other Christian", "Protestant", "Roman Catholic", "Russian / Eastern Orthodox")] <- "Christian"

###

data$VaxImpChild <- as.factor(data$VaxImpChild)

levels(data$VaxImpChild)[levels(data$VaxImpChild) == 1] <- "Strongly Agree"
levels(data$VaxImpChild)[levels(data$VaxImpChild) == 2] <- "Somewhat Agree"
levels(data$VaxImpChild)[levels(data$VaxImpChild) == 3] <- "Somewhat Disagree"
levels(data$VaxImpChild)[levels(data$VaxImpChild) == 4] <- "Strongly Disagree"
levels(data$VaxImpChild)[levels(data$VaxImpChild) %in% c(5,9,98,99)] <- "Missing"

data$VaxSaf <- as.factor(data$VaxSaf)

levels(data$VaxSaf)[levels(data$VaxSaf) == 1] <- "Strongly Agree"
levels(data$VaxSaf)[levels(data$VaxSaf) == 2] <- "Somewhat Agree"
levels(data$VaxSaf)[levels(data$VaxSaf) == 3] <- "Somewhat Disagree"
levels(data$VaxSaf)[levels(data$VaxSaf) == 4] <- "Strongly Disagree"
levels(data$VaxSaf)[levels(data$VaxSaf) %in% c(5,9,98,99)] <- "Missing"

data$VaxEff <- as.factor(data$VaxEff)

levels(data$VaxEff)[levels(data$VaxEff) == 1] <- "Strongly Agree"
levels(data$VaxEff)[levels(data$VaxEff) == 2] <- "Somewhat Agree"
levels(data$VaxEff)[levels(data$VaxEff) == 3] <- "Somewhat Disagree"
levels(data$VaxEff)[levels(data$VaxEff) == 4] <- "Strongly Disagree"
levels(data$VaxEff)[levels(data$VaxEff) %in% c(5,9,98,99)] <- "Missing"

data$VaxRel <- as.factor(data$VaxRel)

levels(data$VaxRel)[levels(data$VaxRel) == 1] <- "Strongly Agree"
levels(data$VaxRel)[levels(data$VaxRel) == 2] <- "Somewhat Agree"
levels(data$VaxRel)[levels(data$VaxRel) == 3] <- "Somewhat Disagree"
levels(data$VaxRel)[levels(data$VaxRel) == 4] <- "Strongly Disagree"
levels(data$VaxRel)[levels(data$VaxRel) %in% c(5,9,98,99)] <- "Missing"

data$Year <- as.factor(data$Year)

# Missing values not explained - exclude

data <- data %>% filter(VaxImpChild != "Missing")
data <- data %>% filter(VaxSaf != "Missing")
data <- data %>% filter(VaxEff != "Missing")
data <- data %>% filter(VaxRel != "Missing")

data <- data %>% filter(Religion != "No Answer")
data <- data %>% filter(Education != "No Answer")

data <- subset(data, select = c(Country,Age,Gender,Religion,Education,Year,VaxImpChild, VaxSaf, VaxEff, VaxRel))

summary(data$Country)

data <- data %>%
  filter(Country != "France")

data <- data %>%
  mutate(id = row_number())

data <- droplevels(data)

write.csv(data, "Results/cleanedData.csv")
