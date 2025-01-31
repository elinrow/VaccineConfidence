library(table1)

table1(~ Gender + Age + Religion + Education | VaxImpChild, data=data, 
       caption = "Characteristics of different responses to the statement 'vaccines are important for children to have'.")

table1(~ Gender + Age + Religion + Education | VaxSaf, data=data, 
       caption = "Characteristics of different responses to the statement 'vaccines are safe'.")

table1(~ Gender + Age + Religion + Education | VaxEff, data=data, 
       caption = "Characteristics of different responses to the statement 'vaccines are effective'.")

table1(~ Gender + Age + Religion + Education | VaxRel, data=data, 
       caption = "Characteristics of different responses to the statement 'vaccines are compatible with my beliefs'.")


table1(~ Gender + Age + Religion + Education | Country, data=data, 
       caption = "Characteristics of data used in logistic regression models")
