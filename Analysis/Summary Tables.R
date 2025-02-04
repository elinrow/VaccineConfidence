table_html <- table1(~ Gender + Age + Religion + Education + VaxImpChild + VaxSaf + VaxEff + VaxRel | Country, 
                     data = data, caption = "Country-level characteristics")

