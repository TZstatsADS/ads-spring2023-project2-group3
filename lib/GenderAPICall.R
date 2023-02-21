#use genderizeR package to create dataset with gender of all first names in payroll data
df = read.csv('ads-spring2023-project2-group3-main/data/Citywide_Payroll_Data__Fiscal_Year_.csv')
x = c(unique(df$First.Name))
key = 'private'
givenNames = findGivenNames(x, apikey = key)
write.csv(givenNames, "ads-spring2023-project2-group3-main/data/NameGender.csv")

