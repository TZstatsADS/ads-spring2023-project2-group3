#read in data
payroll = read.csv('ads-spring2023-project2-group3-main/data/Citywide_Payroll_Data__Fiscal_Year_.csv')
name_gender = read.csv('ads-spring2023-project2-group3-main/data/NameGender.csv')

#assign the correct columns name
names(name_gender)[names(name_gender) == "name"] <- "cnt"
names(name_gender)[names(name_gender) == "count"] <- "prob"
names(name_gender)[names(name_gender) == "probability"] <- "name"

#filter out names with less than 95% probability of gender and remove duplicates
name_gender = name_gender[name_gender$prob >= 0.95,]
name_gender <- name_gender %>% distinct(name,.keep_all=TRUE)

#filter out payroll data with NaN values for name
payroll = payroll[!is.na(payroll$First.Name),]

#merge dataframes based on names and filter out unnecessary columns 
payroll$name = tolower(payroll$First.Name)
df = left_join(payroll, name_gender, by = 'name')
df = select(df, "Fiscal.Year", "Agency.Name", "Title.Description","Base.Salary", "Pay.Basis", "Regular.Hours","Regular.Gross.Paid","gender")

#split up annual and hourly pay
annum = df[df$Pay.Basis == "per Annum",]
hourly = df[df$Pay.Basis == "per Hour",]

#PRODUCE DATASETS FOR VISUALIZATIONS

#Annum Visualizations
annum_vis = annum %>% group_by(Agency.Name,Fiscal.Year,Title.Description,gender) %>% summarise_at(vars(Base.Salary), list(avg_base = mean))
annum_vis = annum_vis[!is.na(annum_vis$gender),]

gender_counts = annum_vis %>%                         
  group_by(Agency.Name,Fiscal.Year,Title.Description) %>% 
  summarise(n = n())

missing_gender = gender_counts[gender_counts$n == 1,]
missing_gender = select(missing_gender, "Agency.Name", "Fiscal.Year", "Title.Description")

annum_vis = annum_vis %>% anti_join( missing_gender, 
                      by=c("Agency.Name","Fiscal.Year","Title.Description"))

annum_vis = annum_vis %>%
  pivot_wider(names_from = gender,
              values_from = avg_base,
              id_cols = c(Agency.Name,Fiscal.Year,Title.Description)) %>%
  mutate(base_diff = male - female)

annum_vis$percent_diff = (annum_vis$male - annum_vis$female) / annum_vis$male

annum_vis = annum %>% 
  group_by(Agency.Name,Fiscal.Year,Title.Description) %>% 
  summarise(job_count = n()) %>%
  right_join(annum_vis)

annum_vis = annum_vis[annum_vis$job_count >= 10,]
annum_vis
write.csv(annum_vis, "ads-spring2023-project2-group3-main/data/AnnualSalary.csv")

#Hourly Visualizations
hourly_vis = hourly %>% group_by(Agency.Name,Fiscal.Year,Title.Description,gender) %>% summarise_at(vars(Base.Salary), list(avg_base = mean))

hourly_vis = hourly_vis[!is.na(hourly_vis$gender),]

gender_counts = hourly_vis %>%                         
  group_by(Agency.Name,Fiscal.Year,Title.Description) %>% 
  summarise(n = n())

missing_gender = gender_counts[gender_counts$n == 1,]
missing_gender = select(missing_gender, "Agency.Name", "Fiscal.Year", "Title.Description")

hourly_vis = hourly_vis %>% anti_join( missing_gender, 
                                     by=c("Agency.Name","Fiscal.Year","Title.Description"))

hourly_vis = hourly_vis %>%
  pivot_wider(names_from = gender,
              values_from = avg_base,
              id_cols = c(Agency.Name,Fiscal.Year,Title.Description)) %>%
  mutate(base_diff = male - female)

hourly_vis$percent_diff = (hourly_vis$male - hourly_vis$female) / hourly_vis$male

hourly_vis = hourly %>% 
  group_by(Agency.Name,Fiscal.Year,Title.Description) %>% 
  summarise(job_count = n()) %>%
  right_join(hourly_vis)

hourly_vis = hourly_vis[hourly_vis$job_count >= 10,]
hourly_vis
write.csv(hourly_vis, "ads-spring2023-project2-group3-main/data/HourlySalary.csv")