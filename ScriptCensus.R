install.packages("plyr")
install.packages("randomForest")
install.packages("ggplot2")

library(plyr)
library(ggplot2) 
library(randomForest)

###           Download data        ###
temp <- tempfile()
download.file("http://thomasdata.s3.amazonaws.com/ds/us_census_full.zip",temp)
learn.data <- read.csv(unz(temp, "us_census_full/census_income_learn.csv"))
test.data <- read.csv(unz(temp, "us_census_full/census_income_test.csv"))
unlink(temp)

names(learn.data) <- c("age", "classofworker", "industrycode", "occupationcode", "education", "wageperhour", "enrolled.school", 
                       "marital.status", "major.industry", "major.occupation", "race", "hispanic", "sex", "union", "reason.unemployment", 
                       "workstat", "capgain", "caploss", "divstocks", "taxfilerstatus", "state.previous", "region.previous", "householdandfamily", 
                       "household", "instance.weight", "MIGMTR1", "MIGMTR3", "MIGMTR4", "livethishouse", "sunbeltmigr", "nbunder18", "parent.present", "birthfather", 
                       "birthmother", "birthself", "citizenship", "ownbusiness", "questionnaireveteran", "veteransbenefits", "weeksworked", "year", "income")

names(test.data) <- c("age", "classofworker", "industrycode", "occupationcode", "education", "wageperhour", "enrolled.school", 
                      "marital.status", "major.industry", "major.occupation", "race", "hispanic", "sex", "union", "reason.unemployment", 
                      "workstat", "capgain", "caploss", "divstocks", "taxfilerstatus", "state.previous", "region.previous", "householdandfamily", 
                      "household", "instance.weight", "MIGMTR1", "MIGMTR3", "MIGMTR4", "livethishouse", "sunbeltmigr", "nbunder18", "parent.present", "birthfather", 
                      "birthmother", "birthself", "citizenship", "ownbusiness", "questionnaireveteran", "veteransbenefits", "weeksworked", "year", "income")

###           Cleaning the data        ###

# We'll join both data sets for less data-wrangling lines
data <- rbind(learn.data, test.data)

# First I check for missing data -> there is none 
sum(is.na(data))

# Let's have a first view at what we have 
summary(learn.data)
# There are more that 93,8% with an income under 50k, our model should do even better than this,
# because we could have a hypothesis saying that everybody has an income less than 50k, and we'd be right in 93,8% of cases


# instance.weight will be used to weight our model, we can also notice that we two years of census. 94 and 95. 
# The situation for some people might have changed between these two. 
(sum(test.data$instance.weight)+sum(learn.data$instance.weight))/2
# Dividing by two the sum of weights of both data sets gives us the average population between 94 and 95
# The issue here is that the census changed between 94 and 95 giving us less variables. 
# We will keep both, and use only the data available for both years. 

# Let's look at variables present in different years of the census.
year94 <- filter(learn.data, learn.data$year==94)
year95 <- filter(learn.data, learn.data$year==95)

# Let's detect which variable are present or not in different census years
summary(year94)
summary(year95)

# In year 95, some variables are not present. Since we'll need to predict for both years in our test, we'll delete them. 
data$MIGMTR1 <- NULL
data$MIGMTR3 <- NULL
data$MIGMTR4 <- NULL
data$sunbeltmigr <- NULL
data$region.previous <- NULL
data$state.previous <- NULL 
data$livethishouse <- NULL


# Let's put all the columns which are supposed to be interpreted as factors but are instead interpreted like ints

data$industrycode = as.factor(data$industrycode)
data$ocuupationcode = as.factor(data$ocuupationcode)
data$ownbusiness = as.factor(data$ownbusiness)
data$veteransbenefits = as.factor(data$veteransbenefits)
data$year = as.factor(data$year)


# We can see that industry code and occupation code are redundant with major industry and major occupation, which might give biased the classification.
# Thus, I delete both industry code and occupation code
data$industrycode <- NULL 
data$occupationcode <- NULL 

# Questionnairevetetran and union also seem to relevant since a very small part of the population answers to the question
data$questionnaireveteran <- NULL
data$union <- NULL

# The year of the census also isn't relevant to our analysis. I'll erase that column
data$year <- NULL



###           Data analysis and wrangling        ###



# Now that we have cleared out all obviously irrelevant data for our analysis, we will try to get a sense about what is happining with visualisation

# Age 
ggplot() + aes(learn.data$age, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(binwidth=1, position = "stack")
# We notice that the count of people of 90 years old is really high. It might mean that 90 represents people that are older than 90 years old. It won't spoil our anlysis. 
# Otherwise the data shows us what we expected: people start to get money from approximately 20 years old. 
ggsave(file="age.png")


# Sex
ggplot() + aes(learn.data$sex, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(binwidth=1, position = "stack")
# Females are more represented as it is the fact in the population though the difference seems  high. But our instances are weighted to it is difficult to count. 
# This was expected: there's an income gap between males and females
ggsave(file="sex.png")

# Weeks worked in year
ggplot() + aes(x =learn.data$weeksworked, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(position = "stack")
ggsave(file="weeksworked.png")
# Beautiful, the more you work, the more you earn. 
ggplot() + aes(x =learn.data$capgain, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(position = "stack")
ggsave(file="capgain.png")
ggplot() + aes(x =learn.data$caploss, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(position = "stack")
ggsave(file="caploss.png")
ggplot() + aes(x =learn.data$divstocks, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(position = "stack")
ggsave(file="divstocks.png")
ggplot() + aes(x =learn.data$wageperhour, fill= learn.data$income, weight = learn.data$instance.weight)+ geom_bar(position = "stack")
ggsave(file="wageperhour.png")

# They have very narrow distributions, and will not be considered for our regression analysis. With $0 wage per hour, we can notice that a large part earns more than 50k, the same goes for no capital gains. 
data$capgain <- NULL
data$caploss <- NULL
data$divstocks <- NULL
data$wageperhour <- NULL

# Class of worker
ggplot() + aes(data$classofworker, fill= data$income, weight = data$instance.weight)+ geom_bar(binwidth=1, position = "stack")+ coord_flip()
ggsave(file="classofworker.png")
# We need to reduce the number of factors in order to have a convergent regression. We can notice that without pay or never worked are very similar. 
data$classofworker <- as.character(data$classofworker)

data$classofworker[data$classofworker==" Local government"] = "Governement"
data$classofworker[data$classofworker==" State government"] = "Governement"
data$classofworker[data$classofworker==" Self-employed-incorporated"] = "Self-Employed"
data$classofworker[data$classofworker==" Self-employed-not incorporated"] = "Self-Employed"
data$classofworker[data$classofworker==" Without pay"] = "Not-Working"
data$classofworker[data$classofworker==" Never worked"] = "Not-Working"

data$classofworker <- as.factor(data$classofworker)
summary(data$classofworker)

#Education
ggplot() + aes(learn.data$education, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack")+ coord_flip()
ggsave(file="education.png")
# We have too many factors, but this variable is very important for our prediction
data$education <- as.character(data$education)
# As we can see for income, the grade in which the pupil is won't change our outcome. 
data$education[data$education == " 5th or 6th grade"] = "No Degree"
data$education[data$education == " 7th and 8th grade"] = "No Degree"
data$education[data$education == " 1st 2nd 3rd or 4th grade"] = "No Degree"
data$education[data$education == " 10th grade"] = "No Degree"
data$education[data$education == " 11th grade"] = "No Degree"
data$education[data$education == " 12th grade no diploma"] = "No Degree"
data$education[data$education == " 9th grade"] = "No Degree"
data$education[data$education == " 9th grade"] = "No Degree"
data$education[data$education == " Children"] = "No Degree"
data$education[data$education == " Less than 1st grade"] = "No Degree"
data$education[data$education == " Associates degree-academic program"] = "Associate"
data$education[data$education == " Associates degree-occup /vocational"] = "Associate"
data$education[data$education == " Doctorate degree(PhD EdD)"] = "Doctorate"
data$education[data$education == " Prof school degree (MD DDS DVM LLB JD)"] = "Doctorate"
data$education[data$education == " Some college but no degree"] = " High school graduate"


data$education <- as.factor(data$education)
summary(data$education)

# Major industry. This variable might be very useful for our classifacation but cannot really be summarized
ggplot() + aes(learn.data$major.industry, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="majorindustry.png")

# Major occupation.
ggplot() + aes(learn.data$major.occupation, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="majoroccupation.png")

# Marital status 
ggplot() + aes(learn.data$marital.status, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill") + coord_flip()
ggsave(file="maritalstatus.png")
data$marital.status <- as.character(data$marital.status)
data$marital.status[data$marital.status==" Divorced"] = " Separated"
data$marital.status[data$marital.status==" Married-spouse absent"] = " Separated"

data$marital.status <- as.factor(data$marital.status)
summary(data$marital.status)

# Work status
ggplot() + aes(learn.data$workstat, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="workstatus.png")

# Number of children in the household 
ggplot() + aes(learn.data$nbunder18, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack")
ggsave(file="nbunder18.png")

# Parent present
ggplot() + aes(learn.data$parent.present, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="parentpresent.png")
# This data shows that when the question is answered, the person has no income. It must be because the person is a child. 
# So this data must be highly correlated with age, or school enrollement We won't use it. 
data$parent.present <- NULL

# Race
ggplot() + aes(learn.data$race, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="race.png")

# Citizenship
ggplot() + aes(x =learn.data$citizenship, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="citizenship.png")

# Owns business
ggplot() + aes(x =learn.data$ownbusiness, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack") + coord_flip()
ggsave(file="ownsbusiness.png")

# Birth of the person and of parents. 
ggplot() + aes(x =learn.data$birthself, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="birthself.png")

ggplot() + aes(x =learn.data$birthmother, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="birthmother.png")

ggplot() + aes(x =learn.data$birthfather, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="birthfather.png")

# We can notice that some origins, primary European, are highly correlated with an income above 50k

ggplot() + aes(x =learn.data$household, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="household.png")

ggplot() + aes(x =learn.data$householdandfamily, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="householdandfamily.png")
# Household and Family seems just to be a more precise variable than household. 
# For this reason, I choose to delete Household and Family 
data$householdandfamily <- NULL

# Tax filer status
ggplot() + aes(x =learn.data$taxfilerstatus, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack")+ coord_flip()
ggsave(file="taxfilerstatus.png")
# This one show good distribution, and good insights for rour classification

# Reason for unemployment
ggplot() + aes(x =learn.data$reason.unemployment, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill") + coord_flip()
ggsave(file="reasonunemployement.png")

# Hispanic origin
ggplot() + aes(x =learn.data$hispanic, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "stack")+ coord_flip()
ggsave(file="hispanic.png")
# Hispanic origin has quite a narrow distribution and doesn't help us a lot to understand the data
data$hispanic <- NULL

# Work statistics
ggplot() + aes(x =learn.data$workstat, fill= learn.data$income, weight = learn.data$weight)+ geom_bar(binwidth=1, position = "fill")+ coord_flip()
ggsave(file="workstatistics.png")
# This variable will be relevant for it has a good distribution and good insights



###               Classification            ###

learn <- slice(data , 1 : 199522)
test <- slice(data, 199523 : 299283)

### Logistic regression

glm.fit = glm(income ~ age
              + weeksworked
              + education
              + classofworker
              + enrolled.school
              + marital.status
              + race
              + reason.unemployment
              + nbunder18
              + ownbusiness
              + weeksworked
              + sex
              + major.industry 
              + major.occupation
              + workstat
              + birthself
              + citizenship
              + household
              + taxfilerstatus
              + birthfather
              + birthmother, 
              data = learn, family = binomial)


# At first I wanted to weight our data. But the algorithm doesn't converge in this case. Even with three only three simple numerical variables used in our prediction. 

# Let's have a look at the coefficients of our logistic regression. 
glm.summary <- summary(glm.fit)$coef
# And order it we increasing p values 
sorter <- order(glm.summary[,4])
glm.summary <- glm.summary[sorter,]
glm.summary
### insights ###
# Here we can see that the most important predictors are sex, age, major occupation, education, weeksworked and number of children under 18 in the household
# In the estimate, we can see the influence of different variables. 
# Being a male, old, have a managerial role, working a lot of weeks in the year and having a high degree all play for a higher income

# Let's sort it through z value, which gives us the impact 
sorter <- order(glm.summary[,3])
glm.summary <- glm.summary[sorter,]
glm.summary
# We notice that being a non taxfiler, no married and working for state level governement are all bad for your income

# Let's now look at the performances of our model with the learn data 
glm.probs <- predict(glm.fit, type="response")
glm.pred <- rep("Moins",length(learn$income))
glm.pred[glm.probs >.5]="Plus"
confusion <- table(glm.pred,learn$income)
confusion
accuracy <- sum(diag(confusion)) / length(learn$income)

accuracy
# We have a 94,84% accuracy, which is better than the accuracy in the case we are saying that everybody earns less than 50k (93,8%)
# We miss some of high earners but get right some of them. The result doesn't satisfy me, so I could have played longer with the data. 

# Let's apply our algorithm to the test data 
glm.probs<-predict.glm(glm.fit, newdata = test, type="response")
glm.pred<-rep("Moins",length(test$income))
glm.pred[glm.probs >.5]="Plus"
confusion <- table(glm.pred,test.data$income)
confusion
accuracy <- sum(diag(confusion)) / length(test$income)

accuracy
# We have an accuracy of 94,88%. Which is a little strange: our model fits better to the test data set than the one our alogorithm learnt from


###           Random forest

rf.fit <- randomForest(income ~ age
                       + weeksworked
                       + education
                       + classofworker
                       + enrolled.school
                       + marital.status
                       + race
                       + reason.unemployment
                       + nbunder18
                       + ownbusiness
                       + weeksworked
                       + sex
                       + major.industry 
                       + major.occupation
                       + workstat
                       + birthself
                       + citizenship
                       + household
                       + taxfilerstatus
                       + birthfather
                       + birthmother, 
                       data = learn)

print(rf.fit)

# We get an error rate of 6,32%, which is worse than the 6,2% when we say that everybody earns less. 
# For this reason we'll choose the logistic regression

# The most challenging parts for me was to understand what the data meant and to choose which variables to use.
# Another problem was to adapt my predictors in order to have a convergent logistic regression. 
# I couldn't use the weight of each instance because it makes the logistic regression diverge, even with few predictors. 
# I could have summarized more some variables manipulating strings in order to have a faster prediction and more solid results. 