# STA304-Final-Project

---
title: "Predict Vote Outcome of 2019 Canadian Federal Election"
author: "YUXUAN JU"
date: "12/8/2020"
output: pdf_document
---

# Abstract
In this report, a prediction model will be built based on the 2019 Canadian election study which is an online study, and utilize this model to predict the vote outcome of 2019 Canadian Federal Election. Then compare it with the real vote result and find out the next step on improvement of the model.

# Keywords
Vote Outcome, 2019 Election, Canada

# Introduction
Statistical analysis is widely used on predictions. Find out the significance level of association or causation between multiple explanatory variables and the response variable, then build up a model. This allow people has precognition which also let decision makers be able to adjust for more optimized outcome before processing. 

The whole process for federal election usually takes couple of weeks, and there is always voter survey during the election. Building a prediction model and finding out what citizens attach importance to can gain more vote via placing adjustment on campaign speech include improvement on policies. Since vote for the candidates or not is a dummy question, building a logistic model will have lucid result. 

After building the model for 2019 Canadian federal election based on 2019 voter survey data set[1], subtract census data[2] into it and get the predicted result. Compare it with the actual vote outcome, figure out the next improvement step can be taken into the model.


```{r setup, include=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```
# Methodology
##Data
```{r}
library(haven)
library(dplyr)
library(tidyverse)
# Loading in the cleaned census Data
census_data <- read_csv("/Users/pro/Desktop/ipumsi_00001.csv")

# Loading in the cleaned survey Data
survey_data <- read_dta("/Users/pro/Desktop/CES.dta")

```

```{r}
survey_data <- labelled::to_factor(survey_data)
sur_data <- 
  survey_data %>%
  mutate( sex = ifelse(cps19_gender == "A man", "male", "female"))

sur_data$AGE <- 2019 - (as.integer(sur_data$cps19_yob) + 1919)


sur_data$EDATTAIN1 <- as.numeric( factor(sur_data$cps19_education) )

sur_data <- 
  sur_data %>%
  mutate(sur_data, EDATTAIN = ifelse( EDATTAIN1 %in% 1:3, 0,
                                     ifelse(EDATTAIN1 %in% 4:5, 1,
                                            ifelse(EDATTAIN1 %in% 6:8, 2,
                                                   ifelse(EDATTAIN1 %in% 9:10, 3,
                                                           ifelse(EDATTAIN1 %in% 11, 3, 0))))))

sur_data$INCTOT <- as.numeric(sur_data$cps19_income_number)


sur_data<-
  sur_data %>%
  mutate(vote_liberal = 
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))

sur_data<-
  sur_data %>%
  mutate(vote_conservative = 
           ifelse(cps19_votechoice=="Conservative Party", 1, 0))


```

```{r}
census_data <- 
  census_data %>%
  mutate( sex = ifelse(SEX == "1", "male", "female"))

census_data <- 
  census_data %>%
  count(AGE,sex,EDATTAIN,INCTOT) %>%
  group_by(AGE,sex,EDATTAIN,INCTOT) 
```

## Table 1
### For census data
```{r}
summary(census_data)
```
The census data is get from the IPUMS INTERNATIONAL data set[2] and extract 286542 observations with four basic information including age, sex, education level and income which used in building the prediction model from survey data, and use the wider population to do prediction on the 2019 election. 

### For survey data
```{r}
su_data <- data.frame(sur_data$sex,sur_data$AGE,sur_data$EDATTAIN,sur_data$INCTOT,sur_data$vote_liberal,sur_data$vote_conservative)
summary(su_data)
```
The survey data is obtained from 2019 Canadian federal election based on 2019 voter survey data set[1] which contains 37822 responds, and in order to build the prediction model, variables including age, sex, education level, income and their preference to vote are chosen.

# MODEL

To predict the 2019 Canadian federal election voting outcomes, logistic regression models are used since the dependent variable is binary. Logistic regression predicts a categorical variable based on a set of independent variables and shows the impact of each variable on the odds ratio of the observed data. When analyzing the association of all variables, it prevents confounding effects. 

One of the model is to explore the proportion of voters who will vote for Liberal Party while the other one is for Conservative Party. Since the survey shows that some people are still not sure who they will vote for,we need to build two models to see the supporting rate of each elector. Age, sex, education level, incomeare independent variables since they are relatively the key factor impacting the voting outcomes. The logistic regression model we are using is:

$$ log(p/(1-p)) = \beta_0+\beta_1  x_{age} + \beta_2  x_{sex} + \beta_3  x_{education} + \beta_4  x_{income} +  \epsilon$$
Where $log(p/(1-p))$ represents the proportion of voters who will vote for Liberal Party/Conservative Party. Similarly, $\beta_0$ is the "fixed component" of that component-wise method to describe log odds of 2019 Canadian federal election voting outcomes. Additionally, $\beta_1$ to $\beta_4$, it represents the extend of increase in the log odds of voting for Liberal Party/Conservative Party for every one unit increase in the corresponding continuous independent variable which are age and income, otherwise, for categorical independent variables, it represents the performance of each category is evaluated with respect to a base category. 


## Model for Liberal Party
```{r}
# Creating the Model
model_liberal <- glm(vote_liberal ~ sex + AGE + EDATTAIN + INCTOT, 
            data=sur_data, family= "binomial")

summary(model_liberal)
```

## Model for Conservative Party
```{r}
model_conservative<- glm(vote_conservative ~ sex + AGE + EDATTAIN + INCTOT, 
            data=sur_data, family= "binomial")

summary(model_conservative)
```

## Post-Stratification 
When doing survey analysis, post-stratification is a common technique to incorporate population distributions of variables into survey estimates. It first divides samples into cells and uses demographics to predict how the entire population will vote based on the estimate for each cell. Use the two models we built previously to estimate the proportion of voters in each cell, then weight these proportion estimate via corresponding population size and divide the sum of these value by entire population size.By survey sampling, post-stratification effectively makes an accurate estimate of the population and correct non-sampling errors.

In order to estimate the proportion of voters who will vote for Liberal Party/Conservative Party, we need to perform a post-stratification analysis. Here we create cells based on different ages, genders, education levels and income.

```{r}
# Perform the post-stratification calculation
census_data$logodds_estimate <-
  model_liberal %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarize(alp_predict = sum(alp_predict_prop)/sum(n))

```


```{r}
census_data$logodds_estimate <-
  model_conservative %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarize(alp_predict = sum(alp_predict_prop)/sum(n))

```


# Results

The result shows that the proportion of voters voting for Liberal(Justin Trudeau) $$\hat{y}^{PSl}$$ is 25.3991% and the proportion of voters voting for Conservative(Andrew Scheer) $$\hat{y}^{PSc}$$is 23.3637% based on post-stratification analysis concerning the proportion of voters voting for either liberal or conservative. This is modelled by the logistic regression model with independent variables sex,age,education level and income.

# Discussion

## Summary

The logistic linear regression model was built from a reduced dataset containing the four independent variables that choose to predict the vote result, where it has 37822 observations and 620 variables in the original raw data set. For preparing the census data set which contains a higher volume of observations -- 286542 observations, divide them into different cells based on the explanatory variables that will be fitted into the previous model. Since the citizens who chose to vote for other candidates or abstain from voters are accounted as blackball when estimating the approval rate for both liberal and conservative. Use a separate model to estimate the approval rates and compare the difference between them, and find out the predicted winner in the 2019 Canadian Federal Election.

## Conclusion
Since post-stratification obtains an accurate estimate of the entire population, Predict that Liberal party will win the election based on estimation of the proportion of voters voting for Liberal being 25.3991% and proportion of voters voting for Conservative being 23.3637%. The rate for Liberal Party is 2.0354% higher than the approval rate of Conservative Party.

## Weaknesses

In analysis, the number of independent variables is not enough. With more variables being used to make models and set post-stratification, the result would be more accurate. Besides, The census data set is still not large enough which bring bias and in accurate to the result. And after checking the real result in 2019 Canadian federal election is Liberal(Justin Trudeau):39.47%, Conservative(Andrew Scheer):31.89%, The difference: 7.58% which indicate the inaccurate of this model.


## Next Steps

To improve future estimation model, keep the diversity of data by improving cleaning techniques to match corresponding subsets in each explanatory variable between two data sets are needed. However, simply clean and classify the response first in the process of collecting data by the survey is another suitable choice. Also use various plots to visualize models which are able to show the advantages and disadvantages of the model as well as the data. Moreover, filter part of the NAs from the original survey data set which can improve the accuracy of the model.

# Reference
[1]survey data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V
                "2019 Canadian Election Study - Online Survey" 
                by Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John(2020-04-20)
[2]census data: https://international.ipums.org/international-action/data_requests/download
                by IPUMS INTERNATIONAL



