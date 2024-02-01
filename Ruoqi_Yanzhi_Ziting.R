# Team Member List
#Ruoqi Zhang
#Yanzhi Meng
#Ziting Yang


#If not installed yet
#install.packages("lfe")
#install.packages("data.table")

#Load packages
install.packages("lfe")
install.packages("data.table")
library("lfe")
library("data.table")


#Set working space
rm(list = ls());
setwd("/Users/xxx/Downloads")
#setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2024 Spring A pricing analytics/Project 1")

#Load data
cardata=fread("cars(3).csv",stringsAsFactors = F)



#----------------------------------------------------------
#----------------------------------------------------------
#SECTION 1: Run regressions with control variables (X) and fixed effects using "felm" function

#  Standard syntax for the felm function is as follows.
#  felm(log(Q) ~ log(P) + X | factor(name of the categorical variables for fixed effects), data source)  


#With no fixed effects, the syntax is identical to the default "lm" function.
#Your colleague's regression
reg=felm(log(qu)~log(eurpr), data=cardata)
summary(reg)

#Questions:
#1. What is the interpretation of the regression result (e.g. intercept and coefficient)?

###Answer
#Intercept (11.2904): This value represents the expected log of the number of new car registrations (qu) 
#when the price in Euros (eurpr) is at the baseline level (1 Euro). As with most intercepts in logarithmic models, 
#it's a theoretical construct, often difficult to interpret in practical scenarios.
#Coefficient for log(eurpr) (-0.2925): This coefficient indicates the price elasticity of demand in the context of new car
#registrations. A 1% increase in the price of new cars (measured in Euros) is associated with a decrease of about 0.2925% 
#in the number of new car registrations. This negative value highlights the inverse relationship between the price of new cars
#and the quantity sold, as measured by new registrations.

#2. β1 estimate from this regression is likely not causal. Explain why.

###Answer
#The β1 estimate from this regression is likely not causal because it's based on observational data, which might contain confounding 
#variables not accounted for in the model. A low R-squared value indicates that many other factors influencing the relationship between 
#price and quantity are not captured in the model. This lack of comprehensive variables in the analysis means that the observed relationship 
#between price and quantity could be spurious or influenced by other external factors, preventing a clear establishment of causality.


#Example of adding controls: run a log-log regression with "year" as 
#fixed effect and "li" as a control variable
reg2=felm(log(qu) ~ log(eurpr)+li | factor(ye), data=cardata)
summary(reg2)

#Questions：
#1. From the data, pick control variables and fixed effects to add to your regression, find your preferred specification and report results (coefficients and standard errors).
###Answer


reg1 = felm(log(qu) ~ log(eurpr)+ ma+org + do + le + he + princ + avppr+ pop:engdp + log(avgurprrival)| factor(ye):factor(co), data = cardata)
summary(reg1)

#2. Justify your specification choice. Why did you choose that set of variables over others?
###Answer
#The variable selection for our regression model was executed through a stepwise refinement process. We began with a comprehensive set of variables to ensure no potential 
#predictor of car demand was overlooked. Each variable's influence on car demand was evaluated for statistical significance within the regression framework. Non-significant 
#variables were pruned iteratively, with the process repeated until only significant variables remained. This not only streamlined the model but also reinforced the reliability of our findings.

#Further, we scrutinized the remaining variables for multicollinearity, particularly the relationship between population (`pop`) and GDP metrics (`engdp` and `ergdp`).
#To mitigate overfitting and capture the distinct effects of population size and economic output, we introduced interaction terms for these variables. The final model
#has a Multiple R-squared of 71.96%.


#We can add multiple FE separately, or interact them. This is how we add two separate fixed effects
#one for each year (same value across all car models) and the other for each car model (same value across all years)
reg3_1=felm(log(qu)~log(eurpr)+li | factor(ye)+factor(co), data=cardata)
summary(reg3_1)

#This is how we add interacting fixed effects for "each year-car model combination" - use ":" instead of "+".
#Note the difference between 3_1 and 3_2 (run both and check what FE is included using "getfe" below). Here
#each car model - year combination gets assigned a unique value of FE.
reg3_2=felm(log(qu)~log(eurpr)+li | factor(ye):factor(co), data=cardata)
summary(reg3_2)


#Note that unlike default "lm", "felm" function does not provide estimates of 
#the intercepts and fixed effects. To get those numbers, we use "getfe" function.

fe=getfe(reg3_2)

#getfe function produces all values for fixed effects as a list.

#Say we want to get the fixed effect value for year 1990. We need to
#find the right location of the list.

#Find where it is located - use "match" function.
idc <- match('90',fe$idx)
fe90=fe[idc,1]
#fe90 is the value of fixed effect for year 1990.

#If you included interactive fixed effects (reg3_2 above), you need
#to specify both the year and market to find the value of the corresponding
#fixed effect.
fe2=getfe(reg3_2)

#Say you want to find the value of FE for year 1990 in market 3.
idc <- match('90.3',fe2$idx)
fe90_3=fe[idc,1]

#Another example use of "getfe" function is available in topic 1 
#R code around line 200.


#For presenting results:
#Unfortunately, some shortcut functionalities to present results from the standard "lm" 
#won't work for the felm - like "predict" function won't work.
#Hence, drawing figures of felm outcome requires the use of generic plot functions. 

#An example:
#Take estimated coefficients from the regression above.
coef2=reg3_2$coefficients

#Take the space of prices to plot against the demand
pricespace=seq(0,50000,100)

#Calculate predicted demand at each price point, evaluated in year 1990 in market 3, for a 
#car with an average value of li.
fitted=(exp(fe90_3+coef2[1]*log(pricespace)+coef2[2]*mean(cardata$li)))

#Plot the line against the raw data.
plot(cardata$eurpr,cardata$qu, pch=1, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,fitted,col="blue",lwd=2)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#SECTION 2: Run an IV regression using "felm" function

#Examples of using an IV: 
# The syntax of felm function with an IV is as follows:

# felm(log(Q)~ X | factor(name of the categorical variable for FE) | (log(P)~ Z), data source) 

# "(log(P)~ Z)" part represents "we instrument log(P) with Z".
# Z is our IV for log(P). Importantly, if you instrument for log(P),
# we need to drop log(P) from the first part of the code.


#Run a log-log regression with "ye" as 
#fixed effect, "li" as a control variable and "we" as an
#IV for log(P). 
reg4=felm(log(qu)~li | factor(ye) | (log(eurpr)~we), data=cardata)
summary(reg4)

#Example of using an IV: if we don't include any X variable in
#an IV regression, simply place "1" in place of X.
reg5=felm(log(qu)~1 | factor(ye) | (log(eurpr)~we), data=cardata)
summary(reg5)

#Example of using an IV: if we don't include fixed effects in 
#an IV regression, place "0" in place of factor variable.
reg6=felm(log(qu)~li | 0 | (log(eurpr)~we), data=cardata)
summary(reg6)

#Example of using an IV: if we need to instrument for more than one
#variables, here's how.
#This is an example where we instrument "log(eurpr)" and "log(avgurprrival)" in
#our regression with instruments "we" and "do" (doesn't mean this is 
#the right combination). Note that if we want to
#instrument for two variables, we need at least two Z variables (generally, the
#number of Z variables needs to be equal to or larger than the number of variables
#that need to be instrumented).
reg7=felm(log(qu)~li | 0 | (log(eurpr)| log(avgurprrival) ~ we + do) ,data=cardata)
summary(reg7)

## Questions:
#1. Building on the specification from the previous section, consider running an IV regression. From the data, use a variable (or variables) that you find appropriate as an IV for the price and report results from that IV regression. How does the result change with and without IV?
###Answer
reg_loc = felm(log(qu) ~ 1 | factor(ye) | (log(eurpr) ~ loc), data = cardata)
summary(reg_loc)
reg_exchangerate = felm(log(qu) ~ 1 | factor(ye) | (log(eurpr) ~ avexr + avdexr), data = cardata)
summary(reg_exchangerate)
reg_gdp = felm(log(qu) ~ 1 | factor(ye) | (log(eurpr) ~ engdp + ergdp), data = cardata)
summary(reg_gdp)
#2. Justify your choice - why do you think it’s a good IV?
###Answer
# we can see that the F-statistics for the instruments are large and the p-values are very small, which indicates strong instruments.    
# would need to argue that these variables are likely to influence the car prices but are not directly related to the quantity sold except through the price, fulfilling the exogeneity condition
# Location (loc): The place where a car is made might impact its price due to different costs of production, but it wouldn't directly change how many cars people buy.
# Exchange Rates (avexr and avdexr): These rates can change how much cars cost in local currency without changing how many cars people want to buy, as long as the reasons for the rate changes are not connected to the car market itself.
# GDP Measures (engdp and ergdp): The overall economic condition represented by GDP could make cars more or less expensive depending on market conditions, but this wouldn't necessarily change how many cars of a specific model are sold, unless those sales are having a direct impact on the GDP.


### 7.Cross-elasticities and competitive effects
## Questions:
#1. Conceptually, what does the coefficient of log(avgeurprrival) represent? (Note that the average rival price is the sum of rival prices divided by the number of rival car models.)

###Answer:The coefficient of log(avgurprrival) in this regression model represents the impact of the logarithm of the average rival price on the dependent variable log(qu).
#Here, when log(avgurprrival) is greater than 0, it indicates that as the average price of competitors' products increases, we anticipate an increase in product sales.

# 2. Conceptually, what is the most plausible range for the coefficient of log(avgeurprrival) (e.g.,
should it be positive or negative, etc.)? Does your estimate fit within that range? If not, is
there anything you can do about it?
# Conceptually, the coefficient of log(avgurprrival) is most likely expected to be positive because as rival prices rise, product sales might also increase due to the attractiveness of the focal product. 
# Here the coefficient of log(avgurprrival) should be positive because coefficient of log(eurpr) is negative. And actually my log(avgurprrival) is positive. If the estimated coefficient is not positive, 
# it suggests a potential issue. It's better to address it by refining your model, considering instrumental variables, or conducting causal inference analysis to validate the relationship.

#3. What does the estimated coefficient tell you about the competitiveness of the market?
#The estimated coefficient of log(avgurprrival) can indicate market competitiveness. A positive and significant coefficient suggests that when rival prices are higher, the focal product tends to have higher sales, 
#indicating competitiveness. Conversely, a negative and significant coefficient implies that the focal product maintains sales even when rivals offer lower prices, reflecting a strong market position.

### 8. Recovering costs
## Questions
# 1. First, use β1 = −0.2925 (your colleague’s estimate) to recover the costs. What do you find? What does the result imply about the validity of demand estimates your colleague had?
### Answer
Cost_colleague = cardata$eurpr * ((1-0.2925) / (-0.2925))
print(Cost_colleague)
# For my colleague's estimate, the resulting costs are negative.
# That's due to (1-0.2925) / (-0.2925) < 0 and cardata$eurpr is always positive, so the costs will be negative
# It implies that the demand estimate is not accurate since costs should logically be lower than the price, and it cannot be negative.

# 2. Next, use β1 you obtained in the previous section to recover the costs. How does your cost estimate improve over your colleague’s? What does the result imply about the validity of your demand estimates?
### Answer
Cost_new = cardata$eurpr * ((1-1.834) / (-1.834))
print(Cost_new)
# For my estimate, β1 = -1.834, the costs are reasonable, which means they are positive and less than the price.  
# It is because 0 < (1-1.834) / (-1.834) < 1 and cardata$eurpr is always positive, so the costs are always be positive and less than their corresponding prices.
# It suggests that my demand estimates are more likely to be valid, as they lead to a plausible calculation of rivals' costs.
