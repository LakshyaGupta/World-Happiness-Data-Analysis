install.packages("rio")
#install.packages("RCurl")
#install.packages("bitops")

library(rio)
library(RCurl)
library(bitops)

#load 2015 data
x <- getURL("https://raw.githubusercontent.com/excelsiordata/DATA606/master/2015.csv")
WHR2015 <- read.csv(text = x, head=TRUE, sep=",", stringsAsFactors=FALSE, col.names = c("Country","Region","Happiness Rank","Happiness Score","Standard Error","Economy (GDP per Capita)","Family","Health (Life Expectancy)","Freedom","Trust (Government Corruption)","Generosity","Dystopia Residual"))

#load 2016 data
x2 <- getURL("https://raw.githubusercontent.com/excelsiordata/DATA606/master/2016.csv")
WHR2016 <- read.csv(text = x2, head=TRUE, sep=",", stringsAsFactors=FALSE, col.names = c("Country","Region","Happiness Rank","Happiness Score","Standard Error","Economy (GDP per Capita)","Family","Health (Life Expectancy)","Freedom","Trust (Government Corruption)","Generosity","Dystopia Residual"))
summary(WHR2015$Happiness.Score)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.839   4.526   5.232   5.376   6.244   7.587
mean(WHR2015$Happiness.Score)
## [1] 5.375734
var(WHR2015$Happiness.Score)
## [1] 1.311048
median(WHR2015$Happiness.Score)
## [1] 5.2325
sd(WHR2015$Happiness.Score)
## [1] 1.14501
plot(WHR2015$Happiness.Score, main = "2015 Happiness Score by Frequency", xlab = "Frequency", ylab = "Happiness Score")


summary(WHR2016$Happiness.Score)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.905   4.404   5.314   5.382   6.269   7.526
mean(WHR2016$Happiness.Score)
## [1] 5.382185
var(WHR2016$Happiness.Score)
## [1] 1.303418
median(WHR2016$Happiness.Score)
## [1] 5.314
sd(WHR2016$Happiness.Score)
## [1] 1.141674
plot(WHR2016$Happiness.Score, main = "2016 Happiness Score by Frequency", xlab = "Frequency", ylab = "Happiness Score")


head(WHR2015)
##       Country         Region Happiness.Rank Happiness.Score Standard.Error
## 1 Switzerland Western Europe              1           7.587        1.39651
## 2     Iceland Western Europe              2           7.561        1.30232
## 3     Denmark Western Europe              3           7.527        1.32548
## 4      Norway Western Europe              4           7.522        1.45900
## 5      Canada  North America              5           7.427        1.32629
## 6     Finland Western Europe              6           7.406        1.29025
##   Economy..GDP.per.Capita.  Family Health..Life.Expectancy. Freedom
## 1                  1.34951 0.94143                  0.66557 0.41978
## 2                  1.40223 0.94784                  0.62877 0.14145
## 3                  1.36058 0.87464                  0.64938 0.48357
## 4                  1.33095 0.88521                  0.66973 0.36503
## 5                  1.32261 0.90563                  0.63297 0.32957
## 6                  1.31826 0.88911                  0.64169 0.41372
##   Trust..Government.Corruption. Generosity Dystopia.Residual
## 1                       0.29678    2.51738                NA
## 2                       0.43630    2.70201                NA
## 3                       0.34139    2.49204                NA
## 4                       0.34699    2.46531                NA
## 5                       0.45811    2.45176                NA
## 6                       0.23351    2.61955                NA
head(WHR2016)
##       Country         Region Happiness.Rank Happiness.Score Standard.Error
## 1     Denmark Western Europe              1           7.526        1.44178
## 2 Switzerland Western Europe              2           7.509        1.52733
## 3     Iceland Western Europe              3           7.501        1.42666
## 4      Norway Western Europe              4           7.498        1.57744
## 5     Finland Western Europe              5           7.413        1.40598
## 6      Canada  North America              6           7.404        1.44015
##   Economy..GDP.per.Capita.  Family Health..Life.Expectancy. Freedom
## 1                  1.16374 0.79504                  0.57941 0.44453
## 2                  1.14524 0.86303                  0.58557 0.41203
## 3                  1.18326 0.86733                  0.56624 0.14975
## 4                  1.12690 0.79579                  0.59609 0.35776
## 5                  1.13464 0.81091                  0.57104 0.41004
## 6                  1.09610 0.82760                  0.57370 0.31329
##   Trust..Government.Corruption. Generosity Dystopia.Residual
## 1                       0.36171    2.73939                NA
## 2                       0.28083    2.69463                NA
## 3                       0.47678    2.83137                NA
## 4                       0.37895    2.66465                NA
## 5                       0.25492    2.82596                NA
## 6                       0.44834    2.70485                NA
table(WHR2016$Region)
## 
##       Australia and New Zealand      Central and Eastern Europe 
##                               2                              29 
##                    Eastern Asia     Latin America and Caribbean 
##                               6                              24 
## Middle East and Northern Africa                   North America 
##                              19                               2 
##               Southeastern Asia                   Southern Asia 
##                               9                               7 
##              Sub-Saharan Africa                  Western Europe 
##                              38                              21
hist(WHR2015$Happiness.Score, main = "Histogram of Happiness Score Worldwide in 2015", xlab = "Happiness Score")


hist(WHR2016$Happiness.Score, main = "Histogram of Happiness Score Worldwide in 2016", xlab = "Happiness Score")


boxplot(WHR2015$Happiness.Score ~ WHR2015$Region, xlab = "Region", ylab = "Happiness Score", main = "Boxplot of Happiness Score by Region for 2015")


boxplot(WHR2016$Happiness.Score ~ WHR2016$Region, xlab = "Region", ylab = "Happiness Score", main = "Boxplot of Happiness Score by Region for 2016")


#I noticed here that there was a huge shift in Sub-Saharan Africa between 2015 and 2016
#This is what made me choose to dig into it below

#Dig into the Sub-Saharan Africa data
SSA2015 <- subset(WHR2015, Region == "Sub-Saharan Africa")
SSA2016 <- subset(WHR2016, Region == "Sub-Saharan Africa")

summary(SSA2015)
##    Country             Region          Happiness.Rank  Happiness.Score
##  Length:40          Length:40          Min.   : 71.0   Min.   :2.839  
##  Class :character   Class :character   1st Qu.:115.8   1st Qu.:3.756  
##  Mode  :character   Mode  :character   Median :132.0   Median :4.272  
##                                        Mean   :127.9   Mean   :4.203  
##                                        3rd Qu.:146.2   3rd Qu.:4.581  
##                                        Max.   :158.0   Max.   :5.477  
##  Standard.Error   Economy..GDP.per.Capita.     Family      
##  Min.   :0.0000   Min.   :0.0000           Min.   :0.0000  
##  1st Qu.:0.2039   1st Qu.:0.6767           1st Qu.:0.1651  
##  Median :0.3084   Median :0.8784           Median :0.2982  
##  Mean   :0.3805   Mean   :0.8091           Mean   :0.2823  
##  3rd Qu.:0.4831   3rd Qu.:1.0016           3rd Qu.:0.3721  
##  Max.   :1.0602   Max.   :1.1847           Max.   :0.7095  
##  Health..Life.Expectancy.    Freedom        Trust..Government.Corruption.
##  Min.   :0.1008           Min.   :0.03060   Min.   :0.06822              
##  1st Qu.:0.3013           1st Qu.:0.07231   1st Qu.:0.18260              
##  Median :0.3829           Median :0.10387   Median :0.20730              
##  Mean   :0.3659           Mean   :0.12388   Mean   :0.22114              
##  3rd Qu.:0.4620           3rd Qu.:0.13289   3rd Qu.:0.24334              
##  Max.   :0.5920           Max.   :0.55191   Max.   :0.50318              
##    Generosity     Dystopia.Residual
##  Min.   :0.6704   Mode:logical     
##  1st Qu.:1.6693   NA's:40          
##  Median :1.9501                    
##  Mean   :2.0200                    
##  3rd Qu.:2.4583                    
##  Max.   :3.0514
summary(SSA2016)
##    Country             Region          Happiness.Rank  Happiness.Score
##  Length:38          Length:38          Min.   : 66.0   Min.   :2.905  
##  Class :character   Class :character   1st Qu.:117.5   1st Qu.:3.745  
##  Mode  :character   Mode  :character   Median :133.5   Median :4.130  
##                                        Mean   :129.7   Mean   :4.136  
##                                        3rd Qu.:144.8   3rd Qu.:4.433  
##                                        Max.   :157.0   Max.   :5.648  
##  Standard.Error   Economy..GDP.per.Capita.     Family      
##  Min.   :0.0000   Min.   :0.0000           Min.   :0.0000  
##  1st Qu.:0.2800   1st Qu.:0.4819           1st Qu.:0.1605  
##  Median :0.3945   Median :0.6312           Median :0.2419  
##  Mean   :0.4743   Mean   :0.5937           Mean   :0.2399  
##  3rd Qu.:0.6265   3rd Qu.:0.7615           3rd Qu.:0.3144  
##  Max.   :1.1585   Max.   :0.9605           Max.   :0.6619  
##  Health..Life.Expectancy.    Freedom        Trust..Government.Corruption.
##  Min.   :0.0000           Min.   :0.03050   Min.   :0.06244              
##  1st Qu.:0.2551           1st Qu.:0.06682   1st Qu.:0.18328              
##  Median :0.3402           Median :0.09586   Median :0.21621              
##  Mean   :0.3154           Mean   :0.12038   Mean   :0.22635              
##  3rd Qu.:0.4132           3rd Qu.:0.12895   3rd Qu.:0.25789              
##  Max.   :0.5678           Max.   :0.50521   Max.   :0.51479              
##    Generosity     Dystopia.Residual
##  Min.   :0.9674   Mode:logical     
##  1st Qu.:1.9384   NA's:38          
##  Median :2.1231                    
##  Mean   :2.1664                    
##  3rd Qu.:2.4415                    
##  Max.   :3.8377
#2015

#all of these pieces are within the summary, but I wanted to pull them out
mean(SSA2015$Happiness.Score)
## [1] 4.2028
range(SSA2015$Happiness.Score)
## [1] 2.839 5.477
range(SSA2015$Happiness.Rank)
## [1]  71 158
hist(SSA2015$Happiness.Score, main = "Histogram of Happiness Score in Sub-Saharan Africa in 2015", xlab = "Happiness Score", breaks = 40)


boxplot(SSA2015$Happiness.Score ~ SSA2015$Country, main = "Boxplot of Happiness Score in Sub-Saharan Africa in 2015", xlab = "Country", ylab = "Happiness Score")


plot(SSA2015$Happiness.Score ~ SSA2015$Trust..Government.Corruption., main = "Scatterplot of Happiness Score and Trust in 2015 Sub-Saharan Africa", xlab = "Trust", ylab = "Happiness Score")


plot(SSA2015$Happiness.Score ~ SSA2015$Family, main = "Scatterplot of Happiness Score and Family in 2015 Sub-Saharan Africa", xlab = "Family", ylab = "Happiness Score")


plot(SSA2015$Happiness.Score ~ SSA2015$Health..Life.Expectancy., main = "Scatterplot of Happiness Score and Health in 2015 Sub-Saharan Africa", xlab = "Health", ylab = "Happiness Score")


#These relationships look linear - proceeding with quantifying the
#strength of the correlation coefficient

plot(SSA2015$Happiness.Score ~ SSA2015$Economy..GDP.per.Capita., main = "Scatterplot of Happiness Score and Economy in 2015 Sub-Saharan Africa", xlab = "Economy", ylab = "Happiness Score")


plot(SSA2015$Happiness.Score ~ SSA2015$Generosity, main = "Scatterplot of Happiness Score and Generosity in 2015 Sub-Saharan Africa", xlab = "Generosity", ylab = "Happiness Score")


cor(SSA2015$Happiness.Score, SSA2015$Economy..GDP.per.Capita.)
## [1] 0.5769741
cor(SSA2015$Happiness.Score, SSA2015$Generosity)
## [1] 0.5829218
SSA2015e <- lm(Happiness.Score ~ Economy..GDP.per.Capita., data = SSA2015)
summary(SSA2015e)
## 
## Call:
## lm(formula = Happiness.Score ~ Economy..GDP.per.Capita., data = SSA2015)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.78143 -0.44288 -0.04344  0.40806  1.04291 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                3.1403     0.2567  12.234 9.49e-15 ***
## Economy..GDP.per.Capita.   1.3132     0.3016   4.355 9.73e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5044 on 38 degrees of freedom
## Multiple R-squared:  0.3329, Adjusted R-squared:  0.3153 
## F-statistic: 18.96 on 1 and 38 DF,  p-value: 9.728e-05
plot(SSA2015$Happiness.Score ~ SSA2015$Economy..GDP.per.Capita., xlab = "Economy (GDP per Capita)", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2015 Sub-Saharan Africa")
abline(SSA2015e)


SSA2015g <- lm(Happiness.Score ~ Generosity, data = SSA2015)
summary(SSA2015g)
## 
## Call:
## lm(formula = Happiness.Score ~ Generosity, data = SSA2015)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.17777 -0.23711  0.05247  0.23073  1.44017 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.9060     0.3038   9.566 1.15e-11 ***
## Generosity    0.6420     0.1452   4.422 7.91e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5018 on 38 degrees of freedom
## Multiple R-squared:  0.3398, Adjusted R-squared:  0.3224 
## F-statistic: 19.56 on 1 and 38 DF,  p-value: 7.913e-05
plot(SSA2015$Happiness.Score ~ SSA2015$Generosity, xlab = "Generosity", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2015 Sub-Saharan Africa")
abline(SSA2015g)


SSA2015le <- lm(Happiness.Score ~ Health..Life.Expectancy., data = SSA2015)
summary(SSA2015le)
## 
## Call:
## lm(formula = Happiness.Score ~ Health..Life.Expectancy., data = SSA2015)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.36308 -0.41911  0.02015  0.42122  1.17458 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                4.0156     0.3137  12.801 2.35e-15 ***
## Health..Life.Expectancy.   0.5116     0.8151   0.628    0.534    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6143 on 38 degrees of freedom
## Multiple R-squared:  0.01026,    Adjusted R-squared:  -0.01578 
## F-statistic: 0.394 on 1 and 38 DF,  p-value: 0.534
plot(SSA2015$Happiness.Score ~ SSA2015$Health..Life.Expectancy., xlab = "Health (Life Expectancy)", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2015 Sub-Saharan Africa")
abline(SSA2015le)


#Just double checking to make sure these are not any more significant than they look
#cor(SSA2015$Happiness.Score ~ SSA2015$Trust..Government.Corruption.)
#cor(SSA2015$Happiness.Score ~ SSA2015$Family)
#As I suspected, the correlation coefficient is really low
#Therefore, not moving forward in analyzing these variables

#2016

#all of these pieces are within the summary, but I wanted to pull them out
mean(SSA2016$Happiness.Score)
## [1] 4.136421
range(SSA2016$Happiness.Score)
## [1] 2.905 5.648
range(SSA2016$Happiness.Rank)
## [1]  66 157
hist(SSA2016$Happiness.Score, main = "Histogram of Happiness Score in Sub-Saharan Africa in 2016", xlab = "Happiness Score", breaks = 40)


boxplot(SSA2016$Happiness.Score ~ SSA2016$Country, main = "Boxplot of Happiness Score in Sub-Saharan Africa in 2016", xlab = "Country", ylab = "Happiness Score")


plot(SSA2016$Happiness.Score ~ SSA2016$Trust..Government.Corruption., main = "Scatterplot of Happiness Score and Trust in 2016 Sub-Saharan Africa", xlab = "Trust", ylab = "Happiness Score")


plot(SSA2016$Happiness.Score ~ SSA2016$Family, main = "Scatterplot of Happiness Score and Family in 2016 Sub-Saharan Africa", xlab = "Family", ylab = "Happiness Score")


plot(SSA2016$Happiness.Score ~ SSA2016$Health..Life.Expectancy., main = "Scatterplot of Happiness Score and Health in 2016 Sub-Saharan Africa", xlab = "Health", ylab = "Happiness Score")


#These relationships look linear - proceeding with quantifying the
#strength of the correlation coefficient

cor(SSA2016$Happiness.Score, SSA2016$Economy..GDP.per.Capita.)
## [1] 0.3276954
cor(SSA2016$Happiness.Score, SSA2016$Generosity)
## [1] 0.493634
SSA2016e <- lm(Happiness.Score ~ Economy..GDP.per.Capita., data = SSA2016)
summary(SSA2016e)
## 
## Call:
## lm(formula = Happiness.Score ~ Economy..GDP.per.Capita., data = SSA2016)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.9548 -0.3356 -0.1209  0.3183  1.5019 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                3.6793     0.2362  15.578   <2e-16 ***
## Economy..GDP.per.Capita.   0.7700     0.3700   2.081   0.0446 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5349 on 36 degrees of freedom
## Multiple R-squared:  0.1074, Adjusted R-squared:  0.08259 
## F-statistic: 4.331 on 1 and 36 DF,  p-value: 0.0446
plot(SSA2016$Happiness.Score ~ SSA2016$Economy..GDP.per.Capita., xlab = "Economy (GDP per Capita)", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2016 Sub-Saharan Africa")
abline(SSA2016e)


SSA2016g <- lm(Happiness.Score ~ Generosity, data = SSA2016)
summary(SSA2016g)
## 
## Call:
## lm(formula = Happiness.Score ~ Generosity, data = SSA2016)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.1995 -0.2465 -0.0217  0.3110  1.4932 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.0280     0.3351   9.035 8.69e-11 ***
## Generosity    0.5117     0.1502   3.406  0.00164 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4924 on 36 degrees of freedom
## Multiple R-squared:  0.2437, Adjusted R-squared:  0.2227 
## F-statistic:  11.6 on 1 and 36 DF,  p-value: 0.001636
plot(SSA2016$Happiness.Score ~ SSA2016$Generosity, xlab = "Generosity", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2016 Sub-Saharan Africa")
abline(SSA2016g)


#Just double checking to make sure these are not any more significant than they look
#cor(SSA2016$Happiness.Score ~ SSA2016$Trust..Government.Corruption.)
#cor(SSA2016$Happiness.Score ~ SSA2016$Family)
#cor(SSA2016$Happiness.Score ~ SSA2016$Health..Life.Expectancy.)
#Life Expectancy is showing a much stronger linear relationship in 2016. Let's take a look.

SSA2016le <- lm(Happiness.Score ~ Health..Life.Expectancy., data = SSA2016)
summary(SSA2016le)
## 
## Call:
## lm(formula = Happiness.Score ~ Health..Life.Expectancy., data = SSA2016)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.97992 -0.37802  0.01915  0.27196  1.28174 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                3.6400     0.2079  17.508   <2e-16 ***
## Health..Life.Expectancy.   1.5739     0.6026   2.612   0.0131 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5191 on 36 degrees of freedom
## Multiple R-squared:  0.1593, Adjusted R-squared:  0.1359 
## F-statistic: 6.821 on 1 and 36 DF,  p-value: 0.01305
plot(SSA2016$Happiness.Score ~ SSA2016$Health..Life.Expectancy., xlab = "Health (Life Expectancy)", ylab = "Happiness Score", main = "Scatterplot with a LSR Line for 2016 Sub-Saharan Africa")
abline(SSA2016le)


summary(SSA2015$Health..Life.Expectancy.)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1008  0.3013  0.3829  0.3659  0.4620  0.5920
summary(SSA2016$Health..Life.Expectancy.)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.2551  0.3402  0.3154  0.4132  0.5678
hist(SSA2015$Health..Life.Expectancy., breaks = 20, main = "Histogram of Health in 2015 Sub-Saharan Africa", xlab = "Health (Life Expectancy)")


hist(SSA2016$Health..Life.Expectancy., breaks = 20, main = "Histogram of Health in 2016 Sub-Saharan Africa", xlab = "Health (Life Expectancy)")


#Quick peek at eastern and central europe
ECE2015 <- subset(WHR2015, Region == "Central and Eastern Europe")
ECE2016 <- subset(WHR2016, Region == "Central and Eastern Europe")

summary(ECE2015)
##    Country             Region          Happiness.Rank Happiness.Score
##  Length:29          Length:29          Min.   : 31    Min.   :4.218  
##  Class :character   Class :character   1st Qu.: 59    1st Qu.:4.959  
##  Mode  :character   Mode  :character   Median : 77    Median :5.286  
##                                        Mean   : 79    Mean   :5.333  
##                                        3rd Qu.: 95    3rd Qu.:5.813  
##                                        Max.   :134    Max.   :6.505  
##  Standard.Error   Economy..GDP.per.Capita.     Family      
##  Min.   :0.3905   Min.   :0.3856           Min.   :0.5389  
##  1st Qu.:0.8015   1st Qu.:0.9056           1st Qu.:0.6509  
##  Median :1.0122   Median :1.1061           Median :0.7313  
##  Mean   :0.9424   Mean   :1.0530           Mean   :0.7188  
##  3rd Qu.:1.1225   3rd Qu.:1.2279           3rd Qu.:0.7736  
##  Max.   :1.1850   Max.   :1.3404           Max.   :0.8734  
##  Health..Life.Expectancy.    Freedom        Trust..Government.Corruption.
##  Min.   :0.09245          Min.   :0.00227   Min.   :0.00199              
##  1st Qu.:0.25883          1st Qu.:0.02652   1st Qu.:0.10686              
##  Median :0.35068          Median :0.04212   Median :0.15275              
##  Mean   :0.35827          Mean   :0.08667   Mean   :0.15226              
##  3rd Qu.:0.44888          3rd Qu.:0.14296   3rd Qu.:0.20951              
##  Max.   :0.65821          Max.   :0.38331   Max.   :0.30030              
##    Generosity     Dystopia.Residual
##  Min.   :0.8999   Mode:logical     
##  1st Qu.:1.7393   NA's:29          
##  Median :2.0250                    
##  Mean   :2.0214                    
##  3rd Qu.:2.2464                    
##  Max.   :3.1071
summary(ECE2016)
##    Country             Region          Happiness.Rank   Happiness.Score
##  Length:29          Length:29          Min.   : 27.00   Min.   :4.217  
##  Class :character   Class :character   1st Qu.: 60.00   1st Qu.:5.145  
##  Mode  :character   Mode  :character   Median : 74.00   Median :5.488  
##                                        Mean   : 78.45   Mean   :5.371  
##                                        3rd Qu.: 91.00   3rd Qu.:5.813  
##                                        Max.   :129.00   Max.   :6.596  
##  Standard.Error   Economy..GDP.per.Capita.     Family      
##  Min.   :0.4884   Min.   :0.1925           Min.   :0.4401  
##  1st Qu.:0.9014   1st Qu.:0.7417           1st Qu.:0.5739  
##  Median :1.1131   Median :0.9316           Median :0.6408  
##  Mean   :1.0475   Mean   :0.8619           Mean   :0.6316  
##  3rd Qu.:1.2323   3rd Qu.:1.0469           3rd Qu.:0.6810  
##  Max.   :1.3092   Max.   :1.1681           Max.   :0.7915  
##  Health..Life.Expectancy.    Freedom        Trust..Government.Corruption.
##  Min.   :0.09511          Min.   :0.00000   Min.   :0.02025              
##  1st Qu.:0.19770          1st Qu.:0.03586   1st Qu.:0.09929              
##  Median :0.29091          Median :0.04762   Median :0.16840              
##  Mean   :0.30053          Mean   :0.08807   Mean   :0.17090              
##  3rd Qu.:0.40212          3rd Qu.:0.12721   3rd Qu.:0.22567              
##  Max.   :0.60848          Max.   :0.31880   Max.   :0.38432              
##    Generosity    Dystopia.Residual
##  Min.   :1.154   Mode:logical     
##  1st Qu.:1.979   NA's:29          
##  Median :2.275                    
##  Mean   :2.270                    
##  3rd Qu.:2.493                    
##  Max.   :3.380