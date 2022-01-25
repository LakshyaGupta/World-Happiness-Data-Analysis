install.packages("tidyverse")

require(ggplot2)
require(reshape2)

df2015 <- read.csv("2015.csv")

head(df2015)
Country         Region Happiness.Rank Happiness.Score Standard.Error
1 Switzerland Western Europe              1           7.587        0.03411
2     Iceland Western Europe              2           7.561        0.04884
3     Denmark Western Europe              3           7.527        0.03328
4      Norway Western Europe              4           7.522        0.03880
5      Canada  North America              5           7.427        0.03553
6     Finland Western Europe              6           7.406        0.03140
Economy..GDP.per.Capita.  Family Health..Life.Expectancy. Freedom
1                  1.39651 1.34951                  0.94143 0.66557
2                  1.30232 1.40223                  0.94784 0.62877
3                  1.32548 1.36058                  0.87464 0.64938
4                  1.45900 1.33095                  0.88521 0.66973
5                  1.32629 1.32261                  0.90563 0.63297
6                  1.29025 1.31826                  0.88911 0.64169
Trust..Government.Corruption. Generosity Dystopia.Residual
1                       0.41978    0.29678           2.51738
2                       0.14145    0.43630           2.70201
3                       0.48357    0.34139           2.49204
4                       0.36503    0.34699           2.46531
5                       0.32957    0.45811           2.45176
6                       0.41372    0.23351           2.61955
##Countries with highest, lowest Economy GDP per capita

maxEco <- max(df2015$Economy..GDP.per.Capita.)
subset(df2015[c(1,6)], Economy..GDP.per.Capita. == maxEco) ##this will return the country and Economy 
Country Economy..GDP.per.Capita.
28   Qatar                  1.69042
df2015[df2015$Economy..GDP.per.Capita. == maxEco, ] ##this will return the country and Economy - not using subset
Country                          Region Happiness.Rank Happiness.Score
28   Qatar Middle East and Northern Africa             28           6.611
Standard.Error Economy..GDP.per.Capita. Family Health..Life.Expectancy.
28        0.06257                  1.69042 1.0786                  0.79733
Freedom Trust..Government.Corruption. Generosity Dystopia.Residual
28  0.6404                       0.52208    0.32573           1.55674
subset(df2015, Economy..GDP.per.Capita. == maxEco) ##this will return all the details of the country with highest  Economy 
Country                          Region Happiness.Rank Happiness.Score
28   Qatar Middle East and Northern Africa             28           6.611
Standard.Error Economy..GDP.per.Capita. Family Health..Life.Expectancy.
28        0.06257                  1.69042 1.0786                  0.79733
Freedom Trust..Government.Corruption. Generosity Dystopia.Residual
28  0.6404                       0.52208    0.32573           1.55674
df2015[df2015$Economy..GDP.per.Capita. == maxEco, ]
Country                          Region Happiness.Rank Happiness.Score
28   Qatar Middle East and Northern Africa             28           6.611
Standard.Error Economy..GDP.per.Capita. Family Health..Life.Expectancy.
28        0.06257                  1.69042 1.0786                  0.79733
Freedom Trust..Government.Corruption. Generosity Dystopia.Residual
28  0.6404                       0.52208    0.32573           1.55674
head(df2015[c(1,6)]) ## Selecting multiple columns
Country Economy..GDP.per.Capita.
1 Switzerland                  1.39651
2     Iceland                  1.30232
3     Denmark                  1.32548
4      Norway                  1.45900
5      Canada                  1.32629
6     Finland                  1.29025
head(df2015[ ,c(1,6)]) ## Selecting multiple columns - Same as df2015[c(1,6)]
Country Economy..GDP.per.Capita.
1 Switzerland                  1.39651
2     Iceland                  1.30232
3     Denmark                  1.32548
4      Norway                  1.45900
5      Canada                  1.32629
6     Finland                  1.29025
minEco <- min(df2015$Economy..GDP.per.Capita.)
subset(df2015[c(1,6)], Economy..GDP.per.Capita. == minEco) ##Country with lowest Economy GDP per Capita
Country Economy..GDP.per.Capita.
120 Congo (Kinshasa)                        0
##Countries with highest,lowest Family

maxFamily <- max(df2015$Family)
subset(df2015[c(1,7)], Family == maxFamily) ##this will return highest score of the family and the corresponding country
Country  Family
2 Iceland 1.40223
subset(df2015[c("Country","Family")], Family == maxFamily) ##this will return highest score of the family and the corresponding country
Country  Family
2 Iceland 1.40223
minFamily <- min(df2015$Family)
subset(df2015[c(1,7)], Family == minFamily) ##this will return lowest score of the family and the corresponding country
Country Family
148 Central African Republic      0
##Countries with highest,lowest Trust Govt Correpution
maxTrust <- max(df2015$Trust..Government.Corruption.)
subset(df2015[c("Country","Trust..Government.Corruption.")], Trust..Government.Corruption. == maxTrust) ##this will return highest score of the Trust and the corresponding country
Country Trust..Government.Corruption.
154  Rwanda                       0.55191
minTrust <- min(df2015$Trust..Government.Corruption.)
subset(df2015[c("Country","Trust..Government.Corruption.")], Trust..Government.Corruption. == minTrust) ##this will return lowest score of the Trust and the corresponding country
Country Trust..Government.Corruption.
74 Indonesia                             0
##Countries with highest,lowest Genorosity
maxGenerosity <- max(df2015$Generosity)
subset(df2015[c("Country","Generosity")], Generosity == maxGenerosity) ##this will return highest score of the Generosity and the corresponding country
Country Generosity
129 Myanmar    0.79588
minGenerosity <- min(df2015$Generosity)
subset(df2015[c("Country","Generosity")], Generosity == minGenerosity) ##this will return lowest score of the Generosity and the corresponding country
Country Generosity
102  Greece          0
##Countries with highest,lowest Healthlife expectancy

maxHLE <- max(df2015$Health..Life.Expectancy.)
subset(df2015[c("Country","Health..Life.Expectancy.")], Health..Life.Expectancy. == maxHLE) ##this will return highest score of the Health..Life.Expectancy. and the corresponding country
Country Health..Life.Expectancy.
24 Singapore                  1.02525
minHLE <- min(df2015$Health..Life.Expectancy.)
subset(df2015[c("Country","Health..Life.Expectancy.")], Health..Life.Expectancy. == minHLE) ##this will return lowest score of the Health..Life.Expectancy. and the corresponding country
Country Health..Life.Expectancy.
123 Sierra Leone                        0
##Countries with highest,lowest Freedom
maxFreedom <- max(df2015$Freedom)
subset(df2015[c("Country","Freedom")], Freedom == maxFreedom) ##this will return highest score of the Freedom and the corresponding country
Country Freedom
4  Norway 0.66973
minFreedom <- min(df2015$Freedom)
subset(df2015[c("Country","Freedom")], Freedom == minFreedom) ##this will return lowest score of the Freedom and the corresponding country
Country Freedom
112    Iraq       0
#is there is correlation between region and freedom
head(subset(df2015[,c("Region","Freedom")])) ## we have multiple Region which are same. So, let's take average of the regions which are same. 
Region Freedom
1 Western Europe 0.66557
2 Western Europe 0.62877
3 Western Europe 0.64938
4 Western Europe 0.66973
5  North America 0.63297
6 Western Europe 0.64169
#This can be done using aggregate function. Check this link for more details <https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame>
dfRegFre <- aggregate(df2015[,c("Freedom")], list(df2015$Region), mean)

names(dfRegFre) <- c("Region","Freedom") ###Add the header to the dataframe

dfRegFre
Region   Freedom
1        Australia and New Zealand 0.6453100
2       Central and Eastern Europe 0.3582686
3                     Eastern Asia 0.4624900
4      Latin America and Caribbean 0.5017395
5  Middle East and Northern Africa 0.3617510
6                    North America 0.5895050
7                Southeastern Asia 0.5571044
8                    Southern Asia 0.3733371
9               Sub-Saharan Africa 0.3659440
10                  Western Europe 0.5499262
#Since Regions is categorical variable, we cannot find correlation



#is there is correlation between region and health life expectancy

#is there is correlation between country and Economy
#is there is correlation between country and Genorosity
#is there is correlation between country and freedom
#is there is correlation between country and health life expectancy

#is there is correlation between Happiness score and Economy of the countries?
##df2015$Economy..GDP.per.Capita.
dfHapp_vs_Ecnmy <- (subset(df2015[,c("Country","Happiness.Score","Economy..GDP.per.Capita.")])) 
names(dfHapp_vs_Ecnmy) <- c("Country","Happiness Score", "Economy GDP Per Capita")
head(dfHapp_vs_Ecnmy)
Country Happiness Score Economy GDP Per Capita
1 Switzerland           7.587                1.39651
2     Iceland           7.561                1.30232
3     Denmark           7.527                1.32548
4      Norway           7.522                1.45900
5      Canada           7.427                1.32629
6     Finland           7.406                1.29025
plot(dfHapp_vs_Ecnmy) ##bit confusing plot


class(dfHapp_vs_Ecnmy)
[1] "data.frame"
#Let's try scatter plot from the original data frame df2015
plot(df2015$Happiness.Score, df2015$Economy..GDP.per.Capita.)


#Let us colorize the plots 
plot(df2015$Happiness.Score, df2015$Economy..GDP.per.Capita., col ="chocolate", type = "p", pch = 8, main = "R Scatter Plot", xlab = "Happiness.Score", ylab = "Economy.GDP.per.Capita") #This shows positive correlation

##Let's fit the data using linear regression model 
#Check this youtube link for more better understanding: <https://www.youtube.com/watch?v=hokALdIst8k>
simple.Regresssion <- lm(df2015$Economy..GDP.per.Capita.~df2015$Happiness.Score)
summary(simple.Regresssion) ## R-square =0.60, p <0.05 shows there is significant correlation between both happiness and economy of the country, Adjusted R-Sqaure is more used for Multiple regressions

Call:
  lm(formula = df2015$Economy..GDP.per.Capita. ~ df2015$Happiness.Score)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.65177 -0.16831 -0.00513  0.18067  0.62096 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)            -0.63193    0.09675  -6.531 8.71e-10 ***
  df2015$Happiness.Score  0.27495    0.01761  15.617  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2526 on 156 degrees of freedom
Multiple R-squared:  0.6099,    Adjusted R-squared:  0.6074 
F-statistic: 243.9 on 1 and 156 DF,  p-value: < 2.2e-16
##Let's plot Regression line
#abline(lm(df2015$Happiness.Score~df2015$Economy..GDP.per.Capita.)
abline(lm(df2015$Economy..GDP.per.Capita.~df2015$Happiness.Score), col = "red", lwd = 2) 


##Let do Multiple Regression now

df <- df2015[c("Happiness.Score","Economy..GDP.per.Capita.","Generosity")]
head(df)
Happiness.Score Economy..GDP.per.Capita. Generosity
1           7.587                  1.39651    0.29678
2           7.561                  1.30232    0.43630
3           7.527                  1.32548    0.34139
4           7.522                  1.45900    0.34699
5           7.427                  1.32629    0.45811
6           7.406                  1.29025    0.23351
plot(df)


multiple.Regresssion <- lm(df2015$Economy..GDP.per.Capita.~df2015$Happiness.Score + df2015$Generosity)
summary(multiple.Regresssion)

Call:
  lm(formula = df2015$Economy..GDP.per.Capita. ~ df2015$Happiness.Score + 
       df2015$Generosity)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.65502 -0.14707  0.00845  0.16766  0.60883 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)            -0.56722    0.09627  -5.892  2.3e-08 ***
  df2015$Happiness.Score  0.28488    0.01740  16.369  < 2e-16 ***
  df2015$Generosity      -0.49759    0.15730  -3.163  0.00188 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2456 on 155 degrees of freedom
Multiple R-squared:  0.6336,    Adjusted R-squared:  0.6288 
F-statistic:   134 on 2 and 155 DF,  p-value: < 2.2e-16
##Since pvalue < 0.05 and adjusted R-Square is 0.63. It means that Economy and happiness, Generosity have direct linear relationship between them

#But it would still be nice if the data points are colored according to their Regions (since countries are all unique). Let's try!


library(dplyr)
library(formattable)
library(ggplot2)
library(ggthemes)
library(viridis)

open_data_happiness <- df2015[c("Region", "Happiness.Score","Generosity")]

ggplot(open_data_happiness, 
       aes(x = Happiness.Score, 
           y = Generosity)) +
  geom_point(aes(colour = Region),
             size = 2) +
  geom_smooth(method="lm") +
  labs(x = "Happiness.Score",
       y = "Generosity",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data openness and happiness by country in 2015") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))


#this shows that Western Europe countries are happy and are more Generous, on the other hand sub-saharan people are less happy and less generous.

#But it would be interesting to make a global map based on freedom score.

#To work on this in the coming days <https://www.r-bloggers.com/interactive-maps-for-the-web-in-r/>. Let's try chorpleth.

library(rworldmap)

#create a map-shaped window
ddf <- subset(df2015[c("Country","Freedom")])
class(ddf)
[1] "data.frame"
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="Country")
155 codes from your data successfully matched countries in the map
3 codes from your data failed to match with a country code in the map
88 codes from the map weren't represented in your data
mapCountryData(spdf, nameColumnToPlot="Freedom", catMethod="fixedWidth")


#With different colors
mapCountryData(spdf, nameColumnToPlot="Freedom", colourPalette=c('red','orange','blue','green'), catMethod="fixedWidth")


# The more yellowish, the country has more freedom.. Chorpleth maps are Cool!
 mapCountryData(spdf, nameColumnToPlot="Freedom", colourPalette=c('red','orange','blue','green','yellow'), catMethod="fixedWidth") #This takes a while to load. SO, commenting this code for a while/


#mapCountryData(spdf, nameColumnToPlot="Freedom", colourPalette="terrain", catMethod="fixedWidth")
#mapCountryData(spdf, nameColumnToPlot="Freedom", colourPalette="palette", catMethod="fixedWidth")

#More information about rworldmap cab be found here: <https://cran.r-project.org/web/packages/rworldmap/vignettes/rworldmap.pdf>

# I want to generate box plot for all the columns in one chart. Let's do it!
  
  library(ggplot2)

ggplot(stack(df2015), aes(x = ind, y = values)) + geom_boxplot() ##problems : 1) Happiness column have bigger values. so bigger plot and all the rest are small 2) Axis label names are overlapped.


library(dplyr)
##dropping certain columns. Let's drop both columns "Happiness.Rank", "Happiness.Score". Problem 1 solved.
df2_2015 <- df2015[, !names(df2015) %in% c("Happiness.Rank", "Happiness.Score")] 
head(df2_2015)
Country         Region Standard.Error Economy..GDP.per.Capita.  Family
1 Switzerland Western Europe        0.03411                  1.39651 1.34951
2     Iceland Western Europe        0.04884                  1.30232 1.40223
3     Denmark Western Europe        0.03328                  1.32548 1.36058
4      Norway Western Europe        0.03880                  1.45900 1.33095
5      Canada  North America        0.03553                  1.32629 1.32261
6     Finland Western Europe        0.03140                  1.29025 1.31826
Health..Life.Expectancy. Freedom Trust..Government.Corruption. Generosity
1                  0.94143 0.66557                       0.41978    0.29678
2                  0.94784 0.62877                       0.14145    0.43630
3                  0.87464 0.64938                       0.48357    0.34139
4                  0.88521 0.66973                       0.36503    0.34699
5                  0.90563 0.63297                       0.32957    0.45811
6                  0.88911 0.64169                       0.41372    0.23351
Dystopia.Residual
1           2.51738
2           2.70201
3           2.49204
4           2.46531
5           2.45176
6           2.61955
ggplot(stack(df2_2015), aes(x = ind, y = values)) + geom_boxplot() +labs(x = "Criteria",
                                                                         y = "Value",
                                                                         title = "Box plot without columns with high values",
                                                                         subtitle = "Still prblem with axis labels!! Hey look at this minimal theme") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))


ggplot(stack(df2_2015), aes(x = ind, y = values)) + geom_boxplot() +labs(x = "Criteria",
                                                                         y = "Value",
                                                                         title = "Box plot without columns with high values",
                                                                         subtitle = "Still prblem with axis labels!! Hey look at this gray theme") +
  scale_color_viridis(discrete = T) +
  theme_gray() +
  theme(text = element_text(size=16))


ggplot(stack(df2_2015), aes(x = ind, y = values)) + geom_boxplot() +labs(x = "Criteria",
                                                                         y = "Value",
                                                                         title = "Box plot without columns with high values",
                                                                         subtitle = "Still prblem with axis labels!! Hey look at this dark theme") +
  scale_color_viridis(discrete = T) +
  theme_dark() +
  theme(text = element_text(size=16))


ggplot(stack(df2_2015), aes(x = ind, y = values, color = "blue")) + geom_boxplot() +labs(x = "Criteria",
                                                                                         y = "Value",
                                                                                         title = "Box plot without columns with high values",
                                                                                         subtitle = "Still prblem with axis labels!! Dark theme. Let ggplot choose color") +
  scale_fill_viridis() +
  theme_dark() +
  theme(text = element_text(size=16))


##More problems - Instead of value, can I make it percentage on the y-axis?


#dfRegFre <- aggregate(df2015[,c("Freedom")], list(df2015$Region), mean)
#is there is correlation between Happiness score and Genorosity
#is there is correlation between Happiness score and freedom
#is there is correlation between Happiness score and health life expectancy