install.packages("data.table")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("plotly")
install.packages("wildcard")
install.packages("shiny")
install.packages("PerformanceAnalytics")
install.packages("DT")

library(data.table)
library(tidyverse)
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
## ✔ tibble  1.4.2          ✔ dplyr   0.7.4     
## ✔ tidyr   0.8.0          ✔ stringr 1.3.0     
## ✔ readr   1.1.1          ✔ forcats 0.3.0
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::between()   masks data.table::between()
## ✖ dplyr::filter()    masks stats::filter()
## ✖ dplyr::first()     masks data.table::first()
## ✖ dplyr::lag()       masks stats::lag()
## ✖ dplyr::last()      masks data.table::last()
## ✖ purrr::transpose() masks data.table::transpose()
## ✖ dplyr::vars()      masks ggplot2::vars()
library(corrplot)
## corrplot 0.84 loaded
library(plotly)
## 
## Attaching package: 'plotly'
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
## The following object is masked from 'package:stats':
## 
##     filter
## The following object is masked from 'package:graphics':
## 
##     layout
library(wildcard)
library(shiny)
library(PerformanceAnalytics)
## Loading required package: xts
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## 
## Attaching package: 'xts'
## The following objects are masked from 'package:dplyr':
## 
##     first, last
## The following objects are masked from 'package:data.table':
## 
##     first, last
## 
## Attaching package: 'PerformanceAnalytics'
## The following object is masked from 'package:graphics':
## 
##     legend
library(DT)
## 
## Attaching package: 'DT'
## The following objects are masked from 'package:shiny':
## 
##     dataTableOutput, renderDataTable

setwd("/Users/lakshyagupta/Desktop/Data Science/world happiness project")
#(https://www.kaggle.com/unsdsn/world-happiness)
wh15 <- fread("2015.csv", data.table = FALSE)
wh16 <- fread("2016.csv", data.table = FALSE)
wh17 <- fread("2017.csv", data.table = FALSE)
wh15$year <- 2015
wh16$year <- 2016
wh17$year <- 2017

names(wh17)[2] <- "Happiness Rank"
names(wh17)[3] <- "Happiness Score"
names(wh17)[6] <- "Economy (GDP per Capita)"
names(wh17)[8] <- "Health (Life Expectancy)"
names(wh17)[11] <- "Trust (Government Corruption)"
names(wh17)[12] <- "Dystopia Residual"

wh15_17 <- bind_rows(wh15,wh16,wh17)
names(wh15_17)[5] <- "SD_error"
wh15_17 <- wh15_17 %>% select(Country:year,-Region, -SD_error)

names(wh15_17) <- c("Country","Happiness_Rank","Happiness_Score","Economy_GDP",
                    "Family","Health","Freedom","Trust",
                    "Generosity","Dystopia_Residual","year")

names(wh15_17)
##  [1] "Country"           "Happiness_Rank"    "Happiness_Score"  
##  [4] "Economy_GDP"       "Family"            "Health"           
##  [7] "Freedom"           "Trust"             "Generosity"       
## [10] "Dystopia_Residual" "year"
head(wh15_17)
##       Country Happiness_Rank Happiness_Score Economy_GDP  Family  Health
## 1 Switzerland              1           7.587     1.39651 1.34951 0.94143
## 2     Iceland              2           7.561     1.30232 1.40223 0.94784
## 3     Denmark              3           7.527     1.32548 1.36058 0.87464
## 4      Norway              4           7.522     1.45900 1.33095 0.88521
## 5      Canada              5           7.427     1.32629 1.32261 0.90563
## 6     Finland              6           7.406     1.29025 1.31826 0.88911
##   Freedom   Trust Generosity Dystopia_Residual year
## 1 0.66557 0.41978    0.29678           2.51738 2015
## 2 0.62877 0.14145    0.43630           2.70201 2015
## 3 0.64938 0.48357    0.34139           2.49204 2015
## 4 0.66973 0.36503    0.34699           2.46531 2015
## 5 0.63297 0.32957    0.45811           2.45176 2015
## 6 0.64169 0.41372    0.23351           2.61955 2015
str(wh15_17)
## 'data.frame':    470 obs. of  11 variables:
##  $ Country          : chr  "Switzerland" "Iceland" "Denmark" "Norway" ...
##  $ Happiness_Rank   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Happiness_Score  : num  7.59 7.56 7.53 7.52 7.43 ...
##  $ Economy_GDP      : num  1.4 1.3 1.33 1.46 1.33 ...
##  $ Family           : num  1.35 1.4 1.36 1.33 1.32 ...
##  $ Health           : num  0.941 0.948 0.875 0.885 0.906 ...
##  $ Freedom          : num  0.666 0.629 0.649 0.67 0.633 ...
##  $ Trust            : num  0.42 0.141 0.484 0.365 0.33 ...
##  $ Generosity       : num  0.297 0.436 0.341 0.347 0.458 ...
##  $ Dystopia_Residual: num  2.52 2.7 2.49 2.47 2.45 ...
##  $ year             : num  2015 2015 2015 2015 2015 ...
summary(wh15_17)
##    Country          Happiness_Rank   Happiness_Score  Economy_GDP    
##  Length:470         Min.   :  1.00   Min.   :2.693   Min.   :0.0000  
##  Class :character   1st Qu.: 40.00   1st Qu.:4.509   1st Qu.:0.6053  
##  Mode  :character   Median : 79.00   Median :5.282   Median :0.9954  
##                     Mean   : 78.83   Mean   :5.371   Mean   :0.9278  
##                     3rd Qu.:118.00   3rd Qu.:6.234   3rd Qu.:1.2524  
##                     Max.   :158.00   Max.   :7.587   Max.   :1.8708  
##      Family           Health          Freedom           Trust        
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:0.7930   1st Qu.:0.4023   1st Qu.:0.2976   1st Qu.:0.05978  
##  Median :1.0257   Median :0.6301   Median :0.4183   Median :0.09950  
##  Mean   :0.9903   Mean   :0.5800   Mean   :0.4028   Mean   :0.13479  
##  3rd Qu.:1.2287   3rd Qu.:0.7683   3rd Qu.:0.5169   3rd Qu.:0.17316  
##  Max.   :1.6106   Max.   :1.0252   Max.   :0.6697   Max.   :0.55191  
##    Generosity     Dystopia_Residual      year     
##  Min.   :0.0000   Min.   :0.3286    Min.   :2015  
##  1st Qu.:0.1528   1st Qu.:1.7380    1st Qu.:2015  
##  Median :0.2231   Median :2.0946    Median :2016  
##  Mean   :0.2422   Mean   :2.0927    Mean   :2016  
##  3rd Qu.:0.3158   3rd Qu.:2.4556    3rd Qu.:2017  
##  Max.   :0.8381   Max.   :3.8377    Max.   :2017

countries.didnt.appear.3years <- wh15_17 %>% group_by(Country) %>% mutate(count = sum(year))
countries.didnt.appear.3years %>% filter(count != 6048) %>% select(Country, Happiness_Rank, year) %>% arrange(Country)
## # A tibble: 32 x 3
## # Groups:   Country [20]
##    Country                  Happiness_Rank  year
##    <chr>                             <int> <dbl>
##  1 Belize                               52 2016.
##  2 Belize                               50 2017.
##  3 Central African Republic            148 2015.
##  4 Central African Republic            155 2017.
##  5 Comoros                             140 2015.
##  6 Comoros                             138 2016.
##  7 Djibouti                            126 2015.
##  8 Hong Kong                            72 2015.
##  9 Hong Kong                            75 2016.
## 10 Hong Kong S.A.R., China              71 2017.
## # ... with 22 more rows


corrplot(cor(wh15_17 %>% 
               select(Happiness_Score:Dystopia_Residual)), 
         method="color",  
         sig.level = 0.01, insig = "blank",
         addCoef.col = "black", 
         tl.srt=45, 
         type="upper"
)


hist(wh15_17$Happiness_Score , xlab = "World Happiness Score from 2015 to 2017", main = "World Happiness Score from 2015 to 2017")



p <- ggplot(wh15_17 %>% filter(year==2017), aes(x= Happiness_Score,y= 
                                                  reorder(Country,Happiness_Score))) + 
  geom_point(colour = "red", alpha = .5) + 
  geom_segment(aes(yend=reorder(Country, Happiness_Score)), xend = 0, colour="pink", alpha = .5) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  labs(title = "World Happiness Rank in 2017", y = "Country Name", x = "Happiness Score")
ggplotly(p)

world <- map_data('world')
## 
## Attaching package: 'maps'
## The following object is masked from 'package:purrr':
## 
##     map
world <- world %>% filter(region != "Antarctica")
world <- fortify(world)
happiness.score17 <- wh15_17 %>% select(Country, Happiness_Score, year) %>% filter(year == 2017)
happiness.score17 <- wildcard(df = happiness.score17, wildcard = "United States", values = "USA",
                              expand = TRUE, rules = NULL)
happiness.score17 <- wildcard(df = happiness.score17, wildcard = "United Kingdom", values = "UK",
                              expand = TRUE, rules = NULL)

happiness.score17 <- wildcard(df = happiness.score17, wildcard = "Democratic Republic of the Congo", values = "Congo (Kinshasa)",
                              expand = TRUE, rules = NULL)
ggplot() + 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_map(data=happiness.score17, map=world,
           aes(fill=Happiness_Score, map_id=Country),
           colour="black") + 
  scale_fill_continuous(low="red", high="yellow",
                        guide="colorbar") + 
  labs(title = "World Happiness Score in 2017")
## Warning: Ignoring unknown aesthetics: x, y


plot_ly(data = wh15_17, 
        x=~Economy_GDP, y=~Happiness_Score, color=~Health, type = "scatter",
        text = ~paste("Country:", Country)) %>% 
  layout(title = "Happiness, GDP and Health relationship", 
         xaxis = list(title = "GDP per Capita"),
         yaxis = list(title = "Happiness Score"))
## No scatter mode specifed:
##   Setting the mode to markers
##   Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode

names(wh16)[4] <- "Happiness_Score"
ggplot(wh16, aes(x=Region, y= Happiness_Score, colour = Region)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Happiness Score Boxplot",
       x = "Region",
       y = "Happiness Score")


world.happiness17 <- wh15_17 %>% filter(year == 2017)
top5 <- world.happiness17 %>% head(5) %>% mutate(Level = "TOP5")
middle5 <- world.happiness17[76:80, ] %>% mutate(Level = "MIDDLE5")
worst5 <- world.happiness17 %>% tail(5) %>% mutate(Level = "WORST5")

comparison <- bind_rows(top5, middle5, worst5)

comparison$Level <- as.factor(comparison$Level)
comparison <- transform(comparison, Level = factor(Level, levels = c("TOP5", "MIDDLE5", "WORST5" )))
datatable(comparison,
          options = list(
            lengthMenu = c(5, 10, 15)
          ),
          caption = 
            htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: center;', 
              htmltools::em('Data table that only includes top5, middle5 and worst5 countries'))
)

comparison.score <- comparison %>% gather(key = "columns", value = "score", Happiness_Score:Dystopia_Residual)
comparison.score %>% 
  ggplot(aes(x = Level, y = score, colour = Level, fill = Level)) + 
  geom_boxplot(position=position_dodge(width=1)) + facet_wrap(~columns, scales = "free")

