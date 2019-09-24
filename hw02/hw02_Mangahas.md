    library(gapminder)
    library(tidyverse)

1.1= Filter to three countries in the 1970s
===========================================

    gapminder.assign2<- gapminder %>%
      filter(country %in% c('Canada', 'Ireland' , 'Madagascar'))%>%
      filter(year <= 1979 & year >= 1970)

\#Assign new name for filtered dataset

    gapminder.assign2 <-gapminder %>%
      filter(country %in% c('Canada', 'Ireland' , 'Madagascar'))%>%
      filter(year <= 1979 & year >= 1970)

1.2= Filtered dataset gapminder.assign2, select for country and gdpPercap
=========================================================================

    gapminder.assign2 %>%
      select (country, gdpPercap) 

    ## # A tibble: 6 x 2
    ##   country    gdpPercap
    ##   <fct>          <dbl>
    ## 1 Canada        18971.
    ## 2 Canada        22091.
    ## 3 Ireland        9531.
    ## 4 Ireland       11151.
    ## 5 Madagascar     1749.
    ## 6 Madagascar     1544.

1.3= Create new column with change of lifeExp, filter to values that have a negative value (displaying decrease in values)
==========================================================================================================================

    gapminder %>% 
      group_by(country) %>%
      mutate(change_lifeExp=(lifeExp-lag(lifeExp)))%>%
      filter(change_lifeExp <0)

    ## # A tibble: 102 x 7
    ## # Groups:   country [52]
    ##    country  continent  year lifeExp     pop gdpPercap change_lifeExp
    ##    <fct>    <fct>     <int>   <dbl>   <int>     <dbl>          <dbl>
    ##  1 Albania  Europe     1992    71.6 3326498     2497.         -0.419
    ##  2 Angola   Africa     1987    39.9 7874230     2430.         -0.036
    ##  3 Benin    Africa     2002    54.4 7026113     1373.         -0.371
    ##  4 Botswana Africa     1992    62.7 1342614     7954.         -0.877
    ##  5 Botswana Africa     1997    52.6 1536536     8647.        -10.2  
    ##  6 Botswana Africa     2002    46.6 1630347    11004.         -5.92 
    ##  7 Bulgaria Europe     1977    70.8 8797022     7612.         -0.09 
    ##  8 Bulgaria Europe     1992    71.2 8658506     6303.         -0.15 
    ##  9 Bulgaria Europe     1997    70.3 8066057     5970.         -0.87 
    ## 10 Burundi  Africa     1992    44.7 5809236      632.         -3.48 
    ## # … with 92 more rows

1.4= Filter each country by maxgdpPercap
========================================

    gapminder %>%
      group_by(country) %>%
        filter(gdpPercap==max(gdpPercap))

    ## # A tibble: 142 x 6
    ## # Groups:   country [142]
    ##    country     continent  year lifeExp       pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>     <int>     <dbl>
    ##  1 Afghanistan Asia       1982    39.9  12881816      978.
    ##  2 Albania     Europe     2007    76.4   3600523     5937.
    ##  3 Algeria     Africa     2007    72.3  33333216     6223.
    ##  4 Angola      Africa     1967    36.0   5247469     5523.
    ##  5 Argentina   Americas   2007    75.3  40301927    12779.
    ##  6 Australia   Oceania    2007    81.2  20434176    34435.
    ##  7 Austria     Europe     2007    79.8   8199783    36126.
    ##  8 Bahrain     Asia       2007    75.6    708573    29796.
    ##  9 Bangladesh  Asia       2007    64.1 150448339     1391.
    ## 10 Belgium     Europe     2007    79.4  10392226    33693.
    ## # … with 132 more rows

1.5= Scatterplot Canada’s life expectancy vs gdpPercap
======================================================

    gapminder_Canada<- gapminder %>%
      filter(country=='Canada')

    ggplot(gapminder_Canada, aes(lifeExp,gdpPercap)) +
      geom_line(colour="red", alpha=0.8) +
      scale_y_log10("GDP per capita", labels=scales::dollar_format()) +
      ylab("GDP per capita") +
      xlab("Life Expectancy") +
      ggtitle("Canada's Life Expectancy vs GDP per capita (1952-2007)")+
      theme(plot.title = element_text(hjust = 0.5))

![](hw02_Mangahas_files/figure-markdown_strict/unnamed-chunk-7-1.png)

2= Exploring categorical variable (continent) and quantitative variable(pop)
============================================================================

    Gapminder_Con_Pop <-gapminder %>% 
      select(continent,pop)

\#Number of entries from each continent (summary)

    summary(Gapminder_Con_Pop$continent, digits=1)

    ##   Africa Americas     Asia   Europe  Oceania 
    ##      624      300      396      360       24

\#Number of entries from each continent (table)

    Gapminder_Con_Pop %>% 
      count(continent)

    ## # A tibble: 5 x 2
    ##   continent     n
    ##   <fct>     <int>
    ## 1 Africa      624
    ## 2 Americas    300
    ## 3 Asia        396
    ## 4 Europe      360
    ## 5 Oceania      24

\#Maximum ppulation values for each of the 5 continents

    Gapminder_Con_Pop %>% 
      group_by(continent) %>% 
      summarise(max(pop))

    ## # A tibble: 5 x 2
    ##   continent `max(pop)`
    ##   <fct>          <int>
    ## 1 Africa     135031164
    ## 2 Americas   301139947
    ## 3 Asia      1318683096
    ## 4 Europe      82400996
    ## 5 Oceania     20434176

\#Minimum population values for each of the 5 continents

    Gapminder_Con_Pop %>% 
      group_by(continent) %>% 
      summarise(min(pop))

    ## # A tibble: 5 x 2
    ##   continent `min(pop)`
    ##   <fct>          <int>
    ## 1 Africa         60011
    ## 2 Americas      662850
    ## 3 Asia          120447
    ## 4 Europe        147962
    ## 5 Oceania      1994794

\#In summary, there are 5 continents with differing number of entries,
and each of them has a wide range of populations over time.

\#Finding the mean population in the gapminder dataset for each
continent

    Gapminder_Con_Pop_Avg <-gapminder %>% 
      group_by(continent) %>% 
      summarise(average=mean(pop))

\#Creating a column graph displaying results

    ggplot(Gapminder_Con_Pop_Avg, aes(continent, average)) +
      geom_col()+
      xlab("Continent")+
      ylab("Average Population")+
      ggtitle("Continent vs Average Population \n in Gapminder Dataset") +
        theme(plot.title = element_text(hjust = 0.5))

![](hw02_Mangahas_files/figure-markdown_strict/unnamed-chunk-14-1.png)

\#In conclusion, this ggplot shows that Asia has the highest average
population within the dataset, while Oceania has the lowest (albeit
close to that of Africa), with average values ranging from the thousands
to millions.

3= Exploring various plot types
===============================

\#Found a new dataset to look at through ‘R Datasets Package’ called
rock, n=48

    library(rock)

\#First ggplot (scatterplot) of 2 quantitative variables (area,
perimeter)

    ggplot(rock, aes(area, peri)) +
      geom_point(colour='purple', alpha=0.8) +
      theme_bw() +
      xlab('Area of Pores Space (pixels out of 256x256)')+
      ylab('Perimeter (pixels)') +
      ggtitle("Area of Pores Space vs Perimeter 
            of Petroleum Rock Samples") +
        theme(plot.title = element_text(hjust = 0.5))

![](hw02_Mangahas_files/figure-markdown_strict/unnamed-chunk-16-1.png)

\#Second ggplot (smooth) of 2 quantitative variables (area, perimeter)

    ggplot(rock, aes(area, peri)) +
      geom_smooth(colour='pink', alpha=0.8) +
      theme_bw() +
      xlab('Area of Pores Space (pixels out of 256x256)')+
      ylab('Perimeter (pixels)') +
      ggtitle("Petroleum Rock Samples: Area of Pores Space vs Perimeter") +
        theme(plot.title = element_text(hjust = 0.5))

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw02_Mangahas_files/figure-markdown_strict/unnamed-chunk-17-1.png)

Recycling (Optional) (extra 2%)
===============================

    filter(gapminder, country == c("Rwanda", "Afghanistan"))

    ## # A tibble: 12 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1957    30.3  9240934      821.
    ##  2 Afghanistan Asia       1967    34.0 11537966      836.
    ##  3 Afghanistan Asia       1977    38.4 14880372      786.
    ##  4 Afghanistan Asia       1987    40.8 13867957      852.
    ##  5 Afghanistan Asia       1997    41.8 22227415      635.
    ##  6 Afghanistan Asia       2007    43.8 31889923      975.
    ##  7 Rwanda      Africa     1952    40    2534927      493.
    ##  8 Rwanda      Africa     1962    43    3051242      597.
    ##  9 Rwanda      Africa     1972    44.6  3992121      591.
    ## 10 Rwanda      Africa     1982    46.2  5507565      882.
    ## 11 Rwanda      Africa     1992    23.6  7290203      737.
    ## 12 Rwanda      Africa     2002    43.4  7852401      786.

\#No, they did not succeed as not all of the data for Afghanistan and
Rwanda is being displayed. It seems that it is only showing data from
each decade.

\#Correct way to do this

    gapminder %>% 
      filter (country=='Rwanda' | country== 'Afghanistan')

    ## # A tibble: 24 x 6
    ##    country     continent  year lifeExp      pop gdpPercap
    ##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Afghanistan Asia       1952    28.8  8425333      779.
    ##  2 Afghanistan Asia       1957    30.3  9240934      821.
    ##  3 Afghanistan Asia       1962    32.0 10267083      853.
    ##  4 Afghanistan Asia       1967    34.0 11537966      836.
    ##  5 Afghanistan Asia       1972    36.1 13079460      740.
    ##  6 Afghanistan Asia       1977    38.4 14880372      786.
    ##  7 Afghanistan Asia       1982    39.9 12881816      978.
    ##  8 Afghanistan Asia       1987    40.8 13867957      852.
    ##  9 Afghanistan Asia       1992    41.7 16317921      649.
    ## 10 Afghanistan Asia       1997    41.8 22227415      635.
    ## # … with 14 more rows
