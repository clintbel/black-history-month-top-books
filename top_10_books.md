Top 10 African-American History Books
================
Clint Barnard-El
February 27, 2024

### R Markdown file for project that includes

1.  Web Scraping
2.  Data Cleaning and Analysis
3.  Reporting

## Web Scraping Notes

- Source: Google Scholar
- Tool: Octoparse
- Extraction Date: 2/24/24

###### Objective: Identify top 10 African-American History books based on citations

### Load libraries and Data

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dtplyr)
library(readxl)
library(tidyselect)
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.3.2

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.3.2

``` r
library(yaml)
```

### Import Dataset

Load dataset from results of web scraping (Google Scholar/Octoparse) and
clean column names

``` r
df_raw <- read_excel("C:/Users/Clint/Downloads/Data Project/google_scholar_article_listing_raw.xlsx")
clean_df_raw <- clean_names(df_raw)
df_a <- clean_df_raw #rename for simplicity
print(df_a)
```

    ## # A tibble: 579 × 8
    ##    title   author published_year description article_link cited_for all_versions
    ##    <chr>   <chr>  <chr>          <chr>       <chr>        <chr>     <chr>       
    ##  1 From N… BL Ma… 1991           "… politic… https://www… 322       4           
    ##  2 [BOOK]… NI Pa… 2006           "… deepen … https://boo… 357       3           
    ##  3 The sh… CL Ni… 2007           "… As WEB … https://hei… 60        4           
    ##  4 [BOOK]… TM Sh… 2004           "… Black W… https://boo… 1634      7           
    ##  5 The Ne… G Ear… 2008           "… the sam… https://www… 20        8           
    ##  6 Wealth… T Cra… 2020           "… We will… https://jou… 55        4           
    ##  7 [BOOK]… HL Ga… 2008           "… to chan… https://www… 217       6           
    ##  8 [BOOK]… BT Wa… 2011           "… church … https://boo… 208       8           
    ##  9 [BOOK]… A App… 2005           "… history… https://boo… 595       4           
    ## 10 Gettin… D Con… 1999           "… In the … https://www… 58        6           
    ## # ℹ 569 more rows
    ## # ℹ 1 more variable: related_articles_link <chr>

### Clean and Filter Data

Correct columns formats (character to integer) and values for case
sensitive functions (lowercase)

``` r
df_a[, c(3, 6, 7)] <-lapply(df_a[, c(3, 6, 7)], as.integer)
```

    ## Warning in lapply(df_a[, c(3, 6, 7)], as.integer): NAs introduced by coercion

``` r
#df_a[, c(1, 2)] <- apply(df_a[, c(1, 2)], 2, tolower)
df_a$title <- tolower(df_a$title)
print(df_a)
```

    ## # A tibble: 579 × 8
    ##    title   author published_year description article_link cited_for all_versions
    ##    <chr>   <chr>           <int> <chr>       <chr>            <int>        <int>
    ##  1 from n… BL Ma…           1991 "… politic… https://www…       322            4
    ##  2 [book]… NI Pa…           2006 "… deepen … https://boo…       357            3
    ##  3 the sh… CL Ni…           2007 "… As WEB … https://hei…        60            4
    ##  4 [book]… TM Sh…           2004 "… Black W… https://boo…      1634            7
    ##  5 the ne… G Ear…           2008 "… the sam… https://www…        20            8
    ##  6 wealth… T Cra…           2020 "… We will… https://jou…        55            4
    ##  7 [book]… HL Ga…           2008 "… to chan… https://www…       217            6
    ##  8 [book]… BT Wa…           2011 "… church … https://boo…       208            8
    ##  9 [book]… A App…           2005 "… history… https://boo…       595            4
    ## 10 gettin… D Con…           1999 "… In the … https://www…        58            6
    ## # ℹ 569 more rows
    ## # ℹ 1 more variable: related_articles_link <chr>

Verify data to ensure title include specific keywords

``` r
filter_df <- df_a[grepl("african|negro|black", 
                           tolower(df_a$title)) & grepl("history", tolower(df_a$description)), ] 
print(filter_df)
```

    ## # A tibble: 120 × 8
    ##    title   author published_year description article_link cited_for all_versions
    ##    <chr>   <chr>           <int> <chr>       <chr>            <int>        <int>
    ##  1 [book]… NI Pa…           2006 "… deepen … https://boo…       357            3
    ##  2 [book]… A App…           2005 "… history… https://boo…       595            4
    ##  3 [book]… M Mar…           2006 "… of weal… https://boo…       179            4
    ##  4 [book]… WD Jo…           2013 "… So whet… https://boo…      4229            7
    ##  5 [book]… P Fin…           2009 "… two enc… https://boo…        79            3
    ##  6 [book]… B Qua…           1996 "… African… https://boo…       711            3
    ##  7 [book]… WS Ro…           2009 "… , white… https://boo…        14            2
    ##  8 “negro… RO Se…           2003 "… that hi… https://lin…        14           NA
    ##  9 [book]… WEB D…           2017 "… tions t… https://www…      5715            5
    ## 10 [html]… R Aus…           2004 "… he is m… https://sea…        29            3
    ## # ℹ 110 more rows
    ## # ℹ 1 more variable: related_articles_link <chr>

Remove columns outside of project objective, populate empty fields in
new column “format”

``` r
df <- filter_df[ -c(4, 5, 7, 8) ]
df$format <- NA #assign NA to null
print(df)
```

    ## # A tibble: 120 × 5
    ##    title                                  author published_year cited_for format
    ##    <chr>                                  <chr>           <int>     <int> <lgl> 
    ##  1 [book][b] creating black americans: a… NI Pa…           2006       357 NA    
    ##  2 [book][b] africana: the encyclopedia … A App…           2005       595 NA    
    ##  3 [book][b] living black history: how r… M Mar…           2006       179 NA    
    ##  4 [book][b] white over black: american … WD Jo…           2013      4229 NA    
    ##  5 [book][b] encyclopedia of african ame… P Fin…           2009        79 NA    
    ##  6 [book][b] negro in the making of amer… B Qua…           1996       711 NA    
    ##  7 [book][b] the african american entrep… WS Ro…           2009        14 NA    
    ##  8 “negro leadership and negro money”: a… RO Se…           2003        14 NA    
    ##  9 [book][b] black reconstruction in ame… WEB D…           2017      5715 NA    
    ## 10 [html][html] kwanzaa and the commodif… R Aus…           2004        29 NA    
    ## # ℹ 110 more rows

Prepare data for organization; populate format column based on 1 of 3
keywords from title; replace “NA” in cited_for with “0” (avoid
summarizing error)

``` r
df$format [grep("book", df$title)] <- "book"
df$format [grep("pdf", df$title)] <- "pdf"
df$format [is.na(df$format)] <- "article"
df$cited_for [is.na(df$cited_for)] <- 0
print(df)
```

    ## # A tibble: 120 × 5
    ##    title                                  author published_year cited_for format
    ##    <chr>                                  <chr>           <int>     <dbl> <chr> 
    ##  1 [book][b] creating black americans: a… NI Pa…           2006       357 book  
    ##  2 [book][b] africana: the encyclopedia … A App…           2005       595 book  
    ##  3 [book][b] living black history: how r… M Mar…           2006       179 book  
    ##  4 [book][b] white over black: american … WD Jo…           2013      4229 book  
    ##  5 [book][b] encyclopedia of african ame… P Fin…           2009        79 book  
    ##  6 [book][b] negro in the making of amer… B Qua…           1996       711 book  
    ##  7 [book][b] the african american entrep… WS Ro…           2009        14 book  
    ##  8 “negro leadership and negro money”: a… RO Se…           2003        14 artic…
    ##  9 [book][b] black reconstruction in ame… WEB D…           2017      5715 book  
    ## 10 [html][html] kwanzaa and the commodif… R Aus…           2004        29 artic…
    ## # ℹ 110 more rows

Create new table of top 10 book titles that had the most citations; view
table for verification

``` r
top_books <- df %>%
  filter(grepl("book", title)) %>%   
  arrange(desc(cited_for)) %>%      
  head(10)  
print(top_books)
```

    ## # A tibble: 10 × 5
    ##    title                                  author published_year cited_for format
    ##    <chr>                                  <chr>           <int>     <dbl> <chr> 
    ##  1 [book][b] black reconstruction in ame… WEB D…           2017      5715 book  
    ##  2 [book][b] white over black: american … WD Jo…           2013      4229 book  
    ##  3 [book][b] black families               HP Mc…           2007      1493 book  
    ##  4 [book][b] from savage to negro: anthr… LD Ba…           1998      1097 book  
    ##  5 [book][b] the negro in the american r… B Qua…           2012       724 book  
    ##  6 [book][b] negro in the making of amer… B Qua…           1996       711 book  
    ##  7 [book][b] africana: the encyclopedia … A App…           2005       595 book  
    ##  8 [book][b] in the shadow of slavery: a… LM Ha…           2023       554 book  
    ##  9 [book][b] in hope of liberty: culture… JO Ho…           1998       503 book  
    ## 10 [book][b] la city limits: african ame… J Sid…           2004       494 book

Clean title column to remove superfluous sub-string characters, print
table

``` r
df_clean <- top_books %>%
  mutate(title = substr(title, 11, nchar(title)))
print(df_clean)
```

    ## # A tibble: 10 × 5
    ##    title                                  author published_year cited_for format
    ##    <chr>                                  <chr>           <int>     <dbl> <chr> 
    ##  1 black reconstruction in america: towa… WEB D…           2017      5715 book  
    ##  2 white over black: american attitudes … WD Jo…           2013      4229 book  
    ##  3 black families                         HP Mc…           2007      1493 book  
    ##  4 from savage to negro: anthropology an… LD Ba…           1998      1097 book  
    ##  5 the negro in the american revolution   B Qua…           2012       724 book  
    ##  6 negro in the making of america: revis… B Qua…           1996       711 book  
    ##  7 africana: the encyclopedia of the afr… A App…           2005       595 book  
    ##  8 in the shadow of slavery: african ame… LM Ha…           2023       554 book  
    ##  9 in hope of liberty: culture, communit… JO Ho…           1998       503 book  
    ## 10 la city limits: african american los … J Sid…           2004       494 book

Title-Case columns 1 and 5 for better presentation

``` r
df_clean$title <- tools::toTitleCase(df_clean$title)
df_clean$format <- tools::toTitleCase(df_clean$format)
print(df_clean)
```

    ## # A tibble: 10 × 5
    ##    title                                  author published_year cited_for format
    ##    <chr>                                  <chr>           <int>     <dbl> <chr> 
    ##  1 Black Reconstruction in America: Towa… WEB D…           2017      5715 Book  
    ##  2 White over Black: American Attitudes … WD Jo…           2013      4229 Book  
    ##  3 Black Families                         HP Mc…           2007      1493 Book  
    ##  4 From Savage to Negro: Anthropology an… LD Ba…           1998      1097 Book  
    ##  5 The Negro in the American Revolution   B Qua…           2012       724 Book  
    ##  6 Negro in the Making of America: Revis… B Qua…           1996       711 Book  
    ##  7 Africana: The Encyclopedia of the Afr… A App…           2005       595 Book  
    ##  8 In the Shadow of Slavery: African Ame… LM Ha…           2023       554 Book  
    ##  9 In Hope of Liberty: Culture, Communit… JO Ho…           1998       503 Book  
    ## 10 La City Limits: African American Los … J Sid…           2004       494 Book

### Print Table

Clean title column to remove superfluous sub-string characters, print
table

``` r
data <- df_clean[1:10, ]
knitr::kable(data,
 caption = "Top 10 African-American History Books")
```

| title                                                                                                                                               | author               | published_year | cited_for | format |
|:----------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------|---------------:|----------:|:-------|
| Black Reconstruction in America: Toward a History of the Part which Black Folk Played in the Attempt to Reconstruct Democracy in America, 1860-1880 | WEB Du Bois          |           2017 |      5715 | Book   |
| White over Black: American Attitudes Toward the Negro, 1550-1812                                                                                    | WD Jordan            |           2013 |      4229 | Book   |
| Black Families                                                                                                                                      | HP McAdoo            |           2007 |      1493 | Book   |
| From Savage to Negro: Anthropology and the Construction of Race, 1896-1954                                                                          | LD Baker             |           1998 |      1097 | Book   |
| The Negro in the American Revolution                                                                                                                | B Quarles            |           2012 |       724 | Book   |
| Negro in the Making of America: Revised, Updated, and Expanded                                                                                      | B Quarles            |           1996 |       711 | Book   |
| Africana: The Encyclopedia of the African and African American Experience                                                                           | A Appiah, HL Gates   |           2005 |       595 | Book   |
| In the Shadow of Slavery: African Americans in New York City, 1626-1863                                                                             | LM Harris            |           2023 |       554 | Book   |
| In Hope of Liberty: Culture, Community and Protest among Northern Free Blacks, 1700-1860                                                            | JO Horton, LE Horton |           1998 |       503 | Book   |
| La City Limits: African American Los Angeles from the Great Depression to the Present                                                               | J Sides              |           2004 |       494 | Book   |

Top 10 African-American History Books

Export table as .csv file

``` r
#Export table as csv file
write.csv(data, file = "top_10_books.csv", row.names = FALSE)
```
