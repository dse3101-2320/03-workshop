Week 3 Workshop
================
YH
2024-01-31

- [Exercise with data](#exercise-with-data)
  - [CPI for key categories (Page 7 on
    PDF)](#cpi-for-key-categories-page-7-on-pdf)
  - [YOY inflation (Page 8)](#yoy-inflation-page-8)

# Exercise with data

Download `wk3_cpi2023.pdf` from Canvas. The data contain **Singapore’s
Consumer Price Index (CPI) in 2023**, originally retrieved from
[MAS](https://www.mas.gov.sg/monetary-policy/consumer-price-developments).

Let’s read the file into `R`.

``` r
library(tidyverse)
library(pdftools)
txt <- pdf_text("../data/wk3_cpi2023.pdf")
```

## CPI for key categories (Page 7 on PDF)

We first extract the table on Page 7. This will give us the **CPI for
key categories, with 2019 as the base year**.

``` r
# Table on page 7
tab1 <- txt[7]

# Split text file into rows
rows <- str_split(tab1, "\\n+", simplify = TRUE) %>% str_trim()
  
# Column names in lines 4-6
names1 <- rows[4] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE)
names2 <- rows[5] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  # adding necessary strings
  append(c("", ""), after = 0) %>%
  append(c("", ""), after = 4)
names3 <- rows[6] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  # adding necessary strings
  append(c("", ""), after = 0) %>%
  append(c("", "", ""), after = 3) %>%
  append(c("", "", ""), after = 7)
colnames <- str_c(names1, names2, names3, sep = " ") %>% 
  str_replace_all("\\s", "") %>%
  append("Month", after = 0)

# Final table on page 7
rows[8:20] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame() %>%
  setNames(colnames) %>%
  mutate_at(-1, parse_number) -> tab1

tab1
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["Month"],"name":[1],"type":["chr"],"align":["left"]},{"label":["AllItems"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["MASCore"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["CPILessAccommodation"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CPILessOOA"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Food"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Services"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Retail&OtherGoods"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Electricity&Gas"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["PrivateTransport"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Accommodation"],"name":[11],"type":["dbl"],"align":["right"]}],"data":[{"1":"2022 Dec","2":"111.186","3":"107.316","4":"111.790","5":"111.609","6":"112.182","7":"106.111","8":"100.103","9":"115.666","10":"135.911","11":"109.040"},{"1":"2023 Jan","2":"111.397","3":"108.195","4":"112.313","5":"111.733","6":"113.743","7":"106.669","8":"100.861","9":"115.037","10":"134.513","11":"108.142"},{"1":"Feb","2":"112.019","3":"108.173","4":"112.622","5":"112.444","6":"113.818","7":"106.270","8":"101.676","9":"115.321","10":"136.608","11":"109.876"},{"1":"Mar","2":"112.583","3":"108.441","4":"113.233","5":"113.035","6":"114.243","7":"106.424","8":"101.858","9":"116.237","10":"139.066","11":"110.274"},{"1":"Apr","2":"112.669","3":"108.856","4":"114.019","5":"113.058","6":"114.636","7":"107.085","8":"102.214","9":"113.105","10":"141.854","11":"107.873"},{"1":"May","2":"113.034","3":"108.957","4":"113.615","5":"113.422","6":"115.061","7":"107.072","8":"101.918","9":"113.890","10":"138.723","11":"110.971"},{"1":"Jun","2":"113.576","3":"109.163","4":"114.219","5":"114.002","6":"115.101","7":"107.513","8":"101.811","9":"113.784","10":"141.475","11":"111.293"},{"1":"Jul","2":"113.309","3":"109.383","4":"114.202","5":"113.562","6":"115.293","7":"107.744","8":"101.976","9":"114.498","10":"140.174","11":"110.139"},{"1":"Aug","2":"114.303","3":"109.465","4":"114.845","5":"114.685","6":"115.479","7":"107.829","8":"101.874","9":"114.415","10":"143.849","11":"112.378"},{"1":"Sep","2":"114.880","3":"109.602","4":"115.495","5":"115.312","6":"115.649","7":"108.146","8":"101.423","9":"114.664","10":"147.267","11":"112.694"},{"1":"Oct","2":"115.111","3":"110.033","4":"116.226","5":"115.521","6":"115.855","7":"108.610","8":"101.832","9":"117.147","10":"149.612","11":"111.151"},{"1":"Nov","2":"114.910","3":"110.154","4":"115.369","5":"115.223","6":"116.248","7":"108.809","8":"101.242","9":"117.157","10":"143.478","11":"113.281"},{"1":"Dec","2":"115.343","3":"110.866","4":"115.844","5":"115.684","6":"116.340","7":"110.241","8":"101.191","9":"117.168","10":"142.681","11":"113.562"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

## YOY inflation (Page 8)

Using a similar approach, we can extract data on Page 8. This gives us
the **year-on-year inflation on key CPI categories**.

``` r
# Table on page 8
tab2 <- txt[8]

# Split text file into rows
rows <- str_split(tab2, "\\n+", simplify = TRUE) %>% str_trim()
# Same column names as the previous table
colnames
```

    ##  [1] "Month"                "AllItems"             "MASCore"             
    ##  [4] "CPILessAccommodation" "CPILessOOA"           "Food"                
    ##  [7] "Services"             "Retail&OtherGoods"    "Electricity&Gas"     
    ## [10] "PrivateTransport"     "Accommodation"

``` r
# Final table on page 8
rows[7:19] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame() %>%
  setNames(colnames) %>%
  mutate_at(-1, parse_number) -> tab2

tab2
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["Month"],"name":[1],"type":["chr"],"align":["left"]},{"label":["AllItems"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["MASCore"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["CPILessAccommodation"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CPILessOOA"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Food"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Services"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Retail&OtherGoods"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Electricity&Gas"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["PrivateTransport"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Accommodation"],"name":[11],"type":["dbl"],"align":["right"]}],"data":[{"1":"2022 Dec","2":"6.5","3":"5.1","4":"6.9","5":"6.7","6":"7.5","7":"3.7","8":"2.8","9":"16.5","10":"15.5","11":"4.7"},{"1":"2023 Jan","2":"6.6","3":"5.5","4":"7.1","5":"6.9","6":"8.1","7":"4.2","8":"3.3","9":"11.5","10":"14.3","11":"5.0"},{"1":"Feb","2":"6.3","3":"5.5","4":"6.7","5":"6.5","6":"8.1","7":"3.9","8":"3.8","9":"12.1","10":"12.1","11":"4.9"},{"1":"Mar","2":"5.5","3":"5.0","4":"5.7","5":"5.6","6":"7.7","7":"3.4","8":"3.3","9":"12.2","10":"8.6","11":"4.8"},{"1":"Apr","2":"5.7","3":"5.0","4":"6.0","5":"5.9","6":"7.1","7":"4.3","8":"2.9","9":"2.7","10":"10.4","11":"4.9"},{"1":"May","2":"5.1","3":"4.7","4":"5.2","5":"5.1","6":"6.8","7":"3.9","8":"2.8","9":"3.3","10":"7.2","11":"4.7"},{"1":"Jun","2":"4.5","3":"4.2","4":"4.5","5":"4.5","6":"5.9","7":"3.6","8":"2.7","9":"3.1","10":"5.8","11":"4.5"},{"1":"Jul","2":"4.1","3":"3.8","4":"4.0","5":"4.0","6":"5.3","7":"3.6","8":"2.6","9":"-1.6","10":"4.8","11":"4.6"},{"1":"Aug","2":"4.0","3":"3.4","4":"3.9","5":"3.9","6":"4.8","7":"3.1","8":"2.0","9":"-1.4","10":"6.3","11":"4.4"},{"1":"Sep","2":"4.1","3":"3.0","4":"4.1","5":"4.1","6":"4.3","7":"3.1","8":"0.9","9":"-1.4","10":"8.5","11":"4.3"},{"1":"Oct","2":"4.7","3":"3.3","4":"4.9","5":"4.9","6":"4.1","7":"3.4","8":"1.6","9":"1.8","10":"11.7","11":"4.2"},{"1":"Nov","2":"3.6","3":"3.2","4":"3.4","5":"3.4","6":"4.0","7":"3.5","8":"1.0","9":"1.5","10":"4.2","11":"4.1"},{"1":"Dec","2":"3.7","3":"3.3","4":"3.6","5":"3.7","6":"3.7","7":"3.9","8":"1.1","9":"1.3","10":"5.0","11":"4.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

**The following code plots the year-on-year inflation.**

``` r
tab2 %>%
  select(1:3) %>%
  separate(Month, into = c("year", "month"), fill = "left") %>%
  mutate(year = case_when(
    is.na(year) ~ "2023",
    TRUE ~ year
  )) %>%
  mutate(yr_month = ym(paste(year, month, sep = "-"))) %>%
  pivot_longer(AllItems:MASCore, names_to = "category", values_to = "inflation") %>%
  select(yr_month, category, inflation) -> tab3

tab4 <- tab3 %>% filter(yr_month == "2023-12-01")
range <-  c(as.Date("2022-12-01"), as.Date("2024-01-01"))
```

``` r
library(ggthemes)
ggplot(data = tab3, aes(x = yr_month, y = inflation, color = category)) +
  geom_line(lwd = 2) +
  geom_text(data = tab4, aes(label = paste0(inflation, "%")), 
            size = 4.5, hjust = "left", nudge_x = 6, show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b", limits = range) +
  scale_color_manual(values = c("darkgray", "steelblue")) +
  labs(x = "", y = "% YOY", color = "", 
       title = "MAS Core and CPI-All Items Inflation") +
  theme_fivethirtyeight()
```

![](03-workshop_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
