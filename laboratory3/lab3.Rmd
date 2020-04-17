## Исследование возможностей автоматизации сбора данных о доменах.
### Цель:
Собрать информацию о топ 15 доменах в категорий E-Books.  

### Используемое ПО:
1.Rstudio IDE    
2.nmap      
3.dig        
4.whois   
5.whatweb

### Варианты решения задачи:
1.Собрать информацию вручную с помощью веб-браузера, инструментов whois, dig, nmap и т.д.    
2.Использовать интегрированные инструменты такие как SpiderFoot, Maltego CE, Datasploit, Recon-ng.    
3.Самостоятельно разработать (для образовательных целей) автоматизированное решение для сбора информации.    

### Содержание лабораторной работы:
В данной лабораторной работе используется 1 вариант решения задачи. Соберем необходимую информацию автоматизированным способом.

```{r cache=TRUE}
library(tidyverse)
get_sum_df <- function(company_url) {
  country_state <- NA
  dig <- system2('dig', company_url, stdout = TRUE)
  ip <- dig %>%
    grep(pattern = company_url, value = TRUE) %>%
    str_extract(pattern = "\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b")
  ip <- ip[!is.na(ip)]
 
  whois <- system2('whois', ip[1], stdout = TRUE)
  phones <- whois %>%
    grep(pattern = "Phone", value = TRUE, ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ") %>%
    data.table::transpose() %>%
    .[[2]] %>%
    unique() %>%
    str_c(collapse = " ")
 
  netblock <- whois %>%
    grep(pattern = "CIDR", value = TRUE, ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ", simplify = TRUE) %>%
    .[-1] %>%
    str_c(collapse = " ")
 
  country <- whois %>%
    grep(pattern = "Country",
         value = TRUE,
         ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ", simplify = TRUE) %>%
    .[-1]
 
  country_state <- whois %>%
    grep(pattern = "State",
         value = TRUE,
         ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ", simplify = TRUE) %>%
    .[-1]
  if(length(country_state)==0) country_state <- NA
 
  address <- whois %>%
    grep(pattern = "address",
         value = TRUE,
         ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ", simplify = TRUE) %>%
    .[-1] %>%
    str_c(collapse = " ")
 
  hosting <- whois %>%
    grep(pattern = "Hosting",
         value = TRUE,
         ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ")
  hosting <- lapply(hosting, collapse = " ", str_c) %>%
    str_c(collapse = " ")
 
  nmap <-
    system2('nmap',
            args = c('-p', '22,21,80,443', ip[1]),
            stdout = TRUE)
  ports <- nmap %>%
    grep(pattern = "open",
         value = TRUE,
         ignore.case = TRUE) %>%
    str_squish() %>%
    str_split(pattern = " ") %>%
    data.table::transpose() %>%
    .[[1]] %>%
    str_c(collapse = " ")
  ip <- str_c(ip,collapse = ' ')
 
  company_sum <-
    data.frame(
      csum = c(
        company_url,
        ip,
        netblock,
        country,
        country_state,
        address,
        phones,
        hosting,
        ports
      ),
      row.names = c(
        'company_url',
        'ip',
        'netblock',
        'country',
        'country_state',
        'address',
        'phones',
        'hosting',
        'ports'
      )
    )
  company_sum
 
}
 
urls <- c("Gutenberg.org", "Manybooks.net", "Bookboon.com", "Freebookcentre.net", "Kobobooks.com", "Obooko.com", "Planetebook.com","Bookgorilla.com", "Idpf.org", "Fbreader.org", "Freetechbooks.com", "Isilo.com", "Ebookmaestro.com", "Freeebooknetwork.com", "Jsfiddle.net")
 
dfs <- lapply(urls, get_sum_df) 
result <- bind_cols(dfs) 
 
row.names(result) <- c('company_url',
        'ip',
        'netblock',
        'country',
        'country_state',
        'address',
        'phones',
        'hosting',
        'ports'
      )
colnames(result) <- map(result[1,],as.character) %>% unlist()
result <- result[-1,]
knitr::kable(result)
```

Отдельно соберем информацию о веб-технологиях, так как rappalyzer использует непосредственно формат rappalyzer (до этого использовали DataFrame) и самостоятельно строит таблицы.

```{r cache=TRUE}
library(rappalyzer)
rappalyze("Gutenberg.org")
rappalyze("Manybooks.net")
rappalyze("Bookboon.com")
rappalyze("Freebookcentre.net")
rappalyze("Kobobooks.com")
rappalyze("Obooko.com")
rappalyze("Planetebook.com")
rappalyze("Bookgorilla.com")
rappalyze("Idpf.org")
rappalyze("Fbreader.org")
rappalyze("Freetechbooks.com")
rappalyze("Isilo.com")
rappalyze("Ebookmaestro.com")
rappalyze("Freeebooknetwork.com")
rappalyze("Jsfiddle.net")
```

### Оценка результата.
В результате выполнения задачи, нами было получено достаточно универсальное решение по сбору информации о доменах.    

### Вывод.    
В ходе лабораторной работы мы научились автоматизированно собирать информацию о доменах. Теперь эта рутинная и долгая работа выполняется в пару кликов.