

#esa journals

library(tidyverse)

setwd("G:\\中美生态学发展比较\\数据源\\英文源")

dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
 read_csv(filename[[i]]) %>% select('Link','Year','Author Keywords','Source title') -> a
 eco_raw %>% bind_rows(a) -> eco_raw
}

eco_raw %>% 
 rename(link='Link',year='Year',keyword='Author Keywords',journal='Source title') %>%
 drop_na(keyword) %>%
 filter(year >= 1988,year<=2017) %>%
 filter(journal != "J. ECOL.") %>%
 distinct() %>%
 mutate(journal = str_to_upper(journal)) %>%
 mutate(journal = str_replace(journal,"ECOLOGICAL APP.+","ECOLOGICAL APPLICATIONS")) -> eco  

eco$journal %>% as.factor %>% summary

eco->esa

nrow(esa)

#统计每年文章总数
esa %>%
 count(year,journal) %>%
 mutate(year=as.factor(year)) %>%
 ggplot(aes(x=year,y=n,fill=journal)) +
 geom_bar(stat="identity") +
 ylab("no. of article")
 

setwd("G:\\中美生态学发展比较\\数据源")
write_csv(esa,"esa.csv")

#选取了1988-2017年ESA旗下5个杂志，共15706篇文章.4个杂志，则一共有12302篇文章。

#分词
esa %>% 
 tidytext::unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";") %>% 
 mutate(word=str_trim(word)) %>% 
 #mutate(word = str_replace(word,"\\(.*\\)","")) %>%
 filter(word != "")-> tidy

write_csv(tidy,"tidy_en.csv")


