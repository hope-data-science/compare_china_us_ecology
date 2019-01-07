
library(tidyverse)
library(tidytext)

#不能用read_csv,write_csv，不然会有乱码

setwd("G:\\中美生态学发展比较\\数据源\\中文源")

dir() -> files

as_tibble()->alldata

for(i in 1:length(files))
{
 read.csv(files[i],stringsAsFactors=F)->newfile
 newfile %>% 
  select(-1) %>%
  mutate(journal=str_replace(files[i],".csv","")) %>%
  select(journal,everything())  -> new.data.frame
 alldata=bind_rows(alldata,new.data.frame)
}

alldata %>% 
  select(1:10) %>% 
  na.omit %>% 
  unique %>% 
  select(-title,-abstract,-source,-authorlist,-institute,-category) %>%
  filter(year >= 1988,year<=2017) %>%
  mutate(journal = str_replace(journal,"生态学报2017增补","生态学报"))-> esc


esc$journal %>% as.factor %>% summary

nrow(esc)

#选取了1988-2017年中国生态学会旗下3个杂志，共28756篇文章
setwd("G:\\中美生态学发展比较\\数据源")

esc %>% 
  tidytext::unnest_tokens(word, keyword, token = stringr::str_split, pattern = ",") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "")-> tidy

write.csv(esc,"esc.csv")
write.csv(tidy,"tidy_cn.csv")   

##############

setwd("G:\\中美生态学发展比较\\数据源")
read.csv("tidy_cn2.csv") %>% select(-1) %>% as_tibble()-> tidy

tidy %>%
  mutate(journal = str_replace(journal,"应用生态学报","CHINESE JOURNAL OF APPLIED ECOLOGY")) %>%
  mutate(journal = str_replace(journal,"生态学报","ACTA ECOLOGICAL SINICA")) %>%
  mutate(journal = str_replace(journal,"生态学杂志","CHINESE JOURNAL OF ECOLOGY")) -> tidy.cn 

write.csv(tidy,"tidy_cn2.csv")  
write.csv(tidy.cn,"tidy_cn.csv")


