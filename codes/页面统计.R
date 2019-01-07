
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
  select(source) %>%
  mutate(page=str_extract(source,"共.+页")) %>%
  mutate(page.no=str_extract(page,"[0-9]+")) %>%
  mutate(page.no=as.numeric(page.no))->a

mean(a$page.no) #中文杂志平均页面数量为7.1（7.094）



#esa journals


setwd("G:\\中美生态学发展比较\\数据源\\英文源")

dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
  read_csv(filename[[i]]) %>% select('Link','Page start','Page end') -> a
  eco_raw %>% bind_rows(a) -> eco_raw
}

eco_raw %>% 
  na.omit %>%
  rename(link='Link',ps='Page start',pe='Page end') %>%
  distinct() %>%
  mutate(page.no=as.numeric(pe)-as.numeric(ps)) -> b

mean(b$page.no) #英文杂志平均页面数量为11.7（11.72）
