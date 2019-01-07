
setwd("G:\\中美生态学发展比较\\数据源")
library(tidyverse)

read.csv("tidy_cn.csv") %>% select(-1) %>% as_tibble()-> esc
read_csv("tidy_en.csv") -> esa

########paper volume

esc %>%
  group_by(year,journal) %>%
  distinct(link) %>%
  count(year,journal) %>% 
  rename(avg=n) %>%
  mutate(country="China") -> cn.year.journal

esa %>%
  group_by(year,journal) %>%
  distinct(link) %>%
  count(year,journal) %>% 
  rename(avg=n) %>%
  mutate(country="US") -> en.year.journal

bind_rows(cn.year.journal,en.year.journal) -> year.journal.volume


#richness
esc %>%
  select(-link) %>%
  group_by(year,journal) %>%
  distinct(word) %>%
  count(year,journal) %>%
  rename(avg=n) %>%
  mutate(country="China") -> cn.year.journal

esa %>%
  select(-link) %>%
  group_by(year,journal) %>%
  distinct(word) %>%
  count(year,journal) %>%
  rename(avg=n) %>%
  mutate(country="US") -> en.year.journal

bind_rows(cn.year.journal,en.year.journal) -> year.journal.richness

year.journal.richness %>%
  rename(richness=avg) %>%
  left_join(year.journal.volume) %>%
  rename(volume=avg) %>%
  ggplot(aes(x=volume,y=richness,colour=country)) +
   geom_point()+geom_smooth(method="lm",se=F) +
  xlab("paper volume") +
  ylab("keyword richness")
  
