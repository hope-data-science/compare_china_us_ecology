
setwd("G:\\中美生态学发展比较\\数据源")
library(tidyverse)
library(tidytext)
library(vegan)

read.csv("tidy_cn.csv") %>% select(-1) %>% as_tibble()-> esc
read_csv("tidy_en.csv") -> esa

#计算平均关键词数量

esc %>%
  group_by(year,journal) %>%
  count(word) %>% 
  unite(col="yj",year,journal) %>%
  cast_dtm(yj,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="yj",value="avg") %>%
  separate(yj,sep="_",into=c("year","journal"))%>%
  group_by(journal)%>%
  summarise(avg=mean(avg)) %>%
  mutate(country="China") -> t1

esc %>%
  group_by(year) %>%
  count(word) %>% 
  cast_dtm(year,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="year",value="avg") %>%
  summarise(avg=mean(avg)) %>%
  mutate(country="China")  -> esc.t 

esa %>%
  group_by(year,journal) %>%
  count(word) %>% 
  unite(col="yj",year,journal) %>%
  cast_dtm(yj,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="yj",value="avg") %>%
  separate(yj,sep="_",into=c("year","journal"))%>%
  group_by(journal)%>%
  summarise(avg=mean(avg)) %>%
  mutate(country="US") -> t2

esa %>%
  group_by(year) %>%
  count(word) %>% 
  cast_dtm(year,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="year",value="avg") %>%
  summarise(avg=mean(avg)) %>%
  mutate(country="US") -> esa.t 

bind_rows(t1,t2) -> even.journal
bind_rows(esc.t,esa.t) -> intercept

even.journal %>%
 ggplot(aes(x=fct_reorder(journal,avg),y=avg,colour=country)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  xlab("")+ ylab("keyword diversity") +
  geom_hline(aes(yintercept=avg,colour=country),data=intercept,linetype="dashed",size=1)+
  coord_flip() -> p9

########

#each year

esc %>%
  group_by(year,journal) %>%
  count(word) %>% 
  unite(col="yj",year,journal) %>%
  cast_dtm(yj,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="yj",value="avg") %>%
  separate(yj,sep="_",into=c("year","journal")) %>%
  mutate(country="China") -> cn.year.journal

esc %>%
  group_by(year) %>%
  count(word) %>% 
  cast_dtm(year,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="year",value="avg") %>%
  mutate(country="China") -> cn.year.total

esa %>%
  group_by(year,journal) %>%
  count(word) %>% 
  unite(col="yj",year,journal) %>%
  cast_dtm(yj,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="yj",value="avg") %>%
  separate(yj,sep="_",into=c("year","journal")) %>%
  mutate(country="US") -> en.year.journal

esa %>%
  group_by(year) %>%
  count(word) %>% 
  cast_dtm(year,word,n) %>%
  diversity(index = "shannon") %>%
  enframe(name="year",value="avg") %>%
  mutate(country="US") -> en.year.total

bind_rows(cn.year.journal,en.year.journal) %>% mutate(year=as.integer(year)) -> year.journal
bind_rows(cn.year.total,en.year.total) %>% mutate(year=as.integer(year)) -> year.total

year.journal %>% 
  ggplot() +
  geom_line(aes(x=year,y=avg,colour=country,group=journal),linetype="dashed",size=1)+
  geom_point(aes(x=year,y=avg,colour=country,shape=journal),size=3)+
  geom_line(aes(x=year,y=avg,colour=country),data=year.total,size=1)+
  geom_point(aes(x=year,y=avg,colour=country),data=year.total,pch=19,size=2)+
  scale_shape_manual(values=1:25)+
  ylab("keyword diversity") -> p10
 
