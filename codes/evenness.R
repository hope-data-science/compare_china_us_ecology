
setwd("G:\\中美生态学发展比较\\数据源")
library(tidyverse)
library(tidytext)
library(vegan)

read.csv("tidy_cn.csv") %>% select(-1) %>% as_tibble()-> esc
read_csv("tidy_en.csv") -> esa

############diversity
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

bind_rows(t1,t2) -> div.journal
bind_rows(esc.t,esa.t) -> div.intercept
##############
##########richness

esc %>%
  select(-link) %>%
  group_by(journal,year) %>%
  distinct(word)%>%
  count(journal,year) %>%
  group_by(journal) %>%
  summarise(avg = mean(n)) %>%
  mutate(country="China") -> t1

esc %>%
  select(-link,-journal) %>%
  distinct(year,word) %>%
  count(year) %>%
  summarise(avg=mean(n)) %>%
  mutate(country="China") -> esc.t 

esa %>%
  select(-link) %>%
  group_by(journal,year) %>%
  distinct(word)%>%
  count(journal,year) %>%
  group_by(journal) %>%
  summarise(avg = mean(n)) %>%
  mutate(country="US") -> t2

esa %>%
  select(-link,-journal) %>%
  distinct(year,word) %>%
  count(year) %>%
  summarise(avg=mean(n)) %>%
  mutate(country="US") -> esa.t 

bind_rows(t1,t2) -> richness.journal
bind_rows(esc.t,esa.t) -> rich.intercept

##########
########bind them
div.intercept %>%
  rename(div=avg) %>%
  left_join(rich.intercept) %>%
  rename(rich=avg)%>%
  mutate(evenness=div/log(rich))  %>%
  rename(avg=evenness)-> even.intercept

div.journal %>%
  rename(div=avg) %>%
  left_join(rich.intercept) %>%
  rename(rich=avg)%>%
  mutate(evenness=div/log(rich)) %>%
  rename(avg=evenness)-> even.journal

even.journal %>%
  ggplot(aes(x=fct_reorder(journal,avg),y=avg,colour=country)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  xlab("")+ ylab("keyword evenness") +
  geom_hline(aes(yintercept=avg,colour=country),data=even.intercept,linetype="dashed",size=1)+
  coord_flip()->p7
#############################yearly
#########################
esc %>%
  select(-link) %>%
  group_by(year,journal) %>%
  distinct(word) %>%
  count(year,journal) %>%
  rename(avg=n) %>%
  mutate(country="China") -> cn.year.journal

esc %>%
  select(-link,-journal) %>%
  distinct(year,word) %>%
  count(year) %>%
  rename(avg=n) %>%
  mutate(country="China") -> cn.year.total

esa %>%
  select(-link) %>%
  group_by(year,journal) %>%
  distinct(word) %>%
  count(year,journal) %>%
  rename(avg=n) %>%
  mutate(country="US") -> en.year.journal

esa %>%
  select(-link,-journal) %>%
  distinct(year,word) %>%
  count(year) %>%
  rename(avg=n) %>%
  mutate(country="US") -> en.year.total

bind_rows(cn.year.journal,en.year.journal) -> rich.year
bind_rows(cn.year.total,en.year.total) -> rich.intercept


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

bind_rows(cn.year.journal,en.year.journal) %>% mutate(year=as.integer(year)) -> div.year
bind_rows(cn.year.total,en.year.total) %>% mutate(year=as.integer(year)) -> div.intercept

div.intercept %>%
  rename(div=avg) %>%
  left_join(rich.intercept) %>%
  rename(rich=avg)%>%
  mutate(evenness=div/log(rich))  %>%
  rename(avg=evenness)-> even.intercept

div.year %>%
  rename(div=avg) %>%
  left_join(rich.year) %>%
  rename(rich=avg)%>%
  mutate(evenness=div/log(rich)) %>%
  rename(avg=evenness)-> even.year

even.year %>% 
  ggplot() +
  geom_line(aes(x=year,y=avg,colour=country,group=journal),linetype="dashed",size=1)+
  geom_point(aes(x=year,y=avg,colour=country,shape=journal),size=3)+
  geom_line(aes(x=year,y=avg,colour=country),data=even.intercept,size=1)+
  geom_point(aes(x=year,y=avg,colour=country),data=even.intercept,pch=19,size=2)+
  scale_shape_manual(values=1:25)+
  ylab("keyword evenness") -> p8


