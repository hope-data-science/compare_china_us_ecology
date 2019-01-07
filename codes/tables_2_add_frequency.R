
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(tidytext)
library(quanteda)
library(widyr)
library(data.table)

setwd("G:\\中美生态学发展比较\\数据源")

fread("tidy_cn.csv") %>% select(-1) %>% as_tibble()-> esc
fread("tidy_en.csv") %>% as_tibble() -> esa

get_freq = function(dt,start,end){
  dt %>%
    filter(year>=start,year<=end) %>%
    count(word) %>%
    rename(freq = n)
}

esa %>% count(word) %>% rename(freq=n) -> esa.freq

esa %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(esa.freq) %>%
  as_tibble %>%
  #select(-freq) %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  filter(word != "usa") %>%      #usa is filtered out
  top_n(20,degree) -> esa.all

esa %>%
  filter(year>=1988,year<=1997) %>%
  pairwise_count(word,link,upper=F) %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esa,1988,1997)) %>%
  as_tibble %>%
  #select(-freq) %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(10,degree) -> esa1

esa %>%
  filter(year>=1998,year<=2007) %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esa,1998,2007)) %>%
  as_tibble %>%
  #select(-freq) %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(10,degree) -> esa2

esa %>%
  filter(year>=2008,year<=2017) %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esa,2008,2017)) %>%
  as_tibble %>%
  #select(-freq) %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  filter(word != "usa") %>%      #usa is filtered out
  top_n(10,degree) -> esa3

bind_cols(esa1,esa2,esa3) -> esa.year

#esa.all,esa.year are the final tables we want

read.csv("translation2.csv") %>%
  select(-1) %>% 
  as_tibble() %>%
  mutate_all(funs(as.character(.)))-> trans

#esc->esc1
#esc1 %>% mutate(word=as.character(word),link=as.character(link)) -> esc

esc %>% count(word) %>% rename(freq=n) -> esc.freq

esc %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(esc.freq) %>%
  as_tibble %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(20,degree) -> esc.all

esc %>%
  filter(year>=1988,year<=1997) %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F) %>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esc,1988,1997)) %>%
  as_tibble %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(10,degree) -> esc1

esc %>%
  filter(year>=1998,year<=2007) %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esc,1998,2007)) %>%
  as_tibble %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(10,degree) -> esc2

esc %>%
  filter(year>=2008,year<=2017) %>%
  pairwise_count(word,link,upper=F)  %>%                              
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(get_freq(esc,2008,2017)) %>%
  as_tibble %>%
  mutate(degree=as.integer(degree)) %>%
  arrange(desc(degree)) %>%
  top_n(10,degree) -> esc3

esc.all %>% 
  left_join(trans) %>%
  select(word.en,degree,freq) %>%
  rename(word=word.en) -> esc.all.f

esc1%>% 
  left_join(trans) %>%
  select(word.en,degree,freq) %>%
  rename(word=word.en) -> esc1.f

esc2 %>% 
  left_join(trans) %>%
  select(word.en,degree,freq) %>%
  rename(word=word.en) -> esc2.f

esc3 %>% 
  left_join(trans) %>%
  select(word.en,degree,freq) %>%
  rename(word=word.en) -> esc3.f

bind_cols(esc1.f,esc2.f,esc3.f) -> esc.year

#esc.all.f,esc.year are the final tables we want

##export
bind_cols(esa.all,esc.all.f) -> all.30

all.30 %>% write_csv("table_all.csv")
esa.year %>% write_csv("table_esa.csv")
esc.year %>% write_csv("table_esc.csv")

#results
esa.all -> a
esc.all.f -> b
inner_join(a,b,by="word")
anti_join(a,b,by="word")
anti_join(b,a,by="word")

anti_join(esa2,esa1,by="word")
anti_join(esa3,esa2,by="word")

anti_join(esc2.f,esc1.f,by="word")
anti_join(esc3.f,esc2.f,by="word")


##discussion
##############################  
esa.net %>%
  activate(nodes) %>%
  as_tibble %>%
  top_n(20,degree) ->a

esa.net %>%
  activate(nodes) %>%
  as_tibble %>%
  top_n(20,freq) -> b

anti_join(a,b)
anti_join(b,a)
##############################




