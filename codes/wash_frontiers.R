
setwd("G:\\中美生态学发展比较\\数据源")
library(tidyverse)

read_csv("tidy_en.csv") -> tidy

write_csv(tidy,"tidy_en2.csv")

tidy %>%
  count(journal,link) %>%
  group_by(journal) %>%
  filter(journal=="FRONTIERS IN ECOLOGY AND THE ENVIRONMENT",n==1) %>%
  ungroup() %>%
  pull(link) -> delete 

tidy %>%
  filter(!(link %in% delete)) -> new

write_csv(new,"tidy_en.csv")

