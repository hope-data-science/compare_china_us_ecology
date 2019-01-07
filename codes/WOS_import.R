
setwd("G:\\中美生态学发展比较\\数据源\\英文源\\raw")

library(bibliometrix)
library(tidyverse)

dir() -> filenames


#D <- readFiles("1.txt")
tibble() -> all

for(i in seq(filenames)){
  readFiles(filenames[i]) %>%
   convert2df(dbsource = "isi", format = "plaintext") %>%
   select(PY,DE,SO,ID) %>%
   bind_rows(all) ->all
}

#M <- convert2df(D, dbsource = "isi", format = "plaintext")
# ?convert2df


setwd("G:\\中美生态学发展比较\\数据源")
write_csv(all,"ECO_WOS.csv")
