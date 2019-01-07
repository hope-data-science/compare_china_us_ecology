library(rvest)
library(tcltk)
library(tidyverse)

setwd("G:\\中美生态学发展比较\\数据源\\中文源")


c("http://www.cqvip.com/QK/90772X/201723/","http://www.cqvip.com/QK/90772X/201724/") -> link1

vector() -> links
data.frame() -> alldata
is.error = function(x) inherits(x,"try-error")



pb <- tkProgressBar("进度","已完成 %", 0, 100) 

for(i in 1:length(link1))  #1:length(link1)
{
  repeat
  {
    try({
      link1[i] %>% read_html() -> a
    },silent=T)
    ifelse(is.error(a),Sys.sleep(2),break)
  }
  
  a%>% html_nodes("li em.titles a") %>% html_attr(name="href") ->L2_link
  str_c("http://www.cqvip.com",L2_link) -> link2
  c(links,link2) -> links
  info<- sprintf("%d / %d 已完成 %d%%", i,length(link1),round(i*100/length(link1)))  
  setTkProgressBar(pb, i*100/length(link1), sprintf("进度 (%s)", info),info) 
  
  Sys.sleep(0.3)
}

close(pb)

links %>% unique -> links 


pb <- tkProgressBar("进度","已完成 %", 0, 100) 
for(i in 1:length(links))
{
 
   repeat
   {
    try({
     links[i] %>% read_html() -> a
     },silent=T)
    ifelse(is.error(a),Sys.sleep(2),break)
   }	   
			   
  #链接
  links[i] -> link
	
  #题目
  a %>% html_nodes("h1") %>% html_text() -> title

  #摘要
  a %>% html_nodes("tr")%>%html_text %>% str_subset("摘　要") %>% `[`(length(.)) %>%
   str_replace_all("\r","") %>% str_replace_all("\n","") %>% str_replace_all("\t","") %>% str_trim() %>%
   str_replace_all("摘　要：","") -> abstract
 
  #关键词
  a %>% html_nodes("tr")%>%html_text %>% str_subset("【关键词】") %>% `[`(length(.)) %>%
   str_replace_all("\r","") %>% str_replace_all("\n","") %>% str_replace_all("\t","") %>% str_trim() %>%
   str_replace_all("【关键词】","") %>% str_replace_all(" ",",")-> keyword

  #出处
  a %>% html_nodes("tr")%>%html_text %>% str_subset("【出　处】") %>% `[`(length(.)) %>%
   str_replace_all("\r","") %>% str_replace_all("\n","") %>% str_replace_all("\t","") %>%
   str_replace_all("【出　处】","") %>% str_trim() -> source

  #分类
  a %>% html_nodes("tr")%>%html_text %>% str_subset("【分　类】") %>% `[`(length(.)) %>%
   str_replace_all("\r","") %>% str_replace_all("\n","") %>% str_replace_all("\t","") %>%
   str_replace_all("【分　类】","") %>% str_trim() -> category

  #作者
  a %>% html_nodes("strong i a[href*='aspx?w=']") %>% html_text %>% str_c(collapse=",") -> authorlist

  #单位
  a %>% html_nodes("strong i a[href*='aspx?o=']") %>% html_text %>% str_c(collapse=",") -> institute
  
  #年份
  a %>% html_nodes("strong i a") %>% `[`(1) %>% html_text %>% str_extract(".+年") %>% str_sub(start=-5,end=-2) -> year
  
  list(link,title,abstract,keyword,source,category,authorlist,institute,year)->b  
  b[lapply(b,length)==0]=NA
  b[lapply(b,length)>1]=str_c(b[lapply(b,length)>1],collapse="##")
  as.data.frame(b,fix.empty.names=F)->newb

  rbind(alldata,newb)->alldata
 #########################################################################注意更改 
  write.csv(alldata,"current.csv")   #delete afterwards
  
  #显示进度条
   info<- sprintf("%d / %d 已完成 %d%%", i,length(links),round(i*100/length(links)))  
   setTkProgressBar(pb, i*100/length(links), sprintf("进度 (%s)", info),info) 
   
  #插入时滞
   Sys.sleep(0.3) #0.75
}

close(pb)

alldata %>% unique -> alldata
colnames(alldata)=c("link","title","abstract","keyword","source","category","authorlist","institute","year")
######################################################################注意更改#######################################################
write.csv(alldata,"生态学报2017增补.csv")
