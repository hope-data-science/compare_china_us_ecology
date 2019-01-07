
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(tidytext)
library(quanteda)
library(widyr)

setwd("G:\\中美生态学发展比较\\数据源")

read.csv("tidy_cn.csv") %>% select(-1) %>% as_tibble()-> esc
read_csv("tidy_en.csv") -> esa

esa %>% count(word) %>% rename(freq=n) -> esa.freq
  
esa %>%
  pairwise_count(word,link,upper=F)  %>%
  top_n(200) %>%                                  #500/200
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  left_join(esa.freq) -> esa.net

esa.net %>%
  top_n(10,degree) %>%
  pull(word) -> esa.top.degree

esa.net %>%
  top_n(10,freq) %>%
  pull(word) -> esa.top.freq

esa.net %>%
  mutate(degree.index=word %in% esa.top.degree) %>%
  mutate(freq.index=word %in% esa.top.freq) %>%
  mutate(index=degree.index & freq.index) -> esa.network

esa.network %>%
  ggraph(layout = "kk") +               #linear 'fr', 'kk', 'lgl', 'graphopt'
  geom_edge_link(aes(edge_width = weight), edge_colour = "skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = freq/50,colour=index)) +
  geom_node_text(aes(label = word,colour=index), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none") +
  labs(edge_width="weight",size="freq")+
  scale_color_manual(values=c("#000000","#FF0000"))

#guides(edge_width=F,size=F)
########################################################################

esc %>% count(word) %>% rename(freq=n) %>% mutate(id=1:nrow(.)) -> esc.freq
esc %>% left_join(esc.freq) -> new.esc

new.esc %>%
  pairwise_count(id,link,upper=F)  %>%
  top_n(200) %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  mutate(id=as.integer(name)) %>%
  select(-name)%>%
  mutate(degree = centrality_degree()) %>%
  left_join(esc.freq) -> esc.net

#esc.net %>% as_tibble() %>% pull(word) %>% paste(collapse = ",")
#translated from google
#translation="Biomass, n, Temperature sensitivity, Soil nutrient, Phosphorus, Carbon, Landscape pattern, Ecosystem services, Microorganisms, Soil organic carbon, Natural forest, Yangtze River estuary, Environmental factors, Biodiversity, Zooplankton, Community structure, Soil respiration , Climate change, salinity, photoperiod, soil enzyme, soil enzyme activity, biolog, soil, root exudates, temperature, forest community, Three Gorges reservoir area, nitrogen deposition, soil moisture, soil temperature, diversity, phytoplankton, Chinese fir , Water use efficiency, Gas exchange, Photosynthetic characteristics, Meteorological factors, Distribution pattern, Allelopathy, Yellow River delta, Forest age, Population structure, Age structure, Life table, Chlorophyll fluorescence, Co2, Seasonal changes, Ecological footprint, Actinomycetes , Maize, Genetic diversity, Spartina alterniflora, Alpine meadow, Sustainable development, Soil moisture, Broad-leaved Korean pine forest, Transpiration rate, Geographic information system, Gis, Sediment, Phytoremediation, Carbon storage, Modes, Space Association, alpha diversity, nitrate nitrogen, 16s, rice, photosynthesis, survival, methane, bacteria, seed germination, root system, growth, growth and development, cadmium, ch4, wheat, macrobenthos, spatial pattern, spatial distribution , Spatial variation, Spatial heterogeneity, Principal component analysis, Organic carbon, Heavy metals, Fertilizer, winter wheat, nitrogen application rate, rapd, wetland, yield, trophic level, species diversity, niche breadth, interspecific competition, intraspecific competition, seeds, antioxidant enzymes, fine roots, mangroves, no-tillage, reproduction, Nitrogen, natural enemies, total nitrogen, Xishuangbanna, precipitation, p, driving force, soil total nitrogen, plantation, urbanization, value assessment, landscape index, soil microbes, functional diversity, soil nematodes, plants, tree rings, precipitation , Fluctuation zone, Huaping area, stability, light intensity, canonical correspondence analysis, litter, moisture, sap flow, soil animal, community, loess plateau, survival curve, diurnal variation, gradient analysis, genetic structure, reed , Qinghai-Tibet Plateau, arbuscular mycorrhizal fungi, human activities, emergy analysis, Changbai Mountain, net photosynthetic rate, remote sensing, carbon density, ndvi, spatial distribution pattern, β diversity, ammonium nitrogen, abundance, rdna, soil moisture, Land use, brown planthopper, nitrous oxide, q10 value, density, productivity, paddy field, fungus, seedling growth, ecological carrying capacity, vegetation, compound pollution, species composition, n2o, secondary productivity, geostatistics, cluster analysis, whole Nitrogen, Mixed forest, Microsatellite, Intertidal, Intercropping, Bioavailability, Transpiration, Stomatal conductance, Genetic differentiation, Sanjiang Plain , peanut, stable isotope, variation function, soybean, grain yield, soil pollution, plant community, issr, cotton, niche overlap, static life table, competition index, germination, quality, summer corn, reactive oxygen species, rrna, allelochemicals , Humidity, East China Sea, Photosynthetic rate, q10, Root canal, Candida, Spring wheat, Straw mulch, Light, Development, PAH, Potassium, Lead, Cucumber, rs, Insect community, Arthropod, Zinc, Pest, Total phosphorus, Tropical Seasonal Rainforest, Verification"

#str_split(translation,",") %>% `[[`(1) %>% enframe(name="id",value = "word.en") -> trans

str_split(translation,",") %>% `[[`(1) -> trans

esc.net %>%
  top_n(10,degree) %>%
  pull(word) %>%
  as.character() -> esc.top.degree

esc.net %>%
  top_n(10,freq) %>%
  pull(word) %>%
  as.character()-> esc.top.freq

esc.net %>%
  mutate(word=as.character(word)) %>%
  mutate(degree.index=word %in% esc.top.degree) %>%
  mutate(freq.index=word %in% esc.top.freq) %>%
  mutate(index=degree.index & freq.index) %>%
  mutate(word.en=str_to_lower(trans)) %>%
  mutate(word.en=str_trim(word.en)) -> esc.network

esc.network %>%
  ggraph(layout = "kk") +               #linear 'fr', 'kk', 'lgl', 'graphopt'
  geom_edge_link(aes(edge_width = weight), edge_colour = "skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = freq/50,colour=index)) +
  geom_node_text(aes(label = word.en,colour=index), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none") +
  labs(edge_width="weight",size="freq")+
  scale_color_manual(values=c("#000000","#FF0000"))

esc.network %>%
  select(word,word.en)%>%
  write.csv("translation.csv")
