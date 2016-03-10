# qmap(location = "sanfransisco")  
# qmap(location = "boston university", zoom = 14)  
# qmap(location = "boston university", zoom = 14, source = "osm") 

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)
train <- read_csv('C:/Users/harini/Downloads/train.csv (1)/train.csv')
sfMap <- qmap("San Francisco", zoom = 12, color = "bw")
#Map <- readRDS('')
map<-get_map(location="sanfrancisco",zoom=12,source="osm")
map
ggmap(map)
counts <- summarise(group_by(train, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]
top12 <- train[train$Category %in% counts$Category[c(1,2:13)],]
p <- ggmap(map) +
  geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Top Crimes in San Francisco") +
  theme_light(base_size=20) 
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")
getwd()
