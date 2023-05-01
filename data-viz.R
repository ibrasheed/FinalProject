library(ggplot2)
library(dplyr)
library(wbstats)
library(RColorBrewer)
library(sf)
library(fields)



setwd("/Users/rasheed/Documents/GitHub/FinalProject")

##Scraping corruption data for 2021 ----
corruption <- wb_data("CC.EST", start_date = 2021)

## Importing African countries shapefile ----
country_map <- st_read("afshape/Africa_Boundaries.shp")

## Merging shapefile with corruption data
df_map <- merge(country_map, corruption, by.x= "ISO", by.y = "iso3c", all.x = T)
names(df_map)[names(df_map) == 'CC.EST'] <- 'Control'
df_map$Controln <- df_map$Control + 2.5
plot(as.data.frame(df_map), col = df_map$control)

my_palette <- colorRampPalette(c("red", "green"))
pdf(file = "controlmap-baseR.pdf")
plot(df_map$geometry, col = my_palette(50)[cut(df_map$Controln, breaks = 50)], max.plot = 13) 
legend("topright", 
       legend = c("Low", "High"),
       fill = my_palette(2),
       title = "Control of Corruption",
       box.lty = 0)

dev.off()


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~


##Plotting geospatial data with ggplot----

color_palette <- colorRampPalette(brewer.pal(6, "RdYlBu"))
pdf(file = "corruptionmap3.pdf")
ggplot(data=df_map) +
  geom_sf(mapping = aes(fill=Control, geometry = geometry)) +
  scale_fill_gradientn(colors=color_palette(100)) +
  ggtitle("Control of Corruption by Country in Africa") + 
  theme_void()
dev.off()

#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~



# plotting corruption control by wave and country-----

data <- read.csv("forAnalysis.csv")

pdf(file = "Country-wave1.pdf", paper = "a4r", width = 10 )

ggplot(data, aes(x = iso3c, y = control.cor, group = wave, color = wave)) +
  geom_point(aes(shape = wave), size=2, alpha = .5) +
  labs(title = "Control of Corruption by Country and Wave",
       x = "Country", y = "Control of Corruption") +
  theme( legend.position = c(.95, .9), axis.text=element_text(size=7)) +
  scale_color_manual(values = c( "#DC143C", "#0000FF","#228B22")) +
  coord_flip()

dev.off()


#~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~




