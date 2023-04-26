library(ggplot2)
library(wbstats)
library(RColorBrewer)
library(sf)
corruption <- wb_data("CC.EST", start_date = 2021)


country_map <- st_read("C:\\Users\\ribrahim\\Downloads\\afshape\\Africa_Boundaries.shp")
df_map <- merge(country_map, corruption, by.x= "ISO", by.y = "iso3c", all.x = T)
names(df_map)[names(df_map) == 'CC.EST'] <- 'Control'


color_palette <- colorRampPalette(brewer.pal(6, "RdYlBu"))

pdf(file = "corruptionmap3.pdf")

ggplot(data=df_map) +
  geom_sf(mapping = aes(fill=Control, geometry = geometry)) +
  scale_fill_gradientn(colors=color_palette(100)) +
  ggtitle("Control of Corruption by Country in Africa") + 
  theme_void()

dev.off()