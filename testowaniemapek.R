library(sf)
library(raster)
library(rg)
library(readxl)
library(dplyr)
library(ggplot2)
options(scipen = 999)
mysf<-st_read("C:\\Users\\mateu\\Desktop\\Studies\\AdvancedEconometrics\\Project\\AdvancedR_project\\wojewodztwa.shp")
View(mysf)
mysf <- as.data.frame(mysf)
danewoj<-read_xlsx("dane_woj.xlsx")
danewoj$JPT_KOD_JE <- substr(danewoj$id, 2 ,3)
merged <- merge(x = mysf, y = danewoj, by = "JPT_KOD_JE", all.x = TRUE)
str(merged)

library(RColorBrewer)
gg<-ggplot(merged$geometry)+
  geom_sf(aes(fill=as.factor(merged$val))) + scale_fill_brewer(type = "seq", palette=1, direction = 1, aesthetics = "colour")
print(gg)

?scale_fill_brewer
