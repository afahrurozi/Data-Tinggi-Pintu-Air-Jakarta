library(readr)
tinggi_muka_air <- read_csv("~/Documents/Datasets/Indonesia/DKI/data-tinggi-muka-air-januari-2017.csv")

Kampung_Melayu <- tinggi_muka_air %>% 
  filter(tinggi_muka_air$nama_pintu_air == "PS. Kp. Melayu") %>%
  select(tinggi_air) %>%
  collect()
mean(Kampung_Melayu)

katulampa <- tinggi_muka_air %>%
  filter(tinggi_muka_air$nama_pintu_air == "PS. Katulampa (Hulu)") %>%
  select(tinggi_air) %>%
  collect()
mean(katulampa)

#simplify
katulampa <- katulampa$tinggi_air
Kampung_Melayu <- Kampung_Melayu$tinggi_air

#Regresi
lm(Kampung_Melayu ~ katulampa)
plot(Kampung_Melayu ~ katulampa, pch = 20, cex = 1.3, col = c("blue", "red"))
abline(lm(Kampung_Melayu ~ katulampa))

#dataframe baru
pos_pantau <- subset(tinggi_muka_air)[, c('nama_pintu_air', 'tinggi_air',
                                        'latitude', 'longitude')]

#popup
pos_pantau$popup <- paste0(
  "<strong>", pos_pantau$nama_pintu_air,
  "<br>Rata-Rata Ketinggian : ", format((pos_pantau$tinggi_air), scientific=TRUE, format="d",
                              big.mark=".", decimal.mark=","))

#leaflet
library(leaflet)
leaflet(data=pos_pantau) %>% addTiles() %>% addCircleMarkers(
  ~longitude, ~latitude,
  stroke=FALSE,
  fillOpacity=0.5,
  popup=~popup)
