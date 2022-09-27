
library(rgbif)
library(countrycode)
library(sp)
library(rgdal)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(elevatr)
library(readr)
library(magrittr)
library(speciesgeocodeR)
library(raster)
library(rgbif)
library(scrubr)
library(maps)
library(abjutils)


##################################### GBIF #######################################

raw_occ_gbif<-read_tsv("/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists/0135609-210914110416597.csv",quote="")
#x<-unique(raw_occ_gbif$taxonRank)
sp_vr_occ_gbif<-raw_occ_gbif[!(raw_occ_gbif$taxonRank %in% c('FAMILY','ORDER', 'CLASS','GENUS')), ]

#write.csv(sp_vr_occ_gbif, file = 'gbif_filtered_infraspecies.csv')


# CLEAN THE DATASET to use coordinate
# mind that data often contain errors, so careful inspection and cleaning are necessary! 
# here we'll first remove records of absence or zero-abundance (if any):
names(sp_vr_occ_gbif)
sort(unique(sp_vr_occ_gbif$individualCount))  # notice if some points correspond to zero abundance
sort(unique(sp_vr_occ_gbif$occurrenceStatus))  # check for different indications of "absent", which could be in different languages! and remember that R is case-sensitive
absence_rows <- which(sp_vr_occ_gbif$individualCount == 0 |sp_vr_occ_gbif$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
length(absence_rows)
if (length(absence_rows) > 0) {
  sp_vr_occ_gbif <- sp_vr_occ_gbif[-absence_rows, ]
}

############### CoordinateCleaner ##################
sp_vr_occ_gbif$countryCode <-  countrycode(sp_vr_occ_gbif$countryCode, origin =  'iso2c', destination = 'iso3c')

sp_vr_occ_gbif<-cc_val(
  sp_vr_occ_gbif,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  value = "clean",
  verbose = TRUE
)


flags <- clean_coordinates(x = sp_vr_occ_gbif,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) 

summary(flags)
#plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")
sp_vr_occ_gbif_cl <- sp_vr_occ_gbif[flags$.summary,]

#setwd('/Users/xbaroc/Documents/R/campos_de_altitude')
#write.csv(occ_splink2_cl, file= 'data_splink_clean.csv')


############## Elevation raster ##############
elev_crop<-raster('/Users/xbaroc/Documents/R/campos_de_altitude/elev_CA_high.tif')
sp_vr_occ_gbif_cl$elev_high<-raster::extract(elev_crop,cbind.data.frame(sp_vr_occ_gbif_cl$decimalLongitude,sp_vr_occ_gbif_cl$decimalLatitude))
sp_vr_occ_gbif_cl_CA_high=data.frame(filter(sp_vr_occ_gbif_cl, sp_vr_occ_gbif_cl$elev_high >=1500))
sp_vr_occ_gbif_cl_CA_high<-data.frame(filter(sp_vr_occ_gbif_cl_CA_high, sp_vr_occ_gbif_cl_CA_high$decimalLatitude <= -19))

############## Canopy raster ##############
canopy= raster('/Users/xbaroc/Documents/R/Canopy/Simard_Pinto_3DGlobalVeg_JGR.tif')
sp_vr_occ_gbif_cl_CA_high$canopy<-raster::extract(canopy,cbind.data.frame(sp_vr_occ_gbif_cl_CA_high$decimalLongitude,sp_vr_occ_gbif_cl_CA_high$decimalLatitude))
sp_vr_occ_gbif_cl_CA_high_can=data.frame(filter(sp_vr_occ_gbif_cl_CA_high, sp_vr_occ_gbif_cl_CA_high$canopy <=5))

############## UCs raster ##############
shp= shapefile('/Users/xbaroc/Documents/R/campos_de_altitude/ucstodas.dbf')
head(shp)
dim(shp)
object.size(shp)
r <- raster(ncol=4001, nrow=4001)
extent(r) <- extent(shp)
shp$Grd_ranks <- rank(shp$NOME_UC1)
shp_ras<- rasterize(shp, r, "Grd_ranks", fun='first')

ang_1500<-sp_vr_occ_gbif_cl_CA_high_can
long_1500=ang_1500$decimalLongitude
lat_1500=ang_1500$decimalLatitude
world.inp <- map_data("world")

dev.off()
g1<-ggplot() + geom_map(data = world.inp, map = world.inp, aes(x = long, y = lat, 
                                                               map_id = region), fill = "grey80") + xlim(min(ang_1500$decimalLongitude, na.rm = T), 
                                                                                                         max(ang_1500$decimalLongitude, na.rm = T)) + ylim(min(ang_1500$decimalLatitude, na.rm = T), 
                                                                                                                                                           max(ang_1500$decimalLatitude, na.rm = T)) + geom_point(data = ang_1500, aes(x = decimalLongitude, 
                                                                                                                                                                                                                                       y = decimalLatitude), colour = "darkblue", size = 0.5) + coord_fixed() + theme_bw() + 
  theme(axis.title = element_blank())

g1

ang_1500$uc<-raster::extract(shp_ras,cbind.data.frame(ang_1500$decimalLongitude,ang_1500$decimalLatitude))

############## Check how many ucs, species names and records/ucs ##############
x<-unique(ang_1500$uc)
y<-data.frame(filter(ang_1500, ang_1500$uc ==755))
z<-unique(ang_1500$scientificName)

############## Order and exclude >-19 ##############

ang_1500<- ang_1500[order(ang_1500$uc),]
ang_1500<- ang_1500[order(ang_1500$scientificName),]
ang_1500<-data.frame(filter(ang_1500, ang_1500$decimalLatitude < -19))


############## Exclude non-related UCs  ##############
ang_1500_cl<-ang_1500[!(ang_1500$uc %in% c('223','16', '29','182','203','216','794','875','919')), ]
write.csv2(ang_1500_cl, file = 'list_CA1.csv')
summary(is.na(ang_1500_cl$uc))

short_ang_1500_cl<-ang_1500_cl[,c(1,8,9,10,11,12,13,17,18,22,23,26,30,33,37,38,39,40,45,51,52,53)]
write.csv2(short_ang_1500_cl, file = 'short_list_CA1.csv')
#list<-unique(short_ang_1500_cl[,c(7,22)])


############################# FINISH BACKBONE COORDINATES #############################################


#######################################################################################################
#######################################################################################################


############################# FILTER BY WORDS AND FIXED ELEVATION #####################################
sp_vr_occ_gbif2<-raw_occ_gbif[!(raw_occ_gbif$taxonRank %in% c('FAMILY','ORDER', 'CLASS','GENUS')), ]
names(sp_vr_occ_gbif2)
sort(unique(sp_vr_occ_gbif2$individualCount))  # notice if some points correspond to zero abundance
sort(unique(sp_vr_occ_gbif2$occurrenceStatus))  # check for different indications of "absent", which could be in different languages! and remember that R is case-sensitive
absence_rows <- which(sp_vr_occ_gbif2$individualCount == 0 |sp_vr_occ_gbif2$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
length(absence_rows)

if (length(absence_rows) > 0) {
  sp_vr_occ_gbif2 <- sp_vr_occ_gbif2[-absence_rows, ]
}


sp_vr_occ_gbif_na<-subset(sp_vr_occ_gbif2 , is.na(sp_vr_occ_gbif2$decimalLatitude))
sp_vr_occ_gbif2<-rbind(ang_1500_cl[,c(1:50)], sp_vr_occ_gbif_na)

elev<-data.frame(filter(sp_vr_occ_gbif2, sp_vr_occ_gbif2$elevation >=1500))
CA<- subset(sp_vr_occ_gbif2, grepl("Campo de altitude", sp_vr_occ_gbif2$locality))
CAs<- subset(sp_vr_occ_gbif2, grepl("Campos de altitude", sp_vr_occ_gbif2$locality))
ca<- subset(sp_vr_occ_gbif2, grepl("campo de altitude", sp_vr_occ_gbif2$locality))
cas<- subset(sp_vr_occ_gbif2, grepl("campos de altitude", sp_vr_occ_gbif2$locality))

CASS<-rbind(CA,CAs,ca,cas)
CASS<-CASS[(is.na(CASS$elevation) | CASS$elevation >= 1500),]
words<-rbind(elev,CASS)
words<-words[!(words$stateProvince %in% c('Amazonas','amazonas','AM', 'Roraima','roraima','RR')), ]
words<-unique(words)

list_backbone<-bind_rows(ang_1500_cl,words)
list_backbone<- list_backbone[order(list_backbone$uc),]
list_backbone<- list_backbone[order(list_backbone$scientificName),]
xy<-unique(list_backbone$scientificName)
write.csv2(list_backbone, file = 'list_backbone.csv')


############################# FINISH BACKBONE COORDINATES + NO COORDINATES #####################################


################################################################################################################
################################################################################################################


############################# CORRECTION OF THE BACKBONE LIST #################################################

unique(list_backbone$stateProvince)
list_backbone<-list_backbone[!(list_backbone$stateProvince %in% c('Bahia','bahia','BA', 'Ba','Goiás','Sergipe','Bom Retiro',
                                                                  'Distrito Federal', 'Df','Go','Rio Grande do Norte',
                                                                  'Mato Grosso do Sul','Tocantins','Pará','Mato Grosso','Goi s',
                                                                  'Acre','Pernambuco','Maranhão', 'Par','Paraíba','Rondônia','Diamantina',
                                                                  'Caeté','Piauí','Pinhão','Goias','Amapá','Alagoas','Fernando de Noronha', 'La Paz',
                                                                  'Giruá','Am','Curitiba','Rond nia', 'Peru', 'Río Negro', 'Estado da Bahia','State of Goiás',
                                                                  'Salta','Amazonas','São Joaquim',"Maranh o","Salvador","Estado Da Bahia","Santo André","amapa",
                                                                  "Ceará" ,"Planalto do Brasil" ,"Roraima (Terr.)" ,"Brasília Distrito Federal","Cundinamarca",
                                                                  "Mato Grosso (Former)","Serra dos Surucucus","Ro","Estado de Goias", "Ms","Santa Cruz","Estado de Roraima", 
                                                                  "Serra de Espinhaco","Province de Loja" ,"Campo Mourão" ,"Alto Paraíso de Goiás")), ]

unique(list_backbone$stateProvince)

############## Conditionally exclude NAs  ##############
list_backbone2<-list_backbone[!(is.na(list_backbone$locality) & is.na(list_backbone$uc) & is.na(list_backbone$stateProvince)),]
list_backbone3<-list_backbone2[!(is.na(list_backbone2$locality) & is.na(list_backbone2$uc)),]

############## Excluding rows with selected keywords and coordinate ##############
l1<-list_backbone3[!(is.na(list_backbone3$uc)),]
list_backbone3<-subset(list_backbone3, is.na(list_backbone3$uc))
x<-"aeroporto|arbusto|árvore|arvore|arbórea|arborea|bloqueado|o. pret.|canta galo|flor|fruto|campo rupestre|rupestre|afloramento|itaobim|medina|cachoeira que congela|jacupiranga|Abaíra|Serra do Ray|Small river |Arapiranga|corumba|corumbá|mucug|ouro preto|espomoso|espumoso|Caneorios|canoeiros|canoeirso|lenheiro|paracatu|paracatú|belo horizonte|curvelo|montes claros|serro|salinas|diamantina|diamantiana|alto paraiso|alto paraíso|bocaína funda|bocaina funda|capanema|Minas Geraës|pico das almas|rio de contas|ouro branco|itabirito|Rio Negro|bocaina de minas|Ibitipoca|ibitipoca|Gandarela| cipó| cipo| Cipó| Cipo|neblina|Neblina|itacolomi|Itacolomi|Itacolomy|itacolomí|espinhaço|espinhaco|Espinhaço|Espinhaco|caraça|caraca|Caraça|Caraca|itambé| itambe|Itambé|Itambe|Bahia|bahia|Goiás|Sergipe|Bom Retiro|Distrito Federal|Df|Rio Grande do Norte|Mato Grosso do Sul|Tocantins|Pará|Mato Grosso|Gois|Acre|Pernambuco|Maranhão|Rondônia|Diamantina|Caeté|Piauí|Pinhão|Goias|Amapá|Alagoas|Fernando de Noronha|La Paz|Giruá|Curitiba|Rondnia|Peru|Río Negro|Estado da Bahia|State of Goiás|Salta|Amazonas|São Joaquim|Maranho|Salvador|Estado Da Bahia|Santo André|Ceará|Planalto do Brasil|Roraima(Terr.)|Brasília Distrito Federal|Cundinamarca|Mato Grosso (Former)|Serra dos Surucucus|Estado de Goias|Ms|Santa Cruz|Estado de Roraima|Serra de Espinhaco|Province de Loja|Campo Mourão|Alto Paraíso de Goiás"
list_backbone3<-list_backbone3[!grepl(x,list_backbone3$locality,ignore.case = T),]
list_backbone4<-rbind(list_backbone3,l1)

l1<-list_backbone4[!(is.na(list_backbone4$decimalLatitude)),]
l1<-data.frame(filter(l1, l1$decimalLatitude <= -19))
l1<-data.frame(filter(l1, l1$decimalLongitude <= -37))
l1<-data.frame(filter(l1, l1$decimalLongitude >= -52))
list_backbone4<-subset(list_backbone4, is.na(list_backbone4$decimalLatitude))
list_backbone4<-rbind(list_backbone4,l1)

write.csv2(list_backbone4, file = 'list2.csv')



############## Adding UCs into NAs (by keywords)  ##############

list_na<-subset(list_backbone4, is.na(list_backbone4$uc))
list_non_na<-subset(list_backbone4, (!is.na(list_backbone4$uc)))

x<- subset(list_na, grepl("caparao|caparaó|capara|pico da bandeira|macieira|lajao|lajão|macierias|es, campo de altitude, comum|troqueira|troqnquiera|terreirao|terreirão", list_na$locality, ignore.case = T))
x$uc=933
list_na<-list_na[!grepl("caparao|caparaó|capara|pico da bandeira|macieira|lajao|lajão|macierias|es, campo de altitude, comum|troqueira|troqnquiera|terreirao|terreirão",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x)

x<- subset(list_na, grepl("itatia|itatiaia|itatiaya|agulhas negras|. negras|prateleira|itahaya|rebouças|reboucas|planalto", list_na$locality, ignore.case = T))
x$uc=949
list_na<-list_na[!grepl("itatia|itatiaia|itatiaya|agulhas negras|. negras|prateleira|itahaya|rebouças|reboucas|planalto",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x)  
unique(list_na$uc)

x<- subset(list_na, grepl("orgãos|orgaos|do sino|organ|órgãos|órgaos|Org. |orgais|das antas|dao antas", list_na$locality, ignore.case = T))
x$uc=90
list_na<-list_na[!grepl("orgãos|orgaos|do sino|organ|órgãos|órgaos|Org. |orgais|das antas|dao antas",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("da mina|papagaio|serra fina", list_na$locality, ignore.case = T))
x$uc=213
list_na<-list_na[!grepl("da mina|papagaio|serra fina",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("do baú|do bau|do paiol|bauzinho", list_na$locality, ignore.case = T))
x$uc=389
list_na<-list_na[!grepl("do baú|do bau|do paiol|bauzinho",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("Jordào|jordao|jordão|fazenda da onça|fazenda do onça|são josé dos alpes|sao jose dos alpes|p. e. c. j.|pico do ataque|morro do ataque|itaguaré|taguaré|taguare", list_na$locality, ignore.case = T))
x$uc=22
list_na<-list_na[!grepl("Jordào|jordao|jordão|fazenda da onça|fazenda do onça|são josé dos alpes|sao jose dos alpes|p. e. c. j.|pico do ataque|morro do ataque|itaguaré|taguaré|taguare",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("três picos|tres picos|3 picos|Caledônia|Caledônea|Caledonia|Caledonea", list_na$locality, ignore.case = T))
x$uc=41
list_na<-list_na[!grepl("três picos|tres picos|3 picos|Caledônia|Caledônea|Caledonia|Caledonea",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("macae|macaé", list_na$locality, ignore.case = T))
x$uc=42
list_na<-list_na[!grepl("macae|macaé",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("aracatuba|araçatuba", list_na$locality, ignore.case = T))
x$uc=145
list_na<-list_na[!grepl("aracatuba|araçatuba",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("desengano|santa maria madalena|magdalena|alto da república|alto da republica", list_na$locality, ignore.case = T))
x$uc=788
list_na<-list_na[!grepl("desengano|santa maria madalena|magdalena|alto da república|alto da republica",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("forno grande", list_na$locality, ignore.case = T))
x$uc=790
list_na<-list_na[!grepl("forno grande",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("serra negra", list_na$locality, ignore.case = T))
x$uc=881
list_na<-list_na[!grepl("serra negra",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("bocaina", list_na$locality, ignore.case = T))
x$uc=910
list_na<-list_na[!grepl("bocaina",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco", list_na$locality, ignore.case = T))
x$uc=937
list_na<-list_na[!grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("da farofa", list_na$locality, ignore.case = T))
x$uc=1923
list_na<-list_na[!grepl("da farofa",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("dos marins", list_na$locality, ignore.case = T))
x$uc=213
list_na<-list_na[!grepl("dos marins",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

x<- subset(list_na, grepl("brigadeiro|PESB", list_na$locality, ignore.case = T))
x$uc="PE Serra do Brigadeiro"
list_na<-list_na[!grepl("brigadeiro|PESB",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

#different from 1800 and 2000
x<- subset(list_na, grepl("araras|A.P.A. Petrópolis| cuca|maria comprida|APA Petr", list_na$locality, ignore.case = T))
x$uc="APA Petrópolis"
list_na<-list_na[!grepl("araras|A.P.A. Petrópolis| cuca|maria comprida|APA Petr",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)
#######

x<- subset(list_na, grepl("pico do parana|pico do paraná|caratuva|caratuava", list_na$locality, ignore.case = T))
x$uc="PE Pico do Paraná"
list_na<-list_na[!grepl("pico do parana|pico do paraná|caratuva|caratuava",list_na$locality, ignore.case = T),] 
list_na<-rbind(list_na,x) 
unique(list_na$uc)

short.na<-list_na[,c(13,17,18,22,23,53)]
short.na<-subset(short.na, is.na(short.na$uc))
summary(is.na(short.na$uc))

list_clean<-rbind(list_non_na,list_na)
list_clean$uc[list_clean$uc == "1923"] <- "RPPN Serra da Farofa"
list_clean$uc[list_clean$uc == "949"] <- "PARNA Itatiaia"
list_clean$uc[list_clean$uc == "933"] <- "PARNA Caparaó"
list_clean$uc[list_clean$uc == "1019"] <- "Pedra de São Domingos"
list_clean$uc[list_clean$uc == "151"] <- "Pedra de São Domingos"
list_clean$uc[list_clean$uc == "213"] <- "PE Papagaio/Pedra da Mina/APA Mantiqueira"
list_clean$uc[list_clean$uc == "342"] <- "PE Campos do Jordão"
list_clean$uc[list_clean$uc == "389"] <- "MONA Pedra do Baú"
list_clean$uc[list_clean$uc == "937"] <- "PARNA São Joaquim"
list_clean$uc[list_clean$uc == "790"] <- "PE Forno Grande"
list_clean$uc[list_clean$uc == "90"] <- "PARNA Serra dos Órgãos"
list_clean$uc[list_clean$uc == "109"] <- "Serra da Bandeira"
list_clean$uc[list_clean$uc == "145"] <- "Serra de Araçatuba"
list_clean$uc[list_clean$uc == "788"] <- "PE Desengano"
list_clean$uc[list_clean$uc == "755"] <- "PE Campos do Jordão"
list_clean$uc[list_clean$uc == "910"] <- "PARNA Bocaina"
list_clean$uc[list_clean$uc == "881"] <- "Serra Negra"
list_clean$uc[list_clean$uc == "22"] <- "PE Campos do Jordão"
list_clean$uc[list_clean$uc == "41"] <- "PE Três Picos"
list_clean$uc[list_clean$uc == "42"] <- "APA Macaé de Cima"
list_clean$uc[list_clean$uc == "79"] <- "PARNA Itatiaia"
unique(list_clean$uc)


####removing the remaining campos rupestres
#list_clean<-list_clean[!(list_clean$decimalLatitude > -21 & list_clean$decimalLongitude < -42.5),]

######Fast plot
g1<-ggplot() + geom_map(data = world.inp, map = world.inp, aes(x = long, y = lat, 
                                                               map_id = region), fill = "grey80") + xlim(min(ang_1500_cl$decimalLongitude, na.rm = T), 
                                                                                                         max(ang_1500_cl$decimalLongitude, na.rm = T)) + ylim(min(ang_1500_cl$decimalLatitude, na.rm = T), 
                                                                                                                                                                  max(ang_1500_cl$decimalLatitude, na.rm = T)) + geom_point(data = ang_1500_cl, aes(x = decimalLongitude, 
                                                                                                                                                                                                                                                            y = decimalLatitude), colour = "darkblue", size = 0.5) + coord_fixed() + theme_bw() + 
  theme(axis.title = element_blank())

g1


###### short lists

short.na2<-unique(list_clean[,c(8,13,53)])
#short.na2<-unique(list_clean[,c(8,10,13,17,18,22,23,33,38,39,40,45,51,53)])
short.na2<- short.na2[order(short.na2$uc),]
short.na2<- short.na2[order(short.na2$scientificName),]
short.na2<- short.na2[order(short.na2$family),]
xy<-unique(list_clean$scientificName)
short.na3<-na.omit(short.na2)

itatiaia<-data.frame(filter(short.na2, short.na2$uc =='PARNA Itatiaia'))
PARNASO<-data.frame(filter(short.na2, short.na2$uc =='PARNA Serra dos Órgãos'))
capara<-data.frame(filter(short.na2, short.na2$uc =='PARNA Caparaó'))
bocaina<-data.frame(filter(short.na2, short.na2$uc =='PARNA Bocaina'))
cjordao<-data.frame(filter(short.na2, short.na2$uc =="PE Campos do Jordão"))
papagaio<-data.frame(filter(short.na2, short.na2$uc =="PE Papagaio/Pedra da Mina/APA Mantiqueira"))
farofa<-data.frame(filter(short.na2, short.na2$uc =="RPPN Serra da Farofa"))
sao_domingos<-data.frame(filter(short.na2, short.na2$uc =="Pedra de São Domingos"))
bau<-data.frame(filter(short.na2, short.na2$uc =="MONA Pedra do Baú"))
sao_joaquim<-data.frame(filter(short.na2, short.na2$uc =="PARNA São Joaquim"))
f_grande<-data.frame(filter(short.na2, short.na2$uc =="PE Forno Grande"))
brigadeiro<-data.frame(filter(short.na2, short.na2$uc =="PE Serra do Brigadeiro"))
apa_petropolis<-data.frame(filter(short.na2, short.na2$uc =="APA Petrópolis"))
parana<-data.frame(filter(short.na2, short.na2$uc =="PE Pico do Paraná"))
bandeira <-data.frame(filter(short.na2, short.na2$uc =="Serra da Bandeira"))
aracatuba <-data.frame(filter(short.na2, short.na2$uc =="Serra de Araçatuba"))
desengano <-data.frame(filter(short.na2, short.na2$uc =="PE Desengano"))
s_negra <-data.frame(filter(short.na2, short.na2$uc =="Serra Negra"))
tres_picos<-data.frame(filter(short.na2, short.na2$uc =="PE Três Picos"))
macae <-data.frame(filter(short.na2, short.na2$uc =="APA Macaé de Cima"))

#########vouchers

list_clean$uc<-rm_accent(list_clean$uc)
list_clean$locality<-rm_accent(list_clean$locality)
list_clean$recordedBy<-rm_accent(list_clean$recordedBy)
list_clean$stateProvince<-rm_accent(list_clean$stateProvince)

write.csv2(list_clean[,c(1,10,13,17,18,22,23,45,40,39,38,53)], file = 'vouchers_1500.csv')

