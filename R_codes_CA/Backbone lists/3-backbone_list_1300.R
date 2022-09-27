################ Use the original 'backbone_list.R' to reach this step #####################################
###### Restart from the elevation ########
###### South Brazil >1,300m #############
###### Southeastern excluded (latitude < -24) ##########

############## Elevation raster ##############
sp_vr_occ_gbif_cl_CA_high2=data.frame(filter(sp_vr_occ_gbif_cl, sp_vr_occ_gbif_cl$elev_high >=1300))
sp_vr_occ_gbif_cl_CA_high2<-data.frame(filter(sp_vr_occ_gbif_cl_CA_high2, sp_vr_occ_gbif_cl_CA_high2$decimalLatitude <= -24))

############## Canopy raster ##############
canopy= raster('/Users/xbaroc/Documents/R/Canopy/Simard_Pinto_3DGlobalVeg_JGR.tif')
sp_vr_occ_gbif_cl_CA_high2$canopy<-raster::extract(canopy,cbind.data.frame(sp_vr_occ_gbif_cl_CA_high2$decimalLongitude,sp_vr_occ_gbif_cl_CA_high2$decimalLatitude))
sp_vr_occ_gbif_cl_CA_high_can2=data.frame(filter(sp_vr_occ_gbif_cl_CA_high2, sp_vr_occ_gbif_cl_CA_high2$canopy <= 5))

############## UCs raster ##############

ang_1300<-sp_vr_occ_gbif_cl_CA_high_can2
world.inp <- map_data("world")

dev.off()
g1<-ggplot() + geom_map(data = world.inp, map = world.inp, aes(x = long, y = lat, 
                                                               map_id = region), fill = "grey80") + xlim(min(ang_1300$decimalLongitude, na.rm = T), 
                                                                                                         max(ang_1300$decimalLongitude, na.rm = T)) + ylim(min(ang_1300$decimalLatitude, na.rm = T), 
                                                                                                                                                           max(ang_1300$decimalLatitude, na.rm = T)) + geom_point(data = ang_1300, aes(x = decimalLongitude, 
                                                                                                                                                                                                                                       y = decimalLatitude), colour = "darkblue", size = 0.5) + coord_fixed() + theme_bw() + 
  theme(axis.title = element_blank())

g1

ang_1300$uc<-raster::extract(shp_ras,cbind.data.frame(ang_1300$decimalLongitude,ang_1300$decimalLatitude))

############## Check how many ucs, species names and records/ucs ##############
x<-unique(ang_1300$uc)
y<-data.frame(filter(ang_1300, ang_1300$uc ==1179))
z<-unique(ang_1300$scientificName)

############## Order and exclude >-24 ##############

ang_1300<- ang_1300[order(ang_1300$uc),]
ang_1300<- ang_1300[order(ang_1300$scientificName),]
ang_1300<-data.frame(filter(ang_1300, ang_1300$decimalLatitude <= -24))


############## Exclude non-related UCs  ##############
ang_1300_cl<-ang_1300[!(ang_1300$uc %in% c('1179')), ]
write.csv2(ang_1300_cl, file = 'list_CA1_1300.csv')
summary(is.na(ang_1300_cl$uc))

short_ang_1300_cl<-ang_1300_cl[,c(1,8,9,10,11,12,13,17,18,22,23,26,30,33,37,38,39,40,45,51,52,53)]
#write.csv2(short_ang_1300_cl, file = 'short_list_CA1_1300.csv')
#list<-unique(short_ang_1500_cl[,c(7,22)])


############################# FINISH BACKBONE COORDINATES #############################################


#######################################################################################################
#######################################################################################################


############################# FILTER BY WORDS AND FIXED ELEVATION #####################################

sp_vr_occ_gbif2<-rbind(ang_1300_cl[,c(1:50)], sp_vr_occ_gbif_na)

elev<-data.frame(filter(sp_vr_occ_gbif2, sp_vr_occ_gbif2$elevation >=1300))
CA<- subset(sp_vr_occ_gbif2, grepl("Campo de altitude", sp_vr_occ_gbif2$locality))
CAs<- subset(sp_vr_occ_gbif2, grepl("Campos de altitude", sp_vr_occ_gbif2$locality))
ca<- subset(sp_vr_occ_gbif2, grepl("campo de altitude", sp_vr_occ_gbif2$locality))
cas<- subset(sp_vr_occ_gbif2, grepl("campos de altitude", sp_vr_occ_gbif2$locality))

CASS<-rbind(CA,CAs,ca,cas)
CASS<-CASS[(is.na(CASS$elevation) | CASS$elevation >= 1300),]

words<-rbind(elev,CASS)
words<-words[!(words$stateProvince %in% c('Amazonas','amazonas','AM', 'Roraima','roraima','RR')), ]
words<-unique(words)

list_backbone_1300<-bind_rows(ang_1300_cl,words)
list_backbone_1300<- list_backbone_1300[order(list_backbone_1300$uc),]
list_backbone_1300<- list_backbone_1300[order(list_backbone_1300$scientificName),]
xy<-unique(list_backbone_1300$scientificName)
write.csv2(list_backbone_1300, file = 'list_backbone_1300.csv')


############################# FINISH BACKBONE COORDINATES + NO COORDINATES #####################################


################################################################################################################
################################################################################################################


############################# CORRECTION OF THE BACKBONE LIST #################################################

unique(list_backbone_1300$stateProvince)
list_backbone_1300<-list_backbone_1300[!(list_backbone_1300$stateProvince %in% c('Bahia','bahia','BA', 'Ba','Goiás','Sergipe','Bom Retiro',
                                                                  'Distrito Federal', 'Df','Go','Rio Grande do Norte',
                                                                  'Mato Grosso do Sul','Tocantins','Pará','Mato Grosso','Goi s',
                                                                  'Acre','Pernambuco','Maranhão', 'Par','Paraíba','Rondônia','Diamantina',
                                                                  'Caeté','Piauí','Pinhão','Goias','Amapá','Alagoas','Fernando de Noronha', 'La Paz',
                                                                  'Giruá','Am','Curitiba','Rond nia', 'Peru', 'Río Negro', 'Estado da Bahia','State of Goiás',
                                                                  'Salta','Amazonas','São Joaquim',"Maranh o","Salvador","Estado Da Bahia","Santo André","amapa",
                                                                  "Ceará" ,"Planalto do Brasil" ,"Roraima (Terr.)" ,"Brasília Distrito Federal","Cundinamarca",
                                                                  "Mato Grosso (Former)","Serra dos Surucucus","Ro","Estado de Goias", "Ms","Santa Cruz","Estado de Roraima", 
                                                                  "Serra de Espinhaco","Province de Loja" ,"Campo Mourão" ,"Alto Paraíso de Goiás", "Rio de Janeiro","Minas Gerais",
                                                                  "São Paulo", "Ceará", "Espírito Santo","Mg","Es","Sao paulo", "Sp","MInas Gerais","Rj","Teresópolis","Esp rito Santo",
                                                                  "Rio De Janeiro", "Campos do Jordão","State of Minas Gerais", "Minas Garais","Espirito Santo",               
                                                                  "Belo Horizonte","Rio de Janiero", "Itatiaia", "Caparaó","São José do Barreiro", "Río de Janeiro","Esparito Santo",            
                                                                  "goias","Minas Geraes","Säo Paulo","Minas Gerais / Rio de Janeiro", "Espinhaco","Serro",        
                                                                  "São Paolo","Carangola", "[Minas Gerais]","Serra do Esphinaco","Nova Friburgo",  "Estado de Goiás",              
                                                                  "minas gerais","Estado de Minas Gerais","Rio de Janeiro/Minas Gerais", "S o Paulo" )), ]
                      
unique(list_backbone_1300$stateProvince)

############## Conditionally exclude NAs  ##############

list_backbone2_1300<-list_backbone_1300[!(is.na(list_backbone_1300$locality) & is.na(list_backbone_1300$uc) & is.na(list_backbone_1300$stateProvince)),]
list_backbone3_1300<-list_backbone2_1300[!(is.na(list_backbone2_1300$locality) & is.na(list_backbone2_1300$uc)),]



############## Excluding rows with selected keywords and coordinate ##############
l1_1300<-list_backbone3_1300[!(is.na(list_backbone3_1300$uc)),]
list_backbone3_1300<-subset(list_backbone3_1300, is.na(list_backbone3_1300$uc))
x<-"aeroporto|arbusto|árvore|arvore|arbórea|arborea|bloqueado|o. pret.|canta galo|flor|fruto|campo rupestre|rupestre|afloramento|itaobim|medina|cachoeira que congela|jacupiranga|Abaíra|Serra do Ray|Small river |Arapiranga|corumba|corumbá|mucug|ouro preto|espomoso|espumoso|Caneorios|canoeiros|canoeirso|lenheiro|paracatu|paracatú|belo horizonte|curvelo|montes claros|serro|salinas|diamantina|diamantiana|alto paraiso|alto paraíso|bocaína funda|bocaina funda|capanema|Minas Geraës|pico das almas|rio de contas|ouro branco|itabirito|Rio Negro|bocaina de minas|Ibitipoca|ibitipoca|Gandarela| cipó| cipo| Cipó| Cipo|neblina|Neblina|itacolomi|Itacolomi|Itacolomy|itacolomí|espinhaço|espinhaco|Espinhaço|Espinhaco|caraça|caraca|Caraça|Caraca|itambé| itambe|Itambé|Itambe|Bahia|bahia|Goiás|Sergipe|Bom Retiro|Distrito Federal|Df|Rio Grande do Norte|Mato Grosso do Sul|Tocantins|Pará|Mato Grosso|Gois|Acre|Pernambuco|Maranhão|Rondônia|Diamantina|Caeté|Piauí|Pinhão|Goias|Amapá|Alagoas|Fernando de Noronha|La Paz|Giruá|Curitiba|Rondnia|Peru|Río Negro|Estado da Bahia|State of Goiás|Salta|Amazonas|São Joaquim|Maranho|Salvador|Estado Da Bahia|Santo André|Ceará|Planalto do Brasil|Roraima(Terr.)|Brasília Distrito Federal|Cundinamarca|Mato Grosso (Former)|Serra dos Surucucus|Estado de Goias|Ms|Santa Cruz|Estado de Roraima|Serra de Espinhaco|Province de Loja|Campo Mourão|Alto Paraíso de Goiás"
list_backbone3_1300<-list_backbone3_1300[!grepl(x,list_backbone3_1300$locality,ignore.case = T),]
list_backbone4_1300<-rbind(list_backbone3_1300,l1_1300)

#write.csv2(list_backbone4, file = 'list2.csv')

l1_1300<-list_backbone4_1300[!(is.na(list_backbone4_1300$decimalLatitude)),]
l1_1300<-data.frame(filter(l1_1300, l1_1300$decimalLatitude <= -19))
l1_1300<-data.frame(filter(l1_1300, l1_1300$decimalLongitude <= -37))
l1_1300<-data.frame(filter(l1_1300, l1_1300$decimalLongitude >= -52))
list_backbone4_1300<-subset(list_backbone4_1300, is.na(list_backbone4_1300$decimalLatitude))
list_backbone4_1300<-rbind(list_backbone4_1300,l1_1300)

############## Adding UCs into NAs (by keywords)  ##############

list_na_1300<-subset(list_backbone4_1300, is.na(list_backbone4_1300$uc))
list_non_na_1300<-subset(list_backbone4_1300, (!is.na(list_backbone4_1300$uc)))


x<- subset(list_na_1300, grepl("aracatuba|araçatuba", list_na_1300$locality, ignore.case = T))
x$uc=145
list_na_1300<-list_na_1300[!grepl("aracatuba|araçatuba",list_na_1300$locality, ignore.case = T),] 
list_na_1300<-rbind(list_na_1300,x) 
unique(list_na_1300$uc)

x<- subset(list_na_1300, grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco", list_na_1300$locality, ignore.case = T))
x$uc=937
list_na_1300<-list_na_1300[!grepl("sao joaquim|são joaquim|da igreja|campo do padre|campo dos padres|campos dos padres|campos do padre|los padres|santa bárbara|urubici|pedra furada|corvo branco",list_na_1300$locality, ignore.case = T),] 
list_na_1300<-rbind(list_na_1300,x) 
unique(list_na_1300$uc)

x<- subset(list_na_1300, grepl("da farofa", list_na_1300$locality, ignore.case = T))
x$uc=1923
list_na_1300<-list_na_1300[!grepl("da farofa",list_na_1300$locality, ignore.case = T),] 
list_na_1300<-rbind(list_na_1300,x) 
unique(list_na_1300$uc)

x<- subset(list_na_1300, grepl("marumbi|morro do vigia|morro da vigia", list_na_1300$locality, ignore.case = T))
x$uc=146
list_na_1300<-list_na_1300[!grepl("marumbi|morro do vigia|morro da vigia",list_na_1300$locality, ignore.case = T),] 
list_na_1300<-rbind(list_na_1300,x) 
unique(list_na_1300$uc)

x<- subset(list_na_1300, grepl("pico do parana|pico do paraná|caratuva|caratuava", list_na_1300$locality, ignore.case = T))
x$uc="PE Pico do Parana"
list_na_1300<-list_na_1300[!grepl("pico do parana|pico do paraná|caratuva|caratuava",list_na_1300$locality, ignore.case = T),] 
list_na_1300<-rbind(list_na_1300,x) 
unique(list_na_1300$uc)


short.na_1300<-list_na_1300[,c(13,17,18,22,23,53)]
short.na_1300<-subset(short.na_1300, is.na(short.na_1300$uc))
summary(is.na(short.na_1300$uc))

list_clean_1300<-rbind(list_non_na_1300,list_na_1300)
list_clean_1300$uc[list_clean_1300$uc == "1923"] <- "RPPN Serra da Farofa"
list_clean_1300$uc[list_clean_1300$uc == "937"] <- "PARNA Sao Joaquim"
list_clean_1300$uc[list_clean_1300$uc == "145"] <- "Serra do Aracatuba"
list_clean_1300$uc[list_clean_1300$uc == "146"] <- "PE Pico do Marumbi"
unique(list_clean_1300$uc)

######### short lists


short.na2_1300<-unique(list_clean_1300[,c(8,13,53)])
#short.na2_1300<-unique(list_clean_1300[,c(8,10,13,17,18,22,23,33,38,39,40,45,51,53)])
short.na2_1300<- short.na2_1300[order(short.na2_1300$uc),]
short.na2_1300<- short.na2_1300[order(short.na2_1300$scientificName),]
short.na2_1300<- short.na2_1300[order(short.na2_1300$family),]
xy_1300<-unique(list_clean_1300$scientificName)


farofa4<-data.frame(filter(short.na2_1300, short.na2_1300$uc =="RPPN Serra da Farofa"))
sao_joaquim4<-data.frame(filter(short.na2_1300, short.na2_1300$uc =="PARNA Sao Joaquim"))
parana4<-data.frame(filter(short.na2_1300, short.na2_1300$uc =="PE Pico do Parana"))
aracatuba4<-data.frame(filter(short.na2_1300, short.na2_1300$uc =="Serra do Aracatuba"))


#########vouchers
list_clean_1300$uc<-rm_accent(list_clean_1300$uc)
list_clean_1300$locality<-rm_accent(list_clean_1300$locality)
list_clean_1300$recordedBy<-rm_accent(list_clean_1300$recordedBy)
list_clean_1300$stateProvince<-rm_accent(list_clean_1300$stateProvince)
setwd('/Users/xbaroc/Documents/R/campos_de_altitude/backbone_lists')
write.csv2(list_clean_1300[,c(1,10,13,17,18,22,23,45,40,39,38,53)], file = 'vouchers_1300.csv')
