###############################################################################################
###############################################################################################
############################## ANALYSES LSL AND SSL DATA ######################################

############### Hierarchical clustering ###############
library(BinMat)
PUB_binary_matrix<-t(LSL_final2[,c(15:29)])

pdf('PUB_upgma_LSL2.pdf',width = 12,height = 10)
my_upgma<-BinMat::upgma(PUB_binary_matrix, method = 'binary', hclust ='mcquitty',  bts = 10000)
dev.off()

PUB_binary_matrix2<-t(SSL_final2[,c(15:29)])

pdf('PUB_upgma_SSL2.pdf',width = 12,height = 10)
my_upgma<-BinMat::upgma(PUB_binary_matrix2, method = 'binary', hclust ='mcquitty',  bts = 10000)
dev.off()


############################## Dissimilarity ##############################
library(vegan)
library(ggplot2)
library(viridis)
library(cowplot)
library(betapart)
library(reshape2)
library(jaccard)

PUB_binary_matrix3<- as.data.frame(apply(PUB_binary_matrix, 2, as.numeric))  # Convert all variable types to numeric
sapply(PUB_binary_matrix3, class)                                
PUB_JAC<-vegdist(PUB_binary_matrix3, method = "jaccard")
PUB_richness <- as.data.frame(specnumber(PUB_binary_matrix))  
PUB_mydf<-as.data.frame(as.matrix(PUB_JAC))
write.csv2(PUB_mydf,file = "PUB_dissimilarity_LSL.csv")

PUB_binary_matrix4<-as.data.frame(apply(PUB_binary_matrix2, 2, as.numeric))
sapply(PUB_binary_matrix4, class)                                
PUB_JAC2<-vegdist(PUB_binary_matrix4, method = "jaccard")
PUB_richness2<- as.data.frame(specnumber(PUB_binary_matrix2))  
PUB_mydf2<-as.data.frame(as.matrix(PUB_JAC2))
write.csv2(PUB_mydf2,file = "PUB_dissimilarity_SSL.csv")

###### Number of genera and family ###############
PUB_genus_list<-unique(gsub(LSL_final2$scientific.name, pattern = ' .*', replacement = ''))
PUB_genus_list2<-unique(gsub(SSL_final2$scientific.name, pattern = ' .*', replacement = ''))
PUB_family_list<-unique(LSL_final2$family)
PUB_family_list2<-unique(SSL_final2$family)


#################### Network connections ##############################

library(IsingFit)
library(qgraph)

PUB_binary_matrix5<-LSL_final2[,c(15:24,26:29)]
PUB_binary_matrix5<- as.data.frame(apply(PUB_binary_matrix5, 2, as.numeric))  # Convert all variable types to numeric
sapply(PUB_binary_matrix5, class)     

PUB_fit1<-IsingFit(PUB_binary_matrix5, gamma = 1, AND = T)

PUB_binary_matrix6<-SSL_final2[,c(15:24,26:29)]
PUB_binary_matrix6<- as.data.frame(apply(PUB_binary_matrix6, 2, as.numeric))  # Convert all variable types to numeric
sapply(PUB_binary_matrix6, class) 

PUB_fit2<-IsingFit(PUB_binary_matrix6, gamma = 1, AND = T)

pdf('PUB_qgraph_LSL2.pdf',width = 12,height = 10)
PUB_fit1<-IsingFit(PUB_binary_matrix5, gamma = 1, AND = T)
dev.off()

pdf('PUB_qgraph_SSL2.pdf',width = 12,height = 10)
PUB_fit2<-IsingFit(PUB_binary_matrix6, gamma = 1, AND = T)
dev.off()

########################### Network comparison test ##############################

library(NetworkComparisonTest)
PUB_N<-NCT(PUB_binary_matrix5, PUB_binary_matrix6, binary.data=T, gamma = 1,test.edges=TRUE, edges="all")
PUB_N$glstrinv.real
PUB_N$glstrinv.pval
PUB_N$glstrinv.sep
PUB_N$nwinv.real
PUB_N$nwinv.pval
PUB_N$einv.pvals
PUB_N$edges.tested

pdf('PUB_NCT2.pdf',width = 12,height = 10)
plot(PUB_N, what="strength")
dev.off()


###### Endemic species to campos de altitude
PUB_endemic_LSL<-data.frame(filter(SSL_final2, SSL_final2$vegetation.type == 'Campo de Altitude'))

#### Exporting appendices
setwd('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2')
write.csv(LSL_final2, 'AppS1.csv')
write.csv(SSL_final2, 'AppS2.csv')
write.csv(PUB_N$einv.pvals, 'AppS3.csv')

###############################################################################################
############################## ANALYSES OF CONSERVATION,LIFEFORMS AND ETC #####################

library(flora)
library(abjutils)
library(dplyr)
library(ggplot2)
library(stringr)

LSL_final2$life.form<-rm_accent(LSL_final2$life.form)
LSL_final2$endemism<-rm_accent(LSL_final2$endemism)
LSL_final2$habitat<-rm_accent(LSL_final2$habitat)
SSL_final2$life.form<-rm_accent(SSL_final2$life.form)
SSL_final2$endemism<-rm_accent(SSL_final2$endemism)
SSL_final2$habitat<-rm_accent(SSL_final2$habitat)


##### Conservation status
MMA_2022<-read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2/MMA2022_list.csv', sep = ';')
LSL_final2_cons<-LSL_final2
LSL_final2_cons$Species<-sapply(LSL_final2_cons$scientific.name, remove.authors)
LSL_MMA2<-merge(LSL_final2_cons,MMA_2022, all.x = T,all.y = F)

SSL_final2_cons<-SSL_final2
SSL_final2_cons$Species<-sapply(SSL_final2_cons$scientific.name, remove.authors)
SSL_MMA2<-merge(SSL_final2_cons,MMA_2022, all.x = T,all.y = F)

cons_status2_LSL <- LSL_MMA2 %>%
  group_by(threat.status.mma2022) %>%
  count()
cons_status2_LSL_NT <- LSL_MMA2 %>%
  group_by(threat.status) %>%
  count()

NT_LSL2<- data.frame(threat.status.mma2022=c("NT","LC"),n= c(47,242))
cons_status2_LSL<-rbind(cons_status2_LSL,NT_LSL2)
cons_status2_LSL<-cons_status2_LSL[-4,]

cons_status2_SSL <- SSL_MMA2 %>%
  group_by(threat.status.mma2022) %>%
  count()
cons_status2_SSL_NT <- SSL_MMA2 %>%
  group_by(threat.status) %>%
  count()
NT_SSL2<- data.frame(threat.status.mma2022=c("NT","LC"),n= c(20,80))
cons_status2_SSL<-rbind(cons_status2_SSL,NT_SSL2)
cons_status2_SSL<-cons_status2_SSL[-4,]

##### Endemism

endemism_LSL2 <- LSL_MMA2 %>%
  group_by(endemism) %>%
  count()
endemism_LSL2 <-endemism_LSL2[order(endemism_LSL2$n, decreasing=TRUE),]

endemism_SSL2 <- SSL_MMA2 %>%
  group_by(endemism) %>%
  count()
endemism_SSL2 <-endemism_SSL2[order(endemism_SSL2$n, decreasing=TRUE),]

##### Life form
lifeform_LSL2 <- LSL_MMA2 %>%
  group_by(life.form) %>%
  count()
lifeform_LSL2 <-lifeform_LSL2[order(lifeform_LSL2$n, decreasing=TRUE),]

lifeform_SSL2 <- SSL_MMA2 %>%
  group_by(life.form) %>%
  count()
lifeform_SSL2 <-lifeform_SSL2[order(lifeform_SSL2$n, decreasing=TRUE),]

##### Habitat
habitat_LSL2 <- LSL_MMA2 %>%
  group_by(habitat) %>%
  count()
habitat_LSL2 <-habitat_LSL2[order(habitat_LSL2$n, decreasing=TRUE),]

habitat_SSL2 <- SSL_MMA2 %>%
  group_by(habitat) %>%
  count()
habitat_SSL2 <-habitat_SSL2[order(habitat_SSL2$n, decreasing=TRUE),]

##### Family
family_LSL2 <- LSL_MMA2 %>%
  group_by(family) %>%
  count()
family_LSL2 <-family_LSL2[order(family_LSL2$n, decreasing=TRUE),]

family_SSL2 <- SSL_MMA2 %>%
  group_by(family) %>%
  count()
family_SSL2 <-family_SSL2[order(family_SSL2$n, decreasing=TRUE),]



######### PLOTTING DATA ######
########################################### LSL ########################################
data <- read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2/PUB_merged_DATA.csv', sep= ';')


empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar*5, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(unique(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))


label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar  
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +      
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.1) +
  
   geom_segment(data=grid_data, aes(x = end, y = 1500, xend = start, yend = 1500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend =500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
   annotate("text", x = rep(max(data$id),4), y = c(0, 500, 1000, 1500), label = c("0", "500", "1000", "1500") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-1000,2500) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,6), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle= label_data$angle, inherit.aes = FALSE ) +
  

  geom_segment(data=base_data, aes(x = start, y = -30, xend = end, yend = -30), colour = "black", alpha=0.8, size=0.5 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -120, label=group), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=6, fontface="bold", inherit.aes = FALSE)

p

pdf('PUB_merged_graphs.pdf',width = 10,height = 10)
p
dev.off()

############################################ SSL ########################################


data <- read.csv('/Users/xbaroc/Documents/R/campos_de_altitude/lists_UCs2/PUB_merged_DATA2.csv', sep= ';')


empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar*5, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(unique(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))


label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.1) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 750, xend = start, yend = 750), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend = 500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 250, xend = start, yend =250), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(data$id),4), y = c(0, 250, 500, 750), label = c("0", "250", "500", "750") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-1000,2500) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,6), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle= label_data$angle, inherit.aes = FALSE ) +
  

  geom_segment(data=base_data, aes(x = start, y = -30, xend = end, yend = -30), colour = "black", alpha=0.8, size=0.5 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -120, label=group), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=6, fontface="bold", inherit.aes = FALSE)

p

pdf('PUB_merged_graphs2.pdf',width = 10,height = 10)
p
dev.off()



