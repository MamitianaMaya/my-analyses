Name <-c("Nomena","Kanto","Maya","Valery",'Tojo')
Female<- c(TRUE,TRUE,TRUE,FALSE,FALSE)
Height<-c(1.60,1.52,1.51,1.62,1.71) 
MBC <- data.frame(Name,Female,Height)

"C:/Users/Ant Lab 003/Documents/Data_frame.csv"
read.csv("C:/Users/Ant Lab 003/Documents/Data_frame.csv")
importation_fichier <- read.csv("C:/Users/Ant Lab 003/Documents/Data_frame.csv")
read.csv2("C:/Users/Ant Lab 003/Documents/Data_frame.csv")
library(dplyr)
"c:/Users/Ant Lab 003/Documents/Data_frame.csv"
read.csv("C:/Users/Ant Lab 003/Documents/Data_frame.csv")
importation_fichier <- read.csv("C:/Users/Ant Lab 003/Documents/Data_frame.csv")
mean(importation_fichier$Height)
str(importation_fichier)
sd(importation_fichier$Height)

####IMPORTATION DONNEES EXO1####
read.csv("C:/Users/Ant Lab 003/Documents/Exo/data_exo1.csv")
tableau_exo1 <- read.csv("C:/Users/Ant Lab 003/Documents/Exo/data_exo1.csv")

###Methodes t@correction

mean_temp <-mean(tableau_exo1$temperature) 
mean_eggs <- mean(tableau_exo1$eggs)

med_temp <- median(tableau_exo1$temperature)
med_eggs <- median(tableau_exo1$eggs)

sd_temp <- sd(tableau_exo1$temperature)
sd_eggs <- sd(tableau_exo1$eggs)
              
min_temp <- min(tableau_exo1$temperature)
min_eggs <- min(tableau_exo1$eggs)

max_temp <- max(tableau_exo1$temperature)
max_eggs <- max(tableau_exo1$eggs)

temperature <- c(mean_temp,med_temp,sd_temp,min_temp,max_temp)
eggs <- c(mean_eggs,median_eggs,sd_eggs,min_eggs,max_eggs)

results <- data.frame(temperature,eggs)
row.names(results) <- c("mean","median","sd","min","max")
View(results)

write.csv(results,"C:/Users/Ant Lab 003/Documents/Github/export2_exo1.csv")

####2nd methods

mean(tableau_exo1$temperature)
median(tableau_exo1$temperature)
sd(tableau_exo1$temperature)
min(tableau_exo1$temperature)
max(tableau_exo1$temperature)

####CALCUL_Eggs####
mean(tableau_exo1$eggs)
median(tableau_exo1$eggs)
sd(tableau_exo1$eggs)
min(tableau_exo1$eggs)
max(tableau_exo1$eggs)

####IMPORTATION DATA FRAME.CSV####
results_exo1 <- read.csv("C:/Users/Ant Lab 003/Documents/Exo/data_frame_exo1.csv")

####CODE MANAMBOATRA DATA FRAME####
parameters <- c("mean","median","sd","min","max")
temperature <- c(272.5187,273.426,20.8065,225.193,325.426)
eggs <- c(101.22,100,9.831832,70,130)
final_results <- data.frame(parameters,temperature,eggs)
####Exportation fichier

write.csv(final_results,"C:/Users/Ant Lab 003/Documents/Github/export_data_exo1.csv")

###Milalao donnees @dplyr
library(dplyr)
View(starwars)
dat1 <- starwars |>
mutate(height_m = height * 0.01, bmi = mass/(height_m)^2)  |>
select(name, bmi, gender)  |>
filter(bmi > 25 & gender == "masculine")

###19/01/2022
library(dplyr)
library(ggplot2)

#import data
data <- read.csv("C:/Users/antman/Documents/Data R/data_maya.csv")

#Look at the number of rows and columns
dim(data)

#Calculate the number of individuals and genera per subfamilies
d1 <- data  |>
  group_by(Type,Subfamilies) |> 
  summarise(n_ind=n(),n_gen=n_distinct(Genres),n_sp=n_distinct(SpeciesName))
#Calculate the number of genera, sp, individuals per Type and site
Nind <- data  |>
  group_by(Type, Site) |> 
  summarise(n_gen=n_distinct(Genres),n_sp=n_distinct(SpeciesName), n_ind=n())
#Calculate the number of individuals and genera per transect per pitfall
d2 <- data  |>  
  group_by(Collectors.code, subsample)  |>
  summarise(n_ind=n(),n_gen=n_distinct(Genres)) 

#Total of individuals
sum(d1$n_ind)
View(d2)
sum(d2$n_ind)

#Plot the number of genera per transect
#1
ggplot(d1, aes(x=Collectors.code, y=n_gen))+
  geom_col()

#2
ggplot(data, aes(x=Collectors.code))+
  geom_bar()

#multipanel figure, panel=transect
ggplot(data, aes(x=subsample))+
  geom_bar()+
  facet_wrap(~Collectors.code)

#Exercise with my own data
#Install packages
library(dplyr)
library(ggplot2)

#Import my data
my_data <- read.csv("C:/Users/antman/Documents/Data/my_data.csv")

#Look at the number of rows and columns
dim(my_data)

#Calculate the number of individuals and genera per transect per site
d <- my_data  |>
  group_by(Site, Transect) |>
  summarise(n_gen=n_distinct(Genus), n_ind=n())

#Plot the number of genera per site
ggplot(d, aes(x=n_gen))+
  geom_bar()+
  facet_wrap(~Site)+
  ggtitle("number of genera per site")+xlab("N Genera")


#Calculate the number of individuals and genera per subsample per transect per site
d1a <- my_data  |>
  group_by(Site, Transect, Subsample)  |>  
  summarise(n_gen=n_distinct(Genus), n_ind=n())

#Calculating the number of genera per type 
d1b <- my_data  |>
  group_by(Type)  |>
  summarise(n_gen=n_distinct(Genus))  

#Plot the number of genera per type of culture
ggplot(d1b, aes(x=Type, y=n_gen))+
  geom_col()+
  ggtitle("Number of genera per age of cultivation")+xlab("Age of cultivation")+ylab("N genera")



#Calculating the number of individuals per subsample per transect per site per number
d1c <- my_data  |>
  group_by(Type, Site, Transect, Subsample)  |>
  summarise(n_gen=n_distinct(Genus), n_ind=n())

#Plot the number of individuals per type of culture
ggplot(d1c, aes(x=n_ind, y=Type))+
  geom_col()+
  ggtitle("N individuals")+xlab("Age of cultivation")+ylab("N individuals")

ggplot(d1c, aes(x=Transect))+
  geom_bar()

# multiplanel figure: panel = transect
ggplot(d1c, aes(x =Subsample)) +
  geom_bar()+
  facet_wrap(~Type)+
  ggtitle("number of individuals per age of cultivation")+xlab("Age of cultivation")+ylab("N individuals")

library(dplyr)
final_data <- read.csv("C:/Users/antman/Documents/Data/data_maya.csv")
individuals <- final_data |>
  group_by(Type)  |>
  summarise(n_ind=n(), n_gen=n_distinct(Genus))  

number <- final_data |>
  group_by(Type, Site, Transect)  |>
  summarise(n_ind=n(), n_gen=n_distinct(Genus))  

data_num <- number
write.csv(data_num, "C:/Users/antman/Documents/Data/Nindividuals.csv")

data_mounting <- read.csv("C:/Users/antman/Documents/Data/data_maya.csv")
Total <- data_mounting |>
  group_by(Type, Site, Transect, Subsample, Residus) |> 
  summarise(n_ind=n(), n_res=n_distinct(yes$Residus))  


#RAREFACTION EXERCISE WITH DATA + FAKE SPECIES NAME
##Examples
data(bird) 
out <- iNEXT(bird, datatype="abundance") 
ggiNEXT(out)

data(ant)
data(ciliates)
data(spider)
#with my data
install.packages("iNEXT")
library(iNEXT)
library(ggplot2)
library(dplyr)
inext_data <- read.csv("C:/Users/antman/Documents/Data R/data_rare.csv")
data2 <- inext_data[, -1]
maize <- iNEXT(data2, q=0, datatype="abundance")
ggiNEXT(maize, type=3)
type2 <- iNEXT(data2, q=c(0,1,2), datatype="abundance")
ggiNEXT(type2, type=3, facet.var = "order")

                                #14/04/2023
                           #####DataAnalyses
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
data1 <- read.csv("C:/Users/antman/Documents/Data R/For_Species.csv")
View(data1)
str(data1)

####Add a new column with the short Species names to the data frame
new_data <- data1 %>% 
mutate(shortSp=paste(substr(Genera, 1, 3), substr(Species, 1, 3), sep="_"))
write.csv(new_data,"C:/Users/antman/Documents/Data R/Imported file/shortSp.csv")
####The same with the data + NA
data_NA <- read.csv("C:/Users/antman/Documents/Data R/data_maya.csv")
d <- data_NA %>% 
  mutate(shortSp=paste(substr(Genera, 1, 3), substr(Species, 1, 3), sep="_"))
write.csv(d,"C:/Users/antman/Documents/Data R/Imported file/shortSp_NA.csv")

###Code for picking variables from the data frame
....(data) %>% select(Type, Site, Species, shortSp)
  
###Calculate the number of Species per site
data_Sp_Site <- new_data  |>
  group_by(Type, Site, shortSp) |>
  summarise(n_sp=n())|>
  pivot_wider(names_from = shortSp, values_from = n_sp, values_fill = 0)
write.csv(data_Sp_Site,"C:/Users/antman/Documents/Data R/Imported file/data_Sp_Site.csv")


###Calculate the number of Species per type of culture
data_Sp_Type <- new_data |>
  group_by(Type, shortSp)|>
  summarise(n_sp=n())|>
  pivot_wider(names_from = shortSp, values_from = n_sp, values_fill = 0)
write.csv(data_Sp_Type,"C:/Users/antman/Documents/Data R/Imported file/data_Sp_Type.csv")
view(new_data)
###Calculate the number of endemic, native, exotic and invasive species (total)
data_stat <- new_data|>
  group_by(Status)|>
  summarise(n=n_distinct(Species))
write.csv(data_stat,"C:/Users/antman/Documents/Data R/Imported file/data_stat.csv")


####Calculate the number of endemic vs exotic Sp per type of land use (number of Species)
data_SpStat_Type1 <- new_data |>
  group_by(Type, Status)|>
  summarise(n_sp=n_distinct(Species))|>
  pivot_wider(names_from = Status, values_from = n_sp, values_fill = 0)
write.csv(data_SpStat_Type1,"C:/Users/antman/Documents/Data R/Imported file/data_SpStat_Type1.csv")

####Calculate the number of endemic vs exotic Sp per type of land use (number of individuals)
data_SpStat_Type2 <- new_data |>
  group_by(Type, Status)|>
  summarise(n_ind=n())|>
  pivot_wider(names_from = Status, values_from = n_ind, values_fill = 0)
write.csv(data_SpStat_Type2,"C:/Users/antman/Documents/Data R/Imported file/data_SpStat_Type2.csv")

###Calculate the number of endemic vs exotic Species per site (number of Species)
data_SpStat_Site1 <- new_data  |>
  group_by(Type, Site, Status) |>
  summarise(n_sp=n_distinct(Species))|>
  pivot_wider(names_from = Status, values_from = n_sp, values_fill = 0)
write.csv(data_SpStat_Site1,"C:/Users/antman/Documents/Data R/Imported file/data_SpStat_Site1.csv")

###Calculate the number of endemic vs exotic Species per site (number of individuals)
data_SpStat_Site2 <- new_data  |>
  group_by(Type, Site, Status) |>
  summarise(n_ind=n())|>
  pivot_wider(names_from = Status, values_from = n_ind, values_fill = 0)
write.csv(data_SpStat_Site2,"C:/Users/antman/Documents/Data R/Imported file/data_SpStat_Site2.csv")




data2 <- read.csv("C:/Users/antman/Documents/Data R/real_data.csv")
####Calculate the number of subfamilies, genera, species and individuals 
#per type and site
table1 <- data2  |>
  group_by(Type, Site) |> 
  summarise(n_sf=n_distinct(Subfamilies), n_gen=n_distinct(Genera),n_sp=n_distinct(Species),n_ind=n())
write.csv(table1,"C:/Users/antman/Documents/Data R/Imported file/table1.csv")

#per type
table2 <- data2  |>
  group_by(Type) |> 
  summarise(n_sf=n_distinct(Subfamilies), n_gen=n_distinct(Genera),n_sp=n_distinct(Species),n_ind=n())

#per type, site and transect
table3 <- data2  |>
  group_by(Type, Site, Transect) |> 
  summarise(n_sf=n_distinct(Subfamilies), n_gen=n_distinct(Genera),n_sp=n_distinct(Species),n_ind=n())
write.csv(table3,"C:/Users/antman/Documents/Data R/Imported file/table3.csv")

####Calculation of average, min and max Genera per transect
##Meth1
tableau <- read.csv("C:/Users/antman/Documents/Data R/Imported file/table3.csv")
Tab <- tableau|>
  group_by(Type, Site)|>
  summarise(n_gen, mean(n_gen), min(n_gen), max(n_gen), n_sp, mean(n_sp), min(n_sp), max(n_sp), n_ind, mean(n_ind), min(n_ind), max(n_ind))
write.csv(Tab,"C:/Users/antman/Documents/Data R/Imported file/tableau.csv")

##Meth2
data_mounting <- read.csv("C:/Users/antman/Documents/Data R/real_data.csv")
str(data_mounting)

Ngenera <- data_mounting |>
  group_by(Type, Site, Transect)|>
  summarise(n_gen=n_distinct(Genera))
write.csv(Ngenera,"C:/Users/antman/Documents/Data R/Imported file/Ngenera.csv")

mean(Ngenera$n_gen)
min(Ngenera$n_gen)
max(Ngenera$n_gen)

Genera <- Ngenre|>
  group_by(Type, Site) |>
  summarise(mean(n_gen), min(n_gen), max(n_gen))
write.csv(Genera,"C:/Users/antman/Documents/Data R/Imported file/Genera.csv")

####Calculation of average, min and max Species per transect
Nspecies <- data_mounting |>
  group_by(Type, Site, Transect)|>
  summarise(n_sp=n_distinct(Species))
write.csv(Nspecies,"C:/Users/antman/Documents/Data R/Imported file/Nspecies.csv")

mean(Nspecies$n_sp)
min(Nspecies$n_sp)
max(Nspecies$n_sp)

Species <- Nspecies|>
  group_by(Type, Site) |>
  summarise(mean(n_sp), min(n_sp), max(n_sp))
write.csv(Species,"C:/Users/antman/Documents/Data R/Imported file/Species.csv")

####Calculation of average, min and max Individuals per transect
Nindividuals <- data_mounting |>
  group_by(Type, Site, Transect)|>
  summarise(n_ind=n())
write.csv(Nindividuals,"C:/Users/antman/Documents/Data R/Imported file/Nindividuals.csv")

mean(Nindividuals$n_ind)
min(Nindividuals$n_ind)
max(Nindividuals$n_ind)

Individuals <- Nindividuals|>
  group_by(Type, Site) |>
  summarise(mean(n_ind), min(n_ind), max(n_ind))
write.csv(Individuals,"C:/Users/antman/Documents/Data R/Imported file/Individuals.csv")

####Figures
# 1) Install packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forcats)

# 2) Import the data 
data2 <- read.csv("C:/Users/antman/Documents/Data R/real_data.csv")

#### For figure 1
## 1) Calculation number of individuals per Species per type of culture- data frame used to plot the results
table4 <- data2  |>
  group_by(Type, Species) |> 
  summarise(n_ind=n())
write.csv(table4,"C:/Users/antman/Documents/Data R/Imported file/table4.csv")

max(table4$n_ind)
mean(table4$n_ind)
min(table4$n_ind)
max(table4$Species)

## 2) convert SpeciesName / Type as factor 
table4$Species <- as_factor(table4$Species)
table4$Type <- as.factor((table4$Type))

## 3) Plot the data to show the distribution of the species in the 3 type of land use(histogram)
fig1 <- ggplot(table4, aes(x= reorder(Species, -n_ind), y = n_ind, yend=0, fill=Type))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3", "Maize 6"), values =c("black", "blue", "red"))+
  ggtitle("Number of individuals per species for each type of culture")+
  scale_x_discrete(name = "Species")+
  scale_y_continuous(name="Number of individuals", breaks = seq(0, 200, 20), limits = c(0, 200), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

   ### Tsy asiana elanelana entre ny bars sy ny axe(y)
theme()+expand = c(0, 0)

## 4) Save the plot into my device   
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_type/fig_type.png", fig1, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_type/fig_type.pdf", fig1)

#### For Figure 2
# 1) Import data 
data2 <- read.csv("C:/Users/antman/Documents/Data R/sites_data.csv")
# 2) To get the data.frame for plot
table5 <- data2|>
  group_by(Sites, Species) |> 
  summarise(n_ind=n())

# 3) export the data.frame to device
write.csv(table5,"C:/Users/antman/Documents/Data R/Imported file/table5.csv")
# 4) Some calculations
min(table5$n_ind)
max(table5$n_ind)
mean(table5$n_ind)

# 5) Convert as factor
table5$Species <- as.factor(table5$Species)
table5$Sites <- as.factor(table5$Sites)

# 6) Plot the distribution of species in the different sites
fig2 <- ggplot(table5, aes(x= reorder(Species, -n_ind), y = n_ind, yend=0, fill=Sites))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3 site1", "Maize 3 site2", "Maize 3 site3", "Maize 6 site1", "Maize 6 site2", "Maize 6 site3"), values =c("black", "skyblue", "royalblue", "darkblue", "pink", "red", "darkred"))+
  ggtitle("Number of individuals per species for each site")+xlab("Species")+ylab("Number of individuals")+
  scale_y_continuous(breaks = seq(0, 200, 20), limits = c(0, 200), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 1, size = 12),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_sites/fig_sites.png", fig2, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_sites/fig_sites.pdf", fig2)


  
##doesn't match
ggplot(table4, aes(x = Species, fill = Type)) +
         geom_bar()

      ###26/04/2023
#####Table and figure for species richness
library(dplyr)
library(ggplot2)
data_rich <- read.csv("C:/Users/antman/Documents/Data R/real_data.csv")
table6 <- data_rich|>
  group_by(Type)|>
  summarise(n_sp=n_distinct(Species))

fig3 <- ggplot(table6, aes(x= Type, y=n_sp, yend=0, fill=Type))+
  geom_col(width = 0.8)+
  scale_y_continuous(breaks = seq(0, 15, 5), limits = c(0, 15), expand = c(0, 0))+
  ggtitle("Ant species richness within the type of land use")+ xlab("Type of land use")+ ylab("Ant species richness")+
  scale_fill_manual(values=c("black", "blue", "red"))+
  theme(panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.x=element_blank())
 
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_richness/fig3.png", fig3, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_richness/fig3.pdf", fig3)

####Table and figure for ant species abundance
table7 <- data_rich|>
  group_by(Type, Site, Transect)|>
  summarise(n_ind=n())

fig4 <- ggplot(table7, aes(x=Type, y=n_ind))+
  geom_jitter(aes(color=Type), width=0.25)+
  scale_color_manual(values = c("Grassland" = "black", "Maize 3" = "blue", "Maize 6"="red"))+
  ggtitle("Number of individuals per transect within the type of land use")+ xlab("Type of land use")+ ylab("Number of individuals")

ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_abundance/fig4.png", fig4, dpi = 300, width = 18, height = 12, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Fig_abundance/fig4.pdf", fig4)  

####Figures showing species status
# 1) Install packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forcats)

# 2) Import the data 
data_status <- read.csv("C:/Users/antman/Documents/Data R/data_status.csv")

#### For figure 5: endemic vs exotic for each habitat type
## 1) Calculation number of individuals per Species per type of culture- data frame used to plot the results
table8 <- data_status  |>
  group_by(Type, Status) |> 
  summarise(n_ind=n())
write.csv(table8,"C:/Users/antman/Documents/Data R/Imported file/table8.csv")

max(table8$n_ind)
mean(table8$n_ind)
min(table8$n_ind)
max(table8$n_ind)

## 2) convert SpeciesName / Type as factor 
table8$Status <- as_factor(table8$Status)
table8$Type <- as.factor((table8$Type))

## 3) Plot the data to show the status of the species in the 3 type of land use(histogram)
fig5 <- ggplot(table8, aes(x= reorder(Type, n_ind), y = n_ind, yend=0, fill=Status))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Endemic", "Native", "Exotic", "Invasive"), values =c("green", "yellow", "purple", "red"))+
  ggtitle("Species status for each habitat type")+
  scale_x_discrete(name = "Habitat")+
  scale_y_continuous(name="Number of individuals", breaks = seq(0, 300, 20), limits = c(0, 300), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0),
        axis.text.x =element_text(color="black", size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

## 4) Save the plot into my device   
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/ind_type/ind_type.png", fig5, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/ind_type/ind_type.pdf", fig5)


#### For figure 6: endemic vs exotic (number of species)
## 1) Calculation number of individuals per Species per type of culture- data frame used to plot the results
table9 <- data_status  |>
  group_by(Type, Status) |> 
  summarise(n_sp=n_distinct(Species))
write.csv(table9,"C:/Users/antman/Documents/Data R/Imported file/table9.csv")

max(table9$n_sp)
mean(table9$n_sp)
min(table9$n_sp)


## 2) convert SpeciesName / Type as factor 
table9$Status <- as_factor(table9$Status)
table9$Type <- as.factor((table9$Type))

## 3) Plot the data to show the status of the species in the 3 type of land use(histogram)
fig6 <- ggplot(table9, aes(x= reorder(Type, -n_sp), y = n_sp, yend=0, fill=Status))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Endemic", "Native", "Exotic", "Invasive"), values =c("green", "yellow", "purple", "red"))+
  ggtitle("Species status for each habitat type")+
  scale_x_discrete(name = "Habitat")+
  scale_y_continuous(name="Number of Species", breaks = seq(0, 15, 1), limits = c(0, 15), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0),
        axis.text.x =element_text(color="black", size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

## 4) Save the plot into my device   
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/sp_type/sp_type.png", fig6, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/sp_type/sp_type.pdf", fig6)

#### For Figure 7: endemic vs exotic for each habitat (number of individuals)
# 1) Import data 
data_status2 <- read.csv("C:/Users/antman/Documents/Data R/sites_data.csv")
# 2) To get the data.frame for plot
table10 <- data_status2|>
  group_by(Sites, Status) |> 
  summarise(n_ind=n())

# 3) export the data.frame to device
write.csv(table10,"C:/Users/antman/Documents/Data R/Imported file/table10.csv")

# 4) Some calculations
min(table10$n_ind)
max(table10$n_ind)
mean(table10$n_ind)

# 5) Convert as factor
table10$Species <- as.factor(table10$Status)
table10$Sites <- as.factor(table10$Sites)

# 6) Plot the status of species in the different sites
fig7 <- ggplot(table10, aes(x= Sites, y = n_ind, yend=0, fill=Status))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Endemic", "Native", "Exotic", "Invasive"), values =c("green", "yellow", "purple", "red"))+
  ggtitle("Status of species for each site")+xlab("Sites")+ylab("Number of individuals")+
  scale_y_continuous(breaks = seq(0, 200, 20), limits = c(0, 200), expand = c(0, 0))+
  theme(plot.title = element_text(size = 12, face="bold"),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/ind_sites/ind_sites.png", fig7, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/ind_sites/ind_sites_sites.pdf", fig7)

#### For Figure 8: endemic vs exotic for each habitat (number of Species)
# 1) Import data 
data_status2 <- read.csv("C:/Users/antman/Documents/Data R/sites_data.csv")
# 2) To get the data.frame for plot
table11 <- data_status2|>
  group_by(Sites, Status) |> 
  summarise(n_sp=n_distinct(Species))

# 3) export the data.frame to device
write.csv(table11,"C:/Users/antman/Documents/Data R/Imported file/table11.csv")

# 4) Some calculations
min(table11$n_sp)
max(table11$n_sp)
mean(table11$n_sp)

# 5) Convert as factor
table11$Status <- as.factor(table10$Status)
table11$Sites <- as.factor(table10$Sites)

# 6) Plot the status of species in the different sites
fig8 <- ggplot(table11, aes(x= Sites, y = n_sp, yend=0, fill=Status))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Endemic", "Native", "Exotic", "Invasive"), values =c("green", "yellow", "purple", "red"))+
  ggtitle("Status of species for each site")+xlab("Sites")+ylab("Number of Species")+
  scale_y_continuous(breaks = seq(0, 15, 1), limits = c(0, 15), expand = c(0, 0))+
  theme(plot.title = element_text(size = 12, face="bold"),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/sp_sites/sp_sites.png", fig8, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Status/sp_sites/sp_sites.pdf", fig8)


###Calculate the number of Subfamilies (here I have used the data "For_Species")
##per type of culture (habitat)
new_data <- read.csv("C:/Users/antman/Documents/Data R/For_Species.csv")
view_sf <- new_data |>
  group_by(Type)|>
  summarise(n_sf=n_distinct(Subfamilies))

table12 <- new_data |>
  group_by(Type, Subfamilies)|>
  summarise(n_ind=n())

write.csv(table12,"C:/Users/antman/Documents/Data R/Imported file/data_Sf_Type.csv")
##Figure: number of individuals per subfamilies within habitat
fig9 <- ggplot(table12, aes(x= reorder(Subfamilies, -n_ind), y = n_ind, yend=0, fill=Type))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3", "Maize 6"), values =c("black", "blue", "red"))+
  ggtitle("Number of individuals per subfamilies for each type of culture")+
  scale_x_discrete(name = "Subfamilies")+
  scale_y_continuous(name="Number of individuals", breaks = seq(0, 600, 50), limits = c(0, 600), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sf_type.png", fig9, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sf_type.pdf", fig9)

##per sites
data2 <- read.csv("C:/Users/antman/Documents/Data R/sites_data.csv")
#To get the data.frame for plot
table13 <- data2|>
  group_by(Sites, Subfamilies) |> 
  summarise(n_ind=n())

#Figure: number of individuals per subfamilies within the sites
ggplot(table13, aes(x= reorder(Subfamilies, -n_ind), y = n_ind, yend=0, fill=Sites))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3 site1", "Maize 3 site2", "Maize 3 site3", "Maize 6 site1", "Maize 6 site2", "Maize 6 site3"), values =c("black", "skyblue", "royalblue", "darkblue", "pink", "red", "darkred"))+
  ggtitle("Number of individuals per subfamilies for each site")+xlab("Subfamilies")+ylab("Number of individuals")+
  scale_y_continuous(breaks = seq(0, 600, 50), limits = c(0, 600), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 1, size = 12),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sf_sites.png", fig10, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sf_sites.pdf", fig10)


####to have the number of species for each subfamily in each habitat and each site
data <- read.csv("C:/Users/antman/Documents/Data R/real_data.csv")
##per habitat
#To get the data.frame for plot
table14 <- data|>
  group_by(Type, Subfamilies) |> 
  summarise(n_sp=n_distinct(Species))

#Figure: number of species per subfamilies wthin habitats
fig11 <- ggplot(table14, aes(x= reorder(Subfamilies, -n_sp), y = n_sp, yend=0, fill=Type))+
  geom_col(width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3", "Maize 6"), values =c("black", "blue", "red"))+
  ggtitle("Number of species per subfamily for each habitat")+xlab("Subfamilies")+ylab("Number of species")+
  scale_y_continuous(breaks = seq(0, 30, 2), limits = c(0, 30), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 1, size = 12),
        axis.text.x =element_text(color="black", angle = 60, hjust=1, size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sp_sf_type.png", fig11, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sp_sf_type.pdf", fig11)


##per sites
data2 <- read.csv("C:/Users/antman/Documents/Data R/sites_data.csv")
#To get the data.frame for plot
table15 <- data2|>
  group_by(Sites, Subfamilies) |> 
  summarise(n_sp=n_distinct(Species))

#Figure: number of species per subfamilies within sites
fig12 <- ggplot(table15, aes(x= reorder(Subfamilies, -n_sp), y = n_sp, yend=0, fill=Sites))+
  geom_col(position="stack" , width = 0.9)+
  scale_fill_continuous(guide= guide_legend(label.position = "left"))+
  scale_fill_manual(breaks= c("Grassland", "Maize 3 site1", "Maize 3 site2", "Maize 3 site3", "Maize 6 site1", "Maize 6 site2", "Maize 6 site3"), values =c("black", "skyblue", "royalblue", "darkblue", "pink", "red", "darkred"))+
  ggtitle("Number of species per subfamilies for each site")+xlab("Subfamilies")+ylab("Number of species")+
  scale_y_continuous(breaks = seq(0, 55, 5), limits = c(0, 55), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 1, size = 12),
        axis.text.x =element_text(color="black", size = 10, margin = margin(t=0)),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        axis.line.y= element_line(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size=10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"))

# 7) Save the plots
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sp_sf_sites.png", fig12, dpi = 300, width = 15, height = 13, units = c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/Subfamilies/sp_sf_sites.pdf", fig12)



#####Rarefaction curves with my final real data
library(iNEXT)
library(ggplot2)
library(dplyr)
data_rare <- read.csv("C:/Users/antman/Documents/Data R/mydata_rare.csv")
data2 <- data_rare[, -1]
q0 <- iNEXT(data2, q=0, datatype="abundance")
ggiNEXT(q0, type=1)
ggiNEXT(q0, type=2)
ggiNEXT(q0, type=3)

##Type=1, plot the species diversity(y-axis) by the number of individuals (x-axis)
type1 <- iNEXT(data2, q=c(0,1,2), datatype = "abundance")
type1_1 <- ggiNEXT(type1, type = 1, facet.var = "Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type1/type1_1.png", plot=type1_1, width =18, height = 12, units=c("cm"))

type1_2 <-ggiNEXT(type1, type=1, facet.var="Order.q", color.var="Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type1/type1_2.png", plot=type1_2, width =18, height = 12, units=c("cm"))
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type1/type1_2.pdf", plot=type1_2)

##Type=2, plot the sample coverage (y-axis) by the number of individuals (x-axis)
type2 <- iNEXT(data2, q=c(0,1,2), datatype = "abundance")
type2_1 <- ggiNEXT(type2, type = 2, facet.var = "Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type2/type2_1.png", plot=type2_1, width =18, height = 12, units=c("cm"))

type2_2 <- ggiNEXT(type2, type=2, facet.var="Order.q", color.var="Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type2/type2_2.png", plot=type2_2, width =18, height = 12, units=c("cm"))

##Type=3, plot the Species diversity (y-axis) by the sample coverage (y-axis)
type3 <- iNEXT(data2, q=c(0,1,2), datatype="abundance")
type3_1 <- ggiNEXT(type3, type=3, facet.var = "Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type3/type3_1.png", plot=type3_1, width =18, height = 12, units=c("cm"))

type3_2 <- ggiNEXT(type3, type=1, facet.var="Order.q", color.var="Assemblage")
ggsave("C:/Users/antman/Documents/Plots from R/Figures/rarefaction/Type3/type3_2.png", plot=type3_2, width =18, height = 12, units=c("cm"))

#11/05/2023
####Exercise with the data+variables
library(ggplot2)
###Create data for 
#Case1
nn <- 50
skin <- sample(c("pale", "light", "black", "white", "brown", "dark"), nn, replace=TRUE)
eyes <- sample(c("blue", "green", "black", "brown"), nn, replace=TRUE)

data_case1 <- data.frame(skin, eyes)
View(data_case1)
write.csv(data_case1,"C:/Users/antman/Documents/Data R/Imported file/data_case1.csv")

data2 <- data_case1|>
  group_by(skin, eyes)|>
  summarise(n=n())|>
  pivot_wider(names_from = skin, values_from = n, values_fill = 0)
chisq.test(data2[,-1])
str(data2)

ggplot(data_case1, aes(x=eyes, fill=skin))+
  geom_bar(position = "dodge")

results <- chisq.test(data_case1$skin, data_case1$eyes)


#Case2
nn <- 50
sites <- sample(c("grassland", "eucalyptus", "maize", "jatropha"), nn, replace=TRUE)
individuals <- rpois(nn, 50)

data_case2 <- data.frame(sites, individuals)
View(data_case2)
write.csv(data_case2,"C:/Users/antman/Documents/Data R/Imported file/data_case2.csv")



ggplot(data_case2, aes(x= sites, y=individuals))+
  geom_col()

nat <- c(rep("M", 50), rep("S", 50))
skin <- c(rep("D", 49), rep('C', 1),rep("C", 48), rep("D", 2))
eyes <- c(rep("bu", 1), rep("br", 49),rep("bu", 47), rep("br", 3))
data <- data.frame(nat, skin, eyes)
chisq.test(data$skin, data$eyes)

ggplot(data, aes(x= skin, fill=eyes))+
  geom_bar(position = "dodge")

data2 <- data|>
  group_by(skin, eyes)|>
  summarise(n=n())|>
  pivot_wider(names_from = skin, values_from = n, values_fill = 0)
chisq.test(data2[,-1])
str(data2)

m1 <- matrix(c(10,20,20,10), nrow = 2, ncol = 2)
chisq.test(m1)
