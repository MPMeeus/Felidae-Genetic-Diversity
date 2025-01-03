###Code for felid heterozygosity analysis###

###############
#LOAD PACKAGES#
###############
library(tidyverse)
library(scales)
library(flextable)
library(flexlsx)
library(rphylopic)
library(ape)
library(GGally)
library(ggpmisc)
library(car)
library(tree)
library(sensemakr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggsignif)
library(ggpubr)
library(patchwork)


##################
#DATA PREPARATION#
##################

#Putting data from csv in variables
samples<-read.csv("Sample_info_manuscript.csv", header = T, sep = ";")
ecology <- read.csv("Felid_Ecological_Data.csv", header = T, sep = ";",na.strings="")
vcf<-read.csv("Felid_Heterozygosity_manuscript.csv", header = T, sep = ";")
hetcomp<-read.csv("Het_Comp_manuscript.csv", header = T, sep = ";")

#Merging data together for analysis
data<-merge(samples, vcf, by = "unique_id")
datae_nf<-merge(data, ecology, by = "Species")

#Exclude samples with coverage below 7
datae <- datae_nf %>% filter(Coverage_cat > 7 | Coverage_tiger > 7)

#Create datasets for the comparison between reference genomes
covcat <- data.frame(datae_nf['unique_id'], datae_nf['Coverage_cat'], datae_nf['Coverage_tiger'])
comp_nf<-merge(samples, hetcomp, by = "unique_id")
comp_nf<-merge(comp_nf, ecology, by = "Species")
comp_nf<-merge(comp_nf, covcat, by = "unique_id")

#Exclude samples with coverage below 7 for both genomes
comp <- comp_nf %>% filter(Coverage_cat > 7 | Coverage_tiger > 7)


#Add a distance column to the comparison dataframe
attach(comp)
comp_ref<-data.frame(unique_id, Subfamily, genus, species, heterozygosity, reference_genome)
comp_ref<-comp_ref %>% mutate(distance = case_when(Subfamily == "Felinae" & reference_genome == "Housecat" ~ "Close",
                                                   Subfamily == "Pantherinae" & reference_genome == "Tiger" ~ "Close",
                                                   Subfamily == "Felinae" & reference_genome == "Tiger" ~ "Distant",
                                                   Subfamily == "Pantherinae" & reference_genome == "Housecat" ~ "Distant"))

comp_dist<-comp_ref %>% filter(distance == "Distant")
comp_dist<-rename(comp_dist, heterozygosity_dist = heterozygosity)


comp_close<-comp_ref %>% filter(distance == "Close")
comp_close<-rename(comp_close, heterozygosity_close = heterozygosity)

comp_hist<-merge(comp_close, comp_dist, by = "unique_id")
comp_hist["Distance_diff"] <- comp_hist["heterozygosity_dist"] - comp_hist["heterozygosity_close"]
comp_hist["Percent_diff"] <- (comp_hist["Distance_diff"]/comp_hist["heterozygosity_close"])


#Create table and set values in percentages
Comparison<-data.frame(comp_hist$unique_id, comp_hist$heterozygosity_close, comp_hist$heterozygosity_dist, comp_hist$Distance_diff, comp_hist$Percent_diff)
colnames(Comparison) <- c("Unique id", "Heterozygosity (close reference)", "Heterozygosity (distant reference)", "Difference (pp)", "Percent difference")
Comparison$`Heterozygosity (close reference)`<-label_percent()(Comparison$`Heterozygosity (close reference)`)
Comparison$`Heterozygosity (distant reference)`<-label_percent()(Comparison$`Heterozygosity (distant reference)`)
Comparison$`Percent difference`<-label_percent()(Comparison$`Percent difference`)
Comparison$`Difference (pp)`<-Comparison$`Difference (pp)` * 100
Comparison<-flextable(Comparison)
Comparison<-width(Comparison, j = c(2,3), 1.5)
comparison<-align(Comparison, align = "center")
Comparison


#Save comparison as an Excel-file in the temp folder 
excel <- openxlsx2::wb_workbook()$add_worksheet("Comparison")
excel <- wb_add_flextable(excel, "Comparison", Comparison)
tmpfile <- tempfile(fileext = ".xlsx")
excel$save(tmpfile)


#Make data easily accessible
attach(datae)

#Determine mean He value per species
mean_felid<-datae
mean_felids<-mean_felid %>%
  group_by(species) %>%
  summarise_at(vars(-unique_id), funs(mean(., na.rm=TRUE)))


#Determine mean He value per subspecies
mean_sub<-datae
mean_subs<-mean_sub %>%
  group_by(Species) %>%
  summarise_at(vars(-unique_id), funs(mean(., na.rm=TRUE)))

#Determine mean He value in regards to density following the protocol
datae_den<-datae

for(i in 1:length(datae_den$Density)){
  if(is.na(datae_den$Density[i])){
    datae_den$Density[i] <- datae_den$Density_sp[i]
    datae_den$Species[i] <- datae_den$species[i]
  }
}


datae_den<-as.tbl(datae_den) %>% 
  bind_rows(datae_den) %>% 
  group_by(Species, Density) %>%
  summarise_all(c("mean")) %>%
  drop_na(Density)



#Determine mean He value in regards to census size following the protocol
datae_cen<-datae

for(i in 1:length(datae_cen$Census_size)){
  if(is.na(datae_cen$Census_size[i])){
    datae_cen$Census_size[i] <- datae_cen$Census_sp[i]
    datae_cen$Species[i] <- datae_cen$species[i]
  }
}



datae_cen<-as.tbl(datae_cen) %>% 
  bind_rows(datae_cen) %>% 
  group_by(Species, Census_size) %>% 
  summarise_all(c("mean")) %>%
  drop_na(Census_size)


#Add column with italicized species names
datae<-datae %>% 
  mutate(StyledSpecies = stringr::str_glue("<i>{datae$species}<i>"))


#Add column with italicized subspecies names
datae<-datae %>% 
  mutate(StyledSubspecies = stringr::str_glue("<i>{datae$Species}<i>"))



for (i in 1:length(datae$Captive)){
  datae$Captive[datae$Captive == "Yes"] <- "Captive"
  datae$Captive[datae$Captive == "No"] <- "Non-captive"
}




#Prepare data for IUCN comparison using mean subspecies values of heterozygosity
mean_IUCN<-datae
mean_IUCN<-as.tbl(mean_IUCN) %>% 
  bind_rows(mean_IUCN) %>% 
  group_by(Species, IUCN_status) %>% 
  summarise_all(c("mean"))



#Function to grab outliers for IUCN
#Helps label outliers on plots for identification purposes
is_outlier_mean_IUCN <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
mean_IUCN<-as.data.frame(mean_IUCN)
rownames(mean_IUCN)<-mean_IUCN[,1]


#Adapt datae to label outliers for IUCN comparison
#Necessary to label outliers
mean_IUCN <- mean_IUCN %>% tibble::rownames_to_column(var="outlier_mean_IUCN") %>%
  group_by(IUCN_status) %>%
  mutate(is_outlier_mean_IUCN=ifelse(is_outlier_mean_IUCN(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
mean_IUCN$outlier_mean_IUCN[which(is.na(mean_IUCN$is_outlier_mean_IUCN))] <- as.numeric(NA)


mean_IUCN$IUCN_status <- factor(mean_IUCN$IUCN_status,
                                levels = c("Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered"),ordered = TRUE)


#Filtering the data for the different IUCN groups
meaniucnlc <-mean_IUCN %>% filter(IUCN_status == "Least Concern")
meaniucne <-mean_IUCN %>% filter(IUCN_status == "Endangered")
meaniucnv <-mean_IUCN %>% filter(IUCN_status == "Vulnerable")
meaniucnnt <-mean_IUCN %>% filter(IUCN_status == "Near Threatened")
meaniucnce <-mean_IUCN %>% filter(IUCN_status == "Critically Endangered")


#Creating a "threatened" and "non-threatened" group
meaniucn_safe<-rbind(meaniucnlc, meaniucnnt)
safe<-c(rep("Non-threatened", length(meaniucn_safe$heterozygosity_cat)))
safe<-as.data.frame(safe)
colnames(safe)<-"Safety"
meaniucn_safe<-cbind(meaniucn_safe, safe)

meaniucn_threat<-rbind(meaniucnv, meaniucne, meaniucnce)
threat<-c(rep("Threatened", length(meaniucn_threat$heterozygosity_cat)))
threat<-as.data.frame(threat)
colnames(threat)<-"Safety"
meaniucn_threat<-cbind(meaniucn_threat, threat)

#Combine the two groups into one dataframe
meaniucnbox<-rbind(meaniucn_safe, meaniucn_threat)
meaniucnbox<-as.data.frame(meaniucnbox)


#Function to grab outliers for threat status
#Helps label outliers on plots for identification purposes
is_outlier_mean_threat <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
rownames(meaniucnbox)<-meaniucnbox[,2]


#Adapt datae to label outliers for threat comparison
#Necessary to label outliers
attach(meaniucnbox)
meaniucnbox <- meaniucnbox %>% tibble::rownames_to_column(var="outlier_mean_threat") %>%
  group_by(Safety) %>%
  mutate(is_outlier_mean_threat=ifelse(is_outlier_mean_threat(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
meaniucnbox$outlier_mean_threat[which(is.na(meaniucnbox$is_outlier_mean_threat))] <- as.numeric(NA)


#Prepare data for IUCN comparison using mean subspecies values of heterozygosity of non-captive individuals
mean_IUCN_nc<-datae
mean_IUCN_nc <- mean_IUCN_nc %>% filter(Captive=="Non-captive")
mean_IUCN_nc <- as.tbl(mean_IUCN_nc) %>%
  bind_rows(mean_IUCN_nc) %>%
  group_by(Species, IUCN_status) %>% 
  summarise_all(c("mean"))



#Function to grab outliers for IUCN
#Helps label outliers on plots for identification purposes
is_outlier_mean_IUCN_nc <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
mean_IUCN_nc<-as.data.frame(mean_IUCN_nc)
rownames(mean_IUCN_nc)<-mean_IUCN_nc[,1]


#Adapt datae to label outliers for IUCN comparison
#Necessary to label outliers
mean_IUCN_nc <- mean_IUCN_nc %>% tibble::rownames_to_column(var="outlier_mean_IUCN_nc") %>%
  group_by(IUCN_status) %>%
  mutate(is_outlier_mean_IUCN_nc=ifelse(is_outlier_mean_IUCN_nc(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
mean_IUCN_nc$outlier_mean_IUCN_nc[which(is.na(mean_IUCN_nc$is_outlier_mean_IUCN_nc))] <- as.numeric(NA)


mean_IUCN_nc$IUCN_status <- factor(mean_IUCN_nc$IUCN_status,
                                   levels = c("Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered"),ordered = TRUE)


#Filtering the data for the different IUCN groups
meaniucn_nc_lc <-mean_IUCN_nc %>% filter(IUCN_status == "Least Concern")
meaniucn_nc_e <-mean_IUCN_nc %>% filter(IUCN_status == "Endangered")
meaniucn_nc_v <-mean_IUCN_nc %>% filter(IUCN_status == "Vulnerable")
meaniucn_nc_nt <-mean_IUCN_nc %>% filter(IUCN_status == "Near Threatened")
meaniucn_nc_ce <-mean_IUCN_nc %>% filter(IUCN_status == "Critically Endangered")


#Creating a "threatened" and "non-threatened" group
meaniucn_nc_safe<-rbind(meaniucn_nc_lc, meaniucn_nc_nt)
safe_nc<-c(rep("Non-threatened", length(meaniucn_nc_safe$heterozygosity_cat)))
safe_nc<-as.data.frame(safe_nc)
colnames(safe_nc)<-"Safety"
meaniucn_nc_safe<-cbind(meaniucn_nc_safe, safe_nc)

meaniucn_nc_threat<-rbind(meaniucn_nc_v, meaniucn_nc_e, meaniucn_nc_ce)
threat_nc<-c(rep("Threatened", length(meaniucn_nc_threat$heterozygosity_cat)))
threat_nc<-as.data.frame(threat_nc)
colnames(threat_nc)<-"Safety"
meaniucn_nc_threat<-cbind(meaniucn_nc_threat, threat_nc)

#Combine the two groups into one dataframe
meaniucn_nc_box<-rbind(meaniucn_nc_safe, meaniucn_nc_threat)
meaniucn_nc_box<-as.data.frame(meaniucn_nc_box)


#Function to grab outliers for threat status
#Helps label outliers on plots for identification purposes
is_outlier_mean_threat_nc <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
rownames(meaniucn_nc_box)<-meaniucn_nc_box[,2]


#Adapt datae to label outliers for threat comparison
#Necessary to label outliers
attach(meaniucn_nc_box)
meaniucn_nc_box <- meaniucn_nc_box %>% tibble::rownames_to_column(var="outlier_mean_threat_nc") %>%
  group_by(Safety) %>%
  mutate(is_outlier_mean_threat_nc=ifelse(is_outlier_mean_threat_nc(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
meaniucn_nc_box$outlier_mean_threat_nc[which(is.na(meaniucn_nc_box$is_outlier_mean_threat_nc))] <- as.numeric(NA)



#Prepare data for captivity comparison using mean subspecies values of heterozygosity
#Across the family and across genus Panthera
Captivity<-datae
Panther<-Captivity %>% filter(Lineage == "Panthera")

#function to grab outliers for threat status
#Helps label outliers on plots for identification purposes
is_outlier_captive <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
rownames(Captivity)<-Captivity[,2]


#Adapt datae to label outliers for threat comparison
#Necessary to label outliers
attach(Captivity)
Captivity <- Captivity %>% tibble::rownames_to_column(var="outlier_captive") %>%
  group_by(Captive) %>%
  mutate(is_outlier_captive=ifelse(is_outlier_captive(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
Captivity$outlier_captive[which(is.na(Captivity$is_outlier_captive))] <- as.numeric(NA)


#Function to grab outliers for threat status
#Helps label outliers on plots for identification purposes
is_outlier_panther <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Turn unique_id into rownames before using tibble
#Necessary to label outliers
rownames(Panther)<-Panther[,2]


#Adapt datae to label outliers for threat comparison
#Necessary to label outliers
attach(Panther)
Panther <- Panther %>% tibble::rownames_to_column(var="outlier_panther") %>%
  group_by(Captive) %>%
  mutate(is_outlier_panther=ifelse(is_outlier_panther(heterozygosity_cat), heterozygosity_cat, as.numeric(NA)))
Panther$outlier_panther[which(is.na(Panther$is_outlier_panther))] <- as.numeric(NA)



#Function to add sample sizes to plots
sample.size<- function(x){
  return(c(y = mean(x^2), label = length(x)))
}


#Get phylopic images
Ctem <- get_phylopic(uuid = "a42bad64-5ded-489e-9aa4-b3d93d6db7ab")
Ccar <- get_phylopic(uuid = "22f9f707-172a-4795-9092-c11120ace165")
Pviv <- get_phylopic(uuid = "efb7c00a-16f0-4311-9af8-3fe37e73f514")
Llyn <- get_phylopic(uuid = "27a2173a-5903-46fc-83c5-29ed7f421046")
Ltig <- get_phylopic(uuid = "3961f620-a0b4-448c-b69c-8b5c6595e6c4")
Pleo <- get_phylopic(uuid = "dd7c7bc6-2c6d-48e4-860b-02bf37886b5b")
Pcon <- get_phylopic(uuid = "3f8eff77-2868-4121-8d7d-a55ebdd49e04")
Fsil <- get_phylopic(uuid = "cc22d71c-a014-41a9-aaf1-5f2cffad6121")

get_attribution("a42bad64-5ded-489e-9aa4-b3d93d6db7ab")
get_attribution("22f9f707-172a-4795-9092-c11120ace165")
get_attribution("efb7c00a-16f0-4311-9af8-3fe37e73f514")
get_attribution("27a2173a-5903-46fc-83c5-29ed7f421046")
get_attribution("3961f620-a0b4-448c-b69c-8b5c6595e6c4")
get_attribution("dd7c7bc6-2c6d-48e4-860b-02bf37886b5b")
get_attribution("3f8eff77-2868-4121-8d7d-a55ebdd49e04")
get_attribution("cc22d71c-a014-41a9-aaf1-5f2cffad6121")

Lin <- c("Bay cat","Caracal","Leopard cat","Lynx","Ocelot","Panthera","Puma","Wildcat")
uuid <- c("a42bad64-5ded-489e-9aa4-b3d93d6db7ab","22f9f707-172a-4795-9092-c11120ace165","efb7c00a-16f0-4311-9af8-3fe37e73f514","27a2173a-5903-46fc-83c5-29ed7f421046","3961f620-a0b4-448c-b69c-8b5c6595e6c4","dd7c7bc6-2c6d-48e4-860b-02bf37886b5b","3f8eff77-2868-4121-8d7d-a55ebdd49e04","cc22d71c-a014-41a9-aaf1-5f2cffad6121")


silhouette_df <- data.frame(Lin, uuid)
names(silhouette_df) <- c("Lin", "uuid")
silhouette_df$svg <- lapply(silhouette_df$uuid, get_phylopic)
attach(silhouette_df)

Ctem_df <- data.frame(x = 1, y = 1, Lineage = "Bay cat")
Ccar_df <- data.frame(x = 1, y = 1, Lineage = "Caracal")
Pviv_df <- data.frame(x = 1, y = 1, Lineage = "Leopard cat")
Llyn_df <- data.frame(x = 1, y = 1, Lineage = "Lynx")
Ltig_df <- data.frame(x = 1, y = 1, Lineage = "Ocelot")
Pleo_df <- data.frame(x = 1, y = 1, Lineage = "Panthera")
Pcon_df <- data.frame(x = 1, y = 1, Lineage = "Puma")
Fsil_df <- data.frame(x = 1, y = 1, Lineage = "Wildcat")


#Create phylogenetic tree (based on Li et al. (2016))
felid_tree<-read.tree(text = "(((((((((((Felis_catus:1.12, Felis_silvestris:1.12):1.52, (Felis_lybica:1.18, Felis_bieti:1.18):1.52):2.67, Felis_margarita:2.67):3.28, Felis_nigripes:3.28):4.23, Felis_chaus:4.23):7.25,
                      (((((Prionailurus_bengalensis:1.30, Prionailurus_javanensis:1.30):1.79, Prionailurus_viverrinus:1.79):2.40, Prionailurus_planiceps:2.40):3.76, Prionailurus_rubiginosus:3.67):5.95, Otocolobus_manul:5.95):7.25):8.17,
                      ((Herpailurus_yagouaroundi:3.26, Puma_concolor:3.26):4.99, Acinonyx_jubatus:4.99):8.17):8.67,
                      (((Catopuma_temminckii:3.27, Catopuma_badia:3.27):5.50, Pardofelis_marmorata:5.50):8.08,
                      (((Lynx_lynx:1.09, Lynx_pardinus:1.09):1.61, Lynx_canadensis:1.61):3.48, Lynx_rufus:3.48):8.08):8.67):9.81,
                      (((Leopardus_pardalis:2.47, Leopardus_wiedii:2.47):2.91, Leopardus_jacobita:2.91):3.14, ((((Leopardus_guigna:0.86, Leopardus_geoffroyi:0.86):1.33, Leopardus_guttulus:1.33):1.65,(Leopardus_tigrinus:1, Leopardus_emiliae:1):1.65):2.67, Leopardus_colocola:2.67):3.14):9.81):10.67,
                      ((Caracal_aurata:1.80, Caracal_caracal:1.80):6.28, Leptailurus_serval:6.28):10.67):11.46,
                      ((((Panthera_leo:1.86, Panthera_pardus:1.86):2.73, Panthera_onca:2.73):3.72, (Panthera_tigris:2.67, Panthera_uncia:2.67):3.72):5.67, (Neofelis_nebulosa:1.99, Neofelis_diardi:1.99):5.67):11.46);")


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

######################
#Statistical analysis#
######################

###Power regression
attach(datae_nf)

#Plot heterozygosity as a function of Coverage
plot(heterozygosity_cat~Coverage_cat)

#Estimate best values for a & b
PowerLaw<-nls(heterozygosity_cat~a*I(Coverage_cat^b), data = datae_nf, start = list(a = 1, b = 1), trace = T)

#Grab values for a & b
exp_a <- environment(PowerLaw[["m"]][["resid"]])[["env"]][["a"]]
exp_a <- round(exp_a, 6)
exp_a <- toString(exp_a)

exp_b <- environment(PowerLaw[["m"]][["resid"]])[["env"]][["b"]]
exp_b <- round(exp_b, 6)
exp_b <- toString(exp_b)

#Write formula to add to plot
power_formula<-str_glue("y == {exp_a} * x^{exp_b}")

#Carry out regression analysis by using logarithm of heterozygosity and coverage
lm_power<-lm(log(heterozygosity_cat)~log(Coverage_cat), data = datae_nf)
par(mfrow = c(2,2))
plot(lm_power)
summary(lm_power)


###Linear regression###

attach(mean_subs)


###Male mass
lm_mass<-lm(heterozygosity_cat~log10(Mass_M), data = mean_subs)
plot(lm_mass)
summary(lm_mass)
cor(log10(Mass_M), mean_subs$heterozygosity_cat, use="pairwise.complete.obs")


###Male size
lm_size<-lm(heterozygosity_cat~Size_M_nt, data = mean_subs)
plot(lm_size)
summary(lm_size)
cor(Size_M_nt, mean_subs$heterozygosity_cat, use="pairwise.complete.obs")


###Geographic range
lm_range<-lm(heterozygosity_cat~Range, data = mean_subs)
plot(lm_range)
summary(lm_range)
cor(Range, mean_subs$heterozygosity_cat, use="pairwise.complete.obs")


attach(mean_felids)

####Gestation time
lm_gestation<-lm(heterozygosity_cat~Gestation_sp, data = mean_felids)
plot(lm_gestation)
summary(lm_gestation)
cor(Gestation_sp, mean_felids$heterozygosity_cat, use="pairwise.complete.obs")


####Lifespan
lm_longevity<-lm(heterozygosity_cat~Longevity_sp, data = mean_felids)
plot(lm_longevity)
summary(lm_longevity)
cor(Longevity_sp, mean_felids$heterozygosity_cat, use="pairwise.complete.obs")


###Litter size
lm_littersize<-lm(heterozygosity_cat~Litter_size_sp, data = mean_felids)
plot(lm_littersize)
summary(lm_littersize)
cor(Litter_size_sp, mean_felids$heterozygosity_cat, use="pairwise.complete.obs")


###Population density
lm_density<-lm(heterozygosity_cat~Density, data = datae_den)
plot(lm_density)
datae_den$Species[14]


#Filter out first outlier
datae_den_filt1<-datae_den %>% filter(Species != "Leopardus guigna")
lm_density1<-lm(heterozygosity_cat~Density, data = datae_den_filt1)
plot(lm_density1)
datae_den_filt1$Species[4]


#Filter out second outlier
datae_den_filt2<-datae_den_filt1 %>% filter(Species != "Felis chaus")
lm_density2<-lm(heterozygosity_cat~Density, data = datae_den_filt2)

#Save the data under more general name
datae_den_filt<-datae_den_filt2 

plot(lm_density2)
summary(lm_density2)
cor(datae_den_filt2$Density, datae_den_filt2$heterozygosity_cat, use="pairwise.complete.obs")


#Alternatively, use more conservative value for guiña
datae_den_con<-datae_den
datae_den_con['14', 'Density'] <- 41
lm_density_con<-lm(heterozygosity_cat~Density, data = datae_den_con)

plot(lm_density_con)
summary(lm_density_con)
cor(datae_den_con$Density, datae_den_con$heterozygosity_cat, use="pairwise.complete.obs")


#Update dataframe to create regression plot
datae_den$Filter <- "Unfiltered"
datae_den_filt$Filter <- "Filtered"
Denplot<-rbind(datae_den, datae_den_filt)


Denplot2<-Denplot
for (i in 1:length(Denplot$Density)){
  if(Denplot$Filter[i] == "Filtered"){
    Denplot2$Density[i] <- Denplot2$Density[i] * (max(Denplot$Density[Denplot$Filter == "Unfiltered"])/max(Denplot$Density[Denplot$Filter == "Filtered"]))
  }
}  


###Population census size
lm_census<-lm(heterozygosity_cat~Census_size, data = datae_cen)
plot(lm_census)
datae_cen$Species[15]


#Filter out first outlier
datae_cen_filt1<-datae_cen %>% filter(Species != "Lynx rufus")
lm_census1<-lm(heterozygosity_cat~Census_size, data = datae_cen_filt1)
datae_cen_filt1$Species[6]


#Filter out second outlier
datae_cen_filt2<-datae_cen_filt1 %>% filter(Species != "Felis silvestris")
lm_census2<-lm(heterozygosity_cat~Census_size, data = datae_cen_filt2)
datae_cen_filt2$Species[10]


#Filter out third outlier
datae_cen_filt3<-datae_cen_filt2 %>% filter(Species != "Leopardus pardalis")
lm_census3<-lm(heterozygosity_cat~Census_size, data = datae_cen_filt3)

#Save data under more general name
datae_cen_filt<-datae_cen_filt3 

plot(lm_census3)
summary(lm_census3)
cor(datae_cen_filt3$Census_size, datae_cen_filt3$heterozygosity_cat, use="pairwise.complete.obs")


#Update dataframe to create regression plot
datae_cen$Filter <- "Unfiltered"
datae_cen_filt$Filter <- "Filtered"
Cenplot <- rbind(datae_cen, datae_cen_filt) 


Cenplot2<-Cenplot
for (i in 1:length(Cenplot$Census_size)){
  if(Cenplot$Filter[i] == "Filtered"){
    Cenplot2$Census_size[i] <- Cenplot2$Census_size[i] * (max(Cenplot$Census_size[Cenplot$Filter == "Unfiltered"])/max(Cenplot$Census_size[Cenplot$Filter == "Filtered"]))
  }
}

#Collect all cook's distance plots for population density and census size 
par(mfrow = c(2,4))

plot(lm_density, which = 5, main = 'a', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
plot(lm_density1, which = 5, main = 'b', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
mtext(expression(paste(bold('Population density'))), side = 3, line = -1.5, outer = TRUE)
plot(lm_density2, which = 5, main = 'c', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
plot(lm_density_con, which = 5, main = 'd', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
plot(lm_census, which = 5, main = 'e', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
plot(lm_census1, which = 5, main = 'f', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
mtext(expression(paste(bold('Census size'))), side = 3, line = -25, outer = TRUE)
plot(lm_census2, which = 5, main = 'g', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)
plot(lm_census3, which = 5, main = 'h', adj = 0.01, cex.lab = 1.5, cex.axis = 1.5)


par(mfrow = c(1,1))
################################################################################
###Significance tests

#Define significance threshold
signif.cutoff <- 0.05

#All samples
attach(meaniucnbox)


#Test for significant differences between IUCN categories
anova(lm(heterozygosity_cat~IUCN_status))
TukeyHSD(aov(heterozygosity_cat~IUCN_status))

#Obtain mean value per category
meaniucnbox %>%
  group_by(IUCN_status) %>%
  summarise(mean(heterozygosity_cat))



#Save significant p-values for plotting
TukeyIUCN <- TukeyHSD(aov(heterozygosity_cat~IUCN_status))$IUCN_status

maxvaluesIUCN <- meaniucnbox %>% group_by(IUCN_status) %>% summarise(MAX = max(heterozygosity_cat))
meaniucnbox$IUCN_status<-as.character(meaniucnbox$IUCN_status)

pvalIUCN <- as.data.frame(TukeyIUCN) %>% rownames_to_column("Group") %>%
  rowwise() %>%
  mutate(Start = unlist(strsplit(Group, "-"))[1],
         End = unlist(strsplit(Group, "-"))[2]) %>%
  left_join(.,maxvaluesIUCN, by = c("Start" = "IUCN_status")) %>%
  left_join(.,maxvaluesIUCN, by = c("End" = "IUCN_status")) %>% ungroup()

pvalIUCN <- subset(pvalIUCN[stats::complete.cases(pvalIUCN),], `p adj` < signif.cutoff)
pvalIUCN
pvalIUCN <- pvalIUCN %>% mutate(ypos = c(0.003, 0.0028))


#Test for significant differences between threat categories
t.test(heterozygosity_cat~Safety)


#Save significant p-values for plotting
tThreat<-t.test(heterozygosity_cat~Safety, data = meaniucnbox)$p.value
if(tThreat<0.001){
  tThreat<- "< 0.001"
}else{tThreat <- formatC(signif(tThreat, digits = 3))}



#Non-captive samples
attach(meaniucn_nc_box)

#Test for significant differences between IUCN categories
anova(lm(heterozygosity_cat~IUCN_status))

#Obtain mean value per category
meaniucn_nc_box %>%
  group_by(IUCN_status) %>%
  summarise(mean(heterozygosity_cat))


#Test for significant differences between threat categories
t.test(heterozygosity_cat~Safety, data = meaniucn_nc_box)

#Save significant p-values for plotting
tThreat_nc<-t.test(heterozygosity_cat~Safety, data = meaniucn_nc_box)$p.value
if(tThreat_nc<0.001){
  tThreat_nc<- "< 0.001"
}else{tThreat_nc <- formatC(signif(tThreat_nc, digits = 3))}
tThreat_nc


#Captive vs non-captive (all felids)
t.test(heterozygosity_cat~Captive, data = Captivity)


#Captive vs non-captive (Panthera-lineage only)
t.test(heterozygosity_cat~Captive, data = Panther)

#Save p-value for plotting
tPanther<-t.test(heterozygosity_cat~Captive, data = Panther)$p.value
if(tPanther<0.001){
  tPanther<- "< 0.001"
}else{tPanther <- formatC(signif(tPanther, digits = 3))}
tPanther
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#################
#Model building #
#################

#General testing on variable relationship to He
#Change ecological variable and dataset as desired
attach(mean_subs)
plot(heterozygosity_cat~Mass_M)
vartest<-lm(heterozygosity_cat~Mass_M)
par(mfrow = c(2,2))
plot(vartest)

#Reset plot window to one plot
par(mfrow=c(1,1))


#Create second dataframe to use transformed variables in the correlation plot
mean_subs2<-mean_subs

#Transform variables as needed
mean_subs2$Mass_M<-log10(mean_subs2$Mass_M)


#Create dataframe with only variables of interest
mr_subs <- mean_subs2 %>% dplyr::select(Range, Mass_M, Size_M_nt, Gestation, Litter_size, Longevity, Density, Census_size)
variable_names <- list("Range", "Male mass", "Male size",  "Gestation time", "Litter size", "Longevity", "Density", "Census size")
names(mr_subs) <- variable_names


#Get p-values for correlation between range and density
attach(datae_den_con)
cor.test(Density, Range, method = "kendall")

attach(datae_den_filt)
cor.test(Density, Range, method = "kendall")


attach(mr_subs)


#Create correlation plot
ggpairs(data = mr_subs, upper = list(continuous = wrap("cor", method = "kendall", size = 5))) +
  theme(text = element_text(size = 15))


#Make multiple regression model with conservative density data
mr_model<-lm(heterozygosity_cat ~ Density + Range, data = datae_den_con)

#Test for colinearity and give summary stats 
vif(mr_model)
summary(mr_model)

#Plot diagnostic plots
par(mfrow = c(2,2))
plot(mr_model)
#title('He ~ range + density (conservative)', line = -2, outer = TRUE)


#Make multiple regression model with filtered density data
mr_model2<-lm(heterozygosity_cat ~ Density + Range, data = datae_den_filt)

#Test for collinearity and give summary stats 
vif(mr_model2)
summary(mr_model2)

#Plot diagnostic plots
par(mfrow = c(2,2))
plot(mr_model2)
#title('He ~ range + density (filtered)', line = -2, outer = TRUE)


#Reset plot window to one plot
par(mfrow=c(1,1))



#Build regression trees
#Conservative
reg_tree_con<-tree(heterozygosity_cat ~ Density + Range, data = datae_den_con)
plot(reg_tree_con)
text(reg_tree_con)

#Filtered
reg_tree_filt<-tree(heterozygosity_cat ~ Density + Range, data = datae_den_filt)
plot(reg_tree_filt)
text(reg_tree_filt)

#Determine partial R2 conservative
partial_r2(mr_model, covariates = c("Density", "Range"))

part_r2_con<-partial_r2(mr_model, c("Density", "Range"))

Part_r2_con<-as.data.frame(part_r2_con)

partial_R2_con<-Part_r2_con$part_r2
Variable_R2_con<-c(rownames(Part_r2_con))

Partial_correlation_con<-cbind.data.frame(Variable_R2_con, partial_R2_con)
Partial_correlation_con


#Plot partial R2 on a graph
ggplot(data = Partial_correlation_con, aes(x = Variable_R2_con, y = partial_R2_con)) +
  geom_point() +
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(title = expression("Partial R"^2 ~ "for every variable"), x = expression("Partial R"^2), y = "Variable")


#Determine partial R2 filtered
partial_r2(mr_model2, covariates = c("Density", "Range"))

part_r2_filt<-partial_r2(mr_model2, c("Density", "Range"))

Part_r2_filt<-as.data.frame(part_r2_filt)

partial_R2_filt<-Part_r2_filt$part_r2
Variable_R2_filt<-c(rownames(Part_r2_filt))


Partial_correlation_filt<-cbind.data.frame(Variable_R2_filt, partial_R2_filt)
Partial_correlation_filt


#Plot partial R2 on a graph
ggplot(data = Partial_correlation_filt, aes(x = Variable_R2_filt, y = partial_R2_filt)) +
  geom_point() +
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(title = expression("Partial R"^2 ~ "for every variable"), x = expression("Partial R"^2), y = "Variable")
################################################################################


###########
#  PLOTS  #
###########

#Plot phylogenetic tree
plot(felid_tree, type = 'p', FALSE)


#Plot heterozygosity estimates with std values as range
attach(datae_nf)

#Average coverage for every sample
#cov_plot<-
ggplot(datae_nf, aes(unique_id, Coverage_cat, colour = Captive, fill = Captive)) +
  geom_point(shape = "circle filled") +
  geom_text_repel(data = . %>% mutate(label = ifelse(Coverage_cat<7, unique_id, "")), aes(label = label), colour = "black") +
  coord_flip() +
  facet_grid(Lineage ~ ., scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(width = 3, multi_line = TRUE)) +
  expand_limits(y = max(max_cat)) +
  theme() +
  scale_colour_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  scale_fill_manual(values = c("white", "red"), labels = c("No", "Yes")) +
  guides(fill = guide_legend(override.aes = list(shape = "circle filled"), "Captive"),
        colour = guide_legend("Captive")) +
  labs(x = "Sample ID", y = "coverage", caption = "* known hybrid individual")

#Heterozygosity in function of the average coverage
#het_cov_plot<-
ggplot(datae_nf, aes(Coverage_cat, heterozygosity_cat, label = unique_id)) + 
  geom_point(data = subset(datae_nf, Coverage_cat>7 & Coverage_tiger>7)) +
  geom_point(data = subset(datae_nf, Coverage_cat<7 & Coverage_tiger<7), aes(colour = "red"), shape = "triangle") +
  theme_light() +
  geom_smooth(method = 'nls', formula = y ~ a*x^b, se = FALSE, method.args = list(start = c(a = 1, b = 1))) +
  stat_poly_eq(use_label('n'), label.x = 0.8) +
  geom_text_repel(data = . %>% mutate(label = ifelse(Coverage_cat<7 & Coverage_tiger<7, unique_id, "")), aes(label = label)) +
  annotate(geom = "text", label = paste(power_formula), parse = T, x = 65, y = 0.008) +
  #geom_text_repel() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("red"), labels = c("")) +
  guides(colour = guide_legend("Low coverage (<7)", shape = guide_legend("Low coverage (<7)"))) +
  labs(x= "coverage", y = "Heterozygosity (%)", tag = "")


attach(datae)

#Compare estimates across reference genomes
het_cor_plot<-
  ggplot(datae, aes(heterozygosity_tiger, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point(data = subset(datae, species != "Panthera tigris" & genus != "Felis"), size = 3) + 
  geom_point(data = subset(datae, species == "Panthera tigris"), aes(colour = species, shape = species), size = 3) +
  geom_point(data = subset(datae, genus == "Felis"), aes(colour = genus, shape = genus), size = 3) +
  geom_smooth(method = "lm") +
  theme_light() + 
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("blue","red"), labels = c("Felis spp.", "Panthera tigris subspp.")) +
  scale_shape_manual(values = c("square", "triangle"), labels = c("Felis spp.", "Panthera tigris subspp.")) +
  guides(colour = guide_legend("Species"),
         shape = guide_legend("Species")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = expression(Heterozygosity~(tiger~reference)), y = "Heterozygosity (cat reference)", tag = "a")


#Compare effect of reference genome on He estimates, grouped per subfamily
#ref_plot<-
ggplot(comp, aes(unique_id, heterozygosity, colour = reference_genome, fill = reference_genome)) +
  geom_point(shape = "circle filled", stroke = 0.8) +
  coord_flip() +
  scale_color_hue(labels = c("Housecat reference", "Tiger reference")) +
  theme_light() +
  theme(legend.position.inside = c(0.9,0.1)) +
  facet_grid(Subfamily ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("blue", "red"), labels = c("Housecat reference", "Tiger reference")) +
  scale_fill_manual(values = c("white", "red"), labels = c("Housecat reference", "Tiger reference")) +
  guides(colour = guide_legend("Reference genome", position = "inside", override.aes = list(shape = "circle filled")), fill = guide_legend("Reference genome")) +
  labs(x = "Sample ID", y = "Heterozygosity (%)")



#compare effect of distance to the reference genome (close - far)
attach(comp_ref)
distance_plot<- 
  ggplot(comp_ref %>% group_by(unique_id) %>%
           mutate(Slope = ((heterozygosity[distance == "Distant"] - heterozygosity[distance == "Close"])/(2-1))*100), 
         aes(distance, heterozygosity, group = unique_id, colour = Slope)) +
  geom_point() +
  geom_text(data = subset(comp_ref, species == "Panthera tigris" & distance == "Distant"), label = "*", colour = "red", show.legend = T, hjust = 0, nudge_x = 0.15) +
  geom_text(data = subset(comp_ref, species == "Panthera tigris" & distance == "Close"), label = "*", colour = "red", show.legend = T, hjust = 0, nudge_x = -0.15) +
  geom_text(data = subset(comp_ref, genus == "Felis" & distance == "Distant"), label = "°", colour = "blue", show.legend = T, hjust = 0, nudge_x = 0.15) +
  geom_text(data = subset(comp_ref, genus == "Felis" & distance == "Close"), label = "°", colour = "blue", show.legend = T, hjust = 0, nudge_x = -0.15) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(plot.caption = element_markdown(size = 10), plot.caption.position = "plot", axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
  #guides(colourbar = guide_legend("Slope (pp)"), override.aes = list(Slope = "Colourbar")) +
  geom_line(aes(group = unique_id)) +
  labs(x = "Relationship to reference", y = "Heterozygosity", colour = "Slope (pp)", caption = "<span style='color:blue;'>° </span> Felis spp. <br> <span style='color:red;'>* </span> Panthera tigris subspp.", tag = "b")


#Create histogram of distance values
hist_dist_plot<-
ggplot(comp_hist, aes(x = Distance_diff)) +
  geom_histogram(bins = length(comp_hist$Distance_diff)) +
  geom_point(data = subset(comp_hist, genus.x == "Felis"), y = 30, colour = "blue", shape = "circle open") +
  geom_point(data = subset(comp_hist, species.x == "Panthera tigris"), y = 30, colour = "red", shape = "asterisk") +
  stat_bin(aes(y=after_stat(count), label=ifelse(after_stat(count)==0,"",after_stat(count))), geom="text", vjust=-.5, bins = length(comp_hist$Distance_diff)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 10)) +
  theme(plot.caption = element_markdown(size = 10), plot.caption.position = "plot", axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = "Percent point difference (Distant-Close)", y = "Number of samples", caption = "<span style='color:blue;'>° </span> Felis spp. <br> <span style='color:red;'>* </span> Panthera tigris subspp.", tag = "d")


#Create histogram of percent difference values
hist_percent_plot<-
  ggplot(comp_hist, aes(x = Percent_diff)) +
  geom_histogram(bins = length(comp_hist$Percent_diff)) +
  geom_point(data = subset(comp_hist, genus.x == "Felis"), y = 11, colour = "blue", shape = "circle open") +
  geom_point(data = subset(comp_hist, species.x == "Panthera tigris"), y = 11, colour = "red", shape = "asterisk") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0,11), breaks = seq(0, 11, by = 1)) +
  theme_light() +
  theme(plot.caption = element_markdown(size = 10), plot.caption.position = "plot", axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.text = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = "Percent change (distant compared to close)", y = "Number of samples", caption = "<span style='color:blue;'>° </span> Felis spp. <br> <span style='color:red;'>* </span> Panthera tigris subspp.", tag = "c")


#Heterozygosity estimates per sample, grouped per lineage (domestic cat reference genome)
attach(datae)
ggplot(datae, aes(unique_id, heterozygosity_cat, colour = Captive)) +
  geom_point() +
  geom_text(data = subset(datae, unique_id == "Pan_par_Maewha"), label = "*", colour = "black", show.legend = FALSE, vjust = 0, nudge_y = 0.00001) +
  geom_errorbar(ymin = heterozygosity_cat-std_cat, ymax = heterozygosity_cat+std_cat) +
  coord_flip() +
  facet_grid(Lineage ~ ., scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(width = 3, multi_line = TRUE)) +
  expand_limits(y = max(max_cat)) +
  theme_light() +
  theme(legend.position = c(.99, .45), 
        legend.justification = "right", 
        legend.box.just = "right") +
  scale_colour_manual(values = c("red", "blue"), labels = c("Captive", "Non-captive")) +
  guides(colour = guide_legend("Captive sample")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sample ID", y = "Heterozygosity", caption = "* known hybrid individual")


#Comparison of heterozygosity between species, grouped per lineage
#sp_plot<-
ggplot(datae, aes(StyledSpecies, heterozygosity_cat)) + 
  geom_point(aes(colour = Captive, fill = Captive), size = 2, shape = "circle filled", stroke = 0.8) +
  geom_line() +
  geom_text(data = subset(datae, unique_id == "Pan_par_Maewha"), label = "*", colour = "black", show.legend = FALSE, hjust = 0, nudge_x = 0.15) +
  scale_y_continuous(labels = scales::percent) + 
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text.x.bottom = ggtext::element_markdown()) +
  facet_grid(. ~ Lineage , scales = "free_x", space = "free_x", switch = "x") +
  scale_colour_manual(values = c("red", "blue"), labels = c("Captive", "Non-captive")) +
  scale_fill_manual(values = c("red", "white"), labels = c("Captive", "Non-captive")) +
  guides(fill = guide_legend(override.aes = list(shape = "circle filled"), "Captivity"),
         colour = guide_legend("Captivity")) +
  geom_phylopic(data = Ctem_df, aes(x = 1.5, y = 0.0012), img = Ctem, alpha = 0.2) +
  geom_phylopic(data = Ccar_df, aes(x = 1.5, y = 0.0012), img = Ccar, alpha = 0.2) +
  geom_phylopic(data = Pviv_df, aes(x = 3, y = 0.0012), img = Pviv, alpha = 0.2) +
  geom_phylopic(data = Llyn_df, aes(x = 2.5, y = 0.0012), img = Llyn, alpha = 0.2) +
  geom_phylopic(data = Ltig_df, aes(x = 5, y = 0.0012), img = Ltig, alpha = 0.2) +
  geom_phylopic(data = Pleo_df, aes(x = 4, y = 0.0012), img = Pleo, alpha = 0.2) +
  geom_phylopic(data = Pcon_df, aes(x = 2, y = 0.0012), img = Pcon, alpha = 0.2) +
  geom_phylopic(data = Fsil_df, aes(x = 3.5, y = 0.0012), img = Fsil, alpha = 0.2) +
  labs(x = "Species", y = "Heterozygosity", caption = "* known hybrid individual")


#Comparison of heterozygosity between subspecies, grouped per lineage
#ssp_plot<-
  ggplot(data = datae, aes(StyledSubspecies, heterozygosity_cat)) + 
  geom_point(aes(shape = ssp_certainty, colour = Captive, fill = Captive), size = 2, stroke = 0.8) +
  geom_line() + 
  geom_text(data = subset(datae, unique_id == "Pan_par_Maewha"), label = "*", colour = "black", show.legend = FALSE, hjust = 0, nudge_x = 0.15) +
  scale_y_continuous(labels = scales::percent) + 
  theme_light() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        axis.text.x.bottom = ggtext::element_markdown()) +
  facet_grid(. ~ Lineage , scales = "free_x", space = "free_x", switch = "x") +
  scale_shape_manual(values = c("circle filled", "triangle filled"), labels = c("Certain", "Uncertain")) +
  scale_colour_manual(values = c("red", "blue"), labels = c("Captive", "Non-captive")) +
  scale_fill_manual(values = c("red", "white"), labels = c("Captive", "Non-captive")) +
  guides(fill = guide_legend(override.aes = list(shape = "circle filled"), "Captivity"),
         colour = guide_legend("Captivity"),
         shape = guide_legend("Subspecies certainty")) +
  geom_phylopic(data = Ctem_df, aes(x = 1.5, y = 0.0012), img = Ctem, alpha = 0.2) +
  geom_phylopic(data = Ccar_df, aes(x = 1.5, y = 0.0012), img = Ccar, alpha = 0.2) +
  geom_phylopic(data = Pviv_df, aes(x = 3, y = 0.0012), img = Pviv, alpha = 0.2) +
  geom_phylopic(data = Llyn_df, aes(x = 2.5, y = 0.0012), img = Llyn, alpha = 0.2) +
  geom_phylopic(data = Ltig_df, aes(x = 6.5, y = 0.0012), img = Ltig, alpha = 0.2) +
  geom_phylopic(data = Pleo_df, aes(x = 7, y = 0.0012), img = Pleo, alpha = 0.2) +
  geom_phylopic(data = Pcon_df, aes(x = 2.5, y = 0.0012), img = Pcon, alpha = 0.2) +
  geom_phylopic(data = Fsil_df, aes(x = 4.2, y = 0.0012), img = Fsil, alpha = 0.2) +
  labs(x = "Subspecies", y = "Heterozygosity", caption = "* known hybrid individual")


captive_plot<-
  ggplot(Captivity, aes(Captive, heterozygosity_cat)) + 
  geom_boxplot(notch = F) + 
  geom_text_repel(aes(label=outlier_captive),na.rm=TRUE, hjust = 0, vjust = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent) +
  stat_summary(fun.data = sample.size, geom = "text") +
  #geom_signif(comparisons = list(c("Captive", "Non-captive")),
  #    map_signif_level = T, step_increase = 0.1) +
  labs(x = "Captivity status", y = "Heterozygosity", tag = "a")


panthera_plot<-
  ggplot(Panther, aes(Captive, heterozygosity_cat)) + 
  geom_boxplot(notch = F) + 
  geom_text_repel(aes(label=outlier_panther),na.rm=TRUE, hjust = 0, vjust = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent) +
  stat_summary(fun.data = sample.size, geom = "text") +
  geom_signif(comparisons = list(c("Non-captive", "Captive")),
             annotation = tPanther) +
  labs(x = "Captivity status", y = "Heterozygosity", tag = "b")




#Create boxplots for mean iucn and threat status
attach(meaniucnbox)
mean_IUCN_plot<-
  ggboxplot(meaniucnbox, x ="IUCN_status" , y = "heterozygosity_cat") +
#    geom_point(data = subset(datae, species == "Puma concolor")) +
    geom_text_repel(aes(label=outlier_mean_IUCN),na.rm=TRUE, hjust = 0, vjust = 1) +
    stat_summary(fun.data = sample.size, geom = "text") +
    theme_light() +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
    scale_y_continuous(labels = scales::percent) +
    geom_signif(data = pvalIUCN, manual = TRUE,
                aes(xmax = End, xmin = Start, y_position = ypos, annotations = round(`p adj`,3))) + 
    labs(x = "IUCN category", y = "Heterozygosity (%)", tag = "a") 


mean_threat_plot<-
  ggboxplot(meaniucnbox, x = "Safety", y = "heterozygosity_cat") +
    geom_text_repel(aes(label=outlier_mean_threat),na.rm=TRUE, hjust = 0, vjust = 1) +
    stat_summary(fun.data = sample.size, geom = "text") +
    theme_light() +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
    geom_signif(comparisons = list(c("Threatened","Non-threatened")), 
                 annotation = tThreat) + 
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Threat status", y = "Heterozygosity (%)", tag = "b") 


attach(meaniucn_nc_box)
#Create boxplots for iucn and threat status
mean_IUCN_nc_plot<-
  ggplot(meaniucn_nc_box, aes(IUCN_status, heterozygosity_cat)) + 
  geom_boxplot() + 
  geom_text_repel(aes(label=outlier_mean_IUCN_nc),na.rm=TRUE, hjust = 0, vjust = 1) +
  stat_summary(fun.data = sample.size, geom = "text") +
  theme_light() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "IUCN category", y = "Heterozygosity (%)", tag = "c") 


mean_threat_nc_plot<-
  ggplot(meaniucn_nc_box, aes(Safety, heterozygosity_cat)) +
  geom_boxplot() + 
  geom_text_repel(aes(label=outlier_mean_threat_nc),na.rm=TRUE, hjust = 0, vjust = 1) +
  stat_summary(fun.data = sample.size, geom = "text") +
  theme_light() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  geom_signif(comparisons = list(c("Threatened","Non-threatened")), 
                annotation = tThreat_nc) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Threat status", y = "Heterozygosity (%)", tag = "d") 



#Regular linear regression plot for single variables
par(mfrow=c(1,1))
attach(mean_subs)
mass_plot<-
  ggplot(mean_subs, aes(Mass_M, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point() + 
  #geom_text_repel(label = Species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(trans = 'log10') +
  labs(x = expression(Body~mass~(kg)), y = "Heterozygosity", tag = "a")


size_plot<-
  ggplot(mean_subs, aes(Size_M_nt, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point() + 
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(Body~size~(cm)), y = "Heterozygosity", tag = "b")


range_plot<-
  ggplot(mean_subs, aes(Range, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point() + 
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = expression(Geographic~ranges~(km^2)), y = "Heterozygosity", tag = "c")


attach(mean_felids)
gestation_plot<-
  ggplot(mean_felids, aes(Gestation_sp, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), rsquared.conf.level = NA, method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point() +
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(Gestation~time~(days)), y = "Heterozygosity", tag = "d")


longevity_plot<-
  ggplot(mean_felids, aes(Longevity_sp, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  geom_point() + 
  annotate("text", label = "R < 0.01", x = 8.05, y = 0.0028) + #Correlation added manually as stat_correlation cannot handle R < 0.01
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(Lifespan~(years)), y = "Heterozygosity", tag = "e")


litter_plot<-
  ggplot(mean_felids, aes(Litter_size_sp, heterozygosity_cat)) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(label.y = 0.85) +
  geom_point() + 
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm", fill = "cyan3") +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(Litter~size), y = "Heterozygosity", tag = "f")


attach(Denplot)
density_plot<-
  ggplot(Denplot2, aes(Density, heterozygosity_cat, group = factor(Filter), colour = factor(Filter), fill = factor(Filter), shape = factor(Filter), linetype = factor(Filter))) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm", lineheight = 0.5) + 
  stat_correlation(vjust = 5.5) +
  geom_point() + 
  #geom_text_repel(label = species) +
  geom_smooth(method = "lm") +
  theme_light() +
  theme(legend.position = "top") +
  theme(axis.text.x.bottom = element_text(colour = "blue"), axis.text.x.top = element_text(colour = "red")) +  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(sec.axis = sec_axis(~ . /(max(Denplot$Density[Denplot$Filter == "Unfiltered"])/max(Denplot$Density[Denplot$Filter == "Filtered"])))) +
  scale_colour_manual(values = c("red","blue"), labels = c("Filtered", "Unfiltered")) +
  scale_shape_manual(values = c("circle", "circle open")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Filtered", "Unfiltered")) +
  guides(colour = guide_legend("Filter status"),
    fill = guide_legend("Filter status"),
    shape = guide_legend("Filter status"),
    linetype = guide_legend("Filter status")) +
  labs(x = expression(Population~density~(individuals/100~km^2)), y = "Heterozygosity", tag = "g")


#Density_plot_con<-
  ggplot(datae_den_con, aes(Density, heterozygosity_cat)) +
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm", lineheight = 0.5) + 
  stat_correlation(vjust = 5.5) +
  geom_point() +
  geom_smooth(method = "lm", fill = 'limegreen', colour = 'darkgreen') +
  theme_light() +
  theme() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(Population~density~(individuals/100~km^2)), y = "Heterozygosity", colour = "")
 
   
attach(Cenplot)
census_plot<-
  ggplot(Cenplot2, aes(Census_size, heterozygosity_cat, group = factor(Filter), colour = factor(Filter), fill = factor(Filter), shape = factor(Filter), linetype = factor(Filter))) + 
  stat_poly_eq(use_label(c("p", "n", "adj.R2")), method = "lm") + 
  stat_correlation(vjust = 5.5) +
  geom_point() +
  #geom_text_repel(label = species +
  geom_smooth(method = "lm") +
  theme_light() +
  theme(legend.position = "top", axis.text.x.bottom = element_text(colour = "blue"), axis.text.x.top = element_text(colour = "red")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::comma, sec.axis = sec_axis(~ . /(max(Cenplot$Census_size[Cenplot$Filter == "Unfiltered"])/max(Cenplot$Census_size[Cenplot$Filter == "Filtered"])), labels = scales::comma)) +
  scale_colour_manual(values = c("red","blue")) +
  scale_shape_manual(values = c("circle", "circle open")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Filtered", "Unfiltered")) +
  guides(colour = guide_legend("Filter status"),
         fill = guide_legend("Filter status"),
         shape = guide_legend("Filter status"),
         linetype = guide_legend("Filter status")) +
  labs(x = expression(Census~size), y = "Heterozygosity", tag = "h")


  
  
#Combine plots
((het_cor_plot|distance_plot)/(hist_percent_plot)) 
ssp_plot
sp_plot
(captive_plot + panthera_plot)
(mean_IUCN_plot|mean_threat_plot)/(mean_IUCN_nc_plot|mean_threat_nc_plot)
(mass_plot|size_plot)/(range_plot|gestation_plot)/(longevity_plot|litter_plot)/(density_plot|census_plot)

################################################################################
