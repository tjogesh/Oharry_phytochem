#ANOSIM 
library(vegan)
scent_cols<-c("Pop2","beta_myrcene","cis_beta_ocimene", "trans_beta_ocimene","ocimene_epoxide",  
"ocimene_epoxide.2", "ocimene_epoxide.1" , "linalool","beta_caryophyllene","methyl_benzoate",
"beta_farnesene", "alpha_humulene","alpha_terpineol","methyl_geranate","E.Z_alpha_farnesene" ,
"E.E_alpha_farnesene", "geraniol","X2PE","phenylacetonitrile",  
"caryophyllene_oxide", "nerolidol", "nitrophenylethane", "methyl_farnesoate",
"isophytol", "jasmine_lactone", "phenylacetaldoxime","farnesol")

morph_cols<-c("Pop2", "corolla" ,"floral_flare","herkogamy",   "tube_length")

all<-c(scent_cols,morph_cols )
all<-all[-28]
#all
data = na.omit(main[,all])
com<-sqrt(data[,-1])
env <- factor(data[, 1])
dist.com <- vegdist(com, method = "bray")
popDiffall<- anosim(dist.com, env)
popDiffall

#scent
data = na.omit(main[,scent_cols])
com<-sqrt(data[,-1])
env <- factor(data[, 1])
dist.com <- vegdist(com, method = "bray")
popDiff<- anosim(dist.com, env)
popDiff

#morph
data = na.omit(main[,morph_cols])
com<-sqrt(data[,-1])
env <- factor(data[, 1])
dist.com <- vegdist(com, method = "bray")
popDiffmorph<- anosim(dist.com, env)
popDiffmorph