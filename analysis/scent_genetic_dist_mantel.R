lin_per_pop <- field_data %>% 
  group_by(pop_ord) %>% 
  summarise(linalool_mean=mean(linalool, na.rm = T))
  
lin_per_pop<-na.omit(lin_per_pop)
linalool_dist<-distance(sqrt(lin_per_pop$linalool_mean), "bray-curtis")
m <- as.matrix(linalool_dist)
colnames(m)<-lin_per_pop$pop_ord
rownames(m)<-lin_per_pop$pop_ord

pairwiseFst<-read_csv('raw_data/pairwiseFst.csv')

spatial<-pairwiseFst%>%
  select(Pop1, Pop2, spatial) %>% 
  pivot_wider(names_from = Pop2, values_from=spatial, values_fill=0)

spatial_dist<-as.dist(spatial[,-1])
#spatial_dist<-as.dist(spatial_matrix[,-1])
spatial_matrix<-as.matrix(spatial[,-1])
rownames(spatial_matrix)<-spatial$Pop1


genetic<-pairwiseFst%>%
  select(Pop1, Pop2, genetic) %>% 
  pivot_wider(names_from = Pop2, values_from=genetic, values_fill=0)

genetic_dist<-as.dist(genetic[,-1])
#spatial_dist<-as.dist(spatial_matrix[,-1])
genetic_matrix<-as.matrix(genetic[,-1])
rownames(genetic_matrix)<-genetic$Pop1

#Mantel test
p_load(phytools)
scentMantel<-multi.mantel(m, list(genetic_matrix,spatial_matrix), nperm=1000)
scentMantel$probF
scentMantel$fstatistic
scentMantel


