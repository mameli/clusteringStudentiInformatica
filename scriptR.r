library(readr)
crediti_totali_prg_arc_clustered <- read_csv("git/clusteringStudentiInformatica/crediti_totali-prg-arc-clustered.csv", 
                                             col_types = cols(ANI = col_skip(), ASD = col_skip(), 
                                                              INGLESE = col_skip(), Instance_number = col_skip(), 
                                                              MDL = col_skip(), TEST = col_skip(), 
                                                              coorte = col_skip(), crediti_con_voto = col_skip(), 
                                                              data_ANI = col_skip(), data_ARC = col_skip(), 
                                                              data_ASD = col_skip(), data_INGLESE = col_skip(), 
                                                              data_MDL = col_skip(), data_PRG = col_skip(), 
                                                              voto_medio = col_skip()))
View(crediti_totali_prg_arc_clustered)
nc = nrow(crediti_totali_prg_arc_clustered)
kDBScan <- function(data,k){
  library(ggplot2)
  D = as.matrix(dist(data[,1:ncol(data)-1],method = 'euclidean',diag = TRUE,upper = TRUE))
  D_1 = D
  for(i in 1:nrow(data)){
    D_1[i,] = sort(D[i,])
  }
  p = 1:nrow(data)
  dist = sort(D_1[, k])
  data = data.frame(p,dist)
  ggplot(data, aes(x=p, y=dist)) +geom_point(shape=1) +  geom_line() + geom_point(color = 'black')
}

kDBScan(crediti_totali_prg_arc_clustered, 6)

# Matrice di incidenza
matriceIncidenza <- function(data){
  nr = nrow(data)
  nc = nrow(data)
  C = matrix(nrow = nr, ncol = nr)
  for(i in 1:nr){
    for(j in 1:nr){
      if(data[i,nc] == data[j,nc])
        C[i,j] = 1
      else
        C[i,j] = 0
    }
  }
  return(C)
}

C = matriceIncidenza(crediti_totali_prg_arc_clustered)

# matrice distanza
D = as.matrix(dist(crediti_totali_prg_arc_clustered[,1:3],method = 'euclidean',diag = TRUE,upper = TRUE))

c = as.vector(t(C))
d = as.vector(t(D))

cor(c,d,method="pearson")
