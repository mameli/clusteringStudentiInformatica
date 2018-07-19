x <- 2:50
y<- c(12.5, 9.7, 7.4, 6.54, 4.87, 4.22, 3.82, 3.52 ,2.94 ,2.75 ,2.57 ,2.3 ,1.97 ,1.88 ,1.76 ,1.71 ,1.58 ,1.48 ,1.43 ,1.36 ,1.29 ,1.24 ,1.15 ,1.1 ,1.07 ,1.03 ,1.01 ,1 ,0.99 ,0.97 ,0.92 ,0.83 ,0.82 ,0.79 ,0.78 ,0.77 ,0.76 ,0.74 ,0.71 ,0.7 ,0.69 ,0.66 ,0.66 ,0.64 ,0.63 ,0.56 ,0.56 ,0.52 ,0.52)
library(ggplot2)
data = data.frame(x,y)
k = x
SSE = y
ggplot(data, aes(x=k, y=SSE)) +    geom_point(shape=1) +  geom_line() + geom_point(color = 'black')

#stampare valori di k con variazione di SSE minore di una soglia eps

eps = 0.01
for(i in 1:48){
  if(data[i,2] - data[i+1,2] <= eps){
    print(data[i,1])    
  }
}

library(readr)
voto_test_clusteredK3 <- read_csv("git/clusteringStudentiInformatica/voto-test-clusteredK43.csv", 
                                  col_types = cols(ANI = col_skip(), ARC = col_skip(), 
                                                   ASD = col_skip(), INGLESE = col_skip(), 
                                                   Instance_number = col_skip(), MDL = col_skip(), 
                                                   PRG = col_skip(), coorte = col_skip(), 
                                                   crediti_con_voto = col_skip(), crediti_totali = col_skip(), 
                                                   data_ANI = col_skip(), data_ARC = col_skip(), 
                                                   data_ASD = col_skip(), data_INGLESE = col_skip(), 
                                                   data_MDL = col_skip(), data_PRG = col_skip()))
View(voto_test_clusteredK3)

C3 = matrix(nrow = 316,ncol = 316)
for(i in 1:316){
  for(j in 1:316){
    if(voto_test_clusteredK3[i,3]==voto_test_clusteredK3 [j,3]){
      C3[i,j] = 1
    }else{
      C3[i,j] = 0
    }
  }
}
#Matrice distanza
D3 = as.matrix(dist(voto_test_clusteredK3[,1:2],method = 'euclidean',diag = TRUE,upper = TRUE))

c3 = as.vector(t(C3))
d3 = as.vector(t(D3))

cor(c3,d3,method="pearson")