# Analisi carriere studenti iscritti al I anno del corso di laurea in Informatica

Analisi delle carriere degli studenti utilizzando il software Weka per eseguire l'algoritmo kmeans sui dati.

Scatterplot di Programmazione e Architetture:
![arc](img/arcPrg.png)
___
Matrice di correlazione:
![cor](img/corMatrix.png)
___
Cluster di Architetture e Programmazione
![clu](img/ARC-PRG-Cluster.png)


Plot sse
df <- read.table("/home/mameli/git/custeringStudentiInformatica/creditiSse.csv", 
+                  header = FALSE)

plot(df)
> lines(df, type = "o", col = "blue", lwd="5")

### Import clustered
```{r}
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

# Matrice di incidenza
matriceIncidenza <- function(data){
  nr = nrow(data)
  nc = ncol(data)
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

# matrice distanza
matriceDistanza <- function(data){
  return(as.matrix(dist(data[,1:(ncol(data)-1)],method = 'euclidean',diag = TRUE,upper = TRUE)))
}

calcoloCorrelazione <- function(data){
  MI <- matriceIncidenza(data)
  D <- matriceDistanza(data)
  mi = as.vector(t(MI))
  d = as.vector(t(D))
  
  return(cor(mi,d,method="pearson"))
}

calcoloCorrelazione(crediti_totali_prg_arc_clustered)
> [1] -0.8539944
```

Voto medio e test correlazione
-0.4755988 k=2
-0.4652048 k=3
-0.2730982 k=43

### k NN per DBSCAN

```{r}
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

```
