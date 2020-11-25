remove(list=ls(all=TRUE)) #remove todos objetos criados

dados = read.csv(file="mds_data.tsv", sep='\t', header=FALSE, dec = '.')
fungos = as.matrix(dados[,ncol(dados)]) # a ultima coluna contem os nome dos organismos
dados = dados[,-ncol(dados)] # remove a ultima coluna dos dados.
matriz_dist = as.matrix(dados) # converte dados de entrada (data frame) em matriz
matriz_dist[which(matriz_dist==0)]=1 # converte valores nulos (0) em 1 para aplicar transformacao log.
matriz_dist.log = log(matriz_dist,base=10) # aplica transformacao log na matriz.
dist_par_a_par.log = cmdscale(matriz_dist.log,k=2) # realiza escalonamento multidimensional


formato = character() #cria um vetor vazio. Se usar cores = c(), ira colocar um elemento NULL no inicio.
for (i in 1:nrow(fungos)){ # percorrer indices da lista de fungos
  if (i == 1){
    formato = c(formato,1)
  }
  else if(i == 2){
    formato = c(formato,1)
  }
  else if(i == 15){
    formato = c(formato,1)
  }
  else if(i == 16){
    formato = c(formato,1)
  }
  else{
    formato = c(formato,19)
  }
}
formato = as.numeric(formato)

plot(x = dist_par_a_par.log[,1], y = dist_par_a_par.log[,2],
     pch = formato,
     cex = 1.2,
     xlab = 'Dimension 1',
     ylab = 'Dimension 2')

