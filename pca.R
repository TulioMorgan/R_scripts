# PCA
# um bom tutorial se encontra em:
# https://www.datacamp.com/community/tutorials/pca-analysis-r

# PCA aims to detect the correlation between variables.
# No caso, as variaveis seriam os tratametos. Acho que por isso elas devem ficar nas linhas da matriz...
# PCA: Finding the directions of maximum variance in high-dimensional data and project it onto a smaller dimensional subspace while retaining most of the information. Logo, se as replciatas estiveram proximas, significa que sao bem relacionadas. A maior varianca plotada nao foi capaz de separa-las.
# eigenvectors (the principal components)

# Devemos plotar a matriz reconstruida com os dois PC`s (eigenvectors) mais importantes.

remove(list=ls(all=TRUE)) #remove todos objetos criados

dados = read.csv(file = 'proteinGroupsAnnotation.csv', header = TRUE, sep = '\t')

dados_pca = t(dados[,2:7]) # fazemos a transposta pois as "variaveis" devem ser o tratamentos, e por isso devem ficar nas linhas da matriz.
resultPCA = prcomp(dados_pca, center = TRUE,scale. = TRUE)
summary(resultPCA)

# obter as variancias explicadas por PC1 e PC2. Da para ver esse resultado com o comando summary() acima.
PC1 = round((resultPCA$sdev[1]**2)*100/sum(resultPCA$sdev**2),1)
PC2 = round((resultPCA$sdev[2]**2)*100/sum(resultPCA$sdev**2),2)

plot(x=resultPCA$x[,1], y=resultPCA$x[,2], # plotar a matriz de covariancia reconstruida com os 2 eigenvectors mais importantes. 
     pch=20,
     cex = 2.0,
     col = c('darkred','darkred','darkred','royalblue','royalblue','royalblue'),
     xaxt = "n", yaxt = "n",
     xlab = paste("PC1 (",PC1,"%)"),
     ylab = paste("PC2 (",PC2, "%)"))
axis(1, at = c(-14,-10,-5,0,5,10,14), las=1) # plotar eixo x. A funcao axis() tem 3 argumentos: o primeiro eh o eixo (1=eixo x, 2=eixo y); o segundo eh o range (at='range') e o terceiro eh a orientacao dos labels do eixo (las=1 eh na horizontal)
axis(2, at = c(-10,-5,0,5,10,15), las=2)


