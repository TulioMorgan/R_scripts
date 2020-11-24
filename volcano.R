#PT-BR: script para criar um volcano plot referente a analise proteomica. Identifcar proteinas com abundacia relativa significativamente diferentes entre dois tratamentos.

#EN: script to create a volcano plot to analyze data from proteomic analysis. Identify significantly different proteins with relative abundance between two treatments.

remove(list=ls(all=TRUE)) #remove todos objetos criados

dados = read.csv(file = 'volcano_plot.csv', header = TRUE, sep = '\t')

dados_volc = c(NULL)
dados_volc = cbind(dados[,12], dados[,11]) # transforma em data.frame para depois colocar a coluna de caracteres (anotacoes)
colnames(dados_volc) = c("log2fold_change", "log10pvalor")
#rownames(dados_volc) = dados$Anotacoes

# cria vetor de cores de acordo com valor de log2 fold-change e p-valor significativos.
cores = c(NULL)
indices = c(NULL) # para numerar as proteinas de interesse no grafico.
proteinas = c(NULL) # para recuperar a anotacao das proteinas de interesse.
for (linha in 1:nrow(dados_volc)) {
  if (dados_volc[linha,1] >= 1){ # log2 fold-change maior ou igual a 1 eh significativo.
    if (dados_volc[linha,2] >= 1.3010299957){  # -log10 (0.05) , ou seja, p-valor significativo do teste T.
      cores = rbind(cores, "darkred")
      indices = rbind(indices, linha)
      protein = as.character(dados[linha,8]) # a coluna 8 contem as informacoes de anotacao.
      proteinas = c(proteinas, protein)
    }
    else {
      cores = rbind(cores, "grey60")
      indices = rbind(indices, NA)
      proteinas = c(proteinas, NA)
    }
  }
  else if(dados_volc[linha,1] <= -1) {
    if (dados_volc[linha,2] >= 1.3010299957){
      cores = rbind(cores, "steelblue3")
      indices = rbind(indices, linha)
      protein = as.character(dados[linha,8]) # a coluna 8 contem as informacoes de anotacao.
      proteinas = c(proteinas, protein)
    }
    else {
      cores = rbind(cores, "grey60")
      indices = rbind(indices, NA)
      proteinas = c(proteinas, NA)
    }
  }
  else{
    cores = rbind(cores, "grey60")
    indices = rbind(indices, NA)
    proteinas = c(proteinas, NA)
  }
}

write.csv(proteinas, file = "proteinas_diferencialmente_expressas_volcano_plot.csv")

# criar o volcano plot
plot(dados_volc[,1], dados_volc[,2], xaxt = "n", yaxt = "n", # remove eixos com os parametros xaxt e yaxt.
     col = cores,
     pch = 20,
     cex = 0.8,
     xlab = "log2(Fold Change)",
     ylab = "-log10(p.value)")
axis(1, at = c(-4,-2,-1,0,1,2,4), las=1) # plotar eixo x. A funcao axis() tem 3 argumentos: o primeiro eh o eixo (1=eixo x, 2=eixo y); o segundo eh o range (at='range') e o terceiro eh a orientacao dos labels do eixo (las=1 eh na horizontal)
axis(2, at = c(0.5, 1.30, 2.1, 2.9), las=2)
text(dados_volc[,1], dados_volc[,2], labels=indices, cex= 0.6, pos = 1) # colocar o numero da linha das proteinas diferencialmente expressas
legend(3, 3.5, legend=c("Up-regulated", "Down-regulated", "Not sig."),
       col=c("darkred", "steelblue3", "grey60"), pch =20, cex=0.6)    





