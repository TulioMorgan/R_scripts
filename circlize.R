require(circlize)
require(stringi)

x = read.table('circlize_data.txt', sep = '\t', header = FALSE)

# editar a coluna 1 da matriz "x" pois os nomes dos setores nao irao corresponder aos nomes dessa coluna (so vai o nome da enzima no setor, sem o carboidrato que atua)
new_x = c(NULL)
for (j in x[,1]){
  jSplit = strsplit(j, "@") # split no "@" pois eh o separador entre nome do carboidrato e o nome da familia da enzima
  enzima = sapply(jSplit, "[", 2) # pegar somente o nome da familia da enzima para colocar na nova matriz, pois assim fica igual ao nome dos setores do grafico
  new_x = c(new_x, enzima)
}

new_matrix = cbind(new_x, x) # nova matriz. Tive que colocar a matriz x antiga interira com cbind, pois estava dando erro quando queria colcoar somente a coluna 2, qeu era de interesse x[,2]
new_matrix = new_matrix[-2] # removi a coluna que nao eh de interesse da nova matriz
new_matrix # matriz que sera usada na funcao "chordDiagram"

setores = c(NULL)
cores_setores = c(NULL)
cores_links = c(NULL)

for (i in x[,1]){
  
  iSplit = strsplit(i, "@") # a familia da enzima possui o carboidrato na qual atual separado por "_"
  enzima = sapply(iSplit, "[", 2) # usa a funcoa sapply para recuperar apenas o segundo (2) elementos da lisya "iSplit"
  
  if (enzima %in% setores){
    setores = c(setores, enzima) # aqui adicionamos setores "repetidos", pois na verdade sao familias que atuam em mais de 1 carboidrato.
  }
  else{
    setores = c(setores, enzima)
    if (stri_count_regex(i, "Cellulose@")){
      cores_setores = c(cores_setores,"forestgreen")
    }
    else if (stri_count_regex(i, "Chitin@")){
      cores_setores = c(cores_setores,"royalblue3")
    }
    else if (stri_count_regex(i, "Hemicellulose@")){
      cores_setores = c(cores_setores,"brown1")
    }
    else if (stri_count_regex(i, "Cutin@")){
      cores_setores = c(cores_setores,"skyblue")
    }
    else if (stri_count_regex(i, "B-1,4-glucan@")){
      cores_setores = c(cores_setores,"red4")
    }
    else if (stri_count_regex(i, "B-1,3-glucan@")){
      cores_setores = c(cores_setores,"darkorange")
    }
    else if (stri_count_regex(i, "Starch@")){
      cores_setores = c(cores_setores,"gray40")
    }
    else if (stri_count_regex(i, "Redox-reactions@")){
      cores_setores = c(cores_setores,"gray50")
    }
    else if (stri_count_regex(i, "Pectin@")){
      cores_setores = c(cores_setores,"gray60")
    }
    else if (stri_count_regex(i, "Others@")){
      cores_setores = c(cores_setores,"gray70")
    }
    else if (stri_count_regex(i, "Non-carbohydrate-esterases@")){
      cores_setores = c(cores_setores,"steelblue4")
    }
  }
}
#setores = rev(setores) # inverter a ordem dos elementos no vetor setores.
#cores_setores = rev(cores_setores)

for (i in x[,2]){
  if (i %in% setores){ # nao devemos adicionar esses setores repetidos
    print(i)
  }
  else{
    setores = c(setores,i)
    if (stri_count_regex(i, "Cellulose")){
      cores_setores = c(cores_setores,"forestgreen")
    }
    else if (stri_count_regex(i, "Chitin")){
      cores_setores = c(cores_setores,"royalblue3")
    }
    else if (stri_count_regex(i, "Hemicellulose")){
      cores_setores = c(cores_setores,"brown1")
    }
    else if (stri_count_regex(i, "Cutin")){
      cores_setores = c(cores_setores,"skyblue")
    }
    else if (stri_count_regex(i, "B-1,4-glucan")){
      cores_setores = c(cores_setores,"red4")
    }
    else if (stri_count_regex(i, "B-1,3-glucan")){
      cores_setores = c(cores_setores,"darkorange")
    }
    else if (stri_count_regex(i, "Starch")){
      cores_setores = c(cores_setores,"gray40")
    }
    else if (stri_count_regex(i, "Redox-reactions")){
      cores_setores = c(cores_setores,"gray50")
    }
    else if (stri_count_regex(i, "Pectin")){
      cores_setores = c(cores_setores,"gray60")
    }
    else if (stri_count_regex(i, "Others")){
      cores_setores = c(cores_setores,"gray70")
    }
    else if (stri_count_regex(i, "Non-carbohydrate-esterases")){
      cores_setores = c(cores_setores,"steelblue4")
    }
  }
}

for (i in x[,1]){
  
  iSplit = strsplit(i, "@") # a familia da enzima possui o carboidrato na qual atual separado por "_"
  enzima = sapply(iSplit, "[", 2) # usa a funcoa sapply para recuperar apenas o segundo (2) elementos da lisya "iSplit"
  
  if (stri_count_regex(i, "Cellulose@")){
     cores_links = c(cores_links,"forestgreen")
  }
  else if (stri_count_regex(i, "Chitin@")){
    cores_links = c(cores_links,"royalblue3")
  }
  else if (stri_count_regex(i, "Hemicellulose@")){
    cores_links = c(cores_links,"brown1")
  }
  else if (stri_count_regex(i, "Cutin@")){
    cores_links = c(cores_links,"skyblue")
  }
  else if (stri_count_regex(i, "B-1,4-glucan@")){
    cores_links = c(cores_links,"red4")
  }
  else if (stri_count_regex(i, "B-1,3-glucan@")){
    cores_links = c(cores_links,"darkorange")
  }
  else if (stri_count_regex(i, "Starch@")){
    cores_links = c(cores_links,"gray40")
  }
  else if (stri_count_regex(i, "Redox-reactions@")){
    cores_links = c(cores_links,"gray50")
  }
  else if (stri_count_regex(i, "Pectin@")){
    cores_links = c(cores_links,"gray60")
  }
  else if (stri_count_regex(i, "Others@")){
    cores_links = c(cores_links,"gray70")
  }
  else if (stri_count_regex(i, "Non-carbohydrate-esterases@")){
    cores_links = c(cores_links,"steelblue4")
  }
}

dev.new(width=50, height=50)
### PLOTAR GRAFICO ###
circos.clear()
circos.par(gap.degree = 1, start.degree = 0) # gap.degree eh o gap entre os setores. Se colocar 2, divide bem os setores entre os carboidratodos e as enzimas
chordDiagram(
new_matrix, ##matriz ou data frame contendo os dados (ver exemplo de dados acima)
grid.col = cores_setores, ##cores dos setores. Pode ser NULL (default do script) ou um vetor com numero de cores igual ao numero de setores. Nesse caso, a primeira cor digitada ir? para o primeiro setor e assim sucessivamente.
grid.border = "black", ##bordas dos setores. Mentenho NA para ser da mesma cor dos setores.
transparency = 0.2, ##transparencia do gr?fico. Mantenho o default (0.5)
col = cores_links, ##Default = NULL. Nao precisa configurar, pois as cores dos links ficam iguias as cores selecionadas para os setores. Pode ser um vetor de cores dos links entre os setores. Deve ter o mesmo n?mero de elementos dos links
column.col = NULL,
#order = setores, ##ordem dos setores no gr?fico. Default ? a ordem que aparecem no arquivo de entrada (data frame ou matriz)
directional = 1, ##direcao dos links. Valor igual a 1 (sentido da primeira coluna do data frame para a segunda coluna. Valor = -1 ? o sentido oposto. Valor =0 sem direcao e valor = 2 eh em ambas as direcoes. 
xmax = NULL,
symmetric = FALSE, ##informa se a matriz ? simetrica ou nao (matriz espelhada). Se for TRUE, soh usa uma das metades da matriz.
keep.diagonal = FALSE, ##se a matriz for diagonal, mantem a visualizacao da diagonal.
direction.type = "diffHeight+arrows", ##tipo de marcacao de direcao dos links. DiffHeight sao as cordas e arrows sao setas. POde-se usar um ou outro ou ambos.
diffHeight = convert_height(2, "mm"), ##altera o inicio (valores positivos) ou termino (valores negativos) dos links.
reduce = 1e-5,
self.link = 2,

#preAllocateTracks = 1,
annotationTrack = c("grid"), ##o argumento "axis" coloca os 'ticks' nos setores.
annotationTrackHeight = convert_height(c(4), "mm"), ##O primeiro valor (=5) indica o tamanho dos nomes dos setores. O segundo valor indica (=10) indica o tamanho dos setores. O valor "mm" singifica escala em milimetros. Pode ser ainda "cm" ou "inches"
  preAllocateTracks = list(ylim = c(1, 2),
                    track.height = 0.05,
                    bg.col = NA,
                    bg.border = NA,
                    bg.lty = 10,
                    bg.lwd = 50),
###Ajustar os links
link.border = "black", ##Nao utilizei bordas nos links
link.lwd = 0.1, ##largura das bordas dos links
link.lty = 1, ##estilo das bordas dos links
link.sort = FALSE,
link.decreasing = TRUE,
link.arr.type = "big.arrow", ##tipo de seta dos links. "triangle" sao setas e "big.arrow" sao setas apenas nas pontas dos links.
link.arr.lty = 20, ##estilo da seta. O valor 20 eh pontilhada. O valor 10 eh pontilhada diferente.
link.arr.lwd = 1, ##largura da linha da seta
link.arr.col = "black", ##cor da seta
link.largest.ontop = FALSE,
link.visible=TRUE, ##plotar ou nao o links. Podemos nao plotar alguns. Para isso, fazer um vetor com TRUE e FALSE.
link.rank = NULL)



###colocar os nomes dos setores de p? (como se fosse ponteiros de relogio)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  xplot = get.cell.meta.data("xplot")
  sector.name = get.cell.meta.data("sector.index")  
    if(abs(xplot[2] - xplot[1]) < 400 || sector.name == "GH12") {
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
            niceFacing = TRUE, adj = c(0, 0.5), col = "black", cex=1.0)
    }

}, bg.border = NA)
  
dev.copy2pdf(file='plot.pdf')
graphics.off()