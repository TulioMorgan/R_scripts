#PT-BR: script para gerar um heatmap.
#EN: script to generate a heatmap.
# Nice tutorial at: https://ryjohnson09.netlify.com/post/how-to-make-a-heatmap-in-r/

install.packages("gplots") # for heatmap.2

require(gplots)

remove(list=ls(all=TRUE)) #remove todos objetos criados

dados = read.table(file = 'dados_heatmap.csv', sep = ';', header = TRUE, row.names = 1)

matrix = as.matrix(dados)

my_pallete = colorRampPalette(c("white","gold", "red1","mediumorchid4"))
col_breaks =c(-3,seq(-1,max(matrix), by = 1))

dev.new(width=50, height=50)

heatmap.2(matrix,
  # main = "Correlation", # heat map title
  cellnote = matrix, # display values on the heatmap cells
  notecol="black", # change font color of cell labels to black
  notecex = 0.6,
  key = TRUE,
  keysize = 0.9,
  labRow = "", # turn-off x labels
  labCol = "", # turn-off y labels
  density.info="none",  # turns off density plot inside color legend
  trace="none",         # turns off trace lines inside the heat map
  colsep=1:ncol(matrix), # Add vertical grid lines
  #rowsep=1:nrow(matrix), # Add horizontal grid lines
  rowsep = c(9,12,16,30,36),
  sepcolor = "white", # Color gridlines white
  sepwidth = c(0.05, 0.1),
  margins =c(4,4),     # widens margins around plot. Quanto menor o valor, maior o plot
  col=my_pallete,
  breaks = col_breaks,
  dendrogram="col",     # only draw a column dendrogram. Utilize dendrogram="none" para nao clusterizar
  cexRow=0.4, #tamanho da letra das linhas
  cexCol=0.6, #tamanho da letra das colunas
  srtCol=90, #rotaciona os nomes das colunas em 25 graus
  Rowv=FALSE,  # turn off row clustering, pois organizei de acordo com a coluna CDH
  Colv=TRUE)            # turn off column clustering

dev.copy2pdf(file='plot.pdf')
graphics.off() # limpar todas as figuras geradas.
