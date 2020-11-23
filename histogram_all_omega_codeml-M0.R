require(ggplot2)

setwd('/home/user/dnds/')
remove(list=ls(all=TRUE)) #remove todos objetos criados

dados = read.csv(file = 'all_omega_codeml-M0.txt', header = FALSE, sep = '\t')
dados = dados[,4] # apenas a coluna 4 contem as informacoes de dN/dS

# contar o numero de genes em intervalos de 0.005 de dN/dS
dados_plot = c(NULL)
faixa = 0.005
contagem = 0 # armazena o numero de genes com dN/dS para determinada faixa
while (faixa <= 1.500) {
  for (omega in dados) {
    if(omega >= (faixa-0.005) && omega < faixa){
      contagem = contagem + 1
    }
    else if(faixa > 1.495 && omega >= faixa){ # agrupa todos os genes com dN/dS >= 1.5 aqui. Eh o ultimo loop while.
      contagem = contagem + 1
    }
  }
  dados_plot = rbind(dados_plot,c(faixa,contagem))
  faixa = faixa + 0.005
  contagem = 0
}

# contar quantos genes tem dN/dS > 1
omega_maior = 0
omega_menor = 0
for(row in 1:nrow(dados_plot)){
  if(dados_plot[row,1] > 1){
    omega_maior = omega_maior + dados_plot[row,2]
  }
  else{
    omega_menor = omega_menor + dados_plot[row,2]
  }
}

# converter em data.frame para plotar
dados_plot = as.data.frame(dados_plot)

dev.new(width=35, height=20)
ggplot(data =  dados_plot, aes(x = dados_plot[,1], y=dados_plot[,2])) + # usei 'reorder' para organizar os dados em ordem decrescente.
  geom_bar(colour="black",stat="identity", fill="grey80") +
  ylim(0,250) +
  theme_classic() +
  xlab('') +
  ylab('Gene Count') +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, color = 'black')) +
  geom_vline(xintercept = 1, linetype="dotted", color = "black", size = 0.4) +
  annotate(geom = "text", x = 1.25, y = 200, label = omega_maior, color = "black", size = 3) +
  annotate(geom = "text", x = 0.5, y = 200, label = omega_menor, color = "black", size = 3)
dev.copy2pdf(file='histograma_codeml_M0_african-clade.pdf')

