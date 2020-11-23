#PT-BR: Script para alterar o estilo de arvores filogenéticas. Adiciona informação sobre perda/aquisição de genes em cada ramo da árvore (calculado utilziando algoritmo Dollop do pacote PHYLIP).

#EN: Script to change the style of phylogenetic trees. Adds information about gene losses/acquisitions in each branch of the tree (calculated using the Dollop algorithm of the PHYLIP package).

install.packages("BiocManager")
BiocManager::install("ggtree")
require('tidyr')
require(ggtree)

remove(list=ls(all=TRUE)) #remove todos objetos criados

tree = read.tree(file = 'phylogentic_tree_rooted.txt')

# transformacao log do tamanho dos ramos (branch lengths)
tree$edge.length = (log(tree$edge.length)+(min(log(tree$edge.length))*-1))

# Load data regarding gene gains and losses. Is the output from dollo2gainLossGenes.py (last update: 25.09.2019)
geneGainLoss = read.csv(file = 'gain_loss_genes_dollop.count', sep = '\t', header = TRUE)
geneGainLoss_names = geneGainLoss[,1:2]

# merge the two columns of 'geneGainLoss[,1:2]'.
geneGainLoss_names = unite(geneGainLoss[,1:2], Ancestral_node_Node, c('Ancestral_node', 'Node'))

# A funcao para adicionar o ganho/perda de genes na arvore primeiro adiciona o ganho/perda para cada especie (ramos terminais), na mesma ordem que aparecem na arvore. Depois vai adionando o ganho/perda para os ramos internos. Com isso, devo alterar a ordem como essa informacao aparece na variavel 'geneGainLoss'.
species_GeneGainLoss = c(NULL)
ancestral_GeneGainLoss = c(NULL)
for(i in 1:nrow(geneGainLoss_names)){
  match_specie = grepl("[a-z]", geneGainLoss_names[i,1], ignore.case = TRUE) # retrona TRUE ou FALSE se encontrar letras na linha 'line'. Isso significa que eh uma linha de ramo tarminal, ou seja, tem uma especie
  if (match_specie == TRUE){
    species_GeneGainLoss = rbind(species_GeneGainLoss, geneGainLoss[i,])
  }
  else{
    ancestral_GeneGainLoss = rbind(ancestral_GeneGainLoss, geneGainLoss[i,])
  }
}
# jogar o ganho/perda do root para o fim do vetor species_GeneGainLoss
move_root = species_GeneGainLoss[1,]
species_GeneGainLoss = species_GeneGainLoss[-1,]
species_GeneGainLoss = rbind(species_GeneGainLoss, move_root)

orderedGeneGainLoss = rbind(species_GeneGainLoss, ancestral_GeneGainLoss)

# separar genes adquirdos e perdidos em vetores diferetes para plotar na arvore.
orderedGeneGainLoss = orderedGeneGainLoss[,3]
geneGain = c(NULL)
geneLoss = c(NULL)
for(line in orderedGeneGainLoss){
  lineSplit = strsplit(line, '/')
  lineSplitVector = unlist(lineSplit)
  geneGain = c(geneGain,lineSplitVector[1])
  geneLoss = c(geneLoss, lineSplitVector[2])
}

# PLOT TREE
ggplot(tree, aes(x, y)) +
  geom_tree() +
  theme_tree() +
  geom_tiplab(size = 6) +
  geom_nodepoint(color="grey70", alpha=0.9, size=11)+
  geom_rootedge(rootedge = 7) +
  geom_label(aes(x=branch, label=geneGain), fill='lightgreen', size = 5, vjust = 1) +
  geom_label(aes(x=branch, label=geneLoss), fill='tomato', size = 5, vjust = 0) 
  
# Save as PDF and force a 'huge' size plot
ggsave("test.pdf", width = 60, height = 40, units = "cm", limitsize = FALSE)

  ggplot(tree, aes(x, y)) +
  geom_tree() +
  theme_tree() +
  geom_tiplab(size = 6) +
  geom_rootedge(rootedge = 2) 
