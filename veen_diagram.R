#PT-BR: script para construir diagramas de Venn. Gera um diagrama com quatro setores.

#EN: script to build Venn diagrams. Generates a diagram with four sectors.

install.packages('VennDiagram')
require(VennDiagram)

# vamos utilizar as saidas do script especiesClusters.py para construir o diagrama.
setwd('/home/tulio/area_de_trabalho/doutorado/R/raquel_filogenomica_bacterias/venn/')
remove(list=ls(all=TRUE)) #remove todos objetos criados

only_borrelia = read.csv(file = 'only_borrelia', header = TRUE, sep = '\t')
only_treponema = read.csv(file = 'only_treponema', header = TRUE, sep = '\t')
only_leptospira = read.csv(file = 'only_leptospira', header = TRUE, sep = '\t')
only_spirochetes = read.csv(file = 'only_spirochetes', header = TRUE, sep = '\t')
borrelia_treponema = read.csv(file = 'borrelia_treponema', header = TRUE, sep = '\t')
borrelia_leptospira = read.csv(file = 'borrelia_leptospira', header = TRUE, sep = '\t')
borrelia_spirochetes = read.csv(file = 'borrelia_spirochetes', header = TRUE, sep = '\t')
treponema_leptospira = read.csv(file = 'treponema_leptospira', header = TRUE, sep = '\t')
treponema_spirochetes = read.csv(file = 'treponema_spirochetes', header = TRUE, sep = '\t')
leptospira_spirochetes = read.csv(file = 'leptospira_spirochetes', header = TRUE, sep = '\t')
borrelia_treponema_leptospira = read.csv(file = 'borrelia_treponema_leptospira', header = TRUE, sep = '\t')
borrelia_treponema_spirochetes = read.csv(file = 'borrelia_treponema_spirochetes', header = TRUE, sep = '\t')
borrelia_leptospira_spirochetes = read.csv(file = 'borrelia_leptospira_spirochetes', header = TRUE, sep = '\t')
treponema_leptospira_spirochetes = read.csv(file = 'treponema_leptospira_spirochetes', header = TRUE, sep = '\t')
borrelia_treponema_leptospira_spirochetes = read.csv(file = 'borrelia_treponema_leptospira_spirochetes', header = TRUE, sep = '\t')

only_borrelia = nrow(only_borrelia)
only_treponema = nrow(only_treponema)
only_leptospira = nrow(only_leptospira)
only_spirochetes = nrow(only_spirochetes)
borrelia_treponema = nrow(borrelia_treponema)
borrelia_leptospira = nrow(borrelia_leptospira)
borrelia_spirochetes = nrow(borrelia_spirochetes)
treponema_leptospira = nrow(treponema_leptospira)
treponema_spirochetes = nrow(treponema_spirochetes)
leptospira_spirochetes = nrow(leptospira_spirochetes)
borrelia_treponema_leptospira = nrow(borrelia_treponema_leptospira)
borrelia_treponema_spirochetes = nrow(borrelia_treponema_spirochetes)
borrelia_leptospira_spirochetes = nrow(borrelia_leptospira_spirochetes)
treponema_leptospira_spirochetes = nrow(treponema_leptospira_spirochetes)
borrelia_treponema_leptospira_spirochetes = nrow(borrelia_treponema_leptospira_spirochetes)

area_borrelia = only_borrelia + borrelia_treponema + borrelia_leptospira + borrelia_spirochetes + borrelia_treponema_leptospira + borrelia_treponema_spirochetes + borrelia_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
area_treponema = only_treponema + borrelia_treponema + treponema_leptospira + treponema_spirochetes + borrelia_treponema_leptospira + borrelia_treponema_spirochetes + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
area_leptospira = only_leptospira + borrelia_leptospira + treponema_leptospira + leptospira_spirochetes + borrelia_treponema_leptospira + borrelia_leptospira_spirochetes + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
area_spirochetes = only_spirochetes + borrelia_spirochetes + treponema_spirochetes + leptospira_spirochetes + borrelia_treponema_spirochetes + borrelia_leptospira_spirochetes + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n12 = borrelia_treponema + borrelia_treponema_leptospira + borrelia_treponema_spirochetes + borrelia_treponema_leptospira_spirochetes
n13 = borrelia_leptospira + borrelia_treponema_leptospira + borrelia_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n14 = borrelia_spirochetes + borrelia_treponema_spirochetes + borrelia_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n23 = treponema_leptospira + borrelia_treponema_leptospira + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n24 = treponema_spirochetes + borrelia_treponema_spirochetes + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n34 = leptospira_spirochetes + borrelia_leptospira_spirochetes + treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n123 = borrelia_treponema_leptospira + borrelia_treponema_leptospira_spirochetes
n124 = borrelia_treponema_spirochetes + borrelia_treponema_leptospira_spirochetes
n134 = borrelia_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n234 = treponema_leptospira_spirochetes + borrelia_treponema_leptospira_spirochetes
n1234 = borrelia_treponema_leptospira_spirochetes

# Chart
draw.quad.venn(area1 = area_borrelia, area2 = area_treponema, area3 = area_leptospira, area4 = area_spirochetes,
                 n12 = n12, n13 = n13, n14 = n14,
                 n23 = n23, n24 = n24,
                 n34 = n34,
                 n123 = n123, n124 = n124, n134 = n134,
                 n234 = n234,
                 n1234 = n1234,
                 category = c("Borrelia", "Treponema", "Leptospira", "Spirochetes"), 
                 fill = c("steelblue", "mediumorchid3", "tomato", "gold"),
               col = c("black","black","black","black"),
               alpha = c(0.6,0.6,0.6,0.6),
                 cex = 1.3)
