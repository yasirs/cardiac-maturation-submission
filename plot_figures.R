
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")





library(data.table)
nodes_df  = read.table(file = "all_acto_nodes_df.txt", header = T, row.names = 1, sep = "\t")
nodes_df$name = rownames(nodes_df)
nodes_df <- as.data.table(nodes_df)
setkey(nodes_df, "name")

intermediate_genes = data.frame(name="", Integrins=0, Actomyosin=0, stringsAsFactors = F)

for (nn in nodes_df[node_type=="other"][nodereach!="none"][,name]) {
  intermediate_genes <- rbind(intermediate_genes, data.frame(name = nn, 
                                                             Integrins=(if (nodes_df[nn, nodereach] %in% c("both","int5")) 1 else 0),
                                                             Actomyosin=(if (nodes_df[nn, nodereach] %in% c("both","act5")) 1 else 0),
                                                             stringsAsFactors = F))
}

intermediate_genes = intermediate_genes[-1,]
intermediate_genes <- as.data.table(intermediate_genes)
intermediate_genes <- intermediate_genes[order(Integrins - 0.5*Actomyosin),]

int_melted = melt(intermediate_genes, id.vars="name")
int_melted$value <- as.factor(int_melted$value)
int_melted$name <- factor(x = int_melted$name, levels = intermediate_genes$name)

pp1 = ggplot(data = int_melted, aes(name, variable) ) + geom_tile(aes(fill=value), color="white", size=4) + theme_classic() + 
  labs(x="Intermediary\nGene", y="Activation") + scale_fill_manual(values = c("lightgrey", "darkgreen"))  +
  theme(text = element_text(family="Palatino Linotype", face="bold", color="black", size=12) , axis.text.x = element_text(angle = 45, hjust=1))

pdf("all_acto_intermediate_genes.pdf", width = 7, height = 3)
print(pp1)
dev.off()

tiff("all_acto_intermediate_genes.tiff", width = 7, height = 2, res = 400, units = "in", compression = "zip")
print(pp1)
dev.off()

