library(factoextra)
library(fpc)
library(here)
library(data.table)
library(ggdendro)

# Beispiel Abbildung for dichtebasiertes Clustern------------------------------

data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

dbc_plot <- fviz_cluster(
  db, df, stand = FALSE, frame = FALSE, 
  geom = "point", ellipse = T, pointsize = 0.5, 
  outlier.color = "grey") +
  labs(
    title = "Dichtebasiertes Clustering",
    x = "Dimension 1", y = "Dimension 2") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(), axis.ticks = element_blank(), 
        legend.title = element_blank())
dbc_plot

ggsave(
  plot = dbc_plot, 
  filename = here("figures/T14/T14-DichtebasiertesClustering.pdf"), 
  width = 4, height = 3)

# Beispielabbildung für hierarchische Clustering-------------------------------

data(USArrests)
dd <- dist(scale(USArrests[1:10,]), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

hc_plot <- ggdendrogram(hc, rotate = F, theme_dendro = FALSE) +
  labs(title = "Hierarchiches Clustering") +
  scale_y_continuous(expand = expansion()) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(), axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

ggsave(
  plot = hc_plot, 
  filename = here("figures/T14/T14-HierarchischeClustering.pdf"), 
  width = 3, height = 2)

# Beispielanwendung PCA - Schweizer Blüten-------------------------------------

bank_notes <- data.table::fread(
  file = here::here("data/T14/T14-SwissBankNotes.csv"), 
  sep = ";", dec = ",") %>%
  dplyr::rename(Art=`note type`) %>%
  dplyr::mutate(Art=ifelse(Art=="counterfeit", "Fälschung", "Echt"))
head(bank_notes[, 2:7])

bn_components <- prcomp(bank_notes[, 2:7], center = TRUE,scale. = TRUE)

plot_data <- data.frame(bn_components$x)
plot_data$Art <- bank_notes$Art

biplot_full <- ggplot(
  data = plot_data, aes(x=PC1, y=PC2, color=Art)) +
  geom_point() +
  scale_color_viridis_d() +
  labs(
    title = "Schweizer Banknoten - PCA",
    x="Komponente 1 (49.1% Varianz)", 
    y="Komponente 2 (21.3% Varianz)") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(), legend.position = "none")
biplot_full

biplot_raw <- biplot_full + geom_point(color="black")

ggsave(
  plot = biplot_full, 
  filename = here::here("figures/T14/T14-PCA-Bank-full.pdf"), 
  width = 4, height = 3)

ggsave(
  plot = biplot_raw, 
  filename = here::here("figures/T14/T14-PCA-Bank-raw.pdf"), 
  width = 4, height = 3)
