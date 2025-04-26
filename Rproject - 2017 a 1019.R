# Instalando os pacotes
packages <- c("readxl", "ca", "ggplot2", "ggrepel", "knitr", "kableExtra", "stats", "pheatmap", "reshape2", "scales")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# Carregar
library(readxl)
library(ca)
library(ggplot2)
library(ggrepel)
library(knitr)
library(kableExtra)
library(stats)
library(pheatmap)
library(reshape2)
library(scales)

# Caminho do arquivo
caminho_arquivo <- "C:/Users/maria/Desktop/TCC 3 ANOS/Tabela de contingencia - desempregados por escolaridade.xlsx"

# Carregar a tabela de contingência
dados <- read_excel(caminho_arquivo)

# Limpeza
colnames(dados) <- trimws(colnames(dados))
dados <- na.omit(dados)

# Converter para matriz de contingência
dados_matriz <- as.matrix(dados[, -1])
rownames(dados_matriz) <- dados[[1]]

### 1. Análise de Correspondência

# Teste Qui-Quadrado
chi2_test <- chisq.test(dados_matriz)

# Tabela com p-valor
p_valor_tabela <- data.frame(
  Estatística_qui2 = chi2_test$statistic,
  Grau_de_liberdade = chi2_test$parameter,
  p_valor = chi2_test$p.value
)

# Exibir a tabela formatada para exportar
kable(p_valor_tabela, caption = "Teste Qui-Quadrado para Análise de Correspondência") %>%
  kable_styling(full_width = FALSE)

### 2. Ajustar os dados brutos para escala -1 a 1

# Normalizar os dados para a escala -1 a 1
normalizar_menos1_1 <- function(x) {
  2 * (x - min(x)) / (max(x) - min(x)) - 1
}
dados_normalizados <- apply(dados_matriz, 2, normalizar_menos1_1)

# Gerar mapa de calor dos dados normalizados
pheatmap(dados_normalizados,
         main = "Mapa de Calor - Dados Brutos",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         breaks = seq(-1, 1, length.out = 101))

### 3. Resíduos padronizados

# Gerar mapa de calor dos resíduos padronizados
residuos <- (dados_matriz - chi2_test$expected) / sqrt(chi2_test$expected)
pheatmap(residuos,
         main = "Mapa de Calor - Resíduos Padronizados",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100))

### 4. Gráfico de Barras

# Derreter dados para gráfico de barras
dados_long <- melt(dados_matriz)
colnames(dados_long) <- c("Escolaridade", "Ano", "Desempregados")

# Paleta de cores
cores_barras <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a")

# Gráfico de barras com formatação do eixo y
ggplot(dados_long, aes(x = Ano, y = Desempregados, fill = Escolaridade)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cores_barras) +
  scale_y_continuous(labels = function(x) format(x/1e6, digits = 2)) + # Formata para milhões
  theme_minimal(base_size = 14) +
  labs(title = "Desempregados por escolaridade e ano",
       x = "Ano",
       y = "Número de desempregados (Milhões)",
       fill = "Nível de escolaridade")

### 5. Análise de Correspondência

# Análise
resultado_ac <- ca(dados_matriz)

# Coordenadas
coord_linhas <- data.frame(resultado_ac$rowcoord, Tipo = "Nível de Escolaridade", Categoria = rownames(resultado_ac$rowcoord))
coord_colunas <- data.frame(resultado_ac$colcoord, Tipo = "Ano", Categoria = colnames(dados_matriz))
coord_total <- rbind(coord_linhas, coord_colunas)

# Cores
cores_perceptual <- c("Nível de Escolaridade" = "#8B0000", "Ano" = "#003366")

# Mapa perceptual
ggplot(coord_total, aes(Dim1, Dim2, label = Categoria, color = Tipo)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text_repel(size = 5, fontface = "bold") +
  scale_color_manual(values = cores_perceptual) +
  theme_minimal(base_size = 14) +
  labs(title = "Mapa Perceptual - Análise de correspondência entre escolaridade e desemprego",
       x = "Dimensão 1",
       y = "Dimensão 2",
       color = "Tipo de Categoria")
