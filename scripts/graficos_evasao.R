# ==========================================
# Análise de evasão - UFCG
# Geração de gráficos para dissertação
# ==========================================
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

# -----------------------------
# CAMINHO ABSOLUTO (IMPORTANTE)
# -----------------------------
pasta_dados <- "/home/diego/Documentos/estatisticas-periodos-exatos-ufcg/dados"
pasta_saida <- "/home/diego/Documentos/estatisticas-periodos-exatos-ufcg/outputs"

# -----------------------------
# Criar pasta outputs se não existir
# -----------------------------
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida)
}

# -----------------------------
# Ler dados
# -----------------------------
p1 <- read.csv(paste0(pasta_dados, "/evasao_p1.csv"))
p1$periodo_num <- 1

p2 <- read.csv(paste0(pasta_dados, "/evasao_p2.csv"))
p2$periodo_num <- 2

p3 <- read.csv(paste0(pasta_dados, "/evasao_p3.csv"))
p3$periodo_num <- 3

p4 <- read.csv(paste0(pasta_dados, "/evasao_p4.csv"))
p4$periodo_num <- 4

# Juntar dados
dados <- rbind(p1, p2, p3, p4)

# -----------------------------
# BOXPLOT
# -----------------------------
g1 <- ggplot(dados, aes(x = factor(periodo_num), y = taxa, fill = factor(curriculo))) +
  geom_boxplot() +
  labs(title = "Distribuição da Evasão", x = "Período", y = "Taxa (%)") +
  theme_minimal()

ggsave(paste0(pasta_saida, "/boxplot.png"), g1, width = 8, height = 5)

# -----------------------------
# MÉDIA POR PERÍODO
# -----------------------------
media <- aggregate(taxa ~ curriculo + periodo_num, dados, mean)

g2 <- ggplot(media, aes(x = periodo_num, y = taxa, color = factor(curriculo))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Evolução da Evasão", x = "Período", y = "Taxa média (%)") +
  theme_minimal()

ggsave(paste0(pasta_saida, "/linha.png"), g2, width = 8, height = 5)

# -----------------------------
# BARRAS
# -----------------------------
g3 <- ggplot(media, aes(x = factor(periodo_num), y = taxa, fill = factor(curriculo))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação por Período", x = "Período", y = "Taxa (%)") +
  theme_minimal()

ggsave(paste0(pasta_saida, "/barras.png"), g3, width = 8, height = 5)

# -----------------------------
# DIFERENÇA
# -----------------------------
media_1999 <- media[media$curriculo == 1999, ]
media_2017 <- media[media$curriculo == 2017, ]

diff <- merge(media_1999, media_2017, by = "periodo_num")
diff$diferenca <- diff$taxa.x - diff$taxa.y

g4 <- ggplot(diff, aes(x = periodo_num, y = diferenca)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Diferença (1999 - 2017)", x = "Período", y = "Diferença (%)") +
  theme_minimal()

ggsave(paste0(pasta_saida, "/diferenca.png"), g4, width = 8, height = 5)

print("Gráficos gerados com sucesso!")