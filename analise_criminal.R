# --- TRABALHO DE ESTATÍSTICA: ANÁLISE DE SEGURANÇA PÚBLICA ---

# 1. Configurações Iniciais
graphics.off() # Fecha qualquer gráfico pendente
setwd("c:/Users/fedut") # Define sua pasta de trabalho

# Carrega a biblioteca gráfica
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 2. Importação dos Dados
dados <- read.csv2("Dataset_Municipios_Estatistica.csv", stringsAsFactors = TRUE)

# 3. Análise Estatística (Medidas de Tendência Central)
# Vamos analisar a variável quantitativa 'FURTO.DE.VEÍCULO'
variavel_quant <- dados$FURTO.DE.VEÍCULO

media_amostral <- mean(variavel_quant, na.rm = TRUE)
media_aparada  <- mean(variavel_quant, trim = 0.05, na.rm = TRUE)
mediana_val    <- median(variavel_quant, na.rm = TRUE)

# Exibe os resultados no terminal
cat("--- Medidas de Tendência Central ---\n")
cat("Média Amostral:", round(media_amostral, 2), "\n")
cat("Média Aparada (5%):", round(media_aparada, 2), "\n")
cat("Mediana:", mediana_val, "\n")

# 4. Geração do Boxplot (Variável Qualitativa no eixo X)
# O as.factor(ANO_ESTATISTICA) transforma o ano em uma categoria (Qualitativa)
png("boxplot_analise_final.png", width = 800, height = 600)

p <- ggplot(dados, aes(x = as.factor(ANO_ESTATISTICA), y = FURTO.DE.VEÍCULO, fill = as.factor(ANO_ESTATISTICA))) +
  geom_boxplot(showfliers = FALSE, color = "black") + # showfliers = FALSE foca na 'caixa'
  labs(title = "Comparativo de Furtos de Veículos por Ano",
       subtitle = "Análise da variabilidade entre municípios paulistas",
       x = "Ano (Variável Qualitativa)", 
       y = "Ocorrências (Variável Quantitativa)",
       fill = "Legenda Ano") +
  theme_minimal()

print(p)
dev.off()

cat("\nSucesso! O gráfico 'boxplot_analise_final.png' foi gerado.\n")