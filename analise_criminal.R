# ==============================================================================
# SCRIPT DEFINITIVO: ANÁLISE DE CRIMINALIDADE (AULAS 2 E 5)
# ==============================================================================

# 1. CARREGAMENTO DOS DADOS
# Certifique-se de que o arquivo está na mesma pasta do seu script
dados <- read.csv2("Dataset_Municipios_Estatistica_Formatado.csv", 
                   sep = ";", check.names = FALSE, stringsAsFactors = TRUE)

# Definimos a variável de estudo (Exemplo: FURTO - OUTROS)
# Para trocar o crime, basta mudar o nome entre as crases abaixo
crime_estudo <- "FURTO - OUTROS"
x <- dados[[crime_estudo]]

# ==============================================================================
# PARTE 1: ESTATÍSTICA DESCRITIVA (AULA 2)
# ==============================================================================

# --- Estimativas de Localização ---
media_arit   <- mean(x, na.rm = TRUE)
mediana      <- median(x, na.rm = TRUE)
# Média Geométrica (Ajustada para zeros: log(x+1) conforme pág. 10 da Aula 2)
media_geom   <- exp(mean(log(x + 1), na.rm = TRUE)) - 1
quartis      <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# --- Medidas de Variabilidade ---
variancia_tot <- var(x, na.rm = TRUE)
desvio_padrao <- sd(x, na.rm = TRUE)
amplitude     <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
# Coeficiente de Variação (CV)
cv_perc       <- (desvio_padrao / media_arit) * 100

# --- Tabelas de Frequência ---
freq_anos     <- table(dados$ANO_ESTATISTICA)
prop_anos     <- prop.table(freq_anos)

# ==============================================================================
# PARTE 2: ANÁLISE POR CATEGORIA E INFERÊNCIA (AULA 5)
# ==============================================================================

# --- Média das Variâncias (pág. 18 da Aula 5) ---
# n_i = meses por ano | var_i = variância por ano
n_i   <- tapply(x, dados$ANO_ESTATISTICA, length)
var_i <- tapply(x, dados$ANO_ESTATISTICA, var, na.rm = TRUE)

# Filtro para ignorar anos com 1 dado só (ex: 2021) que impedem o cálculo
validos <- !is.na(var_i)
media_variancias <- sum(n_i[validos] * var_i[validos]) / sum(n_i[validos])

# ==============================================================================
# PARTE 3: GRÁFICOS OTIMIZADOS (VISUALMENTE BONS)
# ==============================================================================

# Configuramos a tela para mostrar 2 gráficos por vez
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# --- Gráfico 1: Boxplot Logarítmico (Aula 5) ---
# Usamos log1p para "desachatado" o gráfico e ver a diferença entre anos
boxplot(log1p(x) ~ ANO_ESTATISTICA, data = dados,
        main = paste("Boxplot:", crime_estudo),
        xlab = "Ano", ylab = "Frequência (log1p)",
        col = c("#4e79a7", "#e15759", "#76b7b2", "#54a24b"),
        pch = 19, cex = 0.4, outcol = "gray")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# --- Gráfico 2: Histograma Logarítmico (Aula 2) ---
hist(log1p(x), breaks = 30,
     main = paste("Histograma:", crime_estudo),
     xlab = "log1p(Ocorrências)", ylab = "Frequência Absoluta",
     col = "skyblue", border = "white")
# Adiciona linha da média aritmética transformada no histograma
abline(v = log1p(media_arit), col = "red", lwd = 2, lty = 2)

# Reseta o layout da tela
par(mfrow = c(1, 1))

# ==============================================================================
# PARTE 4: RELATÓRIO DE RESULTADOS NO CONSOLE
# ==============================================================================

cat("\n======================================================\n")
cat("          RELATÓRIO ESTATÍSTICO DE CRIMINALIDADE      \n")
cat("======================================================\n")
cat("Crime Analisado: ", crime_estudo, "\n\n")
cat("[LOCALIZAÇÃO]\n")
cat("- Média Aritmética: ", round(media_arit, 2), "\n")
cat("- Média Geométrica: ", round(media_geom, 2), "\n")
cat("- Mediana:          ", mediana, "\n")
cat("- 1º Quartil (Q1):  ", quartis[1], "\n")
cat("- 3º Quartil (Q3):  ", quartis[3], "\n\n")
cat("[VARIABILIDADE]\n")
cat("- Variância Total:  ", round(variancia_tot, 2), "\n")
cat("- Desvio Padrão:    ", round(desvio_padrao, 2), "\n")
cat("- Coeficiente Var:  ", round(cv_perc, 2), "%\n")
cat("- Amplitude:        ", amplitude, "\n\n")
cat("[INFERÊNCIA - AULA 5]\n")
cat("- Média das Variâncias (Anual):", round(media_variancias, 2), "\n")
cat("======================================================\n")