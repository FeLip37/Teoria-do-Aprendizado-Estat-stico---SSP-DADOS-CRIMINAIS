dados <- read.csv2("Dataset_Municipios_Estatistica_Formatado.csv", sep=";", check.names = FALSE)

dados <- subset(dados, as.numeric(ANO_ESTATISTICA) %in% c(2023, 2024, 2025))

furtos <- dados$`FURTO - OUTROS`
ano <- dados$ANO_ESTATISTICA

trafico <- dados[, grep("TR.FICO", names(dados))[1]]
homicidios <- dados[, grep("HOMIC.DIO DOLOSO$", names(dados))]


media <- mean(furtos, na.rm = TRUE)
mediana <- median(furtos, na.rm = TRUE)
variancia <- var(furtos, na.rm = TRUE)
desvio <- sd(furtos, na.rm = TRUE)
cv <- (desvio / media) * 100
media_geo <- exp(mean(log(furtos + 1), na.rm = TRUE)) - 1

quartis <- quantile(furtos, probs = c(0.25, 0.75), na.rm = TRUE)
amplitude <- max(furtos, na.rm = TRUE) - min(furtos, na.rm = TRUE)

ni <- tapply(furtos, ano, length)
vari <- tapply(furtos, ano, var, na.rm = TRUE)
media_variancias <- sum(ni * vari) / sum(ni)

categoria_furto <- ifelse(furtos > media, "Acima da Media", "Abaixo da Media")
tabela_contingencia <- table(ano, categoria_furto)
teste_qui <- chisq.test(tabela_contingencia)

correlacao <- cor(trafico, homicidios, use = "complete.obs")

par(mfrow = c(1, 2)) # Divide a tela em 2

boxplot(furtos ~ ano, data = dados,
        col = "darkblue", border = "grey",
        main = "Distribuição de Furtos por Ano",
        xlab = "Ano", ylab = "Furtos",
        outline = FALSE)
hist(furtos[furtos < 100],
     main = "Histograma de Furtos",
     xlab = "Furtos (Ate 100 casos)", ylab = "Frequencia",
     col = "lightblue", border = "grey")

par(mfrow = c(1, 1)) # Volta a tela ao normal (1 grafico por vez)

dados_interior <- subset(dados, NOME_MUNICIPIO != "S.PAULO")

tabela_limpa <- data.frame(
  Municipio = dados_interior$NOME_MUNICIPIO,
  Homicidios = dados_interior[, grep("HOMIC.DIO DOLOSO$", names(dados_interior))]
)

hom_por_cidade <- aggregate(Homicidios ~ Municipio, data = tabela_limpa, FUN = sum)

top5_hom <- hom_por_cidade[order(hom_por_cidade$Homicidios, decreasing = TRUE), ][1:5, ]

par(mar = c(8, 4, 4, 2)) 

barplot(top5_hom$Homicidios, 
        names.arg = top5_hom$Municipio,
        main = "Top 5 Cidades do Interior: Homicídios (2023-2025)",
        col = "darkred",
        ylab = "Total de Registros",
        border = "black",
        ylim = c(0, max(top5_hom$Homicidios) * 1.2),
        las = 2,           
        cex.names = 0.8)

par(mar = c(5, 4, 4, 2))

cat("\n Media Aritmetica:", media)
cat("\n Media Geometrica:", media_geo)
cat("\n Mediana (Q2):", mediana)
cat("\n 1º Quartil (Q1):", quartis[1])
cat("\n 3º Quartil (Q3):", quartis[2])
cat("\n Variancia Total:", variancia)
cat("\n Desvio Padrao:", desvio)
cat("\n Amplitude Total:", amplitude)
cat("\n Coeficiente de Variacao (%):", cv)
cat("\n Media das Variancias Anuais:", media_variancias)
cat("\n Correlacao de Pearson (Trafico x Homicidios):", correlacao, "\n")
cat("\n Tabela de Dupla Entrada (Ano x Categoria):\n")
print(tabela_contingencia)