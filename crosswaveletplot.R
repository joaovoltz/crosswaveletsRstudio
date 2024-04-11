# Carregar a biblioteca WaveletComp 
library(WaveletComp)

# Carregar os dados
dados <- read.csv("dados de entrada", header = TRUE)

# Nesse caso eu tinha colunas separadas para DATA e HORA
data <- as.POSIXct(paste(dados$DATA, dados$HORA), format = "%Y-%m-%d %H:%M:%S")

# Aqui defini o intervalo de datas desejado para outubro a dezembro de 2015
data_inicial <- as.POSIXct("2015-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
data_final <- as.POSIXct("2015-12-31 23:59:59", format = "%Y-%m-%d %H:%M:%S")

# Criando o DataFrame com os dados formatados diretamente com o intervalo de datas
my.data <- data.frame(date = data, x = dados$NEE_uStar_f, y = dados$Tair_f)

# Filtrando o DataFrame para incluir apenas as datas dentro do intervalo desejado
my.data <- subset(my.data, date >= data_inicial & date <= data_final)

# Verificando se há dados no intervalo desejado
if (nrow(my.data) > 0) {
  # Realize a análise de coerancia nos dados filtrados
  my.wc <- analyze.coherency(my.data, my.pair = c("x", "y"),
                             loess.span = 0,
                             dt = 1/12, dj = 1/250,
                             lowerPeriod = 1/2,
                             upperPeriod = 32,
                             make.pval = TRUE, n.sim = 10)
  
  # Plot da coerência entre as variaveis
  wc.image(my.wc, n.levels = 250,
           legend.params = list(lab = "Espectro de Coerência"),
           show.date = TRUE, date.format = "%Y-%m-%d",
           timelab = "ano", periodlab = "Período")
  
  # Plot do espectro de ondeleta de x
  wt.image(my.wc, my.series = "x",
           legend.params = list(lab = "Espectro de Ondeleta de x"),
           show.date = TRUE, date.format = "%Y-%m-%d",
           timelab = "ano", periodlab = "Período")
  
  # Coerência média
  wc.avg(my.wc, sigpch = 20)
  
} else {
  # Caso não haja dados dentro do intervalo de datas especificado
  print("Não há dados dentro do intervalo de datas especificado .")
}
