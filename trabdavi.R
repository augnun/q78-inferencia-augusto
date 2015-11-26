#minha programação
#gerando os dados da população
x=rnorm(1000, 18, 9)
summary(x)
sd(x)


# Construindo intervalo de confiança
norm.interval = function(data, variance = var(data), conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  inter=c(xbar - z * sdx, xbar + z * sdx) 
  return(inter)}
norm.interval(x)


#Simulação
n.draw = 100 #quantidades de amostras
mu = 12 #media da amostra
n = 30 #tamanho da amostra
SD = sd(x) #dp da população (conhecido)
draws = matrix(rnorm(n.draw * n, mu, SD), n) #criação de conjunto de dados com todas as amostras, cada uma em uma coluna

intervalo = apply(draws, 2, norm.interval) #criando os intervalos 
intervalo
x11() #criando um gráfico para visualisar a distribuição dos intervalos em comparação com a média
plot(range(intervalo), c(0, 1 + n.draw), type = "n", xlab = "mean tail length", ylab = "sample run")
for (i in 1:n.draw) lines(intervalo[, i], rep(i, 2), lwd = 2)
abline(v = 12, lwd = 2, lty = 2)
