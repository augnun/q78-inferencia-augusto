summary(mtcars)
### Mtcars é um  banco de dados interno do R, onde as variáveis MPG= Milhas por galão e CYL= Número de Cilindros
mean<-tapply(mtcars$mpg, list(mtcars$cyl), mean)
sd<-tapply(mtcars$mpg, list(mtcars$cyl), sd)
x <- data.frame(mean,sd)
### Criando um conjunto de dados com média e desvio padrão. Uso do comando tapply é para retornar uma lista de vetores. Ao final criou-se um data-frame com as variáveis mean(méda) e sd(desvio padrão) 
x$LL <- x$mean-1,96*x$sd
x$UL <- x$mean+196*x$sd
### criando os limites superior e inferior dos intervalos de confiança.
title <-"MPG por Número de Cilindros com Intervalo de Confiança de 95%"
x11() ## comando utilzado para plotar o gráfico em uma janela a parte
dotchart(x$mean, col="blue", xlim=c(floor(min(x$LL)/10)*10, ceiling(max(x$UL)/10)*10), main=title )

for (i in 1:nrow(x)){
  lines(x=c(x$LL[i],x$UL[i]), y=c(i,i))
}
grid() ## comando para adicionar as linhas horizontais no gráfico
### Plotando o gráfico dotchart com os intervalos de confiança.


# Conclusão: Diante do gráfico acima podemos observar que as variáveis são inversamente proporcionais, ou seja, quanto maior o número de cilindros, menor a quantidade de combustível o veículo necessita para deslocar-se por uma milha. E mais, percebe-se que intervalos com mesmo nível de confiança (0,95) possuem comprimentos diferentes, pois quando o número de cilindros é igual a 4, o intervalo de confiança é o de maior amplitude e quando o número de cilindros é 6, o intervalo de confiança é o de menor amplitude.
# 
# Nota: A programação a seguir, feita como resolução do exercício 5.14 do livro Introdução à Inferência Estatística Heleno Bolfarine / Mônica Carneiro Sandoval tem caráter apenas motivacional, tendo em vista que trata-se do mesmo tema abordado no presente trabalho.

## primeiramente vamos definir a função em (3.1.8)
rbolfarine <-function (n, theta)
{
  sample <- runif(n)
  (-1 + 2 * sqrt(0.25 - theta * (0.5 - 0.25 * theta - sample)))/theta
}
## calculando informação de fisher
inffisher <- function (theta.j)
{
  1/(2 * theta.j^3) * (log((1 + theta.j)/(1 - theta.j)) - 2 *
                         theta.j)
}
## aproximando valores de tetha com iterações sucessivas
mmvapprox2 <- function (x, teta.0 = mean(x), maxeps = 1e-04)
{
  eps <- maxeps + 1
  while (eps > maxeps) {
    teta.i <- teta.0 + sum(x/(1 + teta.0 * x))/sum(x^2/(1 +
                                                          teta.0 * x)^2)
    eps <- abs(teta.i - teta.0)
    teta.0 <- teta.i
  }
  teta.i
}
## Cálculo do intervalo de confiança
IC.complete <- function (x, conf = 0.95, teta = NULL)
{
  teta.hat <- mmvapprox2(x, maxeps = 1e-07)
  lower <- teta.hat + qnorm((1 - conf)/2)/sqrt(length(x) *
                                                 inffisher(teta.hat))
  upper <- teta.hat - qnorm((1 - conf)/2)/sqrt(length(x) *
                                                 inffisher(teta.hat))
  if (is.null(teta)) {
    c(lower, upper)
  }
  else {
    if (teta >= lower && teta <= upper) {
      1
    }
    else {
      0
    }
  }
}
## considerando uma amostra n=20 e tetha=0.4, vamos construir um IC com confiança de 95%
sample <- rbolfarine(20, 0.4)
IC.complete(sample)
## esse intervalo está certo? Para responder a essa pergunta, vamos construir um exemplo com 1000 amostras de tamanho 1000 e construir 1000 intervalos de confiança com coeficiente de confiança 0.25. Ao final dividimos por 1000 porque isso é uma proporção. O resultado observado é a proporção de resultados que possuem o parâmetro tetha.
sum(apply(matrix(rbolfarine(1000 * 1000, 0.4), ncol = 1000),
          + 2, IC.complete, conf = 0.25, teta = 0.4))/1000
## mesma simulação feita acima com coeficiente de confiança 0.95
sum(apply(matrix(rbolfarine(1000 * 1000, 0.4), ncol = 1000),
          + 2, IC.complete, conf = 0.95, teta = 0.4))/1000

# As duas últimas programações mostram que o intervalo de confiança gerado pelo software R estão bem próximas do que foi exigido no comando.
