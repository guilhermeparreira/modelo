#------------------------------------------------------------#
# Para rodar este programa deixe no objeto fit.model a saída
# do ajuste da  regressão com erros binomial negativa e
# ligação log.  Deixe  os dados disponíveis através do comando
# attach(...). Depois  use  o comando source(...) no R ou
# S-Plus para executar  o programa. A sequência de comandos
# é a seguinte:
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_nbin")
#
# A saída será o gráfico de  envelope para o resíduo
# componente  do desvio padronizado. Para colocar um título
# no gráfico após a saída use title("..."). Para usar outras
# ligações incluir em glm.nb o termo link=identity para
# ligação identidade ou o termo link=sqrt para 
# ligação raíz quadrada.
#------------------------------------------------------------#
envel_nbin <- function(fit.model, data){
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
fi <- fit.model$theta
e <- matrix(0,n,100)
#
for(i in 1:100){
  resp <- rnegbin(n, fitted(fit.model),fi)
  fit <- glm.nb(resp ~ X)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
}
#------------------------------------------------------------#                      
