#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto   fit.model a
# saída do  ajuste  do modelo de quase-verossimilhança.  
# Deixe  os dados disponíveis  através do comando 
# attach(...). Depois use o comando source(...) no S-Plus ou R
# para  executar o programa. A sequência de comandos é a
# seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_quase")
#
# A saída terá dois gráficos: de pontos influentes  e  
# de resíduo de Pearson. Para identificar os pontos
# que  mais  se destacam usar o comando identify(...). 
# Se por exemplo se destacam três pontos no 
# plot(fitt(fit.model),h,...), após esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito no outro gráfico. No gráfico de 
# resíduos foram colocados os limites ylim=c(a-1,b+1), em que
# a é o menor valor e b o maior valor para o resíduo.
#------------------------------------------------------------#
diag_quasi <- function(fit.model, data){
par(mfrow=c(1,2))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
sigma2 <- summary(fit.model)$dispersion
ts <- resid(fit.model,type="pearson")/sqrt(sigma2)
td <- resid(fit.model,type="deviance")/sqrt(1-h) # EU ADICIONEI
di <- (h/((1-h)*2))*(ts^2)
a <- min(td)
b <- max(td)
#
plot(di,xlab="Indice", ylab="Distancia de Cook",
     pch=16)
#identify(di, n=2)
#
plot(predict(fit.model), ts,xlab="Preditor Linear", ylab="Residuo de Pearson",
     ylim=c(a-1,b+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model), h, n=3)
}
#------------------------------------------------------------#