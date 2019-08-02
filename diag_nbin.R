#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto   fit.model
# a saída do  ajuste  da  regressão com  erros binomial
# negativa. Deixe  os dados disponíveis  através do comando
# attach(...).  Depois use o  comando source(...) no R ou 
# S-Plus para  executar o programa. A  sequência de comandos é
# a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_nbin")
#
# A saída  terá quatro gráficos: de pontos de alacanca, 
# de pontos influentes  e  dois de resíduos.  Para
# identificar os pontos que  mais  se destacam  usar o
# comando  identify(...). Se por exemplo se destacam 3 pontos
# no plot(fitted(fit.model),h,...), após esse comando coloque
#     
#        > identify(fitted(fit,model),h,n=3)
# 
# O mesmo  pode  ser feito para os demais gráficos. Nos
# gráficos de resíduos foram  colocados os limites 
# ylim=c(a-1,b+1), em que a é o menor e b o maior valor para
# o resíduo. #------------------------------------------------------------#
diag_nbin <- function(fit.model, data) {
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
par(mfrow=c(2,2))
a <- max(td)
b <- min(td)
plot(fitted(fit.model),h,xlab="Valor Ajustado", 
ylab="Medida h", pch=16)
# identify(fitted(fit.model), h, n=1)
#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
# identify(di,n=3)
#
plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
# identify(td,n=1)
#
eta = predict(fit.model)
z = eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear", 
ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model), z, df=2))
par(mfrow=c(1,1))
}
#------------------------------------------------------------#


