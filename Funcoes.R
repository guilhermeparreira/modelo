# Formata o valor-p
fixp <- function(x, dig=3){
  x <- round(x, dig)
  x <- ifelse(x == 0, paste0("<.", paste0(rep(0,dig-1), collapse=""), "1"), x)
  x
}
# Descreve vetor
descreve_vetor <- function(vetor = NULL, sep = ", ", sepFinal = " e ", emph = NA, ordenar = TRUE, tirarParaOrdenar = NA){
  if(!is.null(vetor) & length(vetor) > 0){
    if(ordenar){
      if(is.na(tirarParaOrdenar)){
        vetor <- sort(unique(vetor))
      } else if(!is.na(tirarParaOrdenar)){
        vetor <- paste0(tirarParaOrdenar, sort(as.numeric(sub(pattern = tirarParaOrdenar, replacement = "", x = unique(vetor)))))
      }
    }
    if(!is.na(emph)){
      for(i in 1:length(vetor)){
        vetor[i] <- paste0(emph, vetor[i], emph)
      }
    }
    ifelse(length(vetor) > 1, paste(paste(vetor[1:(length(vetor))-1], collapse = sep), vetor[length(vetor)], sep = sepFinal), vetor)
  } else{
    return(NA)
  }
}

# Round
round_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
# Tabelas (Média + Proporção)
tabdesc <- function(resp, exp, ord = T, faltantes = F){
  ta <- round(tapply(resp, exp, sd, na.rm = T),2)
  tb <- round(tapply(resp, exp, mean, na.rm = T),2)
  tc <-       tapply(resp, exp, function(x) length(x) - sum(is.na(x)))
  td <- round(tapply(resp, exp, median, na.rm = T),2)
  te <- round(tapply(resp, exp, min, na.rm = T),2)
  tf <- round(tapply(resp, exp, max, na.rm = T),2)
  tg <- round(tapply(resp, exp, function(x) sum(is.na(x))),0)
  t <- stack(ta)
  t$Média <- tb
  t$n <- tc
  t$Mediana <- td
  t$Minimo <- te
  t$Maximo <- tf
  t$Soma <- tg
  names(t) <- c("Desvio Padrão",gsub(".*\\$","",deparse(substitute(exp))),"Média","Número de Respondentes","Mediana","Mínimo","Máximo","Informação Faltante")
  #names(t) <- c("Desvio Padrão",gsub(".*\\$","",deparse(substitute(exp))),"Média","Mediana","Mínimo","Máximo")
  t <- t[,c(2,3,1,5,6,7,4,8)]
  if(!faltantes){
    t <- t[, -8]
  }
  if (ord){t <- t[order(t$`Média`, decreasing = T),]}
  return(htmlTable(t, caption=paste("Estatísticas descritivas para a variável ", gsub(".*\\$","",deparse(substitute(resp)))," em relação a variável ",gsub(".*\\$","",deparse(substitute(exp)))), rnames = FALSE, align=c("l",rep("c",ncol(t)-1))))
}
# Tabela descritiva (tapply) com média, length, mediana, desvio padrão para variáveis contínuas
#tabdesc(data0p$nota, data0p$formacao, "Medidas descritivas para a nota da avaliação da escala em relação à formação")
desc <- function(data, tfoot="", name="",ordmean=T){
  if(is.null(dim(data))){
    nc <- 1  
  }
  else{nc <- dim(data)[2]}
  ds <- data.frame(`Variável`     = character(),
                   `Média`        = numeric(),
                   `Desvio Padrão` = numeric(),
                   Mediana      = numeric(),
                   `Mínimo`       = numeric(),
                   `Máximo`       = numeric(),stringsAsFactors=FALSE)
  for (i in 1:nc){
    ds[i,1] <- ifelse(nc==1, name,(names(data)[i]))
    ds[i,2] <- round(mean(data[,i]   , na.rm = TRUE),2)
    ds[i,3] <- round(sd(data[,i]     , na.rm = TRUE),2)
    ds[i,4] <- round(median(data[,i] , na.rm = TRUE),2)
    ds[i,5] <- round(min(data[,i]    , na.rm = TRUE),2)
    ds[i,6] <- round(max(data[,i]    , na.rm = TRUE),2)
    
  }
  if (ordmean){ds <- ds[order(-ds$`Média`),]}
  return(htmlTable(ds, caption = "Medidas descritivas para as variáveis de interesse", rnames = F, tfoot = tfoot))
}                     # Tabela descritiva univaridada para variáveis continuas 
#desc(data)

procfreq <- function(vetor, use="ifany",tfoot="", ordena.por.freq=T, casasdecimais = 1, acumulado = F, namevar = NULL){
  if (is.null(namevar)) {namevar <- gsub(".*\\$","",deparse(substitute(vetor)))}
  if (use == "ifany"){vetor <- forcats::fct_explicit_na(vetor, na_level = "Informação Faltante")}
  
  dfm <- as.data.frame(table(vetor))
  
  if(ordena.por.freq){
    dfm <- dfm[order(-dfm$Freq),]
  }
  names(dfm)[dim(dfm)[2]] <- "Frequência"
  dfm$`Percentual (%)` <- round_sum((dfm$`Frequência`/sum(dfm$`Frequência`))*100, casasdecimais)
  dfm$vetor <- factor(dfm$vetor, levels = c(levels(dfm$vetor),"Total"))
  dfm <- rbind(dfm, c("Total",sum(dfm[,2]), 100))
  dfm$`Acumulado (%)` <- round(c(cumsum(dfm$`Percentual (%)`[-length(dfm$`Frequência`)]),NA), casasdecimais)
  names(dfm)[1] <- namevar
  if (!acumulado){
    dfm <- dfm[, -ncol(dfm)]
  }
  return(htmlTable(dfm, rnames=F, caption=paste("Frequência absoluta e relativa(%) da variável ",namevar), 
                   tfoot = tfoot, align = c("l","c","c","c"), align.header = c("l","c","c","c"), total = T))
}  # Tabela de frequência relativa e percentual para um df e única variável

desc.simp <- function(x){
  x2 <- sum(is.na(x))
  x <- x[!is.na(x)]
  data.frame(`Média` = mean(x),
                   `DesvPadrão` = sd(x),
                   `Mediana` = median(x),
                   `Mínimo` = min(x),
                   `Máximo` = max(x),
                   Soma = sum(x),
                   `Tamanho da Amostra` = length(x),
                   `Informações faltantes` = x2)
} # Resumo para variável numérica

mult.freq <- function(ds, ordcol = F,casasdecimais=1, caption = "Frequência absoluta e relativa(%) para cada variável", use = "ifany"){
  nvars <- ncol(ds)
  df <- data.frame(Niveis = factor(),
                   Resposta = factor(),
                   Nome = factor())
  if (use == "ifany"){ds <- as.data.frame(apply(ds, 2, forcats::fct_explicit_na, na_level = "Informação Faltante"))}
  i <- 1
  for (i in 1:nvars){
    a1 <- as.data.frame(addmargins(table(ds[, i])))
    a2 <- as.data.frame(addmargins(round_sum(prop.table(table(ds[, i]))*100, casasdecimais)))
    a2$Freq <- paste0(" (", a2$Freq, "%)")
    a1$Freq <- paste(a1$Freq, a2$Freq, sep= ";")
    names(a1) <- c("Niveis", "Resposta")
    a1$Nome <- names(ds)[i]
    df <- rbind(df,a1)
  }
  levels(df$Niveis) <- trimws(levels(df$Niveis))
  b <- reshape(df, timevar = "Niveis", idvar="Nome",direction = "wide")
  names(b) <- gsub('Resposta\\.',"",names(b))
  names(b)[1] <- "Variáveis"
  b <- b[,c(colnames(b)[colnames(b)!='Sum'],'Sum')]
  names(b)[ncol(b)] <- "Total Linha (100%)"
  if(ordcol){b <- b[,c(1,order(names(b)[-1])+1)]} # Ordena as colunas
  htmlTable(b,
            rnames=F,
            cgroup=c("","Categorias das Variáveis",""),
            col.rgroup = c("none", "#F7F7F7"),
            n.cgroup=c(1,ncol(b)-2,1),
            align=c("l",rep("c",ncol(b)-1)), align.header = c("l",rep("c",ncol(b)-1)),
            #tfoot = "Obs: Categorias vazias correspondem a não frequência dessa categoria",
            caption = caption)
} # Funciona para mais de uma variável. 

cross_easy <- function(data, var_resp, v_exp, caption = paste("Frequência observada e relativa (%) para a variável ",v_exp[i] , " em relação a variável ",var_resp,sep=""), useNA = T){
  lv <- length(v_exp)
  for (i in 1:lv){
    if (!useNA){
      data <- na.omit(data[, c(v_exp[i], var_resp)])
    }
    a <- as.data.frame(addmargins(table(data[,v_exp[i]],data[,var_resp], useNA = "ifany")))
    b <- as.data.frame(round_sum(addmargins(prop.table(table(data[,v_exp[i]],data[,var_resp], useNA = "ifany"),2), margin = 1), 3)*100)
    c <- merge(a,b, by=names(a)[-length(names(a))], sort=F, all.x = TRUE)
    if (useNA){
      c$Var1 <- factor(c$Var1, levels = levels(addNA(c$Var1)), labels = c(levels(c$Var1), "Informação Faltante"), exclude = NULL)
      c$Var2 <- factor(c$Var2, levels = levels(addNA(c$Var2)), labels = c(levels(c$Var2), "Informação Faltante"), exclude = NULL)
    }
    c[!c$Var1=="Sum" & c$Var2=="Sum", 4] <- round_sum(c[!c$Var1=="Sum" & c$Var2=="Sum", 3]/sum(c[!c$Var1=="Sum" & c$Var2=="Sum", 3]),3)*100
    c$Freq.y <- ifelse(c$Var2=="Sum" & c$Var1=="Sum", NA, paste("(",c$Freq.y,"%)", sep = ""))
    c$obsv <- with(c, ifelse(Var2=="Sum" & Var1=="Sum", Freq.x, paste(Freq.x, Freq.y, sep=" ; ")))
    c <- c[,-c(3,4)]
    d <- reshape(c, timevar = "Var2", idvar="Var1",direction = "wide")
    names(d) <- gsub('obsv\\.',"",names(d))
    names(d)[1] <- v_exp[i]
    names(d)[ncol(d)] <- "Total linha"
    levels(d[, v_exp[i]])[levels(d[, v_exp[i]])=="Sum"] <- c("Total Coluna (100%)")
    print(htmlTable(d,  align = c("l",rep("c",length(names(d))-1)),
                    align.header = c("l",rep("c",length(names(d))-1)),
                    caption = caption,
                    rnames = F,
                    cgroup=c("",var_resp),
                    n.cgroup = c(1, ncol(d)-1),
                    total = T
    ))
  }
}
cross_easy_2 <- function(data, var_resp, v_exp, useNA = T){
  lv <- length(v_exp)
  for (i in 1:lv){
    if (!useNA){
      data <- na.omit(data[, c(v_exp[i], var_resp)])
    }
    a <- as.data.frame(addmargins(table(data[,v_exp[i]],data[,var_resp], useNA = "ifany")))
    b <- as.data.frame(round_sum(addmargins(prop.table(table(data[,v_exp[i]],data[,var_resp], useNA = "ifany"),1), margin = 2), 3)*100)
    c <- merge(a,b, by=names(a)[-length(names(a))], sort=F, all.x = TRUE)
    if (useNA){
      c$Var1 <- factor(c$Var1, levels = levels(addNA(c$Var1)), labels = c(levels(c$Var1), "Informação Faltante"), exclude = NULL)
      c$Var2 <- factor(c$Var2, levels = levels(addNA(c$Var2)), labels = c(levels(c$Var2), "Informação Faltante"), exclude = NULL)
    }
    c[c$Var1=="Sum" & !c$Var2=="Sum", 4] <- round_sum(c[c$Var1=="Sum" & !c$Var2=="Sum", 3]/sum(c[c$Var1=="Sum" & !c$Var2=="Sum", 3]),3)*100
    c$Freq.y <- ifelse(c$Var2=="Sum" & c$Var1=="Sum", NA, paste("(",c$Freq.y,"%)", sep = ""))
    c$obsv <- with(c, ifelse(Var2=="Sum" & Var1=="Sum", Freq.x, paste(Freq.x, Freq.y, sep=" ; ")))
    c <- c[,-c(3,4)]
    d <- reshape(c, timevar = "Var2", idvar="Var1",direction = "wide")
    names(d) <- gsub('obsv\\.',"",names(d))
    names(d)[1] <- v_exp[i]
    names(d)[ncol(d)] <- "Total linha (100%)"
    levels(d[, v_exp[i]])[levels(d[, v_exp[i]])=="Sum"] <- c("Total Coluna")
    print(htmlTable(d,  align = c("l",rep("c",length(names(d))-1)),
                    align.header = c("l",rep("c",length(names(d))-1)),
                    caption = paste("Frequência observada e relativa (%) para a variável ",v_exp[i] , " em relação a variável ",var_resp,sep=""),
                    rnames = F,
                    cgroup=c("",var_resp),
                    n.cgroup = c(1, ncol(d)-1),
                    total = T,
                    escape.html = T
    ))
  }
}

respiguais <- function(d1){
  df <- data.frame(0)
  for(i in 1:47){
    for(j in 1:47){s
      df[i,j] <-sum(d1[,i]==d1[,j])/25
    }
  }
  colnames(df) <- names(d1)
  rownames(df) <- names(d1)
  df <- as.matrix(df)
  return(df)
}                 # Proporção amostral de respostas concordantes 
desc.grupo <- function(vars, base, casas){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=21)
  df <- as.data.frame(df)
  names(df) <- c("Variáveis",rep(c("mín","m","med","sd","máx"), times = 4))
  
  
  for (i in 1:nvars){
    var_resp <- vars[i]
    means <- with(base, as.numeric(tapply(base[,var_resp], list(momento, grupo), mean)))
    sds   <- with(base, as.numeric(tapply(base[,var_resp], list(momento, grupo), sd)))
    min   <- with(base, as.numeric(tapply(base[,var_resp], list(momento, grupo), min)))
    max   <- with(base, as.numeric(tapply(base[,var_resp], list(momento, grupo), max)))
    med   <- with(base, as.numeric(tapply(base[,var_resp], list(momento, grupo), median)))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    m2 <- round(means[3],casas)
    m3 <- round(means[4],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    sd2 <- round(sds[3],casas)
    sd3 <- round(sds[4],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    min2 <- round(min[3],casas)
    min3 <- round(min[4],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    max2 <- round(max[3],casas)
    max3 <- round(max[4],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    med2 <- round(med[3],casas)
    med3 <- round(med[4],casas)
    
    df[i,1] <- var_resp
    
    df[i,2] <- min0
    df[i,3] <- m0
    df[i,4] <- med0
    df[i,5] <- sd0
    df[i,6] <- max0
    
    df[i,7] <- min1
    df[i,8] <- m1
    df[i,9] <- med1
    df[i,10] <- sd1
    df[i,11] <- max1
    
    df[i,12] <- min2
    df[i,13] <- m2
    df[i,14] <- med2
    df[i,15] <- sd2
    df[i,16] <- max2
    
    df[i,17] <- min3
    df[i,18] <- m3
    df[i,19] <- med3
    df[i,20] <- sd3
    df[i,21] <- max3
  }
  a <- with(base, paste(levels(momento)[1], levels(grupo)[1]))
  b <- with(base, paste(levels(momento)[1], levels(grupo)[2]))
  c <- with(base, paste(levels(momento)[2], levels(grupo)[1]))
  d <- with(base, paste(levels(momento)[2], levels(grupo)[2]))
  return(htmlTable(df,
                   caption=paste("Medidas descritas para as variáveis em relação aos grupos e momentos"),
                   tfoot="Mín = mínimo. m = média. med = mediana. sd = desvio padrão. máx = máximo.",
                   rnames = FALSE,
                   cgroup=c("",a,c,b,d),
                   n.cgroup=c(1,5,5,5,5),
                   align=c("l",rep("c",ncol(df)-1)), align.header = c("l",rep("c",ncol(df)-1))
  ))
}
# crosstab_multiple <- function(data, resp, casasdecimais=c(0,1,2)){
#   if (missing(casasdecimais)){
#     casasdecimais <- 0
#   }
#   for (i in 1:dim(data)[2]){
#     if (names(data)[i]==resp){
#       next
#     } else{
#       message(paste("                   Variável: ",names(data)[i],"            ",sep=""))
#       print(crosstab(data, row.vars = names(data)[i], col.vars = resp, type = c("f", "c", "r"), addmargins = F, dec.places = casasdecimais, percentages = T))
#       cat("\n")
#     }
#   }
# }
#crosstab_multiple(data, "pepc1a")
avg.hour <- function(t2,...){
  minutes <- hour(t2)*60 + minute(t2)
  r <- paste(round(mean(minutes, na.rm=T)/60),"hora(s) e",formatC(round(mean(minutes, na.rm=T)%%60),width = 2, flag = "0"),"minuto(s)",sep=" ")
}
desc.2grupos <- function(vars, base, casas){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=15)
  df <- as.data.frame(df)
  names(df) <- c("Variáveis",rep(c("mín","m","med","sd","máx","n","ep"), times = 2))
  
  for (i in 1:nvars){
    var_resp <- vars[i]
    means <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), mean)))
    sds   <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), sd)))
    min   <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), min)))
    max   <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), max)))
    med   <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), median)))
    len   <- with(base, as.numeric(tapply(base[,var_resp], list(grupo), length)))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    
    n0 <- round(len[1],casas)
    n1 <- round(len[2],casas)
    
    df[i,1] <- var_resp
    
    df[i,2] <- min0
    df[i,3] <- m0
    df[i,4] <- med0
    df[i,5] <- sd0
    df[i,6] <- max0
    df[i,7] <- n0
    df[i,8] <- round(sd0/sqrt(n0),4)
    
    df[i,9] <- min1
    df[i,10] <- m1
    df[i,11] <- med1
    df[i,12] <- sd1
    df[i,13] <- max1
    df[i,14] <- n1
    df[i,15] <- round(sd1/sqrt(n1),4)
  }
  a <- with(base, levels(grupo)[1])
  b <- with(base, levels(grupo)[2])
  
  return(htmlTable(df,
                   caption=paste("Medidas descritas para a variação (depois em relação ao antes) para cada variável em relação aos grupos"),
                   tfoot="Mín = mínimo. m = média. med = mediana. sd = desvio padrão. máx = máximo. ep = erro padrão",
                   rnames = FALSE,
                   cgroup=c("",a,b),
                   n.cgroup=c(1,7,7),
                   align=c("l",rep("c",ncol(df)-1)), align.header = c("l",rep("c",ncol(df)-1))
  ))
}


# Resumo do modelo

export_log <- function(model, vars = names(coef(model)), logito = T){
  a <- summary(model)$coefficients[,c(1,2,4)]
  coef <- exp(coef(model))
  b <- as.matrix(exp(confint.default(model))[,1]) # Method chooses between "Wald" and "likelihood"
  c <- as.matrix(exp(confint.default(model))[,2]) # Method chooses between "Wald" and "likelihood"
  d <- cbind(a,b,c,coef)
  d <- round(d, 4)
  d <- d[,c(1,2,6,4,5,3)]
  d <- as.data.frame(d)
  d$IC <- paste(d$'V4', d$V5, sep=" ; ")
  d <- d[,-c(4,5)]
  names(d) <- c("Estimativa","Erro\nPadrão","Razão\nDe Chances (RC)","Valor-p","IC p/ RC (95%)")
  names(d)[3] <- ifelse(logito, names(d)[3], "Razão\nDe Médias (RM)")
  names(d)[5] <- ifelse(logito, names(d)[5], "IC p/ RM (95%)")
  d$`Valor-p` <- fixp(d$`Valor-p`)
  d <- d[,c(1,2,3,5,4)]
  row.names(d) <- vars
  logistica <- paste0("Resumo dos resultados do modelo de regressão logística para a variável " , all.vars(terms(model))[1], ": ")
  link.log <- paste0("Resumo dos resultados do modelo com função de ligação log para a variável " , all.vars(terms(model))[1], ": ")
  logistica.2 <- "Odds Ratio (Razão de Chances)"
  link.log.2 <- "Razão de médias"
  htmlTable(d, 
            align = "c",
            caption = ifelse(logito, logistica, link.log),
            tfoot = paste0("Estimativa, Erro padrão e valor-p calculados a partir do preditor linear.
                     OR e IC(95%) foram calculados a partir do preditor linear e estão na escala da ", ifelse(logito, logistica.2, link.log.2))
  )
}                  # Summary do modelo glm logito formatado
export_ordinal <- function(modelo, num, vars = names(coef(modelo))[num:length(coef(modelo))]){ # Não funciona quando tem somente 1 variável!!!
  a <- summary(modelo)$coefficients[num:length(coef(modelo)),c(1,2,4)]
  coef <- exp(coef(modelo)[num:length(coef(modelo))])
  conf <- exp(confint.default(modelo))[num:length(coef(modelo)), ]
  #conf <- t(conf)
  b <- as.matrix(conf[,1])
  c <- as.matrix(conf[,2])
  d <- cbind(a,b,c,coef)
  d <- round(d,3)
  d <- d[,c(1,2,6,4,5,3)]
  d <- as.data.frame(d)
  d$IC <- paste(d$V4, d$V5, sep=" ; ")
  d <- d[,-c(4,5)]
  names(d) <- c("Estimativa","ErroPadrão","Razão de Chances (RC)","Valor-p","IC p/ RC (95%)")
  d$`Valor-p` <- fixp(d$`Valor-p`)
  d <- d[,c(1,2,3,5,4)]
  row.names(d) <- vars
  htmlTable(d, align=c("l",rep("c",5))
            ,caption="Resumo dos resultados do modelo: "
            ,tfoot  = "Estimativa e Erro Padrão na escala do preditor linear."
  )
}                 

export_ordinal_1 <- function(modelo, num, vars = names(coef(modelo))[num:length(coef(modelo))]){ # Funciona para 1 covariável
  a <- as.data.frame(t(round(summary(modelo)$coefficients[num:length(coef(modelo)),c(1,2,4)], 4)))
  a$coef <- round(exp(coef(modelo)[num:length(coef(modelo))]), 4)
  a$conf <- paste(round(as.numeric(exp(confint.default(modelo))[num:length(coef(modelo)), ]), 4), collapse = " ; ")
  
  a <- a[,c(1,2,4,5,3)]
  names(a) <- c("Estimativa","ErroPadrão","Razão de Chances (RC)","IC(95%) p/ RC","Valor-p")
  row.names(a) <- vars
  htmlTable(a, align=c("l",rep("c",5))
            ,caption="Resumo dos resultados ao modelo: "
            ,tfoot  = "Estimativa e Erro Padrão na escala do Preditor linear. \n Se valor-p = 0, lê-se valor-p < 0.00001."
  )
}
export_linear <- function(model, vars = c("Intercepto (fixo)", names(coef(model))[2:length(names(coef(model)))])){
  a <- summary(model)$coefficients[,c(1,2,4)]
  b <- as.matrix(confint(model))[,1]
  c <- as.matrix(confint(model))[,2]
  d <- cbind(a,b,c)
  d <- round(d, 4)
  d <- d[,c(1,2,4,5,3)]
  d <- as.data.frame(d)
  d$IC <- paste(d$b, d$c, sep=" ; ")
  d <- d[,-c(3,4)]
  names(d) <- c("Estimativa","ErroPadrão","Valor-p","IC(95%)")
  d <- d[,c(1,2,4,3)]
  row.names(d) <- vars
  d$`Valor-p` <- fixp(d$`Valor-p`)
  htmlTable::htmlTable(d, align="c"
                       ,caption=paste0("Resumo dos resultados do modelo para a variável ",all.vars(terms(model))[1] ,": "))
}                  # Summary do modelo glm logito formatado
export_linear_lme <- function(model, vars = c("Intercepto (fixo)", names(coef(model))[2:length(names(coef(model)))])){
  a <- summary(model)$tTable[,c(1,2,5)]
  b <- as.matrix(intervals(model)$fixed[, 1])
  c <- as.matrix(intervals(model)$fixed[, 3])
  d <- cbind(a,b,c)
  d <- round(d, 4)
  d <- d[,c(1,2,4,5,3)]
  d <- as.data.frame(d)
  d$IC <- paste(d$V3, d$V4, sep=" ; ")
  d <- d[,-c(3,4)]
  names(d) <- c("Estimativa","ErroPadrão","Valor-p","IC(95%)")
  d <- d[,c(1,2,4,3)]
  row.names(d) <- vars
  d$`Valor-p` <- fixp(d$`Valor-p`)
  htmlTable(d, align="c"
            ,caption=paste0("Resumo dos resultados do modelo para a variável ",all.vars(terms(model))[1] ,": "))
}
export_linear_lme_log <- function(model, vars = c("Intercepto (fixo)", names(coef(model))[2:length(names(coef(model)))])){
  a <- summary(model)$tTable[,c(1,2,5)]
  b <- as.matrix(intervals(model)$fixed[, 1])
  c <- as.matrix(intervals(model)$fixed[, 3])
  d <- cbind(a,b,c)
  d <- round(d, 4)
  d <- d[,c(1,2,4,5,3)]
  d <- as.data.frame(d)
  d$RazaoMedias <- round(exp(d[, 1]), 2)
  d$IC.Inf <- exp(d[, 3])
  d$IC.Sup <- exp(d[, 4])
  d <- d[, c(1,2,5:8)]
  d$IC <- paste(round(d$IC.Inf,3), round(d$IC.Sup,3), sep=" ; ")
  d <- d[,-c(5,6)]
  names(d) <- c("Estimativa","ErroPadrão","Valor-p", "Razão de Médias (RM)","IC para RM (95%)")
  row.names(d) <- vars
  d$`Valor-p` <- fixp(d$`Valor-p`)
  htmlTable(d, align="c"
            ,caption=paste0("Resumo dos resultados do modelo para a variável ",all.vars(terms(model))[1] ,": "),
            tfoot = "Estimativa, Erro Padrão e valor-p na escala do log.\n
            Razão de Médias (RM) e IC na escala da variável resposta")
}
export_mult <- function(model, vars = names(coef(model))){
  a <- summary(model)@coef3[,c(1,2,4)]
  coef <- exp(coef(model))
  conf <- as.matrix(exp(confint.default(model, method = "Wald")))
  b <- conf[,1] # Method chooses between "Wald" and "likelihood"
  c <- conf[,2] # Method chooses between "Wald" and "likelihood"
  d <- cbind(a,b,c,coef)
  d <- fixp(d)
  d <- d[,c(1,2,6,4,5,3)]
  d <- as.data.frame(d)
  d$IC <- paste(d$b, d$c, sep=" ; ")
  d <- d[,-c(4,5)]
  names(d) <- c("Estimativa","Erro\nPadrão","Razão\nDeChances","Valor-p","IC(95%)")
  d <- d[,c(1,2,3,5,4)]
  row.names(d) <- vars
  htmlTable(d, align="c"
            ,caption=paste0("Resumo dos resultados do modelo para a variável ",all.vars(terms(model))[1] ,": ")
            ,tfoot = "Estimativa, Erro padrão e valor-p calculados a partir do preditor linear.
                     OR e IC(95%) foram calculados a partir do preditor linear e estão na escala da Odds Ratio (Razão de Chances)."
  )
}                  # Summary do modelo glm logito formatado
export_vglm <- function(m1, vars = c("Intercepto (fixo)", names(coef(m1))[3:length(names(coef(m1)))]), format = T){
  tb <- summary(m1)
  tb <- cbind(tb@coef3, confintvglm(m1, method = "profile"))
  tb <- tb[, c(1, 2, 5, 6, 4)]
  tb[, 1:4] <- round(tb[, 1:4], 5)
  tb <- as.data.frame(tb)
  tb$`Pr(>|z|)` <- fixp(tb$`Pr(>|z|)`)
  tb$`IC 95%` <- paste(tb$`2.5 %`,tb$`97.5 %`, sep = " ; ")
  tb <- tb[, c(1,2,6,5)]
  names(tb) <- c("Estimativa", "Erro Padrão", "IC 95%", "Valor-p")
  tb <- tb[-2, ]
  row.names(tb) <- vars
  if (format) {
    kable(tb, row.names = T, align = c(rep("c", 4)), escape = F, caption = paste0("Resumo do ajuste de modelo de regressão tobit para a variável", all.vars(terms(m1))[1], ":")) %>% 
      footnote(general = "IC 95% construído através da verossimilhança perfilhada")
  } else{
    tb
  }
}

# Correlação/Associação
cor_poly <- function(matriz){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {mm[i,j] <- round(polychor(table(matriz[,i],matriz[,j])),2)}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
}                    # Matriz de Correlação Policlórica
#htmlTable(cor(anacor), caption="Correlação Policlórica entre as variáveis apenas para o 1o avaliador")
cor_test <- function(matriz, method, exact=NULL, continuity=FALSE){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {
        mm[i,j] <- paste(round(cor.test(matriz[,i], matriz[,j], method = method, exact=exact, continuity = continuity)$estimate,2), " ; " , "(" ,
                         round(cor.test(matriz[,i], matriz[,j], method = method, exact=exact, continuity = continuity)$p.value,4) ,")", sep = "")}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
} 

cor_test_m <- function(matriz, method, exact=NULL, continuity=FALSE){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  tt <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      mm[i,j] <- round(cor.test(matriz[,i], matriz[,j], method = method, exact=exact, continuity = continuity)$estimate,2)
      tt[i,j] <- round(cor.test(matriz[,i], matriz[,j], method = method, exact=exact, continuity = continuity)$p.value,4)}
  }
  return(list(mm,tt))
}



# Coeficiente de Correlação - Escolha entre o Spearman e Pearson
comentario_correlacao <- "Nota: O número antes do ';' é o coeficiente de correlação utilizado. Valores próximos de +1 indicam forte correlação positiva e próximos de -1 indicam forte correlação negativa, enquanto que valores próximos de 0 indicam ausência de correlação. \n O valor entre parêntesis é o valor-p, que nesse caso, testa se o valor do coeficiente de correlação é diferente de zero. Valor-p menor que 0.05 indica que o coeficiente de correlação é diferente de zero, caso contrário, o valor do coeficiente de correlação é igual a zero. Obs.: Se o valor está 0, leia-se valor-p < 0.0001."
tableall <- function(model, casasdecimais){
  tabl <- as.data.frame(summary(model)$t)[,c(1,2,4)]
  row.names(tabl)[1:4] <- c("Intercepto","Aplicação","Nota da Avaliação","Profissionais")
  
  tabl$ICinf <- tabl$Value - 1.96*tabl$Std.Error
  tabl$ICSup <- tabl$Value + 1.96*tabl$Std.Error
  tabl <- round(tabl,casasdecimais)
  tabl$IC <- paste(tabl$ICinf, tabl$ICSup, sep=" ; ")
  names(tabl) <- c("Estimativa","ErroPadrão","Valor-p*","ICI","ICS","IC (95%)")
  tabl <- tabl[,c(1,2,6,3)]
  out <- htmlTable(tabl, align="c"
                   ,caption="Resumo dos resultados do modelo: "
                   ,tfoot = "*Se o valor-p é igual a '0', leia-se valor-p<= 0.0001")
  return(out)
} # Summary do modelo gls formatado
#tableall(m1bf, 2)
#x11();corrplot(df, method="circle")
assoc_binary <- function(vars, base){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]])
      corre <- chisq.test(tab, correct = F)$p.value
      m[i,j] <- corre
    }
  }
  m <- round(m,4)
  m[lower.tri(m, d=T)] <- NA
  return(htmlTable(m, caption = "Valor-p do Teste de Qui-Quadrado", tfoot="Se valor-p = 0, lê-se valor-p < 0.0001"))
}
#f1 <- assoc_binary(vars, base);
assoc_binary_2 <- function(vars, base){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]])
      corre <- chisq.test(tab, correct = F)$p.value
      m[i,j] <- corre
    }
  }
  m <- round(m,3)
  return(m)
}
assoc_binary_sing <- function(vars, base, var_resp, simulate.p.value = F, B = 2000){ #Teste Qui-Quadrado
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=2)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p")
  for (i in 1:nvars){
    v_exp <- vars[i]
    tab <- table(base[,var_resp],base[,vars[i]])
    p.value <- round(chisq.test(tab, correct = T, simulate.p.value = simulate.p.value, B = B)$p.value,5)
    df[i,1] <- v_exp
    df[i,2] <- p.value
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,caption=paste("Valor-p do teste Qui-Quadrado em relação a variável",var_resp, sep=" "), tfoot="Se valor-p = 0, lê-se valor-p < 0.0001", rnames=F))
}
# https://stats.stackexchange.com/questions/159639/exact-formula-yates-correction-in-r
# https://stats.stackexchange.com/questions/5206/yates-continuity-correction-in-confidence-interval-returned-by-prop-test
# https://stats.stackexchange.com/questions/140656/simulated-chi-square-distribution-doesnt-match-theoretical
# http://stackoverflow.com/questions/10894584/simulating-p-values-for-chi-squared-test-using-monte-carlo-method
# https://stats.stackexchange.com/questions/62445/rules-to-apply-monte-carlo-simulation-of-p-values-for-chi-squared-test
# https://stats.stackexchange.com/questions/13266/simple-linear-regression-output-interpretation
assoc_binary_weighted <- function(vars, base, weight){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      corre <- as.numeric(wtd.chi.sq(base[,i], base[,j], weight = weight)[3])
      m[i,j] <- corre
    }
  }
  m <- round(m,3)
  #m[lower.tri(m, d=T)] <- NA
  return(m)
  #return(htmlTable(m, caption = "Valor-p do Teste de Qui-Quadrado ponderado", tfoot="Se valor-p = 0, lê-se valor-p < 0,001"))
}
#tab <- assoc_binary_weighted(names(dutil)[-1], dutil, dutil$q02)
cramer <- function(base, vars = names(base)){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]])
      corre <- vcd::assocstats(tab)$cramer
      m[i,j] <- corre
    }
  }
  m <- round(m,3)
  m[lower.tri(m, d=T)] <- NA
  return(m)
}
cramer_not_matrix <- function(base, vars = names(base), var.filtro = "Ponto", var.resp = "Acertou"){ # Retira a uma variável, que é o filtro
  vars <- vars[!vars%in%c(var.filtro,var.resp)]  # Retira as variáveis de filtro e resposta da base
  l.vars <- length(vars)
  cols <- unique(base[,var.filtro])
  l.cols <- length(cols)
  m.cor <- matrix(data=0, nrow=l.vars, ncol=l.cols, dimnames=list(vars,cols))
  m.fis <- matrix(data=0, nrow=l.vars, ncol=l.cols, dimnames=list(vars,cols))
  for (j in 1:l.cols){ # Pontos
    df.r <- base[base[,var.filtro]==j, ]
    for (i in 1:l.vars){
      tab <- table(df.r[,vars[i]],df.r[,var.resp])
      if (sum(dim(tab)) < 4) {
        m.cor[i,j] <- NA
        m.fis[i,j] <- NA
      } else{
        corre <- vcd::assocstats(tab)$cramer
        fisher <- fisher.test(tab)$p.value
        m.cor[i,j] <- corre
        m.fis[i,j] <- fisher
      }
    }
  }
  m.cor <<- round(m.cor,2)
  m.fis <<- round(m.fis,3)
}
fisher_test <- function(vars, base, var_resp){ #Teste Qui-Quadrado para testes ordinais
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=2)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p")
  for (i in 1:nvars){
    v_exp <- vars[i]
    tab <- table(base[,var_resp],base[,vars[i]])
    p.value <- round(fisher.test(tab,
                                 simulate.p.value = F,
                                 B=1e6,
                                 hybrid = T)$p.value,5)
    df[i,1] <- v_exp
    df[i,2] <- p.value
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,caption=paste("Valor-p do teste Exato de Fisher em relação a variável",var_resp), tfoot="Se valor-p = 0, lê-se valor-p < 0.0001", rnames=F))
}
assoc_fisher <- function(vars, base){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]])
      corre <- fisher.test(tab, simulate.p.value = T, B = 2e7, workspace=2e8)$p.value
      m[i,j] <- fixp(corre)
    }
  }
  m[lower.tri(m, d=T)] <- NA
  return(kable(m, caption = "Valor-p do teste Exato de Fisher"))
}
assoc_fisher_m <- function(base, vars = names(base)){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 1
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]])
      corre <- fisher.test(tab, simulate.p.value = F)$p.value
      m[i,j] <- corre
    }
  }
  m <- round(m,3)
  m[lower.tri(m, d=T)] <- NA
  return(m)
}
corr_binary <- function(vars, base){
  nvars <- length(vars)
  m <- matrix(data=0, nrow=nvars, ncol=nvars, dimnames=list(c(vars),c(vars)))
  i <- 1
  j <- 3
  for (i in 1:nvars){
    for (j in 1:nvars){
      tab <- table(base[,vars[i]],base[,vars[j]]);tab
      corre <- assocstats(tab)$cramer;corre
      m[i,j] <- corre  
    }
  }
  m <- round(m,3)
  return(m)
}        # Medidas de associação
#f2 <- corr_binary(vars, base);
cmh <- function(vars, base, var_exp, caption, tfoot){ #Teste Qui-Quadrado para testes ordinais
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=2)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p")
  for (i in 1:nvars){
    v_exp <- vars[i]
    tab <- table(base[,var_exp],base[,vars[i]])
    p.value <- round((vcdExtra::CMHtest(tab)$table[4,3]),5)
    df[i,1] <- v_exp
    df[i,2] <- p.value
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,caption=caption, tfoot=tfoot, rnames=F))
}


# Testes bivariados


testt <- function(vars, base){
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=5)
  df <- as.data.frame(df)
  names(df) <- c("var_int","covariáveis","valor-p","MédiaGrupo0","MédiaGrupo1")
  for (i in 1:nvars){
    v_exp <- vars[i]
    p.value <- t.test(base[,v_exp]~as.factor(diagnostico), data=base)$p.value
    means <- as.numeric(tapply(base[,v_exp], base[,"diagnostico"], mean))
    m0 <- means[1]
    m1 <- means[2]
    
    df[i,1] <- "diagnostico"
    df[i,2] <- v_exp
    df[i,3] <- p.value
    df[i,4] <- m0
    df[i,5] <- m1
  }
  return(df)
}              # Teste t de student
#testt(cov_cont, base)
desc_vars_grupo <- function(vars, base, var_resp, casas, caption, tfoot){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=6)
  df <- as.data.frame(df)
  names(df) <- c("Escalas","Valor-p","m0","sd0","m1","sd1")
  for (i in 1:nvars){
    v_exp <- vars[i]
    valor.p <- round(wilcox.test(base[,v_exp]~base[,var_resp])$p.value,2)
    means <- as.numeric(tapply(base[,v_exp], base[,var_resp], mean))
    sds <- as.numeric(tapply(base[,v_exp], base[,var_resp], sd))
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- valor.p
    df[i,3] <- m0
    df[i,4] <- sd0
    df[i,5] <- m1
    df[i,6] <- sd1
  }
  df$md0 <- paste(df$m0, df$sd0, sep= " ; ")
  df$md1 <- paste(df$m1, df$sd1, sep= " ; ")
  df <- df[,-c(3:6)]
  names(df)[3:4] <- c("Média e DP no G0","Média e DP no G1")
  return(htmlTable(df, caption=caption,tfoot=tfoot, rnames = FALSE, align = c("l",rep("c",ncol(df)-1))))
}
aovv <- function(vars, base, var_resp, casas, caption, tfoot){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=8)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p","m0","sd0","m1","sd1","m2","sd2")
  for (i in 1:nvars){
    v_exp <- vars[i]
    p.value <- round(anova(aov(base[,v_exp]~base[,var_resp]))$Pr[1],5)
    means <- as.numeric(tapply(base[,v_exp], base[,var_resp], mean))
    sds <- as.numeric(tapply(base[,v_exp], base[,var_resp], sd))
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    m2 <- round(means[3],casas)
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    sd2 <- round(sds[3],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- p.value
    df[i,3] <- m0
    df[i,4] <- sd0
    df[i,5] <- m1
    df[i,6] <- sd1
    df[i,7] <- m2
    df[i,8] <- sd2
  }
  df$md0 <- paste(df$m0, df$sd0, sep= " ; ")
  df$md1 <- paste(df$m1, df$sd1, sep= " ; ")
  df$md2 <- paste(df$m2, df$sd2, sep= " ; ")
  df <- df[,-c(3:8)]
  df <- df[with(df, order(`valor-p`)),]
  names(df)[3:5] <- c("Média e DP no G0","Média e DP no G1","Média e DP no G2")
  return(htmlTable(df, caption=caption,tfoot=tfoot, rnames = FALSE))
}
envelope=function(modelo){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  
  for (i in 1:nsim){
    dados$y=a2[,i]
    aj=update(modelo,y~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  
  quantis=qnorm((1:n-0.5)/n)
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='Resíduos')
  title('Gráfico Normal de Probabilidades')
  lines(quantis,li,type='l')
  lines(quantis,m,type='l',lty=2)
  lines(quantis,ls,type='l')
  points(quantis,r1,pch=16,cex=0.75)
}                  # Envelope Simulado para glm
kruskal_wallis <- function(vars, base, var_resp){ #Teste Qui-Quadrado
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=2)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p")
  for (i in 1:nvars){
    v_exp <- vars[i]
    p.value <- round(kruskal.test(data[,var_resp]~data[,v_exp])$p.value,5)
    df[i,1] <- v_exp
    df[i,2] <- p.value
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,caption=paste("Valor-p do teste de Kruskal Wallis em relação a variável",var_resp, sep=" "), tfoot="Se valor-p = 0, lê-se valor-p < 0.0001", rnames=F))
}
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  
  envir <- globalenv()
  source(tempR, local = envir, ...)
} #It allows to call another script .Rmd from a .Rmd file
aovv_mult <- function(base, var_resp, covs, casas){ #Anova para 3 grupos
  nvars <- length(var_resp)
  ncovs <- length(covs)
  df <- matrix(data=0, nrow=nvars*ncovs, ncol=9)
  df <- as.data.frame(df)
  names(df) <- c("variável resposta","covariáveis","valor-p","m0","sd0","m1","sd1","m2","sd2")
  contador <- 0
  for (i in 1:nvars){
    v_resp <- var_resp[i]
    for (j in 1:ncovs){
      contador <- contador + 1
      cov <- covs[j]
      p.value <- round(anova(aov(base[,v_resp]~base[,cov]))$Pr[1],5)
      means <- as.numeric(tapply(base[,v_resp], base[,cov], mean))
      sds <- as.numeric(tapply(base[,v_resp], base[,cov], sd))
      m0 <- round(means[1],casas)
      m1 <- round(means[2],casas)
      m2 <- round(means[3],casas)
      sd0 <- round(sds[1],casas)
      sd1 <- round(sds[2],casas)
      sd2 <- round(sds[3],casas)
      
      df[contador,1] <- v_resp
      df[contador,2] <- cov
      df[contador,3] <- p.value
      df[contador,4] <- m0
      df[contador,5] <- sd0
      df[contador,6] <- m1
      df[contador,7] <- sd1
      df[contador,8] <- m2
      df[contador,9] <- sd2
    }
  }
  df$md0 <- ifelse(is.na(df$m0),NA,paste(df$m0, df$sd0, sep= " ; "))
  df$md1 <- ifelse(is.na(df$m1),NA,paste(df$m1, df$sd1, sep= " ; "))
  df$md2 <- ifelse(is.na(df$m2),NA,paste(df$m2, df$sd2, sep= " ; "))
  df <- df[,-c(4:9)]
  df <- df[with(df, order(`valor-p`)),]
  names(df)[4:6] <- c("Média e DP no G0","Média e DP no G1","Média e DP no G2")
  return(htmlTable(df, caption="Medidas descritivas da variável resposta segundo as covariáveis, além do valor-p do teste F através da análise de variância",tfoot="Se valor-p = 0, leia-se valor-p < 0.00001.\n G0 refere-se ao menor nível da variável. G2 refere-se ao maior nível da variável", rnames = FALSE))
}
t.u.anova.3g <- function(vars, base, var_resp, casas){ #Anova para 3 grupos
  nvars <- length(var_resp)
  df <- matrix(data=0, nrow=nvars, ncol=18)
  df <- as.data.frame(df)
  names(df) <- c("Variável","valor-p","Teste",rep(c("mín","m","med","sd","máx"), times = 3))
  for (i in 1:nvars){
    v_resp <- var_resp[i]
    model.anova <- lm(base[, v_resp]~ base[, vars])
    if (shapiro.test(residuals(model.anova))$p.value < 0.05 | car::leveneTest(model.anova)$Pr[1] < 0.05) {
      a <- "Kruskal Wallis"
    }
    else{
      a <- "ANOVA"
    }
    if(a == "Kruskal Wallis"){
      `valor-p` <- round(kruskal.test(base[,v_resp]~base[,vars])$p.value, 4)
    }
    else if(a == "ANOVA"){
      `valor-p` <- round(anova(model.anova)$Pr[1],4)
    }
    
    means <- as.numeric(tapply(base[,v_resp], base[,vars], mean))
    sds   <- as.numeric(tapply(base[,v_resp], base[,vars], sd))
    min   <- as.numeric(tapply(base[,v_resp], base[,vars], min))
    max   <- as.numeric(tapply(base[,v_resp], base[,vars], max))
    med   <- as.numeric(tapply(base[,v_resp], base[,vars], median))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    m2 <- round(means[3],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    sd2 <- round(sds[3],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    min2 <- round(min[3],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    max2 <- round(max[3],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    med2 <- round(med[3],casas)
    
    df[i,1] <- v_resp
    df[i,2] <- fixp(`valor-p`)
    df[i,3] <- a
    
    df[i,4] <- min0
    df[i,5] <- m0
    df[i,6] <- med0
    df[i,7] <- sd0
    df[i,8] <- max0
    
    df[i,9] <- min1
    df[i,10] <- m1
    df[i,11] <- med1
    df[i,12] <- sd1
    df[i,13] <- max1
    
    df[i,14] <- min2
    df[i,15] <- m2
    df[i,16] <- med2
    df[i,17] <- sd2
    df[i,18] <- max2
  }
  tam.amostra <- table(base[, vars])
  df <- df[with(df, order(`valor-p`)),]
  return(htmlTable(df,
                   caption=paste("Medidas descritivas e valor-p para as diferenças de médias/postos das variáveis", toString(var_resp), "em relação a", vars),
                   tfoot="Kruskal Wallis aplicado quando as exigências da ANOVA não foram atendidas (Normalidade dos resíduos e homogeneidade de variâncias nos grupos).
                   Covs = Covariáveis. mín = mínimo. m = média. med = mediana. sd = desvio padrão. máx = máximo.",
                   rnames = FALSE,
                   cgroup=c("",paste0(toString(levels(base[,vars])[1]), " (n = ", tam.amostra[1], ")"),
                               paste0(toString(levels(base[,vars])[2]), " (n = ", tam.amostra[2], ")"),
                               paste0(toString(levels(base[,vars])[3]), " (n = ", tam.amostra[3], ")")),
                   n.cgroup=c(3,5,5,5),
                   align=c("l",rep("c",ncol(df)-1)),
                   align.header = c("l",rep("c",ncol(df)-1))
  ))
}
t.u.anova <- function(vars, base, var_resp, casas){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=24)
  df <- as.data.frame(df)
  names(df) <- c("Covs","valor-p","Níveis","Teste",rep(c("mín","m","med","sd","máx"), times = 4))
  
  for (i in 1:nvars){
    v_exp <- vars[i]
    if (length(levels(base[,v_exp]))==2){
      for (lev in 1: length(levels(base[,v_exp]))) { #Decidir entre test t ou Mann Whitney
        if (shapiro.test(base[base[,v_exp]==levels(base[,v_exp])[lev],var_resp])$p.value > 0.05){
          a <- "t de Student"
        }
        else{
          a <- "Mann Whitney"
          break
        }
      }
    } else{
      model.anova <- lm(base[,var_resp]~base[,v_exp])
      if (shapiro.test(residuals(model.anova))$p.value < 0.05 | car::leveneTest(model.anova)$Pr[1] < 0.05) {
        a <- "Kruskal Wallis"
      }
      else{
        a <- "ANOVA"
      }
    }
    if(a =="Mann Whitney"){
      `valor-p` <- round(wilcox.test(base[,var_resp]~base[,v_exp])$p.value,4)
    }   else if (a == "t de Student"){
      `valor-p` <- round(t.test(base[,var_resp]~base[,v_exp], alternative=c("two.sided"), paired=FALSE, var.equal = F)$p.value,4)
    }
    else if(a == "Kruskal Wallis"){
      `valor-p` <- round(kruskal.test(base[,var_resp]~base[,v_exp])$p.value, 4)
    }
    else if(a == "ANOVA"){
      `valor-p` <- round(anova(model.anova)$Pr[1],4)
    }
    
    
    means <- as.numeric(tapply(base[,var_resp], base[,v_exp], mean))
    sds   <- as.numeric(tapply(base[,var_resp], base[,v_exp], sd))
    min   <- as.numeric(tapply(base[,var_resp], base[,v_exp], min))
    max   <- as.numeric(tapply(base[,var_resp], base[,v_exp], max))
    med   <- as.numeric(tapply(base[,var_resp], base[,v_exp], median))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    m2 <- round(means[3],casas)
    m3 <- round(means[4],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    sd2 <- round(sds[3],casas)
    sd3 <- round(sds[4],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    min2 <- round(min[3],casas)
    min3 <- round(min[4],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    max2 <- round(max[3],casas)
    max3 <- round(max[4],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    med2 <- round(med[3],casas)
    med3 <- round(med[4],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- `valor-p`
    df[i,3] <- toString(levels(base[,v_exp]))
    df[i,4] <- a
    
    df[i,5] <- min0
    df[i,6] <- m0
    df[i,7] <- med0
    df[i,8] <- sd0
    df[i,9] <- max0
    
    df[i,10] <- min1
    df[i,11] <- m1
    df[i,12] <- med1
    df[i,13] <- sd1
    df[i,14] <- max1
    
    df[i,15] <- min2
    df[i,16] <- m2
    df[i,17] <- med2
    df[i,18] <- sd2
    df[i,19] <- max2
    
    df[i,20] <- min3
    df[i,21] <- m3
    df[i,22] <- med3
    df[i,23] <- sd3
    df[i,24] <- max3
  }
  # df$md0 <- ifelse(is.na(df$m0), NA, paste(df$m0, df$sd0, sep= " ; "))
  # df$md1 <- ifelse(is.na(df$m1), NA, paste(df$m1, df$sd1, sep= " ; "))
  # df$md2 <- ifelse(is.na(df$m2), NA, paste(df$m2, df$sd2, sep= " ; "))
  # df$md3 <- ifelse(is.na(df$m3), NA, paste(df$m3, df$sd3, sep= " ; "))
  # df <- df[,-c(3:10)]
  
  df <- df[with(df, order(`valor-p`)),]
  return(htmlTable(df,
                   caption=paste("Medidas descritivas da variável", var_resp, "segundo as covariáveis, além do valor-p do teste para diferença de médias/postos"),
                   tfoot="Teste de Mann-Whitney aplicado quando as exigências do teste t de student não foram atendidas.\n Kruskal Wallis aplicado quando as exigências da ANOVA não foram atendidas. \n O primeiro nível da variável corresponde ao 1ºG, o segundo ao 2ºG, o terceiro ao 3ºG e o quarto ao 4ºG. \n Categorias vazias na tabela, pois nem todas covariáveis possuem 4 categorias. \n Covs = Covariáveis. mín = mínimo. m = média. med = mediana. sd = desvio padrão. máx = máximo.",
                   rnames = FALSE,
                   cgroup=c("","1º Grupo","2º Grupo","3º Grupo","4º Grupo"),
                   n.cgroup=c(4,5,5,5,5),
                   align=c("l",rep("c",ncol(df)-1)), align.header = c("l",rep("c",ncol(df)-1))
  ))
}
lmcaseiro <- function(vars, base, var_resp, casas, tfoot="", met = "pearson"){ #Anova para 3 grupos
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=3)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","Correlação","valor-p")
  for (i in 1:nvars){
    v_exp <- vars[i]
    df[i,1] <- v_exp
    df[i,2] <- round(as.numeric(cor.test(base[,var_resp],base[,v_exp], method = met)$estimate),2)
    df[i,3] <- fixp(round(as.numeric(cor.test(base[,var_resp],base[,v_exp], method = met)$p.value),5))
  }
  df <- df[with(df, order(abs(`Correlação`), decreasing = T)),]
  return(kable(df, caption=paste("Valor da Correlação de ", met,"entre", var_resp, "e as covariáveis com seu respectivo teste"), row.names = FALSE))
}
mann_whitney_t_test_perc <- function(vars, base, var_resp, casas=2){
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=14)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p","Teste",rep(c("média","sd","med","min","max"), times = 2), paste(levels(base[, var_resp]), collapse = "/"))
  for (i in 1:nvars){
    v_exp <- vars[i]
    for (lev in 1: length(levels(base[,var_resp]))) { #Decidir entre test t ou Mann Whitney
      if (shapiro.test(base[base[,var_resp]==levels(base[,var_resp])[lev],v_exp])$p.value > 0.05){
        a <- "Teste t de Student"
        `valor-p` <- round(t.test(base[,v_exp]~base[,var_resp], alternative=c("two.sided"), paired=FALSE, var.equal = F)$p.value,5)
      }
      else{
        a <- "Mann Whitney"
        `valor-p` <- round(wilcox.test(base[,v_exp]~base[,var_resp])$p.value,5)
        break
      }
    }
    means <- as.numeric(tapply(base[,v_exp], base[,var_resp], mean, na.rm=T))
    sds   <- as.numeric(tapply(base[,v_exp], base[,var_resp], sd, na.rm=T))
    min   <- as.numeric(tapply(base[,v_exp], base[,var_resp], min, na.rm=T))
    max   <- as.numeric(tapply(base[,v_exp], base[,var_resp], max, na.rm=T))
    med   <- as.numeric(tapply(base[,v_exp], base[,var_resp], median, na.rm=T))
    
    m0 <- means[1]
    m1 <- means[2]
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- fixp(`valor-p`)
    df[i,3] <- a
    df[i,4] <- round(m0, casas)
    df[i,5] <- sd0
    df[i,6] <- med0
    df[i,7] <- min0
    df[i,8] <- max0
    df[i,9] <- round(m1, casas)
    df[i,10] <- sd1
    df[i,11] <- med1
    df[i,12] <- min1
    df[i,13] <- max1
    df[i,14] <- round((m0/m1), 2)
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,
                   caption=paste("Valor-p do teste para a diferença em média (t de Student) ou postos (Mann Whitney) para as diferentes variáveis em relação a covariável",var_resp, sep=" "),
                   tfoot=c("min = mínimo. med = mediana. sd = desvio padrão. max = máximo") ,
                   rnames=F,
                   cgroup=c("",toString(levels(base[,var_resp])[1]),toString(levels(base[,var_resp])[2]), ""),
                   n.cgroup=c(3,5,5, 1),
                   align=c("l",rep("c",ncol(df)-1)),
                   align.header = c("l",rep("c",ncol(df)-1))
  ))
}
mann_whitney_t_test <- function(vars, base, var_resp, casas=2){
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=13)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p","Teste",rep(c("média","sd","med","min","max"), times = 2))
  for (i in 1:nvars){
    v_exp <- vars[i]
    for (lev in 1: length(levels(base[,var_resp]))) { #Decidir entre test t ou Mann Whitney
      if (shapiro.test(base[base[,var_resp]==levels(base[,var_resp])[lev],v_exp])$p.value > 0.05){
        a <- "Teste t de Student"
        `valor-p` <- round(t.test(base[,v_exp]~base[,var_resp], alternative=c("two.sided"), paired=FALSE, var.equal = F)$p.value,5)
      }
      else{
        a <- "Mann Whitney"
        `valor-p` <- round(wilcox.test(base[,v_exp]~base[,var_resp])$p.value,5)
        break
      }
    }
    means <- as.numeric(tapply(base[,v_exp], base[,var_resp], mean, na.rm=T))
    sds   <- as.numeric(tapply(base[,v_exp], base[,var_resp], sd, na.rm=T))
    min   <- as.numeric(tapply(base[,v_exp], base[,var_resp], min, na.rm=T))
    max   <- as.numeric(tapply(base[,v_exp], base[,var_resp], max, na.rm=T))
    med   <- as.numeric(tapply(base[,v_exp], base[,var_resp], median, na.rm=T))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- `valor-p`
    df[i,3] <- a
    df[i,4] <- m0
    df[i,5] <- sd0
    df[i,6] <- med0
    df[i,7] <- min0
    df[i,8] <- max0
    df[i,9] <- m1
    df[i,10] <- sd1
    df[i,11] <- med1
    df[i,12] <- min1
    df[i,13] <- max1
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,
                   caption=paste("Valor-p do teste para diferença de postos em relação a variável",var_resp, sep=" "),
                   tfoot=c("Se valor-p = 0, lê-se valor-p < 0.0001.\n min = mínimo. med = mediana. sd = desvio padrão. max = máximo") ,
                   rnames=F,
                   cgroup=c("",toString(levels(base[,var_resp])[1]),toString(levels(base[,var_resp])[2])),
                   n.cgroup=c(3,5,5),
                   align=c("l",rep("c",ncol(df)-1)),
                   align.header = c("l",rep("c",ncol(df)-1))
  ))
}
mann_whitney_t_test_2 <- function(vars, base, var_resp, casas=2){
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=14)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis", "Categorias","valor-p","Teste",rep(c("média","sd","med","min","max"), times = 2))
  for (i in 1:nvars){
    v_disc <- vars[i]
    for (lev in 1: length(levels(base[,v_disc]))) { #Decidir entre test t ou Mann Whitney
      if (shapiro.test(base[base[,v_disc]==levels(base[,v_disc])[lev],var_resp])$p.value > 0.05){
        a <- "Teste t de Student"
        `valor-p` <- round(t.test(base[,var_resp]~base[,v_disc], alternative=c("two.sided"), paired=FALSE, var.equal = F)$p.value,5)
      }
      else{
        a <- "Mann Whitney"
        `valor-p` <- round(wilcox.test(base[,var_resp]~base[,v_disc])$p.value,5)
        break
      }
    }
    means <- as.numeric(tapply(base[,var_resp], base[,v_disc], mean, na.rm=T))
    sds   <- as.numeric(tapply(base[,var_resp], base[,v_disc], sd, na.rm=T))
    min   <- as.numeric(tapply(base[,var_resp], base[,v_disc], min, na.rm=T))
    max   <- as.numeric(tapply(base[,var_resp], base[,v_disc], max, na.rm=T))
    med   <- as.numeric(tapply(base[,var_resp], base[,v_disc], median, na.rm=T))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    
    df[i,1] <- v_disc
    df[i,2] <- paste(toString(levels(base[,v_disc])[1]),toString(levels(base[,v_disc])[2]), sep = ";")
    df[i,3] <- `valor-p`
    df[i,4] <- a
    df[i,5] <- m0
    df[i,6] <- sd0
    df[i,7] <- med0
    df[i,8] <- min0
    df[i,9] <- max0
    df[i,10] <- m1
    df[i,11] <- sd1
    df[i,12] <- med1
    df[i,13] <- min1
    df[i,14] <- max1
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,
                   caption=paste("Valor-p do teste para diferença de postos em relação a variável",var_resp, sep=" "),
                   tfoot=c("Se valor-p = 0, lê-se valor-p < 0.0001.\n min = mínimo. med = mediana. sd = desvio padrão. max = máximo") ,
                   rnames=F,
                   align=c("l",rep("c",ncol(df)-1)),
                   align.header = c("l",rep("c",ncol(df)-1))
  ))
}
mann_whitney_t_test_ef <- function(vars, base, grupo){ #Teste Qui-Quadrado
  g1 <- levels(grupo)[1]
  g2 <- levels(grupo)[2]
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=8)
  df <- as.data.frame(df)
  names(df) <- c("Variável","valor-p","Teste",paste("Normalidade-",g1), paste("Normalidade-",g2), paste("IC ",g1),paste("IC ",g2),"E.S.
                 ")
  for (i in 1:nvars){
    v_resp <- vars[i]
    df[i,1] <- v_resp
    df[i,4] <- round(shapiro.test(base[grupo==g1,v_resp])$p.value,3)
    df[i,5] <- round(shapiro.test(base[grupo==g2,v_resp])$p.value,3)
    
    if(df[i,4]>0.05 & df[i,5]>0.05){
      df[i,2] <- round(t.test(base[,v_resp]~grupo, alternative=c("two.sided"), paired=FALSE, var.equal = F)$p.value,3)
      df[i,3] <- "Teste t de Student"
      df[i,6] <- paste(round(mean(base[grupo==g1,v_resp]) - (qnorm(0.975)*sd(base[grupo==g1,v_resp])/sqrt(length(base[grupo==g1,v_resp]))),2),";",
                       round(mean(base[grupo==g1,v_resp]) + (qnorm(0.975)*sd(base[grupo==g1,v_resp])/sqrt(length(base[grupo==g1,v_resp]))),2))
      df[i,7] <- paste(round(mean(base[grupo==g2,v_resp]) - (qnorm(0.975)*sd(base[grupo==g2,v_resp])/sqrt(length(base[grupo==g2,v_resp]))),2),";",
                       round(mean(base[grupo==g2,v_resp]) + (qnorm(0.975)*sd(base[grupo==g2,v_resp])/sqrt(length(base[grupo==g2,v_resp]))),2))
      df[i,8] <- round((mean(base[grupo==g2,v_resp])-mean(base[grupo==g1,v_resp]))/sd(base[,v_resp]),2)
      
    } else{
      df[i,2] <- round(wilcox.test(base[,v_resp]~grupo)$p.value,3)
      df[i,3] <- "Mann Whitney"
      set.seed(1021021)
      daux <- base[grupo==g1,v_resp, drop = T]
      Mboot = boot(daux, function(x,i) median(x[i]), R=10000)
      df[i,6] <- paste(round(boot.ci(Mboot, conf = 0.95, type = c("perc"))$percent[4],4), ";", round(boot.ci(Mboot, conf = 0.95, type = c("perc"))$percent[5],4))
      
      set.seed(213129)
      daux <- base[grupo==g2,v_resp, drop = T]
      Mboot = boot(daux, function(x,i) median(x[i]), R=10000)
      df[i,7] <- paste(round(boot.ci(Mboot, conf = 0.95, type = c("perc"))$percent[4],4), ";", round(boot.ci(Mboot, conf = 0.95, type = c("perc"))$percent[5],4))
      df[i,8] <- NA
    }
  }
  df <- df[with(df, order(`valor-p`)), ]
  return(htmlTable(df,caption=paste("Valor-p do teste para diferença de postos em relação aos grupos",sep=" "), tfoot="Se valor-p = 0, lê-se valor-p < 0.0001.\n Effect size calculado diferentemente para os paramétricos (via Cohen) e não paramétricos (Cliff.Delta)", rnames=F))
} # Com effect size
mann_whitney <- function(vars, base, var_resp, casas=2, ord.valor.p = T){ 
  nvars <- length(vars)
  df <- matrix(data=0, nrow=nvars, ncol=13)
  df <- as.data.frame(df)
  names(df) <- c("covariáveis","valor-p","Teste",rep(c("média","sd","med","min","max"), times = 2))
  i <- 1
  for (i in 1:nvars){
    v_exp <- vars[i]
    a <- "Mann Whitney"
    `valor-p` <- round(wilcox.test(base[,v_exp]~base[,var_resp])$p.value,5)
    
    means <- as.numeric(tapply(base[,v_exp], base[,var_resp], mean, na.rm=T))
    sds   <- as.numeric(tapply(base[,v_exp], base[,var_resp], sd, na.rm=T))
    min   <- as.numeric(tapply(base[,v_exp], base[,var_resp], min, na.rm=T))
    max   <- as.numeric(tapply(base[,v_exp], base[,var_resp], max, na.rm=T))
    med   <- as.numeric(tapply(base[,v_exp], base[,var_resp], median, na.rm=T))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    
    df[i,1] <- v_exp
    df[i,2] <- fixp(`valor-p`)
    df[i,3] <- a
    df[i,4] <- m0
    df[i,5] <- sd0
    df[i,6] <- med0
    df[i,7] <- min0
    df[i,8] <- max0
    df[i,9] <- m1
    df[i,10] <- sd1
    df[i,11] <- med1
    df[i,12] <- min1
    df[i,13] <- max1
  }
  if (ord.valor.p) {
    df <- df[with(df, order(`valor-p`)), ]
  }
  return(htmlTable(df,
                   caption=paste("Valor-p do teste para diferença de postos em relação a variável",var_resp, sep=" "),
                   tfoot= "min = mínimo. med = mediana. sd = desvio padrão. max = máximo. \n Teste de Wilcoxon aplicado quando as exigências do teste t de student não foram atendidas.", 
                   rnames=F,
                   cgroup=c("",toString(levels(base[,var_resp])[1]),toString(levels(base[,var_resp])[2])),
                   n.cgroup=c(3,5,5),
                   align=c("l",rep("c",ncol(df)-1)),
                   align.header = c("l",rep("c",ncol(df)-1))
  ))
}
wilcoxs_t_paired <- function(v_exp, data, var_resp, casas){ #Anova para 3 grupos
  nvars <- length(var_resp)
  df <- matrix(data=0, nrow=nvars, ncol=23)
  df <- as.data.frame(df)
  names(df) <- c("Variáveis","valor-p","Teste",rep(c("mín","m","med","sd","máx"), times = 4))
  llv <- length(levels(v_exp))
  for (i in 1:nvars){
    v_resp <- var_resp[i]
    for (lev in 1:llv) { #Decidir entre test t ou Mann Whitney
      if (shapiro.test(data[v_exp==levels(v_exp)[lev],v_resp])$p.value > 0.05){
        a <- "t de Student"
      }
      else{
        a <- "Wilcoxon"
        break
      }
    }
    if(a =="Wilcoxon"){
      `valor-p` <- round(wilcox.test(data[,v_resp] ~ v_exp, paired = TRUE)$p.value,4)
    }   else if (a == "t de Student"){
      `valor-p` <- round(t.test(data[,v_resp]~v_exp, alternative=c("two.sided"), paired=T, var.equal = F)$p.value,4)
    }
    
    means <- as.numeric(tapply(data[,v_resp], v_exp, mean))
    sds   <- as.numeric(tapply(data[,v_resp], v_exp, sd))
    min   <- as.numeric(tapply(data[,v_resp], v_exp, min))
    max   <- as.numeric(tapply(data[,v_resp], v_exp, max))
    med   <- as.numeric(tapply(data[,v_resp], v_exp, median))
    
    m0 <- round(means[1],casas)
    m1 <- round(means[2],casas)
    m2 <- round(means[3],casas)
    m3 <- round(means[4],casas)
    
    sd0 <- round(sds[1],casas)
    sd1 <- round(sds[2],casas)
    sd2 <- round(sds[3],casas)
    sd3 <- round(sds[4],casas)
    
    min0 <- round(min[1],casas)
    min1 <- round(min[2],casas)
    min2 <- round(min[3],casas)
    min3 <- round(min[4],casas)
    
    max0 <- round(max[1],casas)
    max1 <- round(max[2],casas)
    max2 <- round(max[3],casas)
    max3 <- round(max[4],casas)
    
    med0 <- round(med[1],casas)
    med1 <- round(med[2],casas)
    med2 <- round(med[3],casas)
    med3 <- round(med[4],casas)
    
    df[i,1] <- v_resp
    df[i,2] <- fixp(`valor-p`, 5)
    df[i,3] <- a
    
    df[i,4] <- min0
    df[i,5] <- m0
    df[i,6] <- med0
    df[i,7] <- sd0
    df[i,8] <- max0
    
    df[i,9] <- min1
    df[i,10] <- m1
    df[i,11] <- med1
    df[i,12] <- sd1
    df[i,13] <- max1
    
    df[i,14] <- min2
    df[i,15] <- m2
    df[i,16] <- med2
    df[i,17] <- sd2
    df[i,18] <- max2
    
    df[i,19] <- min3
    df[i,20] <- m3
    df[i,21] <- med3
    df[i,22] <- sd3
    df[i,23] <- max3
    
  }
  df <- df[with(df, order(`valor-p`)),]
  df <- df[,1:13]
  names(df) <- c("Variáveis","valor-p","Teste",rep(c("mín","m","med","sd","máx"), times = 2))
  return(htmlTable(df,
                   caption=paste("Valor-p para o teste de diferenças de médias (t de Student) ou postos (Wilcoxon) para as variáveis de interesse seguido de medidas descritivas em relação ao exercício"),
                   tfoot="Vars = Variáveis. mín = mínimo. m = média. med = mediana. sd = desvio padrão. máx = máximo. \n Teste de Wilcoxon aplicado quando as exigências do teste t de student pareado não foram atendidas.",
                   rnames = FALSE,
                   cgroup=c("",toString(levels(v_exp)[1]),toString(levels(v_exp)[2])),
                   n.cgroup=c(3,5,5),
                   align=c("l",rep("c",ncol(df)-1)), align.header = c("l",rep("c",ncol(df)-1))
  ))
}


# Normalidade Multivariada

# Teste Multivariado
#https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
#bvnorm_mardiaTest(anacama[,c(2,3)])

# hzTest(anacama[,2:6], qqplot = TRUE)@p.value
# hzTest(matrix[,c(i,j)], qqplot = TRUE)@p.value
# mardiaTest(anacama[,2:6], qqplot = TRUE)

# ?roystonTest
# 
# roystonTest(anacama[,2:6], qqplot = TRUE)

# Gráficos Univariados
# uniPlot(anacama[,2:6], type = "qqplot")
# uniPlot(anacama[,2:6], type = "histogram")
# Testes Univariados
# uniNorm(anacama[,2:6], type = "SW", desc=T)
# Gráficos Bivariados
# result <- hzTest(anacama[,5:6], qqplot = TRUE)
# 
# par(mfrow=c(1,1))
# 
# mvnPlot(result, type = "persp", default = TRUE)
# mvnPlot(result, type = "contour", default = TRUE)
# 
# hzTest(setosa, qqplot = TRUE)
# mardiaTest(setosa, qqplot = TRUE)
# roystonTest(setosa[,1:3], qqplot = TRUE)
# 
# uniPlot(setosa, type = "qqplot")
# uniPlot(setosa, type = "histogram")
# 
# hzTest(anacama[,c(2,6)])
bvnorm_hzTest <- function(matriz){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {
        mm[i,j] <- round(hzTest(matriz[,c(i,j)], qqplot = F)@p.value,2)}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
}                    # Normalidade bivariada hzTest
#htmlTable(bvnorm_hzTest(anacama[,2:6]), caption="Valor-p associado ao teste de normalidade bivariada Henze-Zirkler", tfoot = comentario_normalbivariada)
comentario_normalbivariada <- "Nota: Valor-p > 0.05 assume-se que a distribuição conjunta das variáveis é Normal bivariada, para valores menores que 0.05, não assume distribuição normal conjunta para as variáveis.\n Se valor-p = 0, lê-se valor-p < 0.0001."
bvnorm_mardiaTest_skew <- function(matriz){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {
        mm[i,j] <- round(mardiaTest(matriz[,c(i,j)], qqplot = F)@p.value.skew,2)}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
}           # Normalidade bivariada MardiaTest skew
bvnorm_mardiaTest_kurt <- function(matriz){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {
        mm[i,j] <- round(mardiaTest(matriz[,c(i,j)], qqplot = F)@p.value.kurt,2)}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
}           # Normalidade bivariada MardiaTest kurtosis
roystonTest <- function(matriz){
  dimm <- dim(matriz)[2]
  mm <- matrix(0, nrow=dimm, ncol=dimm, dimnames=list(names(matriz),
                                                      col.names = names(matriz)))
  for (i in 1:dimm){
    for (j in 1:dimm){
      if(i==j){
        mm[i,j] <- NA
      }
      else {
        mm[i,j] <- round(roystonTest(matriz[,c(i,j)], qqplot = FALSE)@p.value.kurt,2)}
    }
  }
  mm[lower.tri(mm)] <- NA
  return(mm)
}                      # Normalidade bivariada royston Test




# > Explicação do gráfico:
# 
# A figura sequente apresenta o gráfico de dispersão para cada par de variáveis (na respectiva linha e coluna), o histograma de cada variável e o valor de correlação de Pearson. Segue a descrição do conteúdo do gráfico:
# 
# + **No triângulo superior**, é apresentado o **coeficiente de correlação de Pearson ($\rho$)** (cujos valores próximos de -1 e +1 indicam forte correlação linear e próximos de 0 significam ausência de correlação linear). Ainda, os **asteriscos** simbolizam se o valor de correlação é diferente de 0, caso apresente apenas 1 asterisco ao nível de 5% foi significativo, com 2 asteriscos, ao nível de 1%, e por último, com 3 asteriscos, ao nível de 0.1%. A significância desse teste indica que o coeficiente de correlação é diferente de zero, isso é, indica se o valor apontado da correlação pode ser inferido para a população, ou seja, se os resultados não foram encontrados ao acaso.
# 
# + **No triângulo inferior**, são apresentados os diagramas de dispersão das variáveis nas respectivas linhas e colunas, cuja linha vermelha representa uma tendência entre as variáveis.
# 
# + A **diagonal** apresenta o histograma da variável em questão, seguido de uma curva de suavização, representada pela estimação de uma função densidade de probabilidade com um estimador não paramétrico.



#https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
#sum(round_preserve_sum(c(0.333, 0.333, 0.334), digits = 2))



# Gráfico de Barras

barras.editado <- function(data,lim,espaco){
  tabela <- as.data.frame(multi.table(data, true.codes=list("sim","1"), freq=T))
  tabela <- tabela[tabela$n>1,]
  tabela <- tabela[order(tabela$`%multi`),]
  depara <- cbind(row.names(tabela),LETTERS[1:length(row.names(tabela))])
  row.names(tabela) <- LETTERS[1:length(row.names(tabela))]
  
  tent <- barplot(tabela[,1],
                  horiz=T,
                  xlim=lim,
                  xlab=c("Número de entrevistados"),
                  las=1,
                  names.arg = rownames(tabela))
  text(y = tent,
       x = tabela[,1]+espaco,
       labels = (tabela[,1]),
       col="black",
       cex=1)
  
  text(y = tent-0.1,
       x = tabela[,1]+espaco*(6/2),
       labels = paste("(",(tabela[,2]),"%)",sep=""),
       cex=1)
  return(depara)
} #Enter raw data

mult.choice <- function(vetor, loc=2, sp.char = ", ", not.na = T){
  if(not.na){vetor <- vetor[!is.na(vetor)]}
  #print(procfreq(vetor))
  print(paste("O número total de respondentes para essa questão foi igual a",length(vetor),"respondentes"))
  dd <- multi.split(vetor, split.char=sp.char)   # Divide a variável pela virgula
  names(dd) <- substr(names(dd),unlist(str_locate_all(names(dd)[1],"\\."))[loc]+1,nchar(names(dd))) # Pega o nome da variável
  ds <- as.data.frame(multi.table(dd))
  ds$vars <- rownames(ds)
  print(ggplot(ds, aes(x = reorder(vars,+`%multi`), y=`%multi`)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=paste(n," (",`%multi`,"%)", sep="")), col="black", size=3, hjust = -0.05) +
          labs(y="Proporção(%)",
               x="Categorias de cada variável") +
          scale_y_continuous(limits = c(0,65)) +
          coord_flip()) 
}

mult.choice.tabela <- function(vetor, loc=2, sp.char = ", ", use = T){
  if(use){
    vetor <- factor(vetor, levels = levels(addNA(vetor)), labels = c(levels(vetor), "Informação Faltante"), exclude = NULL)
  }
  vetor <- vetor[!is.na(vetor)]
  dd <- multi.split(vetor, split.char=sp.char)   # Divide a variável pela virgula
  names(dd) <- substr(names(dd),unlist(str_locate_all(names(dd)[1],"\\."))[loc]+1,nchar(names(dd))) # Pega o nome da variável
  ds <- as.data.frame(multi.table(dd))
  ds$vars <- rownames(ds)
  ds <- ds[order(ds$n, decreasing = T),]
  ds <- ds[,c(3,1,2)]
  ds$`%` <- round((ds$n/sum(ds$n))*100,1)
  dh <- data.frame(`vars`="Total",`n`=sum(ds$n), `%multi` = sum(ds$`%multi`), `%` = 100)
  names(dh) <- names(ds)
  ds <- rbind(ds, dh)
  names(ds)[3] <- paste0(names(ds)[3], kableExtra::footnote_marker_alphabet(1))
  names(ds)[4] <- paste0(names(ds)[4], kableExtra::footnote_marker_symbol(1))
  kable(ds, "html", row.names = F, escape = F, caption = "Frequência observada e relativa (%) para pergunta de múltipla escolha") %>% 
    kableExtra::footnote(alphabet = paste0("% em relação ao total de respondentes (",length(vetor),")"),
                         symbol = paste0("% em relação ao total de respostas múltiplas (",dh$n,")")) %>% 
    kableExtra::row_spec(nrow(ds), bold = T)
}

mult.choice.tabela.by.factor <- function(vetor, loc = 2, sp.char = ", ", use = F, by){
  if(use){
    vetor <- factor(vetor, levels = levels(addNA(vetor)), labels = c(levels(vetor), "Informação Faltante"), exclude = NULL)
  }
  
  vetor <- vetor[!is.na(vetor)]
  #vetor <- str_replace_all(vetor, " ","")
  dd <- multi.split(vetor, split.char=sp.char)   # Divide a variável pela virgula
  names(dd) <- substr(names(dd),unlist(str_locate_all(names(dd)[1],"\\."))[loc]+1,nchar(names(dd))) # Pega o nome da variável
  dd$by <- by # Variável estratificadora
  teste <- by(dd[, -ncol(dd)], dd[, ncol(dd)], function(x) multi.table(x))
  teste.b <- plyr::ldply(teste, data.frame)
  teste.b$elementos <- rep(names(dd)[-length(names(dd))], length(unique(by)))
  wide <- reshape(teste.b, idvar = "elementos", timevar = ".id" , direction = "wide")
  names(wide)[seq(3,length(unique(by))*2+1,2)] <- c("%")
  names(wide)[seq(2,length(unique(by))*2,2)] <- paste("P",1:length(unique(by)), sep="")
  wide$total <- rowSums(wide[,seq(2,length(unique(by))*2,2)])
  wide <- wide[order(wide$total, decreasing = T), ]
  wide$per <- round((wide$total/440)*100,2)
  names(wide)[ncol(wide)] <- c("%")
  wide["Total", ] <- c("",colSums(wide[, -1]))
  wide[nrow(wide),1] <- c("Total")
  namevar <- gsub(".*\\$","",deparse(substitute(vetor)))
  kable(wide, format = "html", caption = paste("Frequência Observada e relativa das opções por ponto"),
        row.names = F) %>% 
    row_spec(row = nrow(wide), bold = T, color = "black") %>% 
    column_spec(column = ncol(wide), bold = T, color = "black")
}


# Seleção de Variáveis 

# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 

model.select <- function(model,keep,sig=0.05,verbose=F, teste = "F"){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test=teste)
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}              
    } # end while(T) drop var
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}

model.select.ge <- function(model,keep,sig=0.05,verbose=F, teste = "F"){
  counter=1
  # check input
  #if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test=teste)
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    if (length(pval) > 1){
      while(T){
        dropvar <- names(pval)[i]
        check.terms <- terms[-match(dropvar,terms)]
        x <- has.interaction(dropvar,check.terms)
        if(x){i=i+1;next} else {break}              
      } # end while(T) drop var
    }
    
    if (length(pval)==1 & pval[i] > 0.05) {stop("Nenhuma variável permaneceu no modelo")}
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}



# Medidas de Qualidade Preditiva

hold1OutMeans <- function(y,wts=c()){ #returns a vector g where g[i] = mean(y[-i])
  # get per-datum hold-1 out grand means
  if(is.null(wts)) {
    wts <- rep(1.0,length(y))
  }
  sumY <- sum(y*wts)
  sumW <- sum(wts)
  meanP <- (sumY - y*wts)/(sumW - wts)
  meanP[is.na(meanP)] <- 0.5
  meanP
}
hold1OutLMPreds <- function(formula,data,wts=c()){ #is the prediction on the ith row of dframe
  formula <- as.formula(formula)
  nRows <- dim(data)[[1]]
  if(is.null(wts)) {
    wts <- rep(1.0,nRows)
  }
  x <- model.matrix(formula,data=data)
  nVars <- dim(x)[[2]]
  terms <- terms(formula)
  yvarName <- as.character(as.list((attr(terms,'variables')))[[attr(terms,'response')+1]])
  y <- data[,yvarName]
  xTx <- t(x) %*% ( wts * x ) + 1.0e-5*diag(nVars)
  xTy <- t(x) %*% ( wts * y )
  pi <- function(i) {
    xTxi <- xTx - wts[i] * (x[i,] %o% x[i,])
    xTyi <- xTy - wts[i] * (x[i,] * y[i])
    betai <- solve(xTxi,xTyi)
    predi <- as.numeric(x[i,] %*% betai)
  }
  preds <- sapply(1:nRows,pi)
  preds
}
qualidade.do.ajuste <- function(modelo, data, y, glm){
  if (glm){
    `Pseudo R²`   <- as.numeric(PseudoR2(modelo)[1])   # Pseudo R² ajustado
    hopreds       <- hold1OutLMPreds(modelo, data)     # Predições para observação i
    devs          <- y-hopreds                         # Observado - ajustado
    PRESS         <- sum(devs^2)                       # Soma dos desvios ao quadrado em relação a predição = PRESS
    RMPRESS       <- sqrt(mean(devs^2))                # Root mean PRESS - Raiz do desvio médio dos valores preditos
    homeans       <- hold1OutMeans(y)                  # Média da variável sem levar em consideração a observação i)
    dely          <- y-homeans                         # Desvio da observação i menos a média dos valores sem considerar a observação i
    `R² PRESS`    <- 1 - (PRESS/sum(dely^2))           # "R² PRESS"
    EAM           <- mean(abs(y-hopreds))              # Erro absoluto médio
    `ERM (%)`     <- mean((abs(y-hopreds)/y))*100      # Erro relativo médio
    
    return(round(cbind(`Pseudo R²`, `R² PRESS`, PRESS, RMPRESS, EAM, `ERM (%)`),3))
  } else{
    `R² ajustado` <- summary(modelo)$adj.r.squared #R² ajustado
    hopreds       <- hold1OutLMPreds(modelo, data) # Predições para observação i
    devs          <- y-hopreds                     # Observado - ajustado
    PRESS         <- sum(devs^2)                   # Soma dos desvios ao quadrado em relação a predição = PRESS
    RMPRESS       <- sqrt(mean(devs^2))            # Root mean PRESS - Raiz do desvio médio dos valores preditos
    homeans       <- hold1OutMeans(y)              # Média da variável sem levar em consideração a observação i)
    dely          <- y-homeans                     # Desvio da observação i menos a média dos valores sem considerar a observação i
    `R² PRESS`    <- 1 - (PRESS/sum(dely^2))       # "R² PRESS"
    EAM           <- mean(abs(y-hopreds))          # Erro absoluto médio
    `ERM (%)`     <- mean((abs(y-hopreds)/y))*100  # Erro relativo médio
    return(round(cbind(`R² ajustado`, `R² PRESS`, PRESS, RMPRESS, EAM, `ERM (%)`),3))
  }
}


# Cálculo de tamanho amostral

zstar = qnorm(.975)
p = 0.01
E = 0.015
samp <- function(zstar, p, E){
  ((zstar^2)*(p)*(1-p))/E^2
}
# samp(zstar, p, E)

samp.size = function(z.val, margin, c.interval, population) {
  ss = (z.val^2 * margin * (1 - margin))/(c.interval^2)
  return(ss/(1 + ((ss - 1)/population)))
}
samp.size(zstar, p, 0.05, 400)

# Nuvem de palavras
word.cloud <- function(x.texto, words.barras=10, tokenizer = T, dropnum = F, remove.sparse = T, paleta = RColorBrewer::brewer.pal(8, "Dark2"), min.words = 2, stops = NULL, freq.percen = T){
  q17 <- VCorpus(VectorSource(x = x.texto),
                 readerControl = list(language = "pt",
                                      load = TRUE))
  q17 <- tm_map(q17, FUN = content_transformer(tolower)) #Minúsculas
  q17 <- tm_map(q17, FUN = removePunctuation) # retira pontuação (,. etc.)
  if (dropnum) {q17 <- tm_map(q17, FUN = removeNumbers)}     # Remove os números
  q17 <- tm_map(q17, FUN = stripWhitespace)   # Remove espaços em branco extras
  q17 <- tm_map(q17, FUN = removeWords, words = c(stopwords("portuguese"), stops))
  #[!stopwords("portuguese")=="não"]) # Remove stopwords, mas mantém o "não"
  #methods(class = class(q17))
  if (tokenizer) {BigramTokenizer <- function(x) {
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))
  }
  dtm <- DocumentTermMatrix(q17,
                            control = list(tokenize = BigramTokenizer))} else {
                              dtm <- DocumentTermMatrix(q17)}
  
  if (remove.sparse) {dtm <- removeSparseTerms(dtm, 0.94)} # Se tu tem 20 documentos, a palavra tem que aparecer pelo menos em 2 documentos: Se aparecer menos que 6% dos documentos }
  freq <- slam::colapply_simple_triplet_matrix(dtm, FUN = sum)
  freq <- sort(freq, decreasing = TRUE)
  d <- as.data.frame(freq)
  d$word <- row.names(d)
  row.names(d) <- NULL
  d$word.rad <- SnowballC::wordStem(d$word, language = "portuguese") # Mantém o radical da palavra
  # mft <- findFreqTerms(tdm, lowfreq = 10)
  d2 <- aggregate(freq ~ word.rad, FUN = sum, data = d)
  names(d2)[2] <- c("freq2")
  d2 <- merge(d2,d)
  d2 <- d2[!duplicated(d2[,c("word.rad")]), ]
  d2 <- d2[order(d2$freq2, decreasing = T),]
  d2 <- d2[, c("word","freq2")]
  d2$`%multi` <- round((d2$freq2/length(x.texto))*100,1)
  # Print da nuvem de palavras
  par(mfrow = c(1,2), oma = rep(0,4), mai = rep(0,4), mar = rep(0,4))
  wordcloud::wordcloud(words = d2$word,
            freq = d2$freq2,
            random.order = F,
            min.freq = min.words,
            colors = paleta,
            family = "calibre",
            fixed.asp = F,
            rot.per = 0)
  # Print do gráfico de barras
  plot.new()
  vps <- gridBase::baseViewports()
  if (freq.percen){
    g2 <- ggplot(d2[1:words.barras, ], aes(x = reorder(word, freq2), y=freq2)) +
      geom_bar(stat="identity", fill = "lightgray") +
      geom_text(aes(label=paste0(freq2, " (", `%multi`, "%)")), col="black", size=4, hjust = -0.05) +
      labs(y="Frequência (% de respostas)",
           x= paste(words.barras, "mais frequentes")) +
      scale_y_continuous(limits = c(0,d2[1, 2]+5), labels = round) +
      coord_flip() +
      theme(axis.text.x = element_text(size = 13, family = "calibre"),
            axis.text.y = element_text(size = 13, family = "calibre"),
            axis.title.x = element_text(size = 10,family = "calibre"),
            axis.title.y = element_text(size = 13,family = "calibre"),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  } else{
    g2 <- ggplot(d2[1:words.barras, ], aes(x = reorder(word, freq2), y=freq2)) +
      geom_bar(stat="identity", fill = "lightgray") +
      geom_text(aes(label=freq2), col="black", size=4, hjust = -0.05) +
      labs(y="Frequência de respostas",
           x= paste(words.barras, "mais frequentes")) +
      scale_y_continuous(limits = c(0,d2[1, 2]+5), labels = round) +
      coord_flip() +
      theme(axis.text.x = element_text(size = 13, family = "calibre"),
            axis.text.y = element_text(size = 13, family = "calibre"),
            axis.title.x = element_text(size = 10,family = "calibre"),
            axis.title.y = element_text(size = 13,family = "calibre"),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  }
  print(g2, vp = grid::vpStack(vps$figure,vps$plot))
}


# Comparações Múltiplas

## Preparando as matrizes
#source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/pairwise.R")
glht <- function(mat, model, resp = 1, quantil = 0.975){
  
  coef.mat <- coef(model) #Obtem TODOS os coeficientes do modelo
  coefs <- coef.mat[coef.mat$Response==resp & coef.mat$Type=="beta",] # Obtem somente da variável resposta
  positions <- as.numeric(row.names(coef.mat[coef.mat$Response==resp & coef.mat$Type=="beta",])) #Posições das variáveis
  vcovs <- as.matrix(vcov(model))[positions , positions] # Matriz de variância e covariância para a variável resposta
  
  # Medidas Pontuais para cada parâmetros
  mu <- mat%*%coefs$Estimates  #Exponencial disso dá o valor médio na escala do canto
  var <- mat%*%vcovs%*%t(mat)
  e.p <- sqrt(diag(var)) # fazer pelo método delta para obter o erro padrão para ter na escala do Exponencial (se necessário)
  media <- exp(mu)
  ic.inf <- mu-qnorm(quantil)*e.p
  ic.sup <- mu+qnorm(quantil)*e.p
  ic <- paste(round(exp(ic.inf), 2), round(exp(ic.sup),2), sep = ";")
  mu.int <- data.frame(Media = media,
                       Intervalo = ic)
  
  # Calcula as diferenças e os erros padrões das diferenças
  ctr <- apc(mat) # Essa função do prof. Walmes calcula todas as correlações 
  d <- ctr%*%coefs$Estimates
  var <- ctr%*%vcovs%*%t(ctr)
  sd <- sqrt(diag(var)) # Erro Padrao
  
  # Formata, calcular medidas adicionais
  z.value <- abs(d/sd)
  p.values <- (2*pnorm(z.value, lower.tail = F)) # Bilateral
  p.values <- ifelse(p.values>=1,1,p.values)
  p.values.adj <- p.adjust(p.values, method = "fdr")
  # Intervalos de confiança
  IC.Inf <- d - qnorm(quantil)*sd
  IC.Sup <- d + qnorm(quantil)*sd
  IC <- paste(round(IC.Inf,2), round(IC.Sup,2), sep = ";")
  IC.Inf.exp <- exp(IC.Inf)
  IC.Sup.exp <- exp(IC.Sup)
  diff.razao <- exp(d)
  IC.exp <- paste(round(IC.Inf.exp,2), round(IC.Sup.exp,2), sep = ";")
  resumo <- data.frame(Estimativa = d,
                       `Erro Padrão` = sd,
                       Valor.Z = z.value,
                       Valorp = p.values,
                       Valorp.adj = round(p.values.adj,6),
                       IC = IC,
                       IC.exp = IC.exp,
                       RazaoChance = diff.razao)
  list(glht = resumo,
       means = mu.int)
}


###### Comparações Múltiplas multivariado


# Multivariado
# I <- Diagonal(ncol(K))
# I <- Diagonal(length(m1b$mu_list), 1)
# test <- kronecker(as.matrix(I),K)
# coef.mat <- coef(m1b) #Obtem TODOS os coeficientes do modelo
# coefs <- coef.mat[coef.mat$Type=="beta",] # Obtem somente da variável resposta
# apc(test)
# test%*%coefs$Estimates
# ## 15 covariáveis
# ## 4 variáveis respostas
# positions <- as.numeric(row.names(coef.mat[coef.mat$Type=="beta",])) #Posições das variáveis
# vcovs <- as.matrix(vcov(m1b))[positions , positions] # Matriz de variância e covariância para a variável resposta


# Extras

#' crosstab <- function (..., dec.places = NULL,type = NULL,style = "wide",row.vars = NULL,col.vars=NULL,percentages = TRUE,addmargins = TRUE,subtotals=TRUE){
#'   
#'   ###################################################################################
#'   #                                                                                 #
#'   # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
#'   # School of Environmental Sciences, University of Liverpool, UK.                  #
#'   #                                                                                 #
#'   # Adapted from the function ctab() in the catspec packge.                         #
#'   #                                                                                 #
#'   # Version: 12th July 2013                                                         #
#'   #                                                                                 #
#'   # Output best viewed using the companion function print.crosstab()                #
#'   #                                                                                 #
#'   ###################################################################################
#'   
#'   
#'   #Declare function used to convert frequency counts into relevant type of proportion or percentage
#'   
#'   mk.pcnt.tbl <- function(tbl, type) {
#'     a <- length(row.vars)
#'     b <- length(col.vars)
#'     mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
#'                    row.pct = c(row.vars, col.vars[-b]),
#'                    joint.pct = c(row.vars[-a], col.vars[-b]),
#'                    total.pct = NULL)
#'     tbl <- prop.table(tbl, mrgn)
#'     if (percentages) {
#'       tbl <- tbl * 100
#'     }
#'     tbl
#'   }
#'   
#'   #Find no. of vars (all; row; col) for use in subsequent code
#'   n.row.vars <- length(row.vars)
#'   n.col.vars <- length(col.vars)
#'   n.vars <- n.row.vars + n.col.vars
#'   
#'   
#'   #Check to make sure all user-supplied arguments have valid values
#'   stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
#'   #type: see next section of code
#'   stopifnot(is.character(style))    
#'   stopifnot(is.logical(percentages))
#'   stopifnot(is.logical(addmargins))
#'   stopifnot(is.logical(subtotals))
#'   stopifnot(n.vars>=1)
#'   
#'   #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
#'   #If invalid type supplied, failed match gives user automatic error message
#'   types <- NULL
#'   choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
#'   for (tp in type) types <- c(types, match.arg(tp, choices))
#'   type <- types
#'   
#'   #If no type supplied, default to 'frequency + total' for univariate tables and to
#'   #'frequency' for multi-dimenstional tables
#'   
#'   #For univariate table....
#'   if (n.vars == 1) {
#'     if (is.null(type)) {
#'       # default = freq count + total.pct  
#'       type <- c("frequency", "total.pct")
#'       #row.vars <- 1
#'     } else {
#'       #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
#'       type <- ifelse(type == "frequency", "frequency", "total.pct")
#'     }
#'     #For multivariate tables...
#'   } else if (is.null(type)) {
#'     # default = frequency count  
#'     type <- "frequency"
#'   }
#'   
#'   
#'   
#'   #Check for integrity of requested analysis and adjust values of function arguments as required
#'   
#'   if ((addmargins==FALSE) & (subtotals==FALSE)) {
#'     warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
#'     subtotals <- TRUE
#'   }
#'   
#'   if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
#'     warning("WARNING: Only row totals added when more than one table type requested")
#'     #Code lower down selecting type of margin implements this...
#'   }
#'   
#'   if ((length(type)>1) & (subtotals==FALSE)) { 
#'     warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
#'     subtotals <- TRUE
#'   }
#'   
#'   if ((length(type)==1) & (subtotals==FALSE)) {
#'     choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
#'     tp <- match.arg(type, choices)
#'     if (tp %in% c("row.pct","column.pct","joint.pct")) {
#'       warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
#'       subtotals<- TRUE
#'     }
#'   }
#'   
#'   if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
#'     warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
#'   
#'   
#'   if ( (subtotals==FALSE) & (n.vars>2) )  {
#'     #If subtotals not required AND total table vars > 2
#'     #Reassign all but last col.var as row vars
#'     #[because, for simplicity, crosstabs assumes removal of subtotals uses tables with only ONE col var]
#'     #N.B. Subtotals only present in tables with > 2 cross-classified vars...
#'     if (length(col.vars)>1) {
#'       row.vars <- c(row.vars,col.vars[-length(col.vars)])
#'       col.vars <- col.vars[length(col.vars)]
#'       n.row.vars <- length(row.vars)
#'       n.col.vars <- 1
#'     }
#'   }
#'   
#'   #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
#'   #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
#'   if (is.null(dec.places)) {
#'     if ((length(type)==1) & (type[1]=="frequency")) {
#'       dec.places <- 0
#'     } else {
#'       dec.places <-2
#'     }
#'   }
#'   
#'   #Take the original input data, whatever form originally supplied in,
#'   #convert into table format using requested row and col vars, and save as 'tbl'
#'   
#'   args <- list(...)    
#'   
#'   if (length(args) > 1) {
#'     if (!all(sapply(args, is.factor))) 
#'       stop("If more than one argument is passed then all must be factors")
#'     tbl <- table(...)
#'   }
#'   else {
#'     if (is.factor(...)) {
#'       tbl <- table(...)
#'     }
#'     else if (is.table(...)) {
#'       tbl <- eval(...)
#'     }
#'     else if (is.data.frame(...)) {
#'       #tbl <- table(...)
#'       if (is.null(row.vars) && is.null(col.vars)) {
#'         tbl <- table(...)
#'       }
#'       else {
#'         var.names <- c(row.vars,col.vars)
#'         A <- (...)
#'         tbl <- table(A[var.names])
#'         if(length(var.names==1)) names(dimnames(tbl)) <- var.names
#'         #[table() only autocompletes dimnames for multivariate crosstabs of dataframes]
#'       }
#'     }
#'     else if (class(...) == "ftable") {
#'       tbl <- eval(...)
#'       if (is.null(row.vars) && is.null(col.vars)) {
#'         row.vars <- names(attr(tbl, "row.vars"))
#'         col.vars <- names(attr(tbl, "col.vars"))
#'       }
#'       tbl <- as.table(tbl)
#'     }
#'     else if (class(...) == "ctab") {
#'       tbl <- eval(...)
#'       if (is.null(row.vars) && is.null(col.vars)) {
#'         row.vars <- tbl$row.vars
#'         col.vars <- tbl$col.vars
#'       }
#'       for (opt in c("dec.places", "type", "style", "percentages", 
#'                     "addmargins", "subtotals")) if (is.null(get(opt))) 
#'                       assign(opt, eval(parse(text = paste("tbl$", opt, 
#'                                                           sep = ""))))
#'       tbl <- tbl$table
#'     }
#'     else {
#'       stop("first argument must be either factors or a table object")
#'     }
#'   }
#'   
#'   #Convert supplied table style into full text string (e.g. "l" becomes "long")
#'   style <- match.arg(style, c("long", "wide"))
#'   
#'   #Extract row and col names to be used in creating 'tbl' from supplied input data
#'   nms <- names(dimnames(tbl))
#'   z <- length(nms)
#'   if (!is.null(row.vars) && !is.numeric(row.vars)) {
#'     row.vars <- order(match(nms, row.vars), na.last = NA)
#'   }
#'   if (!is.null(col.vars) && !is.numeric(col.vars)) {
#'     col.vars <- order(match(nms, col.vars), na.last = NA)
#'   }
#'   if (!is.null(row.vars) && is.null(col.vars)) {
#'     col.vars <- (1:z)[-row.vars]
#'   }
#'   if (!is.null(col.vars) && is.null(row.vars)) {
#'     row.vars <- (1:z)[-col.vars]
#'   }
#'   if (is.null(row.vars) && is.null(col.vars)) {
#'     col.vars <- z
#'     row.vars <- (1:z)[-col.vars]
#'   }
#'   
#'   #Take the original input data, converted into table format using supplied row and col vars (tbl)
#'   #and create a second version (crosstab) which stores results as percentages if a percentage table type is requested.
#'   if (type[1] == "frequency") 
#'     crosstab <- tbl
#'   else 
#'     crosstab <- mk.pcnt.tbl(tbl, type[1])
#'   
#'   
#'   #If multiple table types requested, create and add these to 
#'   if (length(type) > 1) {
#'     tbldat <- as.data.frame.table(crosstab)
#'     z <- length(names(tbldat)) + 1
#'     tbldat[z] <- 1
#'     pcntlab <- type
#'     pcntlab[match("frequency", type)] <- "Count"
#'     pcntlab[match("row.pct", type)] <- "Row %"
#'     pcntlab[match("column.pct", type)] <- "Column %"
#'     pcntlab[match("joint.pct", type)] <- "Joint %"
#'     pcntlab[match("total.pct", type)] <- "Total %"
#'     for (i in 2:length(type)) {
#'       if (type[i] == "frequency") 
#'         crosstab <- tbl
#'       else crosstab <- mk.pcnt.tbl(tbl, type[i])
#'       crosstab <- as.data.frame.table(crosstab)
#'       crosstab[z] <- i
#'       tbldat <- rbind(tbldat, crosstab)
#'     }
#'     tbldat[[z]] <- as.factor(tbldat[[z]])
#'     levels(tbldat[[z]]) <- pcntlab
#'     crosstab <- xtabs(Freq ~ ., data = tbldat)
#'     names(dimnames(crosstab))[z - 1] <- ""
#'   }
#'   
#'   
#'   #Add margins if required, adding only those margins appropriate to user request
#'   if (addmargins==TRUE) {
#'     
#'     vars <- c(row.vars,col.vars)
#'     
#'     if (length(type)==1) {
#'       if (type=="row.pct") 
#'       { crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
#'       tbl <- addmargins(tbl,margin=c(vars[n.vars]))
#'       }
#'       else 
#'       { if (type=="column.pct") 
#'       { crosstab <- addmargins(crosstab,margin=c(vars[n.row.vars]))
#'       tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
#'       }
#'         else 
#'         { if (type=="joint.pct") 
#'         { crosstab <- addmargins(crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
#'         tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
#'         }
#'           else #must be total.pct OR frequency
#'           { crosstab <- addmargins(crosstab)
#'           tbl <- addmargins(tbl)
#'           }
#'         }
#'       } 
#'     }
#'     
#'     #If more than one table type requested, only adding row totals makes any sense...
#'     if (length(type)>1) {
#'       crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
#'       tbl <- addmargins(tbl,margin=c(vars[n.vars]))
#'     }
#'     
#'   }  
#'   
#'   
#'   #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
#'   #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
#'   t1 <- NULL
#'   if ( (subtotals==FALSE) & (n.vars>2) )  {
#'     
#'     #Create version of crosstab in ftable format
#'     t1 <- crosstab 
#'     t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
#'     
#'     #Convert to a dataframe
#'     t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
#'     
#'     #Remove backslashes from category names AND colnames
#'     t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
#'     #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
#'     #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
#'     t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
#'     
#'     #Reshape dataframe to that variable and category labels display as required
#'     #(a) Move col category names down one row; and move col variable name one column to right
#'     t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
#'     t1[1,] <- ""
#'     t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
#'     #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
#'     t1 <- t1[,-(n.row.vars+1)]
#'     
#'     #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
#'     lab <- t1[,1:n.row.vars]
#'     for (c in 1:n.row.vars) {
#'       for (r in 2:nrow(lab)) {
#'         if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
#'       }
#'     }
#'     
#'     lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
#'     lab <- apply(lab,1,sum)
#'     #Filter out rows of dataframe containing subtotals
#'     
#'     t1 <- t1[((lab==0) | (lab==n.row.vars)),]
#'     
#'     #Move the 'Sum' label associated with last row to the first column; in the process
#'     #setting the final row labels associated with other row variables to ""
#'     t1[nrow(t1),1] <- "Sum"
#'     t1[nrow(t1),(2:n.row.vars)] <- ""
#'     
#'     #set row and column names to NULL
#'     rownames(t1) <- NULL
#'     colnames(t1) <- NULL
#'     
#'   }
#'   
#'   
#'   
#'   #Create output object 'result' [class: crosstab]
#'   result <- NULL
#'   #(a) record of argument values used to produce tabular output
#'   result$row.vars <- row.vars
#'   result$col.vars <- col.vars
#'   result$dec.places <- dec.places
#'   result$type <- type
#'   result$style <- style
#'   result$percentages <- percentages
#'   result$addmargins <- addmargins
#'   result$subtotals <- subtotals
#'   
#'   #(b) tabular output [3 variants]
#'   result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
#'   result$crosstab <- crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
#'   #[class: table]  
#'   result$crosstab.nosub <- t1  #crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
#'   class(result) <- "crosstab"    
#'   
#'   #Return 'result' as output of function
#'   result
#'   
#' }
#' print.crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
#'   
#'   ###################################################################################
#'   #                                                                                 #
#'   # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
#'   # School of Environmental Sciences, University of Liverpool, UK.                  #
#'   #                                                                                 #
#'   # Adapted from the function print.ctab() in the catspec packge.                   #
#'   #                                                                                 #
#'   # Version: 12th July 2013                                                         #
#'   #                                                                                 #
#'   # Designed to provide optimal viewing of the output from crosstab()               #
#'   #                                                                                 #
#'   ###################################################################################
#'   
#'   row.vars <- x$row.vars
#'   col.vars <- x$col.vars
#'   n.row.vars <- length(row.vars)
#'   n.col.vars <- length(col.vars)
#'   n.vars <- n.row.vars + n.col.vars
#'   
#'   if (length(x$type)>1) {
#'     z<-length(names(dimnames(x$crosstab)))
#'     if (x$style=="long") {
#'       row.vars<-c(row.vars,z) 
#'     } else {
#'       col.vars<-c(z,col.vars)
#'     }
#'   }
#'   
#'   if (n.vars==1) {
#'     if (length(x$type)==1) {
#'       tmp <- data.frame(round(x$crosstab,x$dec.places))
#'       colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
#'       print(tmp,row.names=FALSE)
#'     } else {
#'       print(round(x$crosstab,x$dec.places))
#'     }
#'   }
#'   
#'   
#'   #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
#'   #print table using ftable() on x$crosstab
#'   if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
#'     
#'     tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)
#'     
#'     if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
#'     print(tbl,...)
#'     
#'   }
#'   
#'   #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
#'   if ((subtotals==FALSE) & (n.vars>2))  {
#'     
#'     t1 <- x$crosstab.nosub
#'     
#'     #Convert numbers to required decimal places, right aligned
#'     width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
#'     dec.places <- x$dec.places
#'     number.format <- paste("%",width,".",dec.places,"f",sep="")
#'     t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
#'     
#'     #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
#'     col.var.format <- paste("%-",width,"s",sep="")
#'     t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
#'     #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
#'     col.cat.format <- paste("%",width,"s",sep="")
#'     t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
#'     
#'     #Adjust row labels so that each column is of fixed width, using trailing spaces as required
#'     for (i in 1:n.row.vars) {
#'       width <- max(nchar(t1[,i])) + 2
#'       row.lab.format <- paste("%-",width,"s",sep="")
#'         t1[,i] <- sprintf(row.lab.format,t1[,i])
#'       }
#'     
#'       write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
#' 
#'     }
#'     
#' }