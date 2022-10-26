require(unmarked)

##dados
R = 5 # num sitios
J = 1 # num periodos secundarios
T = 8 # num periodos primários

y = matrix(c(
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,0,0,0,0,0,
  0,0,0,1,0,0,0,0,
  0,0,0,1,0,0,0,0), nrow=R, ncol=J*T, byrow=TRUE);y

#covariavel fragmentação
sitio=c("n","n","f","n","f")
sitio=as.data.frame(sitio)

#tempo X frag
temp.frag=matrix(c(
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,
  0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1), nrow=R, ncol=T, byrow=TRUE); temp.frag

#preparação dos dados para unmarked
dados.excol = unmarkedMultFrame(y=y,siteCovs=sitio,yearlySiteCovs=list(year=temp.frag),numPrimary=8)
summary(dados.excol)

#Modelos
mod1= colext(~1, ~1, ~1, ~1, dados.excol)
mod2 = colext(~1, ~1, ~year, ~1, dados.excol)
mod3 = colext(~1, ~year, ~1, ~1, dados.excol)
mod4=colext(~1, ~year, ~year, ~1, dados.excol)


modelos = fitList("psi(.)gam(.)eps(.)p(.)"=mod1,
                  "psi(.)gam()eps(F)p(.)"=mod2, 
                  "psi(.)gam(F)eps(.)p(.)"=mod3,
                  "psi(.)gam(F)eps(F)p(.)"=mod4) 

##selecao de modelos
selecao.mods = modSel(modelos);selecao.mods

#parametros modelos
mod2


#Grafico parametros

#valores

x=c(1,2,3,4)
lower=c(0.2004213,0.03145596,0.008739302,0.564864175)
upper=c(0.8997644,0.3859853,0.3353898,0.9985567)
avg=(lower+upper)/2
  
#plot
par(mar=c(6,5.5,2,1))
plot(1, type="n", ylab="", yaxt="n", xlab="Estimativa dos parâmetros (IC95%)", xlim=c(0,1), ylim=c(0.5,4.5), main="")
title("A", adj = 0.10, line = -1.5)
points(avg,x, pch=16,cex=1)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
axis(side=2,at=x,label=c(expression(psi),expression(gamma),expression(epsilon[Intercepto]),expression(epsilon [Fragmentação])),cex.lab=1.5,las=1)

text(locator(1),"*", cex=1.5)
text(locator(1),"*", cex=1.5)
text(locator(1),"*", cex=1.5)

##gerando as curvas de ocupação
fitted(mod2)

x=rep(1:8)
nfrag=c(0.5999617, 0.6195928, 0.6226051,0.6308489, 0.6375465, 0.642988, 0.6474089, 0.6510007)
frag=c(0.5999691, 0.6124583, 0.2526681, 0.1776778, 0.1620478, 0.158790, 0.1581110,0.1579695)


plot(nfrag~x,type="n",ylim=c(0,0.7),xlab="Tempo", ylab="Valores ajustados de ocupação",main="")
title("B", adj = 0.1, line = -1.5)
lines(nfrag,lwd=3)
lines(frag,lty=3,lwd=3)
legend("bottomleft", legend=c("Não fragmentado", "Fragmentado"),
       lty=c(1,3), box.lty=0,cex=0.8)




