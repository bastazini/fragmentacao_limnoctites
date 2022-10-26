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


##valores preditos
plogis(confint (mod2, type="psi"))
plogis(confint (mod2, type="col"))
plogis(confint (mod2, type="ext"))
plogis(confint (mod2, type="det"))

#Grafico parametros
#valores

x=c(1,2,3,4)
lower=c(0.2004213,0.03145596,0.008739302,0.564864175)
upper=c(0.8997644,0.3859853,0.3353898,0.9985567)
avg=(lower+upper)/2
  
#plot
par(mar=c(6,6,1,1))
plot(1, type="n", ylab="", yaxt="n", xlab="Estimativa dos parâmetros (IC95%)", xlim=c(0,1), ylim=c(0.5,4.5), main="")
points(avg,x, pch=16,cex=1)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
axis(side=2,at=x,label=c(expression(psi),expression(gamma),expression(epsilon),expression(epsilon [fragmentação])),cex.lab=1.5,las=1)

text(locator(1),"*", cex=1.5)





