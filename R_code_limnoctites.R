require(unmarked)
teste=read.table(file.choose(),h=F,row.names = 1)
fator=teste[1,]

##dados
R = 5 # num sitios
J = 1 # num periodos secundarios
T = 8 # numperiodos primários

y = matrix(c(
  1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,
  1,1,1,0,0,0,0,0,
  0,0,0,1,0,0,0,0,
  0,0,0,1,0,0,0,0), nrow=R, ncol=J*T, byrow=TRUE);y

#covariavel fragmentação
sitio=c("n","n","f","n","f")
sitio=as.data.frame(sitio)

#tempoXfrag
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

##sel de modelos
selecao.mods = modSel(modelos);selecao.mods

##valores preditos
predict(mod2, type="psi")
predict(mod2, type="col")
estimativa=predict(mod2, type="ext",data.frame (year=as.vector(temp.frag)))
predict(mod2, type="det")