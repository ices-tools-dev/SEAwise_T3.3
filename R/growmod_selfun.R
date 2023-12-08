
## Formulas for model selection ------------------------------------------
forms.s=c(
  ~1,
  ~nao.s,
  ~ssb.s,
  ~sst.s,
  ~nao.s + ssb.s,
  ~nao.s + sst.s,
  ~ssb.s + sst.s,
  ~nao.s + ssb.s +sst.s,
  
  ~age,
  ~age+ nao.s,
  ~age+ ssb.s,
  ~age+ sst.s,
  ~age+ nao.s + ssb.s,
  ~age+ nao.s + sst.s,
  ~age+ ssb.s + sst.s,
  ~age+ nao.s + ssb.s +sst.s,
  
  ~age +I(age^2),
  ~age +I(age^2)+ nao.s,
  ~age +I(age^2)+ ssb.s,
  ~age +I(age^2)+ sst.s,
  ~age +I(age^2)+ nao.s + ssb.s,
  ~age +I(age^2)+ nao.s + sst.s,
  ~age +I(age^2)+ ssb.s + sst.s,
  ~age +I(age^2)+ nao.s + ssb.s +sst.s,
  
  ~age +I(age^2)+I(age^3),
  ~age +I(age^2)+I(age^3)+ nao.s,
  ~age +I(age^2)+I(age^3)+ ssb.s,
  ~age +I(age^2)+I(age^3)+ sst.s,
  ~age +I(age^2)+I(age^3)+ nao.s + ssb.s,
  ~age +I(age^2)+I(age^3)+ nao.s + sst.s,
  ~age +I(age^2)+I(age^3)+ ssb.s + sst.s,
  ~age +I(age^2)+I(age^3)+ nao.s + ssb.s +sst.s
)


forms2.s=expand.grid(formulaX=forms.s, formulaM=forms.s)

growmod_BIC <- function(dat, i){
	
  growmod::growmod(formulaX = as.formula(forms2.s[i, "formulaX"][[1]]), 
          formulaM = as.formula(forms2.s[i, "formulaM"][[1]]), 
          data=dat)$BIC

}