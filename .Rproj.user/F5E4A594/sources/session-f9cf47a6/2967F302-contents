library(circular)
load("data/planilla_general.RData")

#long = -53.80125134282754 #CP
long = -54.56571349250657 #LG


data_tests <- datos %>%
  filter(type == "Mammal") %>% 
  select(sitio = site, sistema = station, camara = camera, datetime, sp = species)

data_tests<-data_tests %>% 
  filter(sitio == "LG") %>% 
  mutate(decimal_time = to_decimal_time(horasolar(datetime, long, "America/Montevideo")),
         etapa_dia = if_else(decimal_time <= 4 & decimal_time >= 0 | decimal_time >= 20, "nocturno", 
                             if_else(decimal_time >= 8 & decimal_time <= 16, "diurno",
                                     "crepuscular")))

# n_crepuscular <- sum(data_tests$etapa_dia == "crepuscular")
# n_diurnos <- sum(data_tests$etapa_dia == "diurnos")
# n_nocturnos <- sum(data_tests$etapa_dia == "nocturnos")

# Con lo calculado anteriormente puedo hacer un dataframe donde cada fila es una especie y en las columnas este la cantidad de registros dirunos, nocturnos...

y <- data_tests %>% 
  group_by(sp, etapa_dia) %>%
  summarise(n()) %>% 
  pivot_wider(names_from = etapa_dia, values_from = `n()`) %>% 
  na.omit() %>% 
  ungroup() %>% 
  as.data.frame()

# Paso estos datos a "crudo", sin nombres de columna ni nada de eso
rownames(y) <- y$sp

y <- y %>%
  select(twilight = crepuscular, day = diurno, night = nocturno)


multi.fit.fun = function(y){
  out = diel.fit(t(as.matrix(y)),hyp.set=hyp.sets("General"),
                 post.fit = FALSE, prints=FALSE)
  
  list(ms.model=out$ms.model,prob=out$bf.table)
}

out.multi=apply(y,1,multi.fit.fun)


out <- diel.fit(  
  y = y,  
  hyp.set = hyp.sets("Traditional"),  
  post.fit=TRUE,  
  n.chains=3,  
  n.mcmc=5000,  
  burnin=1000  
)

plot_list = list()

for (x in 1:nrow(y)) {
  print(rownames(y)[x])
  out <- diel.fit(  
    y = as.matrix(y[x,1:3]),  
    hyp.set = hyp.sets("Traditional"),  
    post.fit=TRUE,  
    n.chains=3,  
    n.mcmc=5000,  
    burnin=1000  
  )
  plot_list[[x]] <- triplot(out)
  }

#Crepuscular-Nocturnal, Diurnal-Nocturnal, and Diurnal-Crepuscula

temp=sapply(out.multi,function(x) x[2])
sp.model.probs=matrix(unlist(lapply(temp, "[", , 'Posterior')),ncol=7,byrow = TRUE)
rownames(sp.model.probs)=rownames(y)
colnames(sp.model.probs)=rownames(temp[[1]])
round(sp.model.probs,digits=2)

ms.hyps=unlist(lapply(out.multi,'[',1))
ms.hyps

prob.hyps=unlist(lapply(lapply(out.multi,'[',2), FUN=function(x){max(x$prob[,2])}))
ms.hyps=data.frame(ms.hyps,prob.hyps)


y.df=data.frame(y,hyp=ms.hyps[,1])

multi.fit.fun2 = function(y.df){
  out = diel.fit(t(as.integer(y.df[-4])),hyp.set=y.df[4],
                 post.fit = TRUE, prints=FALSE, 
                 n.chains = 3, n.mcmc = 2000, burnin = 1000)
  
  list(post.samp=out$post.samp[[1]], gelman.diag=out$gelm.diag)
}

out.multi2=apply(y.df,1, multi.fit.fun2)

sapply(out.multi2,function(x) x[2])
post.samples=lapply(sapply(out.multi2,function(x) x[1]), FUN=function(x){do.call("rbind",x)})
prob.quantiles=lapply(post.samples, FUN=function(x){apply(x,2,quantile,probs=c(0.025,0.5,0.975))})

prob.quantiles$Aaxi.post.samp
prob.median=matrix(unlist(lapply(prob.quantiles, FUN = function(x){x[2,]})),ncol=3,byrow = TRUE)
rownames(prob.median)=names(post.samples)
colnames(prob.median)=colnames(prob.quantiles$Coyote.post.samp)
prob.median


post=post.samples
n.species=length(post)

library(bayesplot)
library(ggthemes)

post=lapply(post,FUN=function(x){colnames(x)=c("P(twilight)","P(daytime)","P(nighttime)");x})
post=do.call('rbind',lapply(post,FUN=function(x){mcmc_intervals_data(x, prob = 0.5, prob_outer = 0.95)}))
post$Species <- rep(rownames(y), each = 3)
y.pos=rep(rnorm(nrow(post)/3,0,0.07),each=3)
pos <- position_nudge(y = y.pos, x=rep(0,nrow(post)))
p=ggplot(post, aes(x = m, y = parameter, color = Species)) + 
  geom_point(size=6,position=pos) +
  geom_linerange(aes(xmin = ll, xmax = hh),linewidth=1,position=pos)+
  xlab("Probabilty")+ylab("")+xlim(0,1)+
  theme(text = element_text(size = 20))+ coord_flip()+
  scale_colour_colorblind()+
  #geom_hline(yintercept=c(0.7,1.3,1.7,2.3,2.7,3.3),color="black",size=1.2)+ 
  theme(panel.background = element_rect(fill='white', colour='black'), legend.key=element_rect(fill="white"))

p      
