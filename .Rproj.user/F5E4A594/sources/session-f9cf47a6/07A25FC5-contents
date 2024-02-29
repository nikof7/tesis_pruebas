library(tidyverse)
library(dplyr)
load("data/planilla_general.RData")

devtools::install_github("diel-project/Diel-Niche-Modeling",
                         ref="main", dependencies = TRUE,
                         build_vignettes = TRUE
)

# Para Aaxi en CP

df <- tibble(
  dial = c("Crepuscular", "Diurno", "Nocturno"),
  `Cantidad de registros` = c(90, 86, 55)
)

y <- matrix(c(90, 86, 55), nrow = 1, ncol = 3)

library(Diel.Niche)
out <- diel.fit(  
  y = y,  
  hyp.set = hyp.sets("Traditional"),  
  post.fit=TRUE,  
  n.chains=3,  
  n.mcmc=5000,  
  burnin=1000  
)

triplot(out)
