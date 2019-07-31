# Before building a dashboard -- understand model and data 
# Have code for the graphs/tables in place first (mostly)

library(tidyverse)
library(readxl)
library(mgcv)
library(visreg)


negs = read_excel("negative_code.xlsx")
negs %>% filter(variant != "/") %>% mutate(variant_y = ifelse(variant=="D",1,0)) -> negs

#Two models I want to compare#
gam1 = gam(variant_y ~ s(date)+relationship, family="binomial", data=negs)
log1 = glm(variant_y~date+relationship, family="binomial",data=negs)

# The app will use the RDS file
saveRDS(log1,file="log1.rds")
saveRDS(negs,file="negs.rds")

# I could also use this, but I want to produce this in the app. I'm just adopting the code here. 

nonlinear <- visreg(gam1,  scale='response', "date", line.par = list(col = 'red'), plot=FALSE)
linear <- visreg(log1,  scale='response', "date", plot = FALSE)

dplyr::bind_rows(
  dplyr::mutate(nonlinear$fit, Model = "GAM"),
  dplyr::mutate(linear$fit, Model = "Logistic")
) -> fits

saveRDS(fits,file="fitmodels.rds")

ggplot() +
  geom_ribbon(
    data = fits, 
    aes(date, ymin=visregLwr, ymax=visregUpr, group=Model), fill="gray90"
  ) +
  geom_line(data = fits, aes(date, visregFit, group=Model, color=Model)) +
  ylab("P(DO vs Inversion)")+
  xlab("Date") + 
  theme_bw()

logdevexp = 1 - log1$deviance / log1$null.deviance


saveRDS("gam1", file="gam1.rds")
saveRDS("negs",file="negs.rds")

