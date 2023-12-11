# Title: Assemble Toy Surrogate Dataset
# Author: Mary Lofton
# Date: 06NOV23

# Purpose: Create a toy dataset for Bobby's students to run with for surrogate development

# Notes:

# Data frame format: Parameter 1, parameter 2, temp/weather deviation, datetime, 
# variable, prediction, observation

# load packages ----
# install.packages("plgp")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("maximin")

library(tidyverse)
library(lubridate)
library(plgp)
library(maximin)

# set working directory; you can change this to be any calibration folder ----
setwd("./Surrogate_dataset/Deepm2_exm_weight2_EXO") 

# create matrix of parameter values using maximin space-filling design ----
phyto_groups <- c("cyano","green","diatom")
param_names <- c("pd%R_growth", "pd%w_p")
param_values_list <- list(R_growth = c(0.5, 1, 2, 3, 3.5),
                          w_p = c(-1, -0.5, 0, 0.5, 1))
Xorig <- matrix(data = NA, nrow = 100, ncol = 6)
for(i in 1:nrow(Xorig)){
  Xorig[,1] <- runif(nrow(Xorig), 0.5, 3.5)
  Xorig[,2] <- runif(nrow(Xorig), 0.5, 3.5)
  Xorig[,3] <- runif(nrow(Xorig), 0.5, 3.5)
  Xorig[,4] <- runif(nrow(Xorig), -1, 1)
  Xorig[,5] <- runif(nrow(Xorig), -1, 1)
  Xorig[,6] <- runif(nrow(Xorig), -1, 1)
}


library("lhs")
n <- 100
p <- 2
Xorig <- randomLHS(10, p)
## Not run: 
## maximin.cand
# generate the design
n <- 10
p <- 6
x1 <- seq(0.5, 3.5, length.out=n)
x2 <- seq(-1, 1, length.out=n)
param_list <- list(x1, x1, x1, x2, x2, x2)
Xcand <- expand.grid(param_list)
names(Xcand) <- c("R_growth.cyano","R_growth.green","R_growth.diatom",
                  "w_p.cyano","w_p.green","w_p.diatom")
Tmax <- nrow(Xcand)
Xsparse <- maximin.cand(n=n, Xcand=Xcand, Tmax=Tmax, Xorig=Xorig, 
                        init=NULL, verb=FALSE, tempfile=NULL)

# error:
# Error in !is.null(Xorig) && class(Xorig) != "matrix" : 
#   'length = 2' in coercion to 'logical(1)'

# if I convert the Xorig to a dataframe :
# Error in if (class(X[-row.in, ]) != "matrix") { : 
#     the condition has length > 1

maxmd <- as.numeric(format(round(max(na.omit(Xsparse$mis)), 5), nsmall=5))

library("lhs")
n <- 100
p <- 2
Xorig <- randomLHS(10, p)
x1 <- seq(0, 1, length.out=n)
Xcand <- expand.grid(replicate(p, x1, simplify=FALSE))
names(Xcand) <- paste0("x", 1:2)
T <- nrow(Xcand)
Xsparse <- maximin.cand(n=n, Xcand=Xcand, Tmax=T, Xorig=Xorig, 
                        init=NULL, verb=FALSE, tempfile=NULL)

maxmd <- as.numeric(format(round(max(na.omit(Xsparse$mis)), 5), nsmall=5))


# visualization
par(mfrow=c(1, 2))
X <- Xcand[Xsparse$inds,]
plot(X$x1, X$x2, xlab=expression(x[1]), ylab=expression(x[2]), 
     xlim=c(0, 1), ylim=c(0, 1), 
     main=paste0("n=", n, "_p=", p, "_maximin=", maxmd))
points(Xorig, col=2, pch=20)
abline(h=c(0, 1), v=c(0, 1), lty=2, col=2)
if(!is.null(Xorig))
{
  legend("topright", "Xorig", xpd=TRUE, horiz=TRUE, 
         inset=c(-0.03, -0.05), pch=20, col=2, bty="n")
}
plot(log(na.omit(Xsparse$mis)), type="b", 
     xlab="iteration", ylab="log(minimum distance)", 
     main="progress on minimum distance")
abline(v=n, lty=2)
mtext(paste0("design size=", n), at=n, cex=0.6)

## End(Not run)

# set nml filepath
nml_file <- file.path('./aed/aed2_phyto_pars_27NOV23_MEL.nml')

# set file location of output
nc_file <- file.path('./output/output.nc') 

# save starting version of nml in environment so you can reset after
start_nml <- glmtools::read_nml(nml_file = nml_file)

# for-loop to run GLM using different parameter values
for(i in 1:length(phyto_groups)){
  
  for(j in 1:length(param_values_list[[1]])){
    
    for(k in 1:length(param_values_list[[2]])){
  
  # read in nml
  nml <- glmtools::read_nml(nml_file = nml_file)
  
  # get current parameter values
  curr_R_growth <- nml$phyto_data[[param_names[1]]]
  curr_w_p <- nml$phyto_data[[param_names[2]]]
  
  # replace parameter value as desired
  curr_R_growth[i] <- param_values_list[[1]][j]
  curr_w_p[i] <- param_values_list[[2]][k]
  
  # set nml parameter values
  new_nml <- glmtools::set_nml(nml, arg_name = param_names[1], arg_val = curr_R_growth)
  new_nml1 <- glmtools::set_nml(new_nml, arg_name = param_names[2], arg_val = curr_w_p)
  
  # create path to write permuted nml to file
  write_path <- nml_file
  
  # write permuted nml to file
  glmtools::write_nml(new_nml1, file = write_path)
  
  # run GLM-AED using GLM3r
  GLM3r::run_glm()

  # pull variable of interest from model output
  var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)
  
  # pull parameters from model output
  R_growth <- new_nml1$phyto_data$`pd%R_growth`
  w_p <- new_nml1$phyto_data$`pd%w_p`
  
  # pull f factors
  nc_file <- file.path('output/output.nc') 
  
  for(f in 1:length(f_factor_names)){
    
    f_factor <- glmtools::get_var(nc_file, var_name = f_factor_names[f], reference="surface", z_out=1.6) 
    
    if(f == 1){
      f_factors <- f_factor
    } else {
      f_factors <- left_join(f_factors, f_factor, by = c("DateTime"))
    }
  }
  
  final_factors <- f_factors %>% rename(datetime = DateTime)
  
  # assemble dataframe for that model run
  temp <- data.frame(R_growth_cyano = R_growth[1],
                     R_growth_green = R_growth[2],
                     R_growth_diatom = R_growth[3],
                     w_p_cyano = w_p[1],
                     w_p_green = w_p[2],
                     w_p_diatom = w_p[3],
                     deviation = 0,
                     datetime = var$DateTime,
                     variable = "PHY_tchla_1.6",
                     prediction = var$PHY_tchla_1.6)
  temp1 <- left_join(temp, final_factors, by = "datetime")
  
  # make sure you reset nml
  glmtools::write_nml(start_nml, file = nml_file)
  
  # bind to other model runs
  if(i == 1 & j == 1 & k == 1){
    final <- temp
  } else {
    final <- bind_rows(final, temp)
  }

    }
  }
}

# read in observation dataset
obs <- read_csv("/home/rstudio/RProjects/FCR-GLM-metrics/observations/EXOChla.csv") %>%
  filter(Depth == 1.6) %>%
  rename(observed = PHY_TCHLA) %>%
  select(-Depth)

# write final dataset to file
final1 <- final %>%
  filter(datetime %in% obs$DateTime)
write.csv(final1, file = "./collated_model_scenarios_EXOdates.csv",row.names = FALSE)

# plot parameter space
parms <- final1 %>%
  select(R_growth_cyano, w_p_cyano) %>%
  distinct()

ggplot(data = parms, aes(x = R_growth_cyano, y = w_p_cyano))+
  geom_point(size = 3)+
  theme_bw()

# plot output
plot_data <- final1 %>%
  group_by(R_growth_cyano, w_p_cyano) %>%
  mutate(model_run = cur_group_id()) %>%
  rename(DateTime = datetime) %>%
  left_join(., obs, by = "DateTime")

p <- ggplot(data = plot_data)+
  geom_line(aes(x = DateTime, y = prediction, group = as.factor(model_run), color = as.factor(R_growth_cyano), linetype = as.factor(w_p_cyano)))+
  geom_point(aes(x = DateTime, y = observed), size = 0.5)+
  xlab("")+
  ylab("chlorophyll-a (ug/L) at 1.6 m")+
  labs(color = "Cyano growth rate", linetype = "Cyano sinking rate")+
  theme_classic()+
  theme(legend.position = "bottom")
p
ggsave(p, filename = "./figures/example_param_scenarios.png", units = "in",
       dev = "png", height = 3, width = 7)

# look at f factors
glmtools::sim_vars(file = nc_file)

f_factor_names <- c("PHY_green_fI","PHY_green_fNit","PHY_green_fPho","PHY_green_fSal",
               "PHY_green_fSil","PHY_green_fT","PHY_cyano_fI","PHY_cyano_fNit","PHY_cyano_fPho","PHY_cyano_fSal",
               "PHY_cyano_fSil","PHY_cyano_fT","PHY_diatom_fI","PHY_diatom_fNit","PHY_diatom_fPho","PHY_diatom_fSal",
               "PHY_diatom_fSil","PHY_diatom_fT")



plot_factors <- f_factors %>%
  pivot_longer(PHY_green_fI_1.6:PHY_diatom_fT_1.6, names_to = "var_name", values_to = "f_factor") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  select(-c("PHY","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06") 

factor_plot <- ggplot(data = plot_factors, aes(x = DateTime, y = f_factor, group = factor_name, color = factor_name))+
  geom_line()+
  facet_grid(rows = vars(group))+
  theme_bw()
factor_plot