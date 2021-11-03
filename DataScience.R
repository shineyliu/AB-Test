library("plyr")
library(ggplot2)
library(readr)
library(bayesAB)
AdSmartABdata <- read_csv("Desktop/DataScience/AdSmartABdata.csv")
#1) data define
N_controlyes<-sum((AdSmartABdata$experiment=="control")&(AdSmartABdata$yes==1))
N_controlno<-sum((AdSmartABdata$experiment=="control")&(AdSmartABdata$no==1))
N_exposedyes<-sum((AdSmartABdata$experiment=="exposed")&(AdSmartABdata$yes==1))
N_exposedno<-sum((AdSmartABdata$experiment=="exposed")&(AdSmartABdata$no==1))
N_control<-sum((AdSmartABdata$experiment=="control"))
N_exposed<-sum((AdSmartABdata$experiment=="exposed"))
controlyes<- (AdSmartABdata$experiment=="control")&(AdSmartABdata$yes==1)
exposedyes<- (AdSmartABdata$experiment=="exposed")&(AdSmartABdata$yes==1)


#2) data cleaning
AdSmartABdata_Cleaned <- filter(AdSmartABdata, !(AdSmartABdata$yes == 0 & AdSmartABdata$no == 0))

#3) testing whether other factors are significant
device_make=as.factor(AdSmartABdata$device_make)
platform_os=as.factor(AdSmartABdata$platform_os)
browser=as.factor(AdSmartABdata$browser)
glm_model<-glm(AdSmartABdata$yes~browser+platform_os+
                 as.factor(AdSmartABdata$experiment)+AdSmartABdata$hour,family = binomial(link="logit"))
summary(glm_model)
#other factors not crucial

#4) frequentist a/b testing
N_control<-sum((AdSmartABdata$experiment=="control"))
N_exposed<-sum((AdSmartABdata$experiment=="exposed"))
N_controlyesrate <- N_controlyes / N_control
N_exposedyesrate <- N_exposedyes/N_exposed
sample_size = nrow(AdSmartABdata)

uplift <- (N_exposedyesrate - N_controlyesrate) / N_controlyesrate * 100
print(uplift)
p_pool <- (N_controlyes + N_exposedyes) / (N_control + N_exposed)
SE_pool <-sqrt(p_pool * (1 - p_pool) * ((1 / N_control) + (1 / N_exposed)))
d_hat <- N_exposedyesrate - N_controlyesrate

# return the z, which is the test statistics
z_score <- d_hat / SE_pool
print(z_score)
p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2
print(p_value)
# p-value = 3.5 % > 2.5%
# if we use p-value=0.02 as benchmark, the different is statistically significant, \\
#exposed is slightly more effective comparing with 

#5) bay a/b 

#The weakest prior will be completely objective and thus assign an equal probability to each value for the parameter. Examples of this include a Beta(1, 1) prior for the Bernoulli distribution.
bayab=bayesTest(
  as.numeric(exposedyes),
  as.numeric(controlyes),
  list("alpha" = 1, "beta" = 1),
  n_samples = 1e+05,
  distribution = c( "bernoulli")
)

summary(bayab)


