library(learnr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(kableExtra)
library(readr)
library(knitr)
library(randomForest)
require(caTools)
library(ROCit)
library(stargazer)
library(shiny)
library(shinycustomloader)
library(grf)
library(DiagrammeRsvg)
library(caret)
library(estimatr)

set.seed(12345) 
my_data <- read_delim("C:/Users/Usuario/Dropbox/02. IN4402/IN4402-2021-1/EOL/CRF/social_voting.csv", ";", escape_double = FALSE, trim_ws = TRUE)
n_obs <- 6000 
my_data <- my_data[sample(nrow(my_data), n_obs), ]
my_data$sex=as.factor(my_data$sex)
my_data$g2000=as.factor(my_data$g2000)
my_data$g2002=as.factor(my_data$g2002)
my_data$p2000=as.factor(my_data$p2000)
my_data$p2002=as.factor(my_data$p2002)
my_data$p2004=as.factor(my_data$p2004)
my_data_train = my_data %>% sample_frac(0.8)
my_data_test = anti_join(my_data, my_data_train, by = 'id')
Y1 <- my_data_train$Y
W1 <- my_data_train$W
X1 <- my_data_train[,4:ncol(my_data_train)]
X1 <- lapply(X1, as.numeric) ## Usar model.matrix con la matrix X1
Y2 <- my_data_test$Y
W2 <- my_data_test$W
X2 <- my_data_test[,4:ncol(my_data_train)]
X2 <- lapply(X2, as.numeric)

causalforest <- causal_forest(X=as.data.frame(X1), W=W1, Y=Y1, num.trees = 200, mtry = 2)


tr <- get_tree(causalforest, 32)
plot(tr)
p <- plot(tr)

cate_cf1 <- predict(causalforest, data = data.frame(X1))$predictions
hist(cate_cf1, main = "Histograma del CATE esperado")
abline(v = 0, col = "red", lwd=3, lty=2)



cate_se <- sqrt(predict(cf, newdata = data.frame(X2), estimate.variance = TRUE)$variance.estimates)
cate_cf1_high_ci <- cate_cf1 + cate_se* qnorm(0.975)
cate_cf1_low_ci <- cate_cf1 - cate_se* qnorm(0.975)
CATE <- as.data.frame(cbind(cate_cf1, cate_cf1_high_ci, cate_cf1_low_ci))
CATE <- CATE[order(CATE$cate_cf1, decreasing = FALSE),]



ggplot() + 
  geom_point(CATE, mapping = aes(x =1:1800, y = cate_cf1 )) +
  geom_smooth(CATE, mapping = aes(x =1:1800, y = cate_cf1_high_ci )) +
  geom_smooth(CATE, mapping = aes(x =1:1800, y = cate_cf1_low_ci )) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") + 
  theme_fivethirtyeight()+ 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5,size=18), 
        text = element_text(size=18), 
        axis.title = element_text(),
        axis.text.x = element_text(angle=60, hjust=1,size=18)) + 
  xlim(1,1800) + 
  labs(x="Indicador Observaciones", #agrega etiqueta del eje X
       y="Efecto de Tratamiento", #agrega etiqueta del eje y
       title="Heterogeneidad del ATE")



cf <- causal_forest(X=as.data.frame(X1), W=W1, Y=Y1, 
                    num.trees = 1000, mtry = 8, min.node.size=5)
cate_cf1 <- predict(cf, data = data.frame(X1))$predictions
cate_se <- sqrt(predict(cf, data = data.frame(X1), estimate.variance = TRUE)$variance.estimates)
cate_cf1_high_ci <- cate_cf1 + cate_se* qnorm(0.975)
cate_cf1_low_ci <- cate_cf1 - cate_se* qnorm(0.975)
CATE <- as.data.frame(cbind(cate_cf1, cate_cf1_high_ci, cate_cf1_low_ci))
CATE <- CATE[order(CATE$cate_cf1, decreasing = FALSE),]


ggplot() + 
  geom_point(CATE, mapping = aes(x =1:4800, y = cate_cf1 )) +
  geom_smooth(CATE, mapping = aes(x =1:4800, y = cate_cf1_high_ci )) +
  geom_smooth(CATE, mapping = aes(x =1:4800, y = cate_cf1_low_ci )) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") + 
  geom_vline(xintercept = c(1200, 2400, 3600)) +
  theme_fivethirtyeight()+ 
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5,size=18), 
        text = element_text(size=18), 
        axis.title = element_text(),
        axis.text.x = element_text(angle=60, hjust=1,size=18)) + 
  xlim(1,4800) + 
  labs(x="Indicador Observaciones", #agrega etiqueta del eje X
       y="CATE Estimado", #agrega etiqueta del eje y
       title="Generación de cuartiles")

hist(cate_cf1[cate_cf1$num_cuartil == 1])



my_data_train$num_cuartil <- factor(ntile(cate_cf1, n=4))

ols_sample_ate <- lm_robust(Y ~ num_cuartil + num_cuartil:W, data = my_data_train)

ateq1 <- average_treatment_effect(causalforest, subset = my_data_train$num_cuartil == 1, method = "AIPW")
ateq2 <- average_treatment_effect(causalforest, subset = my_data_train$num_cuartil == 2, method = "AIPW")
ateq3 <- average_treatment_effect(causalforest, subset = my_data_train$num_cuartil == 3, method = "AIPW")
ateq4 <- average_treatment_effect(causalforest, subset = my_data_train$num_cuartil == 4, method = "AIPW")

estimated_aipw_ate <- data.frame(rbind(ateq1, ateq2, ateq3, ateq4))
ols_sample_ate <- lm_robust(Y ~ num_cuartil + num_cuartil:W, data = my_data_train)
estimated_sample_ate <- coef(summary(ols_sample_ate))[(4+1):(2*4), c("Estimate", "Std. Error")]
A <- as.data.frame(estimated_sample_ate)
B <- as.data.frame(estimated_aipw_ate)
colnames(A) <- c("Estimate", "Std. Error")
colnames(B) <- c("Estimate", "Std. Error")
C <- rbind(A,B)
C$method <- c(1:8)
C[1:4,]$method <- "Sample ATE"
C[5:8,]$method <- "AIPW ATE"
C$Cuartil <- c(1:8)
C[c(1,5),]$Cuartil <- 1
C[c(2,6),]$Cuartil <- 2
C[c(3,7),]$Cuartil <- 3
C[c(4,8),]$Cuartil <- 4

ggplot(C, aes(x=Cuartil, y=Estimate, colour=method)) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=Estimate-(1.647 * `Std. Error`),
                    ymax=Estimate+(1.647 * `Std. Error`), colour=method), 
                    width=.2,position=position_dodge(.9)) +
  ylim(-0.1, 0.2) +
  theme_fivethirtyeight()+ 
  theme(legend.title = element_blank(),panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5,size=18), 
        text = element_text(size=18), 
        axis.title = element_text(),
        axis.text.x = element_text(angle=60, hjust=1,size=18))


ggplot() +
  geom_point(aes(x = 1:4, y = A$Estimate), size = 3, color = "#619CFF", position = position_dodge(.5)) +
  geom_errorbar(width = .5, aes(x = 1:4, y = A$Estimate,
    ymin = A$Estimate - (1.647 * A$`Std. Error`),
    ymax = A$Estimate + (1.647 * A$`Std. Error`),
    color = "Sample ATE"
  )) + 
    geom_point(aes(x = 1:4, y = B$Estimate), size = 3, color = "#F8766D",  position = position_dodge(.5)) +
  geom_errorbar(position=position_dodge(), width = .5, aes(x = 1:4, y = B$Estimate,
    ymin = B$Estimate - (1.647 * B$`Std. Error`),
    ymax = B$Estimate + (1.647 * B$`Std. Error`),
    color = "AIPW ATE"
  ))  + ylim(-0.2, 0.3) +
  theme_fivethirtyeight()+ 
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5,size=18), 
        text = element_text(size=18), 
        axis.title = element_text(),
        axis.text.x = element_text(angle=60, hjust=1,size=18)) +
  xlab("Cuartiles") + ylab("ATE")


+
  geom_errorbar(position=position_dodge(width=0.75), width = .5, aes(
    ymin = "Estimate" - (1.647 * "Std. Error"),
    ymax = "Estimate" + (1.647 * "Std. Error"),
    color = "error"
  )) 


hypothesis_sample_ate <- paste0("ntile1:W = ", paste0("ntile", seq(2, 4), ":W"))
ftest_pvalue_sample_ate <- linearHypothesis(ols_sample_ate, hypothesis_sample_ate)[2,"Pr(>F)"]



withLoader(tableOutput("CTplot"), type="text", loader=list(marquee("Construyendo tu árbol...", direction="left", style="font-size:20px; color:gray", scrollamount=10))),