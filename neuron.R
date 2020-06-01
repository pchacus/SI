glass <-
  read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
             header=FALSE, sep=",",dec=".")

colnames(glass) <- c('id', 'RI', 'Sodium', 'Magnesium', 'Aluminium',
                     'Silicon','Potassium', 'Calcium', 'Barium',
                     'Iron', 'Type of glass')


levels(glass$`Type of glass`)
library(neuralnet)

set.seed(214)
summary(glass)
size.sample <- floor(0.5 * nrow(glass))
samples_id <- sample(1:nrow(glass), size.sample)
glasstrain <- glass[c(samples_id),]
glassvalidation <- glass[-c(samples_id),]

glass$`Type of glass` = gsub( '1','bwfp',glass$`Type of glass`)
glass$`Type of glass` = gsub( '2','bwnfp',glass$`Type of glass`)
glass$`Type of glass` = gsub( '3','vwfp',glass$`Type of glass`)
glass$`Type of glass` = gsub( '5','cont',glass$`Type of glass`)
glass$`Type of glass` = gsub( '6','tab',glass$`Type of glass`)
glass$`Type of glass` = gsub( '7','head',glass$`Type of glass`)

table(glass$`Type of glass`)
nnet_glasstrain <- glasstrain

nnet_glasstrain$bwfp<- glasstrain$`Type of glass` == "bwfp"
nnet_glasstrain$bwnfp<- glasstrain$`Type of glass` == "bwnfp"
nnet_glasstrain$vwpf<- glasstrain$`Type of glass` == "vwpf"
nnet_glasstrain$cont<- glasstrain$`Type of glass` == "cont"
nnet_glasstrain$tab<- glasstrain$`Type of glass` == "tab"
nnet_glasstrain$hea<- glasstrain$`Type of glass` == "hea"


nn <- neuralnet(bwfp+bwnfp+vwpf+cont+tab+hea ~
                  RI+Sodium+Magnesium+Aluminium+Silicon+Potassium+Calcium+Barium+Iron,
                data=nnet_glasstrain,
                #hidden=c(10))  #10 sie nie zbiega
                hidden=c(13),stepmax = 1e+06)

#plot(nn)


mypredict <- compute(nn, glassvalidation[-c(1,11)])$net.result  # tablica wektorĂłw zwracanych przez neurony
#mypredict[1,]  - wektor dla pierwszego elementu
#teraz trzeba sprawdziÄ‡ ktĂłry neuron zwrĂłciĹ‚ max wartoĹ›Ä‡
maxidx <- function(arr) {
  return(which(arr == max(arr)))  #wchich zwraca pozycjÄ™ max elementu
}

idx <- apply(mypredict, c(1), maxidx)

prediction <- c('bwfp', 'bwnfp', 'vwpf', 'cont', 'tab','hea')[idx]
table(prediction, glassvalidation$`Type of glass`)
A<-as.matrix(table(prediction, glassvalidation$`Type of glass`))
sum(diag(A))/sum(A)
nrow(glassvalidation)

