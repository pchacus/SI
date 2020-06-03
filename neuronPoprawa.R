library(neuralnet)
dane <-
  read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
             header=FALSE, sep=",",dec=".")

colnames(dane) <- c('id', 'RI', 'Sodium', 'Magnesium', 'Aluminium',
                    'Silicon','Potassium', 'Calcium', 'Barium',
                    'Iron', 'Type')


dane$Type = gsub( '1','bwfp',dane$Type)
dane$Type = gsub( '2','bwnfp',dane$Type)
dane$Type = gsub( '3','vwfp',dane$Type)
dane$Type = gsub( '5','cont',dane$Type)
dane$Type = gsub( '6','tab',dane$Type)
dane$Type = gsub( '7','hea',dane$Type)

table(dane$Type)

glass <- dane[dane$Type !='cont',]
glass <- glass[glass$Type !='tab',]
glass <- glass[glass$Type !='vwfp',]

glass$Type <-as.factor(glass$Type)
levels(glass$Type)


set.seed(214)
summary(glass)
size.sample <- floor(0.5 * nrow(glass))
samples_id <- sample(1:nrow(glass), size.sample)
glasstrain <- glass[c(samples_id),]
glassvalidation <- glass[-c(samples_id),]



table(glass$Type)
nnet_glasstrain <- glasstrain

nnet_glasstrain$bwfp<- glasstrain$Type == "bwfp"
nnet_glasstrain$bwnfp<- glasstrain$Type == "bwnfp"
nnet_glasstrain$hea<- glasstrain$Type == "hea"



nn <- neuralnet(bwfp+bwnfp+hea ~
                  RI+Sodium+Magnesium+Aluminium+Silicon+Potassium+Calcium+Barium+Iron,
                data=nnet_glasstrain,
                #hidden=c(10))  #10 sie nie zbiega
                hidden=c(4),stepmax = 1e+06)

#plot(nn)


mypredict <- compute(nn, glassvalidation[-c(1,11)])$net.result  # tablica wektorĂłw zwracanych przez neurony
#mypredict[1,]  - wektor dla pierwszego elementu
#teraz trzeba sprawdziÄ‡ ktĂłry neuron zwrĂłciĹ‚ max wartoĹ›Ä‡
maxidx <- function(arr) {
  return(which(arr == max(arr)))  #wchich zwraca pozycjÄ™ max elementu
}

idx <- apply(mypredict, c(1), maxidx)

prediction <- c('bwfp', 'bwnfp','hea')[idx]
table(prediction, glassvalidation$Type)
A<-as.matrix(table(prediction, glassvalidation$Type))
sum(diag(A))/sum(A)
nrow(glassvalidation)
