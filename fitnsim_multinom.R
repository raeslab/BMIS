#Fit multinomial logistic regression and create simulated dataset from fitted model
#Sara Vieira-Silva 12/11/2019

## packages
library('nnet')
library('Hmisc')
library('tidyr')

###load data to be fitted (containing categorical response variable (enterotypes) and a continuous predictor, and a stratification variable) 
originalmat = read.table("fitnsim.example_input_data.tsv",header=TRUE,sep="\t")
originalmat=originalmat[originalmat$Predictor<=6.0,] #chose any inclusion/exclusion criteria to the data to be used for fitting
summary(originalmat)


####Fit multinomial model and create simulated dataset with fitted parameters

#simulated dataset settings
simname="simulation"
ndata = data.frame("Predictor" = c(seq(1.5, 2.999, length = 50),seq(3.0, 6.0, length = 50))) #predictor range over which to create simulations
niter=10 #create N (niter) simulations over the predictor range
simdata=data.frame() #initialize dataframe to store simulated data

for (stratum in unique(originalmat$Stratum)) {
		mysubmat=originalmat[originalmat$Stratum==stratum,] #subset to stratum
		print(paste(stratum,nrow(mysubmat),ncol(mysubmat)))
		modfitted = multinom(Enterotype ~ Predictor, data = mysubmat) #fit multinomial model for stratum
		preddata=gather(data.frame(rMultinom(predict(modfitted, newdata = ndata, type = 'probs'),niter))) #generate simulated dataset using fitted model
		simdata=rbind.data.frame(simdata,cbind.data.frame("Label"=simname,"Predictor"=ndata$Predictor,"Stratum"=stratum,"Prediction"=preddata$value)) #save predictor and generated predictions
		rm(mysubmat,modfitted,preddata)
}

rm(stratum,ndata,niter,simname)
summary(simdata) #summarize simulated dataset
