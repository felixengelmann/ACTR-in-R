########################################################################################################
###
###  Simple Memory Retrieval Model of Agreement Phenomena (based on ACT-R theory)
###    With HTML trace output and PDF activation plots
###        
###
###  Rick Lewis & William Badecker (rickl@umich.edu)
###  Version 3.0
###  2 Feb 2007
###
###  Edited by Felix Engelmann (felix.engelmann@uni-potsdam.de):
###    + Implemented measure "latency"
###    + Added possibility to specify multiple retrievals for critical.retrieval with measure "latency"
###      (result will be sum of latencies)
###    + Added possibility to specify procedural.duration with measure "latency"
###      (result will be sum of latency and proc. duration)
###  21 Sep 2014
###
#########################################################################################################

library(reshape);
library(gplots);


source("run-model.r");
source("model-data-plots.r");

history <- NULL;


## Generate matrix defining combinatoric space of parameter values.  Each
## column is a parameter; each row is a distinct comabination of parameters
## defining a model simulation to run.

parameters <- list(cat.penalty, F, G, ans, mas, d, match.penalty, VAR.fan, var.mismatch.penalty,
                   modulate.by.distinct, distinctiveness);
num.parameters <- length(parameters);


## The total number of combinations is the product of the number of values
## for each parameter

num.combinations <- prod(as.vector(lapply(parameters, length), mode="integer"));


## Set up matrix of parameter combinations.  Rows are model experiments,
## columns are parameters.

num.params <- length(parameters);
p.matrix <- matrix(nrow=num.combinations, ncol=num.params);

cumulative.num.combs <- 1;
for (p in 1:num.params) {
  p.matrix[,p] <- rep(parameters[p][[1]], each=cumulative.num.combs, length.out=num.combinations);
  cumulative.num.combs <- cumulative.num.combs * length(parameters[p][[1]]);
}


##  Now set up matrix of unique model runs.

count.conditions <- function(e) {
  length(e$conditions);
}

total.conditions <- sum(as.vector(lapply(experiments, count.conditions),
                                  mode="integer"));
model.runs <- data.frame();

for (e in 1:num.experiments) {
  exp.name <- experiments[[e]]$name;
  for (c in 1:length(experiments[[e]]$conditions)) {
    cond <- experiments[[e]]$conditions[[c]];

    if(is.null(cond$procedural.duration)) cond$procedural.duration <- 0
    
    model.runs <- rbind(model.runs,
                        data.frame(experiment = rep(exp.name,num.combinations),
                                   condition = rep(cond$condition, num.combinations),
                                   retrievals = rep(cond$retrievals, num.combinations),
                                   items = rep(cond$items, num.combinations),
                                   num.experimental.items = rep(cond$num.experimental.items, num.combinations),
                                   num.experimental.subjects = rep(cond$num.experimental.subjects, num.combinations),
                                   measure = rep(cond$measure, num.combinations),
                                   critical.retrieval = rep(toString(cond$critical.retrieval), num.combinations),
                                   correct.item = rep(cond$correct.item, num.combinations),
                                   distractor.item = rep(cond$distractor.item, num.combinations),
                                   procedural.duration = rep(cond$procedural.duration, num.combinations),
                                   data = rep(cond$data, num.combinations),
#                                   data.lower = rep(cond$data - cond$data.se, num.combinations),
#                                   data.upper = rep(cond$data + cond$data.se, num.combinations),
                                   model = rep(NA, num.combinations),
                                   model.lower = rep(NA, num.combinations),
                                   model.upper = rep(NA, num.combinations),
                                   all.crit.latencies = rep(NA, num.combinations)
                                     ));     # placeholder for
                                        # model result
  }
};



# Duplicate the parameter matrix "total.conditions" number of times.
total.runs <- total.conditions * num.combinations;

full.parameter.matrix <- matrix(data=t(p.matrix), nrow=total.runs,
                                ncol=num.params, byrow=TRUE);
colnames(full.parameter.matrix) <- c("cat.penalty", "F", "G", "ans", "mas", "d", "match.penalty", "VAR.fan",
                                     "var.mismatch.penalty",
                   "modulate.by.distinct", "distinctiveness");


## Finally, form the complete model run matrix.
all.runs <- as.data.frame(cbind(full.parameter.matrix, model.runs));

if(silent.mode==FALSE) pdf(file="activation-plots.pdf",width=11,height=5);


## Loop over all runs and run the models
# r <- 1
for (r in 1:total.runs) {
  
  output.file <- "output.html";
  
  print(paste("Executing run #",r,"of",total.runs));
  
  ## select out row corresponding to this run
  this.run <- all.runs[r,];      
  
  ## now set the model parameters according to this combination of values
  set.parameters(this.run[1:num.parameters]);
  
  ## and run the model
  item.file <- as.character(this.run$items);
  retrieval.file <- as.character(this.run$retrievals);
  num.experimental.items <- this.run$num.experimental.items;
  num.experimental.subjects <- this.run$num.experimental.subjects;

  if(silent.mode==FALSE) results <- run.model(quiet=FALSE) else results <- run.model.quietly();

  ## plot the activation profiles for the critical and distractor items

    clrs <- c("black", "green","blue","orange", "brown");

    if(silent.mode==FALSE) {
      plot.activation(moments, history, this.run$correct.item,
                       this.run$distractor.item,
                       this.run$experiment, this.run$condition);
    }
  

  ## now extract the relevant measure

  all.crit.latencies <- NULL;
  if (this.run$measure=="percent error") {
    ## TODO: extend percent error to several retrievals
    crit.ret <- results[[as.numeric(unlist(strsplit(toString(this.run$critical.retrieval),",")))]];
    model.result <- crit.ret$retrieval.prob[this.run$distractor.item] * 100;
    model.result.lower <- crit.ret$retrieval.prob.lower[this.run$distractor.item] * 100;
    model.result.upper <- crit.ret$retrieval.prob.upper[this.run$distractor.item] * 100;        
  } 
  else if (this.run$measure=="latency") {
    crit.ret.list <- as.numeric(unlist(strsplit(toString(this.run$critical.retrieval),",")));
    # crit.ret.list <- c(3,4)
    model.result <- this.run$procedural.duration;
    model.result.sd <- 0;
    model.result.lower <- 0;
    model.result.upper <- 0;
    for(retr in crit.ret.list){
      crit.ret <- results[[retr]];
      winner <- length(crit.ret$latency.mean)
      model.result <- model.result + crit.ret$latency.mean[winner];
      model.result.sd <- model.result.sd + crit.ret$latency.sd[winner]^2;
      all.crit.latencies <- c(all.crit.latencies, crit.ret$latency.mean[winner]);
    }
    model.result.lower <- model.result - sqrt(model.result.sd);
    model.result.upper <- model.result + sqrt(model.result.sd);
  }
  else {
    model.result <- NA;
    model.result.lower <- NA;
    model.result.upper <- NA;        
    print(paste("The", this.run$measure, "measure is not yet implemented."));
  }
  all.runs[r,]$model <- model.result;
  all.runs[r,]$model.lower <- model.result.lower;
  all.runs[r,]$model.upper <- model.result.upper;
  all.runs[r,]$all.crit.latencies <- toString(all.crit.latencies);

  
}

if(silent.mode==FALSE) dev.off();







## Compute MSE and R^2 for each unique combination of parameter settings

param.results <- data.frame(experiment=rep(NA,num.combinations*num.experiments),
                            combo=rep(NA,num.combinations*num.experiments),
                            r2=rep(NA,num.combinations*num.experiments),
                            mse=rep(NA,num.combinations*num.experiments),
                            smse=rep(NA,num.combinations*num.experiments)
                            );

print("Computing aggregate model fits for each unique parameter setting....");

i <- 1;

for (e in 1:num.experiments) {
  exp <- experiments[[e]]$name;
  
  for (j in 1:num.combinations) {
    print(paste("    parameter setting #",j,"of",num.combinations));
    index <- seq(from=j, to=total.runs, by=num.combinations);
    runs <- all.runs[index,];
    runs.exp <- runs[runs$experiment==exp,];
    param.results$combo[i] <- j;
    param.results$experiment[i] <- exp;

    d <- runs.exp$data;
    m <- runs.exp$model;
    
    param.results$r2[i] <- cor(d, m)^2;
    param.results$spearman[i] <- cor(d, m, method="spearman");
    param.results$mse[i] <- mean((d - m)^2);
    param.results$smse[i] <- sqrt(param.results$mse[i]);
    i <- i + 1;
  }
}


aggregate.parameter.matrix <- matrix(data=t(p.matrix), nrow=num.combinations*num.experiments,
                                ncol=num.params, byrow=TRUE);
colnames(aggregate.parameter.matrix) <- c("cat.penalty", "F", "G", "ans", "mas", "d", "match.penalty", "VAR.fan",
                                          "var.mismatch.penalty",
                   "modulate.by.distinct", "distinctiveness");

param.results <- cbind(aggregate.parameter.matrix, param.results);


r.melt <- melt(param.results,measure.var=c("r2","mse","smse","spearman"),variable="variable")

r2.summary <- cast(r.melt, combo ~ .,mean,subset = (variable == "r2"));
mse.summary <- cast(r.melt, combo ~ .,mean,subset = (variable == "mse"));
spearman.summary <- cast(r.melt, combo ~ .,mean,subset = (variable == "spearman"));

combo.summary <- cbind(p.matrix,r2.summary[2],mse.summary[2],spearman.summary[2]);

colnames(combo.summary) <- c("cat.penalty", "F", "G", "ans", "mas", "d", "match.penalty", "VAR.fan",
                             "var.mismatch.penalty",
                   "modulate.by.distinct", "distinctiveness","r2","mse","spearman");

combo.summary <- as.data.frame(combo.summary);

combo.summary$combo <- seq(1:length(combo.summary[,1]));





### Now plot various subsets of the parameter settings

# plot.best.overall.no.decay();
# plot.individual.no.decay();
# plot.best.overall.no.decay.no.mp();
# plot.individual.no.decay.no.mp();

# plot.best.overall.decay();
# plot.individual.decay();
# plot.best.overall.decay.no.mp();
# plot.individual.decay.no.mp();

# plot.full.range.no.decay();
# plot.full.range.decay();
