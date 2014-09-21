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
###
#########################################################################################################

library(reshape);
library(gplots);


source("run-model.r");
source("model-data-plots.r");

history <- NULL;

## Experiment definitions


obj.subj.rc.embV <- list(name = "ORC vs. SRC - embedded verb",
                description ="Object vs Subject Relative Clauses - embedded verb",
                conditions = list(

                  ## "the reporter who SENT the photographer to the editor hoped..."                  
                  list(condition = "SRC.embV",
                       num.experimental.subjects = 40,
                       num.experimental.items = 10,
                       retrievals = "retrievals-subj-rel.txt",
                       items = "items-subj-rel.txt",
                       data = 353,
                       # measure="percent error",
                       measure="latency",
                       procedural.duration = 100,
                       correct.item = 2,
                       distractor.item = 1,
                       critical.retrieval = 2),

                  ## "the reporter who the photographer SENT to the editor hoped..."                  
                  list(condition = "ORC.embV",
                       num.experimental.subjects = 40,
                       num.experimental.items = 10,                                              
                       retrievals = "retrievals-obj-rel.txt",
                       items = "items-obj-rel.txt",
                       data = 420,
                       # measure="percent error",
                       measure="latency",
                       procedural.duration = 150,
                       correct.item = 2,
                       distractor.item = 1, 
                       critical.retrieval = 2:3)
                  ));

obj.subj.rc.mainV <- list(name = "ORC vs. SRC - main verb",
                description ="Object vs Subject Relative Clauses - main verb",
                conditions = list(

                  ## "the reporter who sent the photographer to the editor HOPED..."
                  list(condition = "SRC.mainV",
                       num.experimental.subjects = 40,
                       num.experimental.items = 10,
                       retrievals = "retrievals-subj-rel.txt",
                       items = "items-subj-rel.txt",
                       data = 400,
                       # measure="percent error",
                       measure="latency",
                       procedural.duration = 100,
                       correct.item = 1,
                       distractor.item = 4,
                       critical.retrieval = 3),

                  ## "the reporter who the photographer sent to the editor HOPED..."
                  list(condition = "ORC.mainV",
                       num.experimental.subjects = 40,
                       num.experimental.items = 10,                                              
                       retrievals = "retrievals-obj-rel.txt",
                       items = "items-obj-rel.txt",
                       data = 404,
                       # measure="percent error",
                       measure="latency",
                       procedural.duration = 100,
                       correct.item = 1,
                       distractor.item = 3, 
                       critical.retrieval = 4)
                  ));


## Complete list of experiments

experiments <- list(obj.subj.rc.embV, obj.subj.rc.mainV);

num.experiments <- length(experiments);


## number of monte carlo trials per experiment

default.trials <- 1000;
simulated.experiments <- 50;


## Discrete parameter space to search.  Each parameter now contains a list
## of possible values rather than a single value.  

## Latency factor
F <- c(0.2)
#F <- c(0.1, 0.15, 0.2);

## Extra category penalty
cat.penalty <- c(-999);
#cat.penalty <- c(-999, -3);


## Total source activation
G <- c(1.0);

## Activation noise parameter for logistic distribution
ans  <- c(0.15)
#ans  <- c(0.15, 0.2, 0.25, 0.275, 0.3)

## Fan parameter
mas <- c(1.5)
#mas <- c(1.5, 2.0, 2.5, 3.0)


## Base level decay parameter
d <- c(0.5);
#d <- c(0.001,0.5);


## Match penalty
match.penalty <- c(0)
#match.penalty <- c(-1.5)
#match.penalty <- c(0,-0.1, -0.2, -0.3, -0.4, -0.5)


## VAR mismatch penalty
var.mismatch.penalty <- c(FALSE);


## Additional fan associated with VAR retrieval cues
VAR.fan <- c(0);


## Distinctiveness parameters.  Note that the quantitative parameter has no
## effect when modulate.by.distinct is FALSE. So this is some  wasted
## effort in the simple code below.
modulate.by.distinct <- c(FALSE);
distinctiveness <- c(0);


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
                                   procedural.duration = rep(cond$procedural.duration, num.combinations),
                                   critical.retrieval = rep(toString(cond$critical.retrieval), num.combinations),
                                   correct.item = rep(cond$correct.item, num.combinations),
                                   distractor.item = rep(cond$distractor.item, num.combinations),
                                   data = rep(cond$data, num.combinations),
#                                   data.lower = rep(cond$data - cond$data.se, num.combinations),
#                                   data.upper = rep(cond$data + cond$data.se, num.combinations),
                                   model = rep(NA, num.combinations),
                                   model.lower = rep(NA, num.combinations),
                                   model.upper = rep(NA, num.combinations)
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
pdf(file="activation-plots.pdf",width=11,height=5);


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

 # results <- run.model.quietly();
  results <- run.model(quiet=FALSE);

  ## plot the activation profiles for the critical and distractor items

    clrs <- c("black", "green","blue","orange", "brown");

      plot.activation(moments, history, this.run$correct.item,
                       this.run$distractor.item,
                       this.run$experiment, this.run$condition);
  

  ## now extract the relevant measure

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

  
}

dev.off();







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

plot.best.overall.no.decay();
plot.individual.no.decay();
plot.best.overall.no.decay.no.mp();
plot.individual.no.decay.no.mp();

plot.best.overall.decay();
plot.individual.decay();
plot.best.overall.decay.no.mp();
plot.individual.decay.no.mp();


plot.full.range.no.decay();
plot.full.range.decay();
