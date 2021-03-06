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
###  21 Sep 2014
###    + Implemented measure "latency"
###    + Added possibility to specify multiple retrievals for critical.retrieval with measure "latency"
###      (result will be sum of latencies)
###    + Added possibility to specify procedural.duration with measure "latency"
###      (result will be sum of latency and proc. duration)
###  4. Oct. 2014
###    + Implemented output of standard error
###
#########################################################################################################

silent.mode <<- TRUE
use.standard.error <<- TRUE

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

default.trials <- 100;
simulated.experiments <- 50;


## Discrete parameter space to search.  Each parameter now contains a list
## of possible values rather than a single value.  

## Latency factor
# F <- c(0.14)
F <- c(0.14, 0.2, 0.5, 0.7);

## Extra category penalty
cat.penalty <- c(-999);
#cat.penalty <- c(-999, -3);


## Total source activation
G <- c(1.0);
# G <- c(0.75, 1.0, 1.25);

## Activation noise parameter for logistic distribution
ans  <- c(0.15)
# ans  <- c(0.15, 0.2)

## Fan parameter
mas <- c(1.5)
# mas <- c(1.5, 2.0)
# mas <- c(1.5, 2.0, 2.5, 3.0)


## Base level decay parameter
d <- c(0.5);
# d <- c(0.001, 0.5);


## Match penalty
# match.penalty <- c(0)
match.penalty <- c(-1.5)
# match.penalty <- c(0, -1.5, -2)


## VAR mismatch penalty
var.mismatch.penalty <- c(FALSE);


## Additional fan associated with VAR retrieval cues
VAR.fan <- c(0);


## Distinctiveness parameters.  Note that the quantitative parameter has no
## effect when modulate.by.distinct is FALSE. So this is some  wasted
## effort in the simple code below.
modulate.by.distinct <- c(FALSE);
distinctiveness <- c(0);



source("run-experiments.r")
save(results, all.runs, param.results, combo.summary, num.parameters, num.combinations, total.conditions, total.runs, file="simulation-results.RData")


### Now plot various subsets of the parameter settings

# plot.best.overall.no.decay();
# plot.individual.no.decay();
# plot.best.overall.no.decay.no.mp();
# plot.individual.no.decay.no.mp();
# plot.best.overall.decay.no.mp();
# plot.individual.decay.no.mp();

# plot.individual.decay();
# plot.full.range.no.decay();
# plot.full.range.decay();

plot.best.overall.decay();

### Results
results
all.runs
param.results
combo.summary
subset(combo.summary, mse==min(mse))


## Plot all combos:
plot.all(prefix="plots/")


## Plot combos with G==1:
p <- combo.summary$combo[combo.summary$G==1]
plot.all(combos=p, expnum=2, prefix="plots/")

## Export data:
predictions <- all.runs[,c(13,23:27)]
all.latencies <- lapply(predictions$all.crit.latencies, function(x) as.numeric(unlist(strsplit(toString(x),","))))
write.table(predictions, "predictions.txt")

## Export combo 1:
head(all.runs)
exp <- experiments[[1]]$name
combo <- 1
index <- seq(from=combo, to=total.runs, by=num.combinations);
predictions <- all.runs[index,c(13,23:26)]



