

plot.experiment <- function(exp, combo) {
  index <- seq(from=combo, to=total.runs, by=num.combinations);
  runs <- all.runs[index,];
  runs.exp <- runs[runs$experiment==exp,];
  ht <- as.matrix(cbind(runs.exp$data, runs.exp$model));
  measure <- runs.exp$measure[1]

  runs.exp$data.lower <- 0
  runs.exp$data.upper <- 0
  ht.lower <-as.matrix(cbind(runs.exp$data.lower, runs.exp$model.lower));
  ht.upper <-as.matrix(cbind(runs.exp$data.upper, runs.exp$model.upper));

  param.names <- colnames(all.runs)[1:num.parameters];
  param.values <- runs[1,1:num.parameters];
  
  if(measure=="percent error") ylim <- c(0,50) else ylim <- c(0,max(ht)+50)

  barplot2(height=ht,
           bty="n",
          ci.l = ht.lower,
          ci.u = ht.upper,
          ci.color="gray40",
          plot.ci=TRUE,
           # plot.ci=FALSE,
           col=rev(grey.colors(length(runs.exp$condition))),
           ##  legend.text=runs.exp$condition,
           names=c("Data","Retrieval Interference Model"),
           beside=TRUE,
           space = c(0.1, 1),
           ylim = ylim,
           axes=TRUE,
           xlab="", ylab=as.character(runs.exp$measure[1]),
           cex.lab=1.0,
           main=get.description(exp),
           sub=sprintf("lf=%3.2f, noise=%3.2f, mas=%3.2f, mp=%3.2f, d=%3.2f, w=%3.2f", param.values$F, param.values$ans,param.values$mas,
             param.values$match.penalty,param.values$d, param.values$G)
           );
  
  axis(1,labels=FALSE,tcl=0);  ## zero tick length
  legend(x="topright",
         legend=runs.exp$condition,
         fill=rev(grey.colors(length(runs.exp$condition))),
         bty="n");

  
  return(ht);
  
# text(x=2,y=seq(from=38,to=(38-(num.parameters*2)+2),by=-2),labels=param.names,cex=1.0);
# text(x=4,y=seq(from=38,to=(38-(num.parameters*2)+2),by=-2),labels=as.character(param.values),cex=1.0);
}



get.description <- function(exp) {
  for (i in 1:num.experiments) {
    if (experiments[[i]]$name == exp)
      return(experiments[[i]]$description)
  }
  return("No description");
}



plot.best.overall.no.decay <- function() {
  subs <- combo.summary[combo.summary$d==0.001,];
  c <- subs$combo[which.min(subs$mse)];
  
  pdf(file="best-overall-no-decay.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));

  model.points <- c();
  data.points <- c();

  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.001),];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));    
  }
 
  
  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}



plot.best.overall.no.decay.no.mp <- function() {
  subs <- combo.summary[combo.summary$d==0.001 & combo.summary$match.penalty==0,];
  c <- subs$combo[which.min(subs$mse)];
  
  pdf(file="best-overall-no-decay-no-mp.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));

  model.points <- c();
  data.points <- c();

  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.001),];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));    
  }
 
  
  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}


  
plot.individual.no.decay <- function() {
  pdf(file="best-individual-no-decay.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));   # this is a hack to make the Slovak graph wider
  model.points <- c();
  data.points <- c();
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.001),];
    m <- which.min(this.exp$mse);
    c <- this.exp$combo[m];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));        
  }

  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}


  
plot.individual.no.decay.no.mp <- function() {
  pdf(file="best-individual-no-decay-no-mp.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));   # this is a hack to make the Slovak graph wider
  model.points <- c();
  data.points <- c();
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.001) & (param.results$match.penalty==0),];
    m <- which.min(this.exp$mse);
    c <- this.exp$combo[m];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));        
  }

  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}




## Plots best fit across all experiments
plot.best.overall.decay <- function() {
  subs <- combo.summary[combo.summary$d==0.5,];
  c <- subs$combo[which.min(subs$mse)];
  
  pdf(file="best-overall-decay.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));

  model.points <- c();
  data.points <- c();

  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.5),];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));    
  }
 
  
  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}



plot.best.overall.decay.no.mp <- function() {
  subs <- combo.summary[combo.summary$d==0.5 & combo.summary$match.penalty==0,];
  c <- subs$combo[which.min(subs$mse)];
  
  pdf(file="best-overall-decay-no-mp.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));

  model.points <- c();
  data.points <- c();

  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.5),];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));    
  }
 
  
  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}



## Plots best fit for each experiment
plot.individual.decay <- function() {
  pdf(file="best-individual-decay.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));   # this is a hack to make the Slovak graph wider
  model.points <- c();
  data.points <- c();
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.5),];
    m <- which.min(this.exp$mse);
    c <- this.exp$combo[m];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));        
  }

  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}

  
plot.individual.decay.no.mp <- function() {
  pdf(file="best-individual-decay-no-mp.pdf",height=11,width=8.5);
#  par(mfrow=c(3,2));
 par(pin=c(6.5,3.5));   # this is a hack to make the Slovak graph wider
  model.points <- c();
  data.points <- c();
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    this.exp <- param.results[(param.results$experiment==exp) & (param.results$d==0.5) & (param.results$match.penalty==0),];
    m <- which.min(this.exp$mse);
    c <- this.exp$combo[m];
    plotted.points <- plot.experiment(exp, c);
    data.points <- c(data.points, plotted.points[,1]);
    model.points <- c(model.points, plotted.points[,2]);
    par(pin=c(3.5,3.5));        
  }

  plot(model.points ~ data.points,
       xlim=c(0,30),ylim=c(0,30),
       main="Model vs. Data Across 14 Conditions",
       xlab ="Data", ylab="Model");
  lines(x=c(0,30), y=c(0,30),lty=3);
  dev.off();
}





plot.experiment.all.combos <- function(exp,decay=TRUE) {
  ## set up plot
  if (decay) {
    runs.exp <- all.runs[(all.runs$experiment==exp & all.runs$d==0.5),];
  } else {
    runs.exp <- all.runs[(all.runs$experiment==exp & all.runs$d==0.001),];
  }
      
  num.runs <- length(runs.exp[,1]);
  num.conds <- length(unique(runs.exp$condition));
  ## only works if even (e.g. both decay and not decay)
  num.combs <- num.runs/num.conds;

  dummy <- data.frame(condition = 1:num.conds,
                      m = rep(0,num.conds));
  plot(m ~ condition,
       data = dummy,
       type="n",
       xlab="",
       ylab="",
       main = get.description(exp),
       ylim=c(0,40),
       xaxt="n"
       );

  for (combo in 1:num.combs) {
    index <- seq(from=combo, to=num.runs,by=num.combs);
    r <- runs.exp[index,];
    lines(x=1:num.conds, y=r$model,type="l",lty=1,col="grey50",pch=c(20));
  }

  lines(x=1:num.conds, y=r$data,type="o",lty=1,lwd=2,col="black",pch=c(20));
  
  axis(1,labels=FALSE,tcl=0);  ## zero tick length
}



plot.full.range.decay <- function() {
  pdf(file="full-range-decay.pdf",height=10,width=10);
  par(mfrow=c(3,3))
  par(bty="n",xpd=NA);
  par(mgp=c(2.8,1,0));
  par(cex.sub=1.1,font.sub=3);
  par(cex.main=0.9,font.sub=3);  
  par(cex.axis=1.05,cex.lab=1.1);
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    plot.experiment.all.combos(exp,decay=TRUE);
  }
  dev.off();
}


plot.full.range.no.decay <- function() {
  pdf(file="full-range-no-decay.pdf",height=10,width=10);
  par(mfrow=c(3,3))
  par(bty="n",xpd=NA);
  par(mgp=c(2.8,1,0));
  par(cex.sub=1.1,font.sub=3);
  par(cex.main=0.8,font.sub=3);  
  par(cex.axis=1.05,cex.lab=1.1);
  
  for (e in 1:num.experiments) {
    exp <- experiments[[e]]$name;
    plot.experiment.all.combos(exp,decay=FALSE);
  }
  dev.off();
}

