# README #

Simple Memory Retrieval Model based on ACT-R theory, specifically for interference and distance phenomena in sentence processing. Originally developed by Rick Lewis & William Badecker (rickl@umich.edu).

Extended by Felix Engelmann (felix.engelmann@uni-potsdam.de):

* Implemented measure "latency"
* Added possibility to specify multiple retrievals for critical.retrieval with measure "latency" (result will be sum of latencies)
* Added possibility to specify procedural.duration with measure "latency" (result will be sum of latency and proc. duration)
* Implemented output of standard error

### New possibilities in experiment definition: ###

```r
measure = "latency",
procedural.duration = 100,
critical.retrieval = c(2, 3)
```

### New global variable: ###

```r
use.standard.error <<- TRUE
```
