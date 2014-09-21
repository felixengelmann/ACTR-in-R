# README #

Simple Memory Retrieval Model based on ACT-R theory originally developed by Rick Lewis & William Badecker (rickl@umich.edu).
With HTML trace output and PDF activation plots.

Extended by Felix Engelmann (felix.engelmann@uni-potsdam.de):
 * Implemented measure "latency"
 * Added possibility to specify multiple retrievals for critical.retrieval with measure "latency" (result will be sum of latencies)
 * Added possibility to specify procedural.duration with measure "latency" (result will be sum of latency and proc. duration)

### New possibilities in experiment definition: ###
measure = "latency",
procedural.duration = 100,
critical.retrieval = c(2, 3)