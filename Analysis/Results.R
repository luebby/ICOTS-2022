# Start package
library(mosaic)
library(likert)
# Set RNG
set.seed(1896)

# Aggregated data - see https://docs.google.com/spreadsheets/d/1WHZJzdfY4DJ1cAFbdUvWXgIK6NO9mIyxIUfLI6erRhc/

#############################################################################################################

# Auf einer Internetplattform berichten 10000 Personen von einer positiven Wirkung eines Shampoos gegen  graue Haare (Studie A). 
# Ein Experiment mit 100 zufällig ausgewählten Personen findet keine positive Wirkung des Shampoos (Studie B). 
# Mit den gegebenen Informationen, das Ergebnis welcher Studie ist glaubwürdiger?
#   
# A: Das Ergebnis von Studie A
# B: Das Ergebnis von Studie B
# C: Beide Studien sind gleich glaubwürdig. 

# Pre-Results
preA <- c( 7,26,11)
preB <- c(10,30,21)
preC <- c(15,26,20)
preD <- c(13,20, 7)
preE <- c(11,18, 8)
preF <- c( 3,14, 9)
preG <- c( 3, 6, 4)

# Post-Results
postA <- c(10,33, 6)
postB <- c( 3,45,14)
postC <- c(10,34,12)
postD <- c(14,25, 5)
postE <- c( 9,22, 5)
postF <- c( 6, 9, 5)
postG <- c( 2, 7, 4)

CDK <- data.frame(
  course = c(rep("A", sum(preA)), rep("B", sum(preB)), rep("C", sum(preC)), rep("D", sum(preD)),
             rep("E", sum(preE)), rep("F", sum(preF)), rep("G", sum(preG)),
             rep("A", sum(postA)), rep("B", sum(postB)), rep("C", sum(postC)), rep("D", sum(postD)),
             rep("E", sum(postE)), rep("F", sum(postF)), rep("G", sum(postG))),
  time = c(rep("pre", sum(preA)+sum(preB)+sum(preC)+sum(preD)+sum(preE)+sum(preF)+sum(preG)),
           rep("post", sum(postA)+sum(postB)+sum(postC)+sum(postD)+sum(postE)+sum(postF)+sum(postG))),
  choice = c(rep(c("A","B","C"), times = preA), rep(c("A","B","C"), times = preB), rep(c("A","B","C"), times = preC), rep(c("A","B","C"), times = preD),
             rep(c("A","B","C"), times = preE), rep(c("A","B","C"), times = preF), rep(c("A","B","C"), times = preG),
             rep(c("A","B","C"), times = postA), rep(c("A","B","C"), times = postB), rep(c("A","B","C"), times = postC), rep(c("A","B","C"), times = postD),
             rep(c("A","B","C"), times = postE), rep(c("A","B","C"), times = postF), rep(c("A","B","C"), times = postG))#
  ) %>%
  mutate(time = factor(time, levels = c("pre", "post")))
# Check Results
tally(choice ~ time + course, format = "proportion", data = CDK)
# Overall Results
prop(choice ~ time, success = "B", data = CDK)
effect <- diffprop(choice ~ time, success = "B", data = CDK)
# Simulate (nested) permutation distribution
Nulldist <- do(10000)* diffprop(choice ~ shuffle(time, groups = course), 
                                success = "B", data = CDK)

gf_histogram( ~ diffprop, data = Nulldist, center = 0, nbins = 21) %>%
  gf_vline(xintercept = ~ effect)

# P-Value
prop( ~ diffprop >= effect, data = Nulldist)

# Simple 2-sample test
prop.test(choice ~ time, success = "B", data = CDK, alternative = "less")

#############################################################################################################

# Wie sehr stimmen Sie folgender Aussage zu: 
# "Die Diagramme (Graphen) zur Beschreibung der Datenentstehung sind hilfreich, 
# Konzepte der Datenerhebung (randomisierte Stichprobe und Zuordnung) zu verstehen." 

evalA <- c(12,19,7,4,1)
evalB <- c(12,27,7,9,0)
evalC <- c(9,10,7,3,2)
evalD <- c(5,24,9,1,1)
evalE <- c(8,20,7,0,1)
evalF <- c(3,11,4,1,1)
evalG <- c(4,6,3,0,0)

eval_levels <- c("Fully agree", "Strongly agree", "Partly agree",
                 "Strongly disagree", "Fully disagree")
ECD <- data.frame(
  eval = c(rep(eval_levels, times = evalA), rep(eval_levels, times = evalB), 
           rep(eval_levels, times = evalC), rep(eval_levels, times = evalD),
           rep(eval_levels, times = evalE), rep(eval_levels, times = evalF), 
           rep(eval_levels, times = evalG))
  ) %>%
  mutate(eval = factor(eval, levels = eval_levels, ordered = TRUE)) %>%
#  rename("Graphs are useful for understanding" = eval)
  rename(" " = eval)

ecd_likert <- likert(items = ECD[,1, drop = FALSE]) 
png("Evaluation.png", width = 2000, height = 1000)
# plot(ecd_likert)
size <- 12
plot(ecd_likert,
     legend.position = "bottom",
     panel.background = element_rect(size = NA, color = "grey70", fill = "lightgray"),
     text.size = size) |> 
  gf_labs(title = "Graphs are useful for understanding") |> 
  gf_theme(legend.text = element_text(size = 3 * size), 
           legend.title = element_text(size = 3 * size),
           axis.title.x = element_text(size = 3 * size), 
           axis.text.x = element_text(size = 2.5 * size),
           plot.title = element_text(size = 3.5 * size),
           plot.margin = unit(c(0, 1, 0, -6), "mm")) # t r b l
dev.off()

