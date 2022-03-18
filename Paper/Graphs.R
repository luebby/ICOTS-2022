library(ggplot2)
library(ggdag)

# Experiment
co <- data.frame(x = c(0,0,1), y = c(1,0,0), name = c("Z", "X", "Y")) 

dag_e1<-dagify(Y ~ X,
               X ~ Z,
               Y ~ Z, coords = co) %>% 
  ggdag(node_size = 20, text_size = 8, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(15, "pt"), type = "closed"))  + 
  geom_text(label = "Z - Prior knowledge\nX - Learning time\nY - Test score", 
            hjust = 1, vjust = 1,
            x = 1, y = 1, size = 7, color = "darkgrey") +
  labs(title = "Observational study")

co <- data.frame(x = c(0,0,1,-1), y = c(1,0,0,0), name = c("Z", "X", "Y", "R")) 

dag_e2<-dagify(Y ~ X,
               X ~ Z,
               X ~ R,
               Y ~ Z, coords = co) %>% 
  ggdag(node_size = 20, text_size = 8, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(15, "pt"), type = "closed"))  + 
  geom_segment(aes(x = -.1, y = .475, xend = .1, yend = .575), color = "darkgrey") +
  geom_segment(aes(x = -.1, y = .425, xend = .1, yend = .525), color = "darkgrey") +
  geom_text(label = "Z - Prior knowledge\nX - Learning time\nY - Test score\nR - Researcher", 
            hjust = 0, vjust = 1,
            x = -1.15, y = 0.85, size = 7, color = "darkgrey") +
  labs(title = "Randomised trial")

# Stichprobe

co <- data.frame(x = c(0.5,0,1), y = c(1,0,0), name = c("Z", "S", "X")) 

dag_s1<-dagify(S ~ Z,
               X ~ Z,
               coords = co) %>% 
  ggdag(node_size = 20, text_size = 8, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(15, "pt"), type = "closed"))  + 
  geom_text(label = "Z - Conscientiousness\nS - Sample\nX - Learning time", 
            hjust = 0, vjust = 0,
            x = 0.15, y = 0, size = 7, color = "darkgrey") +
  labs(title = "Convenience sample")

co <- data.frame(x = c(0.5,0,1,-1), y = c(1,0,0,0), name = c("Z", "S", "X", "R")) 

dag_s2<-dagify(X ~ Z,
               S ~ Z,
               S ~ R, coords = co) %>% 
  ggdag(node_size = 20, text_size = 8, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(15, "pt"), type = "closed"))  + 
  geom_segment(aes(x = .15, y = .475, xend = .35, yend = .575), color = "darkgrey") +
  geom_segment(aes(x = .15, y = .425, xend = .35, yend = .525), color = "darkgrey") +
  geom_text(label = "Z - Conscientiousness\nS - Sample\nX - Learning time\nR - Researcher", 
            hjust = 0, vjust = 1,
            x = -1.1, y = 0.75, size = 7, color = "darkgrey") + 
  labs(title = "Random sample")

