library(ggpattern)
library(ggplot2)
library(dplyr)
library(tidyr)


df <- data.frame(start = c(1, 5, 6), 
           end = c(6, 11, 15), 
           type = c("type1", "type2", "type2"),
           id = 1L:3)

p1 <- ggplot(df, aes(xmin = start, xmax = end, 
               ymin = id - 0.25, ymax = id + 0.25)) +
  geom_rect()

p2 <- ggplot(df, aes(xmin = start, xmax = end, 
               ymin = id - 0.25, ymax = id + 0.25, 
               fill = type)) +
  geom_rect() +
  scale_fill_manual(values = c("#D5E8D4", "#F8CECC"))


all_pos <- c(min(df[["start"]]):max(df[["end"]]))
base_colors <- c("#D5E8D4", "#F8CECC") %>% 
  setNames(unique(df[["type"]]))

col_df <- expand.grid(unique(df[["type"]]), unique(df[["type"]])) %>% 
  group_by(Var1, Var2) %>% 
  mutate(name = paste0(unique(c(Var1, Var2)), collapse = "")) %>% 
  mutate(col1 = base_colors[Var1], col2 = base_colors[Var2])

col_vec <- col_df

hm_df <- lapply(unique(df[["type"]]), function(ith_type) {
  min_pos <- min(df[df[["type"]] == ith_type, "start"])
  max_pos <- max(df[df[["type"]] == ith_type, "end"])
  all_pos >= min_pos & all_pos <= max_pos
}) %>% 
  setNames(unique(df[["type"]])) %>% 
  data.frame() %>% 
  mutate(aa_pos = min(df[["start"]]):max(df[["end"]])) %>% 
  pivot_longer(cols = -aa_pos) %>% 
  mutate(type_name = ifelse(value, name, "")) %>% 
  group_by(aa_pos) %>% 
  summarise(type = paste0(type_name, collapse = ""))

ggplot(hm_df, aes(xmin = aa_pos, xmax = aa_pos + 1, 
               ymin = -0.25, ymax = 0.25, 
               fill = type)) +
  geom_rect_pattern(color = "black") + 
  scale_fill_manual(values = c("#D5E8D4", "#F8CECC"))
