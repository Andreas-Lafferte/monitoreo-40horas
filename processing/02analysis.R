

proc_db[[1]] %>% 
  select(anotrim, media, media_low, media_upp) %>% 
  mutate(anotrim = factor(anotrim, levels = anotrim)) %>% 
  ggplot(aes(x = anotrim, y = media, group = 1)) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_ribbon(aes(ymin = media_low, ymax = media_upp), alpha = 0.3, fill = "orange") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))




proc_db[[2]] %>% 
  select(anotrim, seg_ocup, media, media_low, media_upp) %>% 
  pivot_wider(id_cols = anotrim, 
              names_from = seg_ocup,
              values_from = c(media, media_low, media_upp)) %>% 
  mutate(anotrim = factor(anotrim, levels = anotrim)) %>% 
  pivot_longer(cols= -anotrim,
               names_pattern = "(.*)(0|1)$",
               names_to = c(".value", "names")) %>% 
  rename_with(~str_remove(., "_*$")) %>% 
  ggplot(aes(x = anotrim, y = media, group = names, color = names)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = media_low, ymax = media_upp), alpha = 0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))



proc_db[[8]] %>% 
  select(anotrim, rama, media, media_low, media_upp) %>%
  na.omit() %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = media, group = 1, color = "darkorange")) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = media_low, ymax = media_upp), alpha = 0.3, fill = "orange") +
  facet_wrap(~rama) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))


proc_db[[14]] %>% 
  select(anotrim, hab_t_dos, sexo, total) %>%
  na.omit() %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = total, group = as.factor(sexo), color = as.factor(sexo))) +
  geom_line(linewidth = 1) +
  facet_wrap(~hab_t_dos) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))




proc_db[[17]] %>% 
  select(anotrim, hab_t_dos, tam_emp, total) %>%
  na.omit() %>% 
  mutate(anotrim = factor(anotrim, levels = unique(anotrim))) %>% 
  ggplot(aes(x = anotrim, y = total, group = tam_emp, color = tam_emp)) +
  geom_line(linewidth = 1) +
  facet_wrap(~hab_t_dos) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   colour = "#4a4a4a",
                                   size = rel(0.8)))
