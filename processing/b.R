


base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  mutate(prop = prop.table(total)*100)

base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(sexo)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, sexo) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(sexo) %>% 
  mutate(prop = prop.table(total)*100)


base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(ciuo08)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, ciuo08) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(ciuo08) %>% 
  mutate(prop = prop.table(total)*100)

base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(rama)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, rama) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(rama) %>% 
  mutate(prop = prop.table(total)*100)


base_pond %>% 
  filter(asal_priv == 1 & formal == 1 & !is.na(hab_t_dos) & !is.na(tam_emp)) %>% 
  group_by(ano_trimestre, mes_central, hab_t_dos, tam_emp) %>% 
  summarise(total = survey_total(na.rm = T, vartype = "ci")) %>% 
  ungroup() %>%
  group_by(tam_emp) %>% 
  mutate(prop = prop.table(total)*100)
