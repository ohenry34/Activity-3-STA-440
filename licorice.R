library(tidyverse)
licorice <- read_csv("licorice.csv", show_col_types = F)


# Demographic -------------------------------------------------------------



demographic <- cbind(licorice %>%
  group_by(treat) %>%
  summarize(mean_age = round(mean(preOp_age)),
            sd_age = round(sd(preOp_age)),
            `Gender (female), %` = round(100*mean(preOp_gender)),
            mean_bmi = round(mean(preOp_calcBMI)),
            sd_bmi = round(sd(preOp_calcBMI)),
            mean_pain = round(100*mean(preOp_pain))),
  licorice %>%
    group_by(treat) %>%
    count(preOp_smoking) %>%
    pivot_wider(names_from = preOp_smoking,
              names_glue = "preOp_smoking{preOp_smoking}",
              values_from = n),

licorice %>%
  group_by(treat) %>%
  count(preOp_mallampati) %>%
  pivot_wider(names_from = preOp_mallampati,
              names_glue = "preOp_mallampati{preOp_mallampati}",
              values_from = n) %>%
  mutate(preOp_mallampati4 = ifelse(is.na(preOp_mallampati4), 0, preOp_mallampati4)),

licorice %>%
  group_by(treat) %>%
  count(preOp_asa) %>%
  pivot_wider(names_from = preOp_asa,
              names_glue = "preOp_asa{preOp_asa}",
              values_from = n),

licorice %>%
  group_by(treat) %>%
  count(intraOp_surgerySize) %>%
  pivot_wider(names_from = intraOp_surgerySize,
              names_glue = "intraOp_surgerySize{intraOp_surgerySize}",
              values_from = n)) %>%
  select(-treat) %>%
  mutate(treat = c(0, 1)) %>%
  select(treat, everything()) %>%
  mutate(`Age, yr` = paste0(mean_age, " +/- ", sd_age), 
         BMI = paste0(mean_bmi, " +/- ", sd_bmi)) %>%
  select(-mean_age, -sd_age, -mean_bmi, -sd_bmi) %>%
  select(`Age, yr`, `Gender (female), %`, BMI, everything())


demographic <- demographic %>%
  remove_rownames %>% column_to_rownames(var="treat")

demographic <- as_tibble(cbind(treat = names(demographic), t(demographic))) %>%
  remove_rownames %>% column_to_rownames(var="treat")

demographic <- demographic %>% select(`1`, `0`)



# Cough -------------------------------------------------------------------



cough <- rbind(licorice %>%
                       filter(!is.na(extubation_cough)) %>%
                       mutate(extubation_cough = ifelse(extubation_cough == 0, 0, 1)) %>%
                       group_by(treat, extubation_cough) %>%
                       summarise(count = n()) %>%
                       mutate(prop = round(100*count/sum(count))) %>%
                       filter(extubation_cough == 1) %>%
                       pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                       pivot_longer(extubation_cough, names_to = "result") %>%
                       select(-value),
                     
                     licorice %>%
                       filter(!is.na(pacu30min_cough)) %>%
                       mutate(pacu30min_cough = ifelse(pacu30min_cough == 0, 0, 1)) %>%
                       group_by(treat, pacu30min_cough) %>%
                       summarise(count = n()) %>%
                       mutate(prop = round(100*count/sum(count))) %>%
                       filter(pacu30min_cough == 1) %>%
                       pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                       pivot_longer(pacu30min_cough, names_to = "result") %>%
                       select(-value),
                     
                     licorice %>%
                       filter(!is.na(pacu90min_cough)) %>%
                       mutate(pacu90min_cough = ifelse(pacu90min_cough == 0, 0, 1)) %>%
                       group_by(treat, pacu90min_cough) %>%
                       summarise(count = n()) %>%
                       mutate(prop = round(100*count/sum(count))) %>%
                       filter(pacu90min_cough == 1) %>%
                       pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                       pivot_longer(pacu90min_cough, names_to = "result") %>%
                       select(-value),
               
               licorice %>%
                 filter(!is.na(postOp4hour_cough)) %>%
                 mutate(postOp4hour_cough = ifelse(postOp4hour_cough == 0, 0, 1)) %>%
                 group_by(treat, postOp4hour_cough) %>%
                 summarise(count = n()) %>%
                 mutate(prop = round(100*count/sum(count))) %>%
                 filter(postOp4hour_cough == 1) %>%
                 pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                 pivot_longer(postOp4hour_cough, names_to = "result") %>%
                 select(-value),
               
               licorice %>%
                 filter(!is.na(pod1am_cough)) %>%
                 mutate(pod1am_cough = ifelse(pod1am_cough == 0, 0, 1)) %>%
                 group_by(treat, pod1am_cough) %>%
                 summarise(count = n()) %>%
                 mutate(prop = round(100*count/sum(count))) %>%
                 filter(pod1am_cough == 1) %>%
                 pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                 pivot_longer(pod1am_cough, names_to = "result") %>%
                 select(-value)) %>%
  mutate(result = case_when(
    str_detect(result, "ext") ~ "Immediately after extubation",
    str_detect(result, "30min") ~ "0.5 h",
    str_detect(result, "90min") ~ "1.5 h",
    str_detect(result, "4hour") ~ "4 h",
    T ~ "Morning after"
  ),
  values_0 = paste0(count_0, " (", prop_0, ")"),
  values_1 = paste0(count_1, " (", prop_1, ")")) %>%
  select(result, values_1, values_0)

cough0 <- licorice %>%
  filter(!is.na(extubation_cough)) %>%
  mutate(extubation_cough = ifelse(extubation_cough == 0, 0, 1)) %>%
  group_by(treat, extubation_cough) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = extubation_cough, values_from = count,
              names_glue = "extubation_cough{extubation_cough}") %>%
  column_to_rownames(var = "treat")

cough30 <- licorice %>%
  filter(!is.na(pacu30min_cough)) %>%
  mutate(pacu30min_cough = ifelse(pacu30min_cough == 0, 0, 1)) %>%
  group_by(treat, pacu30min_cough) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pacu30min_cough, values_from = count,
              names_glue = "pacu30min_cough{pacu30min_cough}") %>%
  column_to_rownames(var = "treat")

cough90 <- licorice %>%
  filter(!is.na(pacu90min_cough)) %>%
  mutate(pacu90min_cough = ifelse(pacu90min_cough == 0, 0, 1)) %>%
  group_by(treat, pacu90min_cough) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pacu90min_cough, values_from = count,
              names_glue = "pacu90min_cough{pacu90min_cough}") %>%
  column_to_rownames(var = "treat")

cough4 <- licorice %>%
  filter(!is.na(postOp4hour_cough)) %>%
  mutate(postOp4hour_cough = ifelse(postOp4hour_cough == 0, 0, 1)) %>%
  group_by(treat, postOp4hour_cough) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = postOp4hour_cough, values_from = count,
              names_glue = "postOp4hour_cough{postOp4hour_cough}") %>%
  column_to_rownames(var = "treat")

cough1 <- licorice %>%
  filter(!is.na(pod1am_cough)) %>%
  mutate(pod1am_cough = ifelse(pod1am_cough == 0, 0, 1)) %>%
  group_by(treat, pod1am_cough) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pod1am_cough, values_from = count,
              names_glue = "pod1am_cough{pod1am_cough}") %>%
  column_to_rownames(var = "treat")

cough <- cbind(cough, p_values = 
                       c(chisq.test(cough0)$p.value,
                         chisq.test(cough30)$p.value,
                         chisq.test(cough90)$p.value,
                         chisq.test(cough4)$p.value,
                         chisq.test(cough1)$p.value)) %>%
  mutate(p_values = scales::pvalue(p_values))


# Throat Pain -------------------------------------------------------------




throat_pain <- rbind(licorice %>%
  filter(!is.na(pacu30min_throatPain)) %>%
  mutate(pacu30min_throatPain = ifelse(pacu30min_throatPain == 0, 0, 1)) %>%
  group_by(treat, pacu30min_throatPain) %>%
  summarise(count = n()) %>%
  mutate(prop = round(100*count/sum(count))) %>%
  filter(pacu30min_throatPain == 1) %>%
  pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
  pivot_longer(pacu30min_throatPain, names_to = "result") %>%
  select(-value),

licorice %>%
  filter(!is.na(pacu90min_throatPain)) %>%
  mutate(pacu90min_throatPain = ifelse(pacu90min_throatPain == 0, 0, 1)) %>%
  group_by(treat, pacu90min_throatPain) %>%
  summarise(count = n()) %>%
  mutate(prop = round(100*count/sum(count))) %>%
  filter(pacu90min_throatPain == 1) %>%
  pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
  pivot_longer(pacu90min_throatPain, names_to = "result") %>%
  select(-value),

licorice %>%
  filter(!is.na(postOp4hour_throatPain)) %>%
  mutate(postOp4hour_throatPain = ifelse(postOp4hour_throatPain == 0, 0, 1)) %>%
  group_by(treat, postOp4hour_throatPain) %>%
  summarise(count = n()) %>%
  mutate(prop = round(100*count/sum(count))) %>%
  filter(postOp4hour_throatPain == 1) %>%
  pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
  pivot_longer(postOp4hour_throatPain, names_to = "result") %>%
  select(-value),

licorice %>%
  filter(!is.na(pod1am_throatPain)) %>%
  mutate(pod1am_throatPain = ifelse(pod1am_throatPain == 0, 0, 1)) %>%
  group_by(treat, pod1am_throatPain) %>%
  summarise(count = n()) %>%
  mutate(prop = round(100*count/sum(count))) %>%
  filter(pod1am_throatPain == 1) %>%
  pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
  pivot_longer(pod1am_throatPain, names_to = "result") %>%
  select(-value)) %>%
  mutate(result = case_when(
    str_detect(result, "30min") ~ "0.5 h",
    str_detect(result, "90min") ~ "1.5 h",
    str_detect(result, "4hour") ~ "4 h",
    T ~ "Morning after"
  ),
  values_0 = paste0(count_0, " (", prop_0, ")"),
  values_1 = paste0(count_1, " (", prop_1, ")")) %>%
  select(result, values_1, values_0)




throat30 <- licorice %>%
  filter(!is.na(pacu30min_throatPain)) %>%
  mutate(pacu30min_throatPain = ifelse(pacu30min_throatPain == 0, 0, 1)) %>%
  group_by(treat, pacu30min_throatPain) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pacu30min_throatPain, values_from = count,
              names_glue = "pacu30min_throatPain{pacu30min_throatPain}") %>%
  column_to_rownames(var = "treat")

throat90 <- licorice %>%
  filter(!is.na(pacu90min_throatPain)) %>%
  mutate(pacu90min_throatPain = ifelse(pacu90min_throatPain == 0, 0, 1)) %>%
  group_by(treat, pacu90min_throatPain) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pacu90min_throatPain, values_from = count, 
              names_glue = "pacu90min_throatPain{pacu90min_throatPain}") %>%
  column_to_rownames(var = "treat")

throat4 <- licorice %>%
  filter(!is.na(postOp4hour_throatPain)) %>%
  mutate(postOp4hour_throatPain = ifelse(postOp4hour_throatPain == 0, 0, 1)) %>%
  group_by(treat, postOp4hour_throatPain) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = postOp4hour_throatPain, values_from = count,
              names_glue = "postOp4hour_throatPain{postOp4hour_throatPain}")  %>%
  column_to_rownames(var = "treat")

throat1 <- licorice %>%
  filter(!is.na(pod1am_throatPain)) %>%
  mutate(pod1am_throatPain = ifelse(pod1am_throatPain == 0, 0, 1)) %>%
  group_by(treat, pod1am_throatPain) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = pod1am_throatPain, values_from = count,
              names_glue = "pod1am_throatPain{pod1am_throatPain}")  %>%
  column_to_rownames(var = "treat")

throat_pain <- cbind(throat_pain, p_values = 
c(chisq.test(throat30)$p.value,
  chisq.test(throat90)$p.value,
  chisq.test(throat4)$p.value,
  chisq.test(throat1)$p.value)) %>%
  mutate(p_values = scales::pvalue(p_values))

pp <- c(round(epitools::riskratio(as.matrix(throat30))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat90))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat4))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat1))$measure[[2]], 2))

throat30$pacu30min_throatPain0[[1]] <- throat30$pacu30min_throatPain0[[1]] + 1
throat90$pacu90min_throatPain0[[1]] <- throat90$pacu90min_throatPain0[[1]] + 1
throat4$postOp4hour_throatPain0[[1]] <- throat4$postOp4hour_throatPain0[[1]] + 1
throat1$pod1am_throatPain0[[1]] <- throat1$pod1am_throatPain0[[1]] + 1

throat30$pacu30min_throatPain1[[2]] <- throat30$pacu30min_throatPain1[[2]] + 1
throat90$pacu90min_throatPain1[[2]] <- throat90$pacu90min_throatPain1[[2]] + 1
throat4$postOp4hour_throatPain1[[2]] <- throat4$postOp4hour_throatPain1[[2]] + 1
throat1$pod1am_throatPain1[[2]] <- throat1$pod1am_throatPain1[[2]] + 1

itt <- c(round(epitools::riskratio(as.matrix(throat30))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat90))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat4))$measure[[2]], 2),
round(epitools::riskratio(as.matrix(throat1))$measure[[2]], 2))

rr_throat <- tibble("Sore throat at rest" = c("0.5 h", "1.5 h", "4 h", "Morning after"),
                    "ITT" = itt, `Per-protocol` = pp)

# Swallow Pain ------------------------------------------------------------



secondary <- rbind(licorice %>%
                       filter(!is.na(pacu30min_swallowPain)) %>%
                       mutate(pacu30min_swallowPain = 
                                ifelse(pacu30min_swallowPain == 0, 0, 1)) %>%
                       group_by(treat, pacu30min_swallowPain) %>%
                       summarise(count = n()) %>%
                       mutate(prop = round(100*count/sum(count))) %>%
                       filter(pacu30min_swallowPain == 1) %>%
                       pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
                       pivot_longer(pacu30min_swallowPain, names_to = "result") %>%
                       select(-value)%>%
  mutate(result = case_when(
    str_detect(result, "30min") ~ "Sore throat swallowing (0.5 h)"
  ),
  values_0 = paste0(count_0, " (", prop_0, ")"),
  values_1 = paste0(count_1, " (", prop_1, ")")) %>%
  select(result, values_1, values_0),
  
  
  licorice %>%
    filter(!is.na(pod1am_throatPain)) %>%
    mutate(pod1am_throatPain = ifelse(pod1am_throatPain == 0, 0, 1)) %>%
    group_by(treat, pod1am_throatPain) %>%
    summarise(count = n()) %>%
    mutate(prop = round(100*count/sum(count))) %>%
    filter(pod1am_throatPain == 1) %>%
    pivot_wider(names_from = treat, values_from = c(count, prop)) %>%
    pivot_longer(pod1am_throatPain, names_to = "result") %>%
    select(-value) %>%
    mutate(result = "Sore throat at rest (Morning after)",
           values_0 = paste0(count_0, " (", prop_0, ")"),
           values_1 = paste0(count_1, " (", prop_1, ")")) %>%
    select(result, values_1, values_0))





# Sore Throat Score -------------------------------------------------------


score <- licorice %>%
  filter(!is.na(pacu30min_throatPain)) %>%
  group_by(treat) %>%
  summarise(mean_30pain = mean(pacu30min_throatPain),
            sd_30pain = sd(pacu30min_throatPain),
            mean_30pain_swallow = mean(pacu30min_swallowPain),
            sd_30pain_swallow = sd(pacu30min_swallowPain),
            mean_90pain = mean(pacu90min_throatPain),
            sd_90pain = sd(pacu90min_throatPain),
            mean_4pain = mean(postOp4hour_throatPain),
            sd_4pain = sd(postOp4hour_throatPain),
            mean_1morn = mean(pod1am_throatPain),
            sd_1morn = sd(pod1am_throatPain)) %>%
  mutate(across(-treat, round, 2)) %>%
  select(-treat) %>%
  mutate(treat = c(0, 1)) %>%
  select(treat, everything()) %>%
  mutate(`0.5 h (at rest)` = paste0(mean_30pain, " +/- ", sd_30pain),
         `0.5 h (swallowing)` = paste0(mean_30pain_swallow, " +/- ", sd_30pain_swallow),
         `1.5 h (at rest)` = paste0(mean_90pain, " +/- ", sd_90pain),
         `4 h (at rest)` = paste0(mean_4pain, " +/- ", sd_4pain),
         `Morning after (at rest)` = paste0(mean_1morn, " +/- ", sd_1morn)) %>%
  select(-mean_30pain, -sd_30pain, -mean_30pain_swallow, -sd_30pain_swallow,
         -mean_90pain, -sd_90pain, -mean_4pain, -sd_4pain, -mean_1morn, -sd_1morn)


score <- score %>%
  remove_rownames %>% column_to_rownames(var="treat")

score <- as_tibble(cbind(treat = names(score), t(score))) %>%
  remove_rownames %>% column_to_rownames(var="treat")

score <- score %>% select(`1`, `0`)


x <- licorice %>%
  filter(!is.na(pacu30min_throatPain)) %>%
  group_by(treat) %>%
  summarise(mean_30pain = mean(pacu30min_throatPain),
            sd_30pain = sd(pacu30min_throatPain),
            mean_30pain_swallow = mean(pacu30min_swallowPain),
            sd_30pain_swallow = sd(pacu30min_swallowPain),
            mean_90pain = mean(pacu90min_throatPain),
            sd_90pain = sd(pacu90min_throatPain),
            mean_4pain = mean(postOp4hour_throatPain),
            sd_4pain = sd(postOp4hour_throatPain),
            mean_1morn = mean(pod1am_throatPain),
            sd_1morn = sd(pod1am_throatPain))

x[3,] <- x[2,]-x[1,]
x <- x[3,] %>% select(contains("mean"))
score <- cbind(score, `Mean difference` = t(x)) %>%
  mutate(`Mean difference` = round(`Mean difference`, 2))
