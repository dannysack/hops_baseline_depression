library(plyr)
library(tidyverse)
library(rms)
library(lubridate)
library(readxl)

setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/HoPS+ Baseline/")

# pull in data
raw <- read_csv("2021_09_14_data.csv", 
                col_types = list(demographics_5 = "c",
                                 demographics_5_homem = "c")) %>%
  mutate(demographics_5 = parse_number(demographics_5),
         demographics_5_homem = parse_number(demographics_5_homem))

# make female and male dfs of baseline data
df_female <- raw %>% select(record_id, data_de_inscricao, 
                            patient_id_for_matching, 
                            district, uni_sani_gile:uni_sani_quel,
                            demographics_5:demographics_6,
                            demographics_14:demographics_15a_1,
                            estado_actual_mulher, data_1a_entrevista_mulher,
                            estigma_1:custos_5, codigo) %>% mutate(sex = "Female")

df_male <- raw %>% select(record_id, data_de_inscricao,
                          male_patient_id_for_matching,
                          distrito_h, unidade_sani_gile_h:unidade_sani_quel_h,
                          demographics_5_homem:demographics_6_homem,
                          demographics_14_homem:demographics_15a1_homem,
                          estado_actual_homem, data_1a_entrevista_homem,
                          estigma_1_h_1:custos_5_h, codigo_homem) %>% mutate(sex = "Male")

# rename columns
col_names <- c("record_id", "reg_date", "pt_id", "district", "uni_sani_gile",
               "uni_sani_inha", "uni_sani_mag", "uni_sani_moc", "uni_sani_nam",
               "uni_sani_peb", "uni_sani_quel",
               "age", "age_check", "rel_stat", "edu_text", "edu_cat",
               "job_free", "job_cat", "job_other", "study_status", 
               "baseline_int_date", "stigb1", "stigb2", "stigb3", 
               "stigb4", "stigb5", "stigb6", "stigb7", "stigb8", 
               "stigb9", "stigb10", "stigb11", "stigb12", "stigb13", 
               "stigb14", "stigb15", "stigb16", "stigb17", "stigb18", 
               "stigb19", "stigb20", "trustb1", "trustb2", "trustb3",
               "trustb5", "trustb6", "trustb7", "trustb8",
               "trustb9", "trustb10", "trustb11", "trustb12", "trustb13",
               "trustb14", "trustb15", "trustb16", "trustb17", "trustb18",
               "trustb19", "trustb20", "trustb21", "trustb22", "trustb23",
               "trustb24", "trustb25", "trustb26", "empb1", "empb2", "empb3",
               "empb4", "empb5", "empb6", "empb7", "empb8", "empb9", "empb10",
               "empb11", "empb12", "empb13", "empb14", "empb15", "empb16", 
               "empb17", "empb18", "empb19", "empb20", "empb21", "empb22",
               "empb23", "emb24", "empb25", "empb26", "empb27", "empb28",
               "ssb1", "ssb2", "ssb3", "ssb4", 
               "ssb5", "ssb6", "ssb7", "ssb8", "ssb9", "ssb10", "ssb11", 
               "ssb12", "ssb13", "ssb14", "ssb15", "ssb16", "ssb17", 
               "hkb1", "hkb2", "hkb3", "hkb4", "hkb5", "hkb6", "hkb7", 
               "hkb8", "hkb9", "hkb10", "hkb11", "hkb12", "hkb13", "hkb14", 
               "hkb15", "hkb16", "hkb17", "hkb18", "hkb19", "hkb20", "hkb21", 
               "hkb22", "hkb23", "hkb24", "hkb25", "hkb26", "hkb27", "db1", 
               "db2", "db3", "db4", "db5", "db6", "db7", "db8", "db9", "costb1",
               "costb1a", "costb2", "costb2a", "costb3", "costb3a", "costb4",
               "costb4a", "costb5", "old_pt_id", "sex")

names(df_female) <- names(df_male) <- col_names

# combine rows
df <- rbind(df_female, df_male)

# Intervention sites

# Muiane CS III uni_sani_gile == 1
# Mixixine CS III uni_sani_nam == 2
# Palane-Mucula CS II uni_sani_inha == 3
# Gurai PSA uni_sani_moc == 0
# Muebele PS uni_sani_nam == 3
# Maganja da Costa CS III uni_sani_mag == 0
# Naburi CS III uni_sani_peb == 3
# Nante CS III uni_sani_mag == 1
# Tomeia PSA uni_sani_peb == 6
# 7 de Abril CS III uni_sani_peb == 0
# Pele-Pele CS uni_sani_peb == 5
# Madal CS uni_sani_quel == 0

# Control sites

# Mocubela CS III uni_sani_moc == 1
# Inhassunge CS I uni_sani_inha == 2
# Mugubia CS uni_sani_nam == 4
# Bingagira PS uni_sani_inha == 0
# Gonhane CS III uni_sani_inha == 1
# Gilé CS II uni_sani_gile == 0
# Tapata CS III uni_sani_moc == 2
# Malei PS uni_sani_nam == 0
# Pebane CS II uni_sani_peb == 4
# Magiga PSA uni_sani_peb == 2
# Alto Maganha CS uni_sani_peb == 1
# M'Baua CS III uni_sani_nam == 1

df$group <- NA
# intervention
df$group[df$uni_sani_gile == 1 | df$uni_sani_inha == 3 | 
           df$uni_sani_mag %in% c(0, 1) | df$uni_sani_moc == 0 |
           df$uni_sani_nam %in% c(2, 3) | df$uni_sani_peb %in% c(0, 3, 5, 6) |
           df$uni_sani_quel == 0] <- 1

# control
df$group[df$uni_sani_gile == 0 | df$uni_sani_inha %in% c(0, 1, 2) | 
           df$uni_sani_moc %in% c(1, 2) | df$uni_sani_nam %in% c(0, 1, 4) | 
           df$uni_sani_peb %in% c(1, 2, 4)] <- 0

# add clinic name column
df$clinic <- NA

# Intervention sites

df$clinic[df$uni_sani_gile == 1] <- "Muiane CS III"
df$clinic[df$uni_sani_nam == 2] <- "Mixixine CS III"
df$clinic[df$uni_sani_inha == 3] <- "Palane-Mucula CS II"
df$clinic[df$uni_sani_moc == 0] <- "Gurai PSA"
df$clinic[df$uni_sani_nam == 3] <- "Muebele PS"
df$clinic[df$uni_sani_mag == 0] <- "Maganja da Costa CS III"
df$clinic[df$uni_sani_peb == 3] <- "Naburi CS III"
df$clinic[df$uni_sani_mag == 1] <- "Nante CS III"
df$clinic[df$uni_sani_peb == 6] <- "Tomeia PSA"
df$clinic[df$uni_sani_peb == 0] <- "7 de Abril CS III"
df$clinic[df$uni_sani_peb == 5] <- "Pele-Pele CS"
df$clinic[df$uni_sani_quel == 0] <- "Madal CS"

# Control sites

df$clinic[df$uni_sani_moc == 1] <- "Mocubela CS III"
df$clinic[df$uni_sani_inha == 2] <- "Inhassunge CS I"
df$clinic[df$uni_sani_nam == 4] <- "Mugubia CS"
df$clinic[df$uni_sani_inha == 0] <- "Bingagira PS"
df$clinic[df$uni_sani_inha == 1] <- "Gonhane CS III"
df$clinic[df$uni_sani_gile == 0] <- "Gilé CS II"
df$clinic[df$uni_sani_moc == 2] <- "Tapata CS III"
df$clinic[df$uni_sani_nam == 0] <- "Malei PS"
df$clinic[df$uni_sani_peb == 4] <- "Pebane CS II"
df$clinic[df$uni_sani_peb == 2] <- "Magiga PSA"
df$clinic[df$uni_sani_peb == 1] <- "Alto Maganha CS"
df$clinic[df$uni_sani_nam == 1] <- "M'Baua CS III"

# factor, doesn't matter, just needs to be a factor
df$clinic <- factor(df$clinic)


# remove extra columns
df <- df %>% select(-uni_sani_gile:-uni_sani_quel)

# now work on info about registration date
df <- df %>% mutate(day = reg_date - min(reg_date),
                    year = year(reg_date))

# now need to get to work renaming things...

# district
table(df$district, useNA = "always")
df$district[df$district == 0] <- "Gilé"
df$district[df$district == 1] <- "Inhassunge"
df$district[df$district == 2] <- "Maganja da Costa"
df$district[df$district == 3] <- "Mocubela"
df$district[df$district == 4] <- "Namacurra"
df$district[df$district == 5] <- "Pebane"
df$district[df$district == 6] <- "Quelimane"
table(df$district, useNA = "always")
df$district <- factor(df$district, levels = c("Pebane", "Inhassunge",
                                              "Namacurra", "Mocubela",
                                              "Maganja da Costa", "Gilé",
                                              "Quelimane"))

# age
# if age is NA, then should be age_check
df <- df %>% mutate(age = ifelse(is.na(age), age_check, age))

# rel_stat
table(df$rel_stat, useNA = "always")
df$rel_stat[df$rel_stat == 0] <- "Married"
df$rel_stat[df$rel_stat == 1] <- "Single"
df$rel_stat[df$rel_stat == 2] <- "Living Together"
table(df$rel_stat, useNA = "always")
df$rel_stat <- factor(df$rel_stat, levels = c("Living Together",
                                              "Single", "Married"))

# edu
table(df$edu_text, useNA = "always")
table(df$edu_cat, useNA = "always")
df$edu_cat[df$edu_cat == 0] <- "None"
df$edu_cat[df$edu_cat == 1] <- "Some Primary School (Grades 1-7)"
df$edu_cat[df$edu_cat == 2] <- "Completed Primary School (Grade 7)"
df$edu_cat[df$edu_cat == 3] <- "Some Secondary School (Grades 8-10)"
df$edu_cat[df$edu_cat == 4] <- "Completed Secondary School (Grade 10)"
df$edu_cat[df$edu_cat == 5] <- "College/Higher Education" #"Basic Technical Education"
df$edu_cat[df$edu_cat == 6] <- "College/Higher Education" #"Completed Techincal High School"
df$edu_cat[df$edu_cat == 7] <- "College/Higher Education" # "University"
df$edu_cat[df$edu_cat == 8] <- NA # "Other"
table(df$edu_cat, useNA = "always")
df$edu_cat <- factor(df$edu_cat, levels = c("None",
                                            "Some Primary School (Grades 1-7)", 
                                            "Completed Primary School (Grade 7)",
                                            "Some Secondary School (Grades 8-10)",
                                            "Completed Secondary School (Grade 10)",
                                            #"Basic Technical Education",
                                            #"Completed Techincal High School",
                                            "College/Higher Education"))

# job (which will be more complicated...)
table(df$job_free, useNA = "always")
table(df$job_cat, useNA = "always")

# need to figure out which of the free texts are present in the absense of categories
table(df$job_free[is.na(df$job_cat)], useNA = "always")

# make some buckets
farmer <- c("Campnes", "Camponasa", "Camponenes", "campones", "Campones",
            "CAMPONES", "Camponés", "camponês", "Camponês", "camponesa",
            "Camponesa", "Camponêsa", "camponesa", "comonesa", "Camoponesa", 
            "Campnesa", "Camponeesa", "Camponsa", "Camposa", "Campon~es", 
            "Canpoês", "camponese", "camponês", 
            "Canpnês", "Campnes", "Csmpones", "Agricultor")
domestic <- c("Domesrico", "domestica", "Domestica", "Doméstica", "Domesticado",
              "Domesticas", "Domestico", "Doméstico", "Ddmestica")
trader <- c("Comerciante", "Comerciante de peixe", "Corciante", 
            "Negociante", "Nogociante", "Comprador de cocos", 
            "Vendedor no mercado no Muiane", "Negociante", "Nogociante", 
            "Nogosciante", "Comerisiante", "Comersiante", "Negosiante", 
            "Co erciante", "Comercinte", "Vendedor", "Vendedor do Mercado Local", 
            "Vendedor de roupas", "Vendedor de coco", "Vende carvão", 
            "Vendedor de Peixe", "Comerciante de Coco", "Corciante", 
            "Conprador de Pedras", "negociante", "Ringuista-Negociante")
fisher <- c("Pescador", "PESCADOR", "Prscador", "Pescacor", "Pesador", "pescador")
other <- c("Misionario", "Professor", "Professora", "Taxista",
           "Piloto de Barco", "Taxisista", "Salineiro", "Motosserista", 
           "Alfeate", "Biscateiro", "Mestre de ac", "Serador carpinteiro", 
           "Trabalhador de bagladez", "Madereiro", "Trabalha na infrastrutura", 
           "Agente de serviço", "Carinpeiro", "Pequeno Comerciante local", 
           "Médico tradicional", "Mec^nico", "Mestre de aparelhos sonoros", 
           "Mecánico, Mecanico", "Trabalha para Bagladez",
           "Alfaiate", "Pedreiro", "Moageiro", "Marinheiro",
           "Estudante Mestre de Biscicleta", "Carpinteiro", 
           "Serralheiro", "empregado do balcao", "Pastor", "Sazonal",
           "Trabalhador do porto", "Estudante", "Mestre de Biscicleta", 
           "Mestre de motas e bicicletas")

# now, if "job_cat" is na and "job_free" is not, fill in appropriate job
for(i in 1:nrow(df)){
  if(is.na(df$job_cat[i])){
    if(df$job_free[i] %in% farmer){
      df$job_cat[i] <- 1
    }
    if(df$job_free[i] %in% domestic){
      df$job_cat[i] <- 2
    }
    if(df$job_free[i] %in% trader){
      df$job_cat[i] <- 3
    }
    if(df$job_free[i] %in% fisher){
      df$job_cat[i] <- 4
    }
    if(df$job_free[i] %in% other){
      df$job_cat[i] <- 5
    }
  }
}

# now fix 
df$job_cat[df$job_cat == 1] <- "Farmer"
df$job_cat[df$job_cat == 2] <- "Domestic"
df$job_cat[df$job_cat == 3] <- "Trader"
df$job_cat[df$job_cat == 4] <- "Fisher"
df$job_cat[df$job_cat == 5] <- "Other"
df$job_cat <- factor(df$job_cat, levels = c("Farmer", "Domestic",
                                            "Trader", "Fisher", "Other"))

# study status
table(df$study_status, useNA = "always")
df$study_status[df$study_status == 1] <- "Active"
df$study_status[df$study_status == 2] <- "Died"
df$study_status[df$study_status == 3] <- "Transferred"
df$study_status[df$study_status == 4] <- "Moved out of catchment area"
df$study_status[df$study_status == 5] <- "Informally withdrew"
df$study_status[df$study_status == 6] <- "Formally withdrew"
df$study_status[df$study_status == 7] <- "Completed"
df$study_status <- factor(df$study_status, levels = c("Active", "Completed",
                                                      "Informally withdrew", "Transferred",
                                                      "Died", "Moved out of catchment area",
                                                      "Formally withdrew"))

# stigma (15:34)
# 0-3 are coded correctly!
df[, 15:34][df[, 15:34] == 4] <- NA

# trust
# needs to be coded from 1-5
df[, 35:59][df[, 35:59] == 5] <- NA # go from top to bottom to do so
df[, 35:59][df[, 35:59] == 4] <- 5
df[, 35:59][df[, 35:59] == 3] <- 4
df[, 35:59][df[, 35:59] == 2] <- 3
df[, 35:59][df[, 35:59] == 1] <- 2
df[, 35:59][df[, 35:59] == 0] <- 1

# empathy
# needs to be coded from 0-4
# 0-4 correctly coded!
df[, 60:87][df[, 60:87] == 5] <- NA

# social support
# needs to be coded from 1-4
df[, 88:104][df[, 88:104] == 4] <- NA
df[, 88:104][df[, 88:104] == 3] <- 4
df[, 88:104][df[, 88:104] == 2] <- 3
df[, 88:104][df[, 88:104] == 1] <- 2
df[, 88:104][df[, 88:104] == 0] <- 1

# HIV knowledge
# False: 1, 3, 11, 13:18, 20, 22, 23, 27
# True: 2, 4:10, 12, 19, 21, 24:26
# coded 0 (incorrect or not sure) and 1 (correct)

# first add in NAs
df[, 105:131][df[, 105:131] == 3] <- NA

# now go through and recode things :)
# when the correct answer is True (2, 4:10, 12, 19, 21, 24:26)
# if they said disagree or not sure, they are incorrect
df[,c(106, 108:114, 116, 123, 125, 128:130)][df[,c(106, 108:114, 116, 123, 125, 128:130)] == 0] <- 0
df[,c(106, 108:114, 116, 123, 125, 128:130)][df[,c(106, 108:114, 116, 123, 125, 128:130)] == 1] <- 0
# if they said agree they are correct
df[,c(106, 108:114, 116, 123, 125, 128:130)][df[,c(106, 108:114, 116, 123, 125, 128:130)] == 2] <- 1

# when the correct answer is FALSE (1, 3, 11, 13:18, 20, 22, 23, 27)
# if they said disagree, they are correct (need to make it something odd to work)
df[,c(105, 107, 115, 117:122, 124, 126:127, 131)][df[,c(105, 107, 115, 117:122, 124, 126:127, 131)] == 0] <- 10
# if they said not sure or agree, they are incorrect
df[,c(105, 107, 115, 117:122, 124, 126:127, 131)][df[,c(105, 107, 115, 117:122, 124, 126:127, 131)] == 1] <- 0
df[,c(105, 107, 115, 117:122, 124, 126:127, 131)][df[,c(105, 107, 115, 117:122, 124, 126:127, 131)] == 2] <- 0

# now fix
df[,c(105, 107, 115, 117:122, 124, 126:127, 131)][df[,c(105, 107, 115, 117:122, 124, 126:127, 131)] == 10] <- 1

# depression
# coded from 0-3
df[, 132:140][df[, 132:140] == 4] <- NA

# costs
df$costb1[df$costb1 == 0] <- "None"
df$costb1[df$costb1 == 1] <- "1-2"
df$costb1[df$costb1 == 2] <- "3-5"
df$costb1[df$costb1 == 3] <- "6-10"
df$costb1[df$costb1 == 4] <- "11-16"
df$costb1[df$costb1 == 5] <- ">16"

df$costb2[df$costb2 == 0] <- "None"
df$costb2[df$costb2 == 1] <- "1-2"
df$costb2[df$costb2 == 2] <- "3-5"
df$costb2[df$costb2 == 3] <- "6-10"
df$costb2[df$costb2 == 4] <- "11-16"
df$costb2[df$costb2 == 5] <- ">16"

df$costb3[df$costb3 == 0] <- "None"
df$costb3[df$costb3 == 1] <- "1-2"
df$costb3[df$costb3 == 2] <- "3-5"
df$costb3[df$costb3 == 3] <- "6-10"
df$costb3[df$costb3 == 4] <- "11-16"
df$costb3[df$costb3 == 5] <- ">16"

df$costb4[df$costb4 == 0] <- "None"
df$costb4[df$costb4 == 1] <- "1-2"
df$costb4[df$costb4 == 2] <- "3-5"
df$costb4[df$costb4 == 3] <- "6-10"
df$costb4[df$costb4 == 4] <- "11-16"
df$costb4[df$costb4 == 5] <- ">16"

df$costb5[df$costb5 == 0] <- "Not working, not looking for work"
df$costb5[df$costb5 == 1] <- "Not working, looking for work"
df$costb5[df$costb5 == 2] <- "Working part-time"
df$costb5[df$costb5 == 3] <- "Working full time"

# now need to add in full scores (will fix once get all info on all scales)

# Stigma 16 items
# Community Perspectives: 1:11
# Patient Perspectives: 12, 14, 16, 17, 20

# Empathy 13 final items
# Final Cognitive Scale Questions: 5, 16, 21, 23, 25, 26, 28
# Final Affective Scale Questions: 6, 10, 13, 14, 17, 18

# Social Support 16 final items
# Perceived Emotional Support: 1:4
# Percieved Instrumental Support: 5:8
# Need for Support: 9:11
# Support Seeking: 13:17

df <- df %>% mutate(stigma_com = stigb1 + stigb2 + stigb3 + stigb4 + stigb5 +
                      stigb6 + stigb7 + stigb8 + stigb9 + stigb10 +
                      stigb11 + stigb18,
                    stigma_pt = stigb12 + stigb14 + stigb16 + stigb17 + stigb20,
                    trust = trustb1 + trustb13 + trustb14 + trustb16 + trustb19 +
                      trustb22 + trustb23 + trustb26,
                    cog_emp = empb5 + empb16 + empb21 + empb23 + empb25 + empb26 + empb28, 
                    aff_emp = empb6 + empb10 + empb13 + empb14 + empb17 + empb18, 
                    soc_sup_ps = ssb1 + ssb2 + ssb3 + ssb4 +ssb6 + ssb7 + ssb8, 
                    soc_sup_ns = ssb9 + ssb10 + ssb11 + ssb13 + ssb14 + 
                      ssb15 + ssb16 + ssb17,
                    hivk = hkb1 + hkb2 + hkb3 + hkb4 + hkb5 +
                      hkb6 + hkb7 + hkb8 + hkb9 + hkb10 +
                      hkb11 + hkb12 + hkb13 + hkb14 + hkb15 + 
                      hkb16 + hkb17 + hkb18 + hkb19 + hkb20 + 
                      hkb21 + hkb22 + hkb23 + hkb24 + hkb25 + 
                      hkb26 + hkb27,
                    phq9 = db1 + db2 + db3 + db4 + db5 + db6 + db7 + db8 + db9)

# name groups
df$group[df$group == 0] <- "Control"
df$group[df$group == 1] <- "Intervention"
df$group <- factor(df$group, levels = c("Control", "Intervention"))

### Joining Clinical Data ###

# pull in clinical data
clin_data <- read_excel("./Clinical Data/baseline_clinical.xlsx") %>%
  select(Novo_id_Participante, patient_id,
         art_initiation_date:WHO_clinical_stage_at_art_initiation_date,
         weight, weight_date, `height...22`, height_date, bmi) %>%
  mutate(art_initiation_date = as.Date(art_initiation_date),
         enrollment_date = as.Date(enrollment_date),
         WHO_clinical_stage_at_enrollment_date = as.Date(WHO_clinical_stage_at_enrollment_date),
         WHO_clinical_stage_at_art_initiation_date = as.Date(WHO_clinical_stage_at_art_initiation_date),
         weight_date = as.Date(weight_date),
         height_date = as.Date(height_date)) %>%
  rename(pt_id = Novo_id_Participante)


# add in ART Hx data
arthx <- read_csv("./Clinical Data/2021_04_30_arthx.csv") %>%
  rename(art_hx = "flag_previous_ART") %>%
  mutate(art_hx = ifelse(art_hx == TRUE, "Yes", "No")) %>%
  select(-district)

# join arthx with clinical data
clin_data1 <- left_join(clin_data, arthx, by = "patient_id") 
# remove duplicates
clin_data1 <- clin_data1 %>% distinct(pt_id, .keep_all = TRUE)

# now join with df
df_joined <- left_join(df, clin_data1, by = "pt_id") %>%
  mutate(WHO_clinical_stage_at_art_initiation = factor(WHO_clinical_stage_at_art_initiation))

# some date checks
dates <- df_joined %>% select(pt_id, reg_date, enrollment_date, 
                              WHO_clinical_stage_at_enrollment_date, 
                              WHO_clinical_stage_at_art_initiation_date, 
                              weight_date, height_date) %>%
  mutate(reg_enrol = ifelse(reg_date == enrollment_date, 1, 0),
         reg_ART = ifelse(reg_date == WHO_clinical_stage_at_art_initiation_date, 1, 0),
         enrol_ART = ifelse(enrollment_date == WHO_clinical_stage_at_art_initiation_date, 1, 0),
         reg_wt = ifelse(reg_date == weight_date, 1, 0),
         enrol_wt = ifelse(enrollment_date == weight_date, 1, 0),
         reg_ht = ifelse(reg_date == height_date, 1, 0),
         enrol_ht = ifelse(enrollment_date == height_date, 1, 0))
sum(dates$reg_enrol, na.rm = TRUE) # 1123 /2158
sum(dates$reg_ART, na.rm = TRUE) # 1424 /2158
sum(dates$enrol_ART, na.rm = TRUE) # 1483 /2158
sum(dates$reg_wt, na.rm = TRUE) # 1513 /2158
sum(dates$enrol_wt, na.rm = TRUE) # 1138 /2158
sum(dates$reg_ht, na.rm = TRUE) # 1451 /2158
sum(dates$enrol_ht, na.rm = TRUE) # 1087 /2158
sum(dates$reg_date < dates$enrollment_date, na.rm = TRUE) # 411 / 1123
sum(dates$reg_date > dates$enrollment_date, na.rm = TRUE) # 613 / 1123
summary(as.numeric(dates$reg_date - dates$enrollment_date))
#hist(as.numeric(dates$reg_date - dates$enrollment_date), breaks = 100)
#hist(as.numeric(dates$reg_date - dates$WHO_clinical_stage_at_art_initiation_date), breaks = 100)

### now just select columns of interest ###
cleaned <- df_joined %>% select(record_id:pt_id, old_pt_id, district,
                                age, day, year, group, clinic, sex, 
                                rel_stat, edu_cat, job_cat, study_status, 
                                stigma_com:phq9, art_hx,
                                WHO_clinical_stage_at_art_initiation, bmi,
                                costb1, costb2, costb3, costb4, costb5)

Save(cleaned)