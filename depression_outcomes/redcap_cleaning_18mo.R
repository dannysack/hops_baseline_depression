# load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/Thesis/Analysis/Cleaning/")

# pull in redcap data
raw <- read_csv("../Data/redcap_20221024.csv", 
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
                            estigma_1:paciente_9, codigo,
                            estigma_1_v2:paciente_9_v2,
                            empatia_1_v3:paciente_9_v3,
                            data_2a_entrevista_mulher, 
                            data_3a_entrevista_mulher) %>% mutate(sex = "Female")

df_male <- raw %>% select(record_id, data_de_inscricao,
                          male_patient_id_for_matching,
                          distrito_h, unidade_sani_gile_h:unidade_sani_quel_h,
                          demographics_5_homem:demographics_6_homem,
                          demographics_14_homem:demographics_15a1_homem,
                          estado_actual_homem, data_1a_entrevista_homem,
                          estigma_1_h_1:paciente_9_h_1, codigo_homem,
                          estigma_1_h_1_h_v2:paciente_9_h_1_h_v2,
                          empatia_1_h_v3:paciente_9_h_v3,
                          data_2a_entrevista_homem, 
                          data_3a_entrevista_homem) %>% mutate(sex = "Male")

# rename columns
col_names <- c("record_id", "reg_date", "pt_id", "district", "uni_sani_gile",
               "uni_sani_inha", "uni_sani_mag", "uni_sani_moc", "uni_sani_nam",
               "uni_sani_peb", "uni_sani_quel",
               "age", "age_check", "rel_stat", "edu_text", "edu_cat",
               "job_free", "job_cat", "job_other", "study_status", 
               "baseline_int_date", 
               "stigb1", "stigb2", "stigb3", 
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
               "empb23", "empb24", "empb25", "empb26", "empb27", "empb28",
               "ssb1", "ssb2", "ssb3", "ssb4", 
               "ssb5", "ssb6", "ssb7", "ssb8", "ssb9", "ssb10", "ssb11", 
               "ssb12", "ssb13", "ssb14", "ssb15", "ssb16", "ssb17", 
               "hkb1", "hkb2", "hkb3", "hkb4", "hkb5", "hkb6", "hkb7", 
               "hkb8", "hkb9", "hkb10", "hkb11", "hkb12", "hkb13", "hkb14", 
               "hkb15", "hkb16", "hkb17", "hkb18", "hkb19", "hkb20", "hkb21", 
               "hkb22", "hkb23", "hkb24", "hkb25", "hkb26", "hkb27", "db1", 
               "db2", "db3", "db4", "db5", "db6", "db7", "db8", "db9",
               "codigo", 
               "stig6m1", "stig6m2", "stig6m3", 
               "stig6m4", "stig6m5", "stig6m6", "stig6m7", "stig6m8", 
               "stig6m9", "stig6m10", "stig6m11", "stig6m12", "stig6m13", 
               "stig6m14", "stig6m15", "stig6m16", "stig6m17", "stig6m18", 
               "stig6m19", "stig6m20", "trust6m1", "trust6m2", "trust6m3",
               "trust6m5", "trust6m6", "trust6m7", "trust6m8",
               "trust6m9", "trust6m10", "trust6m11", "trust6m12", "trust6m13",
               "trust6m14", "trust6m15", "trust6m16", "trust6m17", "trust6m18",
               "trust6m19", "trust6m20", "trust6m21", "trust6m22", "trust6m23",
               "trust6m24", "trust6m25", "trust6m26", "emp6m1", "emp6m2", "emp6m3",
               "emp6m4", "emp6m5", "emp6m6", "emp6m7", "emp6m8", "emp6m9", "emp6m10",
               "emp6m11", "emp6m12", "emp6m13", "emp6m14", "emp6m15", "emp6m16", 
               "emp6m17", "emp6m18", "emp6m19", "emp6m20", "emp6m21", "emp6m22",
               "emp6m23", "emp6m24", "emp6m25", "emp6m26", "emp6m27", "emp6m28",
               "ss6m1", "ss6m2", "ss6m3", "ss6m4", 
               "ss6m5", "ss6m6", "ss6m7", "ss6m8", "ss6m9", "ss6m10", "ss6m11", 
               "ss6m12", "ss6m13", "ss6m14", "ss6m15", "ss6m16", "ss6m17", 
               "hk6m1", "hk6m2", "hk6m3", "hk6m4", "hk6m5", "hk6m6", "hk6m7", 
               "hk6m8", "hk6m9", "hk6m10", "hk6m11", "hk6m12", "hk6m13", "hk6m14", 
               "hk6m15", "hk6m16", "hk6m17", "hk6m18", "hk6m19", "hk6m20", "hk6m21", 
               "hk6m22", "hk6m23", "hk6m24", "hk6m25", "hk6m26", "hk6m27", "d6m1", 
               "d6m2", "d6m3", "d6m4", "d6m5", "d6m6", "d6m7", "d6m8", "d6m9",
               "emp18m1", "emp18m2", "emp18m3", "emp18m4", "emp18m5", "emp18m6", 
               "emp18m7", "emp18m8", "emp18m9", "emp18m10", "emp18m11", "emp18m12", 
               "emp18m13", "emp18m14", "emp18m15", "emp18m16", "emp18m17", "emp18m18", 
               "emp18m19", "emp18m20", "emp18m21", "emp18m22", "emp18m23", "emp18m24", 
               "emp18m25", "emp18m26", "emp18m27", "emp18m28", "d18m1", "d18m2", "d18m3", 
               "d18m4", "d18m5", "d18m6", "d18m7", "d18m8", "d18m9",
               "date_6mo", "date_18mo",
               "sex")

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
           df$uni_sani_quel == 0] <- "Intervention"

# control
df$group[df$uni_sani_gile == 0 | df$uni_sani_inha %in% c(0, 1, 2) | 
           df$uni_sani_moc %in% c(1, 2) | df$uni_sani_nam %in% c(0, 1, 4) | 
           df$uni_sani_peb %in% c(1, 2, 4)] <- "Control"

# make groups a factor
df$group <- factor(df$group, levels = c("Control", "Intervention"))

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
df <- df %>% mutate(day = as.numeric(reg_date - min(reg_date)),
                    year = year(reg_date))

# now need to get to work renaming things...

# relabel district
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
# below characterizations informed by Ariano
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

# need to figure out which of the free texts are present in the absence of categories
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

# stigma (15:34, 142:161)
# 0-3 are coded correctly!
df[, c(15:34, 142:161)][df[, c(15:34, 142:161)] == 4] <- NA

# trust
# needs to be coded from 1-5
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 5] <- NA # go from top to bottom to do so
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 4] <- 5
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 3] <- 4
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 2] <- 3
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 1] <- 2
df[, c(35:59, 162:186)][df[, c(35:59, 162:186)] == 0] <- 1

# empathy
# needs to be coded from 0-4
# 0-4 correctly coded!
df[, c(60:87, 187:214, 268:295)][df[, c(60:87, 187:214, 268:295)] == 5] <- NA

# social support
# needs to be coded from 1-4
df[, c(88:104, 215:231)][df[, c(88:104, 215:231)] == 4] <- NA
df[, c(88:104, 215:231)][df[, c(88:104, 215:231)] == 3] <- 4
df[, c(88:104, 215:231)][df[, c(88:104, 215:231)] == 2] <- 3
df[, c(88:104, 215:231)][df[, c(88:104, 215:231)] == 1] <- 2
df[, c(88:104, 215:231)][df[, c(88:104, 215:231)] == 0] <- 1

# HIV knowledge
# False: 1, 3, 11, 13:18, 20, 22, 23, 27
# True: 2, 4:10, 12, 19, 21, 24:26
# coded 0 (incorrect or not sure) and 1 (correct)

# first add in NAs
df[, c(105:131, 232:258)][df[, c(105:131, 232:258)] == 3] <- NA

# now go through and recode things :)
# when the correct answer is True (2, 4:10, 12, 19, 21, 24:26)
# if they said disagree or not sure, they are incorrect
df[, c(106, 108:114, 116, 123, 125, 128:130, 
      233, 235:241, 243, 250, 252, 255:257)][df[, c(106, 108:114, 116, 123, 125, 128:130, 
                                                    233, 235:241, 243, 250, 252, 255:257)] == 0] <- 0
df[, c(106, 108:114, 116, 123, 125, 128:130, 
      233, 235:241, 243, 250, 252, 255:257)][df[, c(106, 108:114, 116, 123, 125, 128:130, 
                                                    233, 235:241, 243, 250, 252, 255:257)] == 1] <- 0
# if they said agree they are correct
df[, c(106, 108:114, 116, 123, 125, 128:130, 
       233, 235:241, 243, 250, 252, 255:257)][df[, c(106, 108:114, 116, 123, 125, 128:130, 
                                                     233, 235:241, 243, 250, 252, 255:257)] == 2] <- 1

# when the correct answer is FALSE (1, 3, 11, 13:18, 20, 22, 23, 27)
# if they said disagree, they are correct (need to make it something odd to work)
df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
      232, 234, 242, 244:249, 251, 253, 258)][df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
                                                     232, 234, 242, 244:249, 251, 253, 258)] == 0] <- 10
# if they said not sure or agree, they are incorrect
df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
       232, 234, 242, 244:249, 251, 253, 258)][df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
                                                      232, 234, 242, 244:249, 251, 253, 258)] == 1] <- 0
df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
       232, 234, 242, 244:249, 251, 253, 258)][df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
                                                      232, 234, 242, 244:249, 251, 253, 258)] == 2] <- 0

# now fix
df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
       232, 234, 242, 244:249, 251, 253, 258)][df[, c(105, 107, 115, 117:122, 124, 126:127, 131,
                                                      232, 234, 242, 244:249, 251, 253, 258)] == 10] <- 1

# depression
# coded from 0-3
df[, c(132:140, 259:267, 296:304)][df[, c(132:140, 259:267, 296:304)] == 4] <- NA

# now need to add in full scores (will fix once get all info on all scales)

# Stigma 16 items
# Community Perspectives: 1:11
# Patient Perspectives: 12, 14, 16, 17, 20

# Trust 8 items: 1, 13, 14, 16, 19, 22, 23, 26

# Empathy 13 final items
# Final Cognitive Scale Questions: 5, 16, 21, 23, 25, 26, 28
# Final Affective Scale Questions: 6, 10, 13, 14, 17, 18

# Social Support 16 final items
# Perceived Support: 1:4, 6:8
# Need for Support: 9:11, 14:17

df <- df %>% mutate(stigma_com_b = stigb1 + stigb2 + stigb3 + stigb4 + stigb5 +
                      stigb6 + stigb7 + stigb8 + stigb9 + stigb10 +
                      stigb11 + stigb18,
                    stigma_pt_b = stigb12 + stigb14 + stigb16 + stigb17 + stigb20,
                    trust_b = trustb1 + trustb13 + trustb14 + trustb16 + trustb19 +
                      trustb22 + trustb23 + trustb26,
                    cog_emp_b = empb5 + empb16 + empb21 + empb23 + empb25 + empb26 + empb28, 
                    aff_emp_b = empb6 + empb10 + empb13 + empb14 + empb17 + empb18, 
                    soc_sup_ps_b = ssb1 + ssb2 + ssb3 + ssb4 +ssb6 + ssb7 + ssb8, 
                    soc_sup_ns_b = ssb9 + ssb10 + ssb11 + ssb13 + ssb14 + 
                      ssb15 + ssb16 + ssb17,
                    hivk_b = hkb1 + hkb2 + hkb3 + hkb4 + hkb5 +
                      hkb6 + hkb7 + hkb8 + hkb9 + hkb10 +
                      hkb11 + hkb12 + hkb13 + hkb14 + hkb15 + 
                      hkb16 + hkb17 + hkb18 + hkb19 + hkb20 + 
                      hkb21 + hkb22 + hkb23 + hkb24 + hkb25 + 
                      hkb26 + hkb27,
                    phq9_b = db1 + db2 + db3 + db4 + db5 + db6 + db7 + db8 + db9,
                    stigma_com_6m = stig6m1 + stig6m2 + stig6m3 + stig6m4 + stig6m5 +
                      stig6m6 + stig6m7 + stig6m8 + stig6m9 + stig6m10 +
                      stig6m11 + stig6m18,
                    stigma_pt_6m = stig6m12 + stig6m14 + stig6m16 + stig6m17 + stig6m20,
                    trust_6m = trust6m1 + trust6m13 + trust6m14 + trust6m16 + trust6m19 +
                      trust6m22 + trust6m23 + trust6m26,
                    cog_emp_6m = emp6m5 + emp6m16 + emp6m21 + emp6m23 + emp6m25 + emp6m26 + emp6m28, 
                    aff_emp_6m = emp6m6 + emp6m10 + emp6m13 + emp6m14 + emp6m17 + emp6m18, 
                    soc_sup_ps_6m = ss6m1 + ss6m2 + ss6m3 + ss6m4 +ss6m6 + ss6m7 + ss6m8, 
                    soc_sup_ns_6m = ss6m9 + ss6m10 + ss6m11 + ss6m13 + ss6m14 + 
                      ss6m15 + ss6m16 + ss6m17,
                    hivk_6m = hk6m1 + hk6m2 + hk6m3 + hk6m4 + hk6m5 +
                      hk6m6 + hk6m7 + hk6m8 + hk6m9 + hk6m10 +
                      hk6m11 + hk6m12 + hk6m13 + hk6m14 + hk6m15 + 
                      hk6m16 + hk6m17 + hk6m18 + hk6m19 + hk6m20 + 
                      hk6m21 + hk6m22 + hk6m23 + hk6m24 + hk6m25 + 
                      hk6m26 + hk6m27,
                    phq9_6m = d6m1 + d6m2 + d6m3 + d6m4 + d6m5 + d6m6 + d6m7 + d6m8 + d6m9,
                    cog_emp_18m = emp18m5 + emp18m16 + emp18m21 + emp18m23 + emp18m25 + emp18m26 + emp18m28, 
                    aff_emp_18m = emp18m6 + emp18m10 + emp18m13 + emp18m14 + emp18m17 + emp18m18,
                    phq9_18m = d18m1 + d18m2 + d18m3 + d18m4 + d18m5 + d18m6 + d18m7 + d18m8 + d18m9)


# now create final dataframe
redcap_cleaned_18m <- df %>%
  select(record_id:pt_id, codigo, district,
         age, day, year, group, clinic, sex, 
         rel_stat, edu_cat, job_cat, study_status,
         baseline_int_date,
         stigma_com_b:phq9_b,
         date_6mo,
         stigma_com_6m:phq9_6m,
         date_18mo,
         cog_emp_18m:phq9_18m)

# now label columns
label(redcap_cleaned_18m$record_id) <- "Record ID"
label(redcap_cleaned_18m$reg_date) <- "Registration Date"
label(redcap_cleaned_18m$pt_id) <- "Patient ID for OpenMRS Matching"
label(redcap_cleaned_18m$codigo) <- "Patient ID for Drive Matching"
label(redcap_cleaned_18m$group) <- "Study Group"
label(redcap_cleaned_18m$age) <- "Age"
units(redcap_cleaned_18m$age) <- "years"
label(redcap_cleaned_18m$year) <- "Enrollment Year"
label(redcap_cleaned_18m$day) <- "Enrollment Day"
label(redcap_cleaned_18m$clinic) <- "Clinic"
label(redcap_cleaned_18m$sex) <- "Sex"
label(redcap_cleaned_18m$district) <- "District"
label(redcap_cleaned_18m$rel_stat) <- "Relationship Status"
label(redcap_cleaned_18m$edu_cat) <- "Education"
label(redcap_cleaned_18m$job_cat) <- "Occupation"
label(redcap_cleaned_18m$study_status) <- "Study Status"
label(redcap_cleaned_18m$baseline_int_date) <- "Baseline Survey Date"
label(redcap_cleaned_18m$stigma_com_b) <- "Baseline Perceived Community Stigma"
label(redcap_cleaned_18m$stigma_pt_b) <- "Baseline Patient Felt/Experienced Stigma"
label(redcap_cleaned_18m$trust_b) <- "Baseline Physician Trust"
label(redcap_cleaned_18m$cog_emp_b) <- "Baseline Cognitive Empathy"
label(redcap_cleaned_18m$aff_emp_b) <- "Baseline Affective Empathy"
label(redcap_cleaned_18m$soc_sup_ps_b) <- "Baseline Perceived Support"
label(redcap_cleaned_18m$soc_sup_ns_b) <- "Baseline Needed Support"
label(redcap_cleaned_18m$hivk_b) <- "Baseline HIV Knowledge (0-27)"
label(redcap_cleaned_18m$phq9_b) <- "Baseline Patient Health Questionaire-9"
label(redcap_cleaned_18m$date_6mo) <- "6-month Survey Date"
label(redcap_cleaned_18m$stigma_com_6m) <- "6-month Perceived Community Stigma"
label(redcap_cleaned_18m$stigma_pt_6m) <- "6-month Patient Felt/Experienced Stigma"
label(redcap_cleaned_18m$trust_6m) <- "6-month Physician Trust"
label(redcap_cleaned_18m$cog_emp_6m) <- "6-month Cognitive Empathy"
label(redcap_cleaned_18m$aff_emp_6m) <- "6-month Affective Empathy"
label(redcap_cleaned_18m$soc_sup_ps_6m) <- "6-month Perceived Support"
label(redcap_cleaned_18m$soc_sup_ns_6m) <- "6-month Needed Support"
label(redcap_cleaned_18m$hivk_6m) <- "6-month HIV Knowledge (0-27)"
label(redcap_cleaned_18m$phq9_6m) <- "6-month Patient Health Questionaire-9"
label(redcap_cleaned_18m$date_18mo) <- "15/18-month Survey Date"
label(redcap_cleaned_18m$cog_emp_18m) <- "15/18-month Cognitive Empathy"
label(redcap_cleaned_18m$aff_emp_18m) <- "15/18-month Affective Empathy"
label(redcap_cleaned_18m$phq9_18m) <- "15/18-month Patient Health Questionaire-9"

# save .rda file for merging to "Date" folder
options(LoadPath = "../Data/")
Save(redcap_cleaned_18m)

# for use in stata
write_csv(redcap_cleaned_18m, "18mo_redcap_data.csv")
