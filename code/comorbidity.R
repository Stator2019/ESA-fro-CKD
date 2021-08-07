# comorbidity pattern ICD9-----------------------------------------------------
  
  hypertension_icd9 <-
    paste(sep = '|',
           '401.*',
           'A269')

  hyperlipidemia_icd9 <-
    paste(sep = '|',
          '272.*',
          'A189')

  depression_icd9 <-
    paste(sep = '|',
          '296[23].*',
          '311.*'
    )

  anxiety_icd9 <-
    paste(sep = '|',
          '300.*',
          'A214'
    )
  
  mi_icd9 <- 
    paste(sep = '|',
          '41[02].*'
    )
    
  chr_icd9 <- 
    paste(sep = '|',
          '398.91', '402.01', '402.11', '402.91',
          '404.01', '404.03', '404.11', '404.13',
          '404.91', '404.93', '425.[456789]', '428.*',
          'A260'
    )
  
  pvd_icd9 <- 
    paste(sep = '|',
          '093.0', '437.3', '44[01].*', '443.[1-9]', 
          '447.1', '557.1', '557.9', 'V43.4'
    )
  
  cvd_icd9 <- 
    paste(sep = '|',
          '362.34', '43[012345678].*'
    )
  
  dementia_icd9 <- 
    paste(sep = '|',
          '290.*', '294.1', '331.2'
    )
     
  copd_icd9 <- 
    paste(sep = '|',
          '416.[89]', '49.*', '50[012345].*',
          '506.4', '508.[18]'
    )
   
  rheum_icd9 <- 
    paste(sep = '|',
          '446.5', '710.[01234]', '714.[012]',
          '714.8', '725.*'
    )
  
  pud_icd9 <- 
    paste(sep = '|',
          '53[1234].*'
    )
  
  dm_icd9 <- 
    paste(sep = '|',
          '250.*'
    )
  
  paralysis_icd9 <- 
    paste(sep = '|',
          '334.1', '34[23].*', '344.[0-69]'
    )
  
  renal_icd9 <- 
    paste(sep = '|',
          '403.[019]1', '404.0[23]', '404.1[23]', 
          '404.9[23]', '582.*', '583.[01234567]',
          '58[56].*', '588.0', 'V42.0', 
          'V45.1', 'V56.*'
    )
   
  liver_icd9 <- 
    paste(sep = '|',
          '070.2[23]', '070.3[23]', '070.[45]4', '070.[69]',
          '57[01].*', '573.[3489]', 'V42.7'
    )
       
  malignancy_icd9 <- 
    paste(sep = '|',
          '1[456].*', '17[012456789].*', '18.*',
          '19[01234].*', '195.[12345678]', '20[012345678].*',
          '238.6'
    )
   
  tumor_icd9 <- 
    paste(sep = '|',
          '19[6789].*'
    )
  
  hiv_icd9 <- 
    paste(sep = '|',
          '04[234].*'
    )

# comorbidity pattern ICD10 -----------------------------------------------
  
  mi_icd10 <- 
    paste(sep = '|',
          'I2[12].*', 'I25.2')
  
  chr_icd10 <- 
    paste(sep = '|',
          'I09.9', 'I1[13].0', 'I13.2', 'I25.5',
          'I42.0', 'I42.[5-9]', 'I43.*', 'I50.*',
          'P29.0')
  
  pvd_icd10 <- 
    paste(sep = '|',
          'I7[01].*', 'I73.[189]', 'I77.1', 'I79.0',
          'I79.2', 'K55.[189]', 'Z95.[89]')
  
  cvd_icd10 <- 
    paste(sep = '|',
          'G4[56].*', 'H34.0', 'I6[0-9].*')
  
  dementia_icd10 <- 
    paste(sep = '|',
          'F0[1-3].*', 'F05.1', 'G30.*', 'G31.1')
  
  copd_icd10 <- 
    paste(sep = '|',
          'I27.[89]', 'J4[0-7].*', 'J6[0-7].*',
          'J68.4', 'J70.[13]')
  
  rheum_icd10 <- 
    paste(sep = '|',
          'M0[56].*', 'M31.5', 'M3[2-4].*', 'M35.1',
          'M35.3', 'M36.0')
  
  pud_icd10 <- 
    paste(sep = '|',
          'K2[5-8].*')
  
  liver_icd10 <- 
    paste(sep = '|',
          'B18.*', 'K70.[0-3]', 'K70.9', 'K71.[3457]',
          'K7[34].*', 'K76.0', 'K76.[234]', 'K76.[89]',
          'Z94.4', 'I85.[09]', 'I86.4', 'I98.2',
          'K70.4', 'K71.1', 'K72.[19]', 'K76.[567]')
  
  dm_icd10 <- 
    paste(sep = '|',
          'E1[0-4].[01689]', 'E1[0-4].[2-5]', 'E1[0-4].7')
  
  paralysis_icd10 <- 
    paste(sep = '|',
          'G04.1', 'G11.4', 'G80.[12]',
          'G8[12].*', 'G83.[0-4]', 'G83.9')
    
  renal_icd10 <- 
    paste(sep = '|',
          'I12.0', 'I13.1', 'N03.[2-7]', 'N05.[2-7]',
          'N18.*','N19.*', 'N25.0', 'Z49.[012]', 'Z94.0',
          'Z99.2')
  
  malignancy_icd10 <- 
    paste(sep = '|',
          'C[01][0-9].*', 'C2[0-6].*', 'C3[0-4].*', 
          'C3[7-9].*', 'C4[01356789].*', 'C5[0-8].*',
          'C6[0-9].*', 'C7[0-6].*', 'C8[1-5].*', 'C88.*',
          'C9[0-7].*')
  
  tumor_icd10 <- 
    paste(sep = '|',
          'C7[7-9].*', 'C80.*'
    )
  
  hiv_icd10 <- 
    paste(sep = '|',
          'B2[0124].*'
    )
  

# create CCI table --------------------------------------------------------
  
  correct_oie_longer_cohort <- 
    oie_longer_cohort %>% 
    mutate(
      icd_type = if_else(icd_code %chin% c('585', '586'),
                         'icd9_code',
                         icd_type,
                         icd_type)
    )
  
  cci_table <- 
    bind_rows(
      correct_oie_longer_cohort[icd_type == 'icd9_code'] %>% 
        distinct(icd_type, icd_code) %>% 
        mutate(
          comorbidity = 
            case_when(
              grepl(icd_code, pattern = hypertension_icd9) ~ 'hypertension',
              grepl(icd_code, pattern = hyperlipidemia_icd9) ~ 'hyperlipidemia',
              grepl(icd_code, pattern = depression_icd9) ~ 'depression',
              grepl(icd_code, pattern = anxiety_icd9) ~ 'anxiety',
              grepl(icd_code, pattern = mi_icd9) ~ 'mi',
              grepl(icd_code, pattern = chr_icd9) ~ 'chr',
              grepl(icd_code, pattern = pvd_icd9) ~ 'pvd',
              grepl(icd_code, pattern = cvd_icd9) ~ 'cvd',
              grepl(icd_code, pattern = dementia_icd9) ~ 'dementia',
              grepl(icd_code, pattern = copd_icd9) ~ 'copd',
              grepl(icd_code, pattern = rheum_icd9) ~ 'rheum',
              grepl(icd_code, pattern = pud_icd9) ~ 'pud',
              grepl(icd_code, pattern = dm_icd9) ~ 'dm',
              grepl(icd_code, pattern = paralysis_icd9) ~ 'paralysis',
              grepl(icd_code, pattern = renal_icd9) ~ 'renal',
              grepl(icd_code, pattern = liver_icd9) ~ 'liver',
              grepl(icd_code, pattern = malignancy_icd9) ~ 'malignancy',
              grepl(icd_code, pattern = tumor_icd9) ~ 'tumor',
              grepl(icd_code, pattern = hiv_icd9) ~ 'hiv',
              T ~ 'no defined'
            )
        ),
      correct_oie_longer_cohort[icd_type == 'icd10_code'] %>% 
        distinct(icd_type, icd_code) %>% 
        mutate(
          comorbidity = 
            case_when(
              grepl(icd_code, pattern = mi_icd10) ~ 'mi',
              grepl(icd_code, pattern = chr_icd10) ~ 'chr',
              grepl(icd_code, pattern = pvd_icd10) ~ 'pvd',
              grepl(icd_code, pattern = cvd_icd10) ~ 'cvd',
              grepl(icd_code, pattern = dementia_icd10) ~ 'dementia',
              grepl(icd_code, pattern = copd_icd10) ~ 'copd',
              grepl(icd_code, pattern = rheum_icd10) ~ 'rheum',
              grepl(icd_code, pattern = pud_icd10) ~ 'pud',
              grepl(icd_code, pattern = liver_icd10) ~ 'liver',
              grepl(icd_code, pattern = dm_icd10) ~ 'dm',
              grepl(icd_code, pattern = paralysis_icd10) ~ 'paralysis',
              grepl(icd_code, pattern = renal_icd10) ~ 'renal',
              grepl(icd_code, pattern = malignancy_icd10) ~ 'malignancy',
              grepl(icd_code, pattern = tumor_icd10) ~ 'tumor',
              grepl(icd_code, pattern = hiv_icd10) ~ 'hiv',
              T ~ 'no defined'
            )
        )
    ) %>% 
    filter(comorbidity != 'no defined')
    

# CCI patient -------------------------------------------------------------

  cci_cohort <- 
    left_join(
      correct_oie_longer_cohort,
      cci_table[comorbidity != 'no defined', ],
      by = c('icd_code' = 'icd_code',
             'icd_type' = 'icd_type')
    ) %>% 
    select(hospital, chr_no, comorbidity) %>% 
    group_by(hospital, chr_no, comorbidity) %>% 
    mutate(freq = row_number()) %>% 
    summarise(freq = max(freq))
  
  cci_person <- 
    cci_cohort %>% 
    filter(!is.na(comorbidity)) %>% 
    group_by(comorbidity, .drop = T) %>% 
    summarise(
      count_person = n_distinct(hospital, chr_no),
      unique_num = unique(freq) %>% length(),
      range = max(freq, na.rm = T) - min(freq, na.rm = T),
      stat_mean = mean(freq, na.rm = T),
      stat_std = var(freq, na.rm = T) %>% sqrt(),
      stat_min = min(freq, na.rm = T),
      Q5 = quantile(freq, 0.05, na.rm = T),
      Q10 = quantile(freq, 0.1, na.rm = T),
      Q25 = quantile(freq, 0.25, na.rm = T),
      median = quantile(freq, 0.5, na.rm = T),
      Q75 = quantile(freq, 0.75, na.rm = T),
      Q90 = quantile(freq, 0.9, na.rm = T),
      Q95 = quantile(freq, 0.95, na.rm = T),
      stat_max = max(freq, na.rm = T)
    ) %>% 
    arrange(desc(count_person))
  
  write_xlsx(
    cci_person,
    'data/cci_result.xlsx'
  )
  