  # 抓取 cohort 的門診、住院及急診資料 ---------------------------------------------------
  
  icd_pattern <- 
    str_c(
      c(paste0('icd9_code', 1:5),
        paste0('icd10_code', 1:5)),
      collapse = ', '
    )
  
  opd_longer_cohort <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, opd_date, fee_no, ',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_opd_basic_',
                 x,
                 ' where chr_no in ( ',
                 str_c("'", 
                       chr_no_cohort[hospital == x, ]$chr_no,
                       "'",
                       collapse = ', '),
                 ') ;')
        ) %>% 
          mutate(hospital = x,
                 data_type = 'opd')
      }
    ) %>% 
    pivot_longer(
      cols = contains('icd'),
      names_to = 'icd_type',
      values_to = 'icd_code'
    ) %>% 
    mutate(
      icd_type = str_replace(icd_type, 
                             pattern = '[0-9]$', 
                             replacement = '')
    ) %>% 
    filter(
      (icd_type == 'icd9_code' & opd_date < 1050101)|
        (icd_type == 'icd10_code' & opd_date >= 1050101)
    ) %>% 
    rename(
      drug_date = opd_date
    )
  
  icd_pattern <- 
    str_c(
      c(paste0('sdiag_code', 1:7),
        paste0('icd10_code', 1:8, '_out')),
      collapse = ', '
    )
  
  ipd_longer_cohort <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, fee_no, 
                        ipd_date, mdiag_code,',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_ipd_basic_',
                 x,
                 ' where chr_no in ( ',
                 str_c("'", 
                       chr_no_cohort[hospital == x, ]$chr_no,
                       "'",
                       collapse = ', '),
                 ') ;')
        ) %>% 
          mutate(hospital = x,
                 data_type = 'ipd')
      }
    ) %>% 
    pivot_longer(
      cols = contains('_code'),
      names_to = 'icd_type',
      values_to = 'icd_code'
    ) %>% 
    mutate(
      icd_type = 
        case_when(
          grepl(icd_type, pattern = 'icd10') ~
            'icd10_code',
          !grepl(icd_type, pattern = 'icd10') ~
            'icd9_code'
        )
    ) %>% 
    filter(
      (!grepl(icd_type, pattern = 'icd10') & ipd_date < 1050101)|
        (grepl(icd_type, pattern = 'icd10') & ipd_date >= 1050101)
    ) %>% 
    rename(
      drug_date = ipd_date
    ) %>% 
    setDT()
  
  icd_pattern <- 
    str_c(
      c(paste0('icd9_code', 1:2)),
      collapse = ', '
    )
  
  epd_longer_cohort <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, epd_date, fee_no, ',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_epd_his_',
                 x,
                 ' where chr_no in ( ',
                 str_c("'", 
                       chr_no_cohort[hospital == x, ]$chr_no,
                       "'",
                       collapse = ', '),
                 ') ;')
        ) %>% 
          mutate(hospital = x,
                 data_type = 'epd')
      }
    ) %>% 
    pivot_longer(
      cols = contains('_code'),
      names_to = 'icd_type',
      values_to = 'icd_code'
    ) %>% 
    mutate(
      icd_type = 
        case_when(
          grepl(icd_type, pattern = 'icd10') ~
            'icd10_code',
          !grepl(icd_type, pattern = 'icd10') ~
            'icd9_code'
        )
    ) %>% 
    filter(
      (!grepl(icd_type, pattern = 'icd10') & epd_date < 1050101)|
        (grepl(icd_type, pattern = 'icd10') & epd_date >= 1050101)
    ) %>% 
    rename(
      drug_date = epd_date
    ) %>% 
    setDT()
  
  oie_longer_cohort <- 
    bind_rows(
      opd_longer_cohort,
      ipd_longer_cohort,
      epd_longer_cohort
    ) %>% 
    distinct() %>% 
    setDT(key = c('chr_no', 'hospital', 'drug_date', 
                  'icd_type', 'icd_code'))
