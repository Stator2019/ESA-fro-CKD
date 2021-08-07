# trans icd10 to icd9 -----------------------------------------------------
  
  icd10_to_icd9 <- 
    sqlQuery(
      ckd_fracture_Grace,
      "select icd9_code, [icd-10-CM]
          from ckd_fracture_Grace.dbo.NHITW_ICD9CM_ICD10CM;"
    ) %>% 
    setDT()
  
  icd10_to_icd9[grepl(icd9_code, pattern = '^58[56].*'), ]
  icd10_to_icd9[grepl(`icd-10-CM`, pattern = '(^N18.[4569])|(^N19$)'), ]
  
# chr_basic ---------------------------------------------------------------
  chr_basic <- 
    sqlQuery(
      db_hospital,
      "select chr_no, HOSP_TYPE, BIRTH_DATE, 's' as hospital
  	        from DB_Hospital.dbo.v_chr_basic_s
  		        union all
         select chr_no, HOSP_TYPE, BIRTH_DATE, 't' as hospital
          	from DB_Hospital.dbo.v_chr_basic_t
          		union all
         select chr_no, HOSP_TYPE, BIRTH_DATE, 'w' as hospital
          	from DB_Hospital.dbo.v_chr_basic_w; "
    ) %>% 
    setDT()
  
  chr_basic_20 <- 
    chr_basic %>% 
    filter(as.integer(BIRTH_DATE) < 1040101 & 
             as.integer(BIRTH_DATE) > 0) %>% 
    mutate(
      age = ceiling((1040101 - as.integer(BIRTH_DATE))/10000)) %>% 
    filter(age >= 20)
  
  # odbcClose(db_hospital)
  
  
# opd_basic ---------------------------------------------------------------
  
  icd_pattern <- 
    str_c(
      c(paste0('icd9_code', 1:5),
        paste0('icd10_code', 1:5)),
      collapse = ', '
    )
  
  icd_condiction <- 
    str_c(
      paste0(str_split(paste0('icd9_code', 1:5), 
                       pattern = ',', 
                       simplify = T), " like '58[56]%' or ",
             str_split(paste0('icd10_code', 1:5), 
                       pattern = ',', 
                       simplify = T), " like 'N1[89]%' "),
      collapse = ' or '
    )
  
  opd_basic <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, opd_date, ',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_opd_basic_',
                 x,
                 ' where ',
                 icd_condiction,
                 ' ;')
        ) %>% 
          mutate(hospital = x)
      }
    ) %>% 
    # filter(opd_date <= 1100101) %>% 
    setDT()
  
  opd_icd9 <- 
    opd_basic[opd_date < 1050101, ] %>% 
    filter(
      grepl(icd9_code1, pattern = '58[56].*') |
        grepl(icd9_code2, pattern = '58[56].*') |
        grepl(icd9_code3, pattern = '58[56].*') |
        grepl(icd9_code4, pattern = '58[56].*') |
        grepl(icd9_code5, pattern = '58[56].*')
    )
  
  opd_icd10 <- 
    opd_basic[opd_date >= 1050101, ] %>% 
    filter(
      grepl(icd10_code1, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code2, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code3, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code4, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code5, pattern = '(^N18.[4569])|(^N19$)')
    )
  
  opd_result <- 
    rbind(
      opd_icd9,
      opd_icd10
    )
  
  opd_result_longer <- 
    opd_result %>% 
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
      ((icd_type == 'icd9_code' & opd_date < 1050101)|
        (icd_type == 'icd10_code' & opd_date >= 1050101)) &
        icd_code != ''
    ) %>% 
    rename(
      drug_date = opd_date
    ) %>% 
    setDT()
  
# ipd_basic ---------------------------------------------------------------
  icd_pattern <- 
    str_c(
      c(paste0('sdiag_code', 1:7),
        paste0('icd10_code', 1:8, '_out')),
      collapse = ', '
    )
  
  icd_condiction <- 
    str_c(
      str_c(
        paste0(str_split(paste0('sdiag_code', 1:7), 
                         pattern = ',', 
                         simplify = T), " like '58[56]%' or ",
               str_split(paste0('icd10_code', 1:8, '_out'), 
                         pattern = ',', 
                         simplify = T), " like 'N1[89]%' "),
        collapse = ' or '
      ),
      "or mdiag_code like '58[56]%' "
    )
  
  
  ipd_basic <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, ipd_date, mdiag_code, ',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_ipd_basic_',
                 x,
                 ' where ',
                 icd_condiction,
                 ' ;')
        ) %>% 
          mutate(hospital = x,
                 sdiag_code7 = as.character(sdiag_code7))
      }
    ) %>% 
    # filter(ipd_date <= 1100101) %>% 
    setDT()
  
  ipd_icd9 <- 
    ipd_basic[ipd_date < 1050101,] %>% 
    filter(
      grepl(sdiag_code1, pattern = '58[56].*') |
        grepl(sdiag_code2, pattern = '58[56].*') |
        grepl(sdiag_code3, pattern = '58[56].*') |
        grepl(sdiag_code4, pattern = '58[56].*') |
        grepl(sdiag_code5, pattern = '58[56].*') |
        grepl(sdiag_code6, pattern = '58[56].*') |
        grepl(sdiag_code7, pattern = '58[56].*') |
        grepl(mdiag_code, pattern = '58[56].*') 
    )
  
  ipd_icd10 <- 
    ipd_basic[ipd_date >= 1050101,] %>% 
    filter(
      grepl(icd10_code1_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code2_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code3_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code4_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code5_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code6_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code7_out, pattern = '(^N18.[4569])|(^N19$)') |
        grepl(icd10_code8_out, pattern = '(^N18.[4569])|(^N19$)') 
    )
  
  ipd_result <- 
    rbind(
      ipd_icd9,
      ipd_icd10
    )
  
  ipd_result_longer <- 
    bind_rows(
      ipd_icd9 %>% 
        pivot_longer(
          cols = contains('code'),
          names_to = 'icd_type',
          values_to = 'icd_code'
        ) %>% 
        mutate(
          icd_type = str_replace(icd_type, 
                                 pattern = '([0-9]$)|([0-9]_out$)', 
                                 replacement = '')
        )
    )
  
  
  ipd_result_longer <- 
    ipd_result %>% 
    pivot_longer(
      cols = contains('code'),
      names_to = 'icd_type',
      values_to = 'icd_code'
    ) %>% 
    mutate(
      icd_type = str_replace(icd_type, 
                             pattern = '([0-9]$)|([0-9]_out$)', 
                             replacement = '')
    ) %>%
    rename(
      drug_date = ipd_date
    ) %>% 
    filter(
      ((icd_type != 'icd10_code' & drug_date < 1050101)|
        (icd_type == 'icd10_code' & drug_date >= 1050101)) &
        icd_code != ''
    ) %>% 
    setDT()
  
# epd_his -----------------------------------------------------------------
  icd_pattern <- 
    str_c(
      c(paste0('icd9_code', 1:2)),
      collapse = ', '
    )
  
  icd_condiction <- 
    str_c(
      paste0(str_split(icd_pattern, 
                       pattern = ',', 
                       simplify = T), " like '58[56]%'"),
      collapse = ' or '
    )
  
  epd_basic <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0('select distinct chr_no, epd_date, ',
                 icd_pattern,
                 ' from DB_Hospital.dbo.v_epd_his_',
                 x,
                 ' where ',
                 icd_condiction,
                 ' ;')
        ) %>% 
          mutate(hospital = x)
      }
    ) %>%  
    # filter(epd_date <= 1100101) %>% 
    setDT()
  
  epd_result_longer <- 
    epd_basic %>% 
    pivot_longer(
      cols = contains('code'),
      names_to = 'icd_type',
      values_to = 'icd_code'
    ) %>% 
    mutate(
      icd_type = str_replace(icd_type,
                             pattern = '[0-9]$',
                             replacement = '')
    ) %>% 
    rename(
      drug_date = epd_date
    )

  

# result_longer -----------------------------------------------------------
  result_longer <- 
    rbind(
      opd_result_longer[between(drug_date, 1040101, 1061231), ],
      ipd_result_longer[between(drug_date, 1040101, 1061231), ],
      epd_result_longer
    ) %>% 
    filter(
      (grepl(icd_type, pattern = '(icd9)|(mdiag)|(sdiag)') &
         grepl(icd_code, pattern = '58[56].*') &
         drug_date < 1050101) |
        (grepl(icd_type, pattern = 'icd10') &
           grepl(icd_code, pattern = '(^N18.[4569])|(^N19$)') &
           drug_date >= 1050101)
    ) %>%
    inner_join(
      chr_basic_20,
      by = c('chr_no' = 'chr_no',
             'hospital' = 'hospital')
    )
  