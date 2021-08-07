ptm <- proc.time()  
  raw_exper_sign <- 
    sqlQuery(
      db_hospital,
      paste0(
        "select 'w' as hospital, TUBE_NUMBER, GROUP_CODE, EXPER_CLASS, EXPER_CODE,
       LAB_NO, CHR_NO, ID_NO, EXPER_DATE, EXPER_TIME, FEE_NO,
       TAKE_DATE, TAKE_TIME, EXPER_DATA, EXPER_DATA2, EXPER_DATA3, EXPER_DATA4, EXPER_DATA5
              from DB_Hospital.dbo.v_exper_sign_w
              where CHR_NO in ( ",
        str_c(
          "'",
          chr_no_cohort[hospital == 'w', ]$chr_no,
          "'",
          collapse = ', '
        ),
        ") ;"
      )
    ) %>% 
    filter(EXPER_CLASS == 1) %>% 
    setDT(key = c('CHR_NO', 'GROUP_CODE', 'TUBE_NUMBER',
                  'EXPER_CODE', 'EXPER_CLASS'))
    
  names(raw_exper_sign) <- names(raw_exper_sign) %>% tolower()
  
  exper_sign <- 
    bind_rows(
      raw_exper_sign[group_code %chin% c('F090010', 'F09001C'),] %>% 
        mutate(exam_item = 'chol'),
      raw_exper_sign[group_code %chin% c('F090040', 'F09004C'),] %>% 
        mutate(exam_item = 'trig'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'nrbc'),] %>% 
        mutate(exam_item = 'nrbc'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'baso'),] %>% 
        mutate(exam_item = 'baso'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'mono'),] %>% 
        mutate(exam_item = 'monocyte'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'neut'),] %>% 
        mutate(exam_item = 'neutrophil'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'lymp'),] %>% 
        mutate(exam_item = 'lymp'),
      raw_exper_sign[group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'eos'),] %>% 
        mutate(exam_item = 'eosinophil'),
      raw_exper_sign[group_code %chin% c('F09011B', 'F09011C', 'F09011CA') & grepl(tolower(exper_code), pattern = 'ca'),] %>% 
        mutate(exam_item = 'ca'),
      raw_exper_sign[group_code %chin% c('F090050' ,'F09005C' ,'F09005C1' ,'F09005CAC' ,'F09005Z') & tolower(exper_code) == 'glu',] %>% 
        mutate(exam_item = 'glu_ac'),
      raw_exper_sign[group_code %chin% c('F09021C', 'F09021CA'),] %>% 
        mutate(exam_item = 'na'),
      raw_exper_sign[group_code %chin% c('F090150' ,'F09015B' ,'F09015C' ) & grepl(tolower(exper_code), pattern = 'cre'),] %>% 
        mutate(exam_item = 'creatinine'),
      raw_exper_sign[group_code %chin% c('F08006C' ,'F080110' ,'F08011C' ) & grepl(tolower(exper_code), pattern = 'pdw'),] %>% 
        mutate(exam_item = 'pdw'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C') & grepl(tolower(exper_code), pattern = 'r-cv'),] %>% 
        mutate(exam_item = 'r_cv'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08083C') & tolower(exper_code) == 'mch',] %>% 
        mutate(exam_item = 'mch'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08084C') & tolower(exper_code) == 'mchc',] %>% 
        mutate(exam_item = 'mchc'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08127C') & tolower(exper_code) == 'mcv',] %>% 
        mutate(exam_item = 'mcv'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C') & tolower(exper_code) == 'rbc',] %>% 
        mutate(exam_item = 'rbc'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08006C') & tolower(exper_code) == 'plt',] %>% 
        mutate(exam_item = 'plt'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08002C') & tolower(exper_code) == 'wbc',] %>% 
        mutate(exam_item = 'wbc'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08004C', 'F08082C', 'F08004CA', 'F08004CB') & tolower(exper_code) == 'hct',] %>% 
        mutate(exam_item = 'hct'),
      raw_exper_sign[group_code == 'F12116A',] %>% 
        mutate(exam_item = 'ferritin'),
      raw_exper_sign[group_code %chin% c('F08001C' ,'F08003C' ,'F08003CA', 'F08003CB', 'F08003CG', 'F080110', 'F08011C', 'F08012C', 'F08082C') & tolower(exper_code) == 'hgb',] %>% 
        mutate(exam_item = 'hgb')
    ) %>% 
    setDT(key = c('chr_no', 'group_code', 'tube_number',
                  'exper_code', 'exper_class', 'exam_item'))
  
  exper_sign <- 
    raw_exper_sign %>% 
    mutate(
      exam_item = 
        case_when(
          group_code %chin% c('F090010', 'F09001C') ~ 'chol',
          group_code %chin% c('F090040', 'F09004C') ~ 'trig',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'nrbc') ~ 'nrbc',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'baso') ~ 'baso',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'mono') ~ 'monocyte',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'neut') ~ 'neutrophil',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'lymp') ~ 'lymp',
          group_code == 'F08013C' & grepl(tolower(exper_code), pattern = 'eos') ~ 'eosinophil',
          group_code %chin% c('F09011B', 'F09011C', 'F09011CA') & grepl(tolower(exper_code), pattern = 'ca') ~ 'ca',
          group_code %chin% c('F090050' ,'F09005C' ,'F09005C1' ,'F09005CAC' ,'F09005Z') & tolower(exper_code) == 'glu' ~ 'glu_ac',
          group_code %chin% c('F09021C', 'F09021CA') ~ 'na',
          group_code %chin% c('F090150' ,'F09015B' ,'F09015C' ) & grepl(tolower(exper_code), pattern = 'cre') ~ 'creatinine',
          group_code %chin% c('F08006C' ,'F080110' ,'F08011C' ) & grepl(tolower(exper_code), pattern = 'pdw') ~ 'pdw',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C') & grepl(tolower(exper_code), pattern = 'r-cv') ~ 'r_cv',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08083C') & tolower(exper_code) == 'mch' ~ 'mch',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08084C') & tolower(exper_code) == 'mchc' ~ 'mchc',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08127C') & tolower(exper_code) == 'mcv' ~ 'mcv',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C') & tolower(exper_code) == 'rbc' ~ 'rbc',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08006C') & tolower(exper_code) == 'plt' ~ 'plt',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08014C', 'F08082C', 'F08002C') & tolower(exper_code) == 'wbc' ~ 'wbc',
          group_code %chin% c('F08001C' ,'F080110' ,'F08011C', 'F08012C', 'F08004C', 'F08082C', 'F08004CA', 'F08004CB') & tolower(exper_code) == 'hct' ~ 'hct',
          group_code == 'F12116A' ~ 'ferritin',
          group_code %chin% c('F08001C' ,'F08003C' ,'F08003CA', 'F08003CB', 'F08003CG', 'F080110', 'F08011C', 'F08012C', 'F08082C') & tolower(exper_code) == 'hgb' ~ 'hgb',
          TRUE ~ 'undefined'
        )
    ) %>% 
    filter(exam_item != 'undefined') %>% 
    setDT(key = c('chr_no', 'group_code', 'tube_number',
          'exper_code', 'exper_class', 'exam_item'))
proc.time() - ptm
