

# Insert path to data files
dirtoken <- ""
# Insert path to working data
worktoken <- ""

keepvars0209 <-
  c("questid2", "year",
    "verep", "vestr", "analwt_c", "analwc1",
    "age2", "catag6", "newrace2", "irsex", "irwrkstat", "irmarit",
    "income",
    "eduhighcat", "schenrl",
    "eduschlgo", "collenr", "collenr2", "collenrlst", "collenrft", "collenrlft",
    "anyhlti2", "irprvhlt", "irmcdchp", "irmedicr",
    "educcat2", "coutyp2", "coutyp4",
    "difobther", "difobtlsd",
    "rskyfqtes"
  )

keepvars2123 <- 
  c("QUESTID2", 
    "verep", "VESTR_C", "ANALWT2_C",
    "AGE3", "CATAG6", "NEWRACE2", "irsex", "irwrkstat", "irmarit",
    "income",
    "eduschlgo",
    "ANYHLTI2", "irprvhlt", "irmcdchp", "irmedicr",
    "eduhighcat", "COUTYP4",
    "difobther", "difobtlsd",
    "rskyfqtes"
  )


read_and_process_data <- function(file_path, col_select, year = NULL) {
  data <- haven::read_dta(file_path, col_select = col_select) %>%
    haven::zap_labels() %>%
    rename_all(tolower)
  
  if (!is.null(year)) {
    data <- data %>% add_column(year = year)
  }
  
  return(data)
}

id0209 <- read_and_process_data(str_c(dirtoken, "NSDUH_2002_2019.dta"), keepvars0209)
id2021 <- read_and_process_data(str_c(dirtoken, "NSDUH_2021_v2.dta"), keepvars2123, '2021')
id2022 <- read_and_process_data(str_c(dirtoken, "NSDUH_2022.dta"), keepvars2123, '2022')
id2023 <- read_and_process_data(str_c(dirtoken, "NSDUH_2023.dta"), keepvars2123, '2023')


# id0209 <-
#   haven::read_dta(
#     str_c(dirtoken, "NSDUH_2002_2019.dta"),
#     col_select =  all_of(keepvars0209)) %>%
#     haven::zap_labels() %>%
#     mutate(questid2 = as.numeric(paste0(questid2)))

# id2021 <- haven::read_dta(
#   str_c(dirtoken, "NSDUH_2021_v2.dta"),
#   col_select = all_of(keepvars2123)) %>%
#   haven::zap_labels() %>%
#   rename_all(tolower) %>% add_column(year = '2021')

# id2022 <- haven::read_dta(
#   str_c(dirtoken, "NSDUH_2022.dta"),
#   col_select = all_of(keepvars2123)
# ) %>% haven::zap_labels() %>%
#   rename_all(tolower) %>% add_column(year = '2022')

# id2023 <- haven::read_dta(
#   str_c(dirtoken, "NSDUH_2023.dta"),
#   col_select = all_of(keepvars2123)) %>%
#   haven::zap_labels() %>%
#   rename_all(tolower) %>% add_column(year = '2023')

datasets_to_merge <- list(id0209, id2021, id2022, id2023)

ident0223 <- bind_rows(datasets_to_merge) %>%
  mutate(
    vestr_ = if_else(year < 2020, vestr, vestr_c),
    age_cat = case_when(
      (age2 %in% c(7:14) | age3 %in% c(4:8)) ~ "18 - 34",
      (age2 == 15 | age3 == 9)  ~ "35 - 49",
      (age2 == 16 | age3 == 10) ~ "50 - 64",
      (age2 == 17 | age3 == 11) ~ "65+",
      .default = "12-17"
    ),
    catag6 = factor(catag6, levels = c(1:6) , labels = c('12-17', '18–25', '26–34', '35–49', '50–64', '65+') 
    ),
    irsex = factor(irsex, levels = c(1, 2), labels = c('Male', 'Female')
    ),
    race_cat = case_when(
      newrace2 == 1 ~ "NH White",
      newrace2 == 2 ~ "NH Black",
      newrace2 == 7 ~ "Hispanic",
      newrace2 %in% c(4, 5) ~ "AAPI",
      newrace2 == 3 ~ "Native/Am",
      newrace2 == 6 ~ "NH > 1 Race",
    ),
    msa = if_else(year < 2015, coutyp2, coutyp4) %>%
      factor(.,
      levels = 1:3, labels = c('Large Metro', 'Small Metro', 'Nonmetro')
      ),
    ins_cat = case_when(
      irmcdchp == 1 | irmedicr == 1  ~ "Medicare/Medicaid/CHP",
      irprvhlt == 1  ~ "Private",
      anyhlti2 == 2  ~ "No Insurance",
      .default = "Other"
    ) ,
    curr_school = case_when(
      eduschlgo == 1 | schenrl == 1 | collenr2 %in% c(1,2) | collenrlst %in% c(1,2) | collenrft == 1 ~ 1,
      .default = 0
    ),
    irmarit = factor(irmarit,
                     levels = c(1:4, 99, -9) ,labels = c('Married', 'Widowed', 'Div/Separated', 'Single', 'N/A','Not Collected')),
    irwrkstat = factor(irwrkstat,
                       levels = c(1:4, 99),
                       labels = c('Employed FT', 'Employed PT', 'Unemployed', 'Other', '12-14')),
    rskyfqtes = factor(rskyfqtes,
                       levels = c(1:4, 85, 94, 97, 98),
                       labels = c('Never', 'Seldom', 'Sometimes', 'Always', 'Oth', 'Oth', 'Oth', 'Oth')),
    eduhighcat = if_else(eduhighcat == -9, educcat2, eduhighcat) %>%
      factor(., levels = c(1:5), labels = c('Less HS','HS Grad','Some Coll/Assoc Deg.','Graduate', '12-17')),
    income = factor(income,
                    levels = c(1:4),
                    labels = c("Less 20K", "20-49K", "50-75K", "Greater 75K")),
    difobtlsd = factor(difobtlsd,
                       levels = c(0, 1),
                       labels = c('Otherwise', 'Fairly Easy or Very')
    ), 
    difobther = factor(difobther,
                       levels = c(0, 1),
                       labels = c('Otherwise', 'Fairly Easy or Very')
    ),
    period_indic = case_when(
      as.numeric(paste0(year)) < 2020 ~ "< 2020",
      as.numeric(paste0(year)) > 2020 ~ "> 2020",
      .default = ""
    ),
  ) 

rm(id0209, id2021, id2022, id2023)
rm(datasets_to_merge)

ident0223 %>% select(eduhighcat,year) %>% table()

substance_vars0209 <-
  c("questid2","year",
    "hallucrec",
    "halyr","hallucyr",
    "irhallucrec",
    "irherrc",
    "abuseher", "abusepypnr", 
    "udpyopi",
    "udpypnr","udpyhrpnr",
    "hallrec",
    "hallucflag", "lsdflag", "pcpflag", "ecstmoflag", "ketminflag", "salviaflag", "damtfxflag", 
    "mesc", "mesc2" , "psilcy", "psilcy2", "peyote", "peyote2",
    "pnranyflag", "trqanyflag", "stmanyflag", "sedanyflag", "inhalflag","mrjflag", "herflag", "anlflag", "methamflag", 
    "inhflag", "inhalever",
    "ketaflgr", "salvflgr" ,"trypflgr", "damtfxflag",  
    "ecsflag", "sedflag",  "cocflag", "cocever", "stmflag", 
    "cocyr", "methamyr", "mrjyr", "ketminyr", "ketayrr", "ecstmoyr", "ecsyr", "lsdyr", "irecstmorec",
    "irhallucyfu", "irhalyfu", 
    "abodher",  "abodcoc", "abodalc", "abodmrj",  "udpymth", "udpyhal", "udpypnr",
    "bngdrkmon"
  )

substance_vars2123  <-
  c("QUESTID2",
    "hallucrec",
    "hallucyr",
    "irhallucrec",
    "irherrc",
    "irhallucyfu",
    "hallucflag", "lsdflag", "pcpflag", "ecstmoflag", "ketminflag", "salviaflag", "damtfxflag",
    "mescever", "peyoteever", "psilcyever",
    "pnranyflag", "trqanyflag", "stmanyflag", "sedanyflag", "inhalflag","mrjflag", "herflag",  "methamflag", "inhalever",
    "cocflag", "cocever",
    "cocyr", "methamyr", "mrjyr", "ketminyr", "ecstmoyr", "lsdyr", "irecstmorec",
    "ketminyr",
    "UD5OPIANY", "UDYR5PNRANY", "IRPYUD5HER",
    "PYUD5ALC", "PYUD5COC", "PYUD5MTH", "PYUD5HAL", "PYUD5MRJ",
    "bngdrkmon"
  )

subst0209 <- read_and_process_data(str_c(dirtoken, "NSDUH_2002_2019.dta"), substance_vars0209)
subst21 <- read_and_process_data(str_c(dirtoken, "NSDUH_2021_v2.dta"), substance_vars2123, '2021')
subst22 <- read_and_process_data(str_c(dirtoken, "NSDUH_2022.dta"), substance_vars2123, '2022')
subst23 <- read_and_process_data(str_c(dirtoken, "NSDUH_2023.dta"), substance_vars2123, '2023')


# subst0209 <- haven::read_dta(
#   "/Users/amherst/Library/CloudStorage/Box-Box/CUPS Population Study/NSDUH Data/DTA/NSDUH_2002_2019.dta",
#   col_select =  all_of(substance_vars0209)) %>% 
#   haven::zap_labels()

# subst21 <- haven::read_dta(
#   "/Users/amherst/Library/CloudStorage/Box-Box/CUPS Population Study/NSDUH Data/DTA/NSDUH_2021_v2.DTA",
#   col_select =  all_of(substance_vars2123)) %>%
#   haven::zap_labels() %>% add_column(year = '2021') %>% rename_all(tolower)

# subst22 <- haven::read_dta(
#   "/Users/amherst/Library/CloudStorage/Box-Box/CUPS Population Study/NSDUH Data/DTA/NSDUH_2022.DTA",
#   col_select =  all_of(substance_vars2123)) %>%
#   haven::zap_labels() %>% add_column(year = '2022') %>% rename_all(tolower)

# subst23 <- haven::read_dta(
#   "/Users/amherst/Library/CloudStorage/Box-Box/CUPS Population Study/NSDUH Data/DTA/NSDUH_2023.DTA",
#   col_select =  all_of(substance_vars2123)) %>% 
#   haven::zap_labels() %>% add_column(year = '2023') %>% rename_all(tolower)


halluc_vars <- c("psilcy_ever", "lsd_ever", "pcp_ever", "ecstasy_ever",
                 "ket_ever", "salv_ever", "dmt_ever",  "mesc_ever", "peyote_ever")

bin_conversion <- function(x, value = 1) {
  if_else(x == value, 1, 0, missing = 0)
}
bin_rc_conversion <- function(x, y, value = 1) {
  if_else(x == value | y == value, 1, 0, missing = 0)
}


subst02092  <- subst0209 %>%
  mutate(
        questid2 = as.numeric(paste0(questid2)),
        lsd_ever = bin_conversion(lsdflag),
        pcp_ever = bin_conversion(pcpflag),
        psilcy_ever = bin_conversion(psilcy2),
        mesc_ever = bin_conversion(mesc2),
       
        ecstasy_ever = bin_rc_conversion(ecstmoflag, ecsflag),
        ket_ever  = bin_rc_conversion(ketminflag, ketaflgr),
        salv_ever = bin_rc_conversion(salviaflag, salvflgr),
        dmt_ever  = bin_rc_conversion(damtfxflag, trypflgr),
        peyote_ever = bin_conversion(peyote2),

 
        hallucly = bin_rc_conversion(halyr, hallucyr),
        #  hallucly = if_else(halyr == 1 | hallucyr == 1, 1, 0, missing = 0),
         
        hallucflag = case_when(
          hallucflag >= 0 ~ hallucflag,
          hallucflag == -9 & hallrec == 91 ~ 0,
          hallucflag == -9 &  !hallrec == 91 ~ 1,
         ),
         
         # Opioid Use
        opioid_use = bin_conversion(udpyopi),
        opioid_use_f = factor(opioid_use, levels = c(0, 1), labels = c('Negative', 'Positive')),
        pain_use = bin_conversion(udpypnr),
        her_use = bin_conversion(udpyhrpnr),

        any_opi = bin_rc_conversion(pnranyflag, herflag),
        mj_ever  = bin_conversion(mrjflag),
        coc_ever = bin_conversion(cocflag),
        stm_ever = bin_conversion(stmflag),
        trq_ever = bin_conversion(trqanyflag),
        inh_ever = bin_conversion(inhflag),
        sed_ever = bin_conversion(sedflag),
        meth_ever = bin_conversion(methamflag),
         
         # Classic Hallucinogens
         halluc_p    = if_else(psilcy_ever == 1 | lsd_ever == 1 | mesc_ever == 1 | peyote_ever == 1 | dmt_ever == 1, 1, 0 , missing = 0),
        
         # CUPS Hallucinogens
         halluc_ever = if_else(psilcy_ever == 1 | lsd_ever == 1 | pcp_ever == 1 | ecstasy_ever==1 | mesc_ever == 1 | peyote_ever == 1 | ket_ever == 1 | salv_ever == 1 | dmt_ever == 1, 1, 0, missing = 0),
         
         any_halluc_ever  = if_else(lsd_ever==1|pcp_ever==1|ecstasy_ever==1|ket_ever==1|peyote_ever==1|psilcy_ever==1|salv_ever==1|dmt_ever==1|mesc_ever==1 , 1, 0),
         any_halluc_ever2 = if_else(lsd_ever==1|pcp_ever==1|ecstasy_ever==1|ket_ever==1|psilcy_ever==1|salv_ever==1|dmt_ever==1 , 1, 0), # Peyote/Mescaline Excl 
         other_hallucs    = if_else(any_halluc_ever == 1 & psilcy_ever == 0, 1, 0),
         other_hallucs2   = if_else(any_halluc_ever2 == 1 & psilcy_ever == 0, 1, 0),
   
        # Problem use
         binge_drink = bin_conversion(bngdrkmon),
         binge_drink_f = factor(bngdrkmon, levels= c(0,1), labels = c('Negative', 'Positive')),
         
         #Use Disorder Modifications
         py_cocmeth = bin_rc_conversion(cocyr, methamyr),
         cocmeth_use = bin_rc_conversion(abodcoc, abodmth),
         cocmeth_use_f = factor(cocmeth_use, levels = c(0, 1), labels = c('Negative', 'Positive')),
         
         mrj_use = bin_conversion(abodmrj),
         mrj_use_f = factor(mrj_use, levels = c(0, 1), labels = c('Negative', 'Positive')),
         
         alc_use = bin_conversion(abodalc),
         alc_use_f = factor(alc_use, levels = c(0, 1), labels = c('Negative', 'Positive')),
  )

subst2123 <-
  bind_rows(subst21, subst22, subst23) %>%
  mutate(questid2 = as.numeric(paste0(questid2)),
         lsd_ever = if_else(lsdflag == 1, 1, 0, missing = 0),
         pcp_ever = if_else(pcpflag == 1, 1, 0, missing = 0),
         ecstasy_ever = if_else(ecstmoflag == 1, 1, 0, missing = 0),
         ket_ever = if_else(ketminflag == 1, 1, 0, missing = 0),
         salv_ever = if_else(salviaflag == 1, 1, 0, missing = 0),
         dmt_ever = if_else(damtfxflag == 1, 1, 0, missing = 0),
         mesc_ever = if_else(mescever == 1, 1, 0, missing = 0),
         peyote_ever = if_else(peyoteever == 1, 1, 0, missing = 0),
         psilcy_ever = if_else(psilcyever == 1, 1, 0, missing = 0),
         halluc_p = if_else(psilcy_ever ==1 | lsd_ever == 1 | mesc_ever == 1 | peyote_ever == 1 | dmt_ever == 1, 1, 0, missing = 0),
         
         hallucly = if_else(hallucyr == 1, 1, 0, missing = 0),
         
         #Opioid use
         opioid_use = if_else(ud5opiany == 1, 1, 0, missing = 0),
         opioid_use_f = factor(opioid_use, levels = c(0, 1), labels = c('Negative', 'Positive')),
         pain_use = if_else(udyr5pnrany == 1, 1, 0, missing = 0),
         her_use = if_else(irpyud5her == 1, 1, 0, missing = 0),
         
         any_opi = if_else(pnranyflag == 1 | herflag == 1,  1, 0),
         # Substance use
         mj_ever  = if_else(mrjflag == 1, 1, 0, missing = 0),
         coc_ever = if_else(cocflag == 1, 1, 0, missing = 0),
         stm_ever = if_else(stmanyflag == 1, 1, 0, missing = 0),
         trq_ever = if_else(trqanyflag == 1, 1, 0, missing = 0),
         inh_ever = if_else(inhalflag == 1, 1, 0, missing = 0),
         sed_ever = if_else(sedanyflag == 1, 1, 0, missing = 0),
         meth_ever = if_else(methamflag == 1, 1, 0, missing = 0),
         
         # CUPS Hallucinogens
         halluc_ever = if_else(psilcy_ever == 1 | lsd_ever == 1 | pcp_ever == 1 | ecstasy_ever==1 | mesc_ever == 1 | peyote_ever == 1 | ket_ever == 1 | salv_ever == 1 | dmt_ever == 1, 1, 0, missing = 0),
         any_halluc_ever  = if_else(lsd_ever==1|pcp_ever==1|ecstasy_ever==1|ket_ever==1|peyote_ever==1|psilcy_ever==1|salv_ever==1|dmt_ever==1|mesc_ever==1 , 1, 0),
         any_halluc_ever2 = if_else(lsd_ever==1|pcp_ever==1|ecstasy_ever==1|ket_ever==1|psilcy_ever==1|salv_ever==1|dmt_ever==1 , 1, 0), # Peyote/Mescaline Excl 
         other_hallucs    = if_else(any_halluc_ever == 1 & psilcy_ever == 0, 1, 0),
         other_hallucs2   = if_else(any_halluc_ever2 == 1 & psilcy_ever == 0, 1, 0), 
         
         # py use
         py_cocmeth = if_else(cocyr == 1 | methamyr == 1, 1, 0),
         
         # problem use
         binge_drink = if_else(bngdrkmon == 1, 1, 0, missing = 0),
         binge_drink_f = factor(bngdrkmon, levels= c(0, 1), labels = c('Negative', 'Positive')),
         
         #Use Disorder Modifications
         cocmeth_use = if_else(pyud5coc == 1 | pyud5mth == 1, 1, 0, missing = 0 ) ,
         cocmeth_use_f = factor(cocmeth_use, levels= c(0, 1), labels = c('Negative', 'Positive')),
         
         mrj_use = if_else(pyud5mrj == 1, 1, 0, missing = 0),
         mrj_use_f = factor(mrj_use, levels= c(0, 1), labels = c('Negative', 'Positive')),
         
         alc_use = if_else(pyud5alc == 1, 1, 0, missing = 0),
         alc_use_f = factor(alc_use, levels= c(0, 1), labels = c('Negative', 'Positive')),
  )

subst0223 <- bind_rows(subst02092, subst2123)

eval_ <- inner_join(ident0223, subst0223, by = c("year", "questid2")) %>%
  mutate(
         peymesc = if_else(peyote_ever == 1 | mesc_ever == 1, 1, 0 , missing = 0),
         
         
         total_hallucs = rowSums(across(all_of(halluc_vars)), na.rm = TRUE),
         total_hallcus2 = rowSums(across(all_of(halluc_vars[1:7])), na.rm = TRUE),
         
         # total_hallucs2 = lsd_ever + pcp_ever + ecstasy_ever + ket_ever + psilcy_ever + salv_ever + dmt_ever,
         # total_hallucs = total_hallucs2 + peyote_ever + mesc_ever,
         
         psilcy_only = if_else(psilcy_ever == 1 & (total_hallucs == 1), 1, 0),
         lsd_only = if_else(lsd_ever == 1 & (total_hallucs == 1), 1, 0),
         mdma_only = if_else(ecstasy_ever == 1 & (total_hallucs == 1), 1, 0),
         nlhallucs_only = if_else(halluc_ever == 1 & (lsd_ever + psilcy_ever + ecstasy_ever == 0), 1, 0),
         
         psilcy_only2 = if_else(psilcy_ever == 1 & (total_hallucs2 == 1), 1, 0),
         psilcy_plus = if_else(psilcy_ever == 1 & (total_hallucs > 1), 1, 0),
         psilcy_plus2 = if_else(psilcy_ever == 1 & (total_hallucs2 > 1), 1, 0),
         lsd_plus = if_else(lsd_ever == 1 & (total_hallucs > 1), 1, 0),
         mdma_plus = if_else(ecstasy_ever == 1 & (total_hallucs > 1), 1, 0),
         syear  = as.numeric(paste0(year)),
         
         halluc_year = if_else(syear < 2015, irhalyfu, irhallucyfu),
         
         yfu = ifelse(halluc_year == 9999, NA, as.numeric(paste0(halluc_year))),
         firstuse = if_else(halluc_ever == 1, yfu, NA),
         
         new_use = if_else(syear - firstuse <= 1, 1, 0, missing = 0),
         new_lsd = if_else(new_use == 1 & lsd_ever == 1, 1, 0, missing = 0),
         new_mdma = if_else(new_use == 1 & ecstasy_ever == 1, 1, 0, missing = 0),
         new_psil = if_else(new_use == 1 & psilcy_ever == 1, 1, 0, missing = 0),
         new_nl_hallucs = if_else(new_use == 1 & (psilcy_ever == 0 & ecstasy_ever == 0 & lsd_ever == 0), 1, 0, missing = 0),
         
         hallucly = if_else(hallucly == 1 & halluc_ever == 1, 1, 0, missing = 0),
         
         pastyr_lsd = if_else(lsdyr == 1, 1, 0, missing = 0),
         pastyr_ecs = if_else(ecsyr == 1 | irecstmorec %in% c(1,2), 1, 0, missing = 0),
         pastyr_psil2 = if_else(new_psil == 1 | (psilcy_only == 1 & hallucly == 1) , 1, 0, missing = 0),
         pastyr_nls = if_else(hallucly == 1 & (pastyr_lsd == 0 & pastyr_ecs == 0 & pastyr_psil2 == 0 ) , 1, 0, missing = 0),
)

eval_ %>% select(tp2,total_hallucs) %>% table()

saveRDS(eval_,  str_c(worktoken, "Temp022125.RDA"))
