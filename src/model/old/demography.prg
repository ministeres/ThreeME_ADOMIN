subroutine BLOCK_Demography

  '***************************************************************************************************************************************
  '******************************************** BLOCK 7 : DEMOGRAPHY ****************************************************
  '**************************************************************************************************************************************
 'equation 7.1
  {%modelname}.equation d(log(empl[coh]))=d(log(L)) if empl[coh] <> 0 where coh in {%list_coh}

'   'equation 7.2
'   For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
'     if @elem(empl_{%coh},%baseyear) <> 0 then
'       {%modelname}.append d(log(empl_{%coh}))=d(log(L))
'     endif
'   next

 'equation 7.2
  {%modelname}.equation LF[coh] = PARTR[coh] * WAPop[coh] where coh in {%list_coh}

'   'equation 7.3
'   For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
'     {%modelname}.append LF_{%coh}=PARTR_{%coh}*WAPop_{%coh}
'   next

 'equation 7.3
 {%modelname}.equation d(PARTR_n[coh]) = d(PARTR_trend[coh]) + betaEmp[coh] * d(UnR[coh]) where coh in {%list_coh}

'' 'equation 7.4
'   For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
'     {%modelname}.append d(PARTR_n_{%coh})=d(PARTR_trend_{%coh}) + betaEmp_{%coh}*d(UnR_{%coh})
'   next

 'equation 7.4
 {%modelname}.equation Un[coh] = LF[coh] - Empl[coh] where coh in {%list_coh}

'   'equation 7.5
'   For %coh  M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
'     {%modelname}.append Un_{%coh}=LF_{%coh}- Empl_{%coh}
'   next

 'equation 7.5

 {%modelname}.equation UnR[coh] = Un[coh] / LF[coh] where coh in {%list_coh}
 {%modelname}.equation UnR[age] = Un[age] / LF[age] where age in {%list_age}
 {%modelname}.equation UnR[sex] = Un[sex] / LF[sex] where sex in {%list_sex}

'   'equation 7.6-7.8
'   For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65 M W 15 20 25 55 60 65
'     {%modelname}.append UnR_{%coh}=Un_{%coh}/LF_{%coh}
'   next

 'equation 7.6
  {%modelname}.append UnR_TOT=Un_TOT/LF_TOT

  'equation 7.7
  {%modelname}.equation UN_|age| = UN_M|age| + UN_W|age| where age in 15 20 25 55 60 65

'   'equation 7.10
'   For %age 15 20 25 55 60 65
'     {%modelname}.append UN_{%age}= UN_M{%age}+UN_W{%age}
'   next

 'equation 7.8
 {%modelname}.equation UN_|sex| = sum(UN_|sex||age| where age in 15 20 25 55 60 65) where sex in M W

'   'equation 7.11
'   For %sex W M
'     %equation = "UN_"+%sex+"=0"
'     For %age 15 20 25 55 60 65
'       %equation = %equation +"+ UN_"+%sex+%age
'     next
'     {%modelname}.append {%equation}
'   next

 'equation 7.9
 {%modelname}.equation UN_TOT = sum(UN[sex] where sex in {%list_sex})

'   'equation 7.12
'   %equation = "UN_TOT=0"
'   For %sex W M
'     %equation = %equation +"+ UN_"+%sex
'   next
'   {%modelname}.append {%equation}

 'equation 7.10
 {%modelname}.equation LF_|age| = sum(LF_|sex||age| where sex in M W) where age in 15 20 25 55 60 65

'   'equation 7.13
'   For %age 15 20 25 55 60 65
'     {%modelname}.append LF_{%age}= LF_M{%age}+LF_W{%age}
'   next

 'equation 7.11 & 7.12
 {%modelname}.equation LF_|sex| = sum(LF_|sex||age| where age in 15 20 25 55 60 65) where sex in M W

 {%modelname}.equation LF_TOT = sum(LF_|sex| where sex in M W)


'   'equation 7.14
'   For %sex W M
'     %equation = "LF_"+%sex+"=0"
'     For %age 15 20 25 55 60 65
'       %equation = %equation +"+ LF_"+%sex+%age
'     next
'     {%modelname}.append {%equation}
'   next
'
'   'equation 7.15
'   %equation = "LF_TOT=0"
'   For %sex W M
'     %equation = %equation +"+ LF_"+%sex
'   next
'   {%modelname}.append {%equation}

  '**************************************************************************************************************************************
  '******************************************** END BLOCK 7 : DEMOGRAPHY ************************************************
  '**************************************************************************************************************************************


endsub