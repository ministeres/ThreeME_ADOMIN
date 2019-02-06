subroutine BLOCK_Other
  '***************************************************************************************************************************************
  '******************************************** BLOCK 8 : OTHER EQUATIONS ****************************************************
  '**************************************************************************************************************************************

  ' AJUSTMENT EQUATIONS AND ANTICIPATION
  'PROBLEME! Probleme de calcul avec IA "Log of non-positive number in DO_AMOD SOLVE"
  ' Ajustment Line 1-4

  '!step_1=0
  'For %varname  IA L E MAT
  'For %varname  Kbis L E MAT

 'equation 8.1
 {%modelname}.equation  log(|V|[s]) = ADJUST(1 + $V, 1) * log(|V|_n[s]) + (1 - ADJUST(1 + $V, 1)) * (log(|V|[s](-1)) + d(log(|V|_e[s]))) if |V|[s] <> 0 where V in L E MAT, s in {%list_sec}\21

 {%modelname}.equation d(log(|V|_e[s])) = ADJUST(1 + $V, 2) * d(log(|V|_e[s](-1))) + ADJUST(1 +$V, 3) * d(log(|V|[s](-1))) + ADJUST(1 + $V, 4) * d(log(|V|_n[s])) if |V|[s] <> 0 where V in L E MAT, s in {%list_sec}\21

 {%modelname}.equation  |V|[21] = |V|_n[21] where V in L E MAT

  ' !step_1=1
  ' For %varname L E MAT
  '   For %sec {%list_sec}
  '     if @elem({%varname}_{%sec},%baseyear) <> 0 and %sec<>"21" then   ' This condition solve the problem of division by 0 (VA_21 = 0). This condition appears at several places in the script.

  '       {%modelname}.append  log({%varname}_{%sec}) = ADJUST(1+!step_1,1)*log({%varname}_n_{%sec}) + (1-ADJUST(1+!step_1,1))*(log({%varname}_{%sec}(-1))+d(log({%varname}_e_{%sec})))

  '       {%modelname}.append d(log({%varname}_e_{%sec})) = ADJUST(1+!step_1,2)*d(log({%varname}_e_{%sec}(-1))) + ADJUST(1+!step_1,3)*d(log({%varname}_{%sec}(-1))) + ADJUST(1+!step_1,4)*d(log({%varname}_n_{%sec}))
  '       '+ ADJUST(1+!step_1,5)*d(log({%varname}_{%sec}(+1)))

  '     endif

  '     if @elem({%varname}_{%sec},%baseyear) <> 0 and %sec="21" then ' This condition solve the problem of division by 0 (VA_21 = 0). This condition appears at several places in the script.

  '       {%modelname}.append  {%varname}_{%sec} = {%varname}_n_{%sec}

  '     endif
  '   next

  '   !step_1=!step_1+1
  ' next

 'equation 8.2
 {%modelname}.equation SUBST_|V|[s] = ADJUST(1 + $V,6) * SUBST_|V|_n[s] + (1 - ADJUST(1 + $V,6)) * (SUBST_|V|[s](-1)) if |V|[s] <> 0 where V in K L E MAT, s in {%list_sec}

'   !step_1=0
'   For %varname K L E MAT
'     For %sec {%list_sec}
'       if @elem({%varname}_{%sec},%baseyear) <> 0 then
'
'         {%modelname}.append SUBST_{%varname}_{%sec} = ADJUST(1+!step_1,6)*SUBST_{%varname}_n_{%sec} +(1- ADJUST(1+!step_1,6))*(SUBST_{%varname}_{%sec}(-1))
'       endif
'
'     next
'     !step_1=!step_1+1
'   next

 'equation 8.3
 ' Ajustment Line 5
 {%modelname}.equation SUBST_E[ce, s] = ADJUST(5, 6) * SUBST_E_n[ce, s] + (1- ADJUST(5, 6)) * (SUBST_E[ce, s](-1)) if E[ce, s] <> 0 where ce in {%list_com_E}, s in {%list_sec}

'   ' Ajustment Line 5
'   For %com {%list_com_E}
'     For %sec {%list_sec}
'       if @elem(E_{%com}_{%sec},%baseyear) <> 0 then
'
'         {%modelname}.append SUBST_E_{%com}_{%sec} = ADJUST(5,6)*SUBST_E_n_{%com}_{%sec} +(1- ADJUST(5,6))*(SUBST_E_{%com}_{%sec}(-1))
'       endif
'
'     next
'   next

 'equation 8.4
 ' Ajustment Line 6
  {%modelname}.equation SUBST_MAT[trsp, s] = ADJUST(6, 6) * SUBST_MAT_n[trsp, s] +(1- ADJUST(6, 6)) * (SUBST_MAT[trsp, s](-1)) if MAT[trsp, s] <> 0 where trsp in {%list_trsp}, s in {%list_sec}

'   ' Ajustment Line 6
'   For %com {%list_trsp}
'     For %sec {%list_sec}
'       if @elem(MAT_{%com}_{%sec},%baseyear) <> 0 then
'
'         {%modelname}.append SUBST_MAT_{%com}_{%sec} = ADJUST(6,6)*SUBST_MAT_n_{%com}_{%sec} +(1- ADJUST(6,6))*(SUBST_MAT_{%com}_{%sec}(-1))
'       endif
'
'     next
'   next

'  'equation 8.5
'  ' Ajustment Line 7
'  {%modelname}.equation SUBST_ED[ce, s] = ADJUST(7, 6) * SUBST_ED_n[ce, s] + (1 - ADJUST(7, 6)) * (SUBST_ED[ce, s](-1)) if ED[ce, s] <> 0 if EM[ce, s] <> 0 , ce in {%list_com_E}, s in {%list_sec}


  ' Ajustment Line 7
  For %com {%list_com_E}
    For %sec {%list_sec}
      if @elem(ED_{%com}_{%sec},%baseyear) <> 0 and @elem(EM_{%com}_{%sec},%baseyear) <> 0 then
        {%modelname}.append SUBST_ED_{%com}_{%sec} = ADJUST(7,6)*SUBST_ED_n_{%com}_{%sec} +(1- ADJUST(7,6))*(SUBST_ED_{%com}_{%sec}(-1))
      endif
    next
  next

'  'equation 8.6
'  ' Ajustment Line 8
'  {%modelname}.equation SUBST_EM[ce, s] = ADJUST(8, 6) * SUBST_EM_n[ce, s] + (1 - ADJUST(8, 6)) * (SUBST_EM[ce, s](-1)) if ED[ce, s] <> 0 and if EM[ce, s] <> 0 , ce in {%list_com_E}, s in {%list_sec}

  ' Ajustment Line 8
  For %com {%list_com_E}
    For %sec {%list_sec}
      if @elem(ED_{%com}_{%sec},%baseyear) <> 0 and @elem(EM_{%com}_{%sec},%baseyear) <> 0 then
        {%modelname}.append SUBST_EM_{%com}_{%sec} = ADJUST(8,6)*SUBST_EM_n_{%com}_{%sec} +(1- ADJUST(8,6))*(SUBST_EM_{%com}_{%sec}(-1))
      endif
    next
  next

'  'equation 8.7
'  ' Ajustment Line 9
'   {%modelname}.append SUBST_MATD[cm, s] = ADJUST(9, 6) * SUBST_MATD_n[cm, s] + (1 - ADJUST(9, 6))  *(SUBST_MATD[cm, s](-1)) if MATD[cm, s] <> 0 and MATM[cm, s] <> 0 , cm in {%list_com_MAT}, s in {%list_sec}

  ' Ajustment Line 9
  For %com {%list_com_MAT}
    For %sec {%list_sec}
      if @elem(MATD_{%com}_{%sec},%baseyear) <> 0 and @elem(MATM_{%com}_{%sec},%baseyear) <> 0 then
        {%modelname}.append SUBST_MATD_{%com}_{%sec} = ADJUST(9,6)*SUBST_MATD_n_{%com}_{%sec} +(1- ADJUST(9,6))*(SUBST_MATD_{%com}_{%sec}(-1))
      endif
    next
  next

'  'equation 8.8
'  ' Ajustment Line 10
'  {%modelname}.append SUBST_MATM[cm, s] = ADJUST(10, 6) * SUBST_MATM_n[cm, s] +(1 - ADJUST(10, 6)) * (SUBST_MATM[cm, s](-1)) if MATD[cm, s] <> 0 and MATM[cm, s] <> 0 , cm in {%list_com_MAT}, s in {%list_sec}

  ' Ajustment Line 10
  For %com {%list_com_MAT}
    For %sec {%list_sec}
      if @elem(MATD_{%com}_{%sec},%baseyear) <> 0 and @elem(MATM_{%com}_{%sec},%baseyear) <> 0 then
        {%modelname}.append SUBST_MATM_{%com}_{%sec} = ADJUST(10,6)*SUBST_MATM_n_{%com}_{%sec} +(1- ADJUST(10,6))*(SUBST_MATM_{%com}_{%sec}(-1))
      endif
    next
  next

 'equation 8.9
 ' Ajustment Line 11
 {%modelname}.equation SUBST_MTD[trsp, c] = ADJUST(11, 6) * SUBST_MTD_n[trsp, c] + (1 - ADJUST(11, 6)) * (SUBST_MTD[trsp, c](-1)) if MTD[trsp, c] <> 0 where trsp in {%list_trsp}, c in {%list_com}\{%list_trsp}

'   ' Ajustment Line 11
'   For %mar 14 15 16 17 18
'     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
'       if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then  ' Do not put 2 if conditions! Substitution between type of margins NOT between domestic and imported
'
'         {%modelname}.append SUBST_MTD_{%mar}_{%com} = ADJUST(11,6)*SUBST_MTD_n_{%mar}_{%com} +(1- ADJUST(11,6))*(SUBST_MTD_{%mar}_{%com}(-1))
'       endif
'
'     next
'   next

 'equation 8.10
 ' Ajustment Line 12
 {%modelname}.equation SUBST_MTM[trsp, c] = ADJUST(12, 6) * SUBST_MTM_n[trsp, c] +(1 - ADJUST(12, 6)) * (SUBST_MTM[trsp, c](-1)) if MTM[trsp, c] <> 0 where trsp in {%list_trsp}, c in {%list_com}\{%list_trsp}

'   ' Ajustment Line 12
'   For %mar {%list_trsp}
'     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
'       if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then ' Do not put 2 if conditions! Substitution between type of margins NOT between domestic and imported !!!
'
'         {%modelname}.append SUBST_MTM_{%mar}_{%com} = ADJUST(12,6)*SUBST_MTM_n_{%mar}_{%com} +(1- ADJUST(12,6))*(SUBST_MTM_{%mar}_{%com}(-1))
'       endif
'     next
'   next


  ' Ajustment Line 13
  For %com  {%list_com}
    For %sec {%list_sec}
      if @elem(IAD_{%com}_{%sec},%baseyear) <> 0 and @elem(IAM_{%com}_{%sec},%baseyear) <> 0 then

        {%modelname}.append SUBST_IAD_{%com}_{%sec} = ADJUST(13,6)*SUBST_IAD_n_{%com}_{%sec} +(1- ADJUST(13,6))*(SUBST_IAD_{%com}_{%sec}(-1))
      endif
    next
  next


  ' Ajustment Line 14
  For %com  {%list_com}
    For %sec {%list_sec}
      if @elem(IAD_{%com}_{%sec},%baseyear) <> 0 and @elem(IAM_{%com}_{%sec},%baseyear) <> 0 then

        {%modelname}.append SUBST_IAM_{%com}_{%sec} = ADJUST(14,6)*SUBST_IAM_n_{%com}_{%sec} +(1- ADJUST(14,6))*(SUBST_IAM_{%com}_{%sec}(-1))
      endif
    next
  next

 'equation 8.13
 ' Ajustment Line 15-16
 {%modelname}.equation  log(|V|[c]) = ADJUST(14 + $V, 1) * log(|V|_n[c]) + (1 - ADJUST(14 + $V, 1)) * (log(|V|[c](-1)) + d(log(|V|_e[c]))) if |V|[c] <> 0 where V in SD SM, c in {%list_com}
 {%modelname}.equation d(log(|V|_e[c])) = ADJUST(14 +$V, 2) * d(log(|V|_e[c](-1))) + ADJUST(14 + $V, 3)*d(log(|V|[c](-1))) + ADJUST(14 + $V, 4) * d(log(|V|_n[c])) if |V|[c] <> 0 where V in SD SM, c in {%list_com}
       '+ ADJUST(15+!step_1,5)*d(log({%varname}_{%com}(+1)))

'   ' Ajustment Line 15-16
'   !step_1=0
'   For %varname SD SM
'     For %com {%list_com}
'       if @elem({%varname}_{%com},%baseyear) <> 0 then
'
'         {%modelname}.append  log({%varname}_{%com}) = ADJUST(15+!step_1,1)*log({%varname}_n_{%com}) + (1-ADJUST(15+!step_1,1))*(log({%varname}_{%com}(-1))+d(log({%varname}_e_{%com})))
'
'         {%modelname}.append d(log({%varname}_e_{%com})) = ADJUST(15+!step_1,2)*d(log({%varname}_e_{%com}(-1))) + ADJUST(15+!step_1,3)*d(log({%varname}_{%com}(-1))) + ADJUST(15+!step_1,4)*d(log({%varname}_n_{%com}))
'         '+ ADJUST(15+!step_1,5)*d(log({%varname}_{%com}(+1)))
'
'       endif
'     next
'     !step_1=!step_1+1
'   next

 'equation 8.14
 ' Ajustment Line 17
  {%modelname}.equation  log(PY[s]) = ADJUST(17, 1) * log(PY_n[s]) + (1 - ADJUST(17, 1)) * (log(PY[s](-1)) + d(log(PY_e[s]))) where s in {%list_sec}
  {%modelname}.equation d(log(PY_e[s])) = ADJUST(17, 2) * d(log(PY_e[s](-1))) + ADJUST(17, 3) * d(log(PY[s](-1))) + ADJUST(17, 4) * d(log(PY_n[s])) where s in {%list_sec}
    '+ ADJUST(17,5)*d(log(W_S_{%sec}(+1)))

'   ' Ajustment Line 17
'   'PROBLEME!
'   For %sec {%list_sec}
'     {%modelname}.append  log(PY_{%sec}) = ADJUST(17,1)*log(PY_n_{%sec}) + (1-ADJUST(17,1))*(log(PY_{%sec}(-1))+d(log(PY_e_{%sec})))
'
'     {%modelname}.append d(log(PY_e_{%sec})) = ADJUST(17,2)*d(log(PY_e_{%sec}(-1))) + ADJUST(17,3)*d(log(PY_{%sec}(-1))) + ADJUST(17,4)*d(log(PY_n_{%sec}))
'     '+ ADJUST(17,5)*d(log(W_S_{%sec}(+1)))
'
'   next

 'equation 8.15
 ' Ajustment Line 18
 {%modelname}.equation d(log(W_S[s])) = ADJUST(18, 1) * d(log(W_S_n[s])) + (1 - ADJUST(18, 1)) * d(log(W_S[s](-1))) if W_S[s] <> 0 where s in {%list_sec}
 '{%modelname}.append  d(log(W_S_{%sec})) = ADJUST(18,1)*d(log(W_S_n_{%sec})) + (1-ADJUST(18,1))*d(log(W_S_{%sec}(-1)))-ADJUST(18,2)*(log(W_S_{%sec}(-1))-log(W_S_n_{%sec}(-1)))

'   ' Ajustment Line 18
'   For %sec {%list_sec}
'     if @elem(W_S_{%sec},%baseyear) <> 0 then
'
'       {%modelname}.append  d(log(W_S_{%sec})) = ADJUST(18,1)*d(log(W_S_n_{%sec})) + (1-ADJUST(18,1))*d(log(W_S_{%sec}(-1)))
'       '{%modelname}.append  d(log(W_S_{%sec})) = ADJUST(18,1)*d(log(W_S_n_{%sec})) + (1-ADJUST(18,1))*d(log(W_S_{%sec}(-1)))-ADJUST(18,2)*(log(W_S_{%sec}(-1))-log(W_S_n_{%sec}(-1)))
'
'
'       ''         {%modelname}.append  log(W_S_{%sec}) = ADJUST(18,1)*log(W_S_n_{%sec}) + (1-ADJUST(18,1))*(log(W_S_{%sec}(-1))+d(log(W_S_e_{%sec})))
'
'       '      {%modelname}.append d(log(W_S_e_{%sec})) = ADJUST(18,2)*d(log(W_S_e_{%sec}(-1))) + ADJUST(18,3)*d(log(W_S_{%sec}(-1))) + ADJUST(18,4)*d(log(W_S_n_{%sec}))
'       '+ ADJUST(18,5)*d(log(W_S_{%sec}(+1)))
'
'     endif
'   next

 'equation 8.16
 {%modelname}.equation  log(PARTR[coh]) = ADJUST(19, 1) * log(PARTR_n[coh]) + (1 - ADJUST(19, 1)) * (log(PARTR[coh](-1)) + d(log(PARTR_e[coh]))) if PARTR[coh] <> 0 where coh in {%list_coh}
 {%modelname}.equation d(log(PARTR_e[coh])) = ADJUST(19, 2) * d(log(PARTR_e[coh](-1))) + ADJUST(19, 3) * d(log(PARTR[coh](-1))) + ADJUST(19, 4) * d(log(PARTR_n[coh])) if PARTR[coh] <> 0 where coh in {%list_coh}
       '+ ADJUST(19,5)*d(log(PARTR_{%coh}(+1)))

'   For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
'     if @elem(PARTR_{%coh},%baseyear) <> 0 then
'
'       {%modelname}.append  log(PARTR_{%coh}) = ADJUST(19,1)*log(PARTR_n_{%coh}) + (1-ADJUST(19,1))*(log(PARTR_{%coh}(-1))+d(log(PARTR_e_{%coh})))
'
'       {%modelname}.append d(log(PARTR_e_{%coh})) = ADJUST(19,2)*d(log(PARTR_e_{%coh}(-1))) + ADJUST(19,3)*d(log(PARTR_{%coh}(-1))) + ADJUST(19,4)*d(log(PARTR_n_{%coh}))
'       '+ ADJUST(19,5)*d(log(PARTR_{%coh}(+1)))
'
'     endif
'   next

 'equation 8.17
  ' Interest rate
  {%modelname}.append  R = ADJUST(20, 1) * R_n + (1  -ADJUST(20, 1)) * (R(-1) + d(R_e))
  {%modelname}.append d(R_e) = ADJUST(20, 2) * d(R_e(-1)) + ADJUST(20, 3) * d(R(-1)) + ADJUST(20, 4) * d(R_n)
  '+ ADJUST(20,5)*d(R(+1))

 'equation 8.18
 ' Ajustment Line 61-62
 {%modelname}.equation SUBST_CHD[c] = ADJUST(61, 6) * SUBST_CHD_n[c] + (1 - ADJUST(61, 6)) * (SUBST_CHD[c](-1)) if CHD[c] <> 0 where c in {%list_com}
 {%modelname}.equation SUBST_CHM[c] = ADJUST(62, 6) * SUBST_CHM_n[c] + (1 - ADJUST(62, 6)) * (SUBST_CHM[c](-1)) if CHM[c] <> 0 where c in {%list_com}


'   ' Ajustment Line 61-62
'   For %com  {%list_com}
'
'     if @elem(CHD_{%com},%baseyear) <>  0 then
'
'       {%modelname}.append SUBST_CHD_{%com} = ADJUST(61,6)*SUBST_CHD_n_{%com}+(1- ADJUST(61,6))*(SUBST_CHD_{%com}(-1))
'     endif
'     if  @elem(CHM_{%com},%baseyear) <> 0 then
'
'       {%modelname}.append SUBST_CHM_{%com} = ADJUST(62,6)*SUBST_CHM_n_{%com}+(1- ADJUST(62,6))*(SUBST_CHM_{%com}(-1))
'     endif
'   next

 'equation 8.19
 ' Ajustment Line 63-64
 {%modelname}.equation SUBST_GD[c] = ADJUST(63, 6) * SUBST_GD_n[c] + (1 - ADJUST(63, 6)) * (SUBST_GD[c](-1)) if GD[c] <> 0 where c in {%list_com}
 {%modelname}.equation SUBST_GM[c] = ADJUST(64, 6) * SUBST_GM_n[c] + (1 - ADJUST(64, 6)) * (SUBST_GM[c](-1)) if GM[c] <> 0 where c in {%list_com}

'   ' Ajustment Line 63-64
'   For %com  {%list_com}
'     if @elem(GD_{%com},%baseyear) <> 0 then
'       {%modelname}.append SUBST_GD_{%com} = ADJUST(63,6)*SUBST_GD_n_{%com}+(1- ADJUST(63,6))*(SUBST_GD_{%com}(-1))
'     endif
'
'     if @elem(GM_{%com},%baseyear) <> 0 then
'       {%modelname}.append SUBST_GM_{%com} = ADJUST(64,6)*SUBST_GM_n_{%com}+(1- ADJUST(64,6))*(SUBST_GM_{%com}(-1))
'     endif
'   next

 'equation 8.20
 ' Ajustment Line 65-67
 {%modelname}.equation SUBST_X[c] = ADJUST(65, 6) * SUBST_X_n[c] + (1- ADJUST(65, 6)) * (SUBST_X[c](-1)) if X[c] <> 0 where c in {%list_com}
 {%modelname}.equation SUBST_XD[c] = ADJUST(66, 6) * SUBST_XD_n[c] + (1- ADJUST(66, 6)) * (SUBST_XD[c](-1)) if XD[c] <> 0 where c in {%list_com}
 {%modelname}.equation SUBST_XM[c] = ADJUST(67, 6) * SUBST_GM_n[c] + (1- ADJUST(67, 6)) * (SUBST_XM[c](-1)) if XM[c] <> 0 where c in {%list_com}

'   For %com  {%list_com}
'
'     if @elem(X_{%com},%baseyear) <> 0 then
'       {%modelname}.append SUBST_X_{%com} = ADJUST(65,6)*SUBST_X_n_{%com}+(1- ADJUST(65,6))*(SUBST_X_{%com}(-1))
'     endif
'
'     if @elem(XD_{%com},%baseyear) <> 0 then
'       {%modelname}.append SUBST_XD_{%com} = ADJUST(66,6)*SUBST_XD_n_{%com}+(1- ADJUST(66,6))*(SUBST_XD_{%com}(-1))
'     endif
'
'     if @elem(XM_{%com},%baseyear) <> 0 then
'       {%modelname}.append SUBST_XM_{%com} = ADJUST(67,6)*SUBST_GM_n_{%com}+(1- ADJUST(67,6))*(SUBST_XM_{%com}(-1))
'     endif
'   next

'  'equation 8.21
'  ' Ajustment Line 68
'   {%modelname}.equation DIV_|V|_VAL = ADJUST(70, 1) * DIV_|V|_VAL_n + (1 - ADJUST(70, 1)) * (DIV_|V|_VAL(-1) + d(DIV_|V|_VAL_e)), V in HH GOV
'   {%modelname}.equation d(DIV_|V|_VAL_e) = ADJUST(70, 2) * d(DIV_|V|_VAL_e(-1)) + ADJUST(70, 3) * d(DIV_|V|_VAL(-1)) + ADJUST(70, 4) * d(DIV_|V|_VAL_n) + (1 + STEADYSTATE(1, 1))^(@trend-({%baseyear}-{%firstdate})) * @elem(d(DIV_|V|_VAL_e) -(ADJUST(70, 2) * d(DIV_|V|_VAL_e(-1))+ ADJUST(70,3)*d(DIV_|V|_VAL(-1)) + ADJUST(70, 4) * d(DIV_|V|_VAL_n)),{%baseyear}), V in HH GOV
'     '+ ADJUST(70,5)*d(DIV_{%list}_VAL(+1)))

  ' Ajustment Line 68
  For %list  HH GOV
    {%modelname}.append  DIV_{%list}_VAL = ADJUST(70,1)*DIV_{%list}_VAL_n + (1-ADJUST(70,1))*(DIV_{%list}_VAL(-1)+d(DIV_{%list}_VAL_e))
    {%modelname}.append d(DIV_{%list}_VAL_e) =ADJUST(70,2)*d(DIV_{%list}_VAL_e(-1))+ ADJUST(70,3)*d(DIV_{%list}_VAL(-1)) + ADJUST(70,4)*d(DIV_{%list}_VAL_n) + (1+STEADYSTATE(1,1))^(@trend-({%baseyear}-{%firstdate}))*@elem(d(DIV_{%list}_VAL_e) -(ADJUST(70,2)*d(DIV_{%list}_VAL_e(-1))+ ADJUST(70,3)*d(DIV_{%list}_VAL(-1)) + ADJUST(70,4)*d(DIV_{%list}_VAL_n)),%baseyear)
    '+ ADJUST(70,5)*d(DIV_{%list}_VAL(+1)))
  next


  '{%modelname}.append  log(EXPG) = ADJUST(69,1)*log(EXPG_n) + (1-ADJUST(69,1))*(log(EXPG(-1))+d(log(EXPG_e)))

  '{%modelname}.append d(log(EXPG_e)) = ADJUST(69,2)*d(log(EXPG_e(-1))) + ADJUST(69,3)*d(log(EXPG(-1))) + ADJUST(69,4)*d(log(EXPG_n))
  '+ ADJUST(69,5)*d(log(EXPG(+1)))

  '**************************************************************************************************************************************
  '******************************************** END BLOCK 7 : OTHER EQUATIONS ************************************************
  '**************************************************************************************************************************************




endsub


' *******************************************************************************************************************************
' ***************************************************** AGGREGATE ENERGY SUBROUTINE*****************************
'******************************************************************************************************************************
subroutine aggregate_energy(string %var) ' This subroutine aggregate the energy subsectors. Ex: for sector 22 make the sum of the 6 subsectors.

  %data = %var+"_22"
  if @elem({%data},%baseyear) <> 0 then

    %equation = %var+"_22 = 0"
    For %subsec 01 02
      ' PROBLEME: INTRODUIRE CONDITIONNALITE dans subroutine aggregate_energy?
      %equation = %equation+" + "+%var+"_22"+%subsec
    next
    {%modelname}.append {%equation}

  endif

  %data = %var+"_23"
  if @elem({%data},%baseyear) <> 0 then
    %equation = %var+"_23 = 0"
    For %subsec 01 02 03 04 05 06 07 08
      %equation = %equation+" + "+%var+"_23"+%subsec
    next
    {%modelname}.append {%equation}

  endif

  %data = %var+"_24"
  if @elem({%data},%baseyear) <> 0 then

    %equation = %var+"_24 = 0"
    For %subsec 01 02 03 04 05 06
      %equation = %equation+" + "+%var+"_24"+%subsec
    next
    {%modelname}.append {%equation}
  endif


endsub


' *******************************************************************************************************************************
' ***************************************************** END AGGREGATE ENERGY SUBROUTINE*****************************
'******************************************************************************************************************************



' *******************************************************************************************************************************
' ***************************************************** PRICE INDEX ENERGY SUBROUTINE*****************************
'******************************************************************************************************************************
' This subroutine calculate the price indexes in the energy sectors from the price and quantities of the subsectors.
' Ex: for sector 22 and the price of capital: CK_22*K_22 = + CK_2201*K_2201 + CK_2202*K_2202

subroutine priceindex_energy(string %p,string %q)

  if @elem({%q}_22,%baseyear) <> 0 then
    %equation = %p+"_22*"+%q+"_22 = 0"
    For %subsec 01 02
      %equation = %equation+" + "+%p+"_22"+%subsec+"*"+%q+"_22"+%subsec
    next
    {%modelname}.append {%equation}
  endif

  if @elem({%q}_23,%baseyear) <> 0 then
    %equation = %p+"_23*"+%q+"_23 = 0"
    For %subsec 01 02 03 04 05 06 07 08
      %equation = %equation+" + "+%p+"_23"+%subsec+"*"+%q+"_23"+%subsec
    next
    {%modelname}.append {%equation}
  endif

  if @elem({%q}_24,%baseyear) <> 0 then
    %equation = %p+"_24*"+%q+"_24 = 0"
    For %subsec 01 02 03 04 05 06
      %equation = %equation+" + "+%p+"_24"+%subsec+"*"+%q+"_24"+%subsec
    next
    {%modelname}.append {%equation}
  endif

endsub