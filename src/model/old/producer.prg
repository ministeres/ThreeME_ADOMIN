subroutine BLOCK_Producer
  '***************************************************************************************************************************************
  '******************************************** BLOCK 2 : PRODUCER ****************************************************
  '**************************************************************************************************************************************
  '**************************************************************************************************************************************

  ' equation 2.01
  {%modelname}.equation PYQ[c] * YQ[c] = sum(PY[s] * Y[c, s] if Y[c, s] <> 0 where s in {%list_sec}) if YQ[c] <> 0 where c in {%list_com}

  ' 'equation 2.01
  ' For %com {%list_com}
  '   if @elem(YQ_{%com},%baseyear) <> 0 then
  '     %equation ="PYQ_"+%com+"*YQ_"+%com+"=0"
  '     For %sec {%list_sec}
  '       if @elem(Y_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation+"+PY_"+%sec+"*Y_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}
  '   endif
  ' next

  ' equation 2.02
  {%modelname}.equation Y[c, s] = PhiY[c, s] * YQ[c] if Y[c, s] <> 0 where c in {%list_com}, s in {%list_sec}
  {%modelname}.equation Y[s] = sum(Y[c, s] if Y[c, s] <> 0 where c in {%list_com}) where s in {%list_sec}

  ' 'equation 2.02
  ' For %sec {%list_sec}
  '   For %com {%list_com}
  '     if @elem(Y_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append Y_{%com}_{%sec}=PhiY_{%com}_{%sec}*YQ_{%com}
  '     endif
  '   next

  '   'equation 2.03
  '   %equation = "Y_"+%sec+"=0"
  '   For %com {%list_com}
  '     if @elem(Y_{%com}_{%sec},%baseyear) <> 0 then
  '       %equation = %equation+" +Y_"+%com+"_"+%sec
  '     endif
  '   next
  '   {%modelname}.append {%equation}
  ' next

  call aggregate_energy ("Y")
  call aggregate_energy ("E")


  '------------------------------------------------------Level I------------------------------------------------------------------------------

  ' equation 2.04
  {%modelname}.equation d(log(K_n[s])) = d(log(Y[s])) + d(SUBST_K[s]) - d(log(PROG_K[s])) + IMP_BUD[s] * @elem(GDP, {%baseyear}) / @elem(K_n[s], {%baseyear}) if K_n[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(SUBST_K_n[s]) = _
  -ES_KLEM($s, 1) * d(log(CK[s]) - log(CL[s]))   * (L[s](-1)   * PROG_L[s](-1) * CL[s](-1) / (CU[s](-1) * Y[s](-1))) _
  -ES_KLEM($s, 2) * d(log(CK[s]) - log(PE[s]))   * (E[s](-1)   * PE[s](-1)                 / (CU[s](-1) * Y[s](-1))) _
  -ES_KLEM($s, 3) * d(log(CK[s]) - log(PMAT[s])) * (MAT[s](-1) * PMAT[s](-1)               / (CU[s](-1) * Y[s](-1))) _
  if K_n[s] <> 0 where s in {%list_sec}

  ' 'equation 2.04
  ' !step=1
  ' For %sec {%list_sec}
  '   if @elem(K_n_{%sec},%baseyear) <> 0 then

  '     {%modelname}.append  d(log(K_n_{%sec})) = d(log(Y_{%sec})) + d(SUBST_K_{%sec}) - d(log(PROG_K_{%sec}))  + IMP_BUD_{%sec}*@elem(GDP,%baseyear)/@elem(K_n_{%sec},%baseyear)

  '     ' PROBLEME! Une fois le modèle terminé, tester la stabilité du modèle sans les @elem() dans les substitutions
  '     {%modelname}.append d(SUBST_K_n_{%sec})= - ES_KLEM(!step,1)*d(log(CK_{%sec}) - log(CL_{%sec}))*(L_{%sec}(-1)*PROG_L_{%sec}(-1)*CL_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,2)*d(log(CK_{%sec}) - log(PE_{%sec}))*(E_{%sec}(-1)*PE_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,3)*d(log(CK_{%sec}) - log(PMAT_{%sec}))*(MAT_{%sec}(-1)*PMAT_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)))



  '   endif
  '   !step=!step+1
  ' next

  ' equation 2.05
  {%modelname}.equation d(log(L_n[s])) = d(log(Y[s])) - d(log(PROG_L[s])) + d(SUBST_L[s]) if L_n[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(SUBST_L_n[s]) = _
    -ES_KLEM($s, 1) * d(log(CL[s]) - log(CK[s]))   * (K[s](-1) * CK[s](-1)     / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 4) * d(log(CL[s]) - log(PE[s]))   * (E[s](-1) * PE[s](-1)     / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 5) * d(log(CL[s]) - log(PMAT[s])) * (MAT[s](-1) * PMAT[s](-1) / (CU[s](-1) * Y[s](-1))) _
    if L_n[s] <> 0 where s in {%list_sec}

  ' 'equation 2.05
  ' !step=1
  ' For %sec {%list_sec}
  '   if @elem(L_n_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(log(L_n_{%sec})) = d(log(Y_{%sec})) - d(log(PROG_L_{%sec})) + d(SUBST_L_{%sec})

  '     {%modelname}.append d(SUBST_L_n_{%sec})= - ES_KLEM(!step,1)*d(log(CL_{%sec}) - log(CK_{%sec}))*(K_{%sec}(-1)*CK_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,4)*d(log(CL_{%sec}) - log(PE_{%sec}))*(E_{%sec}(-1)*PE_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,5)*d(log(CL_{%sec}) - log(PMAT_{%sec}))*(MAT_{%sec}(-1)*PMAT_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)))

  '   endif
  '   !step=!step+1
  ' next

  ' equation 2.06
  {%modelname}.equation d(log(E_n[s])) = d(log(Y[s])) + d(SUBST_E[s]) - d(log(PROG_E[s])) if E_n[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(SUBST_E_n[s]) = _
    -ES_KLEM($s, 2) * d(log(PE[s]) - log(CK[s]))   * (K[s](-1)   * CK[s](-1)                 / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 4) * d(log(PE[s]) - log(CL[s]))   * (L[s](-1)   * PROG_L[s](-1) * CL[s](-1) / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 6) * d(log(PE[s]) - log(PMAT[s])) * (MAT[s](-1) * PMAT[s](-1)               / (CU[s](-1) * Y[s](-1))) _
  if E_n[s] <> 0 where s in {%list_sec}

  ' 'equation 2.06
  ' !step=1
  ' For %sec {%list_sec}
  '   if @elem(E_n_{%sec},%baseyear) <> 0 then

  '     {%modelname}.append  d(log(E_n_{%sec})) = d(log(Y_{%sec}))+ d(SUBST_E_{%sec})-d(log(PROG_E_{%sec}))


  '     {%modelname}.append d(SUBST_E_n_{%sec})= - ES_KLEM(!step,2)*d(log(PE_{%sec}) - log(CK_{%sec}))*(K_{%sec}(-1)*CK_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,4)*d(log(PE_{%sec}) - log(CL_{%sec}))*(L_{%sec}(-1)*PROG_L_{%sec}(-1)*CL_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,6)*d(log(PE_{%sec}) - log(PMAT_{%sec}))*(MAT_{%sec}(-1)*PMAT_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)))
  '   endif
  '   !step=!step+1
  ' next

  ' equation 2.07
  {%modelname}.equation d(log(MAT_n[s])) = d(log(Y[s])) + d(SUBST_MAT[s]) if MAT_n[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(SUBST_MAT_n[s]) = _
    -ES_KLEM($s, 3) * d(log(PMAT[s]) - log(CK[s])) * (K[s](-1) * CK[s](-1)                 / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 5) * d(log(PMAT[s]) - log(CL[s])) * (L[s](-1) * PROG_L[s](-1) * CL[s](-1) / (CU[s](-1) * Y[s](-1))) _
    -ES_KLEM($s, 6) * d(log(PMAT[s]) - log(PE[s])) * (E[s](-1) * PE[s](-1)                 / (CU[s](-1) * Y[s](-1))) _
  if MAT_n[s] <> 0 where s in {%list_sec}


  ' 'equation 2.07
  ' !step=1
  ' For %sec {%list_sec}
  '   if @elem(MAT_n_{%sec},%baseyear) <> 0 then

  '     {%modelname}.append  d(log(MAT_n_{%sec})) = d(log(Y_{%sec}))+ d(SUBST_MAT_{%sec})

  '     {%modelname}.append d(SUBST_MAT_n_{%sec}) =   -ES_KLEM(!step,3)*d(log(PMAT_{%sec}) - log(CK_{%sec}))*(K_{%sec}(-1)*CK_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,5)*d(log(PMAT_{%sec}) - log(CL_{%sec}))*(L_{%sec}(-1)*PROG_L_{%sec}(-1)*CL_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))) _
  '     - ES_KLEM(!step,6)*d(log(PMAT_{%sec}) - log(PE_{%sec}))*(E_{%sec}(-1)*PE_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)))
  '   endif
  '   !step=!step+1
  ' next

  'equation 2.08
  For %sec {%list_sec}
    if @elem(IA_n_{%sec},%baseyear) <> 0 then


      '         {%modelname}.append  IA_n_{%sec} = Kbis_{%sec} - K_{%sec}(-1)+Tdec_{%sec}*K_{%sec}(-1)

      '         {%modelname}.append  IA_n_{%sec} = (d(K_n_{%sec}) + Tdec_{%sec}*K_{%sec}(-1)>0)*(d(K_n_{%sec}) + Tdec_{%sec}*K_{%sec}(-1))+(d(K_n_{%sec}) + Tdec_{%sec}*K_{%sec}(-1)<=0)*(0.00001)

      '         {%modelname}.append  IA_n_{%sec} = (d(K_n_{%sec}) + Tdec_{%sec}*K_n_{%sec}(-1)>0)*(d(K_n_{%sec}) + Tdec_{%sec}*K_n_{%sec}(-1))+(d(K_n_{%sec}) + Tdec_{%sec}*K_n_{%sec}(-1)<=0)*(0.00001)  ' Specification alternative stable
      '         {%modelname}.append  IA_n_{%sec} = (K_n_{%sec} - K_{%sec}(-1)+Tdec_{%sec}*K_{%sec}(-1)>0)*(K_n_{%sec} - K_{%sec}(-1)+Tdec_{%sec}*K_{%sec}(-1))+(K_n_{%sec} -K_{%sec}(-1)+Tdec_{%sec}*K_{%sec}(-1)<=0)*(0.00001)  ' Specification qui crée des oscillations explosive
    endif
  next

  ' equation 2.09
  {%modelname}.equation d(log(IA[c, s])) = d(log(IA[s])) if IA[c, s] <> 0 where c in {%list_com}, s in {%list_sec}

  ' 'equation 2.09
  ' For %sec {%list_sec}
  '   For %com {%list_com}
  '     if @elem(IA_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append    d(log(IA_{%com}_{%sec}))=d(log(IA_{%sec}))
  '     endif
  '   next
  ' next


  ' equation 2.10
  {%modelname}.equation PK[s] * K[s] = (1 - Tdec[s]) * PK[s](-1) * K[s](-1) + PIA[s] * IA[s] if K[s] <> 0 where s in {%list_sec}

  ' 'equation 2.10
  ' For %sec {%list_sec}
  '   if @elem(K_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append   PK_{%sec}*K_{%sec}= (1-Tdec_{%sec})*PK_{%sec}(-1)*K_{%sec}(-1)+PIA_{%sec}*IA_{%sec}
  '   endif
  ' next

  ' equation 2.11
  {%modelname}.equation K[s] = (1 - Tdec[s]) * K[s](-1) + IA[s] if K[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(log(IA[s])) = ADJUST(1, 1) * d(log(IA[s](-1))) + ADJUST(1, 2) * (d(log(Y_e[s])) + ADJUST(1, 3) * (log(K_n[s](-1)) - log(K[s](-1))) + ADJUST(1, 4) * d(SUBST_K[s])) + IMP_BUD[s] * @elem(GDP, {%baseyear}) / @elem(IA[s], {%baseyear}) if K[s] <> 0 where s in {%list_sec}

  {%modelname}.equation d(log(Y_e[s])) = ADJUST(71,2) * d(log(Y_e[s](-1))) + (1 - ADJUST(71,2)) * d(log(Y[s])) where s in {%list_sec}

  ' 'equation 2.11
  ' For %sec {%list_sec}
  '   if @elem(K_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append  K_{%sec}= (1-Tdec_{%sec})*K_{%sec}(-1)+IA_{%sec}

  '     ''          {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec}))) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec})+IMP_BUD_{%sec}
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = 0.2*d(log(IA_{%sec}(-1))) + 0.8*(d(log(Y_e_{%sec})) + 0*d(d(log(Y_{%sec}))) - 0.0*d(d(log(PY_{%sec}))) + 0.1*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + 0.5*d(SUBST_K_{%sec}))+IMP_BUD_{%sec}
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec})) + 0*d(d(log(Y_{%sec}))) - 0.0*d(d(log(PY_{%sec}))) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec}))+IMP_BUD_{%sec}
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec})) - 0.0*d(d(log(PY_{%sec}))) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec}))+IMP_BUD_{%sec}
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = 0.2*d(log(IA_{%sec}(-1))) + 0.8*d(log(Y_e_{%sec})) + 0.05*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + 0.5*d(SUBST_K_{%sec}) + IMP_BUD_{%sec}*@elem(GDP,%baseyear)/@elem(IA_{%sec},%baseyear)  - 0.0*d(d(log(PY_{%sec})))
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec})) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec}))+ IMP_BUD_{%sec}*@elem(GDP,%baseyear)/@elem(IA_{%sec},%baseyear)  - 0.0*d(d(log(PY_{%sec})))    'PROBLEM! Eviews plante sans le dernier terme où elasticité = 0!!'
  '     ''          {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec})) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec}))+ IMP_BUD_{%sec}*@elem(GDP,%baseyear)/@elem(IA_{%sec},%baseyear)  - 0.0*d(d(log(Y_e_{%sec})))    'PROBLEM! Eviews plante sans le dernier terme où elasticité = 0!!'

  '     {%modelname}.append  d(log(IA_{%sec})) = ADJUST(1,1)*d(log(IA_{%sec}(-1))) + ADJUST(1,2)*(d(log(Y_e_{%sec})) + ADJUST(1,3)*(log(K_n_{%sec}(-1))-log(K_{%sec}(-1))) + ADJUST(1,4)*d(SUBST_K_{%sec}))+ IMP_BUD_{%sec}*@elem(GDP,%baseyear)/@elem(IA_{%sec},%baseyear)




  '     '         {%modelname}.append  IA_{%sec} = K_{%sec} - (1-Tdec_{%sec})*K_{%sec}(-1)
  '     '{%modelname}.append  K_{%sec} = (IA_n_{%sec}>0)*Kbis_{%sec} + (IA_n_{%sec}=<0)*((1-Tdec_{%sec})*K_{%sec}(-1)+0.000001)
  '     '{%modelname}.append  Kbis_n_{%sec} = K_n_{%sec}
  '   endif


  '   {%modelname}.append d(log(Y_e_{%sec})) = ADJUST(71,2)*d(log(Y_e_{%sec}(-1))) + (1-ADJUST(71,2))*d(log(Y_{%sec}))

  '   '{%modelname}.append d(log(Y_e_{%sec})) = 0.1*d(log(Y_e_{%sec}(-1)))+0.9*d(log(Y_{%sec}(-1)))
  '   '{%modelname}.append d(log(Y_e_{%sec})) = 0.1*d(log(Y_e_{%sec}(-1)))+0.9*d(log(Y_{%sec}))
  '   ''    {%modelname}.append d(log(Y_e_{%sec})) = 0.5*d(log(Y_e_{%sec}(-1)))+0.5*d(log(Y_{%sec}))
  ' next

  '-------------------------------TRANSPORT MARGINS--------------------

  ' equation 2.12 & 2.13
  {%modelname}.equation d(log(MT|O|[m, c])) = d(log(|V|[c])) + d(SUBST_MT|O|[m, c]) if MT|O|[m, c] <> 0 where (O, V) in (D M, YQ M), m in {%list_trsp}, c in {%list_com} \ {%list_trsp}

  ' 'equation 2.12
  ' For %mar {%list_trsp}
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '     if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
  '       {%modelname}.append d(log(MTD_{%mar}_{%com}))=d(log(YQ_{%com}))+d(SUBST_MTD_{%mar}_{%com})
  '     endif
  '   next
  ' next

'  {%modelname}.equation d(SUBST_MTD_n[t1, c]) = sum(-ES_TRANS_MARG($c, 1) * d(log(PE[t1]) - log(PE[t2])) if t2 <> 14, t2 in 14 16 17 18), t2 in 14 16 17 18

  {%modelname}.equation d(SUBST_MT|O|_n[trsp, c]) = _
                        sum( -ES_TRANSP_MARG($c, cols_trsp(trsp, trsp_oth)) * d(log(PE[trsp]) - log(PE[trsp_oth])) * _
                             value(MT|O|[trsp_oth, c](-1) / (MT|O|[trsp, c](-1) + MT|O|[trsp_oth, c](-1))) if trsp_oth <> trsp where trsp_oth in {%list_trsp} \ 15) _
                        if MT|O|[trsp, c] <> 0 where O in D M, trsp in {%list_trsp} \ 15, c in {%list_com} \ {%list_trsp}

  ' !step_L=1
  ' For %com  01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24

  '   if @elem(MTD_14_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTD_n_14_{%com})=  _
  '     -ES_TRANSP_MARG(!step_L,1)*d(log(PE_14)-log(PE_16))*(PMTD_16_{%com}(-1)*MTD_16_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_16_{%com}(-1)*MTD_16_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,2)*d(log(PE_14)-log(PE_17))*(PMTD_17_{%com}(-1)*MTD_17_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_17_{%com}(-1)*MTD_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,3)*d(log(PE_14)-log(PE_18))*(PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)))
  '   endif


  '   if @elem(MTD_16_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTD_n_16_{%com})=  - ES_TRANSP_MARG(!step_L,1)*d(log(PE_16)-log(PE_14))*(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_16_{%com}(-1)*MTD_16_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,4)*d(log(PE_16)-log(PE_17))*(PMTD_17_{%com}(-1)*MTD_17_{%com}(-1)/(PMTD_16_{%com}(-1)*MTD_16_{%com}(-1)+PMTD_17_{%com}(-1)*MTD_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,5)*d(log(PE_16)-log(PE_18))*(PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)/(PMTD_16_{%com}(-1)*MTD_16_{%com}(-1)+PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)))
  '   endif

  '   if @elem(MTD_17_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTD_n_17_{%com})=  - ES_TRANSP_MARG(!step_L,2)*d(log(PE_17)-log(PE_14))*(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_17_{%com}(-1)*MTD_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,4)*d(log(PE_17)-log(PE_16))*(PMTD_16_{%com}(-1)*MTD_16_{%com}(-1)/(PMTD_17_{%com}(-1)*MTD_16_{%com}(-1)+PMTD_16_{%com}(-1)*MTD_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,6)*d(log(PE_17)-log(PE_18))*(PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)/(PMTD_17_{%com}(-1)*MTD_17_{%com}(-1)+PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)))
  '   endif

  '   if @elem(MTD_18_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTD_n_18_{%com})=  - ES_TRANSP_MARG(!step_L,3)*d(log(PE_18)-log(PE_14))*(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)/(PMTD_14_{%com}(-1)*MTD_14_{%com}(-1)+PMTD_18_{%com}(-1)*MTD_18_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,5)*d(log(PE_18)-log(PE_16))*(PMTD_16_{%com}(-1)*MTD_16_{%com}(-1)/(PMTD_18_{%com}(-1)*MTD_16_{%com}(-1)+PMTD_16_{%com}(-1)*MTD_18_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,6)*d(log(PE_18)-log(PE_17))*(PMTD_17_{%com}(-1)*MTD_17_{%com}(-1)/(PMTD_17_{%com}(-1)*MTD_17_{%com}(-1)+PMTD_18_{%com}(-1)*MTD_18_{%com}(-1)))
  '   endif
  '   !step_L=!step_L+1
  ' next

  ' equation 2.13
  ' {%modelname}.equation d(log(MTD[m, c])) = d(log(YQ[c])) + d(SUBST_MTD[m, c]) if MTD[m, c] <> 0, m in {%list_trsp}, c in {%list_com} \ {%list_trsp}

  ' 'equation 2.13
  ' For %mar 14 15 16 17 18
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '     if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
  '       {%modelname}.append d(log(MTM_{%mar}_{%com}))=d(log(M_{%com}))+d(SUBST_MTM_{%mar}_{%com})
  '     endif
  '   next
  ' next

  ' !step_L=1
  ' For %com  01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24

  '   if @elem(MTM_14_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTM_n_14_{%com})=  - ES_TRANSP_MARG(!step_L,1)*d(log(PE_14)-log(PE_16))*(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_16_{%com}(-1)*MTM_16_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,2)*d(log(PE_14)-log(PE_17))*(PMTM_17_{%com}(-1)*MTM_17_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_17_{%com}(-1)*MTM_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,3)*d(log(PE_14)-log(PE_18))*(PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)))
  '   endif


  '   if @elem(MTM_16_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTM_n_16_{%com})=  - ES_TRANSP_MARG(!step_L,1)*d(log(PE_16)-log(PE_14))*(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_16_{%com}(-1)*MTM_16_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,4)*d(log(PE_16)-log(PE_17))*(PMTM_17_{%com}(-1)*MTM_17_{%com}(-1)/(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)+PMTM_17_{%com}(-1)*MTM_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,5)*d(log(PE_16)-log(PE_18))*(PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)/(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)+PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)))
  '   endif

  '   if @elem(MTM_17_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTM_n_17_{%com})=  - ES_TRANSP_MARG(!step_L,2)*d(log(PE_17)-log(PE_14))*(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_17_{%com}(-1)*MTM_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,4)*d(log(PE_17)-log(PE_16))*(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)/(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)+PMTM_17_{%com}(-1)*MTM_17_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,6)*d(log(PE_17)-log(PE_18))*(PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)/(PMTM_17_{%com}(-1)*MTM_17_{%com}(-1)+PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)))
  '   endif

  '   if @elem(MTM_18_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MTM_n_18_{%com})=  - ES_TRANSP_MARG(!step_L,3)*d(log(PE_18)-log(PE_14))*(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)/(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)+PMTM_18_{%com}(-1)*MTM_18_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,5)*d(log(PE_18)-log(PE_16))*(PMTM_16_{%com}(-1)*MTM_16_{%com}(-1)/(PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)+PMTM_16_{%com}(-1)*MTM_16_{%com}(-1))) + _
  '     - ES_TRANSP_MARG(!step_L,6)*d(log(PE_18)-log(PE_17))*(PMTM_17_{%com}(-1)*MTM_17_{%com}(-1)/(PMTM_17_{%com}(-1)*MTM_17_{%com}(-1)+PMTM_18_{%com}(-1)*MTM_18_{%com}(-1)))
  '   endif


  '   !step_L=!step_L+1
  ' next

  ' equation 2.14 & 2.15
  {%modelname}.equation d(log(MC|O|[c])) = d(log(|V|[c])) if MC|O|[c] <> 0 where (O, V) in (D M, YQ M), c in {%list_com} \ 19

  ' 'equation 2.14
  ' For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '   if @elem(MCD_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(log(MCD_{%com}))=d(log(YQ_{%com}))
  '   endif
  ' next

  ' 'equation 2.15
  ' For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '   if @elem(MCM_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(log(MCM_{%com}))=d(log(M_{%com}))
  '   endif
  ' next


  '-------------------STOCK INVENTORIES FOR COMMODIY C'

  ' equations 2.17 & 2.19
  {%modelname}.equation S|O|_n[c] = STEADYSTATE(50,1) * (CI|O|[c] + CH|O|[c] + G|O|[c] + I|O|[c] + X|O|[c]) if S|O|[c] <> 0 where O in D M, c in {%list_com}

  ' For %com {%list_com}
  '   if @elem(SD_{%com},%baseyear) <> 0 then
  '     'equation 2.16 :
  '     if %ref="realist" then
  '       '                   {%modelname}.append DSD_{%com}=d(SD_{%com})     'PROBLEME! Encore non activée. A tester dans compte central réaliste.
  '     endif
  '     'equation 2.17
  '     {%modelname}.append SD_n_{%com}=STEADYSTATE(50,1)*(CID_{%com}+CHD_{%com}+GD_{%com}+ID_{%com}+XD_{%com})
  '   endif

  '   if @elem(SM_{%com},%baseyear) <> 0 then
  '     'equation 2.18
  '     if %ref="realist" then
  '       '                    {%modelname}.append DSM_{%com}=d(SM_{%com})    'PROBLEME! Encore non activée. A tester dans compte central réaliste.
  '     endif

  '     'equation 2.19
  '     {%modelname}.append SM_n_{%com}=STEADYSTATE(50,1)*(CIM_{%com}+CHM_{%com}+GM_{%com}+IM_{%com}+XM_{%com})
  '   endif


  ' next

  '------------------------------------------------------Level II-------------------------------------------------------------------------------
  '**************Sectorial notional energy demand by type of energy

  ' equation 2.20
  {%modelname}.equation d(log(E[c, s])) = d(log(E[s])) + d(SUBST_E[c, s]) if E[c, s] <> 0 where c in {%list_com_E}, s in {%list_sec}

  ' 'equation 2.20
  ' For %sec {%list_sec}
  '   For %com  {%list_com_E}
  '     if @elem(E_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(log(E_{%com}_{%sec}))=d(log(E_{%sec}))+d(SUBST_E_{%com}_{%sec})
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_E_n[ce, s]) = _
                        sum( -ES_NRJ($s, cols_ce(ce, ce_oth)) * d(log(PE[ce, s]) - log(PE[ce_oth, s])) * _
                             value(E[ce_oth, s](-1) / (E[s](-1)) ) if ce_oth <> ce where ce_oth in {%list_com_E}) _
                        if E[ce, s] <> 0 where ce in {%list_com_E}, s in {%list_sec}

  ' !step=1
  ' For %sec {%list_sec}

  '   if @elem(E_21_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_E_n_21_{%sec})= - ES_NRJ(!step,1)*d(log(PE_21_{%sec})-log(PE_22_{%sec}))*(PE_22_{%sec}(-1)*E_22_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,2)*d(log(PE_21_{%sec})-log(PE_23_{%sec}))*(PE_23_{%sec}(-1)*E_23_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,3)*d(log(PE_21_{%sec})-log(PE_24_{%sec}))*(PE_24_{%sec}(-1)*E_24_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1)))
  '   endif

  '   if @elem(E_22_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_E_n_22_{%sec})= - ES_NRJ(!step,1)*d(log(PE_22_{%sec})-log(PE_21_{%sec}))*(PE_21_{%sec}(-1)*E_21_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,4)*d(log(PE_22_{%sec})-log(PE_23_{%sec}))*(PE_23_{%sec}(-1)*E_23_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,5)*d(log(PE_22_{%sec})-log(PE_24_{%sec}))*(PE_24_{%sec}(-1)*E_24_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1)))
  '   endif

  '   if @elem(E_23_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_E_n_23_{%sec})= - ES_NRJ(!step,2)*d(log(PE_23_{%sec})-log(PE_21_{%sec}))*(PE_21_{%sec}(-1)*E_21_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,4)*d(log(PE_23_{%sec})-log(PE_22_{%sec}))*(PE_22_{%sec}(-1)*E_22_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,6)*d(log(PE_23_{%sec})-log(PE_24_{%sec}))*(PE_24_{%sec}(-1)*E_24_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1)))
  '   endif

  '   if @elem(E_24_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_E_n_24_{%sec})= - ES_NRJ(!step,3)*d(log(PE_24_{%sec})-log(PE_21_{%sec}))*(PE_21_{%sec}(-1)*E_21_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,5)*d(log(PE_24_{%sec})-log(PE_22_{%sec}))*(PE_22_{%sec}(-1)*E_22_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1))) _
  '     - ES_NRJ(!step,6)*d(log(PE_24_{%sec})-log(PE_23_{%sec}))*(PE_23_{%sec}(-1)*E_23_{%sec}(-1)/(PE_{%sec}(-1)*E_{%sec}(-1)))
  '   endif

  '   !step=!step+1
  ' next

  '**************** Self employed and employed **************************************************

  ' equation 2.21
  {%modelname}.equation d(log(L_S[s])) = d(log(L[s])) where s in {%list_sec}
  {%modelname}.equation L_SE[s] = L[s] - L_S[s] if L_SE[s] <> 0 where s in {%list_sec}

  ' ' equation 2.21
  ' For %sec {%list_sec}

  '   {%modelname}.append d(log(L_S_{%sec})) = d(log(L_{%sec}))

  '   if @elem(L_SE_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append L_SE_{%sec} = L_{%sec} - L_S_{%sec}
  '   endif
  ' next

  ' equation 2.22
  {%modelname}.equation L_S = sum(L_S[s] if L_S[s] <> 0 where s in {%list_sec})

  ' ' equation 2.22
  ' %equation = "L_S = 0"
  ' For %sec {%list_sec}
  '   if @elem(L_S_{%sec},%baseyear) <> 0 then

  '     %equation = %equation +"+L_S_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' equation 2.23
  {%modelname}.equation L_SE = sum(L_SE[s] if L_SE[s] <> 0 where s in {%list_sec})

  ' ' equation 2.23
  ' %equation = "L_SE = 0"

  ' For %sec {%list_sec}

  '   if @elem(L_SE_{%sec},%baseyear) <> 0 then
  '     %equation = %equation +"+L_SE_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  '********Notional demand for material c of the sector a

  ' equation 2.24
  {%modelname}.equation d(log(MAT[cm, s])) = d(log(MAT[s])) + d(SUBST_MAT[cm, s]) if MAT[cm, s] <> 0 where cm in {%list_com_MAT}, s in {%list_sec}

  ' 'Equation 2.24
  ' !step_L=1
  ' For %sec {%list_sec}
  '   For %com  {%list_com_MAT}
  '     if @elem(MAT_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(log(MAT_{%com}_{%sec}))=d(log(MAT_{%sec}))+d(SUBST_MAT_{%com}_{%sec})
  '     endif
  '   next
  ' next

  ' HACK: transport sectors 15 and 16 can't be substituted to each other
  ' the pair is disabled manually in the sum with trsp*trsp_oth <> 240 (since 15 * 16 == 240)
  {%modelname}.equation d(SUBST_MAT_n[trsp, s]) = _
                        sum( -ES_TRANSP_CI($s, cols_mat(trsp, trsp_oth)) * d(log(PMAT[trsp, s]) - log(PMAT[trsp_oth, s])) * _
                             value(MAT[trsp_oth, s](-1) / (MAT[s](-1)) ) if (trsp_oth <> trsp) * (trsp*trsp_oth <> 240) where trsp_oth in {%list_trsp}) _
                        if MAT[trsp_cond, s] <> 0 where (trsp, trsp_cond) in ({%list_trsp}, 14 15 16 17 17), s in {%list_sec}



  ' !step_L=1 'Step for the line of the matrix
  ' For %sec {%list_sec}
  '   ' PROBLEME ! Diviser par Somme MAT 15 à 18 et non MAT total?
  '   if @elem(MAT_14_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MAT_n_14_{%sec})= - ES_TRANSP_CI(!step_L,1)*d(log(PMAT_14_{%sec})-log(PMAT_15_{%sec}))*(PMAT_15_{%sec}(-1)*MAT_15_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,2)*d(log(PMAT_14_{%sec})-log(PMAT_16_{%sec}))*(PMAT_16_{%sec}(-1)*MAT_16_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,3)*d(log(PMAT_14_{%sec})-log(PMAT_17_{%sec}))*(PMAT_17_{%sec}(-1)*MAT_17_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,4)*d(log(PMAT_14_{%sec})-log(PMAT_18_{%sec}))*(PMAT_18_{%sec}(-1)*MAT_18_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1)))
  '   endif

  '   if @elem(MAT_15_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MAT_n_15_{%sec})= - ES_TRANSP_CI(!step_L,1)*d(log(PMAT_15_{%sec})-log(PMAT_14_{%sec}))*(PMAT_14_{%sec}(-1)*MAT_14_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,5)*d(log(PMAT_15_{%sec})-log(PMAT_17_{%sec}))*(PMAT_17_{%sec}(-1)*MAT_17_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,6)*d(log(PMAT_15_{%sec})-log(PMAT_18_{%sec}))*(PMAT_18_{%sec}(-1)*MAT_18_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1)))
  '   endif

  '   if @elem(MAT_16_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MAT_n_16_{%sec})=  - ES_TRANSP_CI(!step_L,2)*d(log(PMAT_16_{%sec})-log(PMAT_14_{%sec}))*(PMAT_14_{%sec}(-1)*MAT_14_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,7)*d(log(PMAT_16_{%sec})-log(PMAT_17_{%sec}))*(PMAT_17_{%sec}(-1)*MAT_17_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,8)*d(log(PMAT_16_{%sec})-log(PMAT_18_{%sec}))*(PMAT_18_{%sec}(-1)*MAT_18_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1)))
  '   endif

  '   if @elem(MAT_17_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MAT_n_17_{%sec})=  - ES_TRANSP_CI(!step_L,3)*d(log(PMAT_17_{%sec})-log(PMAT_14_{%sec}))*(PMAT_14_{%sec}(-1)*MAT_14_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,5)*d(log(PMAT_17_{%sec})-log(PMAT_15_{%sec}))*(PMAT_15_{%sec}(-1)*MAT_15_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,7)*d(log(PMAT_17_{%sec})-log(PMAT_16_{%sec}))*(PMAT_16_{%sec}(-1)*MAT_16_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,9)*d(log(PMAT_17_{%sec})-log(PMAT_18_{%sec}))*(PMAT_18_{%sec}(-1)*MAT_18_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1)))
  '   endif


  '   if @elem(MAT_17_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_MAT_n_18_{%sec})=  - ES_TRANSP_CI(!step_L,4)*d(log(PMAT_18_{%sec})-log(PMAT_14_{%sec}))*(PMAT_14_{%sec}(-1)*MAT_14_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,6)*d(log(PMAT_18_{%sec})-log(PMAT_15_{%sec}))*(PMAT_15_{%sec}(-1)*MAT_15_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,8)*d(log(PMAT_18_{%sec})-log(PMAT_16_{%sec}))*(PMAT_16_{%sec}(-1)*MAT_16_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1))) _
  '     - ES_TRANSP_CI(!step_L,9)*d(log(PMAT_18_{%sec})-log(PMAT_17_{%sec}))*(PMAT_17_{%sec}(-1)*MAT_17_{%sec}(-1)/(PMAT_{%sec}(-1)*MAT_{%sec}(-1)))
  '   endif
  '   !step_L=!step_L+1
  ' next

  '------------------------------------------------------Level III-------------------------------------------------------------------------------

  '*******Notional Demand for imported material c of the sector'
  ' equation 2.25
  {%modelname}.equation MATM[cm, s] = (MAT[cm, s] - MATD[cm, s] > 0) * (MAT[cm, s] - MATD[cm, s]) + (MAT[cm, s] - MATD[cm, s] =< 0) * (0.00001) if MATM[cm, s] <> 0 where cm in {%list_com_MAT}, s in {%list_sec}

  ' 'equation 2.25
  ' For %sec {%list_sec}
  '   For %com  {%list_com_MAT}
  '     if @elem(MATM_{%com}_{%sec},%baseyear) <> 0 then
  '       '                {%modelname}.append d(log(MATM_{%com}_{%sec}))=d(log(MAT_{%com}_{%sec}))+d(SUBST_MATM_{%com}_{%sec})
  '       {%modelname}.append MATM_{%com}_{%sec}=(MAT_{%com}_{%sec}-MATD_{%com}_{%sec}>0)*(MAT_{%com}_{%sec}-MATD_{%com}_{%sec}) + (MAT_{%com}_{%sec}-MATD_{%com}_{%sec}=<0)*(0.00001)
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_MATM_n[cm, s]) = -ES_CIM($s, $cm) * d(log(PMATM[cm]) - log(PMATD[cm])) * (PMATD[cm](-1)*MATD[cm, s](-1) / (PMAT[cm, s](-1)*MAT[cm, s](-1))) if MATM[cm, s] <> 0 where cm in {%list_com_MAT}, s in {%list_sec}

  ' !step_C=1
  ' For %com {%list_com_MAT}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(MATM_{%com}_{%sec},%baseyear) <> 0 and @elem(MATD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(SUBST_MATM_n_{%com}_{%sec})=  - ES_CIM(!step_L,!step_C)*d(log(PMATM_{%com})-log(PMATD_{%com}))*(PMATD_{%com}(-1)*MATD_{%com}_{%sec}(-1)/(PMAT_{%com}_{%sec}(-1)*MAT_{%com}_{%sec}(-1)))
  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next

  '*******Notional Demand for domestic material c of the sector'

  ' equation 2.26
  {%modelname}.equation d(log(MATD[cm, s])) = d(log(MAT[cm, s])) + d(SUBST_MATD[cm, s]) if MATD[cm, s] <> 0 where cm in {%list_com_MAT}, s in {%list_sec}

  ' 'equation 2.26
  ' For %sec {%list_sec}
  '   For %com  {%list_com_MAT}
  '     if @elem(MATD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(log(MATD_{%com}_{%sec}))=d(log(MAT_{%com}_{%sec}))+d(SUBST_MATD_{%com}_{%sec})
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_MATD_n[cm, s]) = -ES_CIM($s, $cm) * d(log(PMATD[cm]) - log(PMATM[cm])) * (PMATM[cm](-1) * MATM[cm, s](-1) / (PMAT[cm, s](-1) * MAT[cm, s](-1))) if (MATM[cm, s] <> 0) * (MATD[cm, s]) where cm in {%list_com_MAT}, s in {%list_sec}

  ' !step_C=1
  ' For %com {%list_com_MAT}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(MATM_{%com}_{%sec},%baseyear) <> 0 and @elem(MATD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(SUBST_MATD_n_{%com}_{%sec})=  - ES_CIM(!step_L,!step_C)*d(log(PMATD_{%com})-log(PMATM_{%com}))*(PMATM_{%com}(-1)*MATM_{%com}_{%sec}(-1)/(PMAT_{%com}_{%sec}(-1)*MAT_{%com}_{%sec}(-1)))
  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next

  '--------------------Notional Demand for imported energy c of the sector------------------

  ' equation 2.27
  {%modelname}.equation EM[ce, s] = (E[ce, s] - ED[ce, s] > 0) * (E[ce, s] - ED[ce, s]) + (E[ce, s] - ED[ce, s] =< 0) * 0.00001 if EM[ce, s] <> 0 where ce in {%list_com_E}, s in {%list_sec}

  ' 'equation 2.27
  ' For %sec {%list_sec}
  '   For %com  {%list_com_E}
  '     if @elem(EM_{%com}_{%sec},%baseyear) <> 0 then
  '       '                {%modelname}.append d(log(EM_{%com}_{%sec}))=d(log(E_{%com}_{%sec}))+d(SUBST_EM_{%com}_{%sec})
  '       {%modelname}.append EM_{%com}_{%sec}=(E_{%com}_{%sec}-ED_{%com}_{%sec}>0)*(E_{%com}_{%sec}-ED_{%com}_{%sec}) + (E_{%com}_{%sec}-ED_{%com}_{%sec}=<0)*(0.00001)
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_EM_n[ce, s]) = -ES_CIM($s, 20 + $ce) * d(log(PEM[ce]) - log(PED[ce])) * (PED[ce](-1) * ED[ce, s](-1)/(PE[ce, s](-1) * E[ce, s](-1))) if (EM[ce, s] <> 0) * (ED[ce, s] <> 0) where ce in {%list_com_E}, s in {%list_sec}

  ' !step_C=21
  ' For %com {%list_com_E}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(EM_{%com}_{%sec},%baseyear) <> 0 and @elem(ED_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(SUBST_EM_n_{%com}_{%sec})=  - ES_CIM(!step_L,!step_C)*d(log(PEM_{%com})-log(PED_{%com}))*(PED_{%com}(-1)*ED_{%com}_{%sec}(-1)/(PE_{%com}_{%sec}(-1)*E_{%com}_{%sec}(-1)))
  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next


  '-----------------Notional Demand for domestic Energy c of the sector---------------

  ' equation 2.28
  {%modelname}.equation d(log(ED[ce, s])) = d(log(E[ce, s])) + d(SUBST_ED[ce, s]) if ED[ce, s] <> 0 where ce in {%list_com_E}, s in {%list_sec}

  ' 'equation 2.28
  ' For %sec {%list_sec}
  '   For %com  {%list_com_E}
  '     if @elem(ED_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(log(ED_{%com}_{%sec}))=d(log(E_{%com}_{%sec}))+d(SUBST_ED_{%com}_{%sec})
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_ED_n[ce, s]) = -ES_CIM($s, 20 + $ce) * d(log(PED[ce]) - log(PEM[ce])) * (PEM[ce](-1) * EM[ce, s](-1) / (PE[ce, s](-1) * E[ce, s](-1))) if (EM[ce, s] <> 0) * (ED[ce, s] <> 0) where ce in {%list_com_E}, s in {%list_sec}

  ' !step_C=21
  ' For %com {%list_com_E}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(EM_{%com}_{%sec},%baseyear) <> 0 and @elem(ED_{%com}_{%sec},%baseyear) <> 0 then

  '       {%modelname}.append d(SUBST_ED_n_{%com}_{%sec})=  - ES_CIM(!step_L,!step_C)*d(log(PED_{%com})-log(PEM_{%com}))*(PEM_{%com}(-1)*EM_{%com}_{%sec}(-1)/(PE_{%com}_{%sec}(-1)*E_{%com}_{%sec}(-1)))

  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next

  '-------------------------Allocation of Investment between domestic and import---------IMPORT

  ' equation 2.29
  {%modelname}.equation IAM[c, s] = (IA[c, s] - IAD[c, s] > 0) * (IA[c, s] - IAD[c, s]) + (IA[c, s] - IAD[c, s] =< 0) * (0.00001) if IAM[c, s] <> 0 where c in {%list_com}, s in {%list_sec}

  ' 'equation 2.29
  ' For %sec {%list_sec}
  '   For %com  {%list_com}
  '     if @elem(IAM_{%com}_{%sec},%baseyear) <> 0 then
  '       '{%modelname}.append d(log(IAM_{%com}_{%sec}))=d(log(IA_{%com}_{%sec}))+d(SUBST_IAM_{%com}_{%sec})
  '       {%modelname}.append IAM_{%com}_{%sec}=(IA_{%com}_{%sec}-IAD_{%com}_{%sec}>0)*(IA_{%com}_{%sec}-IAD_{%com}_{%sec}) + (IA_{%com}_{%sec}-IAD_{%com}_{%sec}=<0)*(0.00001)
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_IAM_n[c, s]) = -ES_IAM($s, $c) * d(log(PIAM[c]) - log(PIAD[c])) * (PIAD[c](-1) * IAD[c, s](-1) / (PIA[c, s](-1) * IA[c, s](-1))) if (IAM[c, s] <> 0) * (IAD[c, s] <> 0) where c in {%list_com}, s in {%list_sec}

  ' !step_C=1
  ' For %com  {%list_com}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(IAM_{%com}_{%sec},%baseyear) <> 0 and @elem(IAD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(SUBST_IAM_n_{%com}_{%sec})=  - ES_IAM(!step_L,!step_C)*d(log(PIAM_{%com})-log(PIAD_{%com}))*(PIAD_{%com}(-1)*IAD_{%com}_{%sec}(-1)/(PIA_{%com}_{%sec}(-1)*IA_{%com}_{%sec}(-1)))
  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next



  '--------------------------Allocation of Investment between domestic and import---------DOMESTIC

  ' equation 2.30
  {%modelname}.equation d(log(IAD[c, s])) = d(log(IA[c, s])) + d(SUBST_IAD[c, s]) if IAD[c, s] <> 0 where c in {%list_com}, s in {%list_sec}

  ' 'equation 2.30
  ' For %sec {%list_sec}
  '   For %com  {%list_com}
  '     if @elem(IAD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(log(IAD_{%com}_{%sec}))=d(log(IA_{%com}_{%sec}))+d(SUBST_IAD_{%com}_{%sec})
  '     endif
  '   next
  ' next

  {%modelname}.equation d(SUBST_IAD_n[c, s]) = -ES_IAM($s, $c) * d(log(PIAD[c]) - log(PIAM[c])) * (PIAM[c](-1) * IAM[c, s](-1) / (PIA[c, s](-1) * IA[c, s](-1))) if (IAM[c, s] <> 0) * (IAD[c, s] <> 0) where c in {%list_com}, s in {%list_sec}

  ' !step_C=1
  ' For %com  {%list_com}
  '   !step_L=1
  '   For %sec {%list_sec}
  '     if @elem(IAM_{%com}_{%sec},%baseyear) <> 0 and @elem(IAD_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append d(SUBST_IAD_n_{%com}_{%sec})=  - ES_IAM(!step_L,!step_C)*d(log(PIAD_{%com})-log(PIAM_{%com}))*(PIAM_{%com}(-1)*IAM_{%com}_{%sec}(-1)/(PIA_{%com}_{%sec}(-1)*IA_{%com}_{%sec}(-1)))
  '     endif
  '     !step_L=!step_L+1
  '   next
  '   !step_C=!step_C+1
  ' next

  ' equation 2.31 & 2.33
  {%modelname}.equation PMT|O|[trsp] * MT|O|[trsp] = -|V|[trsp] / (YQ[trsp] + M[trsp]) * _
                        ( sum(value(MTD[trsp, c] + MTM[trsp, c]) if MT|O|[trsp, c] <> 0 where c in {%list_com} \ {%list_trsp})  ) _
                        if MT|O|[trsp] <> 0 where trsp in {%list_trsp}, (O, V) in (D M, YQ M)

  ' 'equation 2.31
  ' For %mar {%list_trsp}
  '   if @elem(MTD_{%mar},%baseyear) <> 0 then
  '     %equation ="PMTD_"+%mar+"*MTD_"+%mar+"=0 - YQ_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
  '     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '       if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+ PMTD_"+%mar+"_"+%com+"*MTD_"+%mar+"_"+%com+"+ PMTM_"+%mar+"_"+%com+"*MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation} )
  '   endif
  ' next

  ' equation 2.32 & 2.34
  {%modelname}.equation MT|O|[trsp] = -|V|[trsp] / (YQ[trsp] + M[trsp]) * _
                                      ( sum(MTD[trsp, c] + MTM[trsp, c] if MT|O|[trsp, c] <> 0 where c in {%list_com} \ {%list_trsp}) ) _
                                      if MT|O|[trsp] <> 0 where trsp in {%list_trsp}, (O, V) in (D M, YQ M)

  ' 'equation 2.32
  ' For %mar {%list_trsp}
  '   if @elem(MTD_{%mar},%baseyear) <> 0 then
  '     %equation ="MTD_"+%mar+"=0 - YQ_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
  '     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '       if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+ MTD_"+%mar+"_"+%com+"+ MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation} )
  '   endif
  ' next

  ' 'equation 2.33
  ' For %mar {%list_trsp}
  '   if @elem(MTM_{%mar},%baseyear) <> 0 then
  '     %equation ="PMTM_"+%mar+"*MTM_"+%mar+"=0 - M_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
  '     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '       if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+ PMTD_"+%mar+"_"+%com+"*MTD_"+%mar+"_"+%com+"+ PMTM_"+%mar+"_"+%com+"*MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation}   )
  '   endif
  ' next

  ' 'equation 2.34
  ' For %mar {%list_trsp}
  '   if @elem(MTM_{%mar},%baseyear) <> 0 then
  '     %equation ="MTM_"+%mar+"=0 - M_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
  '     For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '       if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+ MTD_"+%mar+"_"+%com+"+ MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation} )
  '   endif
  ' next

  ' equation 2.35 - 2.38
  {%modelname}.equation PMC|O|[19] * MC|O|[19] = -|V|[19] / (YQ[19] + M[19]) * (sum(value(MCM[c] + MCD[c]) where c in {%list_com} \ 19)) if MC|O|[19] <> 0 where (O, V) in (D M, YQ M)

  {%modelname}.equation MC|O|[19] = -|V|[19] / (YQ[19] + M[19]) * (sum(MCM[c] + MCD[c] where c in {%list_com} \ 19)) if MC|O|[19] <> 0 where (O, V) in (D M, YQ M)

  ' if @elem(MCD_19,%baseyear) <> 0 then

  '   'equation 2.35
  '   %equation ="PMCD_19*MCD_19=0 - YQ_19/(YQ_19+M_19)*("
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '     %equation = %equation+"+ PMCM_"+%com+"*MCM_"+%com+"+PMCD_"+%com+"*MCD_"+%com
  '   next
  '   {%modelname}.append {%equation} )


  '   'equation 2.36
  '   %equation ="MCD_19=0 - YQ_19/(YQ_19+M_19)*("
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '     %equation = %equation+"+ MCM_"+%com+"+MCD_"+%com
  '   next
  '   {%modelname}.append {%equation} )

  ' endif

  ' if @elem(MCM_19,%baseyear) <> 0 then

  '   'equation 2.37
  '   %equation ="PMCM_19*MCM_19=0 - M_19/(YQ_19+M_19)*("
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '     %equation = %equation+"+ PMCM_"+%com+"*MCM_"+%com+"+PMCD_"+%com+"*MCD_"+%com
  '   next
  '   {%modelname}.append {%equation} )

  '   'equation 2.38
  '   %equation ="MCM_19=0 - M_19/(YQ_19+M_19)*("
  '   For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
  '     %equation = %equation+"+ MCM_"+%com+"+MCD_"+%com
  '   next
  '   {%modelname}.append {%equation} )
  ' endif

  '------------------------------------EXPORTS-------------------------------
  'La variable World Demand (WD) est exogene
  ' equation 2.39
  {%modelname}.equation d(log(X[c])) = d(log(WD[c])) + d(SUBST_X[c]) if X[c] <> 0 where c in {%list_com}
  {%modelname}.equation d(SUBST_X_n[c]) = -ES_X($c, 1) * d(log(PX[c]) - log(TC * PWD[c])) if X[c] <> 0 where c in {%list_com}

  ' 'equation 2.39
  ' !step_C=0
  ' For %com {%list_com}
  '   if @elem(X_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(log(X_{%com})) = d(log(WD_{%com})) + d(SUBST_X_{%com})
  '     {%modelname}.append d(SUBST_X_n_{%com})=- ES_X(!step_C+1,1)*d(log(PX_{%com})-log(TC*PWD_{%com}))
  '   endif
  '   !step_C=!step_C+1
  ' next

  ' equation 2.40
  {%modelname}.equation d(log(XD[c])) = d(log(X[c])) + d(SUBST_XD[c]) if X[c] <> 0 where c in {%list_com}
  {%modelname}.equation XM[c] = (X[c] - XD[c] > 0) * (X[c] - XD[c]) + (X[c] - XD[c] <= 0) * (0.00001) if XM[c] <> 0 where c in {%list_com}

  ' 'equation 2.40
  ' For %com {%list_com}
  '   if @elem(X_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(log(XD_{%com}))=d(log(X_{%com}))+d(SUBST_XD_{%com})
  '   endif
  ' next

  ' equation 2.41
  {%modelname}.equation d(SUBST_X|O1|_n[c]) = -ES_X($c, 2) * d(log(PX|O1|[c]) - log(PX|O2|[c])) * (PX|O2|[c](-1) * X|O2|[c](-1) / (PX[c](-1) * X[c](-1))) if X|O1|[c] <> 0 where c in {%list_com}, (O1, O2) in (D M, M D)

  ' !step_C=0
  ' For %com {%list_com}
  '   if @elem(XD_{%com},%baseyear) <> 0 then
  '     {%modelname}.append d(SUBST_XD_n_{%com})=- ES_X(!step_C+1,2)*d(log(PXD_{%com})-log(PXM_{%com}))*(PXM_{%com}(-1)*XM_{%com}(-1)/(PX_{%com}(-1)*X_{%com}(-1)))
  '   endif
  '   !step_C=!step_C+1
  ' next


  ' !step_C=0
  ' For %com {%list_com}
  '   if @elem(XM_{%com},%baseyear) <> 0 then
  '     {%modelname}.append XM_{%com}=(X_{%com}-XD_{%com}>0)*(X_{%com}-XD_{%com}) + (X_{%com}-XD_{%com}<=0)*(0.00001)

  '     '       {%modelname}.append d(log(XM_{%com}))=d(log(X_{%com}))+d(SUBST_XM_{%com})
  '     {%modelname}.append d(SUBST_XM_n_{%com})=- ES_X(!step_C+1,2)*d(log(PXM_{%com})-log(PXD_{%com}))*(PXD_{%com}(-1)*XD_{%com}(-1)/(PX_{%com}(-1)*X_{%com}(-1)))
  '   endif
  '   !step_C=!step_C+1
  ' next



  '------------------------------***Commercial Balance***---------------------
  ' equation 2.42
  {%modelname}.equation DC_VAL[c] = PX[c] * X[c] - PM[c] * M[c] where c in {%list_com}

  ' 'equation 2.42
  ' 'Per sector'
  ' For %com {%list_com}
  '   {%modelname}.append DC_VAL_{%com}=PX_{%com}*X_{%com}-PM_{%com}*M_{%com}
  ' next

  ' equation 2.43
  {%modelname}.equation DC_VAL = sum(DC_VAL[c] where c in {%list_com})

  ' 'equation 2.43
  ' %equation = "DC_VAL=0"
  ' For %com {%list_com}
  '   %equation = %equation +"+DC_VAL_"+%com

  ' next
  ' {%modelname}.append {%equation}

  {%modelname}.equation PROG_K[s] = PROG_K[s](-1) * (1 + GR_PROG_K[s]) where s in {%list_sec}
  {%modelname}.equation PROG_L[s] = PROG_L[s](-1) * (1 + GR_PROG_L[s]) where s in {%list_sec}
  {%modelname}.equation PROG_E[s] = PROG_E[s](-1) * (1 + GR_PROG_E[s]) where s in {%list_sec}

  ' For %sec {%list_sec}
  '   {%modelname}.append  PROG_K_{%sec} = PROG_K_{%sec}(-1)*(1+GR_PROG_K_{%sec})
  '   {%modelname}.append  PROG_L_{%sec} = PROG_L_{%sec}(-1)*(1+GR_PROG_L_{%sec})
  '   {%modelname}.append  PROG_E_{%sec} = PROG_E_{%sec}(-1)*(1+GR_PROG_E_{%sec})
  ' next

  '**************************************************************************************************************************************
  '******************************************** END BLOCK 2 : PRODUCER ************************************************
  '**************************************************************************************************************************************







endsub