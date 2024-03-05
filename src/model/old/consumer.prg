'***************************************************************************************************************************************
'******************************************** BLOCK 4 :CONSUMER ****************************************************
'**************************************************************************************************************************************
'**************************************************************************************************************************************



subroutine BLOCK_Consumer

 'equation 4.1
 {%modelname}.equation W_S = (sum(W_S[s] * L_S[s], s in {%list_sec}))/L_S

'   'equation 4.1
'   %equation = "W_S=0+("
'   For %sec {%list_sec}
'     %equation = %equation +"+W_S_"+%sec+"*L_S_"+%sec
'   next
'   %equation = %equation + ")/L_S"
'   {%modelname}.append {%equation}

 'equation 4.2
 {%modelname}.equation W_SE = (sum(W_SE[s] * L_SE[s], s in {%list_sec}))/L_SE

'   'equation 4.2
'   %equation = "W_SE=0+("
'   For %sec {%list_sec}
'     if @elem(W_SE_{%sec},%baseyear) <> 0 then
'       %equation = %equation +"+W_SE_"+%sec+"*L_SE_"+%sec
'     endif
'   next
'   %equation = %equation + ")/L_SE"
'   {%modelname}.append {%equation}

 'equations 4.3
 {%modelname}.equation CL_S = (sum(CL_S[s] * L_S[s], s in {%list_sec}))/L_S

'   'equation 4.3
'   %equation = "CL_S=0+("
'   For %sec {%list_sec}
'     %equation = %equation +"+CL_S_"+%sec+"*L_S_"+%sec
'   next
'   %equation = %equation + ")/L_S"
'   {%modelname}.append {%equation}

 'equation 4.4
 {%modelname}.equation CL_SE = (sum(CL_S[s] * L_SE[s], s in {%list_sec}))/L_SE

'   'equation 4.4
'   %equation = "CL_SE=0+("
'   For %sec {%list_sec}
'     %equation = %equation +"+CL_SE_"+%sec+"*L_SE_"+%sec
'   next
'   %equation = %equation + ")/L_SE"
'   {%modelname}.append {%equation}

  'equation 4.5
  {%modelname}.append W * L = W_S * L_S + W_SE * L_SE

  'equation 4.6
  {%modelname}.append CL*L = (CL_S * L_S + CL_SE * L_SE)

  'equation 4.7
  {%modelname}.append L = L_S + L_SE

  'equation 4.9
  {%modelname}.append INT_VAL = Stock_S(-1) * R_S
  {%modelname}.append d(R_S) =  d(R)
  {%modelname}.append DIV_HH_VAL_n = alpha_HH_FW * PRF_NET * RF_NET
  {%modelname}.append FW_VAL = DIV_HH_VAL + INT_VAL

 'equation 4.9.1
 {%modelname}.equation DIV_HH_VAL[h] = PHI_DIS_DIV[h] * DIV_HH_VAL, h in {%list_household}
 {%modelname}.equation INT_VAL[h] = PHI_DIS_INT[h] * INT_VAL, h in {%list_household}
 {%modelname}.equation PRESOC_DOM_VAL[h] = PHI_DIS_PRESOC[h] * PRESOC_DOM_VAL, h in {%list_household}
 {%modelname}.equation L_S[h] = PHI_DIS_LS[h] * L_S, h in {%list_household}
 {%modelname}.equation L_SE[h] = PHI_DIS_LSE[h] * L_SE, h in {%list_household}
 {%modelname}.equation W_S[h] = ALPHA_DIS_WS[h] * W_S, h in {%list_household}
 {%modelname}.equation W_SE[h] = ALPHA_DIS_WSE[h] * W_SE, h in {%list_household}

'   'equation 4.9.1
'
'   For %hous {%list_household}
'
'     {%modelname}.append DIV_HH_VAL_{%hous}=PHI_DIS_DIV_{%hous}*DIV_HH_VAL
'     {%modelname}.append INT_VAL_{%hous}=PHI_DIS_INT_{%hous}*INT_VAL
'     {%modelname}.append PRESOC_DOM_VAL_{%hous}=PHI_DIS_PRESOC_{%hous}*PRESOC_DOM_VAL
'     {%modelname}.append L_S_{%hous}=PHI_DIS_LS_{%hous}*L_S
'     {%modelname}.append L_SE_{%hous}=PHI_DIS_LSE_{%hous}*L_SE
'     {%modelname}.append W_S_{%hous}=ALPHA_DIS_WS_{%hous}*W_S
'     {%modelname}.append W_SE_{%hous}=ALPHA_DIS_WSE_{%hous}*W_SE
'
'   next

 'equation 4.9.2
 {%modelname}.equation FW_VAL[h] = DIV_HH_VAL[h] + INT_VAL[h], h in {%list_household}

'   'equation 4.9.2
'
'   For %hous {%list_household}
'     {%modelname}.append FW_VAL_{%hous}=DIV_HH_VAL_{%hous}+INT_VAL_{%hous}
'   next


  '----------------------------------------***LEVEL I***-----------------------------------------'
 'equation 4.10
 {%modelname}.append DISPINC_AI_VAL = (W_S * L_S + SB_ROW) * (1-TCSS) + (W_SE * L_SE) * (1-TCSS_SE) + PRESOC_DOM_VAL + FW_VAL + TR_ROW_VAL

 'equation 4.11
 {%modelname}.equation DISPINC_AI_VAL[h] = (W_S[h] * L_S[h] + SB_ROW[h]) * (1-TCSS[h]) + (W_SE[h] * L_SE[h]) * (1-TCSS_SE[h]) + PRESOC_DOM_VAL[h] + FW_VAL[h] + TR_ROW_VAL[h], h in {%list_household}

'   For %hous {%list_household}
'     {%modelname}.append DISPINC_AI_VAL_{%hous}=(W_S_{%hous}*L_S_{%hous}+SB_ROW_{%hous})*(1-TCSS_{%hous})+(W_SE_{%hous}*L_SE_{%hous})*(1-TCSS_SE_{%hous})+PRESOC_DOM_VAL_{%hous}+FW_VAL_{%hous}+TR_ROW_VAL_{%hous}
'     '{%modelname}.append DISPINC_AI_VAL=(W_S*L_S+SB_ROW)*(1-TCSS)+(W_SE*L_SE)*(1-TCSS_SE)+PRESOC_DOM_VAL+FW_VAL+TR_ROW_VAL
'   next
'
'   'equation 4.11
'   'For %hous {%list_household}
'   '{%modelname}.append DISPINC_AI_VAL_{%hous}=PhiDI_{%hous}*DISPINC_AI_VAL
'   'next

 'equation 4.12
 {%modelname}.equation DISPINC_VAL[h] = DISPINC_AI_VAL[h] - IR_VAL[h] - AIC_VAL[h], h in {%list_household}

'   'equation 4.12
'   For %hous {%list_household}
'     {%modelname}.append DISPINC_VAL_{%hous}=DISPINC_AI_VAL_{%hous}-IR_VAL_{%hous}-AIC_VAL_{%hous}
'   next

 'equation 4.13
 {%modelname}.equation DISPINC_VAL = sum(DISPINC_VAL[h] if DISPINC_VAL[h] <> 0, h in {%list_household})

'   'equation 4.13
'   %equation = "DISPINC_VAL=0"
'   For %hous {%list_household}
'     if @elem(DISPINC_VAL_{%hous},%baseyear) <> 0 then
'       %equation = %equation +"+ DISPINC_VAL_"+%hous
'     endif
'   next
'   {%modelname}.append {%equation}



  ' ***********************************************************************************************************************
  ' ******************************************* HOUSEHOLDS HYBRID****************************************************************************
  ' ***********************************************************************************************************************

  if %hybrid_household="yes" then
    Call BLOCK_Consumer_Hybrid

    ' ***********************************************************************************************************************
    ' *******************************************END HOUSEHOLDS HYBRID****************************************************************************
    ' ***********************************************************************************************************************8




  else


    ' ***********************************************************************************************************************
    ' *******************************************HOUSEHOLDS LES****************************************************************************
    ' ***********************************************************************************************************************8

 'equation 4.14 & 4.15
 {%modelname}.equation(pv) EXP[h] = sum(EXP[c, h] where c in {%list_com}) where h in {%list_household}

'     'equation 4.14 4.15
'     For %hous {%list_household}
'       %equation = "PEXP_"+%hous+"*EXP_"+%hous+"= 0"
'       For %com {%list_com}
'         %equation = %equation+"+PEXP_"+%com+"_"+%hous+"*EXP_"+%com+"_"+%hous
'       next
'       {%modelname}.append {%equation}
'
'       %equation = "EXP_"+%hous+"= 0"
'       For %com {%list_com}
'         %equation = %equation+"+ EXP_"+%com+"_"+%hous
'       next
'       {%modelname}.append {%equation}
'
'     next

 'equation 4.16 & 4.17
 {%modelname}.equation(pv) EXPH = sum(EXP[h] , h in {%list_household})

'     'equation 4.16 4.17
'     %equation = "EXPH= 0"
'     For %hous {%list_household}
'       %equation = %equation+"+EXP_"+%hous
'     next
'     {%modelname}.append {%equation}
'
'     %equation = "PEXPH*EXPH= 0"
'     For %hous {%list_household}
'       %equation = %equation+"+PEXP_"+%hous+"*EXP_"+%hous
'     next
'     {%modelname}.append {%equation}

 'equation 4.18
  {%modelname}.equation d(MPS[h])= household_hybrid(52, 1 + $h) * d(UNR_TOT) + household_hybrid(53, 1 + $h)* d(R - infl_FR) + household_hybrid(54, 1 + $h) * d(DEBT_G_VAL/(PGDP * GDP)), h in {%list_household}

'     'equation 4.18 Marginal Propension to Save'
'     !step_2 = !step_HH
'     For %hous {%list_household}
'
'       {%modelname}.append d(MPS_{%hous})= household_hybrid(52,1+!step_2)*d(UNR_TOT) + household_hybrid(53,1+!step_2)*d(R-infl_FR) + household_hybrid(54,1+!step_2)*d(DEBT_G_VAL/(PGDP*GDP))
'       !step_2 = !step_2 + 1
'     next

 'equation 4.19 & 4.20
 {%modelname}.equation S[h] = DISPINC_VAL[h] - PEXP[h] * EXP[h], h in {%list_household}
 {%modelname}.equation TS[h] = (DISPINC_VAL[h] - PEXP[h] * EXP[h])/DISPINC_VAL[h], h in {%list_household}

'     'equation 4.19 4.20
'     For %hous {%list_household}
'       {%modelname}.append S_{%hous}=(DISPINC_VAL_{%hous}-PEXP_{%hous}*EXP_{%hous})
'       {%modelname}.append TS_{%hous}=(DISPINC_VAL_{%hous}-PEXP_{%hous}*EXP_{%hous})/DISPINC_VAL_{%hous}
'     next

 'equation 4.21 4.22
 {%modelname}.append S = DISPINC_VAL - PEXPH*EXPH
 {%modelname}.append TS = S/DISPINC_VAL
 {%modelname}.append Stock_S=Stock_S(-1)+S

 'equation 4.23
 {%modelname}.equation EXP_n[c, h] * PEXP[c, h] = PEXP[c, h] * NEXP[c, h] + BetaExp[c, h] * (DISPINC_VAL[h] * (1 - MPS[h]) - PNEXP[h] * NEXP[h]) if EXP[c, h] <> 0, c in {%list_com}, h in {%list_household}

'     'equation 4.23
'     For %hous {%list_household}
'       For %com {%list_com}
'         if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
'           {%modelname}.append EXP_n_{%com}_{%hous}*PEXP_{%com}_{%hous}=PEXP_{%com}_{%hous}*NEXP_{%com}_{%hous}+BetaExp_{%com}_{%hous}*(DISPINC_VAL_{%hous}*(1 - MPS_{%hous})-PNEXP_{%hous}*NEXP_{%hous})
'         endif
'       next
'     next

 'equation 4.24
 {%modelname}.equation d(log(BetaExp[c, h])) = (1-ES_LES_CES(1, 1)) * d(log(PEXP[c, h]/PEXP_CES[h])) if BetaExp[c, h] <> 0, c in {%list_com}, h in {%list_household}

 'equation 4.25
 {%modelname}.equation PEXP_CES[h] = (sum(@elem(BetaExp[c, h], {%baseyear}) * PEXP[c, h]^(1-ES_LES_CES(1,1)), c in {%list_com}))^(1/(1-ES_LES_CES(1,1))), h in {%list_household}

'     'equation 4.24
'     For %hous {%list_household}
'       For %com {%list_com}
'         if @elem(BetaExp_{%com}_{%hous},%baseyear) <> 0 then
'           {%modelname}.append d(log(BetaExp_{%com}_{%hous})) = (1-ES_LES_CES(1,1))*d(log(PEXP_{%com}_{%hous}/PEXP_CES_{%hous}))
'         endif
'       next

'       'equation 4.25
'       %equation = "PEXP_CES_"+%hous+"= ("
'       For %com {%list_com}
'         %equation = %equation+ "+@elem(BetaExp_"+%com+"_"+%hous+","+%baseyear+")*PEXP_"+%com+"_"+%hous+"^(1-ES_LES_CES(1,1))"
'       next
'       %equation = %equation+")^(1/(1-ES_LES_CES(1,1)))"
'
'       {%modelname}.append {%equation}
'
'     next

 'equation 4.26 & 4.27
 {%modelname}.equation(pv) NEXP[h] = sum(NEXP[c, h] , c in {%list_com}) if NEXP[h] <> 0, h in {%list_household}

'     'equation 4.26
'     For %hous {%list_household}
'       if @elem(NEXP_{%hous},%baseyear) <> 0 then
'         %equation = "PNEXP_"+%hous+"*NEXP_"+%hous+"= 0"
'         For %com {%list_com}
'           %equation = %equation+"+PEXP_"+%com+"_"+%hous+"*NEXP_"+%com+"_"+%hous
'         next
'         {%modelname}.append {%equation}
'       endif
'     next
'
'     'equation 4.27
'     For %hous {%list_household}
'       %equation = "NEXP_"+%hous+"= 0"
'       For %com {%list_com}
'         %equation = %equation+"+NEXP_"+%com+"_"+%hous
'       next
'       {%modelname}.append {%equation}
'     next

 'equation 4.28  PEXP_com.EXP_com equation dans lyx mais pas ici pourquoi?

 'equation 4.29
 {%modelname}.equation EXP[c] = sum(EXP[c, h] if EXP[c, h] <> 0, h in {%list_household}), c in {%list_com}

'     'equation 4.29
'     For %com {%list_com}
'       %equation = "EXP_"+%com+"=0"
'       For %hous {%list_household}
'         if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
'           %equation = %equation+"+EXP_"+%com+"_"+%hous
'         endif
'       next
'       {%modelname}.append {%equation}
'     next

 'equation 4.30 & 4.31
 {%modelname}.equation Phi_EXP[c, h] = EXP[c, h]/EXP[c] if EXP[c] <> 0, c in {%list_com}, h in {%list_household}
 {%modelname}.equation PEXP[c, h] = PCH[c], c in {%list_com}, h in {%list_household}

'     'equation 4.30
'     For %com {%list_com}
'       For %hous {%list_household}
'         if @elem(EXP_{%com},%baseyear) <> 0 then
'           {%modelname}.append Phi_EXP_{%com}_{%hous}= (EXP_{%com}_{%hous})/(EXP_{%com})
'         endif
'       next
'     next
'
'     'equation 4.31
'     'PROBLEME! attention a com transp et auto quand réintégration des sous blocs'
'     For %hous {%list_household}
'       For %com {%list_com}
'         {%modelname}.append PEXP_{%com}_{%hous}=PCH_{%com}
'       next
'     next

 '----------------------------------------***LEVEL V***-----------------------------------------'
 'equation 4.32
 {%modelname}.equation d(log(CHD[c])) = d(log(EXP[c])) + d(SUBST_CHD[c]) if CHD[c] <> 0, c in {%list_com}
 {%modelname}.equation d(SUBST_CHD_n[c])= - ES_LVL4_HH(1, 1) * d(log(PCHD[c])-log(PCHM[c])) * (PCHM[c](-1) * CHM[c](-1)/(PCH[c](-1) * CH[c](-1))) if CHD[c] <> 0, c in {%list_com}

'   'equation 4.32'
'
'   !step_1=0
'   For %com  {%list_com}
'     if @elem(CHD_{%com},%baseyear) <> 0 then
'       {%modelname}.append d(log(CHD_{%com})) = d(log(EXP_{%com}))+d(SUBST_CHD_{%com})
'       {%modelname}.append d(SUBST_CHD_n_{%com})= - ES_LVL4_HH(1,1)*d(log(PCHD_{%com})-log(PCHM_{%com}))*(PCHM_{%com}(-1)*CHM_{%com}(-1)/(PCH_{%com}(-1)*CH_{%com}(-1)))
'     endif
'     !step_1=!step_1+1
'   next

 'equation 4.33
 {%modelname}.equation CHM[c] = (EXP[c]-CHD[c] > 0) * (EXP[c] - CHD[c]) + (EXP[c] - CHD[c] =< 0) * (0.00001) if CHM[c] <> 0, c in {%list_com}
 {%modelname}.equation d(SUBST_CHM_n[c]) = - ES_LVL4_HH(1, 1)*d(log(PCHM[c]) - log(PCHD[c])) * (PCHD[c](-1) * CHD[c](-1)/(PCH[c](-1) * CH[c](-1))) if CHM[c] <> 0, c in {%list_com}

'   'equation 4.33'
'   !step_1=0
'   For %com  {%list_com}
'     if @elem(CHM_{%com},%baseyear) <> 0 then
'       {%modelname}.append CHM_{%com}=(EXP_{%com}-CHD_{%com}>0)*(EXP_{%com}-CHD_{%com})+(EXP_{%com}-CHD_{%com}=<0)*(0.00001)
'       '{%modelname}.append d(log(CHM_{%com})) = d(log(EXP_{%com}))+d(SUBST_CHM_{%com})
'       '{%modelname}.append d(SUBST_CHM_n_{%com})= - ES_LVL4_HH(1,1)*d(log(PCHM_{%com})-log(PCHD_{%com}))*(PCHD_{%com}(-1)*CHD_{%com}(-1)/(PCH_{%com}(-1)*CH_{%com}(-1)))
'     endif
'     !step_1=!step_1+1
'   next


    ' Ajustment Line LES--->Ajustement de demande notionnelle de la part des ménages

    'PROBLEM! Changement liste, exclure l'énergie, ,le logement et le transport'

 'equations 4.34 & 4.35
 {%modelname}.equation  log(EXP[c, h]) = ADJUST(20 + $c, 1) * log(EXP_n[c, h]) + (1-ADJUST(20 + $c, 1)) * (log(EXP[c, h](-1)) + d(log(EXP_e[c, h]))) if EXP[c, h] <> 0, c in {%list_com}, h in {%list_household}
 {%modelname}.equation d(log(EXP_e[c, h])) = ADJUST(20 + $c, 2) * d(log(EXP_e[c, h](-1))) + ADJUST(20 + $c, 3) * d(log(EXP[c, h](-1))) + ADJUST(20 + $c, 4) * d(log(EXP_n[c, h])) if EXP[c, h] <> 0, c in {%list_com}, h in {%list_household}
 '+ ADJUST(20 + $c, 5)* d(log(EXP[c, h](+1)))

'   !step_1=0
'     'equation 4.34 4.35'
'     For %com {%list_com}
'       For %hous {%list_household}
'         if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
'           {%modelname}.append  log(EXP_{%com}_{%hous}) = ADJUST(21+!step_1,1)*log(EXP_n_{%com}_{%hous}) + (1-ADJUST(21+!step_1,1))*(log(EXP_{%com}_{%hous}(-1))+d(log(EXP_e_{%com}_{%hous})))
'
'           {%modelname}.append d(log(EXP_e_{%com}_{%hous})) = ADJUST(21+!step_1,2)*d(log(EXP_e_{%com}_{%hous}(-1))) + ADJUST(21+!step_1,3)*d(log(EXP_{%com}_{%hous}(-1))) + ADJUST(21+!step_1,4)*d(log(EXP_n_{%com}_{%hous}))
'           '+ ADJUST(21+!step_1,5)*d(log(EXP_{%com}_{%hous}(+1)))
'
'         endif
'       next
'       !step_1=!step_1+1
'     next


    ' ***********************************************************************************************************************
    ' ************************** *****************END HOUSEHOLDS LES****************************************************************************
    ' ***********************************************************************************************************************8



  endif

endsub



'**************************************************************************************************************************************
'******************************************** END BLOCK 4 :CONSUMER ************************************************
'**************************************************************************************************************************************
