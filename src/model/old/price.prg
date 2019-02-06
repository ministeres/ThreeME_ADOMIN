subroutine BLOCK_Price
  '***************************************************************************************************************************************
  '******************************************** BLOCK 5 : PRICES ****************************************************
  '**************************************************************************************************************************************
  '**************************************************************************************************************************************
    'equation 5.1
    {%modelname}.equation PY_n[s]*Y[s] = (CK[s]*K_n[s]+ CL[s]*L_n[s]*PROG_L[s] + PE[s]*E_n[s] + PMAT[s]*MAT_n[s]) * (1 + TMD[s]) if Y[s] > 0 where s in {%list_sec} \ 21
    {%modelname}.append PY_n_21 = PM_21

    'equation 5.2
    {%modelname}.equation NCU[s]*Y[s] = CU[s]*Y[s] + PIY[s]*IY[s] + PSY[s]*SY[s] + PIS[s]*IS[s] if Y[s] > 0 where s in {%list_sec}

    'equation 5.3
    {%modelname}.equation  CU[s]*Y[s] = CK[s]*K[s] + CL[s]*L[s]*PROG_L[s] + PE[s]*E[s] + PMAT[s]*MAT[s] if Y[s] > 0 where s in {%list_sec}

    'equation 5.4
    {%modelname}.equation CL[s]*L[s] = CL_S[s]*L_S[s] + CL_SE[s]*L_SE[s] if Y[s] > 0 where s in {%list_sec}

    'equation 5.5
    {%modelname}.equation d(log(1+TMD[s])) = STEADYSTATE(49,1) * ( d(log(Y[s])) - d(log(Y[s](-1))) ) if Y[s] > 0 where s in {%list_sec}


    'equation 5.6
    'PROBLEME equation too complex or syntax error'
    '{%modelname}.equation d(log(YOPT[s])) =   d(log(K[s]))*CK[s](-1)*K[s](-1) / (CU[s](-1)*Y[s](-1) _
                                            '+ d(log(L[s]*PROG_L[s]))*CL[s](-1)*L[s](-1)*PROG_L[s](-1) / (CU[s](-1)*Y[s](-1 _
                                            '+ d(log(E[s] + (E[s]=0)*0.0000000001))*PE[s](-1)*E[s](-1) / (CU[s](-1)*Y[s](-1)) _
                                            '+ d(log(MAT[s]))*PMAT[s](-1)*MAT[s](-1) / (CU[s](-1)*Y[s](-1)) _
                                            'if YOPT[s] > 0, s in {%list_sec}

  'equation 5.6
  For %sec {%list_sec}
    if @elem(YOPT_{%sec},%baseyear) <> 0 then
      {%modelname}.append d(log(YOPT_{%sec})) =   d(log(K_{%sec}))*CK_{%sec}(-1)*K_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)) _
      + d(log(L_{%sec}*PROG_L_{%sec}))*CL_{%sec}(-1)*L_{%sec}(-1)*PROG_L_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)) _
      + d(log(E_{%sec}+(E_{%sec}=0)*0.0000000001))*PE_{%sec}(-1)*E_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1)) _
      + d(log(MAT_{%sec}))*PMAT_{%sec}(-1)*MAT_{%sec}(-1)/(CU_{%sec}(-1)*Y_{%sec}(-1))
    endif
  next

    'equation 5.7 5.8 labor cost in activity a
    {%modelname}.equation CL_S[s] =  W_S[s]*(1+TCSE[s]) / PROG_L[s] if L_S[s] > 0 where s in {%list_sec}
    {%modelname}.equation CL_SE[s] = W_SE[s] / PROG_L[s] if L_S[s] > 0 where s in {%list_sec}

    'equation 5.9 5.10 capital cost in activity a
    {%modelname}.equation CK[s] = PIA[s] * (Tdec[s] + r[s]) if K[s] > 0 where s in {%list_sec}

    'equation 5.11
    {%modelname}.equation PMAT[s]*MAT[s] = sum(PMAT[c,s]*MAT[c,s] if MAT[c,s] <> 0 where c in {%list_com_MAT}) if MAT[s] <> 0 where s in {%list_sec}

 'For %sec {%list_sec}
    ''''equation 5.11: Material cost in activity a
    'if @elem(MAT_{%sec},%baseyear) <> 0 then
      '%equation = "PMAT_"+%sec+"*MAT_"+%sec+"=0"
      'For %com {%list_com_MAT}
        'if @elem(MAT_{%com}_{%sec},%baseyear) <> 0 then
          '%equation = %equation +"+ PMAT_"+%com+"_"+%sec+"*MAT_"+%com+"_"+%sec
        'endif
      'next
      '{%modelname}.append {%equation}
    'endif


    'equation 5.12: Energy cost in activity a
    {%modelname}.equation PE[s]*E[s] = sum(PE[c,s]*E[c,s] if E[c,s] <> 0 where c in {%list_com_E}) if E[s] <> 0 where s in {%list_sec}

    ''equation 5.12: Energy cost in activity a
    'if @elem(E_{%sec},%baseyear) <> 0 then
      '%equation = "PE_"+%sec+"*E_"+%sec+"=0"
      'For %com {%list_com_E}
        'if @elem(E_{%com}_{%sec},%baseyear) <> 0 then
          '%equation = %equation +"+ PE_"+%com+"_"+%sec+"*E_"+%com+"_"+%sec
        'endif
      'next
      '{%modelname}.append {%equation}
    'endif
  'next

    'equation 5.13
    For %sec {%list_sec}
    ' Simplification: equation standard du cout d'usage du capital
    'NOTE: Introduire fonction d'accumulation de la dette en fonction de l'autofinancement'
    ''   {%modelname}.append DEBT_{%sec} = K_{%sec}

    next

    'equation 5.14
    {%modelname}.equation PMAT[c,s]*MAT[c,s] = PMATD[c]*MATD[c,s] + PMATM[c]*MATM[c,s] if MAT[c,s] <> 0 where s in {%list_sec}, c in {%list_com_MAT}


  'For %sec {%list_sec}
    'For %com {%list_com_MAT}
      'if @elem(MAT_{%com}_{%sec},%baseyear) <> 0 then
        ''{%modelname}.append PMAT_{%com}_{%sec}*MAT_{%com}_{%sec}=PMATM_{%com}*MATM_{%com}_{%sec}+PMATD_{%com}*MATD_{%com}_{%sec}
      'endif
    'next
  'next


    'equation 5.15
    {%modelname}.equation PE[c,s]*E[c,s] = PED[c]*ED[c,s] + PEM[c]*EM[c,s] + TCO_VAL[c,s] if E[c,s] <> 0 where s in {%list_sec}, c in {%list_com_E}

  'For %sec {%list_sec}
    'For %com {%list_com_E}
      'if @elem(E_{%com}_{%sec},%baseyear) <> 0 then
        '{%modelname}.append PE_{%com}_{%sec}*E_{%com}_{%sec}=PEM_{%com}*EM_{%com}_{%sec}+PED_{%com}*ED_{%com}_{%sec}+ TCO_VAL_{%com}_{%sec}
      'endif
    'next
 'next

    'equation 5.16
    {%modelname}.equation PIA[c,s]*IA[c,s] = PIAD[c]*IAD[c,s] + PIAM[c]*IAM[c,s] if IA[c,s] <> 0 where s in {%list_sec}, c in {%list_com}


    'For %sec {%list_sec}
    'For %com  {%list_com}
      'if @elem(IA_{%com}_{%sec},%baseyear) <> 0 then
        '{%modelname}.append PIA_{%com}_{%sec}*IA_{%com}_{%sec}=PIAM_{%com}*IAM_{%com}_{%sec}+PIAD_{%com}*IAD_{%com}_{%sec}
      'endif
    'next
    'next

    'equation 5.17
    {%modelname}.equation PIA[s]*IA[s] = sum(PIA[c,s]*IA[c,s]  if IA[c,s] <> 0 where c in {%list_com}) if IA[s] <> 0 where s in {%list_sec}

  'For %sec {%list_sec}
    'if @elem(IA_{%sec},%baseyear) <> 0 then
      '%equation = "PIA_"+%sec+"*IA_"+%sec+"=0"
      'For %com {%list_com}
        'if @elem(IA_{%com}_{%sec},%baseyear) <> 0 then
          '%equation = %equation +"+ PIA_"+%com+"_"+%sec+"*IA_"+%com+"_"+%sec
        'endif
      'next
      '{%modelname}.append {%equation}
    'endif
  'next


    'equation 5.18
    {%modelname}.equation PYQS[c]*YQS[c] = PYQ[c]*YQ[c]*(1+TothtD[c]) + YQ[c]*(TenertD[c]+Tsub[c]) + PMTD[c]*MTD[c] + PMCD[c]*MCD[c] if YQS[c] > 0 where c in {%list_com} \ {%list_trsp} 19 {%list_com_E_CO2}

    {%modelname}.equation PYQS[c]*YQS[c] = PYQ[c]*YQ[c]*(1+TothtD[c]) + YQ[c]*(TenertD[c]+Tsub[c]) if YQS[c] > 0 where c in {%list_trsp} 19

    {%modelname}.equation PYQS[c]*YQS[c] = PYQ[c]*YQ[c]*(1+TothtD[c]) + YQ[c]*(TenertD[c]+Tsub[c]) + PMTD[c]*MTD[c] + PMCD[c]*MCD[c] if YQS[c] > 0 where c in {%list_com_E_CO2}

    'equation 5.19
    {%modelname}.equation d(log(YQS[c])) = d(log(YQ[c])) if YQS[c] > 0 where c in {%list_com}


    'equation 5.20
    {%modelname}.equation PMS[c]*MS[c] = PM[c]*M[c]*(1+TothtM[c]) + M[c]*TenertM[c] + PMTM[c]*MTM[c] + PMCM[c]*MCM[c] if MS[c] > 0 where c in {%list_com} \ {%list_trsp} 19 {%list_com_E_CO2}

    {%modelname}.equation PMS[c]*MS[c] = PM[c]*M[c]*(1+TothtM[c]) + M[c]*TenertM[c] if MS[c] > 0 where c in {%list_trsp} 19

    {%modelname}.equation PMS[c]*MS[c] = PM[c]*M[c]*(1+TothtM[c]) + M[c]*(TenertM[c]) + PMTM[c]*MTM[c] + PMCM[c]*MCM[c] if MS[c] > 0 where c in {%list_com_E_CO2}

    'equation 5.21
    {%modelname}.equation d(log(MS[c])) = d(log(M[c])) if MS[c] > 0 where c in {%list_com}

    'equation 5.22
    {%modelname}.equation PMATD[c] = PYQS[c] * (1+TvatDoth[c]) / (1+@elem(TvatDoth[c],{%baseyear})) if CID[c] <> 0 where c in {%list_com_MAT}

  'For %com {%list_com_MAT}
    'if @elem(CID_{%com},%baseyear) <> 0 then        '(If condition on CID_{%com})
      '{%modelname}.append PMATD_{%com}=PYQS_{%com}*(1+TvatDoth_{%com})/(1+@elem(TvatDoth_{%com},%baseyear))
    'endif
  'next

    'equation 5.23
    {%modelname}.equation PED[c] = PYQS[c]*(1+TvatDoth[c]) / (1+@elem(TvatDoth[c],{%baseyear})) if CID[c] <> 0 where c in {%list_com_E}

  'For %com {%list_com_E}
    'if @elem(CID_{%com},%baseyear) <> 0 then    '(If condition on CID_{%com})
      '{%modelname}.append PED_{%com}=PYQS_{%com}*(1+TvatDoth_{%com})/(1+@elem(TvatDoth_{%com},%baseyear))
    'endif
  'next



    'equation 5.24
    {%modelname}.equation PMATM[c] = PMS[c]*(1+TvatMoth[c]) / (1+@elem(TvatMoth[c],{%baseyear})) if CIM[c] <> 0 where c in {%list_com_MAT}

  'For %com {%list_com_MAT}
    'if @elem(CIM_{%com},%baseyear) <> 0 then    '(If condition on CIM_{%com})
      '{%modelname}.append PMATM_{%com}=PMS_{%com}*(1+TvatMoth_{%com})/(1+@elem(TvatMoth_{%com},%baseyear))
    'endif
  'next


    'equation 5.25
    {%modelname}.equation PEM[c] = PMS[c]*(1+TvatMoth[c]) / (1+@elem(TvatMoth[c],{%baseyear})) if CIM[c] <> 0 where c in {%list_com_E}

  'For %com {%list_com_E}
    'if @elem(CIM_{%com},%baseyear) <> 0 then   '(If condition on CIM_{%com})
      '{%modelname}.append PEM_{%com}=PMS_{%com}*(1+TvatMoth_{%com})/(1+@elem(TvatMoth_{%com},%baseyear))
    'endif
  'next

    'equation 5.26
    {%modelname}.equation PCHD[c] = PYQS[c]*(1+TvatD[c]) / (1+@elem(TvatD[c],{%baseyear})) if CHD[c] <> 0 where c in {%list_com} \ {%list_com_E_CO2}

    {%modelname}.equation PCHD[c] = PYQS[c]*(1+TvatD[c]) / (1+@elem(TvatD[c],{%baseyear})) + Ttco*EMS_HH[c]/EXP[c] if CHD[c] <> 0 where c in {%list_com_E_CO2}

  'For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 23
    'if @elem(CHD_{%com},%baseyear) <> 0 then
      '{%modelname}.append PCHD_{%com}=PYQS_{%com}*(1+TvatD_{%com})/(1+@elem(TvatD_{%com},%baseyear))
    'endif
  'next
  'For %com {%list_com_E_CO2}
    'if @elem(CHD_{%com},%baseyear) <> 0 then
      '{%modelname}.append PCHD_{%com}=PYQS_{%com}*(1+TvatD_{%com})/(1+@elem(TvatD_{%com},%baseyear))+Ttco*EMS_HH_{%com}/EXP_{%com}
    'endif
  'next

    'equation 5.27
    {%modelname}.equation PCHM[c]=PMS[c]*(1+TvatM[c]) / (1+@elem(TvatM[c],{%baseyear})) if CHM[c] <> 0 where c in  {%list_com} \ {%list_com_E_CO2}

    {%modelname}.equation PCHM[c]=PMS[c]*(1+TvatM[c]) / (1+@elem(TvatM[c],{%baseyear})) +Ttco*EMS_HH[c]/EXP[c] if CHM[c] <> 0 where c in {%list_com_E_CO2}

'For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 23
    'if @elem(CHM_{%com},%baseyear) <> 0 then
      '{%modelname}.append PCHM_{%com}=PMS_{%com}*(1+TvatM_{%com})/(1+@elem(TvatM_{%com},%baseyear))
    'endif
  'next
'For %com {%list_com_E_CO2}
    'if @elem(CHM_{%com},%baseyear) <> 0 then
      '{%modelname}.append PCHM_{%com}=PMS_{%com}*(1+TvatM_{%com})/(1+@elem(TvatM_{%com},%baseyear))+Ttco*EMS_HH_{%com}/EXP_{%com}
    'endif
'next


    'equation 5.28
    {%modelname}.equation PGD[c] = PYQS[c]*((1+TvatDoth[c]) / (1+@elem(TvatDoth[c],{%baseyear}))) if GD[c] <> 0 where c in {%list_com}

  'For %com {%list_com}
    'if @elem(GD_{%com},%baseyear) <> 0 then
      '{%modelname}.append PGD_{%com}=PYQS_{%com}*((1+TvatDoth_{%com})/(1+@elem(TvatDoth_{%com},{%baseyear})))
    'endif
  'next

    'equation 5.29
    {%modelname}.equation PGM[c] = PMS[c]*((1+TvatMoth[c]) / (1+@elem(TvatMoth[c],{%baseyear}))) if GM[c] <> 0 where c in {%list_com}

  'For %com {%list_com}
    'if @elem(GM_{%com},%baseyear) <> 0 then
      '{%modelname}.append PGM_{%com}=PMS_{%com}*((1+TvatMoth_{%com})/(1+@elem(TvatMoth_{%com},%baseyear)))
    'endif
  'next

    'equation 5.30
    {%modelname}.equation PIAD[c] = PYQS[c]*((1+TvatDoth[c]) / (1+@elem(TvatDoth[c],{%baseyear}))) if ID[c] <> 0 where c in {%list_com}

      'For %com {%list_com}
    'if @elem(ID_{%com},%baseyear) <> 0 then   '(If condition on ID_{%com})
      '{%modelname}.append PIAD_{%com}=PYQS_{%com}*((1+TvatDoth_{%com})/(1+@elem(TvatDoth_{%com},%baseyear)))
    'endif
  'next

    'equation 5.31
    {%modelname}.equation PIAM[c] = PMS[c]*((1+TvatMoth[c]) / (1+@elem(TvatMoth[c],{%baseyear}))) if IM[c] <> 0 where c in {%list_com}

  For %com {%list_com}
    if @elem(IM_{%com},%baseyear) <> 0 then    '(If condition on IM_{%com})
      '{%modelname}.append PIAM_{%com}=PMS_{%com}*((1+TvatMoth_{%com})/(1+@elem(TvatMoth_{%com},%baseyear)))
    endif
  next

    'equation 5.32
    {%modelname}.equation PXD[c] = PYQS[c] if XD[c] <> 0 where c in {%list_com}

    'equation 5.33
    {%modelname}.equation PXM[c] = PMS[c] if XM[c] <> 0 where c in {%list_com}

    'equation 5.34
    {%modelname}.equation PDSD[c] = PYQS[c] if DSD[c] <> 0 where c in {%list_com}

    'equation 5.35
    {%modelname}.equation PDSM[c] = PMS[c] if DSM[c] <> 0 where c in {%list_com}

    'equation 5.36
    {%modelname}.equation PMTD[m, c] = YQ[m] / (YQ[m]+M[m])*PYQS[m] + M[m]/(YQ[m]+M[m])*PMS[m] if MTD[m, c] <> 0 where m in {%list_trsp}, c in {%list_com} \ {%list_trsp} 19

    'equation 5.37
    {%modelname}.equation PMTM[m, c] = PMTD[m, c] if MTM[m, c] <> 0 where m in {%list_trsp}, c in {%list_com} \ {%list_trsp} 19

    'equation 5.38
    {%modelname}.equation PMCD[c] = YQ_19/(YQ_19 + M_19)*PYQS_19 + M_19/(YQ_19 + M_19)*PMS_19 if MCD[c] <> 0 where c in {%list_com} \ {%list_trsp} 19

  'For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 20 21 22 23 24
    'if @elem(MCD_{%com},%baseyear) <> 0 then
      '{%modelname}.append PMCD_{%com} = YQ_19/(YQ_19+M_19)*PYQS_19 + M_19/(YQ_19+M_19)*PMS_19
    'endif
  'next

    'equation 5.39
    {%modelname}.equation PMCM[c] = PMCD[c] if MCM[c] <> 0 where c in {%list_com} \ {%list_trsp} 19

    'equation 5.40
    {%modelname}.equation PM[c] = TC*PWD[c] if M[c] <> 0 where c in {%list_com}

    {%modelname}.equation PM[s] = PM_22 where s in 2201 2202

    {%modelname}.equation PM[s] = PM_23 where s in 2301 2302 2303 2304 2305 2306 2307 2308

    {%modelname}.equation PM[s] = PM_24 where s in 2401 2402 2403 2404 2405 2406


  ' 'equation 5.40
  ' For %com  {%list_com}
  '   if @elem(M_{%com},%baseyear) <> 0 then
  '     {%modelname}.append PM_{%com}=PWD_{%com}*TC
  '   endif
  ' next

  ' 'PROBLEME! Eq. ??? Il serait bien de distinguer les sous secteurs si on veux des prix différents pour le biocarburant
  ' For %sec 2201 2202
  '   {%modelname}.append PM_{%sec}=PM_22
  ' next

  ' For %sec 2301 2302 2303 2304 2305 2306 2307 2308
  '   {%modelname}.append PM_{%sec}=PM_23
  ' next

  ' For %sec 2401 2402 2403 2404 2405 2406
  '   {%modelname}.append PM_{%sec}=PM_24
  ' next

    'equation 5.41
    {%modelname}.equation d(log(W_S_n[s])) = RHO($s,1) + RHO($s,2)*d(log(P_e)) + RHO($s,3)*d(log(PROG_L[s])) - RHO($s,4)*d(log(PM[s])-log(PY[s])) - RHO($s,5)*UnR_TOT - RHO($s,6)*d(UnR_TOT) + RHO($s,7)*d(log(L_S[s]) - log(L))+DNAIRU*RHO($s,5) if W_S_n[s] <> 0 where s in {%list_sec}

    {%modelname}.equation d(log(W_SE[s])) = d(log(W_S[s])) if W_SE[s] <> 0 where s in {%list_sec}


  '-------------------------TAYLOR RULE and interest rates ----------------------'
    'equation 5.42
    {%modelname}.equation d(R_n) = d(R_DIR)

    {%modelname}.equation d(R_G) = d(R)

    {%modelname}.equation d(R_DIR) = STEADYSTATE(37,1)*(d(infl_ZE)-d(infl_ZE_target)) - STEADYSTATE(38,1)*(d(UnR_ZE)-d(UnR_ZE_target)) '"Original" rule

    {%modelname}.equation infl_FR = d(P)/P(-1)

    {%modelname}.equation infl_ZE = STEADYSTATE(42,1)*infl_FR + (1 - STEADYSTATE(42,1))*infl_HFR

    {%modelname}.equation infl_HFR = STEADYSTATE(41,1)*infl_FR + (1 - STEADYSTATE(41,1))*infl_HFR(-1)

    {%modelname}.equation P = PCH

    {%modelname}.equation UnR_FR = UnR_TOT

    {%modelname}.equation UnR_ZE = STEADYSTATE(42,1)*UnR_FR + (1 - STEADYSTATE(42,1))*UnR_HFR

    {%modelname}.equation UnR_HFR = STEADYSTATE(41,1)*UnR_FR + (1 - STEADYSTATE(41,1))*UnR_HFR(-1)

    {%modelname}.equation d(R[s]) = d(R) where s in {%list_sec}

    {%modelname}.equation d(log(P_e)) = ADJUST(68,2)*d(log(P_e(-1))) + (1-ADJUST(68,2))*d(log(P(-1)))


  '**************************************************************************************************************************************
  '**************************************************************************************************************************************
  '********************************************* END BLOCK 5 : PRICES ************************************************
  '**************************************************************************************************************************************

endsub