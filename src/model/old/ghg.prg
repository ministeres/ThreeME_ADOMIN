'***************************************************************************************************************************************
'******************************************** BLOCK 6 : GREEN HOUSE GASES EMISSIONS ****************************************************
'**************************************************************************************************************************************
subroutine BLOCK_GHG
 'equation 6.1
 {%modelname}.equation EMS_CH[s] = EMS_21[s], s in {%list_sec}
 {%modelname}.equation EMS_PT[s] = EMS_22[s], s in {%list_sec}
 {%modelname}.equation EMS_GZ[s] = EMS_24[s], s in {%list_sec}

 {%modelname}.equation d(log(EMS_21[s])) = d(log(E_21[s])) if EMS_21[s] <> 0, s in {%list_sec}
 {%modelname}.equation d(log(EMS_22[s])) = d(log(E_22[s]) + log(PhiY_22_2201)) if EMS_22[s] <> 0, s in {%list_sec}
 {%modelname}.equation d(log(EMS_24[s])) = d(log(E_24[s]) + log(PhiY_24_2401)) if EMS_24[s] <> 0, s in {%list_sec}

'   'equation 6.1    ' EQUATION DE PASSAGE A METTRE A LA FIN SANS NUMEROTE'
'   For %sec {%list_sec}
'     {%modelname}.append EMS_CH_{%sec}=EMS_21_{%sec}
'     {%modelname}.append EMS_PT_{%sec}=EMS_22_{%sec}
'     {%modelname}.append EMS_GZ_{%sec}=EMS_24_{%sec}
'   next


'   For %sec {%list_sec}
'     if @elem(EMS_21_{%sec},%baseyear) <> 0 then
'
'       {%modelname}.append d(log(EMS_21_{%sec}))=d(log(E_21_{%sec}))
'
'     endif
'   next
'
'   For %sec {%list_sec}
'     if @elem(EMS_22_{%sec},%baseyear) <> 0 then
'
'       {%modelname}.append d(log(EMS_22_{%sec}))=d(log(E_22_{%sec})+log(PhiY_22_2201))
'
'     endif
'   next
'
'   For %sec {%list_sec}
'     if @elem(EMS_24_{%sec},%baseyear) <> 0 then
'
'       {%modelname}.append d(log(EMS_24_{%sec}))=d(log(E_24_{%sec})+log(PhiY_24_2401))
'
'     endif
'   next

 'equation 6.2
 {%modelname}.equation EMS[s] = sum(EMS_|V|[s] if EMS_|V|[s] <> 0, V in GZ CH PT), s in {%list_sec}

'   'equation 6.2
'   For %sec {%list_sec}
'     %equation = "EMS_"+%sec+"=0"
'     For %ems  GZ CH PT
'       if @elem(EMS_{%ems}_{%sec},%baseyear) <> 0 then
'         %equation = %equation +"+ EMS_"+%ems+"_"+%sec
'       endif
'     next
'     {%modelname}.append {%equation}
'   next

  ' PROBLEME! Agreger les secteurs energétiques en appellant la subroutine aggregate_energy
  {%modelname}.append IC_21 = EMS_CH / (QD_21 + M_21 - X_21)
  {%modelname}.append IC_22 = (EMS_PT / (QD_22 + M_22 - X_22)) * PhiY_22_2201
  {%modelname}.append IC_24 = (EMS_GZ / (QD_24 + M_24 - X_24)) * PhiY_24_2401

  '--------------------------------------***CO2 SECTORS EMISSIONS***-----------------------------------------------------------------------------------------
 'equation 6.3
 {%modelname}.equation EMS_S = sum(EMS[s] if EMS[s] <> 0, s in {%list_sec})

'   'equation 6.3
'   %equation = "EMS_S=0"
'   For %sec {%list_sec}
'     if @elem(EMS_{%sec},%baseyear) <> 0 then
'       %equation = %equation +"+ EMS_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

  '-------------------------------DECARBONATION------------------------------
 'equation 6.4
 {%modelname}.equation d(log(EMS_DC[c])) = d(log(MAT[c])) if EMS_DC[c] <> 0, c in {%list_com}

 {%modelname}.equation EMS_DC = sum(EMS_DC[c] if EMS_DC[c] <> 0, c in {%list_com})

'   'equation 6.4
'   For %sec {%list_com}
'     if @elem(EMS_DC_{%sec},%baseyear) <> 0 then
'       {%modelname}.append d(log(EMS_DC_{%sec}))=d(log(MAT_{%sec}))
'     endif
'   next
'
'
'   %equation = "EMS_DC= 0"
'   For %sec {%list_com}
'     if @elem(EMS_DC_{%sec},%baseyear) <> 0 then
'       %equation = %equation + "+EMS_DC_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}



  '-------------------------EMISSIONS by energy source-----------------------
 'equation 6.5
 {%modelname}.equation EMS_|V| = sum(EMS_|V|[s] if EMS_|V|[s] <> 0, s in {%list_sec}), V in GZ CH PT

'   'equation 6.5
'   For %ems GZ CH PT
'     %equation = "EMS_"+%ems+"=0"
'     For %sec {%list_sec}
'       if @elem(EMS_{%ems}_{%sec},%baseyear) <> 0 then
'         %equation = %equation +"+ EMS_"+%ems+"_"+%sec
'       endif
'     next
'     {%modelname}.append {%equation}
'   next

' ***********************************************************************************************************************
' *******************************************CO2 HOUSEHOLDS EMISSIONS HYBRID*********************************************
' ***********************************************************************************************************************

  if %hybrid_household="yes" then 'Load if "yes" in the main
    Call BLOCK_GHG_Hybrid

' ***********************************************************************************************************************
' *******************************************END CO2 HOUSEHOLDS EMISSIONS HYBRID*****************************************
' ***********************************************************************************************************************

' ***********************************************************************************************************************




  else


' ***********************************************************************************************************************
' *******************************************CO2 HOUSEHOLDS EMISSIONS****************************************************

' ***********************************************************************************************************************
  ' equation 6.6
  {%modelname}.equation d(log(EMS_HH[ce2, h])) = d(log(EXP[ce2, h])), ce2 in {%list_com_E_CO2}, h in {%list_household}
  {%modelname}.equation EMS_HH[ce2] = sum (EMS_HH[ce2, h], h in {%list_household}), ce2 in {%list_com_E_CO2}
  {%modelname}.equation EMS_HH = sum (EMS_HH[ce2], ce2 in {%list_com_E_CO2})

  {%modelname}.append EMS_TOT=EMS_S+EMS_DC+EMS_HH

'     For %hous {%list_household}
'       For %ener {%list_com_E_CO2}
'         {%modelname}.append d(log(EMS_HH_{%ener}_{%hous}))=d(log(EXP_{%ener}_{%hous}))
'       next
'     next
'
'     For %ener {%list_com_E_CO2}
'       %equation = "EMS_HH_"+%ener+"= 0"
'       For %hous {%list_household}
'         %equation = %equation + "+EMS_HH_"+%ener+"_"+%hous
'       next
'       {%modelname}.append {%equation}
'     next
'
'
'     %equation = "EMS_HH= 0"
'     For %ener {%list_com_E_CO2}
'       %equation = %equation + "+EMS_HH_"+%ener
'     next
'     {%modelname}.append {%equation}
'
'     {%modelname}.append EMS_TOT=EMS_S+EMS_DC+EMS_HH

' ***********************************************************************************************************************
' ********************************************END CO2 HOUSEHOLDS EMISSIONS****************************************************************************
' ***********************************************************************************************************************8

  endif

  '--------------------------------------------------------------------------------------------------------------------------------
  '------------------------------------***ENERGETIC PRODUCTION IN MTEP***----------------------------------------------------------
 'equation 6.7
 {%modelname}.equation d(log(Q_Mtep_H[ce]))=d(log(EXP[ce])), ce in {%list_com_E}

'   For %ene {%list_com_E}
'     {%modelname}.append d(log(Q_Mtep_H_{%ene}))=d(log(EXP_{%ene}))
'   next


  'gel 'For %sec 2201 2202
  'gel '{%modelname}.append d(log(phiY_22_{%sec}))=d(log(phi_Mtep_{%sec}))
  'gel 'next
  'gel '
  'gel 'For %sec 2301 2302 2303 2304 2305 2306 2307 2308
  'gel '{%modelname}.append d(log(phiY_23_{%sec}))=d(log(phi_Mtep_{%sec}))
  'gel 'next
  'gel '
  'gel 'For %sec 2401 2402 2403 2404 2405 2406
  'gel '{%modelname}.append d(log(phiY_24_{%sec}))=d(log(phi_Mtep_{%sec}))
  'gel 'next

 'equation 6.8
 {%modelname}.equation d(phiY_22[se]) = d(phi_Mtep[se]), se in {%list_subsec_22}
 {%modelname}.equation d(phiY_23[se]) = d(phi_Mtep[se]), se in {%list_subsec_23}
 {%modelname}.equation d(phiY_24[se]) = d(phi_Mtep[se]), se in {%list_subsec_24}

'   For %sec 2201 2202
'     {%modelname}.append d(phiY_22_{%sec})=d(phi_Mtep_{%sec})
'   next
'
'   For %sec 2301 2302 2303 2304 2305 2306 2307 2308
'     {%modelname}.append d(phiY_23_{%sec})=d(phi_Mtep_{%sec})
'   next
'
'   For %sec 2401 2402 2403 2404 2405 2406
'     {%modelname}.append d(phiY_24_{%sec})=d(phi_Mtep_{%sec})
'   next


 'equation 6.9
 {%modelname}.equation Q_Mtep_22 = sum(Q_Mtep[se] if Q_Mtep[se] <> 0, se in {%list_subsec_22})
 {%modelname}.equation Q_Mtep_23 = sum(Q_Mtep[se] if Q_Mtep[se] <> 0, se in {%list_subsec_23})
 {%modelname}.equation Q_Mtep_24 = sum(Q_Mtep[se] if Q_Mtep[se] <> 0, se in {%list_subsec_24})

 {%modelname}.equation Q_Mtep = sum(Q_Mtep[se] if Q_Mtep[se] <> 0, se in {%list_sec_E})

'   %equation = "Q_Mtep_22=0"
'   For %ene  2201 2202
'     if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'
'   %equation = "Q_Mtep_23=0"
'   For %ene 2301 2302 2303 2304 2305 2306 2307 2308
'     if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'
'   %equation = "Q_Mtep_24=0"
'   For %ene 2401 2402 2403 2404 2405 2406
'     if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'
'   %equation = "Q_Mtep=0"
'   For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}

  '-----------------------------Energetic Production in primary energy-----------------------------------
 ' equation 6.10
 {%modelname}.equation  Q_Mtep_EP[se] = Q_Mtep[se] * fac_conv_EP[se], se in {%list_sec_E}

 {%modelname}.equation Q_Mtep_EP_22 = sum(Q_Mtep_EP[se] if Q_Mtep_EP[se] <> 0, se in {%list_subsec_22})
 {%modelname}.equation Q_Mtep_EP_23 = sum(Q_Mtep_EP[se] if Q_Mtep_EP[se] <> 0, se in {%list_subsec_23})
 {%modelname}.equation Q_Mtep_EP_24 = sum(Q_Mtep_EP[se] if Q_Mtep_EP[se] <> 0, se in {%list_subsec_24})

 {%modelname}.equation Q_Mtep_EP = sum(Q_Mtep_EP[se] if Q_Mtep_EP[se] <> 0, se in {%list_sec_E})

'   For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     '{%modelname}.append d(log(Q_Mtep_EP_{%ene}))=(Q_Mtep_{%ene}>0)*d(log(Q_Mtep_{%ene}))+(Q_Mtep_{%ene}<=0)*0.000001
'     {%modelname}.append Q_Mtep_EP_{%ene}=Q_Mtep_{%ene}*fac_conv_EP_{%ene}
'
'   next
'
'   %equation = "Q_Mtep_EP_22=0"
'   For %ene 2201 2202
'     if @elem(Q_Mtep_EP_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_EP_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   %equation = "Q_Mtep_EP_23=0"
'   For %ene 2301 2302 2303 2304 2305 2306 2307 2308
'     if @elem(Q_Mtep_EP_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_EP_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   %equation = "Q_Mtep_EP_24=0"
'   For %ene 2401 2402 2403 2404 2405 2406
'     if @elem(Q_Mtep_EP_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_EP_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   %equation = "Q_Mtep_EP=0"
'   For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     if @elem(Q_Mtep_EP_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_EP_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}




  '-------------------------Energetic consumption from the activity sectors---------------------------'
 'equation 6.11
 {%modelname}.equation E_SEC|O|[ce] = sum(E|O|[ce, cm], cm in {%list_com_MAT}), O in D M, ce in {%list_com_E}

'   For %com {%list_com_E}
'     %equation = "E_SECD_"+%com+"=0"
'     For %sec {%list_com_MAT}
'       %equation = %equation +"+ED_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next
'
'   For %com {%list_com_E}
'     %equation = "E_SECM_"+%com+"=0"
'     For %sec {%list_com_MAT}
'       %equation = %equation +"+EM_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next

 'equation 6.12
 {%modelname}.equation E_SEC[ce] = sum(E_SEC|O|[ce], O in D M), ce in {%list_com_E}

'   For %com {%list_com_E}
'     {%modelname}.append E_SEC_{%com}=E_SECD_{%com}+E_SECM_{%com}
'   next

 ' equation 6.13
 {%modelname}.equation d(log(Q_Mtep_SEC[ce])) = d(log(E_SEC[ce])) if E_SEC[ce] <> 0, ce in {%list_com_E}

 {%modelname}.equation Q_Mtep_SEC = sum(Q_Mtep_SEC[ce] if Q_Mtep_SEC[ce] <> 0, ce in {%list_com_E})

'   For %ene {%list_com_E}
'     if @elem(E_SEC_{%ene},%baseyear) <> 0 then
'       {%modelname}.append d(log(Q_Mtep_SEC_{%ene}))=d(log(E_SEC_{%ene}))
'     endif
'   next

'   %equation = "Q_Mtep_SEC=0"
'   For %ene {%list_com_E}
'     if @elem(Q_Mtep_SEC_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_SEC_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}

 'equation 6.14
 {%modelname}.equation E_ESEC|O|[ce] = sum(E|O|[ce, se], se in {%list_sec_E}), O in D M, ce in {%list_com_E}
 {%modelname}.equation E_ESEC[ce] = sum(E_ESEC|O|[ce], O in D M), ce in {%list_com_E}

 {%modelname}.equation d(log(Q_Mtep_ESEC[ce])) = d(log(E_ESEC[ce])) if E_ESEC[ce] <> 0, ce in {%list_com_E}
 {%modelname}.equation Q_Mtep_ESEC = sum(Q_Mtep_ESEC[ce] if Q_Mtep_ESEC[ce] <> 0, ce in {%list_com_E})

'   For %com {%list_com_E}
'     %equation = "E_ESECD_"+%com+"=0"
'     For %sec {%list_sec_E}
'       %equation = %equation +"+ED_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next
'
'   For %com {%list_com_E}
'     %equation = "E_ESECM_"+%com+"=0"
'     For %sec {%list_sec_E}
'       %equation = %equation +"+EM_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next

'   For %com {%list_com_E}
'     {%modelname}.append E_ESEC_{%com}=E_ESECD_{%com}+E_ESECM_{%com}
'   next

'   For %ene {%list_com_E}
'     if @elem(E_ESEC_{%ene},%baseyear) <> 0 then
'       {%modelname}.append d(log(Q_Mtep_ESEC_{%ene}))=d(log(E_ESEC_{%ene}))
'     endif
'   next
'
'   %equation = "Q_Mtep_ESEC=0"
'   For %ene {%list_com_E}
'     if @elem(Q_Mtep_ESEC_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_ESEC_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}

  '-----------------------------Energetic finale consumption-----------------------------'
 'equation 6.15
 {%modelname}.equation Q_Mtep_EF[ce] = Q_Mtep[ce]-Q_Mtep_ESEC[ce], ce in {%list_com_E}
 {%modelname}.equation Q_Mtep_EF = sum(Q_Mtep_EF[ce] if Q_Mtep_EF[ce] <> 0, ce in {%list_com_E})

'   For %ene {%list_com_E}
'     {%modelname}.append Q_Mtep_EF_{%ene}= Q_Mtep_{%ene}-Q_Mtep_ESEC_{%ene}
'   next
'
'   %equation = "Q_Mtep_EF=0"
'   For %ene {%list_com_E}
'     if @elem(Q_Mtep_EF_{%ene},%baseyear) <> 0 then
'       %equation = %equation +"+ Q_Mtep_EF_"+%ene
'     endif
'   next
'   {%modelname}.append {%equation}

  '----------------------------Energetic production by energetic sector------------------------------------
  'if %hybrid_household="yes" then

 'equation 6.16
 {%modelname}.append Q_Mtep_21=Q_Mtep_SEC_21+Q_Mtep_H_21+Q_Mtep_ESEC_21

 {%modelname}.equation Q_Mtep[se] = phi_Mtep[se] * (Q_Mtep_H_22 + Q_Mtep_SEC_22 + Q_Mtep_ESEC_22), se in {%list_subsec_22}
 {%modelname}.equation Q_Mtep[se] = phi_Mtep[se] * (Q_Mtep_H_23 + Q_Mtep_SEC_23 + Q_Mtep_ESEC_23), se in {%list_subsec_23}
 {%modelname}.equation Q_Mtep[se] = phi_Mtep[se] * (Q_Mtep_H_24 + Q_Mtep_SEC_24 + Q_Mtep_ESEC_24), se in {%list_subsec_24}

'   For %subsec 2201 2202
'     {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_22+Q_Mtep_SEC_22+Q_Mtep_ESEC_22)
'   next
'
'   For %subsec 2301 2302 2303 2304 2305 2306 2307 2308
'     {%modelname}.append  Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_23+Q_Mtep_SEC_23+Q_Mtep_ESEC_23)
'   next
'
'   For %subsec 2401 2402 2403 2404 2405 2406
'     {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_24+Q_Mtep_SEC_24+Q_Mtep_ESEC_24)
'   next



  '---------------------------***CARBON TAX***---------------------------------
 'equation 6.17
 {%modelname}.equation TCO_VAL[ce2, s] = Ttco * EMS[ce2, s] * (1 - EXO[ce2, s]), ce2 in {%list_com_E_CO2}, s in {%list_sec}
 {%modelname}.equation TCO_VAL_sec[ce2] = sum(TCO_VAL[ce2, s], s in {%list_sec}), ce2 in {%list_com_E_CO2}
 {%modelname}.equation TCO_VAL[s] = sum(TCO_VAL[ce2, s], ce2 in {%list_com_E_CO2}), s in {%list_sec}
 {%modelname}.equation TCO_VAL = sum(TCO_VAL[s], s in {%list_sec})


'   For %sec {%list_sec}
'     For %com {%list_com_E_CO2}
'       {%modelname}.append  TCO_VAL_{%com}_{%sec} = Ttco*EMS_{%com}_{%sec}*(1-EXO_{%com}_{%sec})
'     next
'   next
'
'   For %com {%list_com_E_CO2}
'     %equation = "TCO_VAL_sec_"+%com+"=0"
'     For %sec {%list_sec}
'       %equation = %equation +"+ TCO_VAL_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next
'
'   For %sec {%list_sec}
'     %equation = "TCO_VAL_"+%sec+"=0"
'     For %com {%list_com_E_CO2}
'       %equation = %equation +"+ TCO_VAL_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next
'
'   %equation = "TCO_VAL=0"
'   For %sec {%list_sec}
'     %equation = %equation +"+ TCO_VAL_"+%sec
'   next
'   {%modelname}.append {%equation}

 'equation 6.18
 {%modelname}.equation TCO_VAL|O|[ce, s] = TCO_VAL[ce, s] * E|O|[ce, s]/E[ce, s] if E[ce, s] <> 0, O in D M, ce in {%list_com_E}, s in {%list_sec}
 {%modelname}.equation TCO_VAL|O|_sec[ce2] =  sum(TCO_VAL|O|[ce2, s] , s in {%list_sec}), O in D M, ce2 in {%list_com_E_CO2}

'   For %sec {%list_sec}
'     For %com {%list_com_E}
'       if @elem(E_{%com}_{%sec},%baseyear) <> 0 then
'         {%modelname}.append TCO_VALD_{%com}_{%sec} = TCO_VAL_{%com}_{%sec}*ED_{%com}_{%sec}/E_{%com}_{%sec}
'         {%modelname}.append TCO_VALM_{%com}_{%sec} = TCO_VAL_{%com}_{%sec}*EM_{%com}_{%sec}/E_{%com}_{%sec}
'         ''        {%modelname}.append Verif_TCO_VAL_{%com}_{%sec} = TCO_VAL_{%com}_{%sec} - (TCO_VALD_{%com}_{%sec}+TCO_VALM_{%com}_{%sec})
'       endif
'     next
'   next

'   For %com {%list_com_E_CO2}
'     %equation = "TCO_VALM_sec_"+%com+"=0"
'     For %sec {%list_sec}
'       %equation = %equation +"+ TCO_VALM_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next
'
'   For %com {%list_com_E_CO2}
'     %equation = "TCO_VALD_sec_"+%com+"=0"
'     For %sec {%list_sec}
'       %equation = %equation +"+ TCO_VALD_"+%com+"_"+%sec
'     next
'     {%modelname}.append {%equation}
'   next

 'equation 6.19
 {%modelname}.equation TCO_HH_VAL[ce2, h] = Ttco * EMS_HH[ce2, h], ce2 in {%list_com_E_CO2}, h in {%list_household}
 {%modelname}.equation TCO_HH_VAL_com[ce2] = sum(TCO_HH_VAL[ce2, h], h in {%list_household}), ce2 in {%list_com_E_CO2}
 {%modelname}.equation TCO_HH_VAL[h] = sum(TCO_HH_VAL[ce2, h], ce2 in {%list_com_E_CO2}), h in {%list_household}
 {%modelname}.equation TCO_HH_VAL = sum(TCO_HH_VAL[h],h in {%list_household})

'   For %hous {%list_household}
'     For %com {%list_com_E_CO2}
'       {%modelname}.append  TCO_HH_VAL_{%com}_{%hous} = Ttco*EMS_HH_{%com}_{%hous}
'     next
'   next


'   For %com {%list_com_E_CO2}
'     %equation = "TCO_HH_VAL_com_"+%com+"=0"
'     For %hous {%list_household}
'       %equation = %equation +"+ TCO_HH_VAL_"+%com+"_"+%hous
'     next
'     {%modelname}.append {%equation}
'   next
'
'
'   For %hous {%list_household}
'     %equation = "TCO_HH_VAL_"+%hous+"=0"
'     For %com {%list_com_E_CO2}
'       %equation = %equation +"+ TCO_HH_VAL_"+%com+"_"+%hous
'     next
'     {%modelname}.append {%equation}
'   next
'
'   %equation = "TCO_HH_VAL=0"
'   For %hous {%list_household}
'     %equation = %equation +"+ TCO_HH_VAL_"+%hous
'   next
'   {%modelname}.append {%equation}


 'equation 6.20
  {%modelname}.append Rec_TCO_VAL=Ttco*(EMS_HH+EMS_S-EMS_21_10-EMS_22_09-EMS_24_08-EMS_24_07)
  {%modelname}.append Rec_TCO=REC_TCO_VAL/PGDP
  {%modelname}.append CO_VAL=1000000*(REC_TCO/(EMS_TOT*PGDP))
  'series CO_VAL=1000000*(REC_TCO+CIDD+Bonus+Bonus_elec+CSPE)/(EMS_TOT*PGDP)) 'Equation telle qu'elle est dans la version V1'

  {%modelname}.append CO2_price_signal= (ENERT_21+OTHT_21+ENERT_22+ENERT_24+Rec_TCO)*1000000/EMS_TOT
  {%modelname}.append EN_price_signal = (ENERT_21 +OTHT_21+ENERT_22+OTHTD_23+ENERT_24+Rec_TCO)/Q_Mtep_EP

  '' ***********ENDOGENOUS EMPLOYER SOCIAL CONTRIBUTION RATE WHEN CARBON TAX
 'equation 6.21
 {%modelname}.equation Rec_TCO_ETS = sum(Ttco * EMS[s], s in {%list_sec}\01 02 03 11 12 13 14 15 16 17 19 20)
 {%modelname}.equation WAGES_ETS = sum(W_S[s] * L_S[s], s in {%list_sec}\01 02 03 11 12 13 14 15 16 17 19 20)
 {%modelname}.equation Rec_TCO_NETS = sum(tTCO * EMS[s], s in {%list_sec}\04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406)

 {%modelname}.append WAGES_NETS = W_S * L_S - WAGES_ETS

'   %equation = "Rec_TCO_ETS ="
'   For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     %equation = %equation + "+ Ttco*EMS_"+%sec
'   next
'   {%modelname}.append {%equation}
'
'   %equation = "WAGES_ETS ="
'   For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     %equation = %equation + "+ W_S_"+%sec+"*L_S_"+%sec
'   next
'   {%modelname}.append {%equation}
'
'
'   %equation = "Rec_TCO_NETS ="
'   For %sec 01 02 03 11 12 13 14 15 16 17 19 20
'     %equation = %equation + "+ tTCO*EMS_"+%sec
'   next
'   {%modelname}.append {%equation}
'
'   {%modelname}.append WAGES_NETS = W_S*L_S - WAGES_ETS




endsub




'**************************************************************************************************************************************
'******************************************** END BLOCK 6 : GREEN HOUSE GASES EMISSIONS ************************************************
'**************************************************************************************************************************************
