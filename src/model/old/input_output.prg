subroutine BLOCK_InputOutput
  '***************************************************************************************************************************************
  '******************************************** BLOCK 1 : INPUT - OUTPUT EQUILIBRIUM ****************************************************
  '**************************************************************************************************************************************
  '**************************************************************************************************************************************

  ' equations 1.1, 1.2, 1.3 & 1.4
  {%modelname}.equation(pv) Q|O|[c] = CI|O|[c] + CH|O|[c] + G|O|[c] + I|O|[c] + X|O|[c] + DS|O|[c] if Q|O|[c] <> 0 where O in D M, c in {%list_com}

  ' {%modelname}.equation(pv) QD[c] = CID[c] + CHD[c] + GD[c] + ID[c] + XD[c] + DSD[c] if QD[c] > 0, c in {%list_com}
  ' {%modelname}.equation(pv) QM[c] = CIM[c] + CHM[c] + GM[c] + IM[c] + XM[c] + DSM[c] if QM[c] > 0, c in {%list_com}

  ' For %sec {%list_com}
  '   if @elem(QD_{%sec},%baseyear) <> 0 then
  '     'equation 1.1
  '     {%modelname}.append   PQD_{%sec}*QD_{%sec} = PCID_{%sec}*CID_{%sec} + PCHD_{%sec}*CHD_{%sec} + PGD_{%sec}*GD_{%sec}+ PID_{%sec}*ID_{%sec}+PXD_{%sec}*XD_{%sec}+ PDSD_{%sec}*DSD_{%sec}

  '     'equation 1.2
  '     {%modelname}.append   QD_{%sec} = CID_{%sec} + CHD_{%sec} + GD_{%sec}+ ID_{%sec}+ XD_{%sec} + DSD_{%sec}
  '   endif

  '   if @elem(QM_{%sec},%baseyear) <> 0 then
  '     'equation 1.3
  '     {%modelname}.append   PQM_{%sec}*QM_{%sec} = PCIM_{%sec}*CIM_{%sec} + PCHM_{%sec}*CHM_{%sec} + PGM_{%sec}*GM_{%sec}+ PIM_{%sec}*IM_{%sec}+PXM_{%sec}*XM_{%sec}+ PDSM_{%sec}*DSM_{%sec}

  '     'equation 1.4
  '     {%modelname}.append   QM_{%sec} = CIM_{%sec} + CHM_{%sec} + GM_{%sec}+ IM_{%sec}+ XM_{%sec} + DSM_{%sec}
  '   endif
  ' next

  ' equations 1.5 & 1.6
  {%modelname}.equation(pv) |V|[c] = |V|D[c] + |V|M[c] if |V|[c] <> 0 where V in Q CH G I DS, c in {%list_com}

  ' 'equation 1.5 1.6
  ' For %varname Q CH G I DS
  '   For %com {%list_com}
  '     if @elem({%varname}_{%com},%baseyear) <> 0 then
  '       {%modelname}.append P{%varname}_{%com}*{%varname}_{%com} = P{%varname}D_{%com}*{%varname}D_{%com} + P{%varname}M_{%com}*{%varname}M_{%com}
  '       {%modelname}.append {%varname}_{%com} = {%varname}D_{%com} + {%varname}M_{%com}
  '     endif
  '   next
  ' next

  ' equation 1.7
  {%modelname}.equation PX[c] * X[c] = PXD[c] * XD[c] + PXM[c] * XM[c] if X[c] <> 0 where c in {%list_com}

  'Equation 1.7
  ' For %com {%list_com}
  '   if @elem(X_{%com},%baseyear) <> 0 then
  '     {%modelname}.append PX_{%com}*X_{%com} = PXD_{%com}*XD_{%com} + PXM_{%com}*XM_{%com}
  '   endif
  ' next

  ' equations 1.8, 1.9, 1.10 & 1.11
  {%modelname}.equation(pv) |V||O| = sum(|V||O|[c] if |V||O|[c] <> 0 where c in {%list_com}) where V in Q CH G I X DS CI MT MC, O in D M
  ' equations 1.12 & 1.13
  {%modelname}.equation(pv) |V| = |V|D + |V|M where V in Q CH G I X DS CI
  {%modelname}.equation |V| = |V|D + |V|M where V in MT MC

  ' For %varname Q CH G I X DS CI MT MC

  '   'equations 1.8
  '   %equation = "P"+%varname+"D*"+%varname+"D = 0"
  '   For %com {%list_com}
  '     if @elem({%varname}D_{%com},%baseyear) <> 0 then
  '       %equation = %equation+" + P"+%varname+"D_"+%com+"*"+%varname+"D_"+%com
  '     endif
  '   next
  '   {%modelname}.append {%equation}

  '   'equations 1.9
  '   %equation = %varname+"D = 0"
  '   For %com {%list_com}
  '     if @elem({%varname}D_{%com},%baseyear) <> 0 then
  '       %equation = %equation+" + "+%varname+"D_"+%com
  '     endif
  '   next
  '   {%modelname}.append {%equation}

  '   'equations 1.10
  '   %equation = "P"+%varname+"M*"+%varname+"M = 0"
  '   For %com {%list_com}
  '     if @elem({%varname}M_{%com},%baseyear) <> 0 then
  '       %equation = %equation+" + P"+%varname+"M_"+%com+"*"+%varname+"M_"+%com
  '     endif
  '   next
  '   {%modelname}.append {%equation}

  '   'equations 1.11
  '   %equation = %varname+"M = 0"
  '   For %com {%list_com}
  '     if @elem({%varname}M_{%com},%baseyear) <> 0 then
  '       %equation = %equation+" + "+%varname+"M_"+%com
  '     endif
  '   next
  '   {%modelname}.append {%equation}

  '   'equations 1.12
  '   if %varname<>"MT" and %varname<>"MC" then
  '     {%modelname}.append P{%varname}*{%varname} = P{%varname}D*{%varname}D + P{%varname}M*{%varname}M
  '   endif

  '   'equations 1.13
  '   {%modelname}.append {%varname} = {%varname}D + {%varname}M

  ' next

  ' **********************************

  ' equations 1.14, 1.15, 1.16 & 1.17
  {%modelname}.equation(pv) CI|O|[c] = sum(CI|O|[c, s] if CI|O|[c, s] <> 0 where s in {%list_com}) if CI|O|[c] > 0 where c in {%list_com}, O in D M

  ' For %com {%list_com}
  '   if @elem(CID_{%com},%baseyear) <> 0 then

  '     'equation 1.14
  '     %equation = "PCID_"+%com+"*CID_"+%com+"=0"
  '     For %sec {%list_com}
  '       if @elem(CID_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation + "+ PCID_"+%com+"_"+%sec+ "*CID_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '     'equation 1.15
  '     %equation = "CID_"+%com+"=0"
  '     For %sec {%list_com}
  '       if @elem(CID_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation + "+ CID_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}
  '   endif

  '   if @elem(CIM_{%com},%baseyear) <> 0 then

  '     'equation 1.16 :
  '     %equation = "PCIM_"+%com+"*CIM_"+%com+"=0"
  '     For %sec {%list_com}
  '       if @elem(CIM_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation + "+ PCIM_"+%com+"_"+%sec+ "*CIM_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '     'equation 1.17
  '     %equation = "CIM_"+%com+"=0"
  '     For %sec {%list_com}
  '       if @elem(CIM_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation + "+ CIM_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}
  '   endif
  ' next


  For %ci {%list_com}

    call aggregate_energy("CID_"+%ci)
    call priceindex_energy("PCID_"+%ci,"CID_"+%ci)

    call aggregate_energy("CIM_"+%ci)
    call priceindex_energy("PCIM_"+%ci,"CIM_"+%ci)
  next



  'equation 1.18 1.19
  ' For %sec {%list_sec}
  '   For %com {%list_com_MAT}
  '     if @elem(CID_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append PCID_{%com}_{%sec}=PMATD_{%com}
  '       {%modelname}.append CID_{%com}_{%sec}=MATD_{%com}_{%sec}
  '     endif
  '   next

  '   For %com {%list_com_E}
  '     if @elem(CID_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append PCID_{%com}_{%sec} = PED_{%com} + TCO_VALD_{%com}_{%sec}/ED_{%com}_{%sec}
  '       {%modelname}.append CID_{%com}_{%sec} = ED_{%com}_{%sec}
  '     endif
  '   next

  ' next

  ' equations 1.18, 1.19, 1.20 & 1.21
  {%modelname}.equation PCI|O|[c, s] = PMAT|O|[c] if CI|O|[c, s] > 0 where s in {%list_sec}, c in {%list_com_MAT}, O in D M
  {%modelname}.equation CI|O|[c, s] = MAT|O|[c, s] if CI|O|[c, s] > 0 where s in {%list_sec}, c in {%list_com_MAT}, O in D M

  {%modelname}.equation PCI|O|[c, s] = PE|O|[c] + TCO_VAL|O|[c, s] / E|O|[c, s] if CI|O|[c, s] > 0 where s in {%list_sec}, c in {%list_com_E}, O in D M
  {%modelname}.equation CI|O|[c, s] = E|O|[c, s] if CI|O|[c, s] > 0 where s in {%list_sec}, c in {%list_com_E}, O in D M

  'equation 1.20 1.21
  ' For %sec {%list_sec}
  '   For %com {%list_com_MAT}
  '     if @elem(CIM_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append PCIM_{%com}_{%sec}=PMATM_{%com}
  '       {%modelname}.append CIM_{%com}_{%sec}=MATM_{%com}_{%sec}
  '     endif
  '   next

  '   For %com {%list_com_E}
  '     if @elem(CIM_{%com}_{%sec},%baseyear) <> 0 then
  '       {%modelname}.append PCIM_{%com}_{%sec} = PEM_{%com} + TCO_VALM_{%com}_{%sec}/EM_{%com}_{%sec}
  '       {%modelname}.append CIM_{%com}_{%sec} = EM_{%com}_{%sec}
  '     endif
  '   next



  ' next

  'equation 1.22
  {%modelname}.equation(pv) M = sum(M[c] if M[c] <> 0 where c in {%list_com})

  ' %equation = "PM*M = 0"
  ' For %com {%list_com}
  '   if @elem(M_{%com},%baseyear) <> 0 then
  '     %equation = %equation+" + PM_"+%com+"*M_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' 'equation 1.23
  ' %equation = "M = 0"
  ' For %com {%list_com}
  '   if @elem(M_{%com},%baseyear) <> 0 then
  '     %equation = %equation+"+ M_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}



  '-----------GDP:PRODUCT DEFINITON------------------

  'equation 1.24
  {%modelname}.append PGDP*GDP = PCH*CH + PG*G + PI*I + PX*X + PDS*DS - PM*M

  'equation 1.25
  {%modelname}.append GDP = CH + G + I + X + DS - M

  ' equation 1.26 & 1.27
  {%modelname}.equation(pv) GDP[c] = CH[c] + G[c] + I[c] + X[c] + DS[c] - M[c] if GDP[c] <> 0 where c in {%list_com}

  ' For %com {%list_com}
  '   if @elem(GDP_{%com},%baseyear) <> 0 then

  '     'equation 1.26
  '     {%modelname}.append PGDP_{%com}*GDP_{%com}= PCH_{%com}*CH_{%com}+PG_{%com}*G_{%com}+PI_{%com}*I_{%com}+PX_{%com}*X_{%com}+PDS_{%com}*DS_{%com}-PM_{%com}* M_{%com}


  '     'equation 1.27
  '     {%modelname}.append GDP_{%com}= CH_{%com}+G_{%com}+I_{%com}+X_{%com}+DS_{%com}-M_{%com}
  '   endif
  ' next

  ' equation 1.28 & 1.29
  {%modelname}.equation(pv) GDPbis = sum(GDP[c] if GDP[c] <> 0 where c in {%list_com})

  ' 'equation 1.28
  ' %equation = "PGDPbis*GDPbis = 0"
  ' For %com {%list_com}
  '   if @elem(GDP_{%com},%baseyear) <> 0 then
  '     %equation = %equation+"+PGDP_"+%com+"*GDP_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' 'equation 1.29
  ' %equation = "GDPbis = 0"
  ' For %com {%list_com}
  '   if @elem(GDP_{%com},%baseyear) <> 0 then
  '     %equation = %equation+"+ GDP_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}


  '-----------GDP:VALUE ADDED DEFINITION--------------------
  'equation 1.30
  {%modelname}.append PGDPter * GDPter = PVA * VA + PTAX * TAX + PSUB * SUB


  'equation 1.31
  {%modelname}.append GDPter = VA + TAX + SUB

  ' equation 1.32
  {%modelname}.equation YQ[c]*PYQ[c] = PQD[c]*QD[c] - PVATD[c]*VATD[c] - POTHTD[c]*OTHTD[c] - PSUB[c]*SUB[c] - (PMCD[c]*MCD[c] + PMTD[c]*MTD[c])- PENERTD[c]*ENERTD[c] - TCO_VALD_sec[c] - TCO_HH_VAL_com[c] * CHD[c] / ((CH[c]>0) * CH[c] + (CH[c]<=0) * 1) if YQ[c] <> 0 where c in {%list_com}

  ' equation 1.33
  {%modelname}.equation YQbis[c] = QD[c] - VATD[c] - OTHTD[c] - SUB[c] - (MCD[c] + MTD[c]) - ENERTD[c] if YQ[c] <> 0 where c in {%list_com}

  ' equation 1.34
  {%modelname}.equation M[c]*PM[c] = PQM[c]*QM[c] - PVATM[c]*VATM[c] - POTHTM[c]*OTHTM[c] - (PMCM[c]*MCM[c] + PMTM[c]*MTM[c]) - PENERTM[c]*ENERTM[c] - TCO_VALM_sec[c] - TCO_HH_VAL_com[c] * CHM[c] / ((CH[c]>0) * CH[c] + (CH[c]<=0) * 1) if M[c] <> 0 where c in {%list_com}

  ' equation 1.35
  {%modelname}.equation Mbis[c] = QM[c] - VATM[c] - OTHTM[c] - (MCM[c]+MTM[c])- ENERTM[c] if M[c] <> 0 where c in {%list_com}

  ' PROBLEME! Eq. PMbis et PQbis devrait être utilisée pour calculer les volumes. Voir si la spécification actuelle est équivalente.
  ' For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  '   if @elem(YQ_{%com},%baseyear) <> 0 then
  '     'equation 1.32
  '     {%modelname}.append YQ_{%com}*PYQ_{%com} = PQD_{%com}*QD_{%com} - PVATD_{%com}*VATD_{%com} - POTHTD_{%com}*OTHTD_{%com} - PSUB_{%com}*SUB_{%com} -(PMCD_{%com}*MCD_{%com}+PMTD_{%com}*MTD_{%com})- PENERTD_{%com}*ENERTD_{%com} - TCO_VALD_sec_{%com} - TCO_HH_VAL_com_{%com}*CHD_{%com}/((CH_{%com}>0)*CH_{%com}+(CH_{%com}<=0)*1)

  '     'equation 1.33
  '     {%modelname}.append YQbis_{%com} = QD_{%com} - VATD_{%com} - OTHTD_{%com} - SUB_{%com} - (MCD_{%com}+MTD_{%com}) - ENERTD_{%com}
  '   endif

  '   if @elem(M_{%com},%baseyear) <> 0 then
  '     'equation 1.34

  '     {%modelname}.append M_{%com}*PM_{%com} = PQM_{%com}*QM_{%com} - PVATM_{%com}*VATM_{%com} - POTHTM_{%com}*OTHTM_{%com} - (PMCM_{%com}*MCM_{%com}+PMTM_{%com}*MTM_{%com})- PENERTM_{%com}*ENERTM_{%com} - TCO_VALM_sec_{%com} - TCO_HH_VAL_com_{%com}*CHM_{%com}/((CH_{%com}>0)*CH_{%com}+(CH_{%com}<=0)*1)


  '     'equation 1.35
  '     {%modelname}.append  Mbis_{%com} = QM_{%com} - VATM_{%com} - OTHTM_{%com} - (MCM_{%com}+MTM_{%com})- ENERTM_{%com}
  '   endif
  ' next


  ' equations 1.36, 1.37, 1.38 & 1.39
  {%modelname}.equation(pv) MT|O|[c] = sum(MT|O|[mar, c] if MT|O|[mar, c] <> 0 where mar in {%list_trsp}) if MT|O|[c] <> 0 where O in D M, c in 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24

  'equation 1.36 1.37  PROBLEME! Double indice sur les prix superflux. Corriger après l'ambiguité sur les indices. Puis endogeneiser PMTD_%mar (sans _%com)
  ' For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '   if @elem(MTD_{%com},%baseyear) <> 0 then
  '     %equation ="PMTD_"+%com+"*MTD_"+%com+"=0"
  '     For %mar {%list_trsp}
  '       if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+PMTD_"+%mar+"_"+%com+"*MTD_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '     %equation = "MTD_"+%com+"=0"
  '     For %mar {%list_trsp}
  '       if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+" +MTD_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '   endif
  ' next

  ' 'equation 1.38 1.39
  ' For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  '   if @elem(MTM_{%com},%baseyear) <> 0 then
  '     %equation ="PMTM_"+%com+"*MTM_"+%com+"=0"
  '     For %mar {%list_trsp}
  '       if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+PMTM_"+%mar+"_"+%com+"*MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '     %equation = "MTM_"+%com+"=0"
  '     For %mar {%list_trsp}
  '       if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
  '         %equation = %equation+"+MTM_"+%mar+"_"+%com
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '   endif
  ' next

  ' equations 1.40 & 1.41
  {%modelname}.equation(pv) MT[c] = MTD[c] + MTM[c] if MT[c] <> 0 where c in {%list_com}

  ' 'equation 1.40 1.41
  ' For %com {%list_com}
  '   if @elem(MT_{%com},%baseyear) <> 0 then

  '     {%modelname}.append PMT_{%com}*MT_{%com} = PMTD_{%com}*MTD_{%com} + PMTM_{%com}*MTM_{%com}
  '     {%modelname}.append MT_{%com} = MTD_{%com} + MTM_{%com}
  '   endif
  ' next

  ' equations 1.42 & 1.44
  {%modelname}.equation PI|O|[c] * I|O|[c] = sum(PIA|O|[c] * IA|O|[c, s] if IA|O|[c, s] <> 0 where s in {%list_sec}) if I|O|[c] <> 0 where O in D M, c in {%list_com}

  ' equations 1.43 & 1.45
  {%modelname}.equation I|O|[c] = sum(IA|O|[c, s] if IA|O|[c, s] <> 0 where s in {%list_sec}) if I|O|[c] <> 0 where O in D M, c in {%list_com}

  ' 'equation 1.42 1.43
  ' For %com {%list_com}
  '   if @elem(ID_{%com},%baseyear) <> 0 then
  '     %equation = "PID_"+%com+"*ID_"+%com+"=0"
  '     For %sec {%list_sec}
  '       if @elem(IAD_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation +"+ PIAD_"+%com+"*IAD_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}


  '     %equation = "ID_"+%com+"=0"
  '     For %sec {%list_sec}
  '       if @elem(IAD_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation +"+IAD_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '   endif
  ' next

  ' 'equation 1.44 1.45
  ' For %com {%list_com}
  '   if @elem(IM_{%com},%baseyear) <> 0 then
  '     %equation = "PIM_"+%com+"*IM_"+%com+"=0"
  '     For %sec {%list_sec}
  '       if @elem(IAM_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation +"+ PIAM_"+%com+"*IAM_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '     %equation = "IM_"+%com+"=0"
  '     For %sec {%list_sec}
  '       if @elem(IAM_{%com}_{%sec},%baseyear) <> 0 then
  '         %equation = %equation +"+IAM_"+%com+"_"+%sec
  '       endif
  '     next
  '     {%modelname}.append {%equation}

  '   endif
  ' next

  ' equations 1.46 & 1.47
  {%modelname}.equation(pv) VA[s] = Y[s] - MAT[s] - E[s] where s in {%list_sec} \ 21
  {%modelname}.append VA_21 * PVA_21 = PY_21 * Y_21 - PMAT_21 * MAT_21 - PE_21 * E_21

  ' 'equation 1.46 1.47
  ' For %sec {%list_sec}
  '   if %sec<>"21" then ' This condition solve the problem of division by 0 (VA_21 = 0). This condition appears at several places in the script.
  '     {%modelname}.append PVA_{%sec}*VA_{%sec} = PY_{%sec}*Y_{%sec} - PMAT_{%sec}*MAT_{%sec} - PE_{%sec}*E_{%sec}
  '     {%modelname}.append VA_{%sec} = Y_{%sec} - MAT_{%sec} - E_{%sec}

  '   Else

  '     {%modelname}.append VA_{%sec}*PVA_{%sec} = PY_{%sec}*Y_{%sec} - PMAT_{%sec}*MAT_{%sec} - PE_{%sec}*E_{%sec}
  '   endif
  ' next


  ' equations 1.48 & 1.49
  {%modelname}.equation(pv) VA = sum(VA[s] where s in {%list_sec})

  ' 'equation 1.48
  ' %equation = "PVA*VA=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+PVA_"+%sec+"*VA_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' 'equation 1.49
  ' %equation = "VA=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+VA_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' equations 1.50 & 1.51
  {%modelname}.equation PEBE[s]*EBE[s] = PVA[s]*VA[s] - CL[s] * L[s] * PROG_L[s] - PIY[s]*IY[s] - PSY[s]*SY[s] where s in {%list_sec} \ 21
  {%modelname}.equation EBE[s] = VA[s] - (CL[s] / PEBE[s]) * L[s] * PROG_L[s] - IY[s] - SY[s] where s in {%list_sec}

  ' 'equation 1.50 1.51
  ' For %sec {%list_sec}
  '   if %sec<>"21" then ' This condition solve the problem of division by 0 (VA_21 = 0). This condition appears at several places in the script.
  '     {%modelname}.append PEBE_{%sec}*EBE_{%sec}=PVA_{%sec}*VA_{%sec}-CL_{%sec}*L_{%sec}*PROG_L_{%sec}-PIY_{%sec}*IY_{%sec}-PSY_{%sec}*SY_{%sec}
  '   endif

  '   {%modelname}.append EBE_{%sec}=VA_{%sec}-(CL_{%sec}/PEBE_{%sec})*L_{%sec}*PROG_L_{%sec}-IY_{%sec}-SY_{%sec}
  ' next

  ' equations 1.52 & 1.53
  {%modelname}.equation(pv) EBE = sum(EBE[s] where s in {%list_sec})

  ' 'equation 1.52
  ' %equation = "PEBE*EBE=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+PEBE_"+%sec+"*EBE_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' 'equation 1.53
  ' %equation = "EBE=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+EBE_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' equations 1.54 & 1.55
  {%modelname}.equation PRF[s]*RF[s] = PEBE[s]*EBE[s] - PK[s](-1) * Tdec[s] * K[s](-1) where s in {%list_sec} \ 21
  {%modelname}.equation RF[s] = EBE[s] - @elem(PK[s](-1), {%baseyear}) * Tdec[s] * K[s](-1) where s in {%list_sec}

  ' 'equation 1.54 1.55
  ' For %sec {%list_sec}
  '   if %sec<>"21" then ' This condition solve the problem of division by 0 (VA_21 = 0). This condition appears at several places in the script.
  '     {%modelname}.append PRF_{%sec}*RF_{%sec} = PEBE_{%sec}*EBE_{%sec} - PK_{%sec}(-1)*Tdec_{%sec}*K_{%sec}(-1)
  '     ''     {%modelname}.append PRF_{%sec}*RF_{%sec} = PEBE_{%sec}*EBE_{%sec} - PIA_{%sec}*Tdec_{%sec}*K_{%sec}(-1)
  '     ''     {%modelname}.append PRF_{%sec}*RF_{%sec} = PEBE_{%sec}*EBE_{%sec} - CK_{%sec}*K_{%sec}(-1)


  '   endif
  '   {%modelname}.append RF_{%sec} = EBE_{%sec} - @elem(PK_{%sec}(-1),%baseyear)*Tdec_{%sec}*K_{%sec}(-1)
  '   ''     {%modelname}.append RF_{%sec} = EBE_{%sec} - Tdec_{%sec}*K_{%sec}(-1)
  '   ''    {%modelname}.append RF_{%sec} = EBE_{%sec} - @elem(CK_{%sec},%baseyear)*K_{%sec}(-1)
  ' next

  ' equations 1.56 & 1.57
  {%modelname}.equation(pv) RF = sum(RF[s] where s in {%list_sec})

  ' 'equation 1.56
  ' %equation = "PRF*RF=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+PRF_"+%sec+"*RF_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' 'equation 1.57
  ' %equation = "RF=0"
  ' For %sec {%list_sec}
  '   %equation = %equation +"+RF_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  {%modelname}.equation RF_NET[s] = @elem(PRF[s](-1), {%baseyear}) * RF[s](-1) - IS[s] where s in {%list_sec_Market}
  {%modelname}.equation PRF_NET[s]*RF_NET[s] = PRF[s](-1)*RF[s](-1) - PIS[s] * IS[s] where s in {%list_sec_Market}

  ' For %sec {%list_sec_Market}
  '   ''  {%modelname}.append RF_NET_{%sec}=RF_{%sec}(-1)-IS_{%sec}
  '   {%modelname}.append RF_NET_{%sec}=@elem(PRF_{%sec}(-1),%baseyear)*RF_{%sec}(-1)-IS_{%sec}
  '   {%modelname}.append PRF_NET_{%sec}*RF_NET_{%sec}=PRF_{%sec}(-1)*RF_{%sec}(-1)-PIS_{%sec}*IS_{%sec}


  ' next


  {%modelname}.equation(pv) RF_NET = sum(RF_NET[s] where s in {%list_sec_Market})
  {%modelname}.equation(pv) |V| = sum(|V|[s] if |V|[s] <> 0 where s in {%list_sec}) where V in Y IA
  {%modelname}.equation(pv) YQ = sum(YQ[c] if YQ[c] <> 0 where c in {%list_com})

  ' 'equation 1.56
  ' %equation = "PRF_NET*RF_NET=0"
  ' For %sec {%list_sec_Market}
  '   %equation = %equation +"+PRF_NET_"+%sec+"*RF_NET_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "RF_NET = 0"
  ' For %sec {%list_sec_Market}
  '   %equation = %equation+"+RF_NET_"+%sec
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "PY*Y =0"
  ' For %sec {%list_sec}
  '   if @elem(Y_{%sec},%baseyear) <> 0 then
  '     %equation = %equation+" +PY_"+%sec+"*Y_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "Y =0"
  ' For %sec {%list_sec}
  '   if @elem(Y_{%sec},%baseyear) <> 0 then
  '     %equation = %equation+" +Y_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}



  ' %equation = "PIA*IA =0"
  ' For %sec {%list_sec}
  '   if @elem(IA_{%sec},%baseyear) <> 0 then
  '     %equation = %equation+" +PIA_"+%sec+"*IA_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "IA=0"
  ' For %sec {%list_sec}
  '   if @elem(IA_{%sec},%baseyear) <> 0 then
  '     %equation = %equation+" +IA_"+%sec
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "PYQ*YQ =0"
  ' For %com {%list_com}
  '   if @elem(YQ_{%com},%baseyear) <> 0 then
  '     %equation = %equation+" +PYQ_"+%com+"*YQ_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}

  ' %equation = "YQ=0"
  ' For %com {%list_com}
  '   if @elem(YQ_{%com},%baseyear) <> 0 then
  '     %equation = %equation+" +YQ_"+%com
  '   endif
  ' next
  ' {%modelname}.append {%equation}


  '-------------------------Energy efficiency Indicator----------------------'

  {%modelname}.equation EFER[s] = E[s] / Y[s] where s in {%list_sec}
  {%modelname}.equation EFER_n[s] = E_n[s] / Y[s] where s in {%list_sec}

  ' For %sec {%list_sec}
  '   'effective Energy efficiency
  '   {%modelname}.append EFER_{%sec}=E_{%sec}/Y_{%sec}
  '   'desired Energy efficiency
  '   {%modelname}.append EFER_n_{%sec}=E_n_{%sec}/Y_{%sec}
  ' next

  ' ----------------------- Verif Compta nat.

  {%modelname}.append VERIF_PY_PYQ  = PY/PYQ-1
  {%modelname}.append VERIF_Y_YQ  = PY/PYQ-1

  {%modelname}.append VERIF_PIAxIA_PIxI  = PIA*IA/(PI*I)-1
  {%modelname}.append VERIF_PIA_PI  = PIA/PI-1
  {%modelname}.append VERIF_IA_I  = IA/I-1

  {%modelname}.append VERIF_GDP_GDPBIS  =  GDP/GDPBIS-1
  {%modelname}.append VERIF_GDP_GDPTER  =  GDP/GDPTER-1
  {%modelname}.append VERIF_PGDP_PGDPBIS  = PGDP/PGDPBIS-1
  {%modelname}.append VERIF_PGDP_PGDPTER  = PGDP/PGDPTER-1

  {%modelname}.append VERIF_ValGDP_ValGDPTER  = PGDP*GDP/(PGDPTER*GDPTER)-1


  '**************************************************************************************************************************************
  '******************************************** END BLOCK 1 : INPUT - OUTPUT EQUILIBRIUM ************************************************
  '**************************************************************************************************************************************




Endsub