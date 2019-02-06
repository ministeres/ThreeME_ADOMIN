subroutine BLOCK_InputOutput
  '***************************************************************************************************************************************
  '******************************************** BLOCK 1 : INPUT - OUTPUT EQUILIBRIUM ****************************************************
  '**************************************************************************************************************************************
  '**************************************************************************************************************************************

  ' equations 1.1, 1.2, 1.3 & 1.4
  {%modelname}.equation(pv) Q|O|[c] = CI|O|[c] + CH|O|[c] + G|O|[c] + I|O|[c] + X|O|[c] + DS|O|[c] if Q|O|[c] > 0, O in D M, c in {%list_com}

  ' equations 1.5 & 1.6
  {%modelname}.equation(pv) |V|[c] = |V|D[c] + |V|M[c] if |V|[c] <> 0, V in Q CH G I DS, c in {%list_com}

  ' equation 1.7
  {%modelname}.equation PX[c] * X[c] = PXD[c] * XD[c] + PXM[c] * XM[c] if X[c] <> 0, c in {%list_com}

  ' equations 1.8, 1.9, 1.10 & 1.11
  {%modelname}.equation(pv) |V||O| = sum(|V||O|[c] if |V||O|[c] <> 0, c in {%list_com}), V in Q CH G I X DS CI MT MC, O in D M
  ' equations 1.12 & 1.13
  {%modelname}.equation(pv) |V| = |V|D + |V|M, V in Q CH G I X DS CI
  {%modelname}.equation |V| = |V|D + |V|M, V in MT MC


  ' equations 1.14, 1.15, 1.16 & 1.17
  {%modelname}.equation(pv) CI|O|[c] = sum(CI|O|[c, s] if CI|O|[c, s] <> 0, s in {%list_com}) if CI|O|[c] > 0, c in {%list_com}, O in D M

  For %ci {%list_com}

    call aggregate_energy("CID_"+%ci)
    call priceindex_energy("PCID_"+%ci,"CID_"+%ci)

    call aggregate_energy("CIM_"+%ci)
    call priceindex_energy("PCIM_"+%ci,"CIM_"+%ci)
  next

  ' equations 1.18, 1.19, 1.20 & 1.21
  {%modelname}.equation PCI|O|[c, s] = PMAT|O|[c] if CI|O|[c, s] > 0, s in {%list_sec}, c in {%list_com_MAT}, O in D M
  {%modelname}.equation CI|O|[c, s] = MAT|O|[c, s] if CI|O|[c, s] > 0, s in {%list_sec}, c in {%list_com_MAT}, O in D M

  {%modelname}.equation PCI|O|[c, s] = PE|O|[c] + TCO_VAL|O|[c, s] / E|O|[c, s] if CI|O|[c, s] > 0, s in {%list_sec}, c in {%list_com_E}, O in D M
  {%modelname}.equation CI|O|[c, s] = E|O|[c, s] if CI|O|[c, s] > 0, s in {%list_sec}, c in {%list_com_E}, O in D M

  'equation 1.22
  {%modelname}.equation(pv) M = sum(M[c] if M[c] <> 0, c in {%list_com})

  '-----------GDP:PRODUCT DEFINITON------------------

  'equation 1.24
  {%modelname}.append PGDP*GDP = PCH*CH + PG*G + PI*I + PX*X + PDS*DS - PM*M

  'equation 1.25
  {%modelname}.append GDP = CH + G + I + X + DS - M

  ' equation 1.26 & 1.27
  {%modelname}.equation(pv) GDP[c] = CH[c] + G[c] + I[c] + X[c] + DS[c] - M[c] if GDP[c] <> 0, c in {%list_com}

  ' equation 1.28 & 1.29
  {%modelname}.equation(pv) GDPbis = sum(GDP[c] if GDP[c] <> 0, c in {%list_com})

  '-----------GDP:VALUE ADDED DEFINITION--------------------
  'equation 1.30
  {%modelname}.append PGDPter*GDPter = PVA*VA + PTAX*TAX + PSUB*SUB


  'equation 1.31
  {%modelname}.append GDPter = VA + TAX + SUB

  ' equation 1.32
  {%modelname}.equation YQ[c]*PYQ[c] = PQD[c]*QD[c] - PVATD[c]*VATD[c] - POTHTD[c]*OTHTD[c] - PSUB[c]*SUB[c] - (PMCD[c]*MCD[c] + PMTD[c]*MTD[c])- PENERTD[c]*ENERTD[c] - TCO_VALD_sec[c] - TCO_HH_VAL_com[c] * CHD[c] / ((CH[c]>0) * CH[c] + (CH[c]<=0) * 1) if YQ[c] <> 0, c in {%list_com}

  ' equation 1.33
  {%modelname}.equation YQbis[c] = QD[c] - VATD[c] - OTHTD[c] - SUB[c] - (MCD[c] + MTD[c]) - ENERTD[c] if YQ[c] <> 0, c in {%list_com}

  ' equation 1.34
  {%modelname}.equation M[c]*PM[c] = PQM[c]*QM[c] - PVATM[c]*VATM[c] - POTHTM[c]*OTHTM[c] - (PMCM[c]*MCM[c] + PMTM[c]*MTM[c]) - PENERTM[c]*ENERTM[c] - TCO_VALM_sec[c] - TCO_HH_VAL_com[c] * CHM[c] / ((CH[c]>0) * CH[c] + (CH[c]<=0) * 1) if M[c] <> 0, c in {%list_com}

  ' equation 1.35
  {%modelname}.equation Mbis[c] = QM[c] - VATM[c] - OTHTM[c] - (MCM[c]+MTM[c])- ENERTM[c] if M[c] <> 0, c in {%list_com}

  ' equations 1.36, 1.37, 1.38 & 1.39
  {%modelname}.equation(pv) MT|O|[c] = sum(MT|O|[mar, c] if MT|O|[mar, c] <> 0, mar in {%list_trsp}) if MT|O|[c] <> 0, O in D M, c in 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24

  ' equations 1.40 & 1.41
  {%modelname}.equation(pv) MT[c] = MTD[c] + MTM[c] if MT[c] <> 0, c in {%list_com}

  ' equations 1.42 & 1.44
  {%modelname}.equation PI|O|[c] * I|O|[c] = sum(PIA|O|[c] * IA|O|[c, s] if IA|O|[c, s] <> 0, s in {%list_sec}) if I|O|[c] <> 0, O in D M, c in {%list_com}

  ' equations 1.43 & 1.45
  {%modelname}.equation I|O|[c] = sum(IA|O|[c, s] if IA|O|[c, s] <> 0, s in {%list_sec}) if I|O|[c] <> 0, O in D M, c in {%list_com}

  ' equations 1.46 & 1.47
  {%modelname}.equation(pv) VA[s] = Y[s] - MAT[s] - E[s], s in {%list_sec} \ 21
  {%modelname}.append VA_21 * PVA_21 = PY_21 * Y_21 - PMAT_21 * MAT_21 - PE_21 * E_21

  ' equations 1.48 & 1.49
  {%modelname}.equation(pv) VA = sum(VA[s], s in {%list_sec})

  ' equations 1.50 & 1.51
  {%modelname}.equation PEBE[s]*EBE[s] = PVA[s]*VA[s] - CL[s] * L[s] * PROG_L[s] - PIY[s]*IY[s] - PSY[s]*SY[s], s in {%list_sec} \ 21
  {%modelname}.equation EBE[s] = VA[s] - (CL[s] / PEBE[s]) * L[s] * PROG_L[s] - IY[s] - SY[s], s in {%list_sec}

  ' equations 1.52 & 1.53
  {%modelname}.equation(pv) EBE = sum(EBE[s], s in {%list_sec})

  'equation 1.54 1.55
  {%modelname}.equation PRF[s]*RF[s] = PEBE[s]*EBE[s] - PK[s](-1) * Tdec[s] * K[s](-1), s in {%list_sec} \ 21
  {%modelname}.equation RF[s] = EBE[s] - @elem(PK[s](-1), {%baseyear}) * Tdec[s] * K[s](-1), s in {%list_sec}

  ' equations 1.56 & 1.57
  {%modelname}.equation(pv) RF = sum(RF[s], s in {%list_sec})

  {%modelname}.equation RF_NET[s] = @elem(PRF[s](-1), {%baseyear}) * RF[s](-1) - IS[s], s in {%list_sec_Market}
  {%modelname}.equation PRF_NET[s]*RF_NET[s] = PRF[s](-1)*RF[s](-1) - PIS[s] * IS[s], s in {%list_sec_Market}

  {%modelname}.equation(pv) RF_NET = sum(RF_NET[s], s in {%list_sec_Market})
  {%modelname}.equation(pv) |V| = sum(|V|[s] if |V|[s] <> 0, s in {%list_sec}), V in Y IA
  {%modelname}.equation(pv) YQ = sum(YQ[c] if YQ[c] <> 0, c in {%list_com})

  '-------------------------Energy efficiency Indicator----------------------'

  {%modelname}.equation EFER[s] = E[s] / Y[s], s in {%list_sec}
  {%modelname}.equation EFER_n[s] = E_n[s] / Y[s], s in {%list_sec}

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