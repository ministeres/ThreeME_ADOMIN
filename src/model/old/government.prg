subroutine BLOCK_Government

  '***************************************************************************************************************************************
  '******************************************** BLOCK 3 :GOVERNMENT****************************************************
  '**************************************************************************************************************************************
  '**************************************************************************************************************************************


  '------------------- TAXES ON ENERGY-----------------
 ' equation 3.1
 {%modelname}.equation PENERTD[c] * ENERTD[c] = TENERTD[c] * YQ[c] if ENERTD[c] <> 0 where c in {%list_com}

'   'equation 3.1
'   For %com {%list_com}
'     if @elem(ENERTD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  PENERTD_{%com}*ENERTD_{%com}= TenertD_{%com}*YQ_{%com}
'     endif
'   next

 'equation 3.2
 {%modelname}.equation ENERTD[c] = @elem(TENERTD[c], {%baseyear}) * YQ[c] if ENERTD[c] <> 0 where c in {%list_com}

'   'equation 3.2
'   For %com {%list_com}
'     if @elem(ENERTD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  ENERTD_{%com}= @elem(TenertD_{%com},%baseyear)*YQ_{%com}
'     endif
'   next

 'equation 3.3
 {%modelname}.equation PENERTM[c] * ENERTM[c] = TENERTM[c] * M[c] if ENERTM[c] <> 0 where c in {%list_com}

'   'equation 3.3
'   For %com {%list_com}
'     if @elem(ENERTM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  PENERTM_{%com}*ENERTM_{%com}= TenertM_{%com}*M_{%com}
'     endif
'   next

 'equation 3.4
 {%modelname}.equation ENERTM[c] = @elem(TENERTM[c], {%baseyear}) * M[c] if ENERTM[c] <> 0 where c in {%list_com}

'   'equation 3.4
'   For %com {%list_com}
'     if @elem(ENERTM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  ENERTM_{%com}= @elem(TenertM_{%com},%baseyear)*M_{%com}
'     endif
'   next

 'equation 3.5, 3.6
 {%modelname}.equation(pv) ENERT[c] = sum(ENERT|O|[c] where O in D M) if ENERT[c] <> 0 where c in {%list_com}

'   For %com {%list_com}
'     'equation 3.5
'     if @elem(ENERT_{%com},%baseyear) <> 0 then
'       %equation ="PENERT_"+%com+"*ENERT_"+%com+"=0"
'       %equation = %equation+"+PENERTD_"+%com+"*ENERTD_"+%com+"+PENERTM_"+%com+"*ENERTM_"+%com
'       {%modelname}.append {%equation}
'       'equation 3.6
'       %equation ="ENERT_"+%com+"=0"
'       %equation = %equation+"+ENERTD_"+%com+"+ENERTM_"+%com
'       {%modelname}.append {%equation}
'     endif
'   next

 'equation 3.7 where 3.8
 {%modelname}.equation(pv) ENERT = sum(ENERT[c] if ENERT[c] <> 0 where c in {%list_com})

'   'equation 3.7
'   %equation = "PENERT*ENERT=0"
'   For %com {%list_com}
'     if @elem(ENERT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+PENERT_"+%com+"*ENERT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.8
'   %equation = "ENERT=0"
'   For %com {%list_com}
'     if @elem(ENERT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+ENERT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}


  '-------------------VAT TAXES ON COMMODITY C-----------------
 'equations 3.9, 3.11
 {%modelname}.equation PVAT|O|[c] * VAT|O|[c] = TVAT|O|[c] * (PCH|O|[c] * CH|O|[c])/(1 + TVAT|O|[c]) + TVAT|O|OTH[c] * (PI|O|[c] * I|O|[c] + PCI|O|[c] * CI|O|[c] + PG|O|[c] * G|O|[c])/(1 + TVAT|O|OTH[c]) if VAT|O|[c] <> 0 where O in D M, c in {%list_com}

 'equations 3.10, 3.12
 {%modelname}.equation VAT|O|[c] = @elem(TVAT|O|[c], {%baseyear}) * (CH|O|[c])/(1 + @elem(TVAT|O|[c], {%baseyear})) + @elem(TVAT|O|OTH[c], {%baseyear}) * (I|O|[c] + CI|O|[c] + G|O|[c])/(1 + @elem(TVAT|O|OTH[c], {%baseyear})) if VAT|O|[c] <> 0 where O in D M, c in {%list_com}

'   'equation 3.9
'   For %com {%list_com}
'     if @elem(VATD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  PVATD_{%com}*VATD_{%com}= TvatD_{%com}*(PCHD_{%com}*CHD_{%com})/(1+TvatD_{%com})+TvatDoth_{%com}*(PID_{%com}*ID_{%com}+PCID_{%com}*CID_{%com}+PGD_{%com}*GD_{%com})/(1+TvatDoth_{%com})
'     endif
'   next
'
'   'equation 3.10
'   For %com {%list_com}
'     if @elem(VATD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  VATD_{%com}= @elem(TvatD_{%com},%baseyear)*(CHD_{%com})/(1+@elem(TvatD_{%com},%baseyear))+@elem(TvatDoth_{%com},%baseyear)*(ID_{%com}+CID_{%com}+GD_{%com})/(1+@elem(TvatDoth_{%com},%baseyear))
'     endif
'   next
'
'   'equation 3.11
'   For %com {%list_com}
'     if @elem(VATM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  PVATM_{%com}*VATM_{%com}= TvatM_{%com}*(PCHM_{%com}*CHM_{%com})/(1+TvatM_{%com})+TvatMoth_{%com}*(PIM_{%com}*IM_{%com}+PCIM_{%com}*CIM_{%com}+PGM_{%com}*GM_{%com})/(1+TvatMoth_{%com})
'     endif
'   next
'
'
'   'equation 3.12
'   For %com {%list_com}
'     if @elem(VATM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  VATM_{%com}= @elem(TvatM_{%com},%baseyear)*(CHM_{%com})/(1+@elem(TvatM_{%com},%baseyear))+@elem(TvatMoth_{%com},%baseyear)*(IM_{%com}+CIM_{%com}+GM_{%com})/(1+@elem(TvatMoth_{%com},%baseyear))
'     endif
'   next

 'equations 3.13 & 3.14
 {%modelname}.equation(pv) VAT[c] = sum(VAT|O|[c] where O in D M) if VAT[c] <> 0 where c in {%list_com}

'   'equation 3.13
'   For %com {%list_com}
'     if @elem(VAT_{%com},%baseyear) <> 0 then
'       %equation ="PVAT_"+%com+"*VAT_"+%com+"=0"
'       %equation = %equation+"+PVATD_"+%com+"*VATD_"+%com+"+PVATM_"+%com+"*VATM_"+%com
'       {%modelname}.append {%equation}
'       'equation 3.14
'       %equation ="VAT_"+%com+"=0"
'       %equation = %equation+"+VATD_"+%com+"+VATM_"+%com
'       {%modelname}.append {%equation}
'     endif
'   next

  ' equations 3.15 & 3.16
 {%modelname}.equation(pv) VAT = sum(VAT[c] if VAT[c] <> 0 where c in {%list_com})

'   'equation 3.15
'   %equation = "PVAT*VAT=0"
'   For %com {%list_com}
'     if @elem(VAT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+PVAT_"+%com+"*VAT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}

'   'equation 3.16
'   %equation = "VAT=0"
'   For %com {%list_com}
'     if @elem(VAT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+VAT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}


  '-------------------OTHER TAXES ON COMMODITY C-----------------
 ' equations 3.17
 {%modelname}.equation POTHTD[c] * OTHTD[c] = TOTHTD[c] * PYQ[c] * YQ[c] if OTHTD[c] <> 0 where c in {%list_com}

'   'equation 3.17
'   For %com {%list_com}
'     if @elem(OTHTD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  POTHTD_{%com}*OTHTD_{%com}= TothtD_{%com}*PYQ_{%com}*YQ_{%com}
'     endif
'   next

 'equations 3.18
 {%modelname}.equation OTHTD[c] = @elem(TOTHTD[c], {%baseyear}) * YQ[c] if OTHTD[c] <> 0 where c in {%list_com}

'   'equation 3.18
'   For %com {%list_com}
'     if @elem(OTHTD_{%com},%baseyear) <> 0 then
'       {%modelname}.append  OTHTD_{%com}= @elem(TothtD_{%com},%baseyear)*YQ_{%com}
'     endif
'   next

 'equation 3.19
 {%modelname}.equation POTHTM[c] * OTHTM[c] = TOTHTM[c] * PM[c] * M[c] if OTHTM[c] <> 0 where c in {%list_com}

'   'equation 3.19
'   For %com {%list_com}
'     if @elem(OTHTM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  POTHTM_{%com}*OTHTM_{%com}= TothtM_{%com}*PM_{%com}*M_{%com}
'     endif
'   next

 'equation 3.20
 {%modelname}.equation OTHTM[c] = @elem(TOTHTM[c], {%baseyear}) * M[c] if OTHTM[c] <> 0 where c in {%list_com}

'   'equation 3.20
'   For %com {%list_com}
'     if @elem(OTHTM_{%com},%baseyear) <> 0 then
'       {%modelname}.append  OTHTM_{%com}= @elem(TothtM_{%com},%baseyear)*M_{%com}
'     endif
'   next

 'equations 3.21 & 3.22
 {%modelname}.equation(pv) OTHT[c] = sum(OTHT|O|[c] where O in D M) if OTHT[c] <> 0 where c in {%list_com}

'   'equation 3.21
'   For %com {%list_com}
'     if @elem(OTHT_{%com},%baseyear) <> 0 then
'       %equation ="POTHT_"+%com+"*OTHT_"+%com+"=0"
'       %equation = %equation+"+POTHTD_"+%com+"*OTHTD_"+%com+"+POTHTM_"+%com+"*OTHTM_"+%com
'       {%modelname}.append {%equation}
'       'equation 3.22
'       %equation ="OTHT_"+%com+"=0"
'       %equation = %equation+"+OTHTD_"+%com+"+OTHTM_"+%com
'       {%modelname}.append {%equation}
'     endif
'   next

 'equations 3.23 & 3.24
 {%modelname}.equation(pv) OTHT = sum(OTHT[c] if OTHT[c] <> 0 where c in {%list_com})

'   'equation 3.23
'   %equation = "POTHT*OTHT=0"
'   For %com {%list_com}
'     if @elem(OTHT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+POTHT_"+%com+"*OTHT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.24
'   %equation = "OTHT=0"
'   For %com {%list_com}
'     if @elem(OTHT_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+OTHT_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}


  '-------------------TOTAL TAXES ON COMMODITY C-----------------

  series  TAX_{%com}=VAT_{%com}+ENERT_{%com}+OTHT_{%com}

 'equation 3.25
 {%modelname}.equation TAX[c] = VAT[c] + ENERT[c] + OTHT[c] if TAX[c] <> 0 where c in {%list_com}

 'equation 3.26
 {%modelname}.equation PTAX[c] * TAX[c] = PVAT[c] * VAT[c] + PENERT[c] * ENERT[c] + POTHT[c] * OTHT[c] + TCO_VAL_sec[c] + TCO_HH_VAL_com[c] if TAX[c] <> 0 where c in {%list_com}

'   For %com {%list_com}
'     'equation 3.25
'     if @elem(TAX_{%com},%baseyear) <> 0 then
'       {%modelname}.append TAX_{%com}=VAT_{%com}+ENERT_{%com}+OTHT_{%com}
'     endif
'     'equation 3.26
'     if @elem(TAX_{%com},%baseyear) <> 0 then
'       {%modelname}.append PTAX_{%com}*TAX_{%com}=PVAT_{%com}*VAT_{%com}+PENERT_{%com}*ENERT_{%com}+POTHT_{%com}*OTHT_{%com} + TCO_VAL_sec_{%com} + TCO_HH_VAL_com_{%com}
'     endif
'   next

 'equations 3.27 & 3.28
 {%modelname}.equation(pv) TAX = sum(TAX[c] if TAX[c] <> 0 where c in {%list_com})

'   'equation 3.27
'   %equation = "PTAX*TAX=0"
'   For %com {%list_com}
'     if @elem(TAX_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+PTAX_"+%com+"*TAX_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}
'
'
'   'equation 3.28
'   %equation = "TAX=0"
'   For %com {%list_com}
'     if @elem(TAX_{%com},%baseyear) <> 0 then
'       %equation = %equation +"+TAX_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}


  '--------------------TAXES ON BENEFITS (by activity)-------------------'
 'equation 3.29
 {%modelname}.equation PIS[s] * IS[s] = TIS * PRF[s](-1) * RF[s](-1) * (RF[s](-1)>0) + 0.00000001 * (RF[s](-1) =< 0) if IS[s] <> 0 where s in {%list_sec}

 'equation 3.30
 {%modelname}.equation  IS[s] = @elem(TIS * PRF[s](-1), {%baseyear}) * RF[s](-1) * (RF[s](-1)>0) + 0.00000001 * (RF[s](-1) =< 0) if IS[s] <> 0 where s in {%list_sec}

  ' For %sec {%list_sec}
  '   'equation 3.29
  '   ' if @elem(IS_{%sec},%baseyear) <> 0 then
  '   '   {%modelname}.append  PIS_{%sec}*IS_{%sec} = (TIS*PRF_{%sec}(-1)*RF_{%sec}(-1))*(RF_{%sec}(-1)>0) + 0.00000001*(RF_{%sec}(-1)=<0)
  '   ' endif

  '   'equation 3.30
  '   if @elem(IS_{%sec},%baseyear) <> 0 then
  '     {%modelname}.append IS_{%sec}= (@elem(TIS*PRF_{%sec}(-1),%baseyear)*RF_{%sec}(-1))*(RF_{%sec}(-1)>0) + 0.0000001*(RF_{%sec}(-1)=<0)

  '   endif
  ' next

 'equations 3.31 & 3.32
 {%modelname}.equation(pv) IS = sum(IS[s] if IS[s] <> 0 where s in {%list_sec})

'   'equation 3.31
'   %equation = "PIS*IS = 0"
'   For %sec {%list_sec}
'     if @elem(IS_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PIS_"+%sec+"*IS_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.32
'   %equation = "IS = 0"
'   For %sec {%list_sec}
'     if @elem(IS_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+ IS_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

 'equations 3.33
 {%modelname}.equation IR_VAL[h] = TIR[h] * DISPINC_AI_VAL[h] where h in {%list_household}
 {%modelname}.equation d(TIR[h]) = -d(PHIDI[h] * TCO_HH_VAL / DISPINC_AI_VAL[h]) where h in {%list_household}

'   'equation 3.33
'   For %hous {%list_household}
'     {%modelname}.append IR_VAL_{%hous} = TIR_{%hous}*DISPINC_AI_VAL_{%hous}
'     {%modelname}.append d(TIR_{%hous}) = -d(PhiDI_{%hous}*TCO_HH_VAL/DISPINC_AI_VAL_{%hous})
'     ''  {%modelname}.append TIR_{%hous} = (DP_G_VAL(-1)<=0.03)*TIR_{%hous}(-1)  + ((DP_G_VAL(-1))>0.04)*(Tir_{%hous}(-1)*1.1)
'   next

 'equation 3.34
  {%modelname}.equation IR_VAL = sum(IR_VAL[h] if IR_VAL[h] <> 0 where h in {%list_household})

'   'equation 3.34
'   %equation = "IR_VAL = 0"
'   For %hous {%list_household}
'     if @elem(IR_VAL_{%hous},%baseyear) <> 0 then
'       %equation = %equation+"+ IR_VAL_"+%hous
'     endif
'   next
'   {%modelname}.append {%equation}

  '-------------------TAXES ON CAPITAL-----------------
 'equation 3.35
 {%modelname}.equation AIC_VAL[h] = TAIC[h] * DISPINC_AI_VAL[h] where h in {%list_household}

'   'equation 3.35
'   For %hous {%list_household}
'     {%modelname}.append  AIC_VAL_{%hous}=TAIC_{%hous}*DISPINC_AI_VAL_{%hous}
'   next

 'equation 3.36
 {%modelname}.equation AIC_VAL = sum(AIC_VAL[h] if AIC_VAL[h] <> 0 where h in {%list_household})

'   'equation 3.36
'   %equation = "AIC_VAL = 0"
'   For %hous {%list_household}
'     if @elem(AIC_VAL_{%hous},%baseyear) <> 0 then
'       %equation = %equation+"+ AIC_VAL_"+%hous
'     endif
'   next
'   {%modelname}.append {%equation}

  '-------------------SUBVENTIONS ON COMMODITY C-----------------

 'equations 3.37
 {%modelname}.equation PSUB[c] * SUB[c] = TSUB[c] * YQ[c] if SUB[c] <> 0 where c in {%list_com}

 'equation 3.38
 {%modelname}.equation SUB[c] = @elem(TSUB[c], {%baseyear}) * YQ[c] if SUB[c] <> 0 where c in {%list_com}

'   'equation 3.37
'   For %com {%list_com}
'     if @elem(SUB_{%com},%baseyear) <> 0 then
'       {%modelname}.append  PSUB_{%com}*SUB_{%com}= Tsub_{%com}*YQ_{%com}
'     endif
'
'     'equation 3.38
'     if @elem(SUB_{%com},%baseyear) <> 0 then
'       {%modelname}.append  SUB_{%com}= @elem(Tsub_{%com},%baseyear)*YQ_{%com}
'     endif
'   next

 'equations 3.39 & 3.40
 {%modelname}.equation(pv) SUB = sum(SUB[c] if SUB[c] <> 0 where c in {%list_com})

'   'equation 3.39
'   %equation = "PSUB*SUB = 0"
'   For %com {%list_com}
'     if @elem(SUB_{%com},%baseyear) <> 0 then
'       %equation = %equation+" + PSUB_"+%com+"*SUB_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.40
'   %equation = "SUB = 0"
'   For %com {%list_com}
'     if @elem(SUB_{%com},%baseyear) <> 0 then
'       %equation = %equation+"+ SUB_"+%com
'     endif
'   next
'   {%modelname}.append {%equation}

  '--------------------TAXES AND SUBVENTIONS ON ACTIVITY-------------------------
 'equations 3.41
 {%modelname}.equation IY[s]  = @elem(TIYN[s], {%baseyear}) * Y[s] if IY[s] <> 0 where s in {%list_sec}

 'equation 3.42
 {%modelname}.equation PIY[s] * IY[s]  = TIYN[s] * PY[s] * Y[s] if IY[s] <> 0 where s in {%list_sec}

'   'equation 3.41
'   For %sec {%list_sec}
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'
'       {%modelname}.append IY_{%sec}= @elem(TIYN_{%sec},%baseyear)*Y_{%sec}
'     endif
'
'     'equation 3.42
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'       {%modelname}.append   PIY_{%sec}*IY_{%sec}= TIYN_{%sec}*PY_{%sec}*Y_{%sec}
'     endif
'   next
'
'   'equation 3.41
'   For %sec {%list_sec}
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'
'       '{%modelname}.append IY_{%sec}= TIYN_{%sec}*Y_{%sec}
'     endif
'     'equation 3.42
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'       '{%modelname}.append   PIY_{%sec}*IY_{%sec}= @elem(TIYN_{%sec},%baseyear)*PY_{%sec}*Y_{%sec}
'     endif
'   next

 'equations 3.43 & 3.44
 {%modelname}.equation(pv) IY  = sum(IY[s] if IY[s] <> 0 where s in {%list_sec})

'   'equation 3.43
'   %equation = "PIY*IY = 0"
'   For %sec {%list_sec}
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PIY_"+%sec+"*IY_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.44
'   %equation = "IY = 0"
'   For %sec {%list_sec}
'     if @elem(IY_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+ IY_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

 'equation 3.45
 {%modelname}.equation SY[s]  = @elem(TSYN[s], {%baseyear}) * Y[s] if SY[s] <> 0 where s in {%list_sec}

 'equation 3.46
 {%modelname}.equation PSY[s] * SY[s]  = TSYN[s] * PY[s] * Y[s] if SY[s] <> 0 where s in {%list_sec}

'   'equation 3.45
'   For %sec {%list_sec}
'     if @elem(SY_{%sec},%baseyear) <> 0 then
'       {%modelname}.append  SY_{%sec}= @elem(TSYN_{%sec},%baseyear)*Y_{%sec}
'
'       'equation 3.46
'       {%modelname}.append  PSY_{%sec}*SY_{%sec}= TSYN_{%sec}*PY_{%sec}*Y_{%sec}
'     endif
'   next

 'equations 3.47 & 3.48
 {%modelname}.equation(pv) SY  = sum(SY[s] if SY[s] <> 0 where s in {%list_sec})

'   'equation 3.47
'   %equation = "PSY*SY = 0"
'   For %sec {%list_sec}
'     if @elem(SY_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PSY_"+%sec+"*SY_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.48
'   %equation = "SY = 0"
'   For %sec {%list_sec}
'     if @elem(SY_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+ SY_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

  '-------------------***Social Security Accounting***-----------------
  ''***************** For ETS sectors

 {%modelname}.equation TCSE[s] = @elem(TCSE[s], {%baseyear}) - Rec_TCO_ETS/WAGES_ETS where s in {%list_sec}\01 02 03 11 12 13 14 15 16 17 19 20

'   For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
'     {%modelname}.append TCSE_{%sec} = @elem(TCSE_{%sec},%baseyear) - Rec_TCO_ETS/WAGES_ETS
'   next

  ''***************** For non ETS sectors
 {%modelname}.equation TCSE[s] = @elem(TCSE[s], {%baseyear}) - Rec_TCO_NETS/WAGES_NETS where s in {%list_sec}\04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406

'   For %sec 01 02 03 11 12 13 14 15 16 17 19 20
'     {%modelname}.append TCSE_{%sec} = @elem(TCSE_{%sec},%baseyear) - Rec_TCO_NETS/WAGES_NETS
'   next

 'equations 3.49 & 3.50
 {%modelname}.equation CSE[s] * PCSE[s] = TCSE[s] * L_S[s] * W_S[s] where s in {%list_sec}
 {%modelname}.equation PCSE[s] = PCH_19 where s in {%list_sec}

'   For %sec {%list_sec}
'     'equation 3.49
'     {%modelname}.append CSE_{%sec}*PCSE_{%sec}=TCSE_{%sec}*L_S_{%sec}*W_S_{%sec}
'
'     'equation 3.50
'     {%modelname}.append PCSE_{%sec}=PCH_19
'   next

 'equations 3.51 & 3.52
 {%modelname}.equation(pv) CSE  = sum(CSE[s] if CSE[s] <> 0 where s in {%list_sec})

'   'equation 3.51
'   %equation = "PCSE*CSE = 0"
'   For %sec {%list_sec}
'     if @elem(CSE_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PCSE_"+%sec+"*CSE_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.52
'   %equation = "CSE = 0"
'   For %sec {%list_sec}
'     if @elem(CSE_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+CSE_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

  'equation 3.53
  {%modelname}.append CSE_ROW*PCSE_ROW = TCSE_ROW*SB_ROW

  'equation 3.54
  {%modelname}.append PCSE_ROW = PCH_19

  'equation 3.55
  {%modelname}.append PCSE_TOT*CSE_TOT = PCSE*CSE + PCSE_ROW*CSE_ROW

  'equation 3.56
  {%modelname}.append CSE_TOT = CSE + CSE_ROW

 'equations 3.57, 3.58 , 3.59 & 3.60
 {%modelname}.equation CSS[s] * PCSS[s] = TCSS * L_S[s] * W_S[s] where s in {%list_sec}
 {%modelname}.equation PCSS[s] = PCH_19 where s in {%list_sec}
 {%modelname}.equation CSS_SE[s] * PCSS_SE[s] = TCSS_SE * L_SE[s] * W_SE[s] where s in {%list_sec}
 {%modelname}.equation PCSS_SE[s] = PCH_19 where s in {%list_sec}

'  For %sec {%list_sec}
'   'equation 3.57
'     {%modelname}.append CSS_{%sec}*PCSS_{%sec} = TCSS*L_S_{%sec}*W_S_{%sec}
'
'     'equation 3.58
'     {%modelname}.append PCSS_{%sec} = PCH_19
'
'     'equation 3.59
'     {%modelname}.append CSS_SE_{%sec}*PCSS_SE_{%sec} = TCSS_SE*L_SE_{%sec}*W_SE_{%sec}
'
'     'equation 3.60
'     {%modelname}.append PCSS_SE_{%sec} = PCH_19
'   next

 'equations 3.61 & 3.62
 {%modelname}.equation(pv) CSS  = sum(CSS[s] if CSS[s] <> 0 where s in {%list_sec})

'   'equation 3.61
'   %equation = "PCSS*CSS =0"
'   For %sec {%list_sec}
'     if @elem(CSS_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PCSS_"+%sec+"*CSS_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.62
'   %equation = "CSS =0"
'   For %sec {%list_sec}
'     if @elem(CSS_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+CSS_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

 'equations 3.63 & 3.64
 {%modelname}.equation(pv) CSS_SE  = sum(CSS_SE[s] if CSS_SE[s] <> 0 where s in {%list_sec})

'   'equation 3.63
'   %equation = "PCSS_SE*CSS_SE =0"
'   For %sec {%list_sec}
'     if @elem(CSS_SE_{%sec},%baseyear) <> 0 then
'       %equation = %equation+" + PCSS_SE_"+%sec+"*CSS_SE_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}
'
'   'equation 3.64
'   %equation = "CSS_SE =0"
'   For %sec {%list_sec}
'     if @elem(CSS_SE_{%sec},%baseyear) <> 0 then
'       %equation = %equation+"+CSS_SE_"+%sec
'     endif
'   next
'   {%modelname}.append {%equation}

  'equation 3.65
  {%modelname}.append  PCSS_TOT*CSS_TOT = PCSS_SE*CSS_SE + PCSS * (CSS + CSS_ROW)

  'equation 3.66
  {%modelname}.append CSS_TOT = CSS + CSS_SE + CSS_ROW


  '----------------------Total Receipts----------------------'

  {%modelname}.append DIV_GOV_VAL_n = alpha_GOV_FW*PRF_NET*RF_NET

  'equation 3.68
  {%modelname}.append REC_VAL=PY_20*Y_20-(CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20)+DIV_GOV_VAL+PTAX*TAX+PIY*IY+PIS*IS+IR_VAL+AIC_VAL+PCSE_TOT*CSE_TOT+PCSS_TOT*CSS_TOT+INC_GOV_OTH_net+REC_TCO_VAL


  '-----------------------***PUBLIC SPENDINGS***--------------------------------
  '----------------------Social Prestations----------------------'

  'Une fraction des prestations sociales (RSA; aide médicale généralisée etc... diminue avec le chômage et la pauvreté)


  'equation 3.69
  {%modelname}.append PRESOC_DOM_U_VAL=0.3*W_S*Un_TOT

  'equation 3.70
  {%modelname}.append d(log(PRESOC_DOM_Oth_VAL)) =eta_prest_1*d(log(PROG_L))+d(log(P))+d(log(POP_tot))'+(UnR_tot>0)*eta_prest_2*(PRESOC_DOM_U_VAL)/(PRESOC_VAL)d(log(UnR_TOT))

  'equation 3.71
  {%modelname}.append PRESOC_DOM_VAL=PRESOC_DOM_Oth_VAL+PRESOC_DOM_U_VAL

  'equation 3.72
  {%modelname}.append PRESOC_VAL=PRESOC_ROW_VAL+PRESOC_DOM_VAL


  '----------------------Government Consumption----------------------'

  'equation 3.73
  %equation = "PEXPG*EXPG=0"
  For %com {%list_com}
    if @elem(EXPG_{%com},%baseyear) <> 0 then
      %equation = %equation +"+ PEXPG_"+%com+"*EXPG_"+%com
    endif
  next
  '{%modelname}.append {%equation}

  'equation 3.74
  %equation = "EXPG=0"
  For %com {%list_com}
    if @elem(EXPG_{%com},%baseyear) <> 0 then
      %equation = %equation +"+ EXPG_"+%com
    endif
  next
  '{%modelname}.append {%equation}

 'equations 3.75 & 3.76
 {%modelname}.equation PEXPG[c] = PG[c] where c in {%list_com}
 {%modelname}.equation d(log(EXPG[c])) = d(log(EXPG)) if EXPG[c] <> 0 where c in {%list_com}

'   For %com {%list_com}
'     'equation 3.75
'     'à mettre dans les équations de prix'
'     {%modelname}.append PEXPG_{%com}=PG_{%com}
'
'     'equation 3.76
'     if @elem(EXPG_{%com},%baseyear) <> 0 then
'
'       {%modelname}.append d(log(EXPG_{%com}))=d(log(EXPG))
'     endif
'   next

 'equation 3.77
 {%modelname}.equation d(log(GD[c])) = d(log(EXPG[c])) + d(SUBST_GD[c]) if GD[c] <> 0 where c in {%list_com}
 {%modelname}.equation d(SUBST_GD_n[c]) = - ES_GOV(1, $c) * d(log(PGD[c]) - log(PGM[c])) * (PGM[c](-1) * GM[c](-1)/(PG[c](-1) * G[c](-1))) if GD[c] <> 0 where c in {%list_com}

'   'equation 3.77
'   !step_1=0
'   For %com  {%list_com}
'     if @elem(GD_{%com},%baseyear) <> 0 then
'       {%modelname}.append d(log(GD_{%com})) = d(log(EXPG_{%com}))+d(SUBST_GD_{%com})
'       {%modelname}.append d(SUBST_GD_n_{%com})= - ES_GOV(1,1+!step_1)*d(log(PGD_{%com}) - log(PGM_{%com}))*(PGM_{%com}(-1)*GM_{%com}(-1)/(PG_{%com}(-1)*G_{%com}(-1)))
'     endif
'     !step_1=!step_1+1
'   next

  'equation 3.78
  {%modelname}.equation GM[c] = (EXPG[c] - GD[c] > 0 ) * (EXPG[c]-GD[c]) + (EXPG[c]-GD[c] =< 0)*(0.00001) if GM[c] <> 0 where c in {%list_com}
  {%modelname}.equation d(SUBST_GM_n[c])= - ES_GOV(1, $c) * d(log(PGM[c]) - log(PGD[c])) * (PGD[c](-1) * GD[c](-1)/(PG[c](-1) * G[c](-1))) if GM[c] <> 0 where c in {%list_com}

  'equation 3.78
  ' !step_1=0
  ' For %com  {%list_com}
  '   if @elem(GM_{%com},%baseyear) <> 0 then
  '     {%modelname}.append GM_{%com} = (EXPG_{%com}-GD_{%com}>0)*(EXPG_{%com}-GD_{%com})+(EXPG_{%com}-GD_{%com}=<0)*(0.00001)
  '     '{%modelname}.append d(log(GM_{%com})) = d(log(EXPG_{%com}))+d(SUBST_GM_{%com})
  '     '{%modelname}.append d(SUBST_GM_n_{%com})= - ES_GOV(1,1+!step_1)*d(log(PGM_{%com}) - log(PGD_{%com}))*(PGD_{%com}(-1)*GD_{%com}(-1)/(PG_{%com}(-1)*G_{%com}(-1)))
  '   endif
  '   !step_1=!step_1+1
  ' next


  '-----------------------Total Spendings--------------------------------
  'equation 3.79
  {%modelname}.append DEP_VAL=CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20+R_G(-1)*DEBT_G_VAL(-1)+PRESOC_VAL+(PG*G-PG_20*G_20)-((PSUB*SUB-PSUB_01*SUB_01)+(PSY*SY-PSY_01*SY_01))+PIA_20*IA_20+TCO_HH_VAL

  '-----------------------***DEFICIT AND PUBLIC DEBT***--------------------------------
  '-----------------------Deficit--------------------------------
  'equation 3.80
  {%modelname}.append BF_G_VAL=DEP_VAL-REC_VAL

  'equation 3.81
  {%modelname}.append DP_G_VAL=BF_G_VAL/(PGDP*GDP)

  {%modelname}.append SP_G_VAL=BF_G_VAL-R_G(-1)*DEBT_G_VAL(-1)

  {%modelname}.append DP_SP_G_VAL=SP_G_VAL/(PGDP*GDP)


  '-----------------------Public Debt--------------------------------
  'equation 3.82
  {%modelname}.append DEBT_G_VAL=DEBT_G_VAL(-1)+BF_G_VAL

  {%modelname}.append d(log(EXPG)) = ADJUST(69,1)*(d(log(EXPG_trend))-STEADYSTATE(62,1)*(DP_G_VAL-DP_G_VAL_n)*PGDP(-1)*GDP(-1)/(PG(-1)*EXPG(-1))) + (1-ADJUST(69,1))*d(log(EXPG(-1)))

  {%modelname}.append d(FISC)=STEADYSTATE(58,1)*(DP_G_VAL-@elem(DP_G_VAL,%baseyear)) - STEADYSTATE(59,1)*(UNR_TOT-(@elem(UNR_TOT,%baseyear)+DNAIRU))

  '**************************************************************************************************************************************
  '**************************************************************************************************************************************
  '******************************************** END BLOCK 3 : GOVERNMENT************************************************
  '**************************************************************************************************************************************









endsub