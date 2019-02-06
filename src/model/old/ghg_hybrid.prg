


subroutine BLOCK_GHG_hybrid

  '-------------------------------CO2 household emissions from housing use--------------------------------------------------

' equation 6b.1

{%modelname}.equation d(log(EMS_HH_BUIL[ce2, h, class])) = (@year > {%baseyear}) * d(log(ENER_BUIL[h, class, ce2])) + (@year =< {%baseyear}) * (log(1 + STEADYSTATE(2,1))) if EMS_HH_BUIL[ce2, h, class] <> 0 where ce2 in {%list_com_E_CO2}, h in {%list_household}, class in {%list_ener_class}

  ' For %ener {%list_com_E_CO2}
  '   For %hous {%list_household}
  '     For %class {%list_ener_class}
  '       if @elem(EMS_HH_BUIL_{%ener}_{%hous}_{%class},%baseyear) <> 0 then
  '         {%modelname}.append d(log(EMS_HH_BUIL_{%ener}_{%hous}_{%class}))=(@year>{%baseyear})*d(log(ENER_BUIL_{%hous}_{%class}_{%ener}))+(@year=<{%baseyear})*(log(1+STEADYSTATE(2,1)))
  '       endif
  '     next
  '   next
  ' next


 ' equation 6b.2

{%modelname}.equation EMS_HH_BUIL[ce2, h] = sum( EMS_HH_BUIL[ce2, h, class] if EMS_HH_BUIL[ce2, h, class] <> 0 where class in {%list_ener_class} ) where ce2 in {%list_com_E_CO2}, h in {%list_household}

  'For %hous {%list_household}
  '  For %ener {%list_com_E_CO2}
  '    %equation = "EMS_HH_BUIL_"+%ener+"_"+%hous+"= 0"
  '    For %class {%list_ener_class}
  '      if @elem(EMS_HH_BUIL_{%ener}_{%hous}_{%class},%baseyear) <> 0 then
  '        %equation = %equation + "+ EMS_HH_BUIL_"+%ener+"_"+%hous+"_"+%class
  '      endif
  '    next
  '   {%modelname}.append {%equation}
  '  next
  'next

  ' equation 6b.3

{%modelname}.equation EMS_HH_BUIL[h, class] = sum( EMS_HH_BUIL[h, class, ce2] if EMS_HH_BUIL[h, class, ce2] <> 0 where ce2 in {%list_com_E_CO2} ) where h in {%list_household}, class in {%list_ener_class}

'  For %hous {%list_household}
'    For %class {%list_ener_class}
'      %equation = "EMS_HH_BUIL_"+%hous+"_"+%class+"= 0"
'      For %ener {%list_com_E_CO2}
'        if @elem(EMS_HH_BUIL_{%ener}_{%hous}_{%class},%baseyear) <> 0 then
'          %equation = %equation + "+ EMS_HH_BUIL_"+%ener+"_"+%hous+"_"+%class
'        endif
'      next
'      {%modelname}.append {%equation}
'    next
'  next

' equation 6b.4

{%modelname}.equation EMS_HH_BUIL[h] = sum( EMS_HH_BUIL[h, ce2] if EMS_HH_BUIL[h, ce2] <> 0 where ce2 in {%list_com_E_CO2}) where h in {%list_household}

'  For %hous {%list_household}
'    %equation = "EMS_HH_BUIL_"+%hous+"= 0"
'    For %hous {%list_household}
'      if @elem(EMS_HH_BUIL_{%ener}_{%hous},%baseyear) <> 0 then
'        %equation = %equation + "+ EMS_HH_BUIL_"+%ener+"_"+%hous
'      endif
'    next
'   {%modelname}.append {%equation}
'  next

' equation 6b.5

{%modelname}.equation EMS_HH_BUIL[class] = sum( EMS_HH_BUIL[class, h] if EMS_HH_BUIL[class, h] <> 0 where h in {%list_household}) where class in {%list_ener_class}

'  For %class {%list_ener_class}
'    %equation = "EMS_HH_BUIL_"+%class+"= 0"
'    For %ener {%list_com_E_CO2}
'      if @elem(EMS_HH_BUIL_{%hous}_{%class},%baseyear) <> 0 then
'        %equation = %equation + "+ EMS_HH_BUIL_"+%hous+"_"+%class
'      endif
'   next
'    {%modelname}.append {%equation}
'  next

' equation 6b.6

  {%modelname}.equation EMS_HH_BUIL = sum( EMS_HH_BUIL[h] if EMS_HH_BUIL[h] <> 0 where h in {%list_household})

  '%equation = "EMS_HH_BUIL= 0"
  'For %hous {%list_household}
  '  if @elem(EMS_HH_BUIL_{%hous},%baseyear) <> 0 then
  '    %equation = %equation + "+EMS_HH_BUIL_"+%hous
  '  endif
  'next
  '{%modelname}.append {%equation}

  '-------------------------------CO2 household emissions from automobile--------------------------------------------------

  ' equation 6b.7

{%modelname}.equation d(log(EMS_HH_AUTO_22[h, class])) = (@year>{%baseyear}) * d(log(EXP_AUTO_22[h, class]) + (@year=<{%baseyear}) * (log(1+STEADYSTATE(2,1))) where h in {%list_household}, class in {%list_ener_class}

 ' For %hous {%list_household}
 '   For %class {%list_ener_class}
 '     {%modelname}.append d(log(EMS_HH_AUTO_22_{%hous}_{%class}))=(@year>{%baseyear})*d(log(EXP_AUTO_{%hous}_{%class}_22))+(@year=<{%baseyear})*(log(1+STEADYSTATE(2,1)))
 '   next
 ' next

 ' equation 6b.8

 {%modelname}.equation EMS_HH_AUTO_22[h] = sum( EMS_HH_AUTO_22[h, class] if EMS_HH_AUTO_22[h, class] <> 0 where class in {%list_ener_class}) where h in {%list_household}

 ' For %hous {%list_household}
 '   %equation = "EMS_HH_AUTO_22_"+%hous+"= 0"
 '   For %class {%list_ener_class}
 '     if @elem(EMS_HH_AUTO_22_{%hous}_{%class},%baseyear) <> 0 then
 '       %equation = %equation + "+ EMS_HH_AUTO_22_"+%hous+"_"+%class
 '     endif
 '   next
 '   {%modelname}.append {%equation}
 ' next

 ' equation 6b.9

{%modelname}.equation EMS_HH_AUTO_22[class] = sum(EMS_HH_AUTO_22[class, h] if EMS_HH_AUTO_22[class, h] <> 0 where h in {%list_household}) where class in {%list_ener_class}

'
'    %equation = "EMS_HH_AUTO_22_"+%class+"= 0"
'    For %hous {%list_household}
'      if @elem(EMS_HH_AUTO_22_{%hous}_{%class},%baseyear) <> 0 then
'        %equation = %equation + "+ EMS_HH_AUTO_22_"+%hous+"_"+%class
'      endif
'    next
'    {%modelname}.append {%equation}
' next


' equation 6b.10

{%modelname}.equation EMS_HH_AUTO_22 = sum(EMS_HH_AUTO_22[h] if EMS_HH_AUTO_22[h] <> 0 where h in {%list_household})


'
'  For %hous {%list_household}
'    if @elem(EMS_HH_AUTO_22_{%hous},%baseyear) <> 0 then
'      %equation = %equation + "+EMS_HH_AUTO_22_"+%hous
'    endif
'  next
'  {%modelname}.append {%equation}


  '-------------------------------Aggregation of automobile and housing emissions--------------------------------------------------

' equation 6b.11

{%modelname}.equation EMS_HH[h, ce2, class] = EMS_HH_BUIL[h, ce2, class] where h in {%list_household}, ce2 in 21 24, class in  {%list_ener_class}

'  For %hous {%list_household}
'    For %ener 21 24
'      For %class {%list_ener_class}
'        {%modelname}.append  EMS_HH_{%ener}_{%hous}_{%class}=EMS_HH_BUIL_{%ener}_{%hous}_{%class}
'      next
'    next
'  next


' equation 6b.12

{%modelname}.equation EMS_HH_22[h, class] = EMS_HH_BUIL_22[h, class] + EMS_HH_AUTO_22[h, class] where h in {%list_household}, class in  {%list_ener_class}


'  For %hous {%list_household}
'    For %class {%list_ener_class}
'      {%modelname}.append  EMS_HH_22_{%hous}_{%class}=EMS_HH_BUIL_22_{%hous}_{%class}+EMS_HH_AUTO_22_{%hous}_{%class}
'    next
'  next


' equation 6b.13

{%modelname}.equation EMS_HH[h, ce2] = sum(EMS_HH[h, ce2, class] if EMS_HH[h, ce2, class]<>0 where class in  {%list_ener_class}) where h in {%list_household}, ce2 in {%list_com_E_CO2}


'  For %hous {%list_household}
'    For %ener {%list_com_E_CO2}
'      %equation = "EMS_HH_"+%ener+"_"+%hous+"= 0"
'      For %class {%list_ener_class}
'        if @elem(EMS_HH_{%ener}_{%hous}_{%class},%baseyear) <> 0 then
'          %equation = %equation + "+ EMS_HH_"+%ener+"_"+%hous+"_"+%class
'        endif
'      next
'      {%modelname}.append {%equation}
'    next
'  next


' equation 6b.14

{%modelname}.equation EMS_HH[h] = sum(EMS_HH[h, ce2] where ce2 in {%list_com_E_CO2}) where h in {%list_household}


' For %hous {%list_household}
'   %equation = "EMS_HH_"+%hous+"= 0"
'   For %ener {%list_com_E_CO2}
'    %equation = %equation + "+EMS_HH_"+%ener+"_"+%hous
'    next
'   {%modelname}.append {%equation}
'  next

' equation 6b.15

{%modelname}.equation EMS_HH[ce2] = sum(EMS_HH[ce2, h] where h in {%list_household}) where ce2 in {%list_com_E_CO2}

' For %ener {%list_com_E_CO2}
'    %equation = "EMS_HH_"+%ener+"= 0"
'    For %hous {%list_household}
'      %equation = %equation + "+EMS_HH_"+%ener+"_"+%hous
'    next
'    {%modelname}.append  {%equation}
'  next

' equation 6b.16

{%modelname}.equation EMS_HH = sum(EMS_HH[h] where h in {%list_household})

'  %equation = "EMS_HH= 0"
'  For %hous {%list_household}
'    %equation = %equation + "+EMS_HH_"+%hous
'  next
'  {%modelname}.append {%equation}

  '--------------------------------------***TOTAL CO2 EMISSIONS***-----------------------------------------------------------------------------------------

  'equation 6.7


  {%modelname}.equation EMS_TOT=EMS_S+EMS_DC+EMS_HH

  '{%modelname}.append EMS_TOT=EMS_S+EMS_DC+EMS_HH



  '------------------------------------***ENERGETIC PRODUCTION IN MTEP***----------------------------------------------------------

  '-----------------------------Energetic households consumption in Mtep by use -----------------------------'

  ' equation 6b.17

{%modelname}.equation d(log(Q_Mtep_H_BUIL[ene]))=(@year>{%baseyear})*d(log(ENER_BUIL[ene]))+(@year=<{%baseyear})*log(1+STEADYSTATE(2,1)) if ENER_BUIL[ene] <> 0 , ene in {%list_com_E}

'  For %ene {%list_com_E}
'    if @elem(ENER_BUIL_{%ene},%baseyear) <> 0 then
'      {%modelname}.append d(log(Q_Mtep_H_BUIL_{%ene}))=(@year>{%baseyear})*d(log(ENER_BUIL_{%ene}))+(@year=<{%baseyear})*log(1+STEADYSTATE(2,1))
'    endif
'  next


' equation 6b.18

{%modelname}.equation Q_Mtep_H_BUIL = sum ( Q_Mtep_H_BUIL[ene] if Q_Mtep_H_BUIL[ene] <> 0 where ene in {%list_com_E})


'  %equation = "Q_Mtep_H_BUIL=0"
'  For %ene {%list_com_E}
'    if @elem(Q_Mtep_H_BUIL_{%ene},%baseyear) <> 0 then
'      %equation = %equation +"+ Q_Mtep_H_BUIL_"+%ene
'    endif
'  next
'  {%modelname}.append {%equation}

 ' equation 6b.19

 {%modelname}.equation d(log(Q_Mtep_H_AUTO[ene]))=(@year>{%baseyear})*(d(log(EXP_AUTO[ene])))+(@year=<{%baseyear})*log(1+STEADYSTATE(2,1)), ene in 22 23

 ' For %ene  22 23
 '   {%modelname}.append d(log(Q_Mtep_H_AUTO_{%ene}))=(@year>{%baseyear})*(d(log(EXP_AUTO_{%ene})))+(@year=<{%baseyear})*log(1+STEADYSTATE(2,1))
 ' next

 'equation 6b.20

{%modelname}.equation Q_Mtep_H_AUTO = sum ( Q_Mtep_H_AUTO[ene] if Q_Mtep_H_AUTO[ene] <> 0 where ene in {%list_com_E})

'  %equation = "Q_Mtep_H_AUTO=0"
'  For %ene 22 23
'    if @elem(Q_Mtep_H_AUTO_{%ene},%baseyear) <> 0 then
'      %equation = %equation +"+ Q_Mtep_H_AUTO_"+%ene
'    endif
'  next
'  {%modelname}.append {%equation}


  '----------------------------Energetic production by energetic sector------------------------------------

  ' equation 6b.21

  {%modelname}.equation Q_Mtep_21 = Q_Mtep_SEC_21 + Q_Mtep_H_BUIL_21 + Q_Mtep_ESEC_21


  '%modelname}.append Q_Mtep_21=Q_Mtep_SEC_21+Q_Mtep_H_BUIL_21+Q_Mtep_ESEC_21

  ' equation 6b.22

{%modelname}.equation Q_Mtep[subsec]=phi_Mtep[subsec]*(Q_Mtep_H_BUIL_22 + Q_Mtep_H_AUTO_22 + Q_Mtep_SEC_22 + Q_Mtep_ESEC_22) where subsec in 2201 2202

 ' For %subsec 2201 2202
 '  {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_22+Q_Mtep_H_AUTO_22+Q_Mtep_SEC_22+Q_Mtep_ESEC_22)
 ' next

  ' equation 6b.23

{%modelname}.equation Q_Mtep[subsec]=phi_Mtep[subsec]*(Q_Mtep_H_BUIL_23 + Q_Mtep_H_AUTO_23 + Q_Mtep_SEC_23 + Q_Mtep_ESEC_23) where subsec in 2301 2302 2303 2304 2305 2306 2307 2308

'  For %subsec 2301 2302 2303 2304 2305 2306 2307 2308
'   {%modelname}.append  Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_23+Q_Mtep_H_AUTO_23+Q_Mtep_SEC_23+Q_Mtep_ESEC_23)
' next

  ' equation 6b.24

{%modelname}.equation Q_Mtep[subsec]=phi_Mtep[subsec]*(Q_Mtep_H_BUIL_24 + Q_Mtep_H_AUTO_24 + Q_Mtep_SEC_24 + Q_Mtep_ESEC_24) where subsec in 2401 2402 2403 2404 2405 2406

'  For %subsec 2401 2402 2403 2404 2405 2406
'    {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_24+Q_Mtep_SEC_24+Q_Mtep_ESEC_24)
'  next


endsub