'***************************************************************************************************************************************
'******************************************** BLOCK 4.B :CONSUMER HYBRID ****************************************************
'**************************************************************************************************************************************
'**************************************************************************************************************************************

subroutine BLOCK_Consumer_Hybrid

  '************************************************HOUSING BLOC************************************************************************************'
  '****************************************************************************************************************************************************'


  '------------------------------------------------------------------------------------------------------------------------------------------------------
  '--------------------------------------------------BUILDING STOCK DYNAMIC----------------------------------------------------------------------------'
  ' Equation H.4.36
  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "d(BUIL_"+%hous+"_"+%class+") = (@year>"+%baseyear+")*(phi_NewBUIL_"+%hous+"_"+%class+"*(d(BUIL_"+%hous+")+BUIL_"+%hous+"_DES)"
      For %class2 {%list_ener_class}
        if @elem(REHAB_{%hous}_{%class2}_{%class},%baseyear) <> 0 then
          %equation = %equation + "+ REHAB_"+%hous+"_"+%class2+"_"+%class
        endif
      next


      For %class2 {%list_ener_class}
        if @elem(delta_BUIL_{%hous}_{%class2}_{%class},%baseyear) <> 0 then
          %equation = %equation + "+ delta_BUIL_"+%hous+"_"+%class2+"_"+%class+"*BUIL_"+%hous+"_"+%class2+"(-1)"
        endif
      next


      For %class2 {%list_ener_class}
        if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
          %equation = %equation + "- REHAB_"+%hous+"_"+%class+"_"+%class2
        endif
      next

      For %class2 {%list_ener_class} DES
        if @elem(delta_BUIL_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
          %equation = %equation + "- delta_BUIL_"+%hous+"_"+%class+"_"+%class2+"*BUIL_"+%hous+"_"+%class+"(-1)"
        endif
      next

      {%modelname}.append {%equation} )

    next
  next

  ' Equation H.4.37

  For %hous {%list_household}
    %equation = "BUIL_"+%hous+"_DES = 0"

    For %class {%list_ener_class}
      if @elem(delta_BUIL_{%hous}_{%class}_DES,%baseyear) <> 0 then
        %equation = %equation + "+ delta_BUIL_"+%hous+"_"+%class+"_DES*BUIL_"+%hous+"_"+%class+"(-1)"
      endif
    next

    {%modelname}.append {%equation}
  next


  ' Equation H.4.38
  For %hous {%list_household}
    {%modelname}.append d(log(BUIL_{%hous})) = (@year>{%baseyear})*(d(log(POP_tot)) + d(log(M2perCapita)))
  next


  ' Equation H.
  For %class {%list_ener_class} DES
    %equation = "BUIL_"+%class+" = 0"
    For %hous {%list_household}
      %equation = %equation + "+ BUIL_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.
  %equation = "BUIL = 0"
  For %hous {%list_household}
    %equation = %equation + "+ BUIL_"+%hous
  next
  {%modelname}.append {%equation}

  ' Equation H.4.39  H.4.40
  For %hous {%list_household}
    For %class {%list_ener_class}

      if @elem(tau_REHAB_{%hous}_{%class},%baseyear) <> 0 then

        {%modelname}.append d(tau_REHAB_N_{%hous}_{%class}) = (@year>{%baseyear})*d(tau_REHAB_trend_{%hous}_{%class} _
        - nu_REHAB_{%hous}_{%class}*log(Payback_REHAB_{%hous}_{%class}))

        '+ nu_REHAB_{%hous}_{%class}*(UC_{%hous}_{%class}-UC_REHAB_{%hous}_{%class})/(UC_{%hous}_{%class}(-1)+UC_REHAB_{%hous}_{%class}(-1))) '' Specification Gaël

        {%modelname}.append tau_REHAB_N2_{%hous}_{%class} =   tau_REHAB_L_{%hous}_{%class}*(tau_REHAB_N_{%hous}_{%class}=<tau_REHAB_L_{%hous}_{%class}) _
        + tau_REHAB_H_{%hous}_{%class}*(tau_REHAB_N_{%hous}_{%class}>=tau_REHAB_H_{%hous}_{%class}) _
        + tau_REHAB_N_{%hous}_{%class}*(tau_REHAB_N_{%hous}_{%class}>tau_REHAB_L_{%hous}_{%class} AND tau_REHAB_N_{%hous}_{%class}<tau_REHAB_H_{%hous}_{%class})


        {%modelname}.append tau_REHAB_{%hous}_{%class} =   ADJUST(74,1)*tau_REHAB_N2_{%hous}_{%class} + (1-ADJUST(74,1))*tau_REHAB_{%hous}_{%class}(-1)

      endif

    next
  next


  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        {%modelname}.append Payback_REHAB_{%hous}_{%class} = (UC_K_REHAB_{%hous}_{%class}*BUIL_D_{%hous}_{%class} - UC_K_{%hous}_{%class}*BUIL_D_{%hous}_{%class})/(UC_E_{%hous}_{%class} - UC_E_REHAB_{%hous}_{%class}) - 1

        {%modelname}.append Payback_DISC_{%hous}_{%class} = -log(1-(STEADYSTATE(18,1)/(1+STEADYSTATE(18,1))*(DISC_C_K_REHAB_{%hous}_{%class} - DISC_C_K_{%hous}_{%class})/(UC_E_{%hous}_{%class} - UC_E_REHAB_{%hous}_{%class})<1)*(STEADYSTATE(18,1)/(1+STEADYSTATE(18,1))*(DISC_C_K_REHAB_{%hous}_{%class} - DISC_C_K_{%hous}_{%class})/(UC_E_{%hous}_{%class} - UC_E_REHAB_{%hous}_{%class}))) _
        /log(1+STEADYSTATE(18,1)) - 1
      endif
    next
  next

  ' Equation H.4.41
  For %hous {%list_household}
    For %class {%list_ener_class}
      For %class2 {%list_ener_class}
        if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
          {%modelname}.append REHAB_{%hous}_{%class}_{%class2} = phi_REHAB_{%hous}_{%class}_{%class2}*tau_REHAB_{%hous}_{%class}*BUIL_{%hous}_{%class}(-1)
        endif
      next
    next
  next

  ' Equation H.4.42
  For %hous {%list_household}
    For %class {%list_ener_class}

      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then

        %equation = "sum_phi_REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ phi_REHAB_"+%hous+"_"+%class+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}


      endif

    next
  next


  ' Equation H.4.43
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        {%modelname}.append UC_REHAB_{%hous}_{%class} = UC_K_REHAB_{%hous}_{%class} + UC_E_REHAB_{%hous}_{%class}
      endif
    next
  next

  ' Equation H.4.44
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then

        %equation = "UC_E_REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ phi_REHAB_"+%hous+"_"+%class+"_"+%class2+"*UC_E_"+%hous+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next
  next

  ' Equation H.4.45
  For %hous {%list_household}
    For %class {%list_ener_class}
      {%modelname}.append UC_{%hous}_{%class} = UC_K_{%hous}_{%class} + UC_E_{%hous}_{%class}
    next
  next

  ' Equation H.4.46 H.4.47  H.4.48'
  For %hous {%list_household}

    For %class {%list_ener_class}

      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        {%modelname}.append UC_K_REHAB_{%hous}_{%class}  =  PREHAB_delta_{%hous}_{%class}*(R_CASH_REHAB_{%hous}_{%class}+R_LOAN_REHAB_{%hous}_{%class}*R_I_REHAB_{%hous}_{%class}(-1)*LD_REHAB_{%hous}_{%class}/(1-(1+R_I_REHAB_{%hous}_{%class}(-1))^(-LD_REHAB_{%hous}_{%class})))

        {%modelname}.append  R_LOAN_REHAB_{%hous}_{%class} = 1 - R_CASH_REHAB_{%hous}_{%class}

        {%modelname}.append DISC_C_K_REHAB_{%hous}_{%class}  =  PREHAB_{%hous}_{%class}*(R_CASH_REHAB_{%hous}_{%class}+((1-(1+STEADYSTATE(18,1))^(-LD_REHAB_{%hous}_{%class}))/STEADYSTATE(18,1))*R_LOAN_REHAB_{%hous}_{%class}*R_I_REHAB_{%hous}_{%class}(-1)/(1-(1+R_I_REHAB_{%hous}_{%class}(-1))^(-LD_REHAB_{%hous}_{%class})))

      endif

      'Equation H.4.49 H.4.50  H.4.51'
      {%modelname}.append  UC_K_{%hous}_{%class}  =  PREHAB_{%hous}_{%class}_{%class}/BUIL_D_{%hous}_{%class}*(R_CASH_{%hous}_{%class}+R_LOAN_{%hous}_{%class}*R_I_BUIL_{%hous}_{%class}(-1)*LD_{%hous}_{%class}/(1-(1+R_I_BUIL_{%hous}_{%class}(-1))^(-LD_{%hous}_{%class})))
      {%modelname}.append  R_LOAN_{%hous}_{%class} = 1 - R_CASH_{%hous}_{%class}

      {%modelname}.append  DISC_C_K_{%hous}_{%class}  =  PREHAB_{%hous}_{%class}_{%class}*(R_CASH_{%hous}_{%class}+((1-(1+STEADYSTATE(18,1))^(-LD_REHAB_{%hous}_{%class}))/STEADYSTATE(18,1))*R_LOAN_{%hous}_{%class}*R_I_BUIL_{%hous}_{%class}(-1)/(1-(1+R_I_BUIL_{%hous}_{%class}(-1))^(-LD_{%hous}_{%class})))

    next

  next

  ' Equation H.4.52
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = "delta_REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(phi_REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ phi_REHAB_"+%hous+"_"+%class+"_"+%class2+"*delta_BUIL_"+%hous+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next

  next

  ' Equation H.4.53
  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "delta_BUIL_"+%hous+"_"+%class+" = 0"
      For %class2 {%list_ener_class} DES
        if @elem(delta_BUIL_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
          %equation = %equation + "+ delta_BUIL_"+%hous+"_"+%class+"_"+%class2
        endif
      next
      {%modelname}.append {%equation}
    next
  next


  ' Equation H.4.54  H.4.55  H.4.56
  For %hous {%list_household}
    For %class {%list_ener_class}  'PROBLEME car BUIL<0, certains PENER_m2 en 2005 sont negatifs

      {%modelname}.append UC_E_{%hous}_{%class} = PENER_m2_{%hous}_{%class}*((GR_PENER_m2_e_{%hous}_{%class}=<0.00001)*1+(GR_PENER_m2_e_{%hous}_{%class}>0.00001)*((1+GR_PENER_m2_e_{%hous}_{%class})^(BUIL_D_{%hous}_{%class})-1) _
      /((GR_PENER_m2_e_{%hous}_{%class}+(GR_PENER_m2_e_{%hous}_{%class}=0)*0.0000001)*BUIL_D_{%hous}_{%class}))


      ' Replace the specification below to avoid division by 0 problem
      '{%modelname}.append UC_E_{%hous}_{%class} = PENER_m2_{%hous}_{%class}*((GR_PENER_m2_e_{%hous}_{%class}=0)*1+(GR_PENER_m2_e_{%hous}_{%class}<>0)*((1+GR_PENER_m2_e_{%hous}_{%class})^(BUIL_D_{%hous}_{%class})-1) _
      '/((GR_PENER_m2_e_{%hous}_{%class}+(GR_PENER_m2_e_{%hous}_{%class}=0)*0.0000001)*BUIL_D_{%hous}_{%class}))


      {%modelname}.append PENER_m2_{%hous}_{%class}*BUIL_{%hous}_{%class} =  PENER_BUIL_{%hous}_{%class}*ENER_BUIL_{%hous}_{%class}



      if @elem(PENER_m2_{%hous}_{%class},%baseyear) <> 0 then

        {%modelname}.append GR_PENER_m2_e_{%hous}_{%class} = ADJUST(72,2)*GR_PENER_m2_e_{%hous}_{%class}(-1) + (1-ADJUST(72,2))*@pchy(PENER_m2_{%hous}_{%class}(-1))
        ''         {%modelname}.append GR_PENER_m2_e_{%hous}_{%class} = (ADJUST(72,2)*GR_PENER_m2_e_{%hous}_{%class}(-1) + (1-ADJUST(72,2))*@pchy(PENER_m2_{%hous}_{%class}(-1)))*(ADJUST(72,2)*GR_PENER_m2_e_{%hous}_{%class}(-1) + (1-ADJUST(72,2))*@pchy(PENER_m2_{%hous}_{%class}(-1))>STEADYSTATE(15,1)) _
        ''                                                                          + STEADYSTATE(15,1)*(ADJUST(72,2)*GR_PENER_m2_e_{%hous}_{%class}(-1) + (1-ADJUST(72,2))*@pchy(PENER_m2_{%hous}_{%class}(-1))=<STEADYSTATE(15,1))
      endif

    next
  next


  ' Equation H.4.57
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = "PREHAB_delta_"+%hous+"_"+%class+"*REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ (1-R_SUB_"+%hous+"_"+%class+"_"+%class2+")*PREHAB_"+%hous+"_"+%class+"_"+%class2+"*REHAB_"+%hous+"_"+%class+"_"+%class2+"/REHAB_D_"+%hous+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next
  next

  ' Equation H.4.58
  For %hous {%list_household}
    For %class {%list_ener_class}

      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = "Ver_PREHABdelta_"+%hous+"_"+%class+"*REHAB_"+%hous+"_"+%class+" = - PREHAB_delta_"+%hous+"_"+%class
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ (1-R_SUB_"+%hous+"_"+%class+"_"+%class2+")*PREHAB_"+%hous+"_"+%class+"_"+%class2+"*phi_REHAB_"+%hous+"_"+%class+"_"+%class2+"*delta_BUIL_"+%hous+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next
  next

  'Equation H.4.59 H.4.60  H.4.61  H.4.62 H.4.63  H.4.64
  'PROBLEM! Rajouter les effets prix dans l'arbitrage a savoir sur PREHAB PNEWBUIL et PENER_BUIL '
  For %hous {%list_household}
    For %class {%list_ener_class}
      'gelvol '{%modelname}.append EXP_HOUSING_Val_{%hous}_{%class} = DEBT_REHAB_Val_{%hous}_{%class}(-1)*(R_I_REHAB_{%hous}_{%class}(-1)+R_RMBS_REHAB_{%hous}_{%class}(-1)) + R_CASH_REHAB_{%hous}_{%class}*PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class} _
      'gelvol '+ DEBT_NewB_Val_{%hous}_{%class}(-1)*(R_I_NewBUIL_{%hous}_{%class}(-1)+R_RMBS_NewBUIL_{%hous}_{%class}(-1)) + R_CASH_NewBUIL_{%hous}_{%class}*PNewBUIL_{%hous}_{%class}*NewBUIL_{%hous}_{%class} _
      'gelvol '+ PENER_BUIL_{%hous}_{%class}*ENER_BUIL_{%hous}_{%class}


      {%modelname}.append EXP_HOUSING_Val_{%hous}_{%class} = (@year>{%baseyear})*(DEBT_REHAB_Val_{%hous}_{%class}(-1)*(R_I_REHAB_{%hous}_{%class}(-1)+R_RMBS_REHAB_{%hous}_{%class}(-1)) + R_CASH_REHAB_{%hous}_{%class}*PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class} _
      + DEBT_NewB_Val_{%hous}_{%class}(-1)*(R_I_NewBUIL_{%hous}_{%class}(-1)+R_RMBS_NewBUIL_{%hous}_{%class}(-1)) + R_CASH_NewBUIL_{%hous}_{%class}*PNewBUIL_{%hous}_{%class}*NewBUIL_{%hous}_{%class} _
      + PENER_BUIL_{%hous}_{%class}*ENER_BUIL_{%hous}_{%class}) _
      + (@year=<{%baseyear})*EXP_HOUSING_Val_{%hous}_{%class}(-1)*(1+STEADYSTATE(1,1))





      'gelvol '{%modelname}.append d(DEBT_REHAB_Val_{%hous}_{%class}) = (@year>{%baseyear})*(-R_RMBS_REHAB_{%hous}_{%class}*DEBT_REHAB_Val_{%hous}_{%class}(-1) + R_LOAN_REHAB_{%hous}_{%class}*PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class})
      'gelvol '
      'gelvol '{%modelname}.append d(DEBT_NewB_Val_{%hous}_{%class}) = (@year>{%baseyear})*(-R_RMBS_NewBUIL_{%hous}_{%class}*DEBT_NewB_Val_{%hous}_{%class}(-1) + R_LOAN_NewBUIL_{%hous}_{%class}*PNewBUIL_{%hous}_{%class}*NewBUIL_{%hous}_{%class})

      ''     {%modelname}.append DEBT_REHAB_Val_{%hous}_{%class} = (@year>{%baseyear})*(1-R_RMBS_REHAB_{%hous}_{%class}*DEBT_REHAB_Val_{%hous}_{%class}(-1) + R_LOAN_REHAB_{%hous}_{%class}*PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class}) _
      {%modelname}.append DEBT_REHAB_Val_{%hous}_{%class} = (@year>{%baseyear})*((1-R_RMBS_REHAB_{%hous}_{%class})*DEBT_REHAB_Val_{%hous}_{%class}(-1) + R_LOAN_REHAB_{%hous}_{%class}*PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class}) _
      + (@year=<{%baseyear})*DEBT_REHAB_Val_{%hous}_{%class}(-1)*(1+STEADYSTATE(1,1))

      ''     {%modelname}.append DEBT_NewB_Val_{%hous}_{%class} = (@year>{%baseyear})*(1-R_RMBS_NewBUIL_{%hous}_{%class}*DEBT_NewB_Val_{%hous}_{%class}(-1) + R_LOAN_NewBUIL_{%hous}_{%class}*PNewBUIL_{%hous}_{%class}*NewBUIL_{%hous}_{%class}) _
      {%modelname}.append DEBT_NewB_Val_{%hous}_{%class} = (@year>{%baseyear})*((1-R_RMBS_NewBUIL_{%hous}_{%class})*DEBT_NewB_Val_{%hous}_{%class}(-1) + R_LOAN_NewBUIL_{%hous}_{%class}*PNewBUIL_{%hous}_{%class}*NewBUIL_{%hous}_{%class}) _
      + (@year=<{%baseyear})*DEBT_NewB_Val_{%hous}_{%class}(-1)*(1+STEADYSTATE(1,1))

      {%modelname}.append d(log(PNewBUIL_{%hous}_{%class})) = d(log(PCH_13))
      'gelprix '{%modelname}.append d(log(PNewBUIL_{%hous}_{%class})) = (@year>{%baseyear})*d(log(PCH_13))

      For %class2 {%list_ener_class}
        if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
          {%modelname}.append d(log(PREHAB_{%hous}_{%class}_{%class2})) = d(log(PCH_13))
          'gelprix '{%modelname}.append d(log(PREHAB_{%hous}_{%class}_{%class2})) = (@year>{%baseyear})*d(log(PCH_13))
        endif
      next

      'gelvol 'if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 and @elem(DEBT_REHAB_Val_{%hous}_{%class},%baseyear) <> 0 then
      'gelvol '{%modelname}.append R_REHAB_DEBT_{%hous}_{%class}=PREHAB_{%hous}_{%class}*REHAB_{%hous}_{%class}/DEBT_REHAB_Val_{%hous}_{%class}
      'gelvol 'endif

    next
  next

  For %hous {%list_household}
    For %class {%list_ener_class}
      {%modelname}.append d(log(PREHAB_{%hous}_{%class}_{%class})) = d(log(PCH_13))   ' Must be added because REHAB_{%hous}_{%class}_{%class} = 0
      'gelprix '{%modelname}.append d(log(PREHAB_{%hous}_{%class}_{%class})) = (@year>{%baseyear})*d(log(PCH_13))   ' Must be added because REHAB_{%hous}_{%class}_{%class} = 0
    next
  next



  '------------AGGREGATION SURFACE RENOVEE EN M2 ET EN MILLION D'EURO'
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = "PREHAB_"+%hous+"_"+%class+"*REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ PREHAB_"+%hous+"_"+%class+"_"+%class2+"*REHAB_"+%hous+"_"+%class+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next
  next


  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = "REHAB_"+%hous+"_"+%class+" = 0"
        For %class2 {%list_ener_class}
          if @elem(REHAB_{%hous}_{%class}_{%class2},%baseyear) <> 0 then
            %equation = %equation + "+ REHAB_"+%hous+"_"+%class+"_"+%class2
          endif
        next
        {%modelname}.append {%equation}
      endif
    next
  next


  ' *************************************

  For %hous {%list_household}
    %equation = "PREHAB_"+%hous+"*REHAB_"+%hous+" = 0"
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ PREHAB_"+%hous+"_"+%class+"*REHAB_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  For %hous {%list_household}
    %equation = "REHAB_"+%hous+" = 0"
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ REHAB_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  '****************************************'

  %equation = "PREHAB*REHAB = 0"
  For %hous {%list_household}
    if @elem(REHAB_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ PREHAB_"+%hous+"*REHAB_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  %equation = "REHAB = 0"
  For %hous {%list_household}
    if @elem(REHAB_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ REHAB_"+%hous
    endif
  next
  {%modelname}.append {%equation}




  For %hous {%list_household}
    %equation = "EXP_HOUSING_Val_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(EXP_HOUSING_Val_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ EXP_HOUSING_Val_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next


  %equation = "EXP_HOUSING_Val= 0"
  For %hous {%list_household}
    if @elem(EXP_HOUSING_Val_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_HOUSING_Val_"+%hous
    endif
  next
  {%modelname}.append {%equation}


  For %hous {%list_household}
    {%modelname}.append  EXP_REHAB_VAL_{%hous} =PREHAB_{%hous}*REHAB_{%hous}
  next

  %equation = "EXP_REHAB_VAL = 0"
  For %hous {%list_household}
    %equation = %equation + "+ EXP_REHAB_VAL_"+%hous
  next
  {%modelname}.append  {%equation}
  For %hous {%list_household}
    {%modelname}.append EXP_NEWBUIL_VAL_{%hous} =(PNEWBUIL_{%hous}*NEWBUIL_{%hous})
  next

  %equation = "EXP_NEWBUIL_VAL = 0"
  For %hous {%list_household}
    %equation = %equation + "+ EXP_NEWBUIL_VAL_"+%hous
  next
  {%modelname}.append {%equation}



  ' Equation H.4.66
  %equation = "EXP_13_OTH_Val = 0"
  For %hous {%list_household}
    %equation = %equation + "+ EXP_13_OTH_Val_"+%hous
  next
  {%modelname}.append {%equation}


  ' Equation H.4.67   H.4.68
  For %hous {%list_household}
    'gelvol '{%modelname}.append d(log(EXP_13_OTH_VAL_{%hous})) = (@year>{%baseyear})*(d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))
    {%modelname}.append d(log(EXP_13_OTH_VAL_{%hous})) = (d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))

    'gelvol '{%modelname}.append EXP_13_{%hous} = @elem(PNewBUIL_{%hous},%baseyear)*NewBUIL_{%hous} + @elem(PREHAB_{%hous},%baseyear)*REHAB_{%hous} +  EXP_13_OTH_val_{%hous}/PEXP_13_{%hous}
    {%modelname}.append EXP_13_{%hous} = (@year>{%baseyear})*(@elem(PNewBUIL_{%hous},%baseyear)*NewBUIL_{%hous} + @elem(PREHAB_{%hous},%baseyear)*REHAB_{%hous} +  EXP_13_OTH_val_{%hous}/PEXP_13_{%hous}) _
    + (@year=<{%baseyear})*EXP_13_{%hous}(-1)*(1+STEADYSTATE(2,1))
  next

  ' Equation H.4.69
  %equation = "EXP_13= 0"
  For %hous {%list_household}
    if @elem(EXP_13_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_13_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.70
  For %hous {%list_household}
    For %class {%list_ener_class}
      ''      {%modelname}.append NewBUIL_{%hous}_{%class} = phi_NewBUIL_{%hous}_{%class}*(d(BUIL_{%hous})+BUIL_{%hous}_DES)
      {%modelname}.append NewBUIL_{%hous}_{%class} = (@year>{%baseyear})*phi_NewBUIL_{%hous}_{%class}*(d(BUIL_{%hous})+BUIL_{%hous}_DES) _
      + (@year<={%baseyear})*NewBUIL_{%hous}_{%class}(-1)

    next
  next

  '------------------------------------------AGGREGATION NEWBUIL-----------------------------------------------------'
  For %hous {%list_household}
    %equation = "NEWBUIL_"+%hous+" = 0"
    For %class {%list_ener_class}
      if @elem(NEWBUIL_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ NEWBUIL_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  For %hous {%list_household}
    %equation = "PNEWBUIL_"+%hous+"*NEWBUIL_"+%hous+" = 0"
    For %class {%list_ener_class}
      if @elem(NEWBUIL_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ PNEWBUIL_"+%hous+"_"+%class+"*NEWBUIL_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  %equation = "NEWBUIL = 0"
  For %hous {%list_household}
    if @elem(NEWBUIL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ NEWBUIL_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  %equation = "PNEWBUIL*NEWBUIL = 0"
  For %hous {%list_household}
    if @elem(NEWBUIL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ PNEWBUIL_"+%hous+"*NEWBUIL_"+%hous
    endif
  next
  {%modelname}.append {%equation}


  '--------------------------------------------------------- VERIFICATIONS-----------------------------------------'

  ' Equation H.4.71
  For %hous {%list_household}
    %equation = "BUIL_verif_"+%hous+" = 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ BUIL_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.72
  %equation = "BUIL_verif = 0"
  For %hous {%list_household}
    %equation = %equation + "+ BUIL_verif_"+%hous
  next
  {%modelname}.append {%equation}

  ' Equation H.4.73
  %equation = "Verif_BUIL = 0"

  For %hous {%list_household}
    %equation = %equation + "+ BUIL_verif_"+%hous+" - BUIL_"+%hous
    ''       %equation = %equation + "+ (BUIL_verif_"+%hous+" - BUIL_"+%hous+")/buil"
  next

  {%modelname}.append {%equation}

  ' Equation H.4.74
  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(REHAB_{%hous}_{%class},%baseyear) <> 0 then
        {%modelname}.append verif_phi_REHAB_{%hous}_{%class} = sum_phi_REHAB_{%hous}_{%class} - 1
      endif
    next
  next

  '*********************************************************************************************************************************
  '*****************************************************TRANSPORTS******************************************************************

  '-----------------------------------------------------AUTOMOBILE-----------------------------------------------------------------''
  ' Equation H.4.77  '
  For %hous {%list_household}
    For %class {%list_ener_class}
      {%modelname}.append d(AUTO_{%hous}_{%class}) = (@year>{%baseyear})*(phi_NewAUTO_{%hous}_{%class}*(d(AUTO_{%hous})+AUTO_{%hous}_DES) -  delta_AUTO_{%hous}_{%class}_DES*AUTO_{%hous}_{%class}(-1))

    next
  next

  ' Equation H.4.78  '
  For %hous {%list_household}
    %equation = "AUTO_"+%hous+"_DES = 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ delta_AUTO_"+%hous+"_"+%class+"_DES*AUTO_"+%hous+"_"+%class+"(-1)"
    next
    {%modelname}.append {%equation}
  next


  ' Equation H.4.79 H.4.80 '
  For %hous {%list_household}
    For %class {%list_ener_class}
      ''      {%modelname}.append NewAUTO_{%hous}_{%class} = phi_NewAUTO_{%hous}_{%class}*(d(AUTO_{%hous})+AUTO_{%hous}_DES)
      {%modelname}.append NewAUTO_{%hous}_{%class} = (@year>{%baseyear})*phi_NewAUTO_{%hous}_{%class}*(d(AUTO_{%hous})+AUTO_{%hous}_DES) _
      + (@year<={%baseyear})*NewAUTO_{%hous}_{%class}(-1)
      {%modelname}.append d(log(PNewAUTO_{%hous}_{%class})) = d(log(PCH_03))
      'gelprix '{%modelname}.append d(log(PNewAUTO_{%hous}_{%class})) = (@year>{%baseyear})*d(log(PCH_03))
    next
  next


  ' Equation H.4.81  '
  For %hous {%list_household}
    For %class {%list_ener_class}
      'gelvol '{%modelname}.append EXP_MOBAUTO_Val_{%hous}_{%class}= DEBT_AUTO_Val_{%hous}_{%class}(-1)*(R_I_AUTO_{%hous}_{%class}(-1)+R_RMBS_AUTO_{%hous}_{%class}(-1))  + R_CASH_AUTO_{%hous}_{%class}*PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class}) _
      'gelvol '+ PEXP_AUTO_{%hous}_{%class}*EXP_AUTO_{%hous}_{%class}

      {%modelname}.append EXP_MOBAUTO_Val_{%hous}_{%class}= (@year>{%baseyear})*(DEBT_AUTO_Val_{%hous}_{%class}(-1)*(R_I_AUTO_{%hous}_{%class}(-1)+R_RMBS_AUTO_{%hous}_{%class}(-1))  + R_CASH_AUTO_{%hous}_{%class}*PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class}) _
      + PEXP_AUTO_{%hous}_{%class}*EXP_AUTO_{%hous}_{%class}) _
      + (@year=<{%baseyear})*EXP_MOBAUTO_Val_{%hous}_{%class}(-1)*(1+STEADYSTATE(1,1))

    next
  next

  ' Equation H.4.82  Cout d usage auto UC_auto '

  For %hous {%list_household}

    For %class {%list_ener_class}
      {%modelname}.append UC_AUTO_{%hous}_{%class} = UC_K_AUTO_{%hous}_{%class} + UC_E_AUTO_{%hous}_{%class}

      {%modelname}.append UC_K_AUTO_{%hous}_{%class}  =  PNewAUTO_{%hous}_{%class}/AUTO_D_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class})*(R_CASH_AUTO_{%hous}_{%class}+R_LOAN_AUTO_{%hous}_{%class}*R_I_AUTO_{%hous}_{%class}(-1)*LD_AUTO_{%hous}_{%class}/(1-(1+R_I_AUTO_{%hous}_{%class}(-1))^(-LD_AUTO_{%hous}_{%class})))

      {%modelname}.append  R_LOAN_AUTO_{%hous}_{%class} = 1 - R_CASH_AUTO_{%hous}_{%class}

      {%modelname}.append UC_E_AUTO_{%hous}_{%class} = PENER_auto_{%hous}_{%class}*((GR_PENER_auto_e_{%hous}_{%class}=<0.00001)*1+(GR_PENER_auto_e_{%hous}_{%class}>0.00001)*((1+GR_PENER_auto_e_{%hous}_{%class})^(AUTO_D_{%hous}_{%class})-1) _
      /((GR_PENER_auto_e_{%hous}_{%class}+(GR_PENER_auto_e_{%hous}_{%class}=0)*0.0000001)*AUTO_D_{%hous}_{%class}))

      {%modelname}.append GR_PENER_auto_e_{%hous}_{%class} = ADJUST(73,2)*GR_PENER_auto_e_{%hous}_{%class}(-1) + (1-ADJUST(73,2))*@pchy(PENER_auto_{%hous}_{%class}(-1))

    next
  next


  For %hous {%list_household}
    For %class {%list_ener_class}

      {%modelname}.append d(phi_NewAUTO_N_{%hous}_{%class}) = (@year>{%baseyear})*d(phi_NewAUTO_trend_{%hous}_{%class} _
      - nu_auto_{%hous}_{%class}*(UC_E_AUTO_{%hous}_{%class} - UC_E_AUTO_Mean_{%hous})/(P*@elem(UC_E_AUTO_Mean_{%hous},%baseyear)))

    next
  next


  For %hous {%list_household}

    For %class {%list_ener_class}

      {%modelname}.append phi_NewAUTO_N2_{%hous}_{%class} =    phi_NewAUTO_L_{%hous}_{%class}*(phi_NewAUTO_N_{%hous}_{%class}=<phi_NewAUTO_L_{%hous}_{%class}) _
      + phi_NewAUTO_H_{%hous}_{%class}*(phi_NewAUTO_N_{%hous}_{%class}>=phi_NewAUTO_H_{%hous}_{%class}) _
      + phi_NewAUTO_N_{%hous}_{%class}*(phi_NewAUTO_N_{%hous}_{%class}>phi_NewAUTO_L_{%hous}_{%class} AND phi_NewAUTO_N_{%hous}_{%class}<phi_NewAUTO_H_{%hous}_{%class})

      {%modelname}.append phi_NewAUTO_{%hous}_{%class} =    phi_NewAUTO_N2_{%hous}_{%class}/Sum_phi_NewAUTO_N2_{%hous}
    next

  next

  For %hous {%list_household}
    %equation = "Sum_phi_NewAUTO_"+%hous+"= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ phi_NewAUTO_"+%hous+"_"+%class
    next
    {%modelname}.append  {%equation}
  next

  For %hous {%list_household}
    %equation = "Sum_phi_NewAUTO_N2_"+%hous+"= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ phi_NewAUTO_N2_"+%hous+"_"+%class
    next
    {%modelname}.append  {%equation}
  next

  For %hous {%list_household}
    %equation = "UC_E_AUTO_"+%hous+"*AUTO_"+%hous+"= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ UC_E_AUTO_"+%hous+"_"+%class+"*AUTO_"+%hous+"_"+%class
    next
    {%modelname}.append  {%equation}
  next

  For %hous {%list_household}
    %equation = "UC_E_AUTO_Mean_"+%hous+"*7= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ UC_E_AUTO_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  For %hous {%list_household}
    For %class {%list_ener_class}
      if @elem(AUTO_{%hous}_{%class},%baseyear) <> 0 then
        {%modelname}.append PENER_AUTO_{%hous}_{%class}*KM_AUTO_{%hous}_{%class} = PEXP_AUTO_{%hous}_{%class}*EXP_AUTO_{%hous}_{%class}
      endif
    next
  next

  ' Equation H.4.83  '
  For %hous {%list_household}
    For %class {%list_ener_class}
      'gelvol '{%modelname}.append d(DEBT_auto_Val_{%hous}_{%class}) = (@year>{%baseyear})*(-R_RMBS_AUTO_{%hous}_{%class}*DEBT_auto_Val_{%hous}_{%class}(-1) + R_LOAN_AUTO_{%hous}_{%class}*PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class}))
      'gelvol '''      {%modelname}.append DEBT_auto_Val_{%hous}_{%class} = DEBT_auto_Val_{%hous}_{%class}(-1)*(1-R_RMBS_AUTO_{%hous}_{%class}) + R_LOAN_AUTO_{%hous}_{%class}*PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class})

      {%modelname}.append DEBT_auto_Val_{%hous}_{%class} = (@year>{%baseyear})*(DEBT_auto_Val_{%hous}_{%class}(-1)*(1-R_RMBS_AUTO_{%hous}_{%class}) + R_LOAN_AUTO_{%hous}_{%class}*PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}*(1-R_SUB_AUTO_{%hous}_{%class})) _
      + (@year=<{%baseyear})*DEBT_auto_Val_{%hous}_{%class}(-1)*(1+STEADYSTATE(1,1))
    next
  next


  'Equation H.4.84  '
  For %hous {%list_household}
    For %class {%list_ener_class}
      {%modelname}.append EXP_NEWAUTO_VAL_{%hous}_{%class}= PNewAUTO_{%hous}_{%class}*NewAUTO_{%hous}_{%class}
    next
  next

  'Equation H.4.85  H.4.86  H.4.87 H.4.88'
  For %hous {%list_household}
    {%modelname}.append d(log(Km_traveler_{%hous})) = (@year>{%baseyear})*d(log(POP_TOT))
    {%modelname}.append d(log(Km_traveler_auto_{%hous})) = (@year>{%baseyear})*d(log(Km_traveler_{%hous}))
    {%modelname}.append d(log(km_AUTO_{%hous}))= (@year>{%baseyear})*d(log(Km_traveler_auto_{%hous}))
    {%modelname}.append d(log(Auto_{%hous})) = (@year>{%baseyear})*d(log(Km_auto_{%hous}))

    ' Equation H.4.89 H.4.90
    For %class {%list_ener_class}

      {%modelname}.append dlog(km_AUTO_{%hous}_{%class}) = (@year>{%baseyear})*dlog(km_AUTO_{%hous}*(AUTO_{%hous}_{%class}/Auto_{%hous}))

      For %ener {%list_com_E}
        if @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          {%modelname}.append d(log(EXP_AUTO_{%hous}_{%class}_{%ener})) = d(log(Km_auto_{%hous}_{%class}))
        endif
      next
    next
  next

  '-------------------------------------------Aggregation of automobile expenditure--------------------------------------------'
  ' Equation H.4.91
  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "EXP_AUTO_"+%hous+"_"+%class+"= 0"
      For %ener {%list_com_E}
        if @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+EXP_AUTO_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append  {%equation}
    next
  next


  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "PEXP_AUTO_"+%hous+"_"+%class+"*EXP_AUTO_"+%hous+"_"+%class+"= 0"
      For %ener {%list_com_E}
        if @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+ PEXP_"+%ener+"_"+%hous+"*EXP_AUTO_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append  {%equation}
    next
  next



  ' Equation H.4.92
  For %hous {%list_household}
    %equation = "EXP_AUTO_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(EXP_AUTO_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+EXP_AUTO_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append  {%equation}
  next

  For %hous {%list_household}
    %equation = "PEXP_AUTO_"+%hous+"*EXP_AUTO_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(EXP_AUTO_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+PEXP_AUTO_"+%hous+"_"+%class+"*EXP_AUTO_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append  {%equation}
  next


  For %ener {%list_com_E}
    %equation = "EXP_AUTO_"+%ener+"= 0"
    For %class {%list_ener_class}
      if @elem(EXP_AUTO_{%class}_{%ener},%baseyear) <> 0 then
        %equation = %equation + "+EXP_AUTO_"+%class+"_"+%ener
      endif
    next
    {%modelname}.append {%equation}
  next


  ' Equation H.4.93
  For %hous {%list_household}
    For %ener {%list_com_E}
      %equation = "EXP_AUTO_"+%hous+"_"+%ener+"= 0"
      For %class {%list_ener_class}
        if @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+EXP_AUTO_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append  {%equation}
    next
  next


  For %hous {%list_household}
    For %ener {%list_com_E}

      'gelprix '{%modelname}.append dlog(PEXP_AUTO_{%hous}_{%ener}) = (@year>{%baseyear})*dlog(PCH_{%ener})
      {%modelname}.append dlog(PEXP_AUTO_{%hous}_{%ener}) = dlog(PCH_{%ener})

      ' EQUIVALENT à ci-dessous mais évite les erreurs sur le add factors'
      'if @elem(EXP_AUTO_{%hous}_{%ener},%baseyear) <> 0 then
      '%equation = "PEXP_AUTO_"+%hous+"_"+%ener+"*EXP_AUTO_"+%hous+"_"+%ener+"= 0"
      'For %class {%list_ener_class}
      'if @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
      '%equation = %equation + "+ PCH_"+%ener+"*EXP_AUTO_"+%hous+"_"+%class+"_"+%ener
      'endif
      'next
      '{%modelname}.append  {%equation}
      'endif
    next
  next


  ' AJT' Equation H.4.94
  ' AJTFor %class {%list_ener_class}
  ' AJTFor %ener {%list_com_E}
  ' AJT%equation = "EXP_AUTO_"+%class+"_"+%ener+"= 0"
  ' AJTFor %hous {%list_household}
  ' AJTif @elem(EXP_AUTO_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
  ' AJT%equation = %equation + "+EXP_AUTO_"+%hous+"_"+%class+"_"+%ener
  ' AJTendif
  ' AJTnext
  ' AJT{%modelname}.append  {%equation}
  ' AJTnext
  ' AJTnext

  ' Equation H.4.95
  %equation = "EXP_AUTO= 0"
  For %hous {%list_household}
    %equation = %equation + "+EXP_AUTO_"+%hous
  next
  {%modelname}.append  {%equation}


  %equation = "PEXP_AUTO*EXP_AUTO= 0"
  For %hous {%list_household}
    %equation = %equation + "+PEXP_AUTO_"+%hous+"*EXP_AUTO_"+%hous
  next
  {%modelname}.append  {%equation}


  '-----------------------------------------------------------AGGREGATION AUTOMOBILE---------------------------------------------------'
  ' Equation H.4.96
  For %class {%list_ener_class}
    %equation = "AUTO_"+%class+" = 0"
    For %hous {%list_household}
      %equation = %equation + "+ AUTO_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.97
  %equation = "AUTO = 0"
  For  %class {%list_ener_class}
    %equation = %equation + "+ AUTO_"+%class
  next
  {%modelname}.append {%equation}

  ' Equation H.4.98
  For %hous {%list_household}
    %equation = "NewAUTO_"+%hous+"= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ NewAUTO_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.99
  For %hous {%list_household}
    %equation = "PNewAUTO_"+%hous+"*NewAUTO_"+%hous+" = 0"
    For %class {%list_ener_class}
      if @elem(NewAUTO_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ PNewAUTO_"+%hous+"_"+%class+"*NewAUTO_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.100
  For %hous {%list_household}
    %equation = "EXP_NEWAUTO_VAL_"+%hous+"= 0"
    For %class {%list_ener_class}
      %equation = %equation + "+ EXP_NewAUTO_VAL_"+%hous+"_"+%class
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.101
  %equation = "EXP_NEWAUTO_VAL= 0"
  For %hous {%list_household}
    if @elem(EXP_NEWAUTO_VAL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_NEWAUTO_VAL_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.102
  For %hous {%list_household}
    {%modelname}.append phi_exp_03bis_{%hous}=   EXP_NEWAUTO_VAL_{%hous}/EXP_NEWAUTO_VAL
  next

  ' Equation H.4.103
  For %hous {%list_household}
    %equation = "EXP_MOBAUTO_Val_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(EXP_MOBAUTO_Val_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ EXP_MOBAUTO_Val_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.104
  %equation = "EXP_MOBAUTO_Val= 0"
  For %hous {%list_household}
    if @elem(EXP_MOBAUTO_Val_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_MOBAUTO_Val_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.105
  %equation = "EXP_03_OTH_VAL= 0"
  For %hous {%list_household}
    if @elem(EXP_03_OTH_VAL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_03_OTH_VAL_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.106
  %equation = "EXP_03= 0"
  For %hous {%list_household}
    if @elem(EXP_03_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ EXP_03_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.107  H.4.108
  For %hous {%list_household}
    {%modelname}.append d(log(EXP_03_OTH_VAL_{%hous})) = (d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))
    'gelvol '{%modelname}.append d(log(EXP_03_OTH_VAL_{%hous})) = (@year>{%baseyear})*(d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))
    'gelvol '{%modelname}.append EXP_03_{%hous} = @elem(PNewAUTO_{%hous},%baseyear)*NewAUTO_{%hous} +  EXP_03_OTH_val_{%hous}/PEXP_03_{%hous}
    {%modelname}.append EXP_03_{%hous} = (@year>{%baseyear})*(@elem(PNewAUTO_{%hous},%baseyear)*NewAUTO_{%hous} +  EXP_03_OTH_val_{%hous}/PEXP_03_{%hous}) _
    + (@year=<{%baseyear})*EXP_03_{%hous}(-1)*(1+STEADYSTATE(2,1))

  next




  '------------------------------------------------------TRANSPORT COLLECTIF-----------------------------------------------------------------''
  ' Equation H.4.113
  For %hous {%list_household}
    %equation = "EXP_MOB_OTH_Val_"+%hous+"= 0"
    For %com {%list_trsp_travel}
      %equation = %equation+"+PEXP_"+%com+"_"+%hous+"*EXP_"+%com+"_"+%hous
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.114   H.4.115
  For %trsp {%list_trsp_travel}
    For %hous {%list_household}
      if @elem(Km_traveler_{%trsp}_{%hous},%baseyear) <> 0 then
        {%modelname}.append  d(log(Km_traveler_{%trsp}_{%hous})) = d(log(Km_traveler_{%hous}))
        'gelvol '{%modelname}.append d(log(EXP_{%trsp}_{%hous})) = (@year>{%baseyear})*d(log(Km_traveler_{%trsp}_{%hous}))
        {%modelname}.append d(log(EXP_{%trsp}_{%hous})) = (@year>{%baseyear})*d(log(Km_traveler_{%trsp}_{%hous})) + (@year=<{%baseyear})*log(1+STEADYSTATE(2,1))
      endif
    next
  next

  ' Equation H.4.116
  For %trsp {%list_trsp_travel}
    %equation = "EXP_"+%trsp+"= 0"
    For %hous {%list_household}
      %equation = %equation + "+ EXP_"+%trsp+"_"+%hous
    next
    {%modelname}.append {%equation}
  next

  '---------------------------------------------------------Total mobility---------------------------------------------------'
  ' Equation H.4.117
  For %hous {%list_household}
    {%modelname}.append EXP_MOB_Val_{%hous} = EXP_MOBAuto_Val_{%hous} + EXP_MOB_OTH_Val_{%hous} + EXP_03_OTH_VAL_{%hous}
  next

  '****************************************************************************************************************************************
  '***************************************************************ENERGIE******************************************************************
  '---------------------------------------ENERGY CONSUMPTION IN BUILDING---------------------------------------------'

  ' Equation H.4.118 H.4.119 H.4.120  '
  For %hous {%list_household}
    For %class {%list_ener_class}
      For %ener {%list_com_E}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          {%modelname}.append ENER_BUIL_{%hous}_{%class}_{%ener} = ENERperM2_{%hous}_{%class}_{%ener}*BUIL_{%hous}_{%class}

          {%modelname}.append d(log(ENERperM2_{%hous}_{%class}_{%ener})) = 0 ' PROBLEME! Ajouter sobriété, reglementation thermique, substitution inter energie '

          {%modelname}.append d(log(EXP_BUIL_{%hous}_{%class}_{%ener})) = d(log(ENER_BUIL_{%hous}_{%class}_{%ener}))

        endif
      next
    next
  next

  ' Equation H.4.121
  For %ener {%list_com_E}
    For %hous {%list_household}
      For %class {%list_ener_class}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          {%modelname}.append PENER_BUIL_{%hous}_{%class}_{%ener}*ENER_BUIL_{%hous}_{%class}_{%ener}=PEXP_{%ener}_{%hous}*EXP_BUIL_{%hous}_{%class}_{%ener}         'donne le prix en million d'euros du kwH de l'energie concerné'
        endif
      next
    next
  next

  '------------AGGREGATION CONSOMMATION D'ENERGIE EN KwH ET EN MILLION D'EURO'
  ' Equation H.4.122
  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "PENER_BUIL_"+%hous+"_"+%class+"*ENER_BUIL_"+%hous+"_"+%class+"= 0"
      For %ener {%list_com_E}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+ PENER_BUIL_"+%hous+"_"+%class+"_"+%ener+"*ENER_BUIL_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append {%equation}
    next
  next

  ' Equation H.4.123
  For %hous {%list_household}
    For %class {%list_ener_class}
      %equation = "ENER_BUIL_"+%hous+"_"+%class+"= 0"
      For %ener {%list_com_E}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+ ENER_BUIL_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append {%equation}
    next
  next

  '*****************************
  ' Equation H.4.124
  For %hous {%list_household}
    %equation = "PENER_BUIL_"+%hous+"*ENER_BUIL_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(ENER_BUIL_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ PENER_BUIL_"+%hous+"_"+%class+"*ENER_BUIL_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.125
  For %hous {%list_household}
    %equation = "ENER_BUIL_"+%hous+"= 0"
    For %class {%list_ener_class}
      if @elem(ENER_BUIL_{%hous}_{%class},%baseyear) <> 0 then
        %equation = %equation + "+ ENER_BUIL_"+%hous+"_"+%class
      endif
    next
    {%modelname}.append {%equation}
  next

  '*****************************
  ' Equation H.4.126
  %equation = "PENER_BUIL*ENER_BUIL= 0"
  For %hous {%list_household}
    if @elem(ENER_BUIL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ PENER_BUIL_"+%hous+"*ENER_BUIL_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  ' Equation H.4.127
  %equation = "ENER_BUIL= 0"
  For %hous {%list_household}
    if @elem(ENER_BUIL_{%hous},%baseyear) <> 0 then
      %equation = %equation + "+ENER_BUIL_"+%hous
    endif
  next
  {%modelname}.append {%equation}

  '*****************************
  ' Equation H.4.128
  For %hous {%list_household}
    For %ener {%list_com_E}
      %equation = "PENER_BUIL_"+%hous+"_"+%ener+"*ENER_BUIL_"+%hous+"_"+%ener+"= 0"
      For %class {%list_ener_class}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+ PENER_BUIL_"+%hous+"_"+%class+"_"+%ener+"*ENER_BUIL_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append {%equation}
    next
  next

  ' Equation H.4.129
  For %hous {%list_household}
    For %ener {%list_com_E}
      %equation = "ENER_BUIL_"+%hous+"_"+%ener+"= 0"
      For %class {%list_ener_class}
        if @elem(ENER_BUIL_{%hous}_{%class}_{%ener},%baseyear) <> 0 then
          %equation = %equation + "+ ENER_BUIL_"+%hous+"_"+%class+"_"+%ener
        endif
      next
      {%modelname}.append {%equation}
    next
  next

  '*****************************
  ' Equation H.4.130
  For %ener {%list_com_E}
    %equation = "PENER_BUIL_"+%ener+"*ENER_BUIL_"+%ener+"= 0"
    For %hous {%list_household}
      if @elem(ENER_BUIL_{%hous}_{%ener},%baseyear) <> 0 then
        %equation = %equation + "+ PENER_BUIL_"+%hous+"_"+%ener+"*ENER_BUIL_"+%hous+"_"+%ener
      endif
    next
    {%modelname}.append {%equation}
  next

  ' Equation H.4.131
  For %ener {%list_com_E}
    %equation = "ENER_BUIL_"+%ener+"= 0"
    For %hous {%list_household}
      if @elem(ENER_BUIL_{%hous}_{%ener},%baseyear) <> 0 then
        %equation = %equation + "+ ENER_BUIL_"+%hous+"_"+%ener
      endif
    next
    {%modelname}.append {%equation}
  next


  '----AGGREGATION Of total energie expenditure (automobile +building)
  ' Equation H.4.132 H.4.133
  For %hous {%list_household}
    For %class {%list_ener_class}

      'eq en volume'
      {%modelname}.append ENER_{%hous}_{%class}= @elem(PENER_BUIL_{%hous}_{%class},%baseyear)*ENER_BUIL_{%hous}_{%class} +  EXP_AUTO_{%hous}_{%class}
      'eq en valeur'
      {%modelname}.append PENER_{%hous}_{%class}*ENER_{%hous}_{%class}= PENER_BUIL_{%hous}_{%class}*ENER_BUIL_{%hous}_{%class} +  PEXP_AUTO_{%hous}_{%class}*EXP_AUTO_{%hous}_{%class}

    next
  next

  ' Equation H.4.134 H.4.135 H.4.136
  For %hous {%list_household}
    {%modelname}.append ENER_{%hous}=@elem(PENER_BUIL_{%hous},%baseyear)*ENER_BUIL_{%hous} +EXP_AUTO_{%hous}
    {%modelname}.append PENER_{%hous}*ENER_{%hous}= PENER_BUIL_{%hous}*ENER_BUIL_{%hous} + PEXP_AUTO_{%hous}*EXP_AUTO_{%hous}
  next


  {%modelname}.append PENER*ENER= PENER_BUIL*ENER_BUIL + PEXP_AUTO*EXP_AUTO
  {%modelname}.append ENER= @elem(PENER_BUIL,%baseyear)*ENER_BUIL + EXP_AUTO


  '--------aggregation dépenses d'énergie par source'
  ' Equation H.4.137
  For %ener {%list_com_E}
    For %hous {%list_household}
      {%modelname}.append EXP_{%ener}_{%hous}=@elem(PENER_BUIL_{%hous}_{%ener},%baseyear)*ENER_BUIL_{%hous}_{%ener}+EXP_AUTO_{%hous}_{%ener}

    next
  next

  ' Equation H.4.138
  'gelvol 'For %ener {%list_com_E}
  'gelvol '%equation = "EXP_"+%ener+"= 0"
  'gelvol 'For %hous {%list_household}
  'gelvol 'if @elem(EXP_{%ener}_{%hous},%baseyear) <> 0 then
  'gelvol '%equation = %equation + "+ EXP_"+%ener+"_"+%hous
  'gelvol 'endif
  'gelvol 'next
  'gelvol '{%modelname}.append {%equation}
  'gelvol 'next

  For %ener {%list_com_E}
    %equation = "EXP_"+%ener+"= (0"
    For %hous {%list_household}
      if @elem(EXP_{%ener}_{%hous},%baseyear) <> 0 then
        %equation = %equation + "+ EXP_"+%ener+"_"+%hous
      endif
    next
    {%modelname}.append {%equation} )*(@year>{%baseyear}) + (@year=<{%baseyear})*EXP_{%ener}(-1)*(1+STEADYSTATE(2,1))
  next

  '-----------------------------------------------Households expenditures --------------------------------'
  'equation H.4.139
  For %hous {%list_household}
    For %com {%list_com_oth}
      if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
        {%modelname}.append EXP_n_{%com}_{%hous} = (@year>{%baseyear})*(PEXP_{%com}_{%hous}*NEXP_{%com}_{%hous}+BetaExp_{%com}_{%hous}*(DISPINC_VAL_{%hous}*(1 - MPS_HH_{%hous})-PNEXP_{%hous}*NEXP_{%hous}-EXP_HOUSING_Val_{%hous}-Exp_13_OTH_VAL_{%hous}-EXP_MOB_Val_{%hous}))/PEXP_{%com}_{%hous} _
        + (@year<={%baseyear})*EXP_n_{%com}_{%hous}(-1)*(1+STEADYSTATE(2,1))

      endif
    next
  next

  'equation H.4.140
  For %hous {%list_household}
    For %com {%list_com_oth}
      if @elem(BetaExp_{%com}_{%hous},%baseyear) <> 0 then
        {%modelname}.append BetaExp_{%com}_{%hous}=@elem((EXP_{%com}_{%hous}*PEXP_{%com}_{%hous}-PEXP_{%com}_{%hous}*NEXP_{%com}_{%hous})/((DISPINC_VAL_{%hous}*(1 - MPS_HH_{%hous})-PNEXP_{%hous}*NEXP_{%hous}-EXP_HOUSING_Val_{%hous}-Exp_13_OTH_VAL_{%hous}-EXP_MOB_Val_{%hous})),%baseyear)
        '{%modelname}.append d(log(BetaExp_{%com}_{%hous})) = (1-ES_LES_CES(1,1))*d(log(PEXP_{%com}_{%hous}/PEXP_CES_{%hous}))
      endif
    next

    'equation H.4.141
    %equation = "PEXP_CES_"+%hous+"= ("
    For %com {%list_com_oth}
      %equation = %equation+ "+@elem(BetaExp_"+%com+"_"+%hous+","+%baseyear+")*PEXP_"+%com+"_"+%hous+"^(1-ES_LES_CES(1,1))"
    next
    %equation = %equation+")^(1/(1-ES_LES_CES(1,1)))"

    {%modelname}.append {%equation}
  next

  'equation H.4.142
  For %hous {%list_household}
    if @elem(NEXP_{%hous},%baseyear) <> 0 then
      %equation = "PNEXP_"+%hous+"*NEXP_"+%hous+"= 0"
      For %com {%list_com_oth}
        %equation = %equation+"+PEXP_"+%com+"_"+%hous+"*NEXP_"+%com+"_"+%hous
      next
      {%modelname}.append {%equation}
    endif
  next

  'equation H.4.143
  For %hous {%list_household}
    %equation = "NEXP_"+%hous+"= 0"
    For %com {%list_com_oth}
      %equation = %equation+"+NEXP_"+%com+"_"+%hous
    next
    {%modelname}.append {%equation}
  next

  'equation H.4.144 PROBLEM total expenditure by producr c pexp_com.exp_com est dans le lyx mais pas ici, pourquoi?

  'equation H.4.145
  For %com {%list_com_oth}
    %equation = "EXP_"+%com+"=0"
    For %hous {%list_household}
      if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
        %equation = %equation+"+EXP_"+%com+"_"+%hous
      endif
    next
    {%modelname}.append {%equation}
  next

  'equation H.4.146 PROBLEM! Why this equation?
  For %com {%list_com_oth}
    For %hous {%list_household}
      if @elem(EXP_{%com},%baseyear) <> 0 then
        {%modelname}.append Phi_EXP_{%com}_{%hous}= (EXP_{%com}_{%hous})/(EXP_{%com})
      endif
    next
  next


  'equation H.4.147
  For %hous {%list_household}
    For %com {%list_com_oth}
      {%modelname}.append PEXP_{%com}_{%hous} = PCH_{%com}
    next

    For %com 03 13 {%list_trsp_travel} {%list_com_E}
      {%modelname}.append dlog(PEXP_{%com}_{%hous}) = dlog(PCH_{%com})
      'gelprix '{%modelname}.append dlog(PEXP_{%com}_{%hous}) = (@year>{%baseyear})*dlog(PCH_{%com})
    next


  next


  For %com {%list_com_oth}
    ''      {%modelname}.append PEXP_{%com} = PCH_{%com}
  next

  For %com 03 13 {%list_trsp_travel} {%list_com_E}
    ''      {%modelname}.append dlog(PEXP_{%com}) = (@year>{%baseyear})*dlog(PCH_{%com})
  next



  ' Ajustment Line HOUSEHOLDS HYBRID---->Ajustement de demande notionnelle de la part des ménages
  !step_1=0

  'equation H.4.150  H.4.151
  For %com {%list_com_oth}
    For %hous {%list_household}
      if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
        {%modelname}.append  log(EXP_{%com}_{%hous}) = ADJUST(21+!step_1,1)*log(EXP_n_{%com}_{%hous}) + (1-ADJUST(21+!step_1,1))*(log(EXP_{%com}_{%hous}(-1))+d(log(EXP_e_{%com}_{%hous})))

        {%modelname}.append d(log(EXP_e_{%com}_{%hous})) = ADJUST(21+!step_1,2)*d(log(EXP_e_{%com}_{%hous}(-1))) + ADJUST(21+!step_1,3)*d(log(EXP_{%com}_{%hous}(-1))) + ADJUST(21+!step_1,4)*d(log(EXP_n_{%com}_{%hous}))
        '+ ADJUST(21+!step_1,5)*d(log(EXP_{%com}_{%hous}(+1)))

      endif
    next
    !step_1=!step_1+1
  next

  'equation H.4.
  For %hous {%list_household}
    'gelvol '%equation = "EXP_OTH_Val_"+%hous+"= (@year>"+%baseyear+")*(0 "
    'gelvol 'For %com {%list_com_oth} 'REMARQUE: 13_OTH provenant des investissement des ménage autres que travaux de rénovation Rajouté au dépenses de consommation courante:EXP_HH_VAL_{%hous})
    'gelvol '%equation = %equation+"+PEXP_"+%com+"_"+%hous+"*EXP_"+%com+"_"+%hous
    'gelvol 'next
    'gelvol '{%modelname}.append {%equation} ) + (@year<={%baseyear})*EXP_OTH_Val_{%hous}(-1)

    %equation = "EXP_OTH_Val_"+%hous+"= 0 "
    For %com {%list_com_oth} 'REMARQUE: 13_OTH provenant des investissement des ménage autres que travaux de rénovation Rajouté au dépenses de consommation courante:EXP_HH_VAL_{%hous})
      %equation = %equation+"+PEXP_"+%com+"_"+%hous+"*EXP_"+%com+"_"+%hous
    next
    {%modelname}.append {%equation}


    {%modelname}.append  EXP_HH_VAL_{%hous} = EXP_HOUSING_Val_{%hous} + EXP_MOB_Val_{%hous} + EXP_OTH_Val_{%hous}+EXP_13_OTH_Val_{%hous}
  next

  'equation H.4.
  For %hous {%list_household}
    %equation = "EXP_"+%hous+"=0"
    For  %com {%list_com_oth}
      if @elem(EXP_{%com}_{%hous},%baseyear) <> 0 then
        %equation = %equation+"+EXP_"+%com+"_"+%hous
      endif
    next
    {%modelname}.append {%equation}
  next

  ' EXP_HH for EXP household hybrid
  %equation = "EXP_HH_Val= 0"
  For %hous {%list_household}
    %equation = %equation+"+EXP_HH_VAL_"+%hous
  next
  {%modelname}.append  {%equation}

  'marginal propension to save

  !step_2 = !step_HH
  For %hous {%list_household}
    {%modelname}.append d(MPS_HH_{%hous})= household_hybrid(52,1+!step_2)*d(UNR_TOT) + household_hybrid(53,1+!step_2)*d(R-infl_FR) + household_hybrid(54,1+!step_2)*d(DEBT_G_VAL/(PGDP*GDP))

    !step_2 = !step_2 + 1
  next


  For %hous {%list_household}

    'gelvol '{%modelname}.append  S_HH_{%hous} = (@year>{%baseyear})*(DISPINC_VAL_{%hous} - EXP_HH_Val_{%hous}) _
    'gelvol '+ (@year<={%baseyear})*S_HH_{%hous}(-1)

    {%modelname}.append  S_HH_{%hous} = (@year>{%baseyear})*(DISPINC_VAL_{%hous} - EXP_HH_Val_{%hous}) _
    + (@year<={%baseyear})*S_HH_{%hous}(-1)*(1+STEADYSTATE(1,1))


    {%modelname}.append  TS_HH_{%hous} = (@year>{%baseyear})*S_HH_{%hous}/DISPINC_VAL_{%hous} _
    + (@year<={%baseyear})*TS_HH_{%hous}(-1)
  next

  'gelvol '{%modelname}.append S_HH = (@year>{%baseyear})*(DISPINC_VAL - EXP_HH_Val) _
  'gelvol '+ (@year<={%baseyear})*S_HH(-1)

  {%modelname}.append S_HH = (@year>{%baseyear})*(DISPINC_VAL - EXP_HH_Val) _
  + (@year<={%baseyear})*S_HH(-1)*(1+STEADYSTATE(1,1))
  {%modelname}.append TS_HH = (@year>{%baseyear})*S_HH/DISPINC_VAL _
  + (@year<={%baseyear})*TS_HH(-1)


  'gelvol '{%modelname}.append d(Stock_S) = (@year>{%baseyear})*S_HH
  {%modelname}.append d(Stock_S) = S_HH

  For %hous {%list_household}
    For %class {%list_ener_class}

      {%modelname}.append  d(R_I_REHAB_{%hous}_{%class}) = d(R)
      {%modelname}.append  d(R_I_NewBUIL_{%hous}_{%class}) = d(R)
      {%modelname}.append  d(R_I_BUIL_{%hous}_{%class}) = d(R)
      {%modelname}.append  d(R_I_AUTO_{%hous}_{%class}) = d(R)

    next
  next



endsub


'***************************************************************************************************************************************
'******************************************** END BLOCK 4.B :CONSUMER HYBRID ****************************************************
'**************************************************************************************************************************************
