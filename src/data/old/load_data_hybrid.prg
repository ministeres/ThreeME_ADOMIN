' ***********************************************************************************************************************
' ******************************************* HOUSEHOLDS HYBRID****************************************************************************
' ***********************************************************************************************************************8



subroutine load_data_hybrid

  matrix(300,10) HOUSEHOLD_HYBRID_BUIL                          ' Matrix of the data HOUSEHOLD_HYBRID_BUIL
  HOUSEHOLD_HYBRID_BUIL.read(D4,s=Household_hybrid_BUIL) {%data_calibration}

  matrix(280,10) HOUSEHOLD_HYBRID_AUTO                          ' Matrix of the data HOUSEHOLD_HYBRID_AUTO
  HOUSEHOLD_HYBRID_AUTO.read(D4,s=Household_hybrid_AUTO) {%data_calibration}

  matrix(100,10) HOUSEHOLD_HYBRID_Trans                         ' Matrix of the data HOUSEHOLD_HYBRID_Trans
  HOUSEHOLD_HYBRID_Trans.read(D4,s=Household_hybrid_Transition) {%data_calibration}
  
  matrix(23,9) MTEP                     ' Matrix of the energy production features
  MTEP.read(C4,s=Donnees_energie) {%data_calibration}
  
  '----------------------------EMISSIONS From Households---------------------------------------------------------------------------------

  !step_E=0
  For %ems 21 22
    !step_1=!step_HH
    For %hous {%list_household}
      !step_2=0
      For %class {%list_ener_class}
        call create_series("EMS_HH_BUIL_"+%ems+"_"+%hous+"_"+%class,STEADYSTATE(2,1),HOUSEHOLD_HYBRID_BUIL(81+!step_2+!step_E,1+!step_1))  'Consommation en KwH de l'energie '
        !step_2 = !step_2 + 1
      next
      !step_1 = !step_1 + 1
    next
    !step_E=!step_E+8
  next

  !step_1=!step_HH
  For %hous {%list_household}
    !step_2=0
    For %class {%list_ener_class}
      call create_series("EMS_HH_BUIL_24_"+%hous+"_"+%class,STEADYSTATE(2,1),HOUSEHOLD_HYBRID_BUIL(105+!step_2,1+!step_1))  'Consommation en KwH de l'energie '
      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next

  '-----------------------EMISSIONS From Households: automobile

  !step_1=!step_HH
  For %hous {%list_household}
    !step_2=0
    For %class {%list_ener_class}
      call create_series("EMS_HH_AUTO_22_"+%hous+"_"+%class,STEADYSTATE(2,1),HOUSEHOLD_HYBRID_AUTO(224+!step_2,1+!step_1))  'Consommation en KwH de l'energie '
      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next

'*********************************************************************************************************************
  !step_1=!step_HH
  For %hous {%list_household}
    !step_2=0
    For %class {%list_ener_class}
      series  REHAB_D_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(269+!step_2,1+!step_1)
      series  BUIL_D_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(261+!step_2,1+!step_1)
      series  AUTO_D_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(188+!step_2,1+!step_1)
      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next


  series M2perCapita = HOUSEHOLD_HYBRID_BUIL(17,1)

  !step_1=!step_HH
  For %hous {%list_household}

    call create_series("BUIL_"+%hous,0,HOUSEHOLD_HYBRID_BUIL(16,1+!step_1))

    !step_2=0
    For %class {%list_ener_class}

      call create_series("BUIL_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(9+!step_2,1+!step_1))

      call create_series("PNewBUIL_"+%hous+"_"+%class,STEADYSTATE(15,1),HOUSEHOLD_HYBRID_BUIL(152+!step_2,1+!step_1)/1000000)   ' Doit être exprimé en million d'euros
      'gelprix 'call create_series("PNewBUIL_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(152+!step_2,1+!step_1)/1000000)   ' oit être exprimé en million d'euros
      call create_series("phi_NewBUIL_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(18+!step_2,1+!step_1))


      call create_series("tau_REHAB_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(25+!step_2,1+!step_1))
      series tau_REHAB_trend_{%hous}_{%class} = tau_REHAB_{%hous}_{%class}
      series tau_REHAB_N_{%hous}_{%class} = tau_REHAB_{%hous}_{%class}
      series tau_REHAB_N2_{%hous}_{%class} = tau_REHAB_{%hous}_{%class}
      call create_series("tau_REHAB_H_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(221+!step_2,1+!step_1))
      call create_series("tau_REHAB_L_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(214+!step_2,1+!step_1))
      call create_series("nu_REHAB_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_BUIL(207+!step_2,1+!step_1))


      call create_series("nu_auto_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(166+!step_2,1+!step_1))

      series R_CASH_REHAB_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(160+!step_2,1+!step_1)
      series R_I_REHAB_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(167+!step_2,1+!step_1)

      series R_CASH_NewBUIL_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(237+!step_2,1+!step_1)
      series R_I_NewBUIL_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(245+!step_2,1+!step_1)


      series R_I_BUIL_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(192+!step_2,1+!step_1)
      series R_CASH_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(184+!step_2,1+!step_1)



      'gelprix 'series GR_PENER_auto_e_{%hous}_{%class} = 0
      series GR_PENER_auto_e_{%hous}_{%class} = STEADYSTATE(15,1)

      'gelprix '''      series GR_PENER_m2_e_{%hous}_{%class} = 0
      series GR_PENER_m2_e_{%hous}_{%class} = STEADYSTATE(15,1)

      For %class2 {%list_ener_class}
        series  R_SUB_{%hous}_{%class}_{%class2} = HOUSEHOLD_HYBRID_BUIL(229+!step_2,1+!step_1)
      next

      !step_2 = !step_2 + 1
    next

    !step_1 = !step_1 + 1
  next


  !step_E=0
  For %ener {%list_com_E}
    !step_1=!step_HH
    For %hous {%list_household}
      !step_2=0
      For %class {%list_ener_class}
        call create_series("ENER_BUIL_"+%hous+"_"+%class+"_"+%ener,0,HOUSEHOLD_HYBRID_BUIL(41+!step_2+!step_E,1+!step_1))  'Consommation en KwH de l energie '
        !step_2 = !step_2 + 1
      next
      !step_1 = !step_1 + 1
    next
    !step_E=!step_E+8
  next


  !step_E=0
  For %ener {%list_com_E}
    !step_1=!step_HH
    For %hous {%list_household}
      !step_2=0
      For %class {%list_ener_class}

        call create_series("EXP_BUIL_"+%hous+"_"+%class+"_"+%ener,0,HOUSEHOLD_HYBRID_BUIL(121+!step_2+!step_E,1+!step_1))  'Consommation en Million  Euros
        !step_2 = !step_2 + 1
      next
      !step_1 = !step_1 + 1
    next
    !step_E=!step_E+8
  next


  For %hous {%list_household}

    !step_1=0
    For %class {%list_ener_class}

      !step_2=0
      For %class2 {%list_ener_class}
        call create_series("phi_REHAB_"+%hous+"_"+%class+"_"+%class2,0,HOUSEHOLD_HYBRID_Trans(10+!step_1,1+!step_2))
        ' Doubt 'series REHAB_{%hous}_{%class}_{%class2} = phi_REHAB_{%hous}_{%class}_{%class2}*tau_REHAB_{%hous}_{%class}*BUIL_{%hous}_{%class}(-1)
        ' Doubt 'smpl @first  @first
        ' Doubt 'series REHAB_{%hous}_{%class}_{%class2} = @elem(REHAB_{%hous}_{%class}_{%class2},@first+1)
        ' Doubt 'smpl @all

        call create_series("PREHAB_"+%hous+"_"+%class+"_"+%class2,STEADYSTATE(15,1),HOUSEHOLD_HYBRID_Trans(2+!step_1,1+!step_2)/(HOUSEHOLD_HYBRID_BUIL(16,1)/HOUSEHOLD_HYBRID_BUIL(8,1))/1000000)  ' PROBLEME ! Doit être exprimé en million d'euros
        'gelprix 'call create_series("PREHAB_"+%hous+"_"+%class+"_"+%class2,0,HOUSEHOLD_HYBRID_Trans(2+!step_1,1+!step_2)/(HOUSEHOLD_HYBRID_BUIL(16,1)/HOUSEHOLD_HYBRID_BUIL(8,1))/1000000)  ' PROBLEME ! Doit être exprimé en million d'euros

        !step_2 = !step_2 + 1
      next

      !step_3=0
      For %class2 {%list_ener_class} DES
        call create_series("delta_BUIL_"+%hous+"_"+%class+"_"+%class2,0,HOUSEHOLD_HYBRID_Trans(18+!step_1,1+!step_3))
        !step_3 = !step_3 + 1
      next


      !step_1 = !step_1 + 1
    next

  next

  !step_1=!step_HH
  For %hous {%list_household}
    !step_2=0
    For %class {%list_ener_class}


      series  LD_REHAB_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(176+!step_2,1+!step_1)
      series  LD_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(200+!step_2,1+!step_1)

      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next


  '-----AGGREGATION_REHAB EN M2 ET EN MILLION D'EURO'



  !step_1=!step_HH
  For %hous {%list_household}
    !step_2=0
    For %class {%list_ener_class}
    
      series  LD_NewBUIL_{%hous}_{%class} = HOUSEHOLD_HYBRID_BUIL(253+!step_2,1+!step_1)

      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next

 '************************************************AUTOMOBILE BLOC************************************************************************************'
  '****************************************************************************************************************************************************'

  '------------------------------------------------------------------------------------------------------------------------------------------------------
  '-----------------------------------------ENERGY CONSUMPTION LINKED TO AUTO----------------------------------------------------------------------------'

  !step_E=0
  For %ener {%list_com_E}
    !step_1=!step_HH
    For %hous {%list_household}
      !step_2=0
      For %class {%list_ener_class}
        call create_series("EXP_AUTO_"+%hous+"_"+%class+"_"+%ener,0,HOUSEHOLD_HYBRID_AUTO(2+!step_2+!step_E,1+!step_1))  'Consommation en euro  '

        !step_2 = !step_2 + 1
      next
      !step_1 = !step_1 + 1
    next
    !step_E=!step_E+8
  next

  '-----------------------------------------------------------------------------------------------------------------------'
  !step_1=!step_HH
  For %hous {%list_household}

    call create_series("KM_traveler_"+%hous,0,HOUSEHOLD_HYBRID_AUTO(111,1+!step_1))
    call create_series("KM_traveler_AUTO_"+%hous,0,HOUSEHOLD_HYBRID_AUTO(105,1+!step_1))
    call create_series("KM_AUTO_"+%hous,0,HOUSEHOLD_HYBRID_AUTO(72,1+!step_1))


    !step_2=0
    For %class {%list_ener_class}

      call create_series("KM_AUTO_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(65+!step_2,1+!step_1))
      !step_2 = !step_2 + 1
    next

    !step_1=!step_1+1
  next

  !step_1=!step_HH
  For %hous {%list_household}
    !step_trsp=0
    For %trsp {%list_trsp}

      call create_series("KM_traveler_"+%trsp+"_"+%hous, 0,HOUSEHOLD_HYBRID_AUTO(106+!step_trsp,1+!step_1))
      !step_trsp = !step_trsp + 1
    next
    !step_1=!step_1+1
  next
  '----------------------------------------AGGREGATION OF AUTOMOBILE ENERGY CONSUMPTION----------------------------------- '


  '------------------------------------------------------------------------------------------------------------------------------------------------------
  '-----------------------------------------------AUTOMOBILE----------------------------------------------------------------------------'

  !step_1=!step_HH
  For %hous {%list_household}

    call create_series("AUTO_"+%hous,0,HOUSEHOLD_HYBRID_AUTO(40,1+!step_1))
    !step_2=0
    For %class {%list_ener_class}


      call create_series("AUTO_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(33+!step_2,1+!step_1))
      call create_series("phi_NewAUTO_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(112+!step_2,1+!step_1))

      call create_series("phi_NewAUTO_L_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(173+!step_2,1+!step_1))
      call create_series("phi_NewAUTO_H_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(180+!step_2,1+!step_1))

      call create_series("PNewAUTO_"+%hous+"_"+%class,STEADYSTATE(15,1),HOUSEHOLD_HYBRID_AUTO(119+!step_2,1+!step_1)/1000)
      'gelprix 'call create_series("PNewAUTO_"+%hous+"_"+%class,0,HOUSEHOLD_HYBRID_AUTO(119+!step_2,1+!step_1)/1000)

      series R_CASH_AUTO_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(127+!step_2,1+!step_1)
      series R_I_AUTO_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(135+!step_2,1+!step_1)
      series  R_SUB_AUTO_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(151+!step_2,1+!step_1)
      series  R_LOAN_AUTO_{%hous}_{%class} = 1 - R_CASH_AUTO_{%hous}_{%class}
      series  LD_AUTO_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(143+!step_2,1+!step_1)
      series R_RMBS_AUTO_{%hous}_{%class} = HOUSEHOLD_HYBRID_AUTO(159+!step_2,1+!step_1)

      !step_2 = !step_2 + 1
    next
    !step_1 = !step_1 + 1
  next



  For %hous {%list_household}
    !step_1=0
    For %class {%list_ener_class}
      call create_series("delta_AUTO_"+%hous+"_"+%class+"_DES",0,HOUSEHOLD_HYBRID_Trans(26+!step_1,8))
      !step_1 = !step_1 + 1
    next
  next



endsub