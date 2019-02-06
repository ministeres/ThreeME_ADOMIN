  '----------------------------------------------------------------------------------------------
  '---------------------------***ENERGY PRODUCTION IN MTEP***---------------------------------

  !step_2=0
  For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    call create_series("Q_Mtep_"+%ene,STEADYSTATE(2,1),MTEP(1+!step_2,5))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    call create_series("Phi_Mtep_"+%ene,0,MTEP(1+!step_2,6))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    call create_series("fac_conv_phi_"+%ene,0,MTEP(1+!step_2,7))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    call create_series("fac_conv_EP_"+%ene,0,MTEP(1+!step_2,9))
    !step_2=!step_2+1
  next


  
  !step_2=0
  For %ene {%list_com_E}
    call create_series("Q_Mtep_H_BUIL_"+%ene,STEADYSTATE(2,1),MTEP(20,1+!step_2))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene {%list_com_E}
    call create_series("Q_Mtep_H_AUTO_"+%ene,STEADYSTATE(2,1),MTEP(21,1+!step_2))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene {%list_com_E}
    call create_series("Q_Mtep_SEC_"+%ene,STEADYSTATE(2,1),MTEP(22,1+!step_2))
    !step_2=!step_2+1
  next

  !step_2=0
  For %ene {%list_com_E}
    call create_series("Q_Mtep_ESEC_"+%ene,STEADYSTATE(2,1),MTEP(23,1+!step_2))
    !step_2=!step_2+1
  next  
