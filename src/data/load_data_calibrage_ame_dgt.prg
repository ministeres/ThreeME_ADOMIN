subroutine load_calibrage_AME_dgt()

  %calibrage_ame_dgt = ".\..\..\data\calibrations\Calibrage_AME_DGT.xls"
  
  vector(1) vectnb_AME_dgt

  vectnb_AME_dgt.read(a1,s=Baseline_modif_DGT) {%calibrage_ame_dgt} 1
  !exo_AME_DGT=vectnb_AME_dgt(1)
  read(c2,s=Baseline_modif_DGT,t) {%calibrage_ame_dgt} !exo_AME_DGT

 
endsub