        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:29 2016
        MODULE WRITE_HEADER_INFORMATION__genmod
          INTERFACE 
            SUBROUTINE WRITE_HEADER_INFORMATION(NSPECIES,SPECIESNAMES,  &
     &CTITLE,TTITLE,PTITLE,STITLE,MTITLE,WTITLE,VTITLE,ISMAESPA)
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: SPECIESNAMES(5)
              CHARACTER(*), INTENT(IN) :: CTITLE
              CHARACTER(*), INTENT(IN) :: TTITLE
              CHARACTER(*), INTENT(IN) :: PTITLE
              CHARACTER(*), INTENT(IN) :: STITLE
              CHARACTER(*), INTENT(IN) :: MTITLE
              CHARACTER(*), INTENT(IN) :: WTITLE
              CHARACTER(*), INTENT(IN) :: VTITLE
              LOGICAL(KIND=4) :: ISMAESPA
            END SUBROUTINE WRITE_HEADER_INFORMATION
          END INTERFACE 
        END MODULE WRITE_HEADER_INFORMATION__genmod
