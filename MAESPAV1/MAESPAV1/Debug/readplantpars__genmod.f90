        !COMPILER-GENERATED INTERFACE MODULE: Thu Sep 08 12:08:46 2016
        MODULE READPLANTPARS__genmod
          INTERFACE 
            SUBROUTINE READPLANTPARS(UFILE,MINROOTWPI,MINLEAFWPI,       &
     &PLANTKTABLEI,KSCALINGI,DATESKPI,NOKPDATES)
              INTEGER(KIND=4) :: UFILE
              REAL(KIND=4) :: MINROOTWPI
              REAL(KIND=4) :: MINLEAFWPI(4)
              REAL(KIND=4) :: PLANTKTABLEI(30)
              REAL(KIND=4) :: KSCALINGI
              INTEGER(KIND=4) :: DATESKPI(30)
              INTEGER(KIND=4) :: NOKPDATES
            END SUBROUTINE READPLANTPARS
          END INTERFACE 
        END MODULE READPLANTPARS__genmod
