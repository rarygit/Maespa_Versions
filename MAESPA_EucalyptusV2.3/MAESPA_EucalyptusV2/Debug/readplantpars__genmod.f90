        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:22 2016
        MODULE READPLANTPARS__genmod
          INTERFACE 
            SUBROUTINE READPLANTPARS(UFILE,MINROOTWPI,MINLEAFWPI,       &
     &PLANTKTABLEI,KSCALINGI,DATESKPI,NOKPDATES)
              INTEGER(KIND=4) :: UFILE
              REAL(KIND=4) :: MINROOTWPI
              REAL(KIND=4) :: MINLEAFWPI(4)
              REAL(KIND=4) :: PLANTKTABLEI(1000)
              REAL(KIND=4) :: KSCALINGI
              INTEGER(KIND=4) :: DATESKPI(1000)
              INTEGER(KIND=4) :: NOKPDATES
            END SUBROUTINE READPLANTPARS
          END INTERFACE 
        END MODULE READPLANTPARS__genmod
