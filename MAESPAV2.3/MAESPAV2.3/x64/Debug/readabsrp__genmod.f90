        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:15 2016
        MODULE READABSRP__genmod
          INTERFACE 
            SUBROUTINE READABSRP(UFILE,NOLAY,ABSRP,REFLEC,TRANS,RHOSOLI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: NOLAY
              REAL(KIND=4) :: ABSRP(15,3)
              REAL(KIND=4) :: REFLEC(15,3)
              REAL(KIND=4) :: TRANS(15,3)
              REAL(KIND=4) :: RHOSOLI(3)
            END SUBROUTINE READABSRP
          END INTERFACE 
        END MODULE READABSRP__genmod
