        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:22 2016
        MODULE PHYINTERP__genmod
          INTERFACE 
            SUBROUTINE PHYINTERP(IDATE,NODATES,IDATEARR,PARAMTABLE,NOLAY&
     &,NOAGEP,PARAMS)
              INTEGER(KIND=4) :: IDATE
              INTEGER(KIND=4) :: NODATES
              INTEGER(KIND=4) :: IDATEARR(1000)
              REAL(KIND=4) :: PARAMTABLE(1000,15,3)
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEP
              REAL(KIND=4) :: PARAMS(15,3)
            END SUBROUTINE PHYINTERP
          END INTERFACE 
        END MODULE PHYINTERP__genmod