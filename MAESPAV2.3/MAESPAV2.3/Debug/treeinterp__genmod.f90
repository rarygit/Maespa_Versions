        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:47 2016
        MODULE TREEINTERP__genmod
          INTERFACE 
            SUBROUTINE TREEINTERP(IDAY,ISTART,NODATES,IDATEARR,         &
     &PARAMTABLE,NOTREES,PARAMS)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: NODATES
              INTEGER(KIND=4) :: IDATEARR(45)
              REAL(KIND=4) :: PARAMTABLE(45,5000)
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: PARAMS(5000)
            END SUBROUTINE TREEINTERP
          END INTERFACE 
        END MODULE TREEINTERP__genmod
