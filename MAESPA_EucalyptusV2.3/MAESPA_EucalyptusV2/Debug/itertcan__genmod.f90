        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:20 2016
        MODULE ITERTCAN__genmod
          INTERFACE 
            SUBROUTINE ITERTCAN(IHOUR,ITERTAIR,ITERTAIRMAX,NUMPNT,      &
     &NOTARGETS,TCAN2,TLEAFTABLE,TAIR,PREVTAIRCAN,VPD,PREVVPDCAN,       &
     &TAIRABOVE,TAIRNEW,VPDNEW)
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: ITERTAIR
              INTEGER(KIND=4) :: ITERTAIRMAX
              INTEGER(KIND=4) :: NUMPNT
              INTEGER(KIND=4) :: NOTARGETS
              REAL(KIND=4) :: TCAN2
              REAL(KIND=4) :: TLEAFTABLE(5000,4000)
              REAL(KIND=4) :: TAIR(96)
              REAL(KIND=4) :: PREVTAIRCAN
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: PREVVPDCAN
              REAL(KIND=4) :: TAIRABOVE
              REAL(KIND=4) :: TAIRNEW
              REAL(KIND=4) :: VPDNEW
            END SUBROUTINE ITERTCAN
          END INTERFACE 
        END MODULE ITERTCAN__genmod
