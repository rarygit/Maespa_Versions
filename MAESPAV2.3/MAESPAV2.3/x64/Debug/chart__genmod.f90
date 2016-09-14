        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 06 12:06:48 2016
        MODULE CHART__genmod
          INTERFACE 
            SUBROUTINE CHART(XSLOPE,YSLOPE,NAZ,NZEN,NECHLAY,DIFZEN,DEXT,&
     &TOTLAI,TAUMIN,RTA,NLAY,DLAI,EXPDIF)
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              INTEGER(KIND=4) :: NAZ
              INTEGER(KIND=4) :: NZEN
              INTEGER(KIND=4) :: NECHLAY
              REAL(KIND=4) :: DIFZEN(20)
              REAL(KIND=4) :: DEXT(20)
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: TAUMIN
              REAL(KIND=4) :: RTA(150)
              INTEGER(KIND=4) :: NLAY
              REAL(KIND=4) :: DLAI
              REAL(KIND=4) :: EXPDIF
            END SUBROUTINE CHART
          END INTERFACE 
        END MODULE CHART__genmod
