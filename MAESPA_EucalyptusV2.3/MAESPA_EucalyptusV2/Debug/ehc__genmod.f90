        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:24 2016
        MODULE EHC__genmod
          INTERFACE 
            SUBROUTINE EHC(NUMPNT,TU,TD,TOTLAI,XSLOPE,YSLOPE,NAZ,NZEN,  &
     &NECHLAY,DIFZEN,DEXT,DLAI,EXPDIF,LAYER,MLAYER)
              INTEGER(KIND=4) :: NUMPNT
              REAL(KIND=4) :: TU(4000)
              REAL(KIND=4) :: TD(4000)
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: XSLOPE
              REAL(KIND=4) :: YSLOPE
              INTEGER(KIND=4) :: NAZ
              INTEGER(KIND=4) :: NZEN
              INTEGER(KIND=4) :: NECHLAY
              REAL(KIND=4) :: DIFZEN(20)
              REAL(KIND=4) :: DEXT(20)
              REAL(KIND=4) :: DLAI
              REAL(KIND=4) :: EXPDIF
              INTEGER(KIND=4) :: LAYER(4000)
              INTEGER(KIND=4) :: MLAYER(4000)
            END SUBROUTINE EHC
          END INTERFACE 
        END MODULE EHC__genmod
