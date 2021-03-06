        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE INPUTCON__genmod
          INTERFACE 
            SUBROUTINE INPUTCON(ISTART,IEND,NSTEP,NUMPNT,NOLAY,PPLAY,   &
     &NZEN,DIFZEN,NAZ,MODELGS,MODELJM,MODELRD,MODELSS,MODELRW,ITERMAX,  &
     &IOHIST,BINSIZE,ICC,CO2INC,TINC,IOTC,TOTC,WINDOTC,PAROTC,FBEAMOTC, &
     &IWATFILE,NSPECIES,SPECIESNAMES,PHYFILES,STRFILES,ITERTAIRMAX,     &
     &NECHLAY)
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: IEND
              INTEGER(KIND=4) :: NSTEP
              INTEGER(KIND=4) :: NUMPNT
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: PPLAY
              INTEGER(KIND=4) :: NZEN
              REAL(KIND=4) :: DIFZEN(20)
              INTEGER(KIND=4) :: NAZ
              INTEGER(KIND=4) :: MODELGS
              INTEGER(KIND=4) :: MODELJM
              INTEGER(KIND=4) :: MODELRD
              INTEGER(KIND=4) :: MODELSS
              INTEGER(KIND=4) :: MODELRW
              INTEGER(KIND=4) :: ITERMAX
              INTEGER(KIND=4) :: IOHIST
              REAL(KIND=4) :: BINSIZE
              INTEGER(KIND=4) :: ICC
              REAL(KIND=4) :: CO2INC
              REAL(KIND=4) :: TINC
              INTEGER(KIND=4) :: IOTC
              REAL(KIND=4) :: TOTC
              REAL(KIND=4) :: WINDOTC
              REAL(KIND=4) :: PAROTC
              REAL(KIND=4) :: FBEAMOTC
              INTEGER(KIND=4) :: IWATFILE
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: SPECIESNAMES(5)
              CHARACTER(LEN=30) :: PHYFILES(5)
              CHARACTER(LEN=30) :: STRFILES(5)
              INTEGER(KIND=4) :: ITERTAIRMAX
              INTEGER(KIND=4) :: NECHLAY
            END SUBROUTINE INPUTCON
          END INTERFACE 
        END MODULE INPUTCON__genmod
