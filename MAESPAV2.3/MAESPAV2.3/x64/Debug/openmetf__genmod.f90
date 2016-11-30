        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:10 2016
        MODULE OPENMETF__genmod
          INTERFACE 
            SUBROUTINE OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,     &
     &USEMEASET,DIFSKY,ALAT,TTIMD,DELTAT,MFLAG,METCOLS,NOMETCOLS,MTITLE,&
     &MSTART,IN_PATH)
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: IEND
              REAL(KIND=4) :: CAK
              REAL(KIND=4) :: PRESSK
              REAL(KIND=4) :: SWMIN
              REAL(KIND=4) :: SWMAX
              INTEGER(KIND=4), INTENT(IN) :: USEMEASET
              REAL(KIND=4) :: DIFSKY
              REAL(KIND=4) :: ALAT
              REAL(KIND=4) :: TTIMD
              REAL(KIND=4) :: DELTAT(12)
              INTEGER(KIND=4) :: MFLAG
              INTEGER(KIND=4) :: METCOLS(21)
              INTEGER(KIND=4) :: NOMETCOLS
              CHARACTER(*), INTENT(INOUT) :: MTITLE
              INTEGER(KIND=4) :: MSTART
              CHARACTER(*), INTENT(IN) :: IN_PATH
            END SUBROUTINE OPENMETF
          END INTERFACE 
        END MODULE OPENMETF__genmod
