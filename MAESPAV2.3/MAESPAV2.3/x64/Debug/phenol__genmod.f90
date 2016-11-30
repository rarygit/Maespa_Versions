        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:15 2016
        MODULE PHENOL__genmod
          INTERFACE 
            SUBROUTINE PHENOL(IDAY,ISTART,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME&
     &,APP,EXPAN,STOCKING,NOTREES,THRESH_FOLT,FOLT,TOTLAI,NEWCANOPY)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: ISTART
              INTEGER(KIND=4) :: IFLUSH
              REAL(KIND=4) :: DT1
              REAL(KIND=4) :: DT2
              REAL(KIND=4) :: DT3
              REAL(KIND=4) :: DT4
              REAL(KIND=4) :: EXPTIME
              REAL(KIND=4) :: APP
              REAL(KIND=4) :: EXPAN
              REAL(KIND=4) :: STOCKING
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: THRESH_FOLT
              REAL(KIND=4) :: FOLT(5000)
              REAL(KIND=4) :: TOTLAI
              INTEGER(KIND=4) :: NEWCANOPY
            END SUBROUTINE PHENOL
          END INTERFACE 
        END MODULE PHENOL__genmod
