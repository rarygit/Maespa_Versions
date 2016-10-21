        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE SCALEUP__genmod
          INTERFACE 
            SUBROUTINE SCALEUP(IHOUR,USESTAND,NOTARGETS,NOALLTREES,FOLT,&
     &ITARGETS,TOTLAI,STOCKING,SCLOSTTREE,THRAB,RADABV,FH2O,PLOTAREA,   &
     &DOWNTHTREE,RGLOBABV,RGLOBUND,RADINTERC,FRACAPAR,ISIMUS,FH2OUS,    &
     &THRABUS,PARUSMEAN,SCLOSTTOT,GSCAN,WIND,ZHT,Z0HT,ZPD,PRESS,TAIR,VPD&
     &,ETMM,ETUSMM)
              INTEGER(KIND=4) :: IHOUR
              INTEGER(KIND=4) :: USESTAND
              INTEGER(KIND=4) :: NOTARGETS
              INTEGER(KIND=4) :: NOALLTREES
              REAL(KIND=4) :: FOLT(5000)
              INTEGER(KIND=4) :: ITARGETS(5000)
              REAL(KIND=4) :: TOTLAI
              REAL(KIND=4) :: STOCKING
              REAL(KIND=4) :: SCLOSTTREE(5000,3)
              REAL(KIND=4) :: THRAB(5000,300,3)
              REAL(KIND=4) :: RADABV(300,3)
              REAL(KIND=4) :: FH2O(5000,300)
              REAL(KIND=4) :: PLOTAREA
              REAL(KIND=4) :: DOWNTHTREE(5000)
              REAL(KIND=4) :: RGLOBABV
              REAL(KIND=4) :: RGLOBUND
              REAL(KIND=4) :: RADINTERC
              REAL(KIND=4) :: FRACAPAR
              INTEGER(KIND=4) :: ISIMUS
              REAL(KIND=4) :: FH2OUS
              REAL(KIND=4) :: THRABUS
              REAL(KIND=4) :: PARUSMEAN
              REAL(KIND=4) :: SCLOSTTOT
              REAL(KIND=4) :: GSCAN(5000,300)
              REAL(KIND=4) :: WIND
              REAL(KIND=4) :: ZHT
              REAL(KIND=4) :: Z0HT
              REAL(KIND=4) :: ZPD
              REAL(KIND=4) :: PRESS
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: VPD
              REAL(KIND=4) :: ETMM
              REAL(KIND=4) :: ETUSMM
            END SUBROUTINE SCALEUP
          END INTERFACE 
        END MODULE SCALEUP__genmod
