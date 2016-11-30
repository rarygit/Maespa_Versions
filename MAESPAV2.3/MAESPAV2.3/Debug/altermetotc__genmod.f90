        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:54 2016
        MODULE ALTERMETOTC__genmod
          INTERFACE 
            SUBROUTINE ALTERMETOTC(TOTC,WINDOTC,PAROTC,FBEAMOTC,TAIR,   &
     &TSOIL,WINDAH,RADABV,FBEAM,RH,VPD,VMFD,PRESS)
              REAL(KIND=4) :: TOTC
              REAL(KIND=4) :: WINDOTC
              REAL(KIND=4) :: PAROTC
              REAL(KIND=4) :: FBEAMOTC
              REAL(KIND=4) :: TAIR(96)
              REAL(KIND=4) :: TSOIL(96)
              REAL(KIND=4) :: WINDAH(96)
              REAL(KIND=4) :: RADABV(96,3)
              REAL(KIND=4) :: FBEAM(96,3)
              REAL(KIND=4) :: RH(96)
              REAL(KIND=4) :: VPD(96)
              REAL(KIND=4) :: VMFD(96)
              REAL(KIND=4) :: PRESS(96)
            END SUBROUTINE ALTERMETOTC
          END INTERFACE 
        END MODULE ALTERMETOTC__genmod
