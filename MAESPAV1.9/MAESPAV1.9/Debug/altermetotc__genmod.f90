        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:31 2016
        MODULE ALTERMETOTC__genmod
          INTERFACE 
            SUBROUTINE ALTERMETOTC(TOTC,WINDOTC,PAROTC,FBEAMOTC,TAIR,   &
     &TSOIL,WINDAH,RADABV,FBEAM,RH,VPD,VMFD,PRESS)
              REAL(KIND=4) :: TOTC
              REAL(KIND=4) :: WINDOTC
              REAL(KIND=4) :: PAROTC
              REAL(KIND=4) :: FBEAMOTC
              REAL(KIND=4) :: TAIR(300)
              REAL(KIND=4) :: TSOIL(300)
              REAL(KIND=4) :: WINDAH(300)
              REAL(KIND=4) :: RADABV(300,3)
              REAL(KIND=4) :: FBEAM(300,3)
              REAL(KIND=4) :: RH(300)
              REAL(KIND=4) :: VPD(300)
              REAL(KIND=4) :: VMFD(300)
              REAL(KIND=4) :: PRESS(300)
            END SUBROUTINE ALTERMETOTC
          END INTERFACE 
        END MODULE ALTERMETOTC__genmod
