        !COMPILER-GENERATED INTERFACE MODULE: Tue Dec 06 10:14:11 2016
        MODULE READGS__genmod
          INTERFACE 
            SUBROUTINE READGS(UFILE,I,MODELGS,GSREFI,GSMINI,PAR0I,D0I,  &
     &VK1I,VK2I,VPD1I,VPD2I,VMFD0I,GSJAI,GSJBI,T0I,TREFI,TMAXI,SMD1I,   &
     &SMD2I,WC1I,WC2I,SWPEXPI,GNIGHTI,G0TABLEI,G1TABLEI,GKI,NOGSDATES,  &
     &DATESGSI,D0LI,GAMMAI,VPDMINI,WLEAFTABLEI,NSIDESI,SF,PSIV,         &
     &DATESWLEAFI,NOWLEAFDATES,G02TABLEI,G12TABLEI,NEWTUZETI)
              INTEGER(KIND=4) :: UFILE
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: MODELGS
              REAL(KIND=4) :: GSREFI
              REAL(KIND=4) :: GSMINI
              REAL(KIND=4) :: PAR0I
              REAL(KIND=4) :: D0I
              REAL(KIND=4) :: VK1I
              REAL(KIND=4) :: VK2I
              REAL(KIND=4) :: VPD1I
              REAL(KIND=4) :: VPD2I
              REAL(KIND=4) :: VMFD0I
              REAL(KIND=4) :: GSJAI
              REAL(KIND=4) :: GSJBI
              REAL(KIND=4) :: T0I
              REAL(KIND=4) :: TREFI
              REAL(KIND=4) :: TMAXI
              REAL(KIND=4) :: SMD1I
              REAL(KIND=4) :: SMD2I
              REAL(KIND=4) :: WC1I
              REAL(KIND=4) :: WC2I
              REAL(KIND=4) :: SWPEXPI
              REAL(KIND=4) :: GNIGHTI
              REAL(KIND=4) :: G0TABLEI(45)
              REAL(KIND=4) :: G1TABLEI(45)
              REAL(KIND=4) :: GKI
              INTEGER(KIND=4) :: NOGSDATES
              INTEGER(KIND=4) :: DATESGSI(45)
              REAL(KIND=4) :: D0LI
              REAL(KIND=4) :: GAMMAI
              REAL(KIND=4) :: VPDMINI
              REAL(KIND=4) :: WLEAFTABLEI(45)
              INTEGER(KIND=4) :: NSIDESI
              REAL(KIND=4) :: SF
              REAL(KIND=4) :: PSIV
              INTEGER(KIND=4) :: DATESWLEAFI(45)
              INTEGER(KIND=4) :: NOWLEAFDATES
              REAL(KIND=4) :: G02TABLEI(45)
              REAL(KIND=4) :: G12TABLEI(45)
              INTEGER(KIND=4) :: NEWTUZETI
            END SUBROUTINE READGS
          END INTERFACE 
        END MODULE READGS__genmod
