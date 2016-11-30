        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:21:10 2016
        MODULE PSTRANSPIF__genmod
          INTERFACE 
            SUBROUTINE PSTRANSPIF(IDAY,IHOUR,RDFIPT,TUIPT,TDIPT,RNET,   &
     &WIND,PAR,TAIR,TMOVE,CA,RH,VPD,VMFD,PRESS,JMAX25,IECO,EAVJ,EDVJ,   &
     &DELSJ,VCMAX25,EAVC,EDVC,DELSC,TVJUP,TVJDN,THETA,AJQ,RD0,Q10F,K10F,&
     &RTEMP,DAYRESP,TBELOW,MODELGS,WSOILMETHOD,EMAXLEAF,SOILMOISTURE,   &
     &SMD1,SMD2,WC1,WC2,SOILDATA,SWPEXP,FSOIL,GSMIN,GNIGHT,G0,D0L,GAMMA,&
     &VPDMIN,G1,GK,WLEAF,NSIDES,VPARA,VPARB,VPARC,VFUN,SF,PSIV,ITERMAX, &
     &GSC,ALEAF,RD,ET,FHEAT,TLEAF,GBH,PLANTK,TOTSOILRES,MINLEAFWP,      &
     &WEIGHTEDSWP,KTOT,HMSHAPE,PSIL,ETEST,ETDEFICIT,CI,ISMAESPA,ISNIGHT,&
     &G02,G12,NEWTUZET)
              INTEGER(KIND=4) :: IDAY
              INTEGER(KIND=4) :: IHOUR
              REAL(KIND=4) :: RDFIPT
              REAL(KIND=4) :: TUIPT
              REAL(KIND=4) :: TDIPT
              REAL(KIND=4) :: RNET
              REAL(KIND=4) :: WIND
              REAL(KIND=4) :: PAR
              REAL(KIND=4) :: TAIR
              REAL(KIND=4) :: TMOVE
              REAL(KIND=4) :: CA
              REAL(KIND=4) :: RH
              REAL(KIND=4) :: VPD
              REAL(KIND=4) :: VMFD
              REAL(KIND=4) :: PRESS
              REAL(KIND=4) :: JMAX25
              INTEGER(KIND=4) :: IECO
              REAL(KIND=4) :: EAVJ
              REAL(KIND=4) :: EDVJ
              REAL(KIND=4) :: DELSJ
              REAL(KIND=4) :: VCMAX25
              REAL(KIND=4) :: EAVC
              REAL(KIND=4) :: EDVC
              REAL(KIND=4) :: DELSC
              REAL(KIND=4) :: TVJUP
              REAL(KIND=4) :: TVJDN
              REAL(KIND=4) :: THETA
              REAL(KIND=4) :: AJQ
              REAL(KIND=4) :: RD0
              REAL(KIND=4) :: Q10F
              REAL(KIND=4) :: K10F
              REAL(KIND=4) :: RTEMP
              REAL(KIND=4) :: DAYRESP
              REAL(KIND=4) :: TBELOW
              INTEGER(KIND=4) :: MODELGS
              INTEGER(KIND=4) :: WSOILMETHOD
              REAL(KIND=4) :: EMAXLEAF
              REAL(KIND=4) :: SOILMOISTURE
              REAL(KIND=4) :: SMD1
              REAL(KIND=4) :: SMD2
              REAL(KIND=4) :: WC1
              REAL(KIND=4) :: WC2
              INTEGER(KIND=4) :: SOILDATA
              REAL(KIND=4) :: SWPEXP
              REAL(KIND=4) :: FSOIL
              REAL(KIND=4) :: GSMIN
              REAL(KIND=4) :: GNIGHT
              REAL(KIND=4) :: G0
              REAL(KIND=4) :: D0L
              REAL(KIND=4) :: GAMMA
              REAL(KIND=4) :: VPDMIN
              REAL(KIND=4) :: G1
              REAL(KIND=4) :: GK
              REAL(KIND=4) :: WLEAF
              INTEGER(KIND=4) :: NSIDES
              REAL(KIND=4) :: VPARA
              REAL(KIND=4) :: VPARB
              REAL(KIND=4) :: VPARC
              INTEGER(KIND=4) :: VFUN
              REAL(KIND=4) :: SF
              REAL(KIND=4) :: PSIV
              INTEGER(KIND=4) :: ITERMAX
              REAL(KIND=4) :: GSC
              REAL(KIND=4) :: ALEAF
              REAL(KIND=4) :: RD
              REAL(KIND=4) :: ET
              REAL(KIND=4) :: FHEAT
              REAL(KIND=4) :: TLEAF
              REAL(KIND=4) :: GBH
              REAL(KIND=4) :: PLANTK
              REAL(KIND=4) :: TOTSOILRES
              REAL(KIND=4) :: MINLEAFWP
              REAL(KIND=4) :: WEIGHTEDSWP
              REAL(KIND=4) :: KTOT
              REAL(KIND=4) :: HMSHAPE
              REAL(KIND=4) :: PSIL
              REAL(KIND=4) :: ETEST
              REAL(KIND=4) :: ETDEFICIT
              REAL(KIND=4) :: CI
              LOGICAL(KIND=4) :: ISMAESPA
              LOGICAL(KIND=4) :: ISNIGHT
              REAL(KIND=4) :: G02
              REAL(KIND=4) :: G12
              INTEGER(KIND=4) :: NEWTUZET
            END SUBROUTINE PSTRANSPIF
          END INTERFACE 
        END MODULE PSTRANSPIF__genmod
