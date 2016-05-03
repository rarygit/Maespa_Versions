        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 28 16:12:48 2016
        MODULE INPUTPHY__genmod
          INTERFACE 
            SUBROUTINE INPUTPHY(NSPECIES,PHYFILES,MODELJM,MODELRD,      &
     &MODELGS,MODELRW,NOLAY,NOAGEC,NOAGEP,PROPC,PROPP,ABSRP,REFLEC,TRANS&
     &,RHOSOL,JMAXTABLE,DATESJ,NOJDATES,IECO,EAVJ,EDVJ,DELSJ,THETA,     &
     &VCMAXTABLE,DATESV,NOVDATES,EAVC,EDVC,DELSC,TVJUP,TVJDN,SLATABLE,  &
     &DATESSLA,NOSLADATES,NOADATES,DATESA,AJQTABLE,RDTABLE,DATESRD,     &
     &NORDATES,RTEMP,DAYRESP,TBELOW,EFFYRW,RMW,RTEMPW,COLLA,COLLK,      &
     &STEMSDW,RMWAREA,STEMFORM,NOFQDATES,DATESFQ,Q10FTABLE,K10F,        &
     &NOWQDATES,DATESWQ,Q10WTABLE,RMFR,RMCR,Q10R,RTEMPR,EFFYRF,RMB,Q10B,&
     &RTEMPB,GSREF,GSMIN,PAR0,D0,VK1,VK2,VPD1,VPD2,VMFD0,GSJA,GSJB,T0,  &
     &TREF,TMAX,SMD1,SMD2,WC1,WC2,SWPEXP,GNIGHT,G0TABLE,G1TABLE,GK,     &
     &NOGSDATES,DATESGS,D0L,GAMMA,VPDMIN,WLEAFTABLE,DATESWLEAF,         &
     &NOWLEAFDATES,NSIDES,SF,PSIV,VPARA,VPARB,VPARC,VFUN,G02TABLE,      &
     &G12TABLE,NEWTUZET,IN_PATH)
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: PHYFILES(4)
              INTEGER(KIND=4) :: MODELJM
              INTEGER(KIND=4) :: MODELRD
              INTEGER(KIND=4) :: MODELGS
              INTEGER(KIND=4) :: MODELRW
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEC(4)
              INTEGER(KIND=4) :: NOAGEP(4)
              REAL(KIND=4) :: PROPC(3,4)
              REAL(KIND=4) :: PROPP(3,4)
              REAL(KIND=4) :: ABSRP(15,3,4)
              REAL(KIND=4) :: REFLEC(15,3,4)
              REAL(KIND=4) :: TRANS(15,3,4)
              REAL(KIND=4) :: RHOSOL(3,4)
              REAL(KIND=4) :: JMAXTABLE(45,15,3,4)
              INTEGER(KIND=4) :: DATESJ(45,4)
              INTEGER(KIND=4) :: NOJDATES(4)
              INTEGER(KIND=4) :: IECO(4)
              REAL(KIND=4) :: EAVJ(4)
              REAL(KIND=4) :: EDVJ(4)
              REAL(KIND=4) :: DELSJ(4)
              REAL(KIND=4) :: THETA(4)
              REAL(KIND=4) :: VCMAXTABLE(45,15,3,4)
              INTEGER(KIND=4) :: DATESV(45,4)
              INTEGER(KIND=4) :: NOVDATES(4)
              REAL(KIND=4) :: EAVC(4)
              REAL(KIND=4) :: EDVC(4)
              REAL(KIND=4) :: DELSC(4)
              REAL(KIND=4) :: TVJUP(4)
              REAL(KIND=4) :: TVJDN(4)
              REAL(KIND=4) :: SLATABLE(45,15,3,4)
              INTEGER(KIND=4) :: DATESSLA(45,4)
              INTEGER(KIND=4) :: NOSLADATES(4)
              INTEGER(KIND=4) :: NOADATES(4)
              INTEGER(KIND=4) :: DATESA(45,4)
              REAL(KIND=4) :: AJQTABLE(45,15,3,4)
              REAL(KIND=4) :: RDTABLE(45,15,3,4)
              INTEGER(KIND=4) :: DATESRD(45,4)
              INTEGER(KIND=4) :: NORDATES(4)
              REAL(KIND=4) :: RTEMP(4)
              REAL(KIND=4) :: DAYRESP(4)
              REAL(KIND=4) :: TBELOW(4)
              REAL(KIND=4) :: EFFYRW(4)
              REAL(KIND=4) :: RMW(4)
              REAL(KIND=4) :: RTEMPW(4)
              REAL(KIND=4) :: COLLA(4)
              REAL(KIND=4) :: COLLK(4)
              REAL(KIND=4) :: STEMSDW(4)
              REAL(KIND=4) :: RMWAREA(4)
              REAL(KIND=4) :: STEMFORM(4)
              INTEGER(KIND=4) :: NOFQDATES(4)
              INTEGER(KIND=4) :: DATESFQ(45,4)
              REAL(KIND=4) :: Q10FTABLE(45,4)
              REAL(KIND=4) :: K10F(4)
              INTEGER(KIND=4) :: NOWQDATES(4)
              INTEGER(KIND=4) :: DATESWQ(45,4)
              REAL(KIND=4) :: Q10WTABLE(45,4)
              REAL(KIND=4) :: RMFR(4)
              REAL(KIND=4) :: RMCR(4)
              REAL(KIND=4) :: Q10R(4)
              REAL(KIND=4) :: RTEMPR(4)
              REAL(KIND=4) :: EFFYRF(4)
              REAL(KIND=4) :: RMB(4)
              REAL(KIND=4) :: Q10B(4)
              REAL(KIND=4) :: RTEMPB(4)
              REAL(KIND=4) :: GSREF(4)
              REAL(KIND=4) :: GSMIN(4)
              REAL(KIND=4) :: PAR0(4)
              REAL(KIND=4) :: D0(4)
              REAL(KIND=4) :: VK1(4)
              REAL(KIND=4) :: VK2(4)
              REAL(KIND=4) :: VPD1(4)
              REAL(KIND=4) :: VPD2(4)
              REAL(KIND=4) :: VMFD0(4)
              REAL(KIND=4) :: GSJA(4)
              REAL(KIND=4) :: GSJB(4)
              REAL(KIND=4) :: T0(4)
              REAL(KIND=4) :: TREF(4)
              REAL(KIND=4) :: TMAX(4)
              REAL(KIND=4) :: SMD1(4)
              REAL(KIND=4) :: SMD2(4)
              REAL(KIND=4) :: WC1(4)
              REAL(KIND=4) :: WC2(4)
              REAL(KIND=4) :: SWPEXP(4)
              REAL(KIND=4) :: GNIGHT(4)
              REAL(KIND=4) :: G0TABLE(45,4)
              REAL(KIND=4) :: G1TABLE(45,4)
              REAL(KIND=4) :: GK(4)
              INTEGER(KIND=4) :: NOGSDATES(4)
              INTEGER(KIND=4) :: DATESGS(45,4)
              REAL(KIND=4) :: D0L(4)
              REAL(KIND=4) :: GAMMA(4)
              REAL(KIND=4) :: VPDMIN(4)
              REAL(KIND=4) :: WLEAFTABLE(45,4)
              INTEGER(KIND=4) :: DATESWLEAF(45,4)
              INTEGER(KIND=4) :: NOWLEAFDATES(4)
              INTEGER(KIND=4) :: NSIDES(4)
              REAL(KIND=4) :: SF(4)
              REAL(KIND=4) :: PSIV(4)
              REAL(KIND=4) :: VPARA(4)
              REAL(KIND=4) :: VPARB(4)
              REAL(KIND=4) :: VPARC(4)
              INTEGER(KIND=4) :: VFUN(4)
              REAL(KIND=4) :: G02TABLE(45,4)
              REAL(KIND=4) :: G12TABLE(45,4)
              INTEGER(KIND=4) :: NEWTUZET
              CHARACTER(*) :: IN_PATH
            END SUBROUTINE INPUTPHY
          END INTERFACE 
        END MODULE INPUTPHY__genmod
