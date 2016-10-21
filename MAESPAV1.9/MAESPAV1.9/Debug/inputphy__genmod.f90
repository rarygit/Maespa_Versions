        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:30 2016
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
     &TREF,TMAX,SMD1,SMD2,WC1,WC2,SWPEXP,G0TABLE,G1TABLE,GK,NOGSDATES,  &
     &DATESGS,D0L,GAMMA,VPDMIN,WLEAFTABLE,DATESWLEAF,NOWLEAFDATES,NSIDES&
     &,SF,PSIV,VPARA,VPARB,VPARC,VFUN,G02TABLE,G12TABLE,NEWTUZET,IN_PATH&
     &)
              INTEGER(KIND=4) :: NSPECIES
              CHARACTER(LEN=30) :: PHYFILES(5)
              INTEGER(KIND=4) :: MODELJM
              INTEGER(KIND=4) :: MODELRD
              INTEGER(KIND=4) :: MODELGS
              INTEGER(KIND=4) :: MODELRW
              INTEGER(KIND=4) :: NOLAY
              INTEGER(KIND=4) :: NOAGEC(5)
              INTEGER(KIND=4) :: NOAGEP(5)
              REAL(KIND=4) :: PROPC(3,5)
              REAL(KIND=4) :: PROPP(3,5)
              REAL(KIND=4) :: ABSRP(15,3,5)
              REAL(KIND=4) :: REFLEC(15,3,5)
              REAL(KIND=4) :: TRANS(15,3,5)
              REAL(KIND=4) :: RHOSOL(3,5)
              REAL(KIND=4) :: JMAXTABLE(30,15,3,5)
              INTEGER(KIND=4) :: DATESJ(30,5)
              INTEGER(KIND=4) :: NOJDATES(5)
              INTEGER(KIND=4) :: IECO(5)
              REAL(KIND=4) :: EAVJ(5)
              REAL(KIND=4) :: EDVJ(5)
              REAL(KIND=4) :: DELSJ(5)
              REAL(KIND=4) :: THETA(5)
              REAL(KIND=4) :: VCMAXTABLE(30,15,3,5)
              INTEGER(KIND=4) :: DATESV(30,5)
              INTEGER(KIND=4) :: NOVDATES(5)
              REAL(KIND=4) :: EAVC(5)
              REAL(KIND=4) :: EDVC(5)
              REAL(KIND=4) :: DELSC(5)
              REAL(KIND=4) :: TVJUP(5)
              REAL(KIND=4) :: TVJDN(5)
              REAL(KIND=4) :: SLATABLE(30,15,3,5)
              INTEGER(KIND=4) :: DATESSLA(30,5)
              INTEGER(KIND=4) :: NOSLADATES(5)
              INTEGER(KIND=4) :: NOADATES(5)
              INTEGER(KIND=4) :: DATESA(30,5)
              REAL(KIND=4) :: AJQTABLE(30,15,3,5)
              REAL(KIND=4) :: RDTABLE(30,15,3,5)
              INTEGER(KIND=4) :: DATESRD(30,5)
              INTEGER(KIND=4) :: NORDATES(5)
              REAL(KIND=4) :: RTEMP(5)
              REAL(KIND=4) :: DAYRESP(5)
              REAL(KIND=4) :: TBELOW(5)
              REAL(KIND=4) :: EFFYRW(5)
              REAL(KIND=4) :: RMW(5)
              REAL(KIND=4) :: RTEMPW(5)
              REAL(KIND=4) :: COLLA(5)
              REAL(KIND=4) :: COLLK(5)
              REAL(KIND=4) :: STEMSDW(5)
              REAL(KIND=4) :: RMWAREA(5)
              REAL(KIND=4) :: STEMFORM(5)
              INTEGER(KIND=4) :: NOFQDATES(5)
              INTEGER(KIND=4) :: DATESFQ(30,5)
              REAL(KIND=4) :: Q10FTABLE(30,5)
              REAL(KIND=4) :: K10F(5)
              INTEGER(KIND=4) :: NOWQDATES(5)
              INTEGER(KIND=4) :: DATESWQ(30,5)
              REAL(KIND=4) :: Q10WTABLE(30,5)
              REAL(KIND=4) :: RMFR(5)
              REAL(KIND=4) :: RMCR(5)
              REAL(KIND=4) :: Q10R(5)
              REAL(KIND=4) :: RTEMPR(5)
              REAL(KIND=4) :: EFFYRF(5)
              REAL(KIND=4) :: RMB(5)
              REAL(KIND=4) :: Q10B(5)
              REAL(KIND=4) :: RTEMPB(5)
              REAL(KIND=4) :: GSREF(5)
              REAL(KIND=4) :: GSMIN(5)
              REAL(KIND=4) :: PAR0(5)
              REAL(KIND=4) :: D0(5)
              REAL(KIND=4) :: VK1(5)
              REAL(KIND=4) :: VK2(5)
              REAL(KIND=4) :: VPD1(5)
              REAL(KIND=4) :: VPD2(5)
              REAL(KIND=4) :: VMFD0(5)
              REAL(KIND=4) :: GSJA(5)
              REAL(KIND=4) :: GSJB(5)
              REAL(KIND=4) :: T0(5)
              REAL(KIND=4) :: TREF(5)
              REAL(KIND=4) :: TMAX(5)
              REAL(KIND=4) :: SMD1(5)
              REAL(KIND=4) :: SMD2(5)
              REAL(KIND=4) :: WC1(5)
              REAL(KIND=4) :: WC2(5)
              REAL(KIND=4) :: SWPEXP(5)
              REAL(KIND=4) :: G0TABLE(30,5)
              REAL(KIND=4) :: G1TABLE(30,5)
              REAL(KIND=4) :: GK(5)
              INTEGER(KIND=4) :: NOGSDATES(5)
              INTEGER(KIND=4) :: DATESGS(30,5)
              REAL(KIND=4) :: D0L(5)
              REAL(KIND=4) :: GAMMA(5)
              REAL(KIND=4) :: VPDMIN(5)
              REAL(KIND=4) :: WLEAFTABLE(30,5)
              INTEGER(KIND=4) :: DATESWLEAF(30,5)
              INTEGER(KIND=4) :: NOWLEAFDATES(5)
              INTEGER(KIND=4) :: NSIDES(5)
              REAL(KIND=4) :: SF(5)
              REAL(KIND=4) :: PSIV(5)
              REAL(KIND=4) :: VPARA(5)
              REAL(KIND=4) :: VPARB(5)
              REAL(KIND=4) :: VPARC(5)
              INTEGER(KIND=4) :: VFUN(5)
              REAL(KIND=4) :: G02TABLE(30,5)
              REAL(KIND=4) :: G12TABLE(30,5)
              INTEGER(KIND=4) :: NEWTUZET
              CHARACTER(*) :: IN_PATH
            END SUBROUTINE INPUTPHY
          END INTERFACE 
        END MODULE INPUTPHY__genmod
