MODULE MAINDECLARATIONS
! Array declarations.
    
    USE maestcom
    
    ! List of trees for which to do calculations
    INTEGER ITARGETS(MAXT)
    INTEGER ISPECIES(MAXT),ISPECIEST(MAXT),ISPECIESTUS(MAXT)
    INTEGER J,USESTAND

    ! Tree positions and dimensions - all trees, all dates
    REAL DXT1(MAXT),DYT1(MAXT),DZT1(MAXT)
    REAL RXTABLE1(maxdate,MAXT),RYTABLE1(maxdate,MAXT)
    REAL RZTABLE1(maxdate,MAXT),ZBCTABLE1(maxdate,MAXT)
    REAL FOLTABLE1(maxdate,MAXT),DIAMTABLE1(maxdate,MAXT)
    ! Tree positions and dimensions - sorted trees, all dates
    REAL RXTABLE(maxdate,MAXT),RYTABLE(maxdate,MAXT)
    REAL RZTABLE(maxdate,MAXT),ZBCTABLE(maxdate,MAXT)
    REAL FOLTABLE(maxdate,MAXT),TOTLAITABLE(maxdate)  !!!
    
    REAL DIAMTABLE(maxdate,MAXT)
    INTEGER IT(MAXT),ITUUS(MAXT),ITP(MAXT) ! Sorted tree numbers.
    ! Dates for tree dimensions
    INTEGER DATESX(maxdate),DATESY(maxdate),DATESZ(maxdate)
    INTEGER DATEST(maxdate),DATESLA(maxdate),DATESD(maxdate)
    ! Tree dimensions on simulation date (by interpolation)
    REAL DXT(MAXT),DYT(MAXT),DZT(MAXT)
    REAL RX(MAXT),RY(MAXT),RZ(MAXT),ZBC(MAXT)
    REAL FOLT(MAXT),DIAM(MAXT), EXPFACTORS(MAXT)
    REAL WEIGHTS(MAXT)
    REAL CANOPYDIMS(6)
    ! Positions of grid points, associated volume & leaf area, etc
    REAL XL(MAXP),YL(MAXP),ZL(MAXP),VL(MAXP),DLT(MAXP),DLI(MAXC,MAXP)
    REAL XL2(MAXP),YL2(MAXP),ZL2(MAXP),VL2(MAXP)
    !REAL DLT2(MAXP),DLI2(MAXC,MAXP) 
    
    INTEGER LGP(MAXP),LAYER(MAXP),MLAYER(MAXP),PPLAY
    REAL FOLLAY(MAXLAY),WINDLAY(MAXLAY)
    REAL LGP2(MAXP),FOLLAY2(MAXLAY)
      
    ! Understorey arrays.
    REAL XLU(MAXP),YLU(MAXP),ZLU(MAXP) ! Understorey points.
    INTEGER LAYERUS(MAXP),MLAYERUS(MAXP) ! not implemented yet
    REAL UIBEAM(MAXHRS,MAXP),UIDIFF(MAXHRS,MAXP)
    REAL PARUS(MAXHRS,MAXP),APARUS(MAXHRS,MAXP)
    REAL PSUS(MAXHRS,MAXP),ETUS(MAXHRS,MAXP)
    REAL PARUNDER(MAXHRS,MAXP),USLAITAB(maxdate,MAXT)
    REAL USLAI(MAXP)!,DIAMUS(maxdate,MAXT)
    REAL HTUS(maxdate,MAXT),FOLNUS(maxdate,MAXT)
    INTEGER DATESFU(maxdate)!,DATESDU(maxdate)
    INTEGER DATESHU(maxdate),DATESNU(maxdate)
    REAL JMAXN25,JMAX25M,RESCALE
    REAL FUS(MAXP),AREAUS(MAXP),FN0US(MAXP)!, DUS(MAXP)
    REAL DXTUS(MAXT),DYTUS(MAXT),DZTUS(MAXT)
    REAL RXTABLEUS(maxdate,MAXT),RYTABLEUS(maxdate,MAXT)
    REAL RZTABLEUS(maxdate,MAXT)
    REAL FOLTABLEUS(maxdate,MAXT),ZBCTABLEUS(maxdate,MAXT)
    REAL DIAMTABLEUS(maxdate,MAXT)
    REAL RXUS(MAXT),RYUS(MAXT),RZUS(MAXT)
    REAL ZBCUS(MAXT),FOLTUS(MAXT),PARUSMEAN(MAXHRS)
    REAL PARUSSD(MAXHRS),THRABUS(MAXHRS),FCO2US(MAXHRS),FH2OUS(MAXHRS)

    ! Met data
    INTEGER METCOLS(MAXMET),SOILDATA,TSOILDATA,REASSIGNRAIN
    INTEGER SIMTSOIL,WSOILMETHOD, RETFUNCTION, USEMEASET,USEMEASSW
    REAL WINDAH(MAXHRS),TSOIL(MAXHRS),TAIR(MAXHRS),RADABV(MAXHRS,3)
    REAL FBEAM(MAXHRS,3),RH(MAXHRS)
    REAL VPD(MAXHRS),VMFD(MAXHRS),ETMEAS(MAXHRS)
    REAL CA(MAXHRS),PRESS(MAXHRS),PPT(MAXHRS),SOILMOIST(MAXHRS)
    REAL DELTAT(12)
    REAL TAIRMEM(10000), TAIRR(MAXHRS)

    ! Physiology inputs by layer
    REAL ABSRP(MAXLAY,3),ARHO(MAXLAY,3),ATAU(MAXLAY,3)
    REAL RHOSOL(3)
    REAL JMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL VCMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL RDTABLE(maxdate,MAXLAY,MAXC)
    REAL SLATABLE(maxdate,MAXLAY,MAXC)
    REAL AJQTABLE(maxdate,MAXLAY,MAXC)
    REAL Q10FTABLE(maxdate), Q10WTABLE(maxdate)
    INTEGER DATESFQ(maxdate), DATESWQ(maxdate)
    INTEGER DATESJ(maxdate), DATESV(maxdate)
    INTEGER DATESRD(maxdate), DATESSLA(maxdate)
    INTEGER DATESA(maxdate)
    REAL JMAX25(MAXLAY,MAXC),VCMAX25(MAXLAY,MAXC)
    REAL RD0(MAXLAY,MAXC),SLA(MAXLAY,MAXC),AJQ(MAXLAY,MAXC)

    ! Structural data inputs
    REAL BPT(8,MAXC),PROPP(MAXC),PROPC(MAXC)
    REAL ALPHA(MAXANG),FALPHA(MAXANG)
    ! Intermediate calculations
    REAL DIFZEN(MAXANG),DEXT(MAXANG),BEXTANG(MAXANG)
    REAL ZEN(MAXHRS),AZ(MAXHRS)
    REAL ZEN0(MAXHRS),AZ0(MAXHRS)
    REAL TU(MAXP),TD(MAXP),RELDF(MAXP)
    REAL TUUS(MAXP),TDUS(MAXP),RELDFUS(MAXP)  ! Understorey.
    REAL DIFDN(MAXP,3),DIFUP(MAXP,3),SCLOST(MAXP,3)
    REAL BFLUX(MAXP,3),DFLUX(MAXP,3),SCATFX(MAXP,3)
    REAL SCLOSTTREE(MAXT,3)
    REAL DOWNTH(MAXP),DOWNTHTREE(MAXT)
    REAL TUAR(MAXT,MAXP),TDAR(MAXT,MAXP),RELDFAR(MAXT,MAXP)
    REAL PLANTKCR(MAXT,MAXP)

    ! Outputs for each tree - NOW TREE ARRAYS! (RAD JUNE 2008)
    REAL THRAB(MAXT,MAXHRS,3),TDYAB(MAXT,3),TCAN(MAXT,MAXHRS)
    REAL FCO2(MAXT,MAXHRS),FH2O(MAXT,MAXHRS)
    REAL GSCAN(MAXT,MAXHRS),FHEAT(MAXT,MAXHRS),FH2OCAN(MAXT,MAXHRS)
    REAL GBHCAN(MAXT,MAXHRS)
    REAL FRESPF(MAXT,MAXHRS),FRESPW(MAXT,MAXHRS),FRESPB(MAXT,MAXHRS)
    REAL FRESPFR(MAXT,MAXHRS),FRESPCR(MAXT,MAXHRS)
    REAL PPAR(MAXT,MAXLAY,MAXHRS),PPS(MAXT,MAXLAY,MAXHRS)
    REAL PTRANSP(MAXT,MAXLAY,MAXHRS)

    ! Daily totals are now also tree arrays (June 2008 RAD).
    REAL TOTCO2(MAXT),TOTRESPF(MAXT),TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT),TOTRESPFR(MAXT),TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT),TOTH2OCAN(MAXT),TOTHFX(MAXT)

    ! Water balance related pars.
    REAL ROOTRESIST, ROOTRAD, MINROOTWP, KTOT,PLOTAREA
    REAL MAXSTORAGE,DRAINLIMIT(MAXSOILLAY)
    REAL DISCHARGE, FRACORGANIC(MAXSOILLAY)
    REAL ROOTMASS(MAXSOILLAY),ROOTLEN(MAXSOILLAY)
    INTEGER NLAYER,NROOTLAYER,EQUALUPTAKE
    INTEGER NSUMMED
    REAL BPAR(MAXSOILLAY),PSIE(MAXSOILLAY),KSAT(MAXSOILLAY)
    REAL LAYTHICK(MAXSOILLAY), INITWATER(MAXSOILLAY)
    REAL FRACROOTTABLE(MAXSOILLAY,MAXDATE), POREFRAC(MAXSOILLAY),FRACROOT(maxsoillay)
    REAL SOILWP(MAXSOILLAY),FRACWATER(MAXSOILLAY)
    REAL SOILCOND(MAXSOILLAY),SOILRRES(MAXSOILLAY)
    REAL ICEPROP(MAXSOILLAY),FRACUPTAKE(MAXSOILLAY)
    REAL WATERGAIN(MAXSOILLAY),WATERLOSS(MAXSOILLAY)
    REAL PPTGAIN(MAXSOILLAY), SOILTEMP(MAXSOILLAY)
    REAL WETTINGBOT(10),WETTINGTOP(10)
    REAL THERMCOND(MAXSOILLAY)
    REAL TESTER(MAXP)
    REAL ALPHARET(MAXSOILLAY),WS(MAXSOILLAY),WR(MAXSOILLAY),NRET(MAXSOILLAY)
    
    ! Multi-species       
    CHARACTER SPECIESNAMES(MAXSP)*30
    CHARACTER PHYFILES(MAXSP)*30
    CHARACTER STRFILES(MAXSP)*30

    ! STR arrays, multi-species versions.
    REAL ALPHASPEC(MAXANG,MAXSP),FALPHASPEC(MAXANG,MAXSP),FALPHATABLESPEC(MAXANG,MAXDATE,MAXSP)
    REAL BPTSPEC(8,MAXC,MAXSP),BPTT(8,MAXC,MAXT),BPTTABLESPEC(8,MAXC,MAXSP,MAXDATE)
    REAL BPTTUS(8,MAXC,MAXT)
    REAL SHAPESPEC(MAXSP),EXTWINDSPEC(MAXSP)
    REAL RANDOMSPEC(MAXSP),COEFFTSPEC(MAXSP)
    REAL EXPONTSPEC(MAXSP),WINTERCSPEC(MAXSP)
    REAL BCOEFFTSPEC(MAXSP),BEXPONTSPEC(MAXSP)
    REAL BINTERCSPEC(MAXSP),DEXTSPEC(MAXSP,MAXANG)
    REAL DEXTT(MAXT,MAXANG),DEXTTUS(MAXT,MAXANG)
    REAL BEXTSPEC(MAXSP),BEXTANGSPEC(MAXSP,MAXANG)
!    REAL BEXTANGT(MAXP,MAXANG),BEXTANGTUS(MAXP,MAXANG)
    REAL BEXTANGT(MAXT,MAXANG),BEXTANGTUS(MAXT,MAXANG),BEXTANGUS(MAXANG) ! modification 17 d�cembre 2012
    REAL BEXTT(MAXT),BEXTTUS(MAXT)
    REAL RCOEFFTSPEC(MAXSP),REXPONTSPEC(MAXSP)
    REAL RINTERCSPEC(MAXSP),FRFRACSPEC(MAXSP)
    INTEGER NOAGECSPEC(MAXSP),NOAGECT(MAXT),NOAGECTUS(MAXT)
    INTEGER JLEAFSPEC(MAXSP),JLEAFT(MAXT),JLEAFTUS(MAXT)
    INTEGER JSHAPESPEC(MAXSP), NALPHASPEC(MAXSP)
    INTEGER JSHAPET(MAXT),JSHAPETUS(MAXT)
    REAL SHAPET(MAXT),SHAPETUS(MAXT)
    REAL VPDMINSPEC(MAXSP),VPDMIN

    ! PHY arrays, multi-species versions.
    REAL ABSRPSPEC(MAXLAY,3,MAXSP),ARHOSPEC(MAXLAY,3,MAXSP)
    REAL ATAUSPEC(MAXLAY,3,MAXSP),RHOSOLSPEC(3,MAXSP)
    REAL PROPPSPEC(MAXC,MAXSP),PROPCSPEC(MAXC,MAXSP)
    REAL PROPPT(MAXC,MAXT),PROPCT(MAXC,MAXT)
    REAL PROPPTUS(MAXC,MAXT),PROPCTUS(MAXC,MAXT)
    REAL LEAFNSPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL JMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL VCMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL RDTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL SLATABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL AJQTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL Q10FTABLESPEC(maxdate,MAXSP),Q10WTABLESPEC(maxdate,MAXSP)
    !INTEGER DATESNSPEC(maxdate,MAXSP)
    INTEGER DATESJSPEC(maxdate,MAXSP)
    INTEGER DATESVSPEC(maxdate,MAXSP),DATESRDSPEC(maxdate,MAXSP)
    INTEGER DATESSLASPEC(maxdate,MAXSP),DATESASPEC(maxdate,MAXSP)
    INTEGER DATESFQSPEC(maxdate,MAXSP),DATESWQSPEC(maxdate,MAXSP)
    INTEGER NOAGEPSPEC(MAXSP),NSIDESSPEC(MAXSP),K 
    real K2
    REAL GSREFSPEC(MAXSP), GSMINSPEC(MAXSP), PAR0SPEC(MAXSP)
    REAL D0SPEC(MAXSP), VK1SPEC(MAXSP), VK2SPEC(MAXSP)
    REAL VPD1SPEC(MAXSP), VPD2SPEC(MAXSP), VMFD0SPEC(MAXSP) 
    REAL GSJASPEC(MAXSP), GSJBSPEC(MAXSP), T0SPEC(MAXSP)
    REAL TREFSPEC(MAXSP), TMAXSPEC(MAXSP), SMD1SPEC(MAXSP)
    REAL SMD2SPEC(MAXSP), WC1SPEC(MAXSP), WC2SPEC(MAXSP)
    REAL SWPEXPSPEC(MAXSP),  D0LSPEC(MAXSP)  ! G0SPEC(MAXSP),
    REAL GAMMASPEC(MAXSP), WLEAFSPEC(MAXSP)   ! G1SPEC(MAXSP), 
    REAL SFSPEC(MAXSP),PSIVSPEC(MAXSP)
    !REAL :: TOTLAI(MAXT)
    INTEGER NOJDATESSPEC(MAXSP),NOVDATESSPEC(MAXSP)
    INTEGER NOADATESSPEC(MAXSP),NOSLADATESSPEC(MAXSP)
    INTEGER NORDATESSPEC(MAXSP)  !NONDATESSPEC(MAXSP),
    INTEGER NOWQDATESSPEC(MAXSP),NOFQDATESSPEC(MAXSP)
    INTEGER IECOSPEC(MAXSP)
    REAL EAVJSPEC(MAXSP), EDVJSPEC(MAXSP)
    REAL DELSJSPEC(MAXSP), EAVCSPEC(MAXSP)
    REAL EDVCSPEC(MAXSP), DELSCSPEC(MAXSP), TVJUPSPEC(MAXSP)
    REAL TVJDNSPEC(MAXSP), THETASPEC(MAXSP)
    REAL RTEMPSPEC(MAXSP), DAYRESPSPEC(MAXSP), EFFYRFSPEC(MAXSP)
    REAL TBELOWSPEC(MAXSP),EFFYRWSPEC(MAXSP),RMWSPEC(MAXSP)
    REAL RTEMPWSPEC(MAXSP),COLLASPEC(MAXSP),COLLKSPEC(MAXSP)
    REAL STEMSDWSPEC(MAXSP),RMWAREASPEC(MAXSP),STEMFORMSPEC(MAXSP)
    REAL Q10RSPEC(MAXSP),RTEMPRSPEC(MAXSP),Q10BSPEC(MAXSP)
    REAL RTEMPBSPEC(MAXSP),RMCRSPEC(MAXSP),RMFRSPEC(MAXSP)
    REAL RMBSPEC(MAXSP),K10FSPEC(MAXSP),K10F
    
    REAL G0TABLESPEC(maxdate,MAXSP),G1TABLESPEC(maxdate,MAXSP)
    INTEGER NOGSDATESSPEC(MAXSP),DATESGSSPEC(maxdate,MAXSP)
    REAL WLEAFTABLESPEC(maxdate,MAXSP)
    INTEGER NOWLEAFDATESSPEC(MAXSP), DATESWLEAFSPEC(maxdate,MAXSP)
    INTEGER DATESGS(maxdate), DATESWLEAF(maxdate),DATESKP(maxdate),DATESROOT(maxdate)
    INTEGER DATESLIA2(maxdate,maxsp),DATESLAD(maxdate,maxsp)
    REAL G0TABLE(maxdate),G1TABLE(maxdate),WLEAFTABLE(maxdate)
    REAL PLANTKTABLE(maxdate),ROOTRADTABLE(maxdate),ROOTSRLTABLE(maxdate),ROOTMASSTOTTABLE(maxdate)
    
    REAL TARGETFOLS(MAXT)
    REAL ABSRPU, AJQU, ALAT, ALEAF, ANIR, APAR, APP, AREA, ATHR
    REAL AX, AY, BALPHA, BLAMBDA, BBINC, BBIOM, BCOEFFT, BEAMP, BEAR
    REAL BEXPONT, BEXT, BEXTUS, BINSIZE, BINTERC, BMULT
    REAL CAK, CANOPY_STORE, CICARAT, CO2INC, COEFFT, COLLA, COLLK
    REAL D0, D0L, DAYRESP, DAYL, DEC, DELSC, DELSCU, DELSJ, DELSJU, DISCHARGETOT
    REAL DAYL0,DEC0,SUNSET0,EQNTIM0,SF,PSIV,HMSHAPE,PSILIN
    INTEGER KEEPZEN
    REAL DIFSKY, DLAI, DMULT2, DRAINSTORE, DRYTHICK, DT1, DT2!, DRYTHICHMIN
    REAL DRYTHICKMIN, DT3, DT4, EAVC, EAVCU, EAVJU, EDVC, EAVJ, EDVJ, DVJU, EDVCU
    REAL EFFY, EFFYRF, EDVJU, EMAXLEAF, EFFYRW, EQNTIM, ESOIL, ET, ETEST
    REAL ETMEASTOT, ETMM, ETMM2, ETMMTOT, ETUSMM, EVAPSTORE, EXPAN, EXPDIF, EXPINF, EXPONT
    REAL EXPTIME, EXTKUS, EXTWIND, FAREA, FBEAMOTC, FBINC, FBIOM, FRFRAC, FSOIL, FTSOIL1, FSOILMEAN
    REAL G0, G1, GAMMA, GAMSOIL, GBH, GRDAREAI, GSBG0U, GSBG1U, FSOIL1, GSC, GSIPT, GSJA, GSJB,WLEAF
    REAL GSREF, HFX, GSMIN,RD0US,SLAUS
    INTEGER I, IAGE, ICC, IDAY, IECO, IECOU, IEND, IFLUSH, IHOUR, IOTC
    INTEGER IPROG, IPROGUS, IPT, IPTUS, ISIMUS, ISPEC, ISTART, ISUNLIT, ITAR, ITERMAX, ITREE, IPOINTS
    INTEGER SIMSOILEVAP,IUSTFILE
    INTEGER IWATFILE, IWAVE, IWHICH, JLEAF, JSHAPE, KEEPWET, MASPDATE, MFLAG, MODELGS, MODELJM
    INTEGER NUMTESTPNT
    LOGICAL ISMAESPA
    INTEGER MODELRD, MODELRW, MODELSS, MOSS, MOVEWINDOW, MSTART, NALPHA, NAZ, NEWCANOPY
    INTEGER NEWTUTD, NOADTES, NOAGEC, NOADATES, NOAGEP, NOALLTREES, NODDATES, NOFQDATES, NOFUDATES
    INTEGER NOGSDATES, NOHUDATES, NOJDATES, NOLADATES, NOLAY, NOMETCOLS, NONUDATES, NORDATES, NOWLEAFDATES
    INTEGER NOSLADATES, NOTARGETS, NOTDATES, NOTREES, NOUSPOINTS, NOVDATES, NOWQDATES 
    INTEGER NOXDATES,NOKPDATES,NOROOTDATES,NOLIADATES(maxsp),NOLADDATES(MAXSP)
    INTEGER NOYDATES, NOZDATES, NSIDES, NSUMMEDW, NTAIRADD, NUMPNT, NZEN, NSTEP
    INTEGER IPROGCUR,RANPOINTS,NOTREESTEST
    
    REAL OUTFLOW, OVERFLOW, PAR, PAR0, PAROTC, PLANTK, KSCALING,PPTDAY, PPTTOT, PRESSK, PREVTSOIL, PSIL
    REAL Q10B, Q10R, Q10W, Q10F, QC, QCTOT, QE, QETOT, QH, QHTOT, QN, QNTOT
    REAL RADINTERC, RADINTERC1, RADINTERC2, RADINTERC3, RADINTERCTOT, RANDOM, RBINC, RBINOM
    REAL RBIOM, RCOEFFT, RD, RD0ACC, RDK, RDT, RESPF, REXPONT, RGLOBABV, RGLOBABV12
    REAL RGLOBUND, RINTERC, RMB, RMCR, RMFR, RMW, RMWAREA, RNET, ROOTRESFRAC, ROOTXSECAREA
    REAL RTEMP, RTEMPB, RTEMPR, RUNOFF, RUTTERB, RUTTERD, SCLOSTTOT, SHADEHT, SHAPE, SMD1, SMD2
    REAL SOILDEPTH, SOILEVAP, SOILEVAPTOT, SOILMOISTURE, SOILTK, SOMULT, STEMFORM
    REAL STEMSDW, STOCKING, SUNLA, SUNSET, RTEMPW, TMAX, TMOVE
    REAL SURFACE_WATERMM, SWMAX, SWMIN
    REAL SWPEXP, T0, THETAM, THROUGHFALL, TINC, TLEAF, TOTC, TOTESTEVAPMM
    REAL TBELOW, TDIFF, TFALLTOT, THETA, TOTRESPRG, TOTRESPBG
    CHARACTER(len=256) ::  CTITLE, TTITLE, PTITLE, STITLE, WTITLE, UTITLE, VTITLE, MTITLE
    CHARACTER(len=256) :: in_path, out_path
    
        ! Test point arrays
    REAL DXTP(MAXT),DYTP(MAXT),DZTP(MAXT)
    REAL RXTABLEP(MAXDATE,MAXT),RYTABLEP(MAXDATE,MAXT)
    REAL RZTABLEP(MAXDATE,MAXT)
    REAL FOLTABLEP(MAXDATE,MAXT),ZBCTABLEP(MAXDATE,MAXT)
    REAL DIAMTABLEP(MAXDATE,MAXT)
    REAL RXP(MAXT),RYP(MAXT),RZP(MAXT)
    REAL ZBCP(MAXT),FOLTP(MAXT)
    INTEGER ISPECIESP(MAXT),ISPECIESTP(MAXT)
    INTEGER JSHAPETP(MAXT),JLEAFTP(MAXT),NOAGECTP(MAXT)
    REAL SHAPETP(MAXT),DEXTTP(MAXT,MAXANG),BPTTP(8,MAXC,MAXT)
    REAL PROPPTP(MAXC,MAXT),PROPCTP(MAXC,MAXT)
    REAL DEXTP(MAXANG),DLAIP,EXPDIFP,SUNLAP,BEXTP,APARSUN,APARSH
    REAL BEXTANGTP(MAXP,MAXANG),BEXTTP(MAXT),TTOT,TBEAM,APARMEAN
    REAL BEXTANGP(MAXANG),THDOWNP(MAXP)
    INTEGER IPTEST
    
    ! Outputs for PAR histogram
    REAL HISTO(MAXT,MAXHISTO)  ! Note dim change (RAD Sept. 08).
   
    ! mgdk...
    INTEGER NSPECIES

    !
    REAL WINDOTC,XSLOPE,YSLOPE,X0,Y0,XMAX,YMAX,ZHT,Z0HT,ZPD,TORTPAR,TTIMD
    REAL TVJUPU,TVJDNU,VCMAXN25,UNMIN,VCMAX25M,WSOIL,WSOILROOT,WSOILMEAN
    REAL WSOILROOTMEAN,SWPMEAN,TOTTMP,TOTLAI,WINTERC,TVJUP,TVJDN,VK1,VK2,VPD1,VPD2
    REAL VMFD0,TREF,WC1,WC2,WBIOM,WBINC,WEIGHTEDSWP,TSCAT
    REAL FRACAPAR,VIEWFACTOR,TOTRESPWG,TOTRESPFRG,TOTRESPCRG
    REAL TOTRESPFG,TOTSOILRES,MINLEAFWP,TMP,CI


    REAL G0SPEC(MAXSP),G1SPEC(MAXSP),GKSPEC(MAXSP)
    REAL GK
    REAL XLP(MAXP),YLP(MAXP),ZLP(MAXP)
    INTEGER LAYERP(MAXP),MLAYERP(MAXP)
    REAL RELDFP(MAXP)
    REAL TUP(MAXP), TDP(MAXP)
    
    REAL EFFK,TOTESTEVAP
    INTEGER LAITHROUGHF
    REAL PSILCAN(MAXT,MAXHRS),CICAN(MAXT,MAXHRS)
    REAL PSILCANMIN(MAXT,MAXHRS)
    REAL ECANMAX(MAXT,MAXHRS),ACANMAX(MAXT,MAXHRS)
    
    REAL VPARASPEC(MAXSP),VPARBSPEC(MAXSP),VPARCSPEC(MAXSP)
    REAL VPARA,VPARB,VPARC
    INTEGER VFUN,VFUNSPEC(MAXSP)

    ! Supplementary M. CHristina
    REAL TAIRABOVE, TAIRNEW, VPDNEW, DELTA, RHNEW, VPDABOVE, tcan2, AREATOT, TSOILSURFACE
    REAL TLEAFTABLE(MAXT,MAXP),EMSKY(MAXHRS),TREEH
    INTEGER ITERTAIR,ILAY, IDIPT,IDTAR,IZEN,ITERTAIRMAX
    REAL APARTABLE(MAXT,MAXP),ETTABLE(MAXT,MAXP),GSCTABLE(MAXT,MAXP),PSILTABLE(MAXT,MAXP),GCANOP, DMEAN
    REAL ANIRTABLE(MAXT,MAXP),ATHRTABLE(MAXT,MAXP),HTABLE(MAXT,MAXP)
    REAL RGLOBUND1, RGLOBUND2, SOILLONGWAVE, SOILLONGWAVETREE(MAXT),SOILLONGWAVEIPT(MAXP) 
    REAL PREVTAIRCAN, PREVVPDCAN,DOWNTHAV
    
END MODULE