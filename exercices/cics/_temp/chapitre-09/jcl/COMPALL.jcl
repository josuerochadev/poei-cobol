//COMPALL  JOB (ACCT),'COMPILE CH09',CLASS=A,MSGCLASS=X
//*****************************************************************
//* JCL : Compilation de tous les programmes CICS Chapitre IX     *
//*****************************************************************
//*
//*------- COMPILATION PROGIO --------------------------------------
//PROGIO   EXEC DFHYITVL,PROGRAM=PROGIO
//TRN.SYSIN DD DSN=hlq.SOURCE(PROGIO),DISP=SHR
//*
//*------- COMPILATION PRGREAD -------------------------------------
//PRGREAD  EXEC DFHYITVL,PROGRAM=PRGREAD
//TRN.SYSIN DD DSN=hlq.SOURCE(PRGREAD),DISP=SHR
//*
//*------- COMPILATION PRGRGEN -------------------------------------
//PRGRGEN  EXEC DFHYITVL,PROGRAM=PRGRGEN
//TRN.SYSIN DD DSN=hlq.SOURCE(PRGRGEN),DISP=SHR
//*
//*------- COMPILATION PRGWRIT -------------------------------------
//PRGWRIT  EXEC DFHYITVL,PROGRAM=PRGWRIT
//TRN.SYSIN DD DSN=hlq.SOURCE(PRGWRIT),DISP=SHR
//*
//*------- COMPILATION PGSTART -------------------------------------
//PGSTART  EXEC DFHYITVL,PROGRAM=PGSTART
//TRN.SYSIN DD DSN=hlq.SOURCE(PGSTART),DISP=SHR
//*
//*------- COMPILATION PGPATHF -------------------------------------
//PGPATHF  EXEC DFHYITVL,PROGRAM=PGPATHF
//TRN.SYSIN DD DSN=hlq.SOURCE(PGPATHF),DISP=SHR
//*
//*------- ASSEMBLAGE MAPIO ----------------------------------------
//ASMAPIO  EXEC DFHMAPS,MAPSET=MAPIO
//SYSUT1   DD DSN=hlq.BMS(MAPIO),DISP=SHR
//*
//*------- ASSEMBLAGE MAPREAD --------------------------------------
//ASMREAD  EXEC DFHMAPS,MAPSET=MAPREAD
//SYSUT1   DD DSN=hlq.BMS(MAPREAD),DISP=SHR
//*
//*------- ASSEMBLAGE MAPWRIT --------------------------------------
//ASMWRIT  EXEC DFHMAPS,MAPSET=MAPWRIT
//SYSUT1   DD DSN=hlq.BMS(MAPWRIT),DISP=SHR
//*
//*------- ASSEMBLAGE MAPBRWS --------------------------------------
//ASMBRWS  EXEC DFHMAPS,MAPSET=MAPBRWS
//SYSUT1   DD DSN=hlq.BMS(MAPBRWS),DISP=SHR
//*
//
