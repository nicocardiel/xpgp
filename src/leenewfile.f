C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
C
C This file is part of xpgp.
C 
C Xpgp is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C Xpgp is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with xpgp. If not, see <http://www.gnu.org/licenses/>.
C------------------------------------------------------------------------------
C Carga un nuevo fichero en el buffer NB. Si el fichero se lee correctamente,
C ISTATUS retorna 1. En caso contrario retorna 0. La variable IMODE indica el
C tipo de lectura a realizar:
C IMODE=1: solo se una variable (eje Y, errY); el eje X es el numero de dato
C IMODE=2: se leen X,errX,Y,errY,name
        SUBROUTINE LEENEWFILE(NB,IMODE,ISTATUS)
        IMPLICIT NONE
        INTEGER NB,IMODE,ISTATUS
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
        INCLUDE 'lenlinea.inc'
C
        INTEGER READI_B
        INTEGER SYSTEMFUNCTION
        INTEGER TRUEBEG,TRUELEN
        INTEGER READILIM_B
        CHARACTER*255 READC_B
C
        INTEGER I,J
        INTEGER L0,L1,L2
        INTEGER ISYSTEM
        INTEGER NSKIP,NDATA
        INTEGER NNAME,NX,NY,NEX,NEY
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER ISTATUSEXTRAE
        INTEGER NCOMMENTS
        INTEGER REDUCEME_NSCAN,REDUCEME_NCHAN
        INTEGER REDUCEME_NCHAR
        INTEGER NS1,NS2
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER FITS_BITPIX,FITS_IREADWRITE,FITS_BLOCKSIZE
        INTEGER FITS_ISTATUS
        INTEGER NEW_HDU,HDUTYPE
        INTEGER NAXIS_(0:2),FITS_NFOUND,FITS_FIRSTPIX
        INTEGER IXAXIS
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL FEXTRAE
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL REDUCEME_STWV,REDUCEME_DISP
        REAL REDUCEME_FDUMMY
        REAL SP(NDATAMAX)
        REAL SPSUM(NDATAMAX)
        REAL CRPIX1,CRVAL1,CDELT1
        CHARACTER*12 REDUCEME_ID
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)  !OJO: tama~no igual que CEXTRAE
        CHARACTER*20 CEXTRAE                    !funcion para extraer la cadena
        CHARACTER*50 DATAKEY(NBUFFMAX)
        CHARACTER*50 CDUMMY
        CHARACTER*50 FITS_COMMENT
        CHARACTER*255 INFILE
        CHARACTER*255 REDUCEME_CDUMMY,FITS_OBJECT
        CHARACTER*(LENLINEA) CLINEA
        LOGICAL LOGFILE
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
        LOGICAL LUNREAD,LNEXTROW
        LOGICAL LBATCH
        LOGICAL FITS_EXTEND
        LOGICAL LCRPIX1,LCRVAL1,LCDELT1
        LOGICAL LSP(NDATAMAX)
        LOGICAL ANYNULL
        LOGICAL LREPEAT
C
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKINFILE/INFILE
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKLXYNAME/LXYNAME
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKSETTINGS8B/DATAKEY
C------------------------------------------------------------------------------
        IF((IMODE.NE.1).AND.(IMODE.NE.2))THEN
          WRITE(*,100) 'IMODE='
          WRITE(*,*) IMODE
          WRITE(*,101) 'FATAL ERROR in subroutine LEENEWFILE: '//
     +     'IMODE must be 1 or 2'
          STOP
        END IF
C------------------------------------------------------------------------------
        ISTATUS=0                          !salvo que se demuestre lo contrario
        LUNREAD=.FALSE.                    !indica si algun dato no se ha leido
5       WRITE(*,100) 'New input data file name (wildcars allowed) '
        IF(INFILE.EQ.'none')THEN
          INFILE=READC_B('*','@')
        ELSE
          INFILE=READC_B(INFILE,'@')
        END IF
        CALL TOLOG77_STRING(INFILE(1:TRUELEN(INFILE)),'Input file name')
        IF((INDEX(INFILE,'*').NE.0).OR.
     +   (INDEX(INFILE,'?').NE.0))THEN
          L1=TRUEBEG(INFILE)
          L2=TRUELEN(INFILE)
          ISYSTEM=SYSTEMFUNCTION('ls '//INFILE(L1:L2))
          GOTO 5
        END IF
C
        INQUIRE(FILE=INFILE,EXIST=LOGFILE)
C------------------------------------------------------------------------------
        IF(.NOT.LOGFILE)THEN
          WRITE(*,101) 'ERROR: this file does not exist.'
          WRITE(*,100) 'Press <CR> to continue...'
          INFILE='none'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
C Miramos si tiene formato REDUCEME
        OPEN(10,FILE=INFILE,STATUS='OLD',FORM='UNFORMATTED')
        READ(10,ERR=6) REDUCEME_ID
        IF(TRUELEN(REDUCEME_ID).EQ.12)THEN
          IF(REDUCEME_ID.EQ.'abcdefghijkl')THEN
            WRITE(*,101) '>>> This file has REDUCEME format!'
            READ(10) REDUCEME_NSCAN,REDUCEME_NCHAN
            WRITE(*,100) '>>> NSCAN : '
            WRITE(*,*) REDUCEME_NSCAN
            WRITE(*,100) '>>> NCHAN : '
            WRITE(*,*) REDUCEME_NCHAN
            IF(REDUCEME_NCHAN.GT.NDATAMAX)THEN
              CLOSE(10)
              WRITE(*,100) '>>> NDATAMAX: '
              WRITE(*,*) NDATAMAX
              WRITE(*,101) 'ERROR: NCHAN.GT.NDATAMAX'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
              RETURN
            END IF
            !stwv,disp
            READ(10) REDUCEME_STWV,REDUCEME_DISP
            WRITE(*,100) '>>> STWV  : '
            WRITE(*,*) REDUCEME_STWV
            WRITE(*,100) '>>> DISP  : '
            WRITE(*,*) REDUCEME_DISP
            !airmass
            READ(10) REDUCEME_FDUMMY
            !timexpos
            READ(10) REDUCEME_FDUMMY
            !object
            READ(10) REDUCEME_NCHAR
            IF(REDUCEME_NCHAR.GT.0)THEN
              READ(10) REDUCEME_CDUMMY(1:REDUCEME_NCHAR)
              WRITE(*,100) '>>> OBJECT: '
              WRITE(*,101) REDUCEME_CDUMMY(1:REDUCEME_NCHAR)
            END IF
            !fitsfile
            READ(10) REDUCEME_NCHAR
            IF(REDUCEME_NCHAR.GT.0)THEN
              READ(10) REDUCEME_CDUMMY(1:REDUCEME_NCHAR)
            END IF
            !comment
            READ(10) REDUCEME_NCHAR
            IF(REDUCEME_NCHAR.GT.0)THEN
              READ(10) REDUCEME_CDUMMY(1:REDUCEME_NCHAR)
            END IF
            !se pide escale en el eje X
            WRITE(*,100) 'X-axis: 1=pixel, 2=wavelength '
            IXAXIS=READILIM_B('1',1,2)
            WRITE(77,111) IXAXIS,'# X-axis: 1=pixel, 2=wavelength'
            !se pide el numero de scan a leer
            IF(REDUCEME_NSCAN.EQ.1)THEN
              NS1=1
              NS2=1
            ELSE
              WRITE(*,*)
              WRITE(*,101) '* Define the scan region to be averaged:'
              WRITE(*,100) 'First scan to be read'
              NS1=READILIM_B('@',1,REDUCEME_NSCAN)
              WRITE(77,111) NS1,'# First scan to be read'
              WRITE(*,100) 'Last  scan to be read'
              WRITE(CDUMMY,*) NS1
              NS2=READILIM_B(CDUMMY,NS1,REDUCEME_NSCAN)
              WRITE(77,111) NS2,'# Last scan to be read'
            END IF
            !data
            IF(NS1.GT.1)THEN
              DO I=1,NS1-1 !spectra to be skipped
                READ(10) (SP(J),J=1,REDUCEME_NCHAN)
              END DO
            END IF
            DO J=1,REDUCEME_NCHAN
              SPSUM(J)=0.0
            END DO
            DO I=NS1,NS2
              READ(10) (SP(J),J=1,REDUCEME_NCHAN)
              DO J=1,REDUCEME_NCHAN
                SPSUM(J)=SPSUM(J)+SP(J)
              END DO
            END DO
            CLOSE(10)
            IF(NS2.GT.NS1)THEN !normalizamos
              DO J=1,REDUCEME_NCHAN
                SPSUM(J)=SPSUM(J)/REAL(NS2-NS1+1)
              END DO
            END IF
            !introducimos datos en el buffer
            DO J=1,REDUCEME_NCHAN
              IF(IXAXIS.EQ.1)THEN
                XDATA(J,NB)=REAL(J)
              ELSE
                XDATA(J,NB)=REDUCEME_STWV+REAL(J-1)*REDUCEME_DISP
              END IF
              EXDATA(J,NB)=0.
              YDATA(J,NB)=SPSUM(J)
              EYDATA(J,NB)=0.
              XYNAME(I,NB)=' '
            END DO
            NDATABUFF(NB)=REDUCEME_NCHAN
            LXERR(NB)=.FALSE.
            LYERR(NB)=.FALSE.
            ISTATUS=1
            WRITE(*,101) 'File read and closed!'
            !update limits
            CALL UPDATELIMITS(NB)
            !el tipo de simbolo por defecto es linea continua
            NSYMBBUFF(NB)=1001
            !actualizamos "key" del buffer
            WRITE(CDUMMY,'(I8,A1,I8)') NS1,':',NS2
            CALL RMBLANK(CDUMMY,CDUMMY,L0)
            L1=TRUELEN(INFILE)
            IF(L1.GT.50-L0)THEN
              DATAKEY(NB)=INFILE(1:50-L0)//'['//CDUMMY(1:L0)//']'
            ELSE
              DATAKEY(NB)=INFILE(1:L1)//'['//CDUMMY(1:L0)//']'
            END IF
            RETURN
          ELSE
            CLOSE(10)
          END IF
        ELSE
          CLOSE(10)
        END IF
        GOTO 7
6       CLOSE(10)
7       CONTINUE
C------------------------------------------------------------------------------
C Miramos si tiene formato FITS
        FITS_ISTATUS=0
        FITS_IREADWRITE=0 !modo READONLY
        CALL FTOPEN(10,INFILE,FITS_IREADWRITE,FITS_BLOCKSIZE,
     +   FITS_ISTATUS)
        !si no es FITS, cerramos el fichero y no hacemos nada
        IF(FITS_ISTATUS.NE.0)THEN
!         CALL FITS_PRINTERROR(FITS_ISTATUS)
!         FITS_ISTATUS=0
          CALL FTCLOS(10,FITS_ISTATUS)
        ELSE
          !es una imagen FITS; miramos si tiene extensiones
          WRITE(*,101) '>>> This file has FITS format!'
          CALL FTGKYL(10,'EXTEND',FITS_EXTEND,FITS_COMMENT,FITS_ISTATUS)
          IF(FITS_ISTATUS.EQ.202)THEN
            FITS_EXTEND=.FALSE.
            FITS_ISTATUS=0
          END IF
          IF(FITS_EXTEND)THEN
            WRITE(*,101) '***WARNING***'
            WRITE(*,101) '=> This file contains extensions'
            WRITE(*,100) 'Extension number to be read (1=primary) '
            NEW_HDU=READI_B('1')
            WRITE(77,111) NEW_HDU,'# Extension number to be read'
            CALL FTMAHD(10,NEW_HDU,HDUTYPE,FITS_ISTATUS)
          END IF
          !leemos BITPIX
          CALL FTGKYJ(10,'BITPIX',FITS_BITPIX,FITS_COMMENT,FITS_ISTATUS)
          !comprobamos que NAXIS=2
          CALL FTGKYJ(10,'NAXIS',NAXIS_(0),FITS_COMMENT,FITS_ISTATUS)
          IF(NAXIS_(0).GT.2)THEN
            WRITE(*,100) '=> NAXIS='
            WRITE(*,*) NAXIS_(0)
            WRITE(*,101) 'ERROR: NAXIS > 2'
            CALL FTCLOS(10,FITS_ISTATUS)
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          ELSEIF(NAXIS_(0).EQ.1)THEN
            NAXIS_(2)=1
          END IF
          !leemos NAXIS1 y NAXIS2
          CALL FTGKNJ(10,'NAXIS',1,2,NAXIS_(1),FITS_NFOUND,FITS_ISTATUS)
          WRITE(*,100) '>>> NAXIS2: '
          WRITE(*,*) NAXIS_(2)
          WRITE(*,100) '>>> NAXIS1: '
          WRITE(*,*) NAXIS_(1)
          IF(NAXIS_(1).GT.NDATAMAX)THEN
            CALL FTCLOS(10,FITS_ISTATUS)
            WRITE(*,100) '>>> NDATAMAX: '
            WRITE(*,*) NDATAMAX
            WRITE(*,101) 'ERROR: NAXIS1.GT.NDATAMAX'
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          END IF
          !CRPIX1
          CALL FTGKYE(10,'CRPIX1',CRPIX1,FITS_COMMENT,FITS_ISTATUS)
          IF(FITS_ISTATUS.EQ.0)THEN
            LCRPIX1=.TRUE.
            WRITE(*,100) '>>> CRPIX1: '
            WRITE(*,*) CRPIX1
          ELSE
            CRPIX1=0.0
            LCRPIX1=.FALSE.
            FITS_ISTATUS=0
          END IF
          !CRVAL1
          CALL FTGKYE(10,'CRVAL1',CRVAL1,FITS_COMMENT,FITS_ISTATUS)
          IF(FITS_ISTATUS.EQ.0)THEN
            LCRVAL1=.TRUE.
            WRITE(*,100) '>>> CRVAL1: '
            WRITE(*,*) CRVAL1
          ELSE
            CRVAL1=0.0
            LCRVAL1=.FALSE.
            FITS_ISTATUS=0
          END IF
          !CDELT1
          CALL FTGKYE(10,'CDELT1',CDELT1,FITS_COMMENT,FITS_ISTATUS)
          IF(FITS_ISTATUS.EQ.0)THEN
            LCDELT1=.TRUE.
            WRITE(*,100) '>>> CDELT1: '
            WRITE(*,*) CDELT1
          ELSE
            CDELT1=0.0
            LCDELT1=.FALSE.
            FITS_ISTATUS=0
          END IF
          !determinamos stwv y disp
          IF(LCRPIX1.AND.LCRVAL1.AND.LCDELT1)THEN
            IF(CRPIX1.EQ.1.0)THEN
              REDUCEME_STWV=CRVAL1
              REDUCEME_DISP=CDELT1
            ELSE
              REDUCEME_STWV=CRVAL1+CDELT1*(1.-CRPIX1)
              REDUCEME_DISP=CDELT1
            END IF
          ELSEIF(LCRVAL1.AND.LCDELT1)THEN
            REDUCEME_STWV=CRVAL1
            REDUCEME_DISP=CDELT1
          ELSE
            REDUCEME_STWV=0.
            REDUCEME_DISP=0.
          END IF
          !object
          CALL FTGKYS(10,'OBJECT',FITS_OBJECT,FITS_COMMENT,FITS_ISTATUS)
          WRITE(*,100) '>>> OBJECT: '
          IF(FITS_ISTATUS.EQ.202)THEN
            WRITE(*,101) '[not found]'
            FITS_ISTATUS=0
          ELSE
            L0=TRUELEN(FITS_OBJECT)
            WRITE(*,101) FITS_OBJECT(1:L0)
          END IF
          !se pide escale en el eje X
          WRITE(*,100) 'X-axis: 1=pixel, 2=wavelength '
          IXAXIS=READILIM_B('1',1,2)
          WRITE(77,111) IXAXIS,'# X-axis: 1=pixel, 2=wavelength'
          !se pide el numero de scan a leer
          IF(NAXIS_(2).EQ.1)THEN
            NS1=1
            NS2=1
          ELSE
            WRITE(*,*)
            WRITE(*,101) '* Define the scan region to be averaged:'
            WRITE(*,100) 'First scan to be read'
            NS1=READILIM_B('@',1,NAXIS_(2))
            WRITE(77,111) NS1,'# First scan to be read'
            WRITE(*,100) 'Last  scan to be read'
            WRITE(CDUMMY,*) NS1
            NS2=READILIM_B(CDUMMY,NS1,NAXIS_(2))
            WRITE(77,111) NS2,'# Last scan to be read'
          END IF
          !data
          DO J=1,NAXIS_(1)
            SPSUM(J)=0.0
          END DO
          DO I=NS1,NS2
            FITS_FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFE(10,1,FITS_FIRSTPIX,NAXIS_(1),SP,LSP,ANYNULL,
     +       FITS_ISTATUS)
            DO J=1,NAXIS_(1)
              SPSUM(J)=SPSUM(J)+SP(J)
            END DO
          END DO
          CALL FTCLOS(10,FITS_ISTATUS)
          IF(NS2.GT.NS1)THEN !normalizamos
            DO J=1,NAXIS_(1)
              SPSUM(J)=SPSUM(J)/REAL(NS2-NS1+1)
            END DO
          END IF
          !si ha habido algun error, lo mostramos
          IF(FITS_ISTATUS.GT.0)THEN
            CALL FITS_PRINTERROR(FITS_ISTATUS)
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          END IF
          !introducimos datos en el buffer
          DO J=1,NAXIS_(1)
            IF(IXAXIS.EQ.1)THEN
              XDATA(J,NB)=REAL(J)
            ELSE
              XDATA(J,NB)=REDUCEME_STWV+REAL(J-1)*REDUCEME_DISP
            END IF
            EXDATA(J,NB)=0.
            YDATA(J,NB)=SPSUM(J)
            EYDATA(J,NB)=0.
            XYNAME(I,NB)=' '
          END DO
          NDATABUFF(NB)=NAXIS_(1)
          ISTATUS=1
          WRITE(*,101) 'File read and closed!'
          !update limits
          CALL UPDATELIMITS(NB)
          !el tipo de simbolo por defecto es linea continua
          NSYMBBUFF(NB)=1001
          !actualizamos "key" del buffer
          WRITE(CDUMMY,'(I8,A1,I8)') NS1,':',NS2
          CALL RMBLANK(CDUMMY,CDUMMY,L0)
          L1=TRUELEN(INFILE)
          IF(L1.GT.50-L0)THEN
            DATAKEY(NB)=INFILE(1:50-L0)//'['//CDUMMY(1:L0)//']'
          ELSE
            DATAKEY(NB)=INFILE(1:L1)//'['//CDUMMY(1:L0)//']'
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
! No es un fichero con formato REDUCEME o FITS. Suponemos que es ASCII.
        OPEN(10,FILE=INFILE,STATUS='OLD',FORM='FORMATTED')
        WRITE(*,100) 'No. of initial rows to be skipped.......'
        NSKIP=READI_B('0')
        IF(NSKIP.LT.0) NSKIP=0
        WRITE(77,111) NSKIP,'# No. of initial rows to be skipped'
        IF(NSKIP.GT.0)THEN
          DO I=1,NSKIP
            READ(10,*,END=901)
          END DO
        END IF
        WRITE(*,100) 'No. of rows to be read (0=ALL)..........'
        NDATA=READI_B('0')
        WRITE(77,111) NDATA,'# No. of rows to be read (0=ALL)'
        IF(NDATA.GT.NDATAMAX)THEN
          WRITE(*,101) 'ERROR: this number of data is too large.'
          WRITE(*,101) 'You must modify the parameter NDATAMAX.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
C..............................................................................
        IF(IMODE.EQ.1)THEN !Y,EY
          LXERR(NB)=.FALSE.
          WRITE(*,100) 'Column No. for Y data.......................'
          NY=READI_B('@')
          WRITE(77,111) NY,'# Column No. for Y data'
          WRITE(*,100) 'Column No. for err(Y) data (0=NONE, -N) '
          NEY=READI_B('0')
          WRITE(77,111) NEY,'# Column No. for err(Y) data (0=none, -N)'
          IF(NEY.LT.0)THEN
            NEY=-NEY
            LNEXTROW=.TRUE.
          ELSE
            LNEXTROW=.FALSE.
          END IF
          LYERR(NB)=(NEY.GT.0)
C..............................................................................
        ELSE !X,EX,Y,EY,NAME
          WRITE(*,100) 'Column No. for X data.......................'
          NX=READI_B('@')
          WRITE(77,111) NX,'# Column No. for X data'
          WRITE(*,100) 'Column No. for err(X) data (0=NONE, -N) '
          NEX=READI_B('0')
          WRITE(77,111) NEX,'# Column No. for err(X) data (0=none, -N)'
          IF(NEX.LT.0)THEN
            NEX=-NEX
            LNEXTROW=.TRUE.
          ELSE
            LNEXTROW=.FALSE.
          END IF
          LXERR(NB)=(NEX.GT.0)
          WRITE(*,100) 'Column No. for Y data.......................'
          NY=READI_B('@')
          WRITE(77,111) NY,'# Column No. for Y data'
          WRITE(*,100) 'Column No. for err(Y) data (0=NONE, -N) '
          NEY=READI_B('0')
          WRITE(77,111) NEY,'# Column No. for err(Y) data (0=none, -N)'
          IF(NEY.NE.0)THEN
            LYERR(NB)=.TRUE.
            IF(LNEXTROW)THEN
              IF(NEY.LT.0)THEN
                NEY=-NEY
              ELSE
                WRITE(*,101) 'ERROR: err(Y) must be in different row!'
                WRITE(*,100) 'Press <CR> to continue'
                IF(LBATCH)THEN
                  WRITE(*,*)
                ELSE
                  READ(*,*)
                END IF
                RETURN
              END IF
            ELSE
              IF(NEY.LT.0)THEN
                IF(NEX.GT.0)THEN
                  WRITE(*,100) 'ERROR: '
                  WRITE(*,101) 'err(X) is not in different row!'
                  WRITE(*,100) 'Press <CR> to continue'
                  IF(LBATCH)THEN
                    WRITE(*,*)
                  ELSE
                    READ(*,*)
                  END IF
                  RETURN
                ELSE
                  NEY=-NEY
                  LNEXTROW=.TRUE.
                END IF
              END IF
            END IF
          ELSE
            LYERR(NB)=.FALSE.
          END IF
          WRITE(*,100) 'Column No. for names (0=NONE)...........'
          NNAME=READI_B('0')
          WRITE(77,111) NNAME,'# Column No. for names (0=none)'
          LXYNAME(NB)=(NNAME.GT.0)
        END IF
C------------------------------------------------------------------------------
        WRITE(*,100) 'Reading file...'
        NCOMMENTS=0
        I=0
10      READ(10,101,END=902) CLINEA
        IF(TRUELEN(CLINEA).EQ.0) GOTO 10 !saltamos lineas en blanco
        !sustituimos tabuladores por espacios en blanco
        IF(INDEX(CLINEA,CHAR(9)).NE.0) CALL CLEANTAB(CLINEA)
        IF(CLINEA(1:1).EQ.'#')THEN !ignora lineas con comentarios
          NCOMMENTS=NCOMMENTS+1
          GOTO 10
        END IF
        I=I+1
C..............................................................................
        IF(IMODE.EQ.1)THEN
C la variable X es el numero de dato
          XDATA(I,NB)=REAL(I)
C no hay error en esta variable
          EXDATA(I,NB)=0.
C leemos variable Y
          YDATA(I,NB)=FEXTRAE(CLINEA,NY,ISTATUSEXTRAE)
          IF(ISTATUSEXTRAE.EQ.0) GOTO 903
          IF(ISTATUSEXTRAE.EQ.-1)THEN
            I=I-1
            LUNREAD=.TRUE.
            GOTO 10
          END IF
C leemos error en variable Y (si procede)
          IF(NEY.GT.0)THEN
            IF(LNEXTROW)THEN !leemos siguiente linea (no en blanco)
              LREPEAT=.TRUE.
              DO WHILE(LREPEAT)
                READ(10,101,END=904) CLINEA
                IF(TRUELEN(CLINEA).NE.0) LREPEAT=.FALSE.
              END DO
            END IF
            EYDATA(I,NB)=FEXTRAE(CLINEA,NEY,ISTATUSEXTRAE)
            IF(ISTATUSEXTRAE.EQ.0) GOTO 903
            IF(ISTATUSEXTRAE.EQ.-1)THEN
              I=I-1
              LUNREAD=.TRUE.
              GOTO 10
            END IF
          ELSE
            EYDATA(I,NB)=0.
          END IF
C..............................................................................
        ELSEIF(IMODE.EQ.2)THEN
C leemos variable X
          XDATA(I,NB)=FEXTRAE(CLINEA,NX,ISTATUSEXTRAE)
          IF(ISTATUSEXTRAE.EQ.0) GOTO 903
          IF(ISTATUSEXTRAE.EQ.-1)THEN
            I=I-1
            LUNREAD=.TRUE.
            GOTO 10
          END IF
C leemos variable Y
          YDATA(I,NB)=FEXTRAE(CLINEA,NY,ISTATUSEXTRAE)
          IF(ISTATUSEXTRAE.EQ.0) GOTO 903
          IF(ISTATUSEXTRAE.EQ.-1)THEN
            I=I-1
            LUNREAD=.TRUE.
            GOTO 10
          END IF
C leemos el nombre (si procede)
          IF(NNAME.GT.0)THEN
            XYNAME(I,NB)=CEXTRAE(CLINEA,NNAME,ISTATUSEXTRAE)
            IF(ISTATUSEXTRAE.EQ.0) GOTO 903
          ELSE
            XYNAME(I,NB)=' '
          END IF
C leemos error en variable X (si procede)
          IF(NEX.GT.0)THEN
            IF(LNEXTROW)THEN !leemos siguiente linea (no en blanco)
              LREPEAT=.TRUE.
              DO WHILE(LREPEAT)
                READ(10,101,END=904) CLINEA
                IF(TRUELEN(CLINEA).NE.0) LREPEAT=.FALSE.
              END DO
            END IF
            EXDATA(I,NB)=FEXTRAE(CLINEA,NEX,ISTATUSEXTRAE)
            IF(ISTATUSEXTRAE.EQ.0) GOTO 903
            IF(ISTATUSEXTRAE.EQ.-1)THEN
              I=I-1
              LUNREAD=.TRUE.
              GOTO 10
            END IF
          ELSE
            EXDATA(I,NB)=0.
          END IF
C leemos error en variable Y (si procede)
          IF(NEY.GT.0)THEN
            IF(LNEXTROW)THEN !leemos siguiente linea (no en blanco)
              IF(NEX.EQ.0)THEN
                LREPEAT=.TRUE.
                DO WHILE(LREPEAT)
                  READ(10,101,END=904) CLINEA
                  IF(TRUELEN(CLINEA).NE.0) LREPEAT=.FALSE.
                END DO
              END IF
            END IF
            EYDATA(I,NB)=FEXTRAE(CLINEA,NEY,ISTATUSEXTRAE)
            IF(ISTATUSEXTRAE.EQ.0) GOTO 903
            IF(ISTATUSEXTRAE.EQ.-1)THEN
              I=I-1
              LUNREAD=.TRUE.
              GOTO 10
            END IF
          ELSE
            EYDATA(I,NB)=0.
          END IF
C..............................................................................
        END IF
C------------------------------------------------------------------------------
        IF(I.EQ.NDATA) GOTO 902
        GOTO 10
C------------------------------------------------------------------------------
901     CLOSE(10)
        WRITE(*,101) 'ERROR: unexpected end of file reached'
        WRITE(*,100) 'Press <CR> to continue...'
        IF(LBATCH)THEN
          WRITE(*,*)
        ELSE
          READ(*,*)
        END IF
        RETURN
C..............................................................................
902     CLOSE(10)
        IF(NDATA.EQ.0)THEN
          IF(I.EQ.0)THEN
            WRITE(*,*)
            WRITE(*,101) 'ERROR: unexpected end of file reached'
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          ELSE
            NDATABUFF(NB)=I
            ISTATUS=1
            WRITE(*,101) 'File read and closed!'
          END IF
        ELSE
          IF(I.NE.NDATA)THEN
            WRITE(*,*)
            WRITE(*,101) 'ERROR: unexpected end of file reached'
            WRITE(*,100) 'Press <CR> to continue...'
            IF(LBATCH)THEN
              WRITE(*,*)
            ELSE
              READ(*,*)
            END IF
            RETURN
          ELSE
            NDATABUFF(NB)=I
            ISTATUS=1
            WRITE(*,101) 'File read and closed!'
          END IF
        END IF
C calculamos los limites de los datos leidos
        CALL UPDATELIMITS(NB)
C mostramos datos basicos sobre los puntos leidos
        WRITE(*,100) '>>> No. of rows with comments (unread): '
        WRITE(*,*) NCOMMENTS
        WRITE(*,100) '>>> No. of rows read..................: '
        WRITE(*,*) NDATABUFF(NB)
        WRITE(*,100) '>>> Xmin..............................: '
        WRITE(*,*) XMINBUFF(NB)
        WRITE(*,100) '>>> Xmax..............................: '
        WRITE(*,*) XMAXBUFF(NB)
        WRITE(*,100) '>>> Ymin..............................: '
        WRITE(*,*) YMINBUFF(NB)
        WRITE(*,100) '>>> Ymax..............................: '
        WRITE(*,*) YMAXBUFF(NB)
        IF(LUNREAD)THEN
          WRITE(*,101) 'WARNING: there were unread data'
        END IF
C actualizamos "key" del buffer
        DATAKEY(NB)=INFILE(1:50)
        RETURN
C..............................................................................
903     CLOSE(10)
        WRITE(*,100) 'ERROR: while reading data #'
        WRITE(*,*) I+1
        WRITE(*,100) 'Press <CR> to continue...'
        IF(LBATCH)THEN
          WRITE(*,*)
        ELSE
          READ(*,*)
        END IF
        RETURN
C..............................................................................
904     CLOSE(10)
        WRITE(*,100) 'ERROR: while reading error in different row, '
        WRITE(*,100) 'data #'
        WRITE(*,*) I+1
        WRITE(*,100) 'Press <CR> to continue...'
        IF(LBATCH)THEN
          WRITE(*,*)
        ELSE
          READ(*,*)
        END IF
        RETURN
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
        END
