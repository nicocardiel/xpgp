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
        PROGRAM XPGP
        IMPLICIT NONE
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
        INCLUDE 'version.inc'
C
        INTEGER SYSTEMFUNCTION
        INTEGER TRUELEN,TRUEBEG
        INTEGER READI_B,READILIM_B
        REAL READF_B
        CHARACTER*255 READC_B
C
        INTEGER I
        INTEGER L1,L2,LMAX
        INTEGER NB,NB_,NB__,NBLOCAL
        INTEGER ID            !identificacion de la ventana grafica con botones
        INTEGER IDNEW
        INTEGER AXISJUST,AXISLW,AXISCF
        INTEGER IEXPAND
        INTEGER IDATAKEY
        INTEGER PGOPEN
        INTEGER ISTATUS
        INTEGER NCOLORBUFF(NBUFFMAX),NCOLORBUFF_
        INTEGER LWBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER IMODE
        INTEGER ISYSTEM
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER IOPCSAVE
        REAL XC,YC,XC1,XC2,YC1,YC2
        REAL XMIN,XMAX,YMIN,YMAX
        REAL AXISCH,FACTORCH
        REAL CHBUFF(NBUFFMAX)
        REAL XLABEL_D,XLABEL_C,XLABEL_J
        REAL YLABEL_D,YLABEL_C,YLABEL_J
        REAL GLABEL_D,GLABEL_C,GLABEL_J
        REAL DATAKEYCH,DATAKEYCHSYMB
        REAL FFACTOR
        REAL XDATA(NDATAMAX,NBUFFMAX),YDATA(NDATAMAX,NBUFFMAX)
        REAL EXDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        CHARACTER*1 CH,CCHANGE,CSAVE,COVER,CSET0,CAPEND,CSURE
        CHARACTER*5 VERSION_BATCH
        CHARACTER*20 XOPT,YOPT
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)  !OJO: tama~no igual que CEXTRAE
        CHARACTER*50 CBUTTON,CDUMMY
        CHARACTER*50 CAXISLW,CAXISCH,CEXPAND
        CHARACTER*50 DATAKEY(NBUFFMAX)
        CHARACTER*255 CBATCH
        CHARACTER*255 FILELOG,BATCHFILE
        CHARACTER*255 CXLABEL,CYLABEL,CGLABEL
        CHARACTER*255 NEWDEV,INFILE,OUTFILE
        LOGICAL LBATCH
        LOGICAL LBEXIST
        LOGICAL LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
        LOGICAL LBUFFER
        LOGICAL LXLABEL,LYLABEL,LGLABEL
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LEXIT,LIGNORE
        LOGICAL LXYNAME(NBUFFMAX)
        LOGICAL LOGFILE
        LOGICAL LLABELS
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LERRXNULL,LERRYNULL
        LOGICAL LANY
        LOGICAL LOOP
C
        COMMON/BLKINFILE/INFILE
        COMMON/BLKID/ID
        COMMON/BLKFACTORCH/FACTORCH
        COMMON/BLKSETTINGS0/LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKSETTINGS2/LBUFFER
        COMMON/BLKSETTINGS3/LXLABEL,LYLABEL,LGLABEL
        COMMON/BLKSETTINGS4/CXLABEL,CYLABEL,CGLABEL
        COMMON/BLKSETTINGS4A/XLABEL_D,XLABEL_C,XLABEL_J
        COMMON/BLKSETTINGS4B/YLABEL_D,YLABEL_C,YLABEL_J
        COMMON/BLKSETTINGS4C/GLABEL_D,GLABEL_C,GLABEL_J
        COMMON/BLKSETTINGS5/XOPT,YOPT
        COMMON/BLKSETTINGS6/AXISJUST,AXISLW,AXISCF,AXISCH
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKSETTINGS8A/IDATAKEY
        COMMON/BLKSETTINGS8B/DATAKEY
        COMMON/BLKSETTINGS8C/DATAKEYCH,DATAKEYCHSYMB
        COMMON/BLKBUFFERS1/LDEFBUFF,LUSEBUFF
        COMMON/BLKBUFFERS2/NCOLORBUFF
        COMMON/BLKBUFFERS3/NSYMBBUFF,LWBUFF
        COMMON/BLKBUFFERS4/CHBUFF
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKLXYNAME/LXYNAME
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKLABELS1/LLABELS
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C Abrimos fichero log de esta sesion
        OPEN(77,FILE='xpgp.log',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(77,101) '# xpgp version: '//VERSION
        LBATCH=.FALSE.
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C Indicamos si usamos un fichero log ya existente
        WRITE(*,100)'Previous log file, <CR>=none (wildcards allowed)? '
        READ(*,101) BATCHFILE
        LBATCH=(TRUELEN(BATCHFILE).GT.0)
        IF(LBATCH)THEN
          LOGFILE=.FALSE.
          DO WHILE(.NOT.LOGFILE)
            IF((INDEX(BATCHFILE,'*').NE.0).OR.
     +       (INDEX(BATCHFILE,'?').NE.0))THEN
              L1=TRUEBEG(BATCHFILE)
              L2=TRUELEN(BATCHFILE)
              ISYSTEM=SYSTEMFUNCTION('ls '//BATCHFILE(L1:L2)//
     +         ' | grep -v xpgp.log')
            ELSE
              INQUIRE(FILE=BATCHFILE,EXIST=LOGFILE)
              IF(.NOT.LOGFILE)THEN
                WRITE(*,100) 'ERROR: this file does not exist. '
                WRITE(*,101) 'Try again.'
                WRITE(*,100) 'Press <CR> to continue...'
                READ(*,*)
              END IF
            END IF
            IF(.NOT.LOGFILE)THEN
              WRITE(*,100) 'Previous log file '//
     >         '(<CR>=none, wildcards allowed)? '
              READ(*,101) BATCHFILE
            END IF
          END DO
          OPEN(78,FILE=BATCHFILE,STATUS='OLD',FORM='FORMATTED')
          READ(78,'(16X,A5)') VERSION_BATCH
          IF(VERSION_BATCH.NE.VERSION)THEN
            WRITE(*,101) '>>> Current version..: '//VERSION
            WRITE(*,101) '>>> Log. file version: '//VERSION_BATCH
            WRITE(*,101) 'WARNING: both versions do not match!'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          END IF
        END IF
C Abrimos la(s) salida(s) grafica(s) y establecemos regiones de dibujo/botones
        CALL RPGBEGOK('/Xserve',0)
        CALL PGQID(ID)
        CALL BUTTSPR(0.000,0.745,0.000,0.750)
        CALL BUTTSBR(0.000,1.000,0.000,1.000)
        CALL BUTTSXB(8)
        CALL BUTTSYB(25)
        CALL BUTTSCF(1)
C------------------------------------------------------------------------------
C Valores por defecto del dibujo
        LBUFFER=.FALSE.                       !no hay ningun buffer cargado aun
        LXLABEL=.FALSE.                !no se ha definido la etiqueta del eje X
        LYLABEL=.FALSE.                !no se ha definido la etiqueta del eje Y
        LGLABEL=.FALSE.             !no se ha definido la etiqueta de la figura
        CXLABEL='@'
        XLABEL_D=2.5                                        !distancia al eje X
        XLABEL_C=0.5                      !posicion del texto respecto al eje X
        XLABEL_J=0.5                                   !justificacion del texto
        CYLABEL='@'
        YLABEL_D=2.5                                        !distancia al eje Y
        YLABEL_C=0.5                      !posicion del texto respecto al eje Y
        YLABEL_J=0.5                                   !justificacion del texto
        CGLABEL='@'
        GLABEL_D=1.0                                        !distancia al eje X
        GLABEL_C=0.5                      !posicion del texto respecto al eje X
        GLABEL_J=0.5                                   !justificacion del texto
        XOPT='BCTSN'
        YOPT='BCTSN'
        AXISJUST=0                                              !JUST de PGPLOT
        AXISLW=2                     !anchura de las lineas en la caja del plot
        AXISCF=1                                     !tipo de FONT en etiquetas
        AXISCH=1.0                            !tama\~{n}o del FONT en etiquetas
        FACTORCH=1.35  !relacion entre FONT en \xserve con botones y postscript
        IEXPAND=10         !expansion en % de los limites en calculo automatico
        IDATAKEY=0 !indica si se muestra un KEY con los simbolos y los ficheros
        DO NB=1,NBUFFMAX
          DATAKEY(NB)='not defined'
        END DO
        DATAKEYCH=0.75                          !current height for text in KEY
        DATAKEYCHSYMB=0.0                     !current height for symbol in KEY
        CALL PLOT_SETTINGS
        LXMINFIX=.FALSE.
        LXMAXFIX=.FALSE.
        LYMINFIX=.FALSE.
        LYMAXFIX=.FALSE.
        LLABELS=.FALSE.
        INFILE='none'
C------------------------------------------------------------------------------
C De momento no hay ningun buffer cargado
        DO NB=1,NBUFFMAX
          LDEFBUFF(NB)=.FALSE.
          LUSEBUFF(NB)=.FALSE.
          NCOLORBUFF(NB)=NB+1
          LWBUFF(NB)=2
          CHBUFF(NB)=1.6
          NSYMBBUFF(NB)=17
        END DO
        CALL SHOW_BUFFERS
C------------------------------------------------------------------------------
C distintas formas de cargar datos en los buffers
        CALL BUTTON( 9,'[f]ile: Xi,Yi',0)
        CALL BUTTON(10,'file: (N) Yi',0)
        CALL BUTTON(11,'f(Xij,Yij)',0)
        CALL BUTTON(11,'f(Xij,Yij)',3)
        CALL BUTTON(12,'f(x)',0)
        CALL BUTTON(13,'# >>> #',0)
        CALL BUTTON(13,'# >>> #',3)
C edicion de datos de los buffers
        CALL BUTTON(14,'edit data',0)
        CALL BUTTON(14,'edit data',3)
C ajustes
        CALL BUTTON(15,'fit data',0)
        CALL BUTTON(15,'fit data',3)
C salvar datos
        CALL BUTTON(16,'sa[v]e buff.',0)
        CALL BUTTON(16,'sa[v]e buff.',3)
C caracteristicas de los simbolos
        CALL BUTTON(41,'color index',0)
        CALL BUTTON(41,'color index',3)
        CALL BUTTON(42,'line width',0)
        CALL BUTTON(42,'line width',3)
        CALL BUTTON(43,'height',0)
        CALL BUTTON(43,'height',3)
        CALL BUTTON(44,'symbols',0)
        CALL BUTTON(44,'symbols',3)
        CALL BUTTON(45,'line type',0)
        CALL BUTTON(45,'line type',3)
        CALL BUTTON(46,'file labels',0)
        CALL BUTTON(46,'file labels',3)
C manejo de errores
        CALL BUTTON(47,'errors',0)
        CALL BUTTON(47,'errors',3)
C boton sin definir
        CALL BUTTON(48,' ',0)
        CALL BUTTON(48,' ',3)
C generacion de fichero PostScript
        CALL BUTTON(55,'[P]ostScript',0)
        CALL BUTTON(55,'[P]ostScript',3)
C actualizacion de imagen cuando se ha producido algun cambio
        CALL BUTTON(56,'[U]pdate',0)
        CALL BUTTON(56,'[U]pdate',3)
C zoom y unzoom de plots
        CALL BUTTON(63,'[Z]oom',0)
        CALL BUTTON(63,'[Z]oom',3)
        CALL BUTTON(64,'[W]hole',0)
        CALL BUTTON(64,'[W]hole',3)
C estadistica
        CALL BUTTON(159,'statistics',0)
        CALL BUTTON(159,'statistics',3)
        CALL BUTTON(160,'randomize',0)
        CALL BUTTON(160,'randomize',3)
        CALL BUTTON(167,'X histogram',0)
        CALL BUTTON(167,'X histogram',3)
        CALL BUTTON(168,'Y histogram',0)
        CALL BUTTON(168,'Y histogram',3)
        CALL BUTTON(167,' ',0)
        CALL BUTTON(167,' ',3)
        CALL BUTTON(168,' ',0)
        CALL BUTTON(168,' ',3)
        CALL BUTTON(175,'norm.ranges',0)
        CALL BUTTON(175,'norm.ranges',3)
        CALL BUTTON(176,' ',0)
        CALL BUTTON(176,' ',3)
        CALL BUTTON(183,' ',0)
        CALL BUTTON(183,' ',3)
        CALL BUTTON(184,' ',0)
        CALL BUTTON(184,' ',3)
        CALL BUTTON(191,' ',0)
        CALL BUTTON(191,' ',3)
        CALL BUTTON(192,' ',0)
        CALL BUTTON(192,' ',3)
        CALL BUTTON(199,' ',0)
        CALL BUTTON(199,' ',3)
C fin del programa
        CALL BUTTON(200,'[Q]UIT',0)
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C Bucle principal
        LEXIT=.FALSE.
        DO WHILE(.NOT.LEXIT)
          IF(LBATCH)THEN
            LOOP=.TRUE.
            DO WHILE(LOOP)
              READ(78,101) CBATCH
              IF(CBATCH(1:1).NE.'#') LOOP=.FALSE.
            END DO
            IF(CBATCH(1:18).EQ.'END_of_xpgp.log')THEN
              CLOSE(78)
              NB=-1
              LBATCH=.FALSE.
            ELSE
              READ(CBATCH,*) NB
            END IF
          ELSE
            CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
            CALL IFBUTTON(XC,YC,NB)
            CALL CHUPPER(CH)
C..........................0000000001111
C..........................1234567890123
            NBLOCAL=INDEX('12345678PUZWQFV',CH)
            IF(NBLOCAL.EQ.9)THEN
              NBLOCAL=55
            ELSEIF(NBLOCAL.EQ.10)THEN
              NBLOCAL=56
            ELSEIF(NBLOCAL.EQ.11)THEN
              NBLOCAL=63
            ELSEIF(NBLOCAL.EQ.12)THEN
              NBLOCAL=64
            ELSEIF(NBLOCAL.EQ.13)THEN
              NBLOCAL=200
            ELSEIF(NBLOCAL.EQ.14)THEN
              NBLOCAL=9
            ELSEIF(NBLOCAL.EQ.15)THEN
              NBLOCAL=16
            END IF
            IF(NBLOCAL.NE.0)THEN
              CALL BUTTQEX(NBLOCAL,LBEXIST)
              IF(LBEXIST) NB=NBLOCAL
            END IF
          END IF
C..............................................................................
          IF(NB.EQ.0)THEN
            CALL FIND_NEAREST(XC,YC)
C..............................................................................
          ELSEIF((NB.GE.1).AND.(NB.LE.NBUFFMAX))THEN
            CALL TOLOG77(NB,'Select buffer <buffer #>')
            IF(.NOT.LDEFBUFF(NB))THEN
              WRITE(*,101) 'WARNING: this buffer is undefined'
            ELSE
              IF(LUSEBUFF(NB))THEN
                LUSEBUFF(NB)=.FALSE.
              ELSE
                LUSEBUFF(NB)=.TRUE.
              END IF
              CALL BUTTON(56,'[U]pdate',-5)        !hay que actualizar graficos
              CALL BUTTON(55,'[P]ostScript',3)         !desactivamos PostScript
              CALL SHOW_BUFFERS
            END IF
C..............................................................................
          ELSEIF(NB.EQ.9)THEN
            CALL TOLOG77(NB,'Open new file <file: Xi,Yi>')
            CALL BUTTON(9,'[f]ile: Xi,Yi',5)
            WRITE(*,100) 'Select buffer...'
            NB_=0
            DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB_)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB_)
                NBLOCAL=INDEX('12345678',CH)
                IF(NBLOCAL.NE.0)THEN
                  CALL BUTTQEX(NBLOCAL,LBEXIST)
                  NB_=NBLOCAL
                END IF
              END IF
            END DO
            WRITE(77,111) NB_,'# Selected buffer number'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
C
            WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB_,']'
            IF(NCOLORBUFF(NB_).NE.0)THEN
              CALL BUTTON(NB_,CBUTTON,-NCOLORBUFF(NB_)-1)
            ELSE
              CALL BUTTON(NB_,CBUTTON,2)
            END IF
C
            IF(LDEFBUFF(NB_))THEN
              WRITE(*,100) 'WARNING: this buffer has already '
              WRITE(*,101) 'been defined'
              WRITE(*,100) 'Do you want to change its content (y/n) '
              CCHANGE(1:1)=READC_B('n','yn')
              WRITE(77,112) CCHANGE,'# change buffer content'
              LIGNORE=(CCHANGE.EQ.'n')
            ELSE
              LIGNORE=.FALSE.
            END IF
C
            IF(.NOT.LIGNORE)THEN
              IMODE=2
              CALL LEENEWFILE(NB_,IMODE,ISTATUS)
              IF(ISTATUS.NE.0)THEN
                LDEFBUFF(NB_)=.TRUE.
                LUSEBUFF(NB_)=.TRUE.
                IF(.NOT.LBUFFER)THEN
                  LBUFFER=.TRUE.
                  CALL BUTTON(11,'f(Xij,Yij)',0)
                  CALL BUTTON(13,'# >>> #',0)
                  CALL BUTTON(14,'edit data',0)
                  CALL BUTTON(15,'fit data',0)
                  CALL BUTTON(16,'sa[v]e buff.',0)
                  CALL BUTTON(41,'color index',0)
                  CALL BUTTON(42,'line width',0)
                  CALL BUTTON(43,'height',0)
                  CALL BUTTON(44,'symbols',0)
                  CALL BUTTON(45,'line type',0)
                  CALL BUTTON(46,'file labels',0)
                  CALL BUTTON(47,'errors',0)
                  CALL BUTTON(159,'statistics',0)
                  CALL BUTTON(160,'randomize',0)
                  CALL BUTTON(167,'X histogram',0)
                  CALL BUTTON(168,'Y histogram',0)
                  CALL BUTTON(175,'norm.ranges',0)
                END IF
                CALL BUTTON(56,'[U]pdate',-5)
                CALL BUTTON(55,'[P]ostScript',3)
              END IF
            END IF
C
            CALL SHOW_BUFFERS
            CALL BUTTON(9,'[f]ile: Xi,Yi',0)
C..............................................................................
          ELSEIF(NB.EQ.10)THEN
            CALL TOLOG77(NB,'Open new file <file: (N) Yi>')
            CALL BUTTON(10,'file: (N) Yi',5)
            WRITE(*,100) 'Select buffer...'
            NB_=0
            DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB_)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB_)
                NBLOCAL=INDEX('12345678',CH)
                IF(NBLOCAL.NE.0)THEN
                  CALL BUTTQEX(NBLOCAL,LBEXIST)
                  NB_=NBLOCAL
                END IF
              END IF
            END DO
            WRITE(77,111) NB_,'# Selected buffer number'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
C
            WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB_,']'
            IF(NCOLORBUFF(NB_).NE.0)THEN
              CALL BUTTON(NB_,CBUTTON,-NCOLORBUFF(NB_)-1)
            ELSE
              CALL BUTTON(NB_,CBUTTON,2)
            END IF
C
            IF(LDEFBUFF(NB_))THEN
              WRITE(*,100) 'WARNING: this buffer has already '
              WRITE(*,101) 'been defined'
              WRITE(*,100) 'Do you want to change its content (y/n) '
              CCHANGE(1:1)=READC_B('n','yn')
              WRITE(77,112) CCHANGE,'# change buffer content'
              LIGNORE=(CCHANGE.EQ.'n')
            ELSE
              LIGNORE=.FALSE.
            END IF
C
            IF(.NOT.LIGNORE)THEN
              IMODE=1
              CALL LEENEWFILE(NB_,IMODE,ISTATUS)
              IF(ISTATUS.NE.0)THEN
                LDEFBUFF(NB_)=.TRUE.
                LUSEBUFF(NB_)=.TRUE.
                IF(.NOT.LBUFFER)THEN
                  LBUFFER=.TRUE.
                  CALL BUTTON(11,'f(Xij,Yij)',0)
                  CALL BUTTON(13,'# >>> #',0)
                  CALL BUTTON(14,'edit data',0)
                  CALL BUTTON(15,'fit data',0)
                  CALL BUTTON(16,'sa[v]e buff.',0)
                  CALL BUTTON(41,'color index',0)
                  CALL BUTTON(42,'line width',0)
                  CALL BUTTON(43,'height',0)
                  CALL BUTTON(44,'symbols',0)
                  CALL BUTTON(45,'line type',0)
                  CALL BUTTON(46,'file labels',0)
                  CALL BUTTON(47,'errors',0)
                  CALL BUTTON(159,'statistics',0)
                  CALL BUTTON(160,'randomize',0)
                  CALL BUTTON(167,'X histogram',0)
                  CALL BUTTON(168,'Y histogram',0)
                  CALL BUTTON(175,'norm.ranges',0)
                END IF
                CALL BUTTON(56,'[U]pdate',-5)
                CALL BUTTON(55,'[P]ostScript',3)
              END IF
            END IF
C
            CALL SHOW_BUFFERS
            CALL BUTTON(10,'file: (N) Yi',0)
C..............................................................................
          ELSEIF(NB.EQ.11)THEN
            CALL TOLOG77(NB,'Function with buffers <f(Xij,Yij)>')
            CALL BUTTON(11,'f(Xij,Yij)',5)
            CALL EXEC_FUNCTION1(ISTATUS)
            IF(ISTATUS.EQ.0)THEN
            ELSE
              CALL SHOW_BUFFERS
              CALL BUTTON(56,'[U]pdate',-5)        !hay que actualizar graficos
              CALL BUTTON(55,'[P]ostScript',3)         !desactivamos PostScript
            END IF
C
            CALL BUTTON(11,'f(Xij,Yij)',0)
C..............................................................................
          ELSEIF(NB.EQ.12)THEN
            CALL TOLOG77(NB,'Function (generic) <f(x)>')
            CALL BUTTON(12,'f(x)',5)
C
            WRITE(*,100) 'Select destination buffer...'
            NB_=0
            DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB_)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB_)
              END IF
              IF((NB_.GE.1).AND.(NB_.LE.NBUFFMAX))THEN
                IF(.NOT.LDEFBUFF(NB_))THEN
                  WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB_,']'
                  CALL BUTTON(NB_,CBUTTON,5)
                  IF(NCOLORBUFF(NB_).NE.0)THEN
                    CALL BUTTON(NB_,CBUTTON,-NCOLORBUFF(NB_)-1)
                  ELSE
                    CALL BUTTON(NB_,CBUTTON,2)
                  END IF
                  LDEFBUFF(NB_)=.TRUE.
                  LUSEBUFF(NB_)=.TRUE.
                END IF
              END IF
            END DO
            WRITE(77,111) NB_,'# Destination buffer for function'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
C
            CALL EXEC_FUNCTION2(ISTATUS,NB_)
            IF(ISTATUS.EQ.0)THEN
            ELSE
              IF(.NOT.LBUFFER)THEN
                LBUFFER=.TRUE.
                CALL BUTTON(11,'f(Xij,Yij)',0)
                CALL BUTTON(13,'# >>> #',0)
                CALL BUTTON(14,'edit data',0)
                CALL BUTTON(15,'fit data',0)
                CALL BUTTON(16,'sa[v]e buff.',0)
                CALL BUTTON(41,'color index',0)
                CALL BUTTON(42,'line width',0)
                CALL BUTTON(43,'height',0)
                CALL BUTTON(44,'symbols',0)
                CALL BUTTON(45,'line type',0)
                CALL BUTTON(46,'file labels',0)
                CALL BUTTON(47,'errors',0)
                CALL BUTTON(159,'statistics',0)
                CALL BUTTON(160,'randomize',0)
                CALL BUTTON(167,'X histogram',0)
                CALL BUTTON(168,'Y histogram',0)
                CALL BUTTON(175,'norm.ranges',0)
              END IF
              CALL BUTTON(56,'[U]pdate',-5)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL SHOW_BUFFERS
            END IF
C
            CALL BUTTON(12,'f(x)',0)
C..............................................................................
          ELSEIF(NB.EQ.13)THEN
            CALL TOLOG77(NB,'Copy buffer <# >>> #>')
            CALL BUTTON(13,'# >>> #',5)
C
            WRITE(*,100) 'Select initial buffer...'
            NB_=0
            DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB_)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB_)
              END IF
              IF(.NOT.LDEFBUFF(NB_)) NB_=0
            END DO
            WRITE(77,111) NB_,'# Initial buffer'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
C
            WRITE(*,100) 'Select destination buffer...'
            NB__=0
            DO WHILE((NB__.LT.1).OR.(NB__.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB__)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB__)
              END IF
              IF(NB__.EQ.NB_) NB__=0
            END DO
            WRITE(77,111) NB__,'# Destination buffer'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB__,' selected'
            WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB__,']'
            IF(NCOLORBUFF(NB__).NE.0)THEN
              CALL BUTTON(NB__,CBUTTON,-NCOLORBUFF(NB__)-1)
            ELSE
              CALL BUTTON(NB__,CBUTTON,2)
            END IF
C
            IF(LDEFBUFF(NB__))THEN
              IF(NDATABUFF(NB_)+NDATABUFF(NB__).LE.NDATAMAX)THEN
                WRITE(*,101) '(1) replace full buffer'
                WRITE(*,101) '(2) append data to existing buffer'
                WRITE(*,100) 'Option (1/2) '
                CAPEND(1:1)=READC_B('@','12')
                WRITE(77,112) CAPEND,
     +           '# 1=replace full buffer, 2=append data'
              ELSE
                CAPEND='1'
              END IF
            ELSE
              CAPEND='1'
            END IF
C
            CALL COPYDATA(NB_,NB__,CAPEND)
            CALL BUTTON(56,'[U]pdate',-5)          !hay que actualizar graficos
            CALL BUTTON(55,'[P]ostScript',3)           !desactivamos PostScript
C
            CALL SHOW_BUFFERS
            CALL BUTTON(13,'# >>> #',0)
C..............................................................................
          ELSEIF(NB.EQ.14)THEN
            CALL TOLOG77(NB,'Edit data <edit data>')
            CALL BUTTON(14,'edit data',5)
C
            CALL SELBUFFER(NB_)
C
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select action...'
              CALL BUTTON(14,'<<< back',0)
              CALL BUTTON(14,'<<< back',-3)
              CALL BUTTON(22,'change 1 p.',0)
              CALL BUTTON(30,'remove 1 p.',0)
              CALL BUTTON(38,'remove region',0)
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,'# 14=back, 22=change 1 p., '//
     +           '30=remove 1 p., 38=remove region'
                IF(NB__.EQ.14)THEN
                  WRITE(*,101) '...OK! Edition cancelled!'
                ELSEIF(NB__.EQ.22)THEN
                  CALL BUTTON(22,'change 1 p.',5)
                  CALL BUTTON(14,'<<< back',3)
                  CALL EDITOR(NB_,NB__)
                ELSEIF(NB__.EQ.30)THEN
                  CALL BUTTON(30,'remove 1 p.',5)
                  CALL BUTTON(14,'<<< back',3)
                  CALL EDITOR(NB_,NB__)
                ELSEIF(NB__.EQ.38)THEN
                  CALL BUTTON(38,'remove region',5)
                  CALL BUTTON(14,'<<< back',3)
                  CALL EDITOR(NB_,NB__)
                ELSE
                  NB__=0
                END IF
              END DO
              CALL SHOW_BUFFERS
              LANY=.FALSE.
              DO NB__=1,NBUFFMAX
                IF(LDEFBUFF(NB__)) LANY=.TRUE.
              END DO
              IF(LANY)THEN
                CALL BUTTON(56,'[U]pdate',-5)
              ELSE
                CALL BUTTON(56,'[U]pdate',3)
                CALL BUTTON(11,'f(Xij,Yij)',3)
                CALL BUTTON(13,'# >>> #',3)
                CALL BUTTON(14,'edit data',3)
                CALL BUTTON(15,'fit data',3)
                CALL BUTTON(16,'sa[v]e buff.',3)
                CALL BUTTON(41,'color index',3)
                CALL BUTTON(42,'line width',3)
                CALL BUTTON(43,'height',3)
                CALL BUTTON(44,'symbols',3)
                CALL BUTTON(45,'line type',3)
                CALL BUTTON(46,'file labels',3)
                CALL BUTTON(47,'errors',3)
                CALL BUTTON(63,'[Z]oom',3)
                CALL BUTTON(64,'[W]hole',3)
                CALL BUTTON(159,'statistics',3)
                CALL BUTTON(160,'randomize',0)
                CALL BUTTON(167,'X histogram',0)
                CALL BUTTON(168,'Y histogram',0)
                CALL BUTTON(175,'norm.ranges',0)
                LBUFFER=.FALSE.
                CALL PLOT_SETTINGS
              END IF
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(22,'change 1 p.',-1)
              CALL BUTTON(30,'remove 1 p.',-1)
              CALL BUTTON(38,'remove region',-1)
            END IF
C
            CALL BUTTON(14,'edit data',0)
C..............................................................................
          ELSEIF(NB.EQ.15)THEN
            CALL TOLOG77(NB,'Compute fits to data <fit data>')
            CALL BUTTON(15,'fit data',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              IF(LUSEBUFF(NB_))THEN
                WRITE(*,100) 'Select type of fit...'
                CALL BUTTON(15,'<<< back',0)
                CALL BUTTON(15,'<<< back',-3)
                CALL BUTTON(23,'linear reg.',0)
                CALL BUTTON(31,'polynomial',0)
                CALL BUTTON(39,'other...',0)
                NB__=0
                DO WHILE(NB__.EQ.0)
                  IF(LBATCH)THEN
                    CALL READ_NB(NB__)
                  ELSE
                    CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                    CALL IFBUTTON(XC,YC,NB__)
                  END IF
                  WRITE(77,111) NB__,'# 15=back, 23=lin.reg., '//
     +             '31=poly., 39=other'
                  IF(NB__.EQ.15)THEN
                    WRITE(*,101) '...OK! Fit cancelled!'
                  ELSEIF(NB__.EQ.23)THEN
                    CALL BUTTON(23,'linear reg.',5)
                    CALL BUTTON(15,'<<< back',3)
                    CALL SLINREG(NB_)
                    CALL SHOW_BUFFERS
                  ELSEIF(NB__.EQ.31)THEN
                    CALL BUTTON(31,'polynomial',5)
                    CALL BUTTON(15,'<<< back',3)
                    CALL SFITPOL(NB_)
                  ELSEIF(NB__.EQ.39)THEN
                    CALL BUTTON(39,'other...',5)
                    CALL BUTTON(15,'<<< back',3)
                    CALL OTHERFIT(NB_)
                  ELSE
                    NB__=0
                  END IF
                END DO
                CALL BUTTON(56,'[U]pdate',-5)
                CALL BUTTON(55,'[P]ostScript',3)
                CALL BUTTON(23,'linear reg.',-1)
                CALL BUTTON(31,'polinomyal',-1)
                CALL BUTTON(39,'other...',-1)
              ELSE
                WRITE(*,101) 'ERROR: selected buffer must be activated.'
                WRITE(*,100) 'Press <CR> to continue...'
                IF(LBATCH)THEN
                  WRITE(*,*)
                ELSE
                  READ(*,*)
                END IF
              END IF
            END IF
C
            CALL BUTTON(15,'fit data',0)
C..............................................................................
          ELSEIF(NB.EQ.16)THEN
            CALL TOLOG77(NB,'Save data in buffer <save buffer>')
            CALL BUTTON(16,'sa[v]e buff.',5)
            WRITE(*,100) 'Select buffer...'
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Output file name'
              OUTFILE(1:255)=READC_B('@','@')
              L1=TRUEBEG(OUTFILE)
              L2=TRUELEN(OUTFILE)
              CALL TOLOG77_STRING(OUTFILE(L1:L2),'Output file name')
              OPEN(10,FILE=OUTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
              IF(LXYNAME(NB_))THEN
                WRITE(*,101) '1) NAME X EX Y EY'
                WRITE(*,101) '2) NAME X Y'
                WRITE(*,101) '3) X EX Y EY'
                WRITE(*,101) '4) X Y'
                WRITE(*,100) 'Option '
                IOPCSAVE=READILIM_B('1',1,4)
                WRITE(*,100) 'Saving file...'
                WRITE(77,111) IOPCSAVE,'# Selection of NAME X EX Y EY'
                IF((IOPCSAVE.EQ.1).OR.(IOPCSAVE.EQ.2))THEN
                  LMAX=0        !determinamos la longitud maxima de las cadenas
                  DO I=1,NDATABUFF(NB_)
                    L1=TRUEBEG(XYNAME(I,NB_))
                    L2=TRUELEN(XYNAME(I,NB_))
                    IF(L2-L1+1.GT.LMAX) LMAX=L2-L1+1
                  END DO
                  DO I=1,NDATABUFF(NB_)
                    L1=TRUEBEG(XYNAME(I,NB_))
                    WRITE(10,100) XYNAME(I,NB_)(L1:L1+LMAX-1)
                    WRITE(10,100) '  '
                    IF(IOPCSAVE.EQ.1)THEN
                      WRITE(10,*)XDATA(I,NB_),EXDATA(I,NB_),
     +                 YDATA(I,NB_),EYDATA(I,NB_)
                    ELSE
                      WRITE(10,*)XDATA(I,NB_),YDATA(I,NB_)
                    END IF
                  END DO
                ELSE
                  DO I=1,NDATABUFF(NB_)
                    IF(IOPCSAVE.EQ.3)THEN
                      WRITE(10,*)XDATA(I,NB_),EXDATA(I,NB_),
     +                 YDATA(I,NB_),EYDATA(I,NB_)
                    ELSE
                      WRITE(10,*)XDATA(I,NB_),YDATA(I,NB_)
                    END IF
                  END DO
                END IF
              ELSE
                WRITE(*,101) '1) X EX Y EY'
                WRITE(*,101) '2) X Y'
                WRITE(*,100) 'Option '
                IOPCSAVE=READILIM_B('1',1,2)
                WRITE(77,111) IOPCSAVE,'# Selection of X EX Y EY'
                WRITE(*,100) 'Saving file...'
                IF(IOPCSAVE.EQ.1)THEN
                  DO I=1,NDATABUFF(NB_)
                    WRITE(10,*) XDATA(I,NB_),EXDATA(I,NB_),
     +               YDATA(I,NB_),EYDATA(I,NB_)
                  END DO
                ELSE
                  DO I=1,NDATABUFF(NB_)
                    WRITE(10,*) XDATA(I,NB_),YDATA(I,NB_)
                  END DO
                END IF
              END IF
              CLOSE(10)
              WRITE(*,101) '...OK! File saved and closed.'
            END IF
C
            CALL BUTTON(16,'sa[v]e buff.',0)
C..............................................................................
          ELSEIF(NB.EQ.41)THEN
            CALL TOLOG77(NB,'Change color <color index>')
            CALL BUTTON(41,'color index',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select color...'
              CALL BUTTON(17,'color 2',0)
              CALL BUTTON(18,'color 3',0)
              CALL BUTTON(19,'color 4',0)
              CALL BUTTON(20,'color 5',0)
              CALL BUTTON(21,'color 6',0)
              CALL BUTTON(22,'color 7',0)
              CALL BUTTON(25,'color 8',0)
              CALL BUTTON(26,'color 9',0)
              CALL BUTTON(27,'color 10',0)
              CALL BUTTON(28,'color 11',0)
              CALL BUTTON(29,'color 13',0)
              CALL BUTTON(30,'number...',0)
C
              CALL BUTTON(17,'color 2',-3)
              CALL BUTTON(18,'color 3',-4)
              CALL BUTTON(19,'color 4',-5)
              CALL BUTTON(20,'color 5',-6)
              CALL BUTTON(21,'color 6',-7)
              CALL BUTTON(22,'color 7',-8)
              CALL BUTTON(25,'color 8',-9)
              CALL BUTTON(26,'color 9',-10)
              CALL BUTTON(27,'color 10',-11)
              CALL BUTTON(28,'color 11',-12)
              CALL BUTTON(29,'color 13',-14)
C
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,'# Selected color (button number!)'
                IF(NB__.EQ.17)THEN
                  NCOLORBUFF(NB_)=2
                ELSEIF(NB__.EQ.18)THEN
                  NCOLORBUFF(NB_)=3
                ELSEIF(NB__.EQ.19)THEN
                  NCOLORBUFF(NB_)=4
                ELSEIF(NB__.EQ.20)THEN
                  NCOLORBUFF(NB_)=5
                ELSEIF(NB__.EQ.21)THEN
                  NCOLORBUFF(NB_)=6
                ELSEIF(NB__.EQ.22)THEN
                  NCOLORBUFF(NB_)=7
                ELSEIF(NB__.EQ.25)THEN
                  NCOLORBUFF(NB_)=8
                ELSEIF(NB__.EQ.26)THEN
                  NCOLORBUFF(NB_)=9
                ELSEIF(NB__.EQ.27)THEN
                  NCOLORBUFF(NB_)=10
                ELSEIF(NB__.EQ.28)THEN
                  NCOLORBUFF(NB_)=11
                ELSEIF(NB__.EQ.29)THEN
                  NCOLORBUFF(NB_)=13
                ELSEIF(NB__.EQ.30)THEN
                  WRITE(*,*)
                  NCOLORBUFF_=NCOLORBUFF(NB_)
                  WRITE(CDUMMY,*) NCOLORBUFF_
                  WRITE(*,100) 'New CI for symbols '
                  NCOLORBUFF_=READI_B(CDUMMY)
                  WRITE(77,111) NCOLORBUFF_,'# Selected color number'
                  NCOLORBUFF(NB_)=NCOLORBUFF_
                ELSE
                  NB__=0
                END IF
              END DO
              WRITE(*,100) '--> CI set to '
              WRITE(*,*) NCOLORBUFF(NB_)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL SHOW_BUFFERS
              CALL BUTTON(17,'color 2',-1)
              CALL BUTTON(18,'color 3',-1)
              CALL BUTTON(19,'color 4',-1)
              CALL BUTTON(20,'color 5',-1)
              CALL BUTTON(21,'color 6',-1)
              CALL BUTTON(22,'color 7',-1)
              CALL BUTTON(25,'color 8',-1)
              CALL BUTTON(26,'color 9',-1)
              CALL BUTTON(27,'color 10',-1)
              CALL BUTTON(28,'color 11',-1)
              CALL BUTTON(29,'color 13',-1)
              CALL BUTTON(30,'number...',-1)
            END IF
C
            CALL BUTTON(41,'color index',0)
C..............................................................................
          ELSEIF(NB.EQ.42)THEN
            CALL TOLOG77(NB,'Change line width <line width>')
            CALL BUTTON(42,'line width',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select line width...'
              CALL BUTTON(17,'width 1',0)
              CALL BUTTON(18,'width 2',0)
              CALL BUTTON(19,'width 3',0)
              CALL BUTTON(20,'width 4',0)
              CALL BUTTON(21,'width 5',0)
              CALL BUTTON(22,'width 6',0)
              CALL BUTTON(25,'width 7',0)
              CALL BUTTON(26,'width 8',0)
              CALL BUTTON(27,'width 9',0)
              CALL BUTTON(28,'width 10',0)
              CALL BUTTON(29,'width 11',0)
              CALL BUTTON(30,'width 12',0)
              CALL BUTTON(33,'width 13',0)
              CALL BUTTON(34,'width 14',0)
              CALL BUTTON(35,'width 15',0)
              CALL BUTTON(36,'width 16',0)
              CALL BUTTON(37,'width 17',0)
              CALL BUTTON(38,'number...',0)
C
              IF(LWBUFF(NB_).EQ.1)THEN
                CALL BUTTON(17,'width 1',1)
              ELSEIF(LWBUFF(NB_).EQ.2)THEN
                CALL BUTTON(18,'width 2',1)
              ELSEIF(LWBUFF(NB_).EQ.3)THEN
                CALL BUTTON(19,'width 3',1)
              ELSEIF(LWBUFF(NB_).EQ.4)THEN
                CALL BUTTON(20,'width 4',1)
              ELSEIF(LWBUFF(NB_).EQ.5)THEN
                CALL BUTTON(21,'width 5',1)
              ELSEIF(LWBUFF(NB_).EQ.6)THEN
                CALL BUTTON(22,'width 6',1)
              ELSEIF(LWBUFF(NB_).EQ.7)THEN
                CALL BUTTON(25,'width 7',1)
              ELSEIF(LWBUFF(NB_).EQ.8)THEN
                CALL BUTTON(26,'width 8',1)
              ELSEIF(LWBUFF(NB_).EQ.9)THEN
                CALL BUTTON(27,'width 9',1)
              ELSEIF(LWBUFF(NB_).EQ.10)THEN
                CALL BUTTON(28,'width 10',1)
              ELSEIF(LWBUFF(NB_).EQ.11)THEN
                CALL BUTTON(29,'width 11',1)
              ELSEIF(LWBUFF(NB_).EQ.12)THEN
                CALL BUTTON(30,'width 12',1)
              ELSEIF(LWBUFF(NB_).EQ.13)THEN
                CALL BUTTON(33,'width 13',1)
              ELSEIF(LWBUFF(NB_).EQ.14)THEN
                CALL BUTTON(34,'width 14',1)
              ELSEIF(LWBUFF(NB_).EQ.15)THEN
                CALL BUTTON(35,'width 15',1)
              ELSEIF(LWBUFF(NB_).EQ.16)THEN
                CALL BUTTON(36,'width 16',1)
              ELSEIF(LWBUFF(NB_).EQ.17)THEN
                CALL BUTTON(37,'width 17',1)
              END IF
C
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,
     +           '# Selected line width (button number!)'
                IF(NB__.EQ.17)THEN
                  LWBUFF(NB_)=1
                ELSEIF(NB__.EQ.18)THEN
                  LWBUFF(NB_)=2
                ELSEIF(NB__.EQ.19)THEN
                  LWBUFF(NB_)=3
                ELSEIF(NB__.EQ.20)THEN
                  LWBUFF(NB_)=4
                ELSEIF(NB__.EQ.21)THEN
                  LWBUFF(NB_)=5
                ELSEIF(NB__.EQ.22)THEN
                  LWBUFF(NB_)=6
                ELSEIF(NB__.EQ.25)THEN
                  LWBUFF(NB_)=7
                ELSEIF(NB__.EQ.26)THEN
                  LWBUFF(NB_)=8
                ELSEIF(NB__.EQ.27)THEN
                  LWBUFF(NB_)=9
                ELSEIF(NB__.EQ.28)THEN
                  LWBUFF(NB_)=10
                ELSEIF(NB__.EQ.29)THEN
                  LWBUFF(NB_)=11
                ELSEIF(NB__.EQ.30)THEN
                  LWBUFF(NB_)=12
                ELSEIF(NB__.EQ.33)THEN
                  LWBUFF(NB_)=13
                ELSEIF(NB__.EQ.34)THEN
                  LWBUFF(NB_)=14
                ELSEIF(NB__.EQ.35)THEN
                  LWBUFF(NB_)=15
                ELSEIF(NB__.EQ.36)THEN
                  LWBUFF(NB_)=16
                ELSEIF(NB__.EQ.37)THEN
                  LWBUFF(NB_)=17
                ELSEIF(NB__.EQ.38)THEN
                  WRITE(*,*)
                  WRITE(CDUMMY,*) LWBUFF(NB_)
                  WRITE(*,100) 'New LW for symbols '
                  LWBUFF(NB_)=READI_B(CDUMMY)
                  WRITE(77,111) LWBUFF(NB_),'# Selected line width'
                ELSE
                  NB__=0
                END IF
              END DO
              WRITE(*,100) '--> LW set to '
              WRITE(*,*) LWBUFF(NB_)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL SHOW_BUFFERS
              CALL BUTTON(17,'width 1',-1)
              CALL BUTTON(18,'width 2',-1)
              CALL BUTTON(19,'width 3',-1)
              CALL BUTTON(20,'width 4',-1)
              CALL BUTTON(21,'width 5',-1)
              CALL BUTTON(22,'width 6',-1)
              CALL BUTTON(25,'width 7',-1)
              CALL BUTTON(26,'width 8',-1)
              CALL BUTTON(27,'width 9',-1)
              CALL BUTTON(28,'width 10',-1)
              CALL BUTTON(29,'width 11',-1)
              CALL BUTTON(30,'width 12',-1)
              CALL BUTTON(33,'width 13',-1)
              CALL BUTTON(34,'width 14',-1)
              CALL BUTTON(35,'width 15',-1)
              CALL BUTTON(36,'width 16',-1)
              CALL BUTTON(37,'width 17',-1)
              CALL BUTTON(38,'number...',-1)
            END IF
C
            CALL BUTTON(42,'line width',0)
C..............................................................................
          ELSEIF(NB.EQ.43)THEN
            CALL TOLOG77(NB,'Change current height <height>')
            CALL BUTTON(43,'height',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select height...'
              CALL BUTTON(17,'0.2',0)
              CALL BUTTON(18,'0.4',0)
              CALL BUTTON(19,'0.6',0)
              CALL BUTTON(20,'0.8',0)
              CALL BUTTON(21,'1.0',0)
              CALL BUTTON(22,'1.2',0)
              CALL BUTTON(25,'1.4',0)
              CALL BUTTON(26,'1.6',0)
              CALL BUTTON(27,'1.8',0)
              CALL BUTTON(28,'2.0',0)
              CALL BUTTON(29,'2.2',0)
              CALL BUTTON(30,'2.4',0)
              CALL BUTTON(33,'2.6',0)
              CALL BUTTON(34,'2.8',0)
              CALL BUTTON(35,'3.0',0)
              CALL BUTTON(36,'3.5',0)
              CALL BUTTON(37,'4.0',0)
              CALL BUTTON(38,'number...',0)
C
              IF(CHBUFF(NB_).EQ.0.2)THEN
                CALL BUTTON(17,'0.2',1)
              ELSEIF(CHBUFF(NB_).EQ.0.4)THEN
                CALL BUTTON(18,'0.4',1)
              ELSEIF(CHBUFF(NB_).EQ.0.6)THEN
                CALL BUTTON(19,'0.6',1)
              ELSEIF(CHBUFF(NB_).EQ.0.8)THEN
                CALL BUTTON(20,'0.8',1)
              ELSEIF(CHBUFF(NB_).EQ.1.0)THEN
                CALL BUTTON(21,'1.0',1)
              ELSEIF(CHBUFF(NB_).EQ.1.2)THEN
                CALL BUTTON(22,'1.2',1)
              ELSEIF(CHBUFF(NB_).EQ.1.4)THEN
                CALL BUTTON(25,'1.4',1)
              ELSEIF(CHBUFF(NB_).EQ.1.6)THEN
                CALL BUTTON(26,'1.6',1)
              ELSEIF(CHBUFF(NB_).EQ.1.8)THEN
                CALL BUTTON(27,'1.8',1)
              ELSEIF(CHBUFF(NB_).EQ.2.0)THEN
                CALL BUTTON(28,'2.0',1)
              ELSEIF(CHBUFF(NB_).EQ.2.2)THEN
                CALL BUTTON(29,'2.2',1)
              ELSEIF(CHBUFF(NB_).EQ.2.4)THEN
                CALL BUTTON(30,'2.4',1)
              ELSEIF(CHBUFF(NB_).EQ.2.6)THEN
                CALL BUTTON(33,'2.6',1)
              ELSEIF(CHBUFF(NB_).EQ.2.8)THEN
                CALL BUTTON(34,'2.8',1)
              ELSEIF(CHBUFF(NB_).EQ.3.0)THEN
                CALL BUTTON(35,'3.0',1)
              ELSEIF(CHBUFF(NB_).EQ.3.5)THEN
                CALL BUTTON(36,'3.5',1)
              ELSEIF(CHBUFF(NB_).EQ.4.0)THEN
                CALL BUTTON(37,'4.0',1)
              END IF
C
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,'# Selected CH (button number!)'
                IF(NB__.EQ.17)THEN
                  CHBUFF(NB_)=0.2
                ELSEIF(NB__.EQ.18)THEN
                  CHBUFF(NB_)=0.4
                ELSEIF(NB__.EQ.19)THEN
                  CHBUFF(NB_)=0.6
                ELSEIF(NB__.EQ.20)THEN
                  CHBUFF(NB_)=0.8
                ELSEIF(NB__.EQ.21)THEN
                  CHBUFF(NB_)=1.0
                ELSEIF(NB__.EQ.22)THEN
                  CHBUFF(NB_)=1.2
                ELSEIF(NB__.EQ.25)THEN
                  CHBUFF(NB_)=1.4
                ELSEIF(NB__.EQ.26)THEN
                  CHBUFF(NB_)=1.6
                ELSEIF(NB__.EQ.27)THEN
                  CHBUFF(NB_)=1.8
                ELSEIF(NB__.EQ.28)THEN
                  CHBUFF(NB_)=2.0
                ELSEIF(NB__.EQ.29)THEN
                  CHBUFF(NB_)=2.2
                ELSEIF(NB__.EQ.30)THEN
                  CHBUFF(NB_)=2.4
                ELSEIF(NB__.EQ.33)THEN
                  CHBUFF(NB_)=2.6
                ELSEIF(NB__.EQ.34)THEN
                  CHBUFF(NB_)=2.8
                ELSEIF(NB__.EQ.35)THEN
                  CHBUFF(NB_)=3.0
                ELSEIF(NB__.EQ.36)THEN
                  CHBUFF(NB_)=3.5
                ELSEIF(NB__.EQ.37)THEN
                  CHBUFF(NB_)=4.0
                ELSEIF(NB__.EQ.38)THEN
                  WRITE(*,*)
                  WRITE(CDUMMY,*) CHBUFF(NB_)
                  WRITE(*,100) 'New CH for symbols '
                  CHBUFF(NB_)=READF_B(CDUMMY)
                  WRITE(77,111) CHBUFF(NB_),'# Selected CH for symbols'
                ELSE
                  NB__=0
                END IF
              END DO
              WRITE(*,100) '--> CH set to '
              WRITE(*,*) CHBUFF(NB_)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL SHOW_BUFFERS
              CALL BUTTON(17,'0.2',-1)
              CALL BUTTON(18,'0.4',-1)
              CALL BUTTON(19,'0.6',-1)
              CALL BUTTON(20,'0.8',-1)
              CALL BUTTON(21,'1.0',-1)
              CALL BUTTON(22,'1.2',-1)
              CALL BUTTON(25,'1.4',-1)
              CALL BUTTON(26,'1.6',-1)
              CALL BUTTON(27,'1.8',-1)
              CALL BUTTON(28,'2.0',-1)
              CALL BUTTON(29,'2.2',-1)
              CALL BUTTON(30,'2.4',-1)
              CALL BUTTON(33,'2.6',-1)
              CALL BUTTON(34,'2.8',-1)
              CALL BUTTON(35,'3.0',-1)
              CALL BUTTON(36,'3.5',-1)
              CALL BUTTON(37,'4.0',-1)
              CALL BUTTON(38,'number...',-1)
            END IF
C
            CALL BUTTON(43,'height',0)
C..............................................................................
          ELSEIF(NB.EQ.44)THEN
            CALL TOLOG77(NB,'Change current symbol <symbols>')
            CALL BUTTON(44,'symbols',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select current symbol number...'
              CALL BUTTON(17,CHAR(01),0)
              CALL BUTTON(18,CHAR(02),0)
              CALL BUTTON(19,CHAR(03),0)
              CALL BUTTON(20,CHAR(04),0)
              CALL BUTTON(21,CHAR(05),0)
              CALL BUTTON(22,CHAR(06),0)
              CALL BUTTON(25,CHAR(07),0)
              CALL BUTTON(26,CHAR(11),0)
              CALL BUTTON(27,CHAR(13),0)
              CALL BUTTON(28,CHAR(14),0)
              CALL BUTTON(29,CHAR(12),0)
              CALL BUTTON(30,CHAR(16),0)
              CALL BUTTON(33,CHAR(17),0)
              CALL BUTTON(34,CHAR(18),0)
              CALL BUTTON(35,CHAR(20),0)
              CALL BUTTON(36,CHAR(21),0)
              CALL BUTTON(37,CHAR(22),0)
              CALL BUTTON(38,'number...',0)
C
              IF(NSYMBBUFF(NB_).EQ.1)THEN
                CALL BUTTON(17,CHAR(1),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.2)THEN
                CALL BUTTON(18,CHAR(2),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.3)THEN
                CALL BUTTON(19,CHAR(3),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.4)THEN
                CALL BUTTON(20,CHAR(4),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.5)THEN
                CALL BUTTON(21,CHAR(5),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.6)THEN
                CALL BUTTON(22,CHAR(6),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.7)THEN
                CALL BUTTON(25,CHAR(7),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.11)THEN
                CALL BUTTON(26,CHAR(11),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.13)THEN
                CALL BUTTON(27,CHAR(13),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.14)THEN
                CALL BUTTON(28,CHAR(14),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.12)THEN
                CALL BUTTON(29,CHAR(12),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.16)THEN
                CALL BUTTON(30,CHAR(16),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.17)THEN
                CALL BUTTON(33,CHAR(17),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.18)THEN
                CALL BUTTON(34,CHAR(18),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.20)THEN
                CALL BUTTON(35,CHAR(20),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.21)THEN
                CALL BUTTON(36,CHAR(21),1)
              ELSEIF(NSYMBBUFF(NB_).EQ.22)THEN
                CALL BUTTON(37,CHAR(22),1)
              END IF
C
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,'# Selected symbol (button number!)'
                IF(NB__.EQ.17)THEN
                  NSYMBBUFF(NB_)=1
                ELSEIF(NB__.EQ.18)THEN
                  NSYMBBUFF(NB_)=2
                ELSEIF(NB__.EQ.19)THEN
                  NSYMBBUFF(NB_)=3
                ELSEIF(NB__.EQ.20)THEN
                  NSYMBBUFF(NB_)=4
                ELSEIF(NB__.EQ.21)THEN
                  NSYMBBUFF(NB_)=5
                ELSEIF(NB__.EQ.22)THEN
                  NSYMBBUFF(NB_)=6
                ELSEIF(NB__.EQ.25)THEN
                  NSYMBBUFF(NB_)=7
                ELSEIF(NB__.EQ.26)THEN
                  NSYMBBUFF(NB_)=11
                ELSEIF(NB__.EQ.27)THEN
                  NSYMBBUFF(NB_)=13
                ELSEIF(NB__.EQ.28)THEN
                  NSYMBBUFF(NB_)=14
                ELSEIF(NB__.EQ.29)THEN
                  NSYMBBUFF(NB_)=12
                ELSEIF(NB__.EQ.30)THEN
                  NSYMBBUFF(NB_)=16
                ELSEIF(NB__.EQ.33)THEN
                  NSYMBBUFF(NB_)=17
                ELSEIF(NB__.EQ.34)THEN
                  NSYMBBUFF(NB_)=18
                ELSEIF(NB__.EQ.35)THEN
                  NSYMBBUFF(NB_)=20
                ELSEIF(NB__.EQ.36)THEN
                  NSYMBBUFF(NB_)=21
                ELSEIF(NB__.EQ.37)THEN
                  NSYMBBUFF(NB_)=22
                ELSEIF(NB__.EQ.38)THEN
                  WRITE(*,*)
                  WRITE(CDUMMY,*) NSYMBBUFF(NB_)
                  WRITE(*,100) 'New symbol number '
                  NSYMBBUFF(NB_)=READF_B(CDUMMY)
                  WRITE(77,111) NSYMBBUFF(NB_),'# Selected symbol'
                ELSE
                  NB__=0
                END IF
              END DO
              WRITE(*,100) '--> NSYMB set to '
              WRITE(*,*) NSYMBBUFF(NB_)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL SHOW_BUFFERS
              CALL BUTTON(17,CHAR(01),-1)
              CALL BUTTON(18,CHAR(02),-1)
              CALL BUTTON(19,CHAR(03),-1)
              CALL BUTTON(20,CHAR(04),-1)
              CALL BUTTON(21,CHAR(05),-1)
              CALL BUTTON(22,CHAR(06),-1)
              CALL BUTTON(25,CHAR(07),-1)
              CALL BUTTON(26,CHAR(11),-1)
              CALL BUTTON(27,CHAR(13),-1)
              CALL BUTTON(28,CHAR(14),-1)
              CALL BUTTON(29,CHAR(12),-1)
              CALL BUTTON(30,CHAR(16),-1)
              CALL BUTTON(33,CHAR(17),-1)
              CALL BUTTON(34,CHAR(18),-1)
              CALL BUTTON(35,CHAR(20),-1)
              CALL BUTTON(36,CHAR(21),-1)
              CALL BUTTON(37,CHAR(22),-1)
              CALL BUTTON(38,'number...',-1)
            END IF
C
            CALL BUTTON(44,'symbols',0)
C..............................................................................
          ELSEIF(NB.EQ.45)THEN
            CALL TOLOG77(NB,'Change current line <line type>')
            CALL BUTTON(45,'line type',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              WRITE(*,100) 'Select current line type or special'
              WRITE(*,100) ' symbol...'
              CALL BUTTON(17, '\(796)\(796)\(796)\(796)',0)
              CALL BUTTON(18, '\(800) \(800) \(800) \(800)',0)
              CALL BUTTON(19, '\(800) \(729) \(800) \(729)',0)
              CALL BUTTON(20, '\(729)\(729)\(729)\(729)\(729)'//
     +         '\(729)\(729)\(729)\(729)\(729)\(729)\(729)',0)
              CALL BUTTON(21, '\(800) \(729)\(729)\(729) \(800) '//
     +         '\(729)\(729)\(729)',0)
              CALL BUTTON(25,'data #',0)
              CALL BUTTON(26,'name',0)
              IF(.NOT.LXYNAME(NB_)) CALL BUTTON(26,'name',3)
C
              IF(NSYMBBUFF(NB_).EQ.100)THEN
                CALL BUTTON(25,'data #',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.200)THEN
              CALL BUTTON(26,'name',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.1001)THEN
                CALL BUTTON(17, '\(796)\(796)\(796)\(796)',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.1002)THEN
                CALL BUTTON(18, '\(800) \(800) \(800) \(800)',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.1003)THEN
                CALL BUTTON(19, '\(800) \(729) \(800) \(729)',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.1004)THEN
                CALL BUTTON(20, '\(729)\(729)\(729)\(729)\(729)'//
     +           '\(729)\(729)\(729)\(729)\(729)\(729)\(729)',1)
              ELSEIF(NSYMBBUFF(NB_).EQ.1005)THEN
                CALL BUTTON(21, '\(800) \(729)\(729)\(729) '//
     +           '\(800) \(729)\(729)\(729)',1)
              END IF
C
              NB__=0
              DO WHILE(NB__.EQ.0)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,
     +           '# Selected line type (button number!)'
                IF(NB__.EQ.17)THEN
                  NSYMBBUFF(NB_)=1001
                ELSEIF(NB__.EQ.18)THEN
                  NSYMBBUFF(NB_)=1002
                ELSEIF(NB__.EQ.19)THEN
                  NSYMBBUFF(NB_)=1003
                ELSEIF(NB__.EQ.20)THEN
                  NSYMBBUFF(NB_)=1004
                ELSEIF(NB__.EQ.21)THEN
                  NSYMBBUFF(NB_)=1005
                ELSEIF(NB__.EQ.25)THEN
                  NSYMBBUFF(NB_)=100
                ELSEIF(NB__.EQ.26)THEN
                  NSYMBBUFF(NB_)=200
                ELSE
                  NB__=0
                END IF
              END DO
              WRITE(*,101) '   ...OK!'
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL SHOW_BUFFERS
              CALL BUTTON(17, '\(796)\(796)\(796)\(796)',-1)
              CALL BUTTON(18, '\(800) \(800) \(800) \(800)',-1)
              CALL BUTTON(19, '\(800) \(729) \(800) \(729)',-1)
              CALL BUTTON(20, '\(729)\(729)\(729)\(729)\(729)'//
     +         '\(729)\(729)\(729)\(729)\(729)\(729)\(729)',-1)
              CALL BUTTON(21, '\(800) \(729)\(729)\(729) \(800) '//
     +         '\(729)\(729)\(729)',-1)
              CALL BUTTON(25,'data #',-1)
              CALL BUTTON(26,'name',-1)
            END IF
C
            CALL BUTTON(45,'line type',0)
C..............................................................................
          ELSEIF(NB.EQ.46)THEN
            CALL TOLOG77(NB,'Open file with labels <file labels>')
            CALL BUTTON(46,'file labels',5)
            IF(LLABELS)THEN
              LLABELS=.FALSE.
              CALL BUTTON(56,'[U]pdate',-5)
              CALL BUTTON(55,'[P]ostScript',3)
            ELSE
              CALL LABELS(0)
            END IF
            IF(LLABELS)THEN
              CALL BUTTON(46,'file labels',0)
              CALL BUTTON(46,'file labels',1)
              CALL BUTTON(56,'[U]pdate',-5)
              CALL BUTTON(55,'[P]ostScript',3)
            ELSE
              CALL BUTTON(46,'file labels',0)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.47)THEN
            CALL TOLOG77(NB,'<errors>')
            CALL BUTTON(47,'errors',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              CALL GIVE_ERRORS(NB_,LERRXNULL,LERRYNULL)
C
              NB__=0
              DO WHILE(NB__.NE.47)
                CALL BUTTON(23,'err X',0)
                CALL BUTTON(31,'+ err X',0)
                CALL BUTTON(31,'+ err X',3)
                CALL BUTTON(39,'err X = 0',0)
                CALL BUTTON(39,'err X = 0',3)
                IF(LXERR(NB_))THEN
                  CALL BUTTON(23,'err X',5)
                  CALL BUTTON(31,'+ err X',0)
                  CALL BUTTON(39,'err X = 0',0)
                END IF
                CALL BUTTON(24,'err Y',0)
                CALL BUTTON(32,'+ err Y',0)
                CALL BUTTON(32,'+ err Y',3)
                CALL BUTTON(40,'err Y = 0',0)
                CALL BUTTON(40,'err Y = 0',3)
                IF(LYERR(NB_))THEN
                  CALL BUTTON(24,'err Y',5)
                  CALL BUTTON(32,'+ err Y',0)
                  CALL BUTTON(40,'err Y = 0',0)
                END IF
                CALL BUTTON(47,'<<< back',0)
                CALL BUTTON(47,'<<< back',-3)
                IF(LBATCH)THEN
                  CALL READ_NB(NB__)
                ELSE
                  CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                  CALL IFBUTTON(XC,YC,NB__)
                END IF
                WRITE(77,111) NB__,
     +           '# Selected error manipulation (button number!)'
                IF(NB__.EQ.23)THEN
                  IF(LXERR(NB_))THEN
                    LXERR(NB_)=.FALSE.
                  ELSE
                    LXERR(NB_)=.TRUE.
                  END IF
                ELSEIF(NB__.EQ.24)THEN
                  IF(LYERR(NB_))THEN
                    LYERR(NB_)=.FALSE.
                  ELSE
                    LYERR(NB_)=.TRUE.
                  END IF
                ELSEIF(NB__.EQ.31)THEN
                  CALL BUTTON(31,'+ err X',5)
                  WRITE(*,100) 'Factor to be added quadratically '
                  WRITE(*,100) 'to err X'
                  FFACTOR=READF_B('@')
                  WRITE(77,*) FFACTOR,
     +             '# Factor to be added quadratically to err X'
                  DO I=1,NDATABUFF(NB_)
                    EXDATA(I,NB_)=EXDATA(I,NB_)*EXDATA(I,NB_)+
     +               FFACTOR*FFACTOR
                    EXDATA(I,NB_)=SQRT(EXDATA(I,NB_))
                  END DO
                ELSEIF(NB__.EQ.32)THEN
                  CALL BUTTON(32,'+ err Y',5)
                  WRITE(*,100) 'Factor to be added quadratically '
                  WRITE(*,100) 'to err Y'
                  FFACTOR=READF_B('@')
                  WRITE(77,*) FFACTOR,
     +             '# Factor to be added quadratically to err Y'
                  DO I=1,NDATABUFF(NB_)
                    EYDATA(I,NB_)=EYDATA(I,NB_)*EYDATA(I,NB_)+
     +               FFACTOR*FFACTOR
                    EYDATA(I,NB_)=SQRT(EYDATA(I,NB_))
                  END DO
                ELSEIF(NB__.EQ.39)THEN
                  CALL BUTTON(39,'err X = 0',5)
                  WRITE(*,100) 'Do you want to set err X = 0 '
                  WRITE(*,100) '(y/n) '
                  CSET0(1:1)=READC_B('y','yn')
                  WRITE(77,112) CSET0,'# set err X = 0'
                  IF(CSET0.EQ.'y')THEN
                    DO I=1,NDATABUFF(NB_)
                      EXDATA(I,NB_)=0.
                    END DO
                  END IF
                ELSEIF(NB__.EQ.40)THEN
                  CALL BUTTON(40,'err Y = 0',5)
                  WRITE(*,100) 'Do you want to set err Y = 0 '
                  WRITE(*,100) '(y/n) '
                  CSET0(1:1)=READC_B('y','yn')
                  WRITE(77,112) CSET0,'# set err Y = 0'
                  IF(CSET0.EQ.'y')THEN
                    DO I=1,NDATABUFF(NB_)
                      EYDATA(I,NB_)=0.
                    END DO
                  END IF
                END IF
              END DO
C
              CALL UPDATELIMITS(NB_)
C
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
C
              CALL BUTTON(23, 'err X',-1)
              CALL BUTTON(24, 'err Y',-1)
              CALL BUTTON(31,'+ err X',-1)
              CALL BUTTON(32,'+ err Y',-1)
              CALL BUTTON(39,'err X = 0',-1)
              CALL BUTTON(40,'err Y = 0',-1)
            END IF
C
            CALL BUTTON(47,'errors',0)
C..............................................................................
          ELSEIF(NB.EQ.63)THEN
            CALL TOLOG77(NB,'<Zoom>')
            CALL BUTTON(63,'[Z]oom',5)
C
            CALL ONLYONE(NB_)
            IF(NB_.EQ.-1)THEN
              WRITE(*,101) 'ERROR: you must activate at least one '//
     >         'buffer.'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
            ELSE
              WRITE(*,101)'Press cursor at two corners of the imaginary'
     +         //' BOX to be zoomed'
              IF(LBATCH)THEN
                LOOP=.TRUE.
                DO WHILE(LOOP)
                  READ(78,101) CBATCH
                  IF(CBATCH(1:1).NE.'#') LOOP=.FALSE.
                END DO
                READ(CBATCH,*) XC1,YC1
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC1,YC1,CH)
              END IF
              WRITE(77,*) XC1,YC1,'# first corner for zoom'
              IF(XC1.LT.XMIN) XC1=XMIN
              IF(XC1.GT.XMAX) XC1=XMAX
              IF(YC1.LT.YMIN) YC1=YMIN
              IF(YC1.GT.YMAX) YC1=YMAX
              IF(LBATCH)THEN
                LOOP=.TRUE.
                DO WHILE(LOOP)
                  READ(78,101) CBATCH
                  IF(CBATCH(1:1).NE.'#') LOOP=.FALSE.
                END DO
                READ(CBATCH,*) XC2,YC2
              ELSE
                CALL PGSCI(4)
                CALL RPGBAND(2,0,XC1,YC1,XC2,YC2,CH)
                CALL PGSCI(1)
              END IF
              WRITE(77,*) XC2,YC2,'# second corner for zoom'
              IF(XC2.LT.XMIN) XC2=XMIN
              IF(XC2.GT.XMAX) XC2=XMAX
              IF(YC2.LT.YMIN) YC2=YMIN
              IF(YC2.GT.YMAX) YC2=YMAX
              XMIN=AMIN1(XC1,XC2)
              XMAX=AMAX1(XC1,XC2)
              YMIN=AMIN1(YC1,YC2)
              YMAX=AMAX1(YC1,YC2)
C
              LXMINFIX=.TRUE.
              LXMAXFIX=.TRUE.
              LYMINFIX=.TRUE.
              LYMAXFIX=.TRUE.
              CALL UPDATEPLOT(.FALSE.,ISTATUS)
              IF(ISTATUS.EQ.0)THEN
              ELSE
                CALL PLOT_SETTINGS
              END IF
            END IF
            CALL BUTTON(63,'[Z]oom',0)
C..............................................................................
          ELSEIF(NB.EQ.64)THEN
            CALL TOLOG77(NB,'Replot (unzoom) <Whole>')
            CALL BUTTON(64,'[W]hole',5)
            CALL ONLYONE(NB_)
            IF(NB_.EQ.-1)THEN
              WRITE(*,101) 'ERROR: you must activate at least one '//
     >         'buffer.'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
            ELSE
              LXMINFIX=.FALSE.
              LXMAXFIX=.FALSE.
              LYMINFIX=.FALSE.
              LYMAXFIX=.FALSE.
              CALL UPDATEPLOT(.FALSE.,ISTATUS)
              IF(ISTATUS.EQ.0)THEN
              ELSE
                CALL PLOT_SETTINGS
              END IF
            END IF
            CALL BUTTON(64,'[W]hole',0)
C..............................................................................
          ELSEIF(NB.EQ.71)THEN
            CALL TOLOG77(NB,'Define Xmin <Xmin>')
            IF(LXMINFIX)THEN
              LXMINFIX=.FALSE.
            ELSE
              CALL BUTTON(71,'Xmin:',5)
              WRITE(*,100) 'New Xmin '
              WRITE(CDUMMY,*) XMIN
              XMIN=READF_B(CDUMMY)
              WRITE(77,*) XMIN,'# New Xmin'
              LXMINFIX=.TRUE.
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.79)THEN
            CALL TOLOG77(NB,'Define Xmax <Xmax>')
            IF(LXMAXFIX)THEN
              LXMAXFIX=.FALSE.
            ELSE
              CALL BUTTON(79,'Xmax:',5)
              WRITE(*,100) 'New Xmax '
              WRITE(CDUMMY,*) XMAX
              XMAX=READF_B(CDUMMY)
              WRITE(77,*) XMAX,'# New Xmax'
              LXMAXFIX=.TRUE.
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.87)THEN
            CALL TOLOG77(NB,'Define Ymin <Ymin>')
            IF(LYMINFIX)THEN
              LYMINFIX=.FALSE.
            ELSE
              CALL BUTTON(87,'Ymin:',5)
              WRITE(*,100) 'New Ymin '
              WRITE(CDUMMY,*) YMIN
              YMIN=READF_B(CDUMMY)
              WRITE(77,*) YMIN,'# New Ymin'
              LYMINFIX=.TRUE.
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.95)THEN
            CALL TOLOG77(NB,'Define Ymax <Ymax>')
            IF(LYMAXFIX)THEN
              LYMAXFIX=.FALSE.
            ELSE
              CALL BUTTON(95,'Ymax:',5)
              WRITE(*,100) 'New Ymax '
              WRITE(CDUMMY,*) YMAX
              YMAX=READF_B(CDUMMY)
              WRITE(77,*) YMAX,'# New Ymax'
              LYMAXFIX=.TRUE.
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.55)THEN
            CALL TOLOG77(NB,'Generate PostScript file <PostScript>')
            CALL BUTTON(55,'[P]ostScript',5)
            WRITE(*,100) 'New graphics device '
            NEWDEV(1:255)=READC_B('/cps','@')
            CALL TOLOG77_STRING(NEWDEV(1:TRUELEN(NEWDEV)),
     +       'New graphics device')
            IDNEW=PGOPEN(NEWDEV)
            IF(IDNEW.LE.0)THEN
              WRITE(*,101) 'ERROR: invalid graphics device.'
              WRITE(*,100) 'Press <CR> to continue...'
              IF(LBATCH)THEN
                WRITE(*,*)
              ELSE
                READ(*,*)
              END IF
            ELSE
              WRITE(*,100) 'Creating new plot...'
              CALL UPDATEPLOT(.TRUE.,ISTATUS)
              CALL PGCLOS(IDNEW)
              WRITE(*,101) '   ...OK! Plot created and closed'
              CALL PGSLCT(ID)
            END IF
            CALL BUTTON(55,'[P]ostScript',0)
            CALL BUTTON(55,'[P]ostScript',-4)
C..............................................................................
          ELSEIF(NB.EQ.56)THEN
            CALL TOLOG77(NB,'Update plot <Update>')
            CALL BUTTON(56,'[U]pdate',5)
            CALL UPDATEPLOT(.FALSE.,ISTATUS)
            IF(ISTATUS.EQ.0)THEN
              CALL BUTTON(56,'[U]pdate',0)
              CALL BUTTON(56,'[U]pdate',-5)
            ELSE
              CALL PLOT_SETTINGS
              CALL BUTTON(16,'sa[v]e buff.',0)
              CALL BUTTON(56,'[U]pdate',0)
              CALL BUTTON(56,'[U]pdate',3)
              CALL BUTTON(55,'[P]ostScript',-4)
              CALL BUTTON(63,'[Z]oom',0)
              CALL BUTTON(64,'[W]hole',0)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.103)THEN
            CALL TOLOG77(NB,'Update X label <X label>')
            CALL BUTTON(103,'X label:',5)
            WRITE(*,100) 'New X label (0=empty) '
            CXLABEL(1:255)=READC_B(CXLABEL,'@')
            CALL TOLOG77_STRING(CXLABEL(1:TRUELEN(CXLABEL)),
     +       'X label')
            IF(CXLABEL.EQ.'0')THEN
              CXLABEL=' '
              LXLABEL=.FALSE.
            ELSE
              LXLABEL=.TRUE.
              WRITE(CDUMMY,*) XLABEL_D
              WRITE(*,100) 'Distance to the X-axis............... '
              XLABEL_D=READF_B(CDUMMY)
              WRITE(77,*) XLABEL_D,'# Distance to the X-axis'
              WRITE(CDUMMY,*) XLABEL_C
              WRITE(*,100) 'Label location relative to the X-axis '
              XLABEL_C=READF_B(CDUMMY)
              WRITE(77,*) XLABEL_C,
     +         '# Label location relative to the X-axis'
              WRITE(CDUMMY,*) XLABEL_J
              WRITE(*,100) 'Label justification.................. '
              XLABEL_J=READF_B(CDUMMY)
              WRITE(77,*) XLABEL_J,'# Label justification'
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.111)THEN
            CALL TOLOG77(NB,'Update Y label <Y label>')
            CALL BUTTON(111,'Y label:',5)
            WRITE(*,100) 'New Y label (0=empty) '
            CYLABEL(1:255)=READC_B(CYLABEL,'@')
            CALL TOLOG77_STRING(CYLABEL(1:TRUELEN(CYLABEL)),
     +       'Y label')
            IF(CYLABEL.EQ.'0')THEN
              CYLABEL=' '
              LYLABEL=.FALSE.
            ELSE
              LYLABEL=.TRUE.
              WRITE(CDUMMY,*) YLABEL_D
              WRITE(*,100) 'Distance to the Y-axis............... '
              YLABEL_D=READF_B(CDUMMY)
              WRITE(77,*) YLABEL_D,'# Distance to the Y-axis'
              WRITE(CDUMMY,*) YLABEL_C
              WRITE(*,100) 'Label location relative to the Y-axis '
              YLABEL_C=READF_B(CDUMMY)
              WRITE(77,*) YLABEL_C,
     +         '# Label location relative to the Y-axis'
              WRITE(CDUMMY,*) YLABEL_J
              WRITE(*,100) 'Label justification.................. '
              YLABEL_J=READF_B(CDUMMY)
              WRITE(77,*) YLABEL_J,'# Label justification'
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.119)THEN
            CALL TOLOG77(NB,'Update plot label <Plot label>')
            CALL BUTTON(119,'Plot label:',5)
            WRITE(*,100) 'New plot label (0=empty) '
            CGLABEL(1:255)=READC_B(CGLABEL,'@')
            CALL TOLOG77_STRING(CGLABEL(1:TRUELEN(CGLABEL)),
     +       'Plot label')
            IF(CGLABEL.EQ.'0')THEN
              CGLABEL=' '
              LGLABEL=.FALSE.
            ELSE
              LGLABEL=.TRUE.
              WRITE(CDUMMY,*) GLABEL_D
              WRITE(*,100) 'Distance to the Y-axis............... '
              GLABEL_D=READF_B(CDUMMY)
              WRITE(77,*) GLABEL_D,'# Distance to the Y-axis'
              WRITE(CDUMMY,*) GLABEL_C
              WRITE(*,100) 'Label location relative to the Y-axis '
              GLABEL_C=READF_B(CDUMMY)
              WRITE(77,*) GLABEL_C,
     +         '# Label location relative to the Y-axis'
              WRITE(CDUMMY,*) GLABEL_J
              WRITE(*,100) 'Label justification.................. '
              GLABEL_J=READF_B(CDUMMY)
              WRITE(77,*) GLABEL_J,'# Label justification'
            END IF
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.127)THEN
            CALL TOLOG77(NB,'Define Expand factor <Expand (%)>')
            WRITE(CEXPAND,*) IEXPAND
            L1=TRUEBEG(CEXPAND)
            L2=TRUELEN(CEXPAND)
            CALL BUTTON(127,'Exp.(%)='//CEXPAND(L1:L2),5)
            WRITE(*,100) 'New expand value (%) '
            IEXPAND=READILIM_B(CEXPAND,0,100)
            WRITE(77,111) IEXPAND,'# New expand value (%)'
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.128)THEN
            CALL TOLOG77(NB,'Modify data key <DATA Key>')
            IF(IDATAKEY.EQ.0)THEN
              CALL BUTTON(128,'KEY off',5)
            ELSEIF(IDATAKEY.EQ.1)THEN
              CALL BUTTON(128,'KEY on (1)',5)
            ELSEIF(IDATAKEY.EQ.2)THEN
              CALL BUTTON(128,'KEY on (2)',5)
            ELSEIF(IDATAKEY.EQ.3)THEN
              CALL BUTTON(128,'KEY on (3)',5)
            ELSEIF(IDATAKEY.EQ.4)THEN
              CALL BUTTON(128,'KEY on (4)',5)
            END IF
C
            CALL UPDATEKEY
C
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.135)THEN
            CALL TOLOG77(NB,'Set X axis parameters <XOPT>')
            L1=TRUEBEG(XOPT)
            L2=TRUELEN(XOPT)
            CALL BUTTON(135,'X:'//XOPT(L1:L2),5)
            WRITE(*,100) 'New XOPT (0=empty) '
            XOPT(1:20)=READC_B(XOPT,'ABCGILNPMTSV120')
            CALL TOLOG77_STRING(XOPT(1:TRUELEN(XOPT)),'XOPT for X axis')
            IF(XOPT.EQ.'0') XOPT=' '
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.136)THEN
            CALL TOLOG77(NB,'Set Y axis parameters <YOPT>')
            L1=TRUEBEG(YOPT)
            L2=TRUELEN(YOPT)
            CALL BUTTON(136,'Y:'//YOPT(L1:L2),5)
            WRITE(*,100) 'New YOPT (0=empty) '
            YOPT(1:20)=READC_B(YOPT,'ABCGILNPMTSV120')
            CALL TOLOG77_STRING(YOPT(1:TRUELEN(YOPT)),'YOPT for Y axis')
            IF(YOPT.EQ.'0') YOPT=' '
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.143)THEN
            CALL TOLOG77(NB,'Set un(equal) scales <JUST>')
            IF(AXISJUST.EQ.0)THEN
              CALL BUTTON(143,'JUST=1',0)
              CALL BUTTON(143,'JUST=1',-8)
              AXISJUST=1
            ELSE
              CALL BUTTON(143,'JUST=0',0)
              CALL BUTTON(143,'JUST=0',-8)
              AXISJUST=0
            END IF
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.144)THEN
            CALL TOLOG77(NB,'Set current font for axis <Axis CF>')
            AXISCF=AXISCF+1
            IF(AXISCF.GT.4) AXISCF=1
            IF(AXISCF.EQ.1)THEN
              CALL BUTTON(144,'CF=1',0)
              CALL BUTTON(144,'CF=1',-8)
            ELSEIF(AXISCF.EQ.2)THEN
              CALL BUTTON(144,'CF=2',0)
              CALL BUTTON(144,'CF=2',-8)
            ELSEIF(AXISCF.EQ.3)THEN
              CALL BUTTON(144,'CF=3',0)
              CALL BUTTON(144,'CF=3',-8)
            ELSEIF(AXISCF.EQ.4)THEN
              CALL BUTTON(144,'CF=4',0)
              CALL BUTTON(144,'CF=4',-8)
            END IF
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.151)THEN
            CALL TOLOG77(NB,'Set line with for axis <Axis LW>')
            WRITE(CAXISLW,*) AXISLW
            L1=TRUEBEG(CAXISLW)
            L2=TRUELEN(CAXISLW)
            CALL BUTTON(151,'LW='//CAXISLW(L1:L2),5)
            WRITE(*,100) 'New Axis LW '
            AXISLW=READI_B(CAXISLW)
            WRITE(77,111) AXISLW,'# New Axis LW'
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.152)THEN
            CALL TOLOG77(NB,'Set current height for axis <Axis CH>')
            WRITE(CAXISCH,'(F7.2)') AXISCH
            L1=TRUEBEG(CAXISCH)
            L2=TRUELEN(CAXISCH)
            CALL BUTTON(152,'CH='//CAXISCH(L1:L2),5)
            WRITE(*,100) 'New Axis CH '
            AXISCH=READF_B(CAXISCH)
            WRITE(77,*) AXISCH,'# New Axis CH'
            CALL PLOT_SETTINGS
            IF(LBUFFER)THEN
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.159)THEN
            CALL TOLOG77(NB,'Perform statistical analysis <statistics>')
            CALL BUTTON(159,'statistics',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              CALL GIVE_STATISTICS(NB_)
            END IF
C
            CALL BUTTON(159,'statistics',0)
C..............................................................................
          ELSEIF(NB.EQ.160)THEN
            CALL TOLOG77(NB,'Randomize buffer data <randomize>')
            CALL BUTTON(160,'randomize',5)
C
            WRITE(*,100) 'Select initial buffer...'
            NB_=0
            DO WHILE((NB_.LT.1).OR.(NB_.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB_)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB_)
              END IF
              IF(.NOT.LDEFBUFF(NB_)) NB_=0
            END DO
            WRITE(77,111) NB_,'# Initial buffer'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB_,' selected'
C
            WRITE(*,100) 'Select destination buffer...'
            NB__=0
            DO WHILE((NB__.LT.1).OR.(NB__.GT.NBUFFMAX))
              IF(LBATCH)THEN
                CALL READ_NB(NB__)
              ELSE
                CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
                CALL IFBUTTON(XC,YC,NB__)
              END IF
              IF(NB__.EQ.NB_) NB__=0
            END DO
            WRITE(77,111) NB__,'# Destination buffer'
            WRITE(*,'(A,I1,A)') '   ...OK! Buffer #',NB__,' selected'
            WRITE(CBUTTON,'(A10,I1,A1)') 'buffer # [',NB__,']'
            IF(NCOLORBUFF(NB__).NE.0)THEN
              CALL BUTTON(NB__,CBUTTON,-NCOLORBUFF(NB__)-1)
            ELSE
              CALL BUTTON(NB__,CBUTTON,2)
            END IF
C
            IF(LDEFBUFF(NB__))THEN
              WRITE(*,101) 'WARNING: last buffer will be overwritten'
            END IF
C
            CALL RANDOMIZEDATA(NB_,NB__,CAPEND)
C
            CALL BUTTON(56,'[U]pdate',-5)          !hay que actualizar graficos
            CALL BUTTON(55,'[P]ostScript',3)           !desactivamos PostScript
C
            CALL SHOW_BUFFERS
C
            CALL BUTTON(160,'randomize',0)
C..............................................................................
          ELSEIF(NB.EQ.167)THEN
            CALL TOLOG77(NB,'Generate X data histogram <X histogram>')
            CALL BUTTON(167,'X histogram',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              CALL HISTOGRAM('X',NB_)
              CALL BUTTON(56,'[U]pdate',-5)
              CALL BUTTON(55,'[P]ostScript',3)
            END IF
C
            CALL BUTTON(167,'X histogram',0)
C..............................................................................
          ELSEIF(NB.EQ.168)THEN
            CALL TOLOG77(NB,'Generate Y data histogram <Y histogram>')
            CALL BUTTON(168,'Y histogram',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              CALL HISTOGRAM('Y',NB_)
              CALL BUTTON(56,'[U]pdate',-5)
              CALL BUTTON(55,'[P]ostScript',3)
            END IF
C
            CALL BUTTON(168,'Y histogram',0)
C..............................................................................
          ELSEIF(NB.EQ.175)THEN
            CALL TOLOG77(NB,'Normalize data ranges')
            CALL BUTTON(175,'norm.ranges',5)
C
            CALL SELBUFFER(NB_)
            IF(NB_.NE.-1)THEN
              CALL NORM_RANGES(NB_)
              CALL BUTTON(55,'[P]ostScript',3)
              CALL BUTTON(56,'[U]pdate',-5)
            END IF
C
            CALL BUTTON(175,'norm.ranges',0)
C..............................................................................
          ELSEIF(NB.EQ.200)THEN
            CALL BUTTON(200,'[Q]UIT',5)
            WRITE(*,100) 'Do you really want to end this session'//
     +       '........(y/n) '
            CSURE(1:1)=READC_B('y','yn')
            IF(CSURE.EQ.'y')THEN
              WRITE(77,101) '#Remove comments in next 3 lines '//
     +         'to execute the log file and stop the program:'
              WRITE(77,101) '#200         # <QUIT>'
              WRITE(77,101) '#y           # really want to leave?' 
              WRITE(77,101) '#n           # save log file?'
              WRITE(77,101) 'END_of_xpgp.log'         !marca fin de fichero log
              CLOSE(77)
              WRITE(*,100) 'Do you want to save a logfile of this '
              WRITE(*,100) 'session (y/n) '
              CSAVE(1:1)=READC_B('n','yn')
              IF(CSAVE.EQ.'y')THEN
                LOGFILE=.TRUE.
                DO WHILE(LOGFILE)
                  WRITE(*,100) 'Log file name'
                  FILELOG=READC_B('@','@')
                  INQUIRE(FILE=FILELOG,EXIST=LOGFILE)
                  IF(LOGFILE)THEN
                    WRITE(*,101) 'WARNING: this file already exist. '
                    WRITE(*,100) 'Do you want to overwrite it (y/n) '
                    COVER(1:1)=READC_B('y','yn')
                    IF(COVER.EQ.'y') LOGFILE=.FALSE.
                  END IF
                END DO
                ISYSTEM=SYSTEMFUNCTION('mv -f xpgp.log '//
     +           FILELOG(TRUEBEG(FILELOG):TRUELEN(FILELOG)))
              ELSE
                ISYSTEM=SYSTEMFUNCTION('rm -f xpgp.log')
              END IF
              LEXIT=.TRUE.
            ELSE
              CALL TOLOG77(NB,'End of program <QUIT>')
              WRITE(77,112) CSURE,'# End of program'
              CALL BUTTON(200,'[Q]UIT',0)
            END IF
C..............................................................................
          END IF
        END DO
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
        CALL PGEND
        IF(LBATCH) CLOSE(78) !fichero BATCH
        STOP
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
        END
