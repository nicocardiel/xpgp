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
C Redibuja los buffers activos. Si no hay ninguno activo, ISTATUS retorna 0.
C Si LPOSTCRIPT=.TRUE., se aumenta el tama\~{n}o del font por FACTORCH, y el
C grafico se realiza en ventana completa.
        SUBROUTINE UPDATEPLOT(LPOSTSCRIPT,ISTATUS)
        IMPLICIT NONE
        LOGICAL LPOSTSCRIPT
        INTEGER ISTATUS
C
        INCLUDE 'nbuffmax.inc'
        INCLUDE 'ndatamax.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER I,N,L1,L2
        INTEGER NB
        INTEGER NDATABUFF(NBUFFMAX)
        INTEGER AXISJUST,AXISLW,AXISCF
        INTEGER IDATAKEY
        INTEGER IEXPAND
        INTEGER OLDLW,OLDCF,OLDCI,OLDFS,OLDLS
        INTEGER NCOLORBUFF(NBUFFMAX),NSYMBBUFF(NBUFFMAX)
        INTEGER LWBUFF(NBUFFMAX)
        REAL CHBUFF(NBUFFMAX)
        REAL XDATA(NDATAMAX,NBUFFMAX),EXDATA(NDATAMAX,NBUFFMAX)
        REAL YDATA(NDATAMAX,NBUFFMAX),EYDATA(NDATAMAX,NBUFFMAX)
        REAL XMINBUFF(NBUFFMAX),XMAXBUFF(NBUFFMAX)
        REAL YMINBUFF(NBUFFMAX),YMAXBUFF(NBUFFMAX)
        REAL XMIN,XMAX,YMIN,YMAX,DX,DY
        REAL AXISCH
        REAL XLABEL_D,XLABEL_C,XLABEL_J
        REAL YLABEL_D,YLABEL_C,YLABEL_J
        REAL GLABEL_D,GLABEL_C,GLABEL_J
        REAL X(NDATAMAX),Y(NDATAMAX)
        REAL EX1(NDATAMAX),EX2(NDATAMAX)
        REAL EY1(NDATAMAX),EY2(NDATAMAX)
        REAL OLDCH
        REAL FACTORCH
        REAL XV1,XV2,YV1,YV2
        REAL XV1_,XV2_,YV1_,YV2_
        REAL RMARGIN,XCH,YCH,DYCH,XBOX(4),YBOX(4)
        REAL XKEY,YKEY,DXKEY,DYKEY,DXKEYLINE,DHKEY
        REAL DATAKEYCH,DATAKEYCHSYMB
        CHARACTER*20 XYNAME(NDATAMAX,NBUFFMAX)
        CHARACTER*20 XOPT,YOPT
        CHARACTER*50 CDUMMY
        CHARACTER*50 DATAKEY(NBUFFMAX)
        CHARACTER*255 CXLABEL,CYLABEL,CGLABEL
        LOGICAL LXLABEL,LYLABEL,LGLABEL
        LOGICAL LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
        LOGICAL LDEFBUFF(NBUFFMAX),LUSEBUFF(NBUFFMAX)
        LOGICAL LOK,LLIMITFIRST
        LOGICAL LXERR(NBUFFMAX),LYERR(NBUFFMAX)
        LOGICAL LXYNAME(NBUFFMAX)
        LOGICAL LBATCH
        LOGICAL LLABELS
        LOGICAL LDISPLAY_THIS_KEY
C
        COMMON/BLKFACTORCH/FACTORCH
        COMMON/BLKSETTINGS0/LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKNDATABUFF/NDATABUFF
        COMMON/BLKXYDATA/XDATA,YDATA
        COMMON/BLKEXYDATA/EXDATA,EYDATA
        COMMON/BLKXYNAME/XYNAME
        COMMON/BLKMINMAXBUFF/XMINBUFF,XMAXBUFF,YMINBUFF,YMAXBUFF
        COMMON/BLKLXYERR/LXERR,LYERR
        COMMON/BLKLXYNAME/LXYNAME
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
        COMMON/BLKLBATCH/LBATCH
        COMMON/BLKLABELS1/LLABELS
C------------------------------------------------------------------------------
        ISTATUS=0                          !salvo que se demuestre lo contrario
C Verificamos que hay algun buffer activo
        LOK=.FALSE.
        DO NB=1,NBUFFMAX
          IF(LUSEBUFF(NB)) LOK=.TRUE.
        END DO
C
        IF(.NOT.LOK)THEN
          WRITE(*,101) 'ERROR: you must activate at least one buffer!'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(LBATCH)THEN
            WRITE(*,*)
          ELSE
            READ(*,*)
          END IF
          RETURN
        END IF
C------------------------------------------------------------------------------
C limites automaticos (si procede)
C XMIN
        IF(.NOT.LXMINFIX)THEN
          LLIMITFIRST=.TRUE.
          DO NB=1,NBUFFMAX
            IF(LUSEBUFF(NB))THEN
              IF(LLIMITFIRST)THEN
                LLIMITFIRST=.FALSE.
                XMIN=XMINBUFF(NB)
              ELSE
                IF(XMIN.GT.XMINBUFF(NB)) XMIN=XMINBUFF(NB)
              END IF
            END IF
          END DO
        END IF
C XMAX
        IF(.NOT.LXMAXFIX)THEN
          LLIMITFIRST=.TRUE.
          DO NB=1,NBUFFMAX
            IF(LUSEBUFF(NB))THEN
              IF(LLIMITFIRST)THEN
                LLIMITFIRST=.FALSE.
                XMAX=XMAXBUFF(NB)
              ELSE
                IF(XMAX.LT.XMAXBUFF(NB)) XMAX=XMAXBUFF(NB)
              END IF
            END IF
          END DO
        END IF
C expansion
        DX=XMAX-XMIN
        IF(.NOT.LXMINFIX) XMIN=XMIN-DX*REAL(IEXPAND)/100.
        IF(.NOT.LXMAXFIX) XMAX=XMAX+DX*REAL(IEXPAND)/100.
C corregimos XMIN=XMAX
        IF(XMIN.EQ.XMAX)THEN
          XMIN=XMIN-0.1
          XMAX=XMAX+0.1
        END IF
C YMIN
        IF(.NOT.LYMINFIX)THEN
          LLIMITFIRST=.TRUE.
          DO NB=1,NBUFFMAX
            IF(LUSEBUFF(NB))THEN
              IF(LLIMITFIRST)THEN
                LLIMITFIRST=.FALSE.
                YMIN=YMINBUFF(NB)
              ELSE
                IF(YMIN.GT.YMINBUFF(NB)) YMIN=YMINBUFF(NB)
              END IF
            END IF
          END DO
        END IF
C YMAX
        IF(.NOT.LYMAXFIX)THEN
          LLIMITFIRST=.TRUE.
          DO NB=1,NBUFFMAX
            IF(LUSEBUFF(NB))THEN
              IF(LLIMITFIRST)THEN
                LLIMITFIRST=.FALSE.
                YMAX=YMAXBUFF(NB)
              ELSE
                IF(YMAX.LT.YMAXBUFF(NB)) YMAX=YMAXBUFF(NB)
              END IF
            END IF
          END DO
        END IF
C expansion
        DY=YMAX-YMIN
        IF(.NOT.LYMINFIX) YMIN=YMIN-DY*REAL(IEXPAND)/100.
        IF(.NOT.LYMAXFIX) YMAX=YMAX+DY*REAL(IEXPAND)/100.
C corregimos YMIN=YMAX
        IF(YMIN.EQ.YMAX)THEN
          YMIN=YMIN-0.1
          YMAX=YMAX+0.1
        END IF
C------------------------------------------------------------------------------
C salvamos opciones de dibujo anteriores
        CALL PGQLW(OLDLW)
        CALL PGQCF(OLDCF)
        CALL PGQCH(OLDCH)
        CALL PGQCI(OLDCI)
        CALL PGQFS(OLDFS)
C------------------------------------------------------------------------------
C dibujamos la caja del plot con las opciones elegidas
        CALL PGSLW(AXISLW)
        CALL PGSCF(AXISCF)
        IF(LPOSTSCRIPT)THEN
          CALL PGSCH(AXISCH*FACTORCH)
        ELSE
          CALL PGSCH(AXISCH)
        END IF
C
        IF(LPOSTSCRIPT)THEN
          CALL PGSCI(1)
          CALL PGENV(XMIN,XMAX,YMIN,YMAX,AXISJUST,-2)
        ELSE
          CALL BUTTQPR(XV1,XV2,YV1,YV2)
          CALL PGSCI(1)
          CALL PGSFS(1)
          CALL PGSVP(XV1,XV2,YV1,YV2)
          CALL PGWINDOW(0.,1.,0.,1.)
          CALL PGRECT(0.,1.,0.,1.)
          CALL PGSCI(0)
          CALL PGQCS(0,XCH,YCH)
          RMARGIN=4.0*YCH
          YV1_=YV1+RMARGIN
          YV2_=YV2-RMARGIN
          RMARGIN=4.0*XCH
          XV1_=XV1+RMARGIN
          XV2_=XV2-RMARGIN
          CALL PGSVP(XV1_,XV2_,YV1_,YV2_)
          IF(AXISJUST.EQ.1)THEN
            CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
          ELSE
            CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
          END IF
        END IF
        CALL PGBOX(XOPT,0.0,0,YOPT,0.0,0)
        IF(LXLABEL) CALL PGMTEXT('B',XLABEL_D,XLABEL_C,XLABEL_J,CXLABEL)
        IF(LYLABEL) CALL PGMTEXT('L',YLABEL_D,YLABEL_C,YLABEL_J,CYLABEL)
        IF(LGLABEL) CALL PGMTEXT('T',GLABEL_D,GLABEL_C,GLABEL_J,CGLABEL)
C------------------------------------------------------------------------------
C dibujamos los datos
        DO NB=1,NBUFFMAX
          IF(LUSEBUFF(NB))THEN
            CALL PGSCI(NCOLORBUFF(NB))
            IF(LPOSTSCRIPT)THEN
              CALL PGSCH(CHBUFF(NB)*FACTORCH)
            ELSE
              CALL PGSCH(CHBUFF(NB))
            END IF
            CALL PGSLW(LWBUFF(NB))
            N=NDATABUFF(NB)
            DO I=1,N
              X(I)=XDATA(I,NB)
              Y(I)=YDATA(I,NB)
            END DO
            IF(LXERR(NB))THEN
              DO I=1,N
                EX1(I)=XDATA(I,NB)-EXDATA(I,NB)
                EX2(I)=XDATA(I,NB)+EXDATA(I,NB)
              END DO
            END IF
            IF(LYERR(NB))THEN
              DO I=1,N
                EY1(I)=YDATA(I,NB)-EYDATA(I,NB)
                EY2(I)=YDATA(I,NB)+EYDATA(I,NB)
              END DO
            END IF
            IF((NSYMBBUFF(NB).GE.1001).AND.
     +       (NSYMBBUFF(NB).LE.1005))THEN !..............................lineas
              CALL PGQLS(OLDLS)
              CALL PGSLS(NSYMBBUFF(NB)-1000)
              CALL PGLINE(N,X,Y)
              CALL PGSLS(OLDLS)
            ELSEIF(NSYMBBUFF(NB).EQ.100)THEN      !etiquetas con numero de dato
              DO I=1,N
                IF((XMIN.LE.X(I)).AND.(XMAX.GE.X(I)).AND.
     >             (YMIN.LE.Y(I)).AND.(YMAX.GE.Y(I)))THEN
                  CALL PGQTXT(X(I),Y(I),0.0,0.5,XYNAME(I,NB),XBOX,YBOX)
                  DYCH=0.3*(YBOX(2)-YBOX(1))
                  WRITE(CDUMMY,*) I
                  CALL PGPTXT(X(I),Y(I)-DYCH,0.0,0.5,
     >             CDUMMY(TRUEBEG(CDUMMY):))
                END IF
              END DO
            ELSEIF(NSYMBBUFF(NB).EQ.200)THEN             !etiquetas con nombres
              DO I=1,N
                IF((XMIN.LE.X(I)).AND.(XMAX.GE.X(I)).AND.
     >             (YMIN.LE.Y(I)).AND.(YMAX.GE.Y(I)))THEN
                  CALL PGQTXT(X(I),Y(I),0.0,0.5,XYNAME(I,NB),XBOX,YBOX)
                  DYCH=0.3*(YBOX(2)-YBOX(1))
                  CALL PGPTXT(X(I),Y(I)-DYCH,0.0,0.5,XYNAME(I,NB))
                END IF
              END DO
            ELSE                                                      !simbolos
              CALL PGPOINT(N,X,Y,NSYMBBUFF(NB))
              IF(LXERR(NB)) CALL PGERRX(N,EX1,EX2,Y,1.0)
              IF(LYERR(NB)) CALL PGERRY(N,X,EY1,EY2,1.0)
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C si procede, dibujamos KEY
        IF(IDATAKEY.NE.0)THEN
          IF(IDATAKEY.EQ.1)THEN
            XKEY=XMIN+2.*DATAKEYCH/40.*(XMAX-XMIN)
            DXKEY=0.0
            DXKEYLINE=3.*DATAKEYCH/40.*(XMAX-XMIN)
            YKEY=YMAX-2.*DATAKEYCH/40.*(YMAX-YMIN)
            DYKEY=-2.*AXISCH/40.*(YMAX-YMIN)
            DHKEY=0.7*DATAKEYCH/40.*(YMAX-YMIN)
          ELSEIF(IDATAKEY.EQ.2)THEN
            XKEY=XMAX-2.*DATAKEYCH/40.*(XMAX-XMIN)
            DXKEY=0.0
            DXKEYLINE=-3.*DATAKEYCH/40.*(XMAX-XMIN)
            YKEY=YMAX-2.*DATAKEYCH/40.*(YMAX-YMIN)
            DYKEY=-2.*AXISCH/40.*(YMAX-YMIN)
            DHKEY=0.7*DATAKEYCH/40.*(YMAX-YMIN)
          ELSEIF(IDATAKEY.EQ.3)THEN
            XKEY=XMIN+2.*DATAKEYCH/40.*(XMAX-XMIN)
            DXKEY=0.0
            DXKEYLINE=3.*DATAKEYCH/40.*(XMAX-XMIN)
            YKEY=YMIN+2.*DATAKEYCH/40.*(YMAX-YMIN)
            DYKEY=2.*AXISCH/40.*(YMAX-YMIN)
            DHKEY=0.7*DATAKEYCH/40.*(YMAX-YMIN)
          ELSEIF(IDATAKEY.EQ.4)THEN
            XKEY=XMAX-2.*DATAKEYCH/40.*(XMAX-XMIN)
            DXKEY=0.0
            DXKEYLINE=-3.*DATAKEYCH/40.*(XMAX-XMIN)
            YKEY=YMIN+2.*DATAKEYCH/40.*(YMAX-YMIN)
            DYKEY=2.*AXISCH/40.*(YMAX-YMIN)
            DHKEY=0.7*DATAKEYCH/40.*(YMAX-YMIN)
          ELSE
            WRITE(*,100) 'IDATAKEY='
            WRITE(*,*) IDATAKEY
            WRITE(*,101) 'FATAL ERROR in subroutine UPDATEPLOT: '//
     +       'invalid IDATAKEY'
            STOP
          END IF
C
          DO NB=1,NBUFFMAX
            LDISPLAY_THIS_KEY=LUSEBUFF(NB) !1) tiene que estar activo el buffer
            IF(LDISPLAY_THIS_KEY)THEN      !2) el key no es "000"
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
              IF(L2-L1+1.EQ.3)THEN
                LDISPLAY_THIS_KEY=(DATAKEY(NB)(L1:L2).NE.'000')
              END IF
            END IF
            IF(LDISPLAY_THIS_KEY)THEN
              CALL PGSCI(NCOLORBUFF(NB))
              XKEY=XKEY+DXKEY
              YKEY=YKEY+DYKEY
C
              CALL PGSLW(LWBUFF(NB))
C
              IF((DATAKEYCHSYMB.EQ.0.0).AND.(NSYMBBUFF(NB).NE.200).
     >         AND.(NSYMBBUFF(NB).NE.100))THEN
                IF(LPOSTSCRIPT)THEN
                  CALL PGSCH(CHBUFF(NB)*FACTORCH)
                ELSE
                  CALL PGSCH(CHBUFF(NB))
                END IF
              ELSE
                IF(LPOSTSCRIPT)THEN
                  CALL PGSCH(DATAKEYCH*FACTORCH)
                ELSE
                  CALL PGSCH(DATAKEYCH)
                END IF
              END IF
              IF((NSYMBBUFF(NB).GE.1001).AND.
     +         (NSYMBBUFF(NB).LE.1005))THEN
                CALL PGQLS(OLDLS)
                CALL PGSLS(NSYMBBUFF(NB)-1000)
                CALL PGMOVE(XKEY,YKEY+DHKEY)
                CALL PGDRAW(XKEY+DXKEYLINE,YKEY+DHKEY)
                CALL PGSLS(OLDLS)
              ELSEIF(NSYMBBUFF(NB).EQ.100)THEN
                CALL PGPTEXT(XKEY+0.5*DXKEYLINE,YKEY,0.,.5,'#')
              ELSEIF(NSYMBBUFF(NB).EQ.200)THEN
                CALL PGPTEXT(XKEY+0.5*DXKEYLINE,YKEY,0.,.5,'name')
              ELSE
                CALL PGPOINT(1,XKEY+0.5*DXKEYLINE,YKEY+DHKEY,
     >           NSYMBBUFF(NB))
              END IF
              IF(LPOSTSCRIPT)THEN
                CALL PGSCH(DATAKEYCH*FACTORCH)
              ELSE
                CALL PGSCH(DATAKEYCH)
              END IF
C
              L1=TRUEBEG(DATAKEY(NB))
              L2=TRUELEN(DATAKEY(NB))
C
              CALL PGSLW(AXISLW)
              IF(IDATAKEY.EQ.1)THEN
                CALL PGPTEXT(XKEY+1.5*DXKEYLINE,YKEY,0.,0.,
     >           DATAKEY(NB)(L1:L2))
              ELSEIF(IDATAKEY.EQ.2)THEN
                CALL PGPTEXT(XKEY+1.5*DXKEYLINE,YKEY,0.,1.,
     >           DATAKEY(NB)(L1:L2))
              ELSEIF(IDATAKEY.EQ.3)THEN
                CALL PGPTEXT(XKEY+1.5*DXKEYLINE,YKEY,0.,0.,
     >           DATAKEY(NB)(L1:L2))
              ELSEIF(IDATAKEY.EQ.4)THEN
                CALL PGPTEXT(XKEY+1.5*DXKEYLINE,YKEY,0.,1.,
     >           DATAKEY(NB)(L1:L2))
              END IF
C
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
C restauramos opciones de dibujo
        CALL PGSLW(OLDLW)
        CALL PGSCF(OLDCF)
        CALL PGSCH(OLDCH)
        CALL PGSCI(OLDCI)
        CALL PGSFS(OLDFS)
C------------------------------------------------------------------------------
C dibujamos etiquetas
        IF(LLABELS) CALL LABELS(1)
C------------------------------------------------------------------------------
        ISTATUS=1
100     FORMAT(A,$)
101     FORMAT(A)
        END
