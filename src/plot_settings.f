C
C******************************************************************************
C Representa los atributos establecidos para el dibujo: limites y etiquetas
C en ambos ejes, asi como tipo de letra, tama\~{n}o y anchura de linea de 
C dichas etiquetas.
        SUBROUTINE PLOT_SETTINGS
        IMPLICIT NONE
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER L1,L2
        INTEGER AXISJUST,AXISLW,AXISCF
        INTEGER IEXPAND
        INTEGER IDATAKEY
        REAL XMIN,XMAX,YMIN,YMAX
        REAL AXISCH
        REAL OLDCH
        CHARACTER*20 XOPT,YOPT
        CHARACTER*50 CXMIN,CXMAX,CYMIN,CYMAX
        CHARACTER*50 CAXISLW,CAXISCH,CEXPAND
        CHARACTER*255 CXLABEL,CYLABEL,CGLABEL
        LOGICAL LBUFFER
        LOGICAL LXLABEL,LYLABEL,LGLABEL
        LOGICAL LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
C
        COMMON/BLKSETTINGS0/LXMINFIX,LXMAXFIX,LYMINFIX,LYMAXFIX
        COMMON/BLKSETTINGS1/XMIN,XMAX,YMIN,YMAX
        COMMON/BLKSETTINGS2/LBUFFER
        COMMON/BLKSETTINGS3/LXLABEL,LYLABEL,LGLABEL
        COMMON/BLKSETTINGS4/CXLABEL,CYLABEL,CGLABEL
        COMMON/BLKSETTINGS5/XOPT,YOPT
        COMMON/BLKSETTINGS6/AXISJUST,AXISLW,AXISCF,AXISCH
        COMMON/BLKSETTINGS7/IEXPAND
        COMMON/BLKSETTINGS8A/IDATAKEY
C------------------------------------------------------------------------------
        CALL BUTTQCH(OLDCH)
C Limites en X e Y
        CALL BUTTON(71,'Xmin:',0)
        CALL BUTTON(79,'Xmax:',0)
        CALL BUTTON(87,'Ymin:',0)
        CALL BUTTON(95,'Ymax:',0)
        IF(LBUFFER)THEN
          IF(LXMINFIX) CALL BUTTON(71,'Xmin:',1)
          IF(LXMAXFIX) CALL BUTTON(79,'Xmax:',1)
          IF(LYMINFIX) CALL BUTTON(87,'Ymin:',1)
          IF(LYMAXFIX) CALL BUTTON(95,'Ymax:',1)
          WRITE(CXMIN,*) XMIN
          CALL RMBLANK(CXMIN,CXMIN,L2)
          WRITE(CXMAX,*) XMAX
          CALL RMBLANK(CXMAX,CXMAX,L2)
          WRITE(CYMIN,*) YMIN
          CALL RMBLANK(CYMIN,CYMIN,L2)
          WRITE(CYMAX,*) YMAX
          CALL RMBLANK(CYMAX,CYMAX,L2)
          CALL BUTTON(72,CXMIN,-1)
          CALL BUTTON(80,CXMAX,-1)
          CALL BUTTON(88,CYMIN,-1)
          CALL BUTTON(96,CYMAX,-1)
          CALL BUTTSCH(0.8*OLDCH)
          CALL BUTTON(72,CXMIN,1)
          CALL BUTTON(80,CXMAX,1)
          CALL BUTTON(88,CYMIN,1)
          CALL BUTTON(96,CYMAX,1)
          CALL BUTTSCH(OLDCH)
        ELSE
          CALL BUTTON(71,'Xmin:',3)
          CALL BUTTON(79,'Xmax:',3)
          CALL BUTTON(87,'Ymin:',3)
          CALL BUTTON(95,'Ymax:',3)
          CALL BUTTON(72,'not defined',-1)
          CALL BUTTON(80,'not defined',-1)
          CALL BUTTON(88,'not defined',-1)
          CALL BUTTON(96,'not defined',-1)
          CALL BUTTSCH(0.8*OLDCH)
          CALL BUTTON(72,'not defined',-3)
          CALL BUTTON(80,'not defined',-3)
          CALL BUTTON(88,'not defined',-3)
          CALL BUTTON(96,'not defined',-3)
          CALL BUTTSCH(OLDCH)
        END IF
C------------------------------------------------------------------------------
C Etiquetas en X, Y y en la parte superior de la Figura
        CALL BUTTON(103,'X label:',0)
        CALL BUTTON(111,'Y label:',0)
        CALL BUTTON(119,'Plot label:',0)
        IF(LBUFFER)THEN
          CALL BUTTSCH(0.8*OLDCH)
          CALL BUTTON(104,CXLABEL,-1)
          IF(LXLABEL)THEN
            IF(TRUELEN(CXLABEL).GT.11)THEN
              CALL BUTTON(104,CXLABEL(1:8)//'...',1)
            ELSE
              CALL BUTTON(104,CXLABEL,1)
            END IF
          ELSE
            CALL BUTTON(104,'not defined',-3)
          END IF
          CALL BUTTON(112,CYLABEL,-1)
          IF(LYLABEL)THEN
            IF(TRUELEN(CYLABEL).GT.11)THEN
              CALL BUTTON(112,CYLABEL(1:8)//'...',1)
            ELSE
              CALL BUTTON(112,CYLABEL,1)
            END IF
          ELSE
            CALL BUTTON(112,'not defined',-3)
          END IF
          CALL BUTTON(120,CGLABEL,-1)
          IF(LGLABEL)THEN
            IF(TRUELEN(CGLABEL).GT.11)THEN
              CALL BUTTON(120,CGLABEL(1:8)//'...',1)
            ELSE
              CALL BUTTON(120,CGLABEL,1)
            END IF
          ELSE
            CALL BUTTON(120,'not defined',-3)
          END IF
          CALL BUTTSCH(OLDCH)
        ELSE
          CALL BUTTON(103,'X label:',3)
          CALL BUTTON(111,'Y label:',3)
          CALL BUTTON(119,'Plot label:',3)
          CALL BUTTON(104,'not defined',-1)
          CALL BUTTON(112,'not defined',-1)
          CALL BUTTON(120,'not defined',-1)
          CALL BUTTSCH(0.8*OLDCH)
          CALL BUTTON(104,'not defined',-3)
          CALL BUTTON(112,'not defined',-3)
          CALL BUTTON(120,'not defined',-3)
          CALL BUTTSCH(OLDCH)
        END IF
C------------------------------------------------------------------------------
C Caracteristicas de representacion de los datos
        WRITE(CEXPAND,*) IEXPAND
        L1=TRUEBEG(CEXPAND)
        L2=TRUELEN(CEXPAND)
        CALL BUTTON(127,'Exp.(%)='//CEXPAND(L1:L2),0)
        CALL BUTTON(127,'Exp.(%)='//CEXPAND(L1:L2),-8)
C DATA Key
        IF(IDATAKEY.EQ.0)THEN
          CALL BUTTON(128,'KEY off',0)
          CALL BUTTON(128,'KEY off',-8)
        ELSEIF(IDATAKEY.EQ.1)THEN
          CALL BUTTON(128,'KEY on (1)',0)
          CALL BUTTON(128,'KEY on (1)',-8)
        ELSEIF(IDATAKEY.EQ.2)THEN
          CALL BUTTON(128,'KEY on (2)',0)
          CALL BUTTON(128,'KEY on (2)',-8)
        ELSEIF(IDATAKEY.EQ.3)THEN
          CALL BUTTON(128,'KEY on (3)',0)
          CALL BUTTON(128,'KEY on (3)',-8)
        ELSEIF(IDATAKEY.EQ.4)THEN
          CALL BUTTON(128,'KEY on (4)',0)
          CALL BUTTON(128,'KEY on (4)',-8)
        END IF
C Caracteristicas de representacion de los ejes
        L1=TRUEBEG(XOPT)
        L2=TRUELEN(XOPT)
        CALL BUTTON(135,'X:'//XOPT(L1:L2),0)
        CALL BUTTON(135,'X:'//XOPT(L1:L2),-8)
        L1=TRUEBEG(YOPT)
        L2=TRUELEN(YOPT)
        CALL BUTTON(136,'Y:'//YOPT(L1:L2),0)
        CALL BUTTON(136,'Y:'//YOPT(L1:L2),-8)
        WRITE(CAXISLW,*) AXISLW
        WRITE(CAXISCH,'(F7.2)') AXISCH
        IF(AXISJUST.EQ.0)THEN
          CALL BUTTON(143,'JUST=0',0)
          CALL BUTTON(143,'JUST=0',-8)
        ELSE
          CALL BUTTON(143,'JUST=1',0)
          CALL BUTTON(143,'JUST=1',-8)
        END IF
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
        L1=TRUEBEG(CAXISLW)
        L2=TRUELEN(CAXISLW)
        CALL BUTTON(151,'LW='//CAXISLW(L1:L2),0)
        CALL BUTTON(151,'LW='//CAXISLW(L1:L2),-8)
        L1=TRUEBEG(CAXISCH)
        L2=TRUELEN(CAXISCH)
        CALL BUTTON(152,'CH='//CAXISCH(L1:L2),0)
        CALL BUTTON(152,'CH='//CAXISCH(L1:L2),-8)
C------------------------------------------------------------------------------
        END
