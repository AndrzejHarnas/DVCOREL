   
      SUBROUTINE PINTER4P16(II, JJ, KK, IIF, JJF,KKF,N,FAG,VISGF,RSOAF,
     &PBOTF,CAPGOF,CAPOWF,MPOTF,MPGTF,MPWTF,DELT,RHOSCOF,RHOSCWF
     &,RHOSCGF)
c	II, JJ, KK - rozmiar siatki "C" - "coarse" - ci�nieniowej
c	IIF, JJF, KKF - rozmiar siatki "F" - "fine" - nasyceniowej
c	XC(I), YC(J), ZC(K) - wsp�rz �rodk�w blok�w "C"
c	XF(I), YF(J), ZF(K) - wsp�czynnik �rodk�w blok�w "F"
c	PC(I, J, K) - ci�nienie w blokach siatki "C"
c	PF(I, J, K) - ci�nienie w blokach siatki "F"
	
	INCLUDE 'PARAMS.FOR'

	DIMENSION XN(LP1F), YN(LP2F)
	DIMENSION RHOSCOF(LP8F),RHOSCWF(LP8F),RHOSCGF(LP8F)
	
	REAL(8)  XFF,YFF,ZFF,PFF,XCC,YCC,ZCC,PC,PNF,IOCODEF,IDMAXF,
     & SONF,SWNF,SGNF,A1F,A2F,A3F,SUMF,GAMF,QSF,XN,YN,D1,D2,D3,D4,P1,P2,
     & P3,P4,BOF,BGF,BWF,QOF,QWF,QGF,GOWTF,GWWTF,GGWTF,O1F,W1F,O2F,W2F,
     & O3F,W3F,G1F,G2F,G3F,QOWGF,CTF,KXF,KYF,KZF,ELF,TXF,TYF,TZF,PDATF,
     & PDATUMF,GRADF,RSOAF,PPF,POTF,RSOTF,MPOTF,PWTF,BWTF,MPWTF,
     & RSOF,PRTF,CRTF,PGTF,BGTF,MPGTF,PPGF,BBGF,PPWF,BBWF,PBOTF,BBOF,CRF
     & ,BOTF,SATF,KROTF,KRWTF,KRGTF,ITHREEF,BWPTF,PCOWTF,
     & PCGOTF,KROGTF,SWRF,MUOTF,BOPTF,RSOPTF,MUWTF,
     & MUGTF,BGPTF,IPVTF,IROCKF,NROCKF,NPVTF,PSITF,
     & WOROCKF,GOROCKF,CAPGOF,CAPOWF,PBOF,VSLOPEF,PMAXTF
     &,IREPRSF,RHOSCOF,RHOSCWF,RHOSCGF,VISGF,D0X,D1X,D2X,D3X,D0Y,D1Y,D2Y
     &,D3Y,P0X,P1X,P2X,P3X,P0Y,P1Y,P2Y,P3Y,P00,P01,P02,P03,P10,P11,P12,
     & P13,P20,P21,P22,P23,P30,P31,P32,P33

     
     
      DIMENSION VISGF(LP1F,LP2F,LP3F),RSOAF(LP1F,LP2F,LP3F),PBOTF
     &(LP1F,LP2F,LP3F),CAPGOF(LP1F,LP2F,LP3F),CAPOWF(LP1F,LP2F,LP3F),
     &MPOTF(LP8F),MPGTF(LP8F),MPWTF(LP8F),APFF(LP1F,LP2F,LP3F)
	
      COMMON /AVEWSPF/ XFF(LP1F),YFF(LP2F),ZFF(LP3F),PFF(LP1F,LP2F,LP3F)
      COMMON /AVEWSP/ XCC(0:LP1+1),YCC(0:LP2+1),ZCC(0:LP3+1),
     &PC(0:LP1+1,0:LP2+1,0:LP3+1)
	
      COMMON /SARRAYF/ IOCODEF,IDMAXF,
     & A1F(LP1F,LP2F,LP3F),A2F(LP1F,LP2F,LP3F),A3F(LP1F,LP2F,LP3F),
     & SUMF(LP1F,LP2F,LP3F),GAMF(LP1F,LP2F,LP3F),QSF(LP1F,LP2F,LP3F)
	
	COMMON /SPARMF/ KXF(LP1F,LP2F,LP3F),KYF(LP1F,LP2F,LP3F),
     & KZF(LP1F,LP2F,LP3F),
     & ELF(LP1F,LP2F,LP3F),TXF(LP4F,LP2F,LP3F),TYF(LP1F,LP5F,LP3F),
     & TZF(LP1F,LP2F,LP6F),
     & PDATF(LP1F,LP2F,LP3F),PDATUMF,GRADF
     
      COMMON /SSOLNF/ BOF(LP1F,LP2F,LP3F),BWF(LP1F,LP2F,LP3F),
     & BGF(LP1F,LP2F,LP3F),
     & QOF(LP1F,LP2F,LP3F),QWF(LP1F,LP2F,LP3f),QGF(LP1F,LP2F,LP3F),
     & GOWTF(LP1F,LP2F,LP3F),GWWTF(LP1F,LP2F,LP3F),GGWTF(LP1F,LP2F,LP3F)
     & ,O1F(LP4F,LP2F,LP3F),W1F(LP4F,LP2F,LP3F),
     & O2F(LP1F,LP5F,LP3F),W2F(LP1F,LP5F,LP3F),
     & O3F(LP1F,LP2F,LP6F),W3F(LP1F,LP2F,LP6F),
     & G1F(LP4F,LP2F,LP3F),G2F(LP1F,LP5F,LP3F),G3F(LP1F,LP2F,LP6F),
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     & QOWGF(LP1F,LP2F,LP3F),CTF(LP1F,LP2F,LP3F)
	
	 COMMON /SPVTF/ SATF(LP7F,LP9F),KROTF(LP7F,LP9F),KRWTF(LP7F,LP9F),
     & BGTF(LP7F,LP9F),
     & KRGTF(LP7F,LP9F),ITHREEF(LP7F),RSOTF(LP7F,LP9F),BWPTF(LP7F,LP9F),
     & PCOWTF(LP7F,LP9F),PCGOTF(LP7F,LP9F),KROGTF(LP7F,LP9F),SWRF(LP7F),
     & POTF(LP7F,LP9F),MUOTF(LP7F,LP9F),BOTF(LP7F,LP9F),BOPTF(LP7F,LP9F)
     & ,RSOPTF(LP7F,LP9F),PWTF(LP7F,LP9F),MUWTF(LP7F,LP9F),
     & BWTF(LP7F,LP9F),
     & PGTF(LP7F,LP9F),MUGTF(LP7F,LP9F),
     & BGPTF(LP7F,LP9F),CRTF(LP7F,LP9F),IPVTF(LP1F,LP2F,LP3F),
     & IROCKF(LP1F,LP2F,LP3F),
     & NROCKF,NPVTF,PSITF(LP7F,LP9F),PRTF(LP7F,LP9F),WOROCKF(LP7F)
     &,GOROCKF(LP7F)
	
	
	 COMMON /BUBAG/ PBOF,VSLOPEF(LP8F),BSLOPEF(LP8F),RSLOPEF(LP8F),
     & PMAXTF,IREPRSF	
      IF(JJ.GT.1.0.AND.JJF.GT.1.0.AND.KK.GT.1.0.AND.KKF.GT.1.0) GOTO 4
      
      xp = IIF/II
      FB = xp*0.5 + 0.5 
      FBK = IIF - xp*0.5 - 0.5+1
    	xpj =JJF/JJ
    	FBj = xpj*0.5 + 0.5 
    	FBjK = JJF-xpj*0.5 - 0.5+1 

    	K=1
    	JF=J
    	KF=K
c      PC(1,1,1)=PC(1,1,1)+200


      Do 1 I=1,IIF
c      IF(I.LE.FB.AND.xp.GT.1) Then
c      XN(I) = XCC(1)
c      Else
      XN(I) = XFF(I)
c        IF(XN(I).GT.XCC(II)) Then
c        XN(I) = XCC(II)
c        End IF
c      End IF
 1    CONTINUE

      

      Do 11 J=1,JJF
c      IF(J.LE.FBj.AND.xpj.GT.1) Then
c      YN(J) = YCC(1)
c      Else
      YN(J) = YFF(J)
c        IF(YN(J).GT.YCC(JJ)) Then
c        YN(J) = YCC(JJ)
c        End IF
c      End IF
 11    CONTINUE

 
  
     
  
  
      DO 22 JF=1,JJF
      DO 22 IFF=1,IIF
      J0 = 1
      I0 = 1
      
      DO 3 I=1,II
         IF(XCC(I).LE.(XN(IFF) + 0.0001)) Then
         
         I0 = I 
         
         End If
 3    CONTINUE
 
       DO 33 J=1,JJ
         IF(YCC(J).LE.(YN(JF) + 0.0001)) Then
         J0 = J
         End If
 33    CONTINUE
     
      IF(IFF.EQ.178.AND.JF.EQ.178) THEN
      continue
      ENDIF
    
    
      IF(I0.EQ.0) then 
     	I0=1
      ENDIF
      
      IF(J0.EQ.0)then
    	J0=1
      ENDIF
      
      on=1
      IF((J0.EQ.1.OR.I0.EQ.1.OR.J0.GE.JJ-1.OR.I0.GE.II-1).AND.on.EQ.1) 
     &THEN

      IF(I0.EQ.1.AND.J0.EQ.1) THEN 
      J1 = J0 + 1
      J2 = J0 + 2 
      J3 = J0 + 3
      I1 = I0 + 1
      I2 = I0 + 2
      I3 = I0 + 3           
      ENDIF
      
      IF(I0.GT.1.AND.J0.EQ.1) THEN 
      J1 = J0 + 1
      J2 = J0 + 2 
      J3 = J0 + 3
      I1 = I0 - 1
      I2 = I0 + 1
      I3 = I0 + 2           
      ENDIF
      
      IF(I0.EQ.1.AND.J0.GT.1) THEN 
      J1 = J0 - 1
      J2 = J0 + 1 
      J3 = J0 + 2
      I1 = I0 + 1
      I2 = I0 + 2
      I3 = I0 + 3           
      ENDIF
      
      IF(I0.EQ.1.AND.J0.GE.JJ-1) THEN 
      J1 = J0 - 1
      J2 = J0 - 2 
      J3 = J0 - 3
      I1 = I0 + 1
      I2 = I0 + 2
      I3 = I0 + 3        
      ENDIF
      
      IF(I0.GT.1.AND.J0.GE.JJ-1) THEN 
      J1 = J0 - 1
      J2 = J0 - 2 
      J3 = J0 - 3
      I1 = I0 - 1
      I2 = I0 + 1
      I3 = I0 + 2        
      ENDIF
      
      IF(I0.GE.II-1.AND.J0.EQ.1) THEN 
      J1 = J0 + 1
      J2 = J0 + 2 
      J3 = J0 + 3
      I1 = I0 - 1
      I2 = I0 - 2
      I3 = I0 - 3        
      ENDIF
      
      IF(I0.GE.II-1.AND.J0.GT.1) THEN 
      J1 = J0 - 1
      J2 = J0 + 1 
      J3 = J0 + 2
      I1 = I0 - 1
      I2 = I0 - 2
      I3 = I0 - 3        
      ENDIF
      
      IF(I0.GE.II-1.AND.J0.GE.JJ-1) THEN 
      J1 = J0 - 1
      J2 = J0 - 2 
      J3 = J0 - 3
      I1 = I0 - 1
      I2 = I0 - 2
      I3 = I0 - 3        
      ENDIF
      
     
     
      ELSE
      
      
      J1 = J0 - 1
      J2 = J0 + 1 
      J3 = J0 + 2
      I1 = I0 - 1
      I2 = I0 + 1
      I3 = I0 + 2
      
      ENDIF
      
      
      
      
      D0X =(XCC(I0)-XCC(I1))*(XCC(I0)-XCC(I2))*(XCC(I0)-XCC(I3))
      D1X =(XCC(I1)-XCC(I0))*(XCC(I1)-XCC(I2))*(XCC(I1)-XCC(I3))
      D2X =(XCC(I2)-XCC(I0))*(XCC(I2)-XCC(I1))*(XCC(I2)-XCC(I3)) 
      D3X =(XCC(I3)-XCC(I0))*(XCC(I3)-XCC(I1))*(XCC(I3)-XCC(I2))
      
      
      D0Y =(YCC(J0)-YCC(J1))*(YCC(J0)-YCC(J2))*(YCC(J0)-YCC(J3))
      D1Y =(YCC(J1)-YCC(J0))*(YCC(J1)-YCC(J2))*(YCC(J1)-YCC(J3))
      D2Y =(YCC(J2)-YCC(J0))*(YCC(J2)-YCC(J1))*(YCC(J2)-YCC(J3))
      D3Y =(YCC(J3)-YCC(J0))*(YCC(J3)-YCC(J1))*(YCC(J3)-YCC(J2)) 


      P0X = (XN(IFF)-XCC(I1))*(XN(IFF)-XCC(I2))*(XN(IFF)-XCC(I3)) / D0X

      P1X = (XN(IFF)-XCC(I0))*(XN(IFF)-XCC(I2))*(XN(IFF)-XCC(I3)) / D1X

      P2X = (XN(IFF)-XCC(I0))*(XN(IFF)-XCC(I1))*(XN(IFF)-XCC(I3)) / D2X
      
      P3X = (XN(IFF)-XCC(I0))*(XN(IFF)-XCC(I1))*(XN(IFF)-XCC(I2)) / D3X
   
      P0Y = (YN(JF)-YCC(J1))*(YN(JF)-YCC(J2))*(YN(JF)-YCC(J3)) / D0Y

      P1Y = (YN(JF)-YCC(J0))*(YN(JF)-YCC(J2))*(YN(JF)-YCC(J3)) / D1Y

      P2Y = (YN(JF)-YCC(J0))*(YN(JF)-YCC(J1))*(YN(JF)-YCC(J3)) / D2Y
      
      P3Y = (YN(JF)-YCC(J0))*(YN(JF)-YCC(J1))*(YN(JF)-YCC(J2)) / D3Y


      P00 = PC(I0,J0,K)*P0X*P0Y
      P01 = PC(I0,J1,K)*P0X*P1Y
      P02 = PC(I0,J2,K)*P0X*P2Y 
      P03 = PC(I0,J3,K)*P0X*P3Y    
      
      P10 = PC(I1,J0,K)*P1X*P0Y
      P11 = PC(I1,J1,K)*P1X*P1Y
      P12 = PC(I1,J2,K)*P1X*P2Y
      P13 = PC(I1,J3,K)*P1X*P3Y
      
      P20 = PC(I2,J0,K)*P2X*P0Y
      P21 = PC(I2,J1,K)*P2X*P1Y
      P22 = PC(I2,J2,K)*P2X*P2Y
      P23 = PC(I2,J3,K)*P2X*P3Y
      
      P30 = PC(I3,J0,K)*P3X*P0Y
      P31 = PC(I3,J1,K)*P3X*P1Y
      P32 = PC(I3,J2,K)*P3X*P2Y
      P33 = PC(I3,J3,K)*P3X*P3Y
      
      

      PFF(IFF,JF,K) = P00 + P01 + P02 + P03 + P10 + P11 + P12 + P13 
     & + P20 + P21 + P22 + P23 + P30 + P31 + P32 + P33
     
     
       



      


  4    CONTINUE
     
      
  
 
  
 22    CONTINUE
 
      PPF=PFF(1,1,1)
      PPGF   = PPF + CAPGOF(1,1,1)
      PPWF   = PPF - CAPOWF(1,1,1)
      BPTF=PBOTF(1,1,1)
      IPVTRF=IPVTF(1,1,1)
      CALL INTPVTF(IPVTRF,BPTF,RSLOPEF(IPVTRF),POTF,RSOTF,MPOTF(IPVTRF),
     & PPF,RSOF)
      RSOAF(1,1,1) = RSOF
      CALL INTERPF(IPVTRF,PRTF,CRTF,MPGTF(IPVTRF),PPF,CRF)
      CALL INTPVTF(IPVTRF,BPTF,BSLOPEF(IPVTRF),POTF,BOTF,MPOTF(IPVTRF),
     & PPF,BBOF)
      CALL INTERPF(IPVTRF,PWTF,BWTF,MPWTF(IPVTRF),PPWF,BBWF)
      CALL INTERPF(IPVTRF,PGTF,BGTF,MPGTF(IPVTRF),PPGF,BBGF)
 
       continue
 
c     PFF(1,1,1)=PFF(2,1,1)+((-QGF(1,1,1))/(2*TXF(2,1,1)*5*1/
c     & (VISGF(2,1,1)*BBGF)))
     
        DO 350 I=FBK,IIF
        DO 350 J=FBJK,JJF
      
      PFF(I,J,1)= PFF(FBK,FBJK,1)
     
     
 350     continue
 



 7000 RETURN
      END
