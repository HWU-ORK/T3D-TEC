      MODULE TEC_PARAM
!----------------------------------------------------------
!     DATA FILE STRUCTURE
!      N (number of turbines) L1 (bottom layer) L2 (top layer)
!      X  Z  Y  L  W  Theta(Deg)  R  DD  Cd  Cp  Vcut-in  Vcut-out  Vr
!     PARAMETERS READ IN THE FORMATTED DATA FILE
      INTEGER:: NTEC
      INTEGER:: L1TEC
      INTEGER:: L2TEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: XTEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: YTEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: ZTEC

!     PARAMETERS DEFINNING THE TEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: THETA
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: HDL
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: HDW
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: RTEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: DD
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: CDTEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: CPTEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: VCUTIN
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: VCUTOUT
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: VRT
!     
!     PARAMETERS USED TO DEFINE THE NODES WITHIN EACH TEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: AREA
      DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE :: VOLUME_HYDR 	  
      INTEGER,DIMENSION(:),ALLOCATABLE:: NNODES
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: INODES
!     ELEMENT AND PROCESSOR OF TEC CENTRE
      INTEGER,DIMENSION(:),ALLOCATABLE:: TECELEM
      INTEGER,DIMENSION(:),ALLOCATABLE:: TECPID
!     PARAMETERS USED TO FIND THE FAR VELOCITY
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: DDELEM
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: DDPID
!     EXTRACTED POWER
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: PTEC
	  DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: CTTEC   ! +
!     FLOW ANGLES
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: THETATEC
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: DELTATHETA
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: URREL
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: UR
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: HALFCDSCOSTHETA
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: HALFCDSSINTHETA
!		Diagnostic
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: DIAG_CUTIN
      END MODULE
!----------------------------------------------------------

!                    *****************
                     SUBROUTINE USER_SOURCE
!                    *****************
     & (S0U,S0V,S0W,S1U,S1V,S1W,
     &  UN3,VN3,WSN3,WN3,
     &  VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD,
     &  NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
!
!
!***********************************************************************
! TELEMAC3D   V8P1                                11/12/2019
!***********************************************************************
!
!history  J-M HERVOUET (LNHE)
!+        29/08/2008
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!----------------------------------------------------------------
!history M.ANDREEWSKY (LNHE)
!+       24/10/2012
!+       Adding drag force to simulate tidal turbines.
!----------------------------------------------------------------
!history J.COUSINEAU (NRC)
!+       13/02/2016
!+       V7P0
!+       Modify drag force to simulate tidal turbines.
!----------------------------------------------------------------
!history  M.ALMOGHAYER (HWU)
!+        10/08/2018
!+        V7P3r1
!+   CREATING CODE FOR DRAG FORCE ON VERTICAL STRUCTURES IN 3D
!+   USER CAN SPECIFY IN WHICH LAYERS THE TEC IS LOCATED, ALSO
!+   THE Z LOCATION OF THE TEC.                     
!
!----------------------------------------------------------------
!history  M.ALMOGHAYER (HWU)
!+        04/03/2019
!+        V8P0
!+   ADDING VCUT-OUT & V RATED                     
!
!----------------------------------------------------------------
!history  M.ALMOGHAYER (HWU)
!+        15/01/2020
!+        V8P1
!+   MAKING SOME CORRECTIONS + ADDING CD EQUATION  
!----------------------------------------------------------------
! history RAYMOND LAM (UoE)
!+        03/10/2023
!+        V8P3
!+   Included the fixed TEC orientation during flood and ebb
!+   (THETATEC)
!+   Included the calculation of URREL 
!+   (Velocity vector aligned with the TEC orientation)
!+   Modified the Cp calculation and used URREL to get Cp
!+   Modified source term equations and used a scalling factor
!+   Included a condition to check if the distance between the 
!+	 nodes and TEC centre are within the specified radius
!+ 	 Added printout variables for model diagnostic
 
!----------------------------------------------------------------
!| AT             |-->| TIME
!| DT             |-->| TIME STEP
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |<->| NUMBER OF PLANE FOR SOURCES
!| LT             |-->| ITERATION NUMBER
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NONHYD         |-->| LOGICAL FOR NON-HYDROSTATIC OPTION
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
!| QSCE           |-->| WATER DISCHARGE OF SOURCES
!| S0U            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES U
!| S0V            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES V
!| S0W            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES W
!| S1U            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES U
!| S1V            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES V
!| S1W            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES W
!| T3             |<->| WORK ARRAY: NOT USED
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| USCE           |-->| VELOCITY FOR SOURCE
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| VSCE           |-->| VELOCITY FOR SOURCE
!| WN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| WSN3           |-->| SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!----------------------------------------------------------------
!-------------------- IMPORTANT ---------------------------------
! PLANES HAVE TO BE FIXED AT LEAST UNTIL THE TOP OF THE TURBINES.
! AFTER THE TOP, PLANES CAN BE MOVED WITHIN THE TIDE.
!----------------------------------------------------------------
!----------------------------------------------------------------
      USE BIEF
!      USE DECLARATIONS_TELEMAC3D     
      USE DECLARATIONS_TELEMAC3D, ONLY : UNSV3D,MESH3D,MESH2D,T3_01, 
     &                                   UN,VN,WN,SVIDE,LISPRD,IKLE2, 
     &                                   T3D_FILES,T3DRFO,ZPLANE,HN,
     &                                   NELEM2,NPLAN,T3DFO1,T3DFO2,
     &                                   X,Y,Z,RHO0, GRAPRD
            
      USE TEC_PARAM
      
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE      
!
!----------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: AT,DT
      LOGICAL, INTENT(IN)           :: NONHYD
!
      INTEGER, INTENT(IN)           :: NPOIN2
      INTEGER, INTENT(IN)           ::           NSCE
      INTEGER, INTENT(IN)           ::           ISCE(NSCE)
      INTEGER, INTENT(IN)           ::           KSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  QSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  USCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  VSCE(NSCE)
!      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3),Y(NPOIN3),Z(NPOIN3)	  
!
!
      INTEGER I, J, K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,NSOM,DISCLIN
      DOUBLE PRECISION SOM,XSOM(4),YSOM(4)
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL P_DSUM
!
!----------------------------------------------------------
! ALLOCATE THE EXTRA VARIABLES FOR TEC
!----------------------------------------------------------
      DOUBLE PRECISION PI, DTR, RTD, TWOPI
      DOUBLE PRECISION HDLCOSTHETA, HDLSINTHETA
      DOUBLE PRECISION HDWCOSTHETA, HDWSINTHETA
      INTEGER IPOIN,IANGLE,INODE,IPOIN2D
      DOUBLE PRECISION ALPHA1,XDD,YDD
      DOUBLE PRECISION UTECX,UTECY,UREFX,UREFY
      INTEGER N1,N2,N3
!      INTEGER LT  
      DOUBLE PRECISION DET1,DET2,DET3,SURDET
!      DOUBLE PRECISION HALFCDSCOSTHETA,HALFCDSSINTHETA
      DOUBLE PRECISION HI,UI,VI
	  DOUBLE PRECISION A
!	  DOUBLE PRECISION A, CTTEC(I)
      DOUBLE PRECISION ZMIN,ZMAX
      DOUBLE PRECISION DIST	  
!	  New variable for explicit implementation	  
!      DOUBLE PRECISION URR,VOLUME			  
      
      INTEGER :: NNODESCOUNT
!----------------------------------------------------------
! ALLOCATE THE EXTRA VARIABLES TO WRITE THE POWER EXTRACTED
!----------------------------------------------------------
      INTEGER POWRES
!
      INTEGER P_IMAX
      EXTERNAL P_IMAX      
!
!----------------------------------------------------------
! DEFINE THE CONSTANTS USED TO CALCULATE ANGLES OF TEC
!----------------------------------------------------------
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
!----------------------------------------------------------
      
!-----------------------------------------------------------------------
!
!     BEWARE : BE SURE TO DO S0U = S0U + YOUR SOURCE TERMS
!              BECAUSE S0U HAS ALREADY BEEN INITIALISED IN TRISOU
!
!
!-----------------------------------------------------------------------
!
!     CODE FOR DRAG FORCE ON VERTICAL STRUCTURES
!      
!-----------------------------------------------------------------------   
!
!-----------------------------------------------------------------------
!     LOCAL CONSTANTS
!-----------------------------------------------------------------------    
      TWOPI = 2.D0*PI
!        
!-----------------------------------------------------------------------   
!     COMPUTES THE MASSE INTEGRALS
!-----------------------------------------------------------------------
      CALL VECTOR (T3_01,'=','MASBAS          ',11,1.D0,SVIDE,
     &      SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,.FALSE.,SVIDE)

      S1U%TYPR='Q'
      S1V%TYPR='Q'

      IF(NONHYD) THEN
        S0W%TYPR='0'
        S1W%TYPR='Q'
      ENDIF 
!-----------------------------------------------------------------------      
!     INITIALIZATION OF SOURCE TERMS
!-----------------------------------------------------------------------     
      DO K=1,NPOIN3
       S1U%R(K)=0.D0
       S1V%R(K)=0.D0
       IF(NONHYD) THEN ! IF NOT HYDROSTATIC CASE
        S1W%R(K)=0.D0
       ENDIF 
      ENDDO ! END OF DO K=1,NPOIN3  
!----------------------------------------------------------
! ON THE FIRST TIME STEP STORE ALL THE TEC PARAMETERS IN
! THE MODULE
!----------------------------------------------------------
! FILE T2D_FILES(T2DFO2)%LU IS DEFINED IN THE STEERING FILE OF TELEMAC-2D
! WITH THE KEYWORD:
! FORMATTED DATA FILE 2
 !     LT = NINT(AT/DT)
      NPLAN = NPOIN3/NPOIN2
      IF(LT.EQ.1) THEN ! BE CAREFUL WHEN RESTARTING CALCULATION LT CANNOT BE 1
!       READ THE NUMBER OF TEC
        READ(T3D_FILES(T3DFO2)%LU,*) NTEC,L1TEC,L2TEC
        WRITE(LU,*) '=========================='
        WRITE(LU,*) 'NTEC=',NTEC
!       ALLOCATING THE TEC PARAMETERS
        WRITE(LU,*) 'ALLOCATING TEC PARAMETERS'
!
        ALLOCATE(XTEC(NTEC))
        ALLOCATE(YTEC(NTEC))
!
        ALLOCATE(ZTEC(NTEC))   
!
        ALLOCATE(THETA(NTEC))
        ALLOCATE(HDL(NTEC))
        ALLOCATE(HDW(NTEC))
        ALLOCATE(RTEC(NTEC))
        ALLOCATE(DD(NTEC))
        ALLOCATE(CDTEC(NTEC))
        ALLOCATE(CPTEC(NTEC))
        ALLOCATE(VCUTIN(NTEC))
        ALLOCATE(VCUTOUT(NTEC))
        ALLOCATE(VRT(NTEC))
!        
        ALLOCATE(AREA(NTEC))
        ALLOCATE(VOLUME_HYDR(NTEC))		
        ALLOCATE(NNODES(NTEC))
        ALLOCATE(INODES(NTEC,1000))
!
        ALLOCATE(TECELEM(NTEC))
        ALLOCATE(TECPID(NTEC))
!
        ALLOCATE(DDELEM(NTEC,-180:180))
        ALLOCATE(DDPID(NTEC,-180:180))
!
        ALLOCATE(PTEC(NTEC))
        ALLOCATE(CTTEC(NTEC))
!		
	ALLOCATE(THETATEC(NTEC))		!new 
	ALLOCATE(DELTATHETA(NTEC))
	ALLOCATE(URREL(NTEC))
	ALLOCATE(UR(NTEC))
	ALLOCATE(HALFCDSCOSTHETA(NTEC))
	ALLOCATE(HALFCDSSINTHETA(NTEC))	
	ALLOCATE(DIAG_CUTIN(NTEC))		
	
!       SET THE VALUES OF THE TEC PARAMETERS
        WRITE(LU,*) '=========================='
        WRITE(LU,*) 'TEC PARAMETERS DEFINED IN FORTRAN FILE'
        DO I = 1,NTEC
!         READING COORDINATES OF THE TURBINES CENTRES
          READ(T3D_FILES(T3DFO2)%LU,*) XTEC(I),YTEC(I),
     &               ZTEC(I),HDL(I),HDW(I),
     &               THETA(I),RTEC(I),DD(I),
     &               CDTEC(I),CPTEC(I),VCUTIN(I),VCUTOUT(I),VRT(I)
          WRITE(LU,*) ' '
          WRITE(LU,*) 'Turbine index : ',I
          WRITE(LU,*) 'Position x,y of the turbine : ',
     &    XTEC(I),YTEC(I)
          WRITE(LU,*) ' '
!
          THETA(I) = THETA(I)*PI/180.D0
          HDL(I) = HDL(I)/2.D0+0.05D0
          HDW(I) = HDW(I)/2.D0+0.05D0
!
        END DO
        WRITE(LU,*) '=========================='

       !----------------------------------------------------------------
!       STORING THE AREA OF THE TEC
       !----------------------------------------------------------------    
       DO I=1,NTEC      
!          
!         WIDTH AND LENGTH ALONG X AND Y COORDINATES
          HDLCOSTHETA = HDL(I)*COS(-THETA(I))
          HDLSINTHETA = HDL(I)*SIN(-THETA(I))
          HDWCOSTHETA = HDW(I)*COS(-THETA(I))
          HDWSINTHETA = HDW(I)*SIN(-THETA(I))
!
          NSOM=4
          XSOM(1) = XTEC(I) + HDWCOSTHETA + HDLSINTHETA
          YSOM(1) = YTEC(I) + HDWSINTHETA - HDLCOSTHETA
          XSOM(2) = XTEC(I) + HDWCOSTHETA - HDLSINTHETA
          YSOM(2) = YTEC(I) + HDWSINTHETA + HDLCOSTHETA
          XSOM(3) = XTEC(I) - HDWCOSTHETA - HDLSINTHETA
          YSOM(3) = YTEC(I) - HDWSINTHETA + HDLCOSTHETA
          XSOM(4) = XTEC(I) - HDWCOSTHETA + HDLSINTHETA
          YSOM(4) = YTEC(I) - HDWSINTHETA - HDLCOSTHETA
!
          AREA(I)=0.D0 
          NNODES(I)=0
          VOLUME_HYDR(I) = 0.D0		  
!
!         NUMBER OF NODES IN TEC ON PLANE
          DO IPOIN=1,NPOIN2
            IF(INPOLY(X(IPOIN),Y(IPOIN),
     &         XSOM,YSOM,NSOM))THEN
              AREA(I) = AREA(I) + T3_01%R(IPOIN)
              NNODES(I) = NNODES(I) + 1
              IF(NNODES(I).GT.1000)THEN     
                WRITE(LU,*)'DRAGFO: TOO MANY NODES IN TEC'
                WRITE(LU,*)' MODIFY ALLOC OF INODES'
                CALL PLANTE(1)
                STOP
              ENDIF
              INODES(I,NNODES(I)) = IPOIN
            ENDIF
          ENDDO                  
!         IN PARALLEL THE AREA MAY BE SPLIT
!         INTO SEVERAL SUB-DOMAINS
          IF(NCSIZE.GT.0) AREA(I)=P_DSUM(AREA(I))

!		Adds up volume of TEC		  
	    DO J=L1TEC,L2TEC		  
          DO IPOIN=1,NPOIN2
            IF(INPOLY(X(IPOIN),Y(IPOIN),
     &         XSOM,YSOM,NSOM))THEN
!        IF(ZPLANE%R(J).LE.ZMAX.AND.ZPLANE%R(J).GE.ZMIN) THEN	 
		 VOLUME_HYDR(I) = VOLUME_HYDR(I) +
     &                 1.D0/UNSV3D%R(IPOIN+(J-1)*NPOIN2)
!		END IF
            ENDIF
          ENDDO
		END DO		  
	 IF(NCSIZE.GT.0) VOLUME_HYDR(I)=P_DSUM(VOLUME_HYDR(I))
	 
          WRITE(LU,*) 'Area_initial : ',AREA(I)	 
          WRITE(LU,*) 'Volume_initial : ',VOLUME_HYDR(I)		  
!
!         LOOK FOR THE ELEM NUM FOR ALL ANGLES OF ALPHA
          DO IANGLE=-180,180
            XDD = XTEC(I) - DD(I)*COS(DBLE(IANGLE)*DTR)
            YDD = YTEC(I) + DD(I)*SIN(DBLE(IANGLE)*DTR)
!           LOOK IN LOCAL MESH IF ELEMENT IS IN MESH
            TECELEM(I)=0
            TECPID(I)=0
            DDELEM(I,IANGLE)=0
            DDPID(I,IANGLE)=0
            DO IELEM=1,NELEM2
!             GET THE VERTICES
              N1=IKLE2%I(IELEM)
              N2=IKLE2%I(NELEM2+IELEM)
              N3=IKLE2%I(2*NELEM2+IELEM)
!             FIND THE ELEM AND PROC NUM OF THE TEC
!             FOR ALL ANGLES
              DET1=(X(N3)-X(N2))*(YTEC(I)-Y(N2))
     &             -(XTEC(I)-X(N2))*(Y(N3)-Y(N2))
              DET2=(X(N1)-X(N3))*(YTEC(I)-Y(N3))
     &             -(XTEC(I)-X(N3))*(Y(N1)-Y(N3))
              DET3=(X(N2)-X(N1))*(YTEC(I)-Y(N1))
     &             -(XTEC(I)-X(N1))*(Y(N2)-Y(N1))
              IF((DET1.GE.0.D0).AND.
     &          (DET2.GE.0.D0).AND.
     &          (DET3.GE.0.D0)) THEN
                TECELEM(I)=IELEM
                IF(NCSIZE.GT.0) TECPID(I)=IPID
              END IF
!             FIND THE ELEM AND PROC NUM AT DD
              DET1=(X(N3)-X(N2))*(YDD-Y(N2))-
     &             (XDD-X(N2))*(Y(N3)-Y(N2))
              DET2=(X(N1)-X(N3))*(YDD-Y(N3))
     &             -(XDD-X(N3))*(Y(N1)-Y(N3))
              DET3=(X(N2)-X(N1))*(YDD-Y(N1))
     &             -(XDD-X(N1))*(Y(N2)-Y(N1))
              IF((DET1.GE.0.D0).AND.
     &           (DET2.GE.0.D0).AND.
     &           (DET3.GE.0.D0)) THEN
                DDELEM(I,IANGLE)=IELEM
                IF(NCSIZE.GT.0) DDPID(I,IANGLE)=IPID
              END IF
            END DO
!
            IF(NCSIZE.GT.0)THEN
              TECPID(I)=P_IMAX(TECPID(I))
              TECELEM(I)=P_IMAX(TECELEM(I))
              IF(IPID.NE.TECPID(I))THEN
                TECELEM(I)=-TECELEM(I)
              END IF
!
              DDPID(I,IANGLE)=P_IMAX(DDPID(I,IANGLE))
              DDELEM(I,IANGLE)=P_IMAX(DDELEM(I,IANGLE))
              IF(IPID.NE.DDPID(I,IANGLE))THEN
                DDELEM(I,IANGLE)=-DDELEM(I,IANGLE)
              END IF
            ENDIF
!
            IF(TECELEM(I).EQ.0)THEN
              WRITE(LU,*) 'DRAGFO:POSITION FOR TEC',I
              WRITE(LU,*) ' IS OUTSIDE OF THE DOMAIN'
              CALL PLANTE(1)
              STOP
            END IF
!
            IF(DDELEM(I,IANGLE).EQ.0)THEN
              WRITE(LU,*)'DRAGFO:POSITION FOR TEC',I
              WRITE(LU,*)'IS TOO CLOSE TO EDGE OF DOMAIN'
              CALL PLANTE(1)
              STOP
            END IF
          END DO
       END DO
      END IF
!-----------------------------------------------------------------------
! APPLY THE DRAG FORCE OF THE TEC
!-----------------------------------------------------------------------       
      DO I = 1,NTEC
!        PTEC(I) = 0.D0        ! relocate it to under if for the plane (j)
        ZMAX = ZTEC(I)+RTEC(I) ! + POSITION MAX OF PLANES SURROUNDING THE TURBINE ITEC
        ZMIN = ZTEC(I)-RTEC(I) ! + POSITION MIN OF PLANES SURROUNDING THE TURBINE ITEC
!        HI=2*RTEC(I)    ! Instead of considring the whole depth HI=H%R(IPOIN)
!           
       DO J=L1TEC,L2TEC
 !        TECELEM(I)=0      !!! CHECK - maybe need to be deleted
 !        TECPID(I)=0       !!! CHECK - maybe need to be deleted
        IF(ZPLANE%R(J).LE.ZMAX.AND.ZPLANE%R(J).GE.ZMIN) THEN   ! +
!
          PTEC(I) = 0.D0
		  UTECX=0.D0
          UTECY=0.D0 
          IF(IPID.EQ.TECPID(I))THEN
            N1=IKLE2%I(TECELEM(I))
            N2=IKLE2%I(NELEM2+TECELEM(I))
            N3=IKLE2%I(2*NELEM2+TECELEM(I))
!
!           N1, N2, N3 are the three nodes of the element containing either the TEC 
!             or a point at a distance DD upstream and at every angle from 0-359
            N1=N1+(J-1)*NPOIN2       !!! CHECK
            N2=N2+(J-1)*NPOIN2       !!! CHECK
            N3=N3+(J-1)*NPOIN2       !!! CHECK
!
!           SURDET: is 1/determinant, which is a facter needed to switch 
!                   to isoparametric element in finite elements
            SURDET=1.D0/((X(N2)-X(N1))*(Y(N3)-Y(N1))-
     &             (X(N3)-X(N1))*(Y(N2)-Y(N1)))
!           CALCULATE THE DETERMINANT
            DET1=(X(N3)-X(N2))*(YTEC(I)-Y(N2))
     &               -(XTEC(I)-X(N2))*(Y(N3)-Y(N2))
            DET2=(X(N1)-X(N3))*(YTEC(I)-Y(N3))
     &               -(XTEC(I)-X(N3))*(Y(N1)-Y(N3))
            DET3=(X(N2)-X(N1))*(YTEC(I)-Y(N1))
     &               -(XTEC(I)-X(N1))*(Y(N2)-Y(N1))
!                  DET*SURDET IS THE SHAPE COEFFECIENT 
!                  DET1*SURDET+DET2*SURDET+DET3*SURDET =1          
            UTECX=UN%R(N1)*DET1*SURDET+
     &            UN%R(N2)*DET2*SURDET+
     &            UN%R(N3)*DET3*SURDET
            UTECY=VN%R(N1)*DET1*SURDET+
     &            VN%R(N2)*DET2*SURDET+
     &            VN%R(N3)*DET3*SURDET
          
          END IF
          IF(NCSIZE.GT.0)UTECX=P_DSUM(UTECX)
          IF(NCSIZE.GT.0)UTECY=P_DSUM(UTECY)
          ALPHA1=-ATAN2(UTECY,UTECX)*RTD ! PI()+ ALPHA
!         FIND THE FAR VELOCITY
          IANGLE=INT(ALPHA1+0.5D0)
          XDD = XTEC(I) - DD(I)*COS(DBLE(IANGLE)*DTR)
          YDD = YTEC(I) + DD(I)*SIN(DBLE(IANGLE)*DTR)
          UREFX=0.D0
          UREFY=0.D0
          IF(IPID.EQ.DDPID(I,IANGLE))THEN
            N1=IKLE2%I(DDELEM(I,IANGLE))
            N2=IKLE2%I(NELEM2+DDELEM(I,IANGLE))
            N3=IKLE2%I(2*NELEM2+DDELEM(I,IANGLE))
!
            N1=N1+(J-1)*NPOIN2
            N2=N2+(J-1)*NPOIN2
            N3=N3+(J-1)*NPOIN2 
!

            SURDET=1.D0/((X(N2)-X(N1))*(Y(N3)-Y(N1))-
     &             (X(N3)-X(N1))*(Y(N2)-Y(N1)))
!
            DET1=(X(N3)-X(N2))*(YDD-Y(N2))
     &           -(XDD-X(N2))*(Y(N3)-Y(N2))
            DET2=(X(N1)-X(N3))*(YDD-Y(N3))
     &           -(XDD-X(N3))*(Y(N1)-Y(N3))
            DET3=(X(N2)-X(N1))*(YDD-Y(N1))
     &           -(XDD-X(N1))*(Y(N2)-Y(N1))       
!         
            UREFX=UN%R(N1)*DET1*SURDET+
     &            UN%R(N2)*DET2*SURDET+
     &            UN%R(N3)*DET3*SURDET
            UREFY=VN%R(N1)*DET1*SURDET+
     &            VN%R(N2)*DET2*SURDET+
     &            VN%R(N3)*DET3*SURDET

          END IF
          IF(NCSIZE.GT.0) UREFX=P_DSUM(UREFX)
          IF(NCSIZE.GT.0) UREFY=P_DSUM(UREFY)
	  THETA(I) = ATAN2(UREFY,UREFX)      ! + CHANGE THE ANGLE TO THE FLOW DIRECTION (ATAN2 {POS COUNTER CLOCK})
!		   UR=UREFX*COS(THETA(I))-UREFY*SIN(THETA(I))  ! - TO BE RE-ACTIVATED IF THE TEC DOESNT FACE THE FLOW
	  UR(I)=UREFX*COS(THETA(I))+UREFY*SIN(THETA(I)) ! + CHANGED THE SIGN TO MATCH FLOW DIR
	  THETATEC(I) = -46.7D0*DTR			!default TEC angle 		
	  DELTATHETA(I) = THETA(I)-THETATEC(I)	!initialise 	
	  URREL(I) = UR(I)*COS(DELTATHETA(I)) 		!initialise 
	  IF((UREFX.LT.0).AND.(UREFY.GT.0))THEN	! ATAN2 angle for EBB in radian (degree to radian - DTR)	  
		THETATEC(I) = 130.2D0*DTR		!set TEC orientation
		DELTATHETA(I) = THETA(I)-THETATEC(I)
		URREL(I) = UR(I)*COS(DELTATHETA(I))
	  ELSE IF((UREFX.GT.0).AND.(UREFY.LT.0))THEN ! ATAN2 angle for FLOOD
		THETATEC(I) = -46.7D0*DTR		!set TEC orientation
		DELTATHETA(I) = THETA(I)-THETATEC(I)
		URREL(I) = UR(I)*COS(DELTATHETA(I))
	  END IF

!		  CALCULATE CT
	  CTTEC(I)=0
	  IF((URREL(I).GE.0.0D0).AND.(URREL(I).LT.0.9D0))THEN
		CPTEC(I)= 0.2526D0
	  END IF
	  IF((URREL(I).GE.0.9D0).AND.(URREL(I).LT.1.5D0))THEN
		CPTEC(I)= -0.7029D0*URREL(I)**3+2.3336D0*URREL(I)**2
     &				-2.2687D0*URREL(I)+0.9203D0
	  END IF 
	  IF((URREL(I).GE.1.5D0).AND.(URREL(I).LT.2.5D0))THEN
		CPTEC(I)= -0.2665D0*URREL(I)**5+2.2133D0*URREL(I)**4
     &			-6.8703D0*URREL(I)**3+9.4613D0*URREL(I)**2
     &			-4.9041D0*URREL(I)+0.4697D0
	  END IF
	  IF((URREL(I).GE.2.5D0).AND.(URREL(I).LT.3.0D0))THEN
		CPTEC(I)= 0.7489D0*URREL(I)**3-6.3413D0*URREL(I)**2
     &				+17.5598D0*URREL(I)-15.5366D0
	  END IF
	  IF((URREL(I).GE.3.0D0).AND.(URREL(I).LT.5.0D0))THEN
		CPTEC(I)= 0.0789D0*URREL(I)**2-0.7457D0*URREL(I)
     &				+1.8177D0
	  END IF   			
	  IF((URREL(I).GE.5.0D0)) CPTEC(I)=0.0617D0
	  IF((URREL(I).LT.0.0D0)) CPTEC(I)=0.2526D0
		A= 0.571D0*CPTEC(I)**2+0.0224D0*CPTEC(I)+0.0328D0 ! + INDUCTION FACTOR
		  CTTEC(I)=4*A*(1-A)            ! + CD EQUATION - OR:
!		  CTTEC(I)=CPTEC(I)/(1-A)            ! + CD EQUATION
!         DO A LOOP OVER ALL THE AFFECTED NODES

		DIAG_CUTIN(I)=1D0
          WRITE(LU,*) 'Volume_dis2 : ',VOLUME_HYDR(I)
          WRITE(LU,*) 'Area_dis2 : ',AREA(I)
		  
!		VOLUME_HYDR(I)=2544.69D0
          DO INODE=1,NNODES(I)
            IPOIN2D = INODES(I,INODE)
            IPOIN = IPOIN2D+(J-1)*NPOIN2
!
              HI=HN%R(IPOIN)
              UI=UN%R(IPOIN)
              VI=VN%R(IPOIN)
			  		DIAG_CUTIN(I)=2D0
            IF((ABS(UR(I)).GE.VCUTIN(I)))THEN
!	          THETA(I) = ATAN2(VI,UI)               !!!!!!!!!!!!!!!! Test
				DIAG_CUTIN(I)=2D0	
		  
              HALFCDSCOSTHETA(I)=0.5D0*(PI*RTEC(I)**2)
     &                        *4.5D0*CTTEC(I)*COS(THETA(I)) !! 
              HALFCDSSINTHETA(I)=0.5D0*(PI*RTEC(I)**2)
     &                        *4.5D0*CTTEC(I)*SIN(THETA(I)) !!
		IF(INODE.EQ.1)THEN	 
          WRITE(LU,*) 'Diag_cutin : ',DIAG_CUTIN(I)	 
          WRITE(LU,*) 'cdcos : ',HALFCDSCOSTHETA(I)	
          WRITE(LU,*) 'cdsin : ',HALFCDSSINTHETA(I)
          WRITE(LU,*) 'Volume_dis3 : ',VOLUME_HYDR(I)
          WRITE(LU,*) 'Area_dis3 : ',AREA(I)
			END IF
			
              DIST = SQRT((X(IPOIN2D)-511115.428D0)**2 
     &			+(Y(IPOIN2D)-6555303.064D0)**2 
     &			+ (Z(IPOIN)+26.6441D0)**2)
            IF(DIST.LE.9.6D0)THEN	 

		IF((ABS(UI).GT.1.D-4))THEN
                  S1U%R(IPOIN)=ABS(HALFCDSCOSTHETA(I))*ABS(UR(I))
     &            				/2544.69D0
		ELSEIF((ABS(VI).GT.1.D-4))THEN
                  S1V%R(IPOIN)=ABS(HALFCDSSINTHETA(I))*ABS(UR(I))
     &            				/2544.69D0
                END IF
		END IF	
	       END IF
          END DO
!        CALCULATE POWER    
          IF((ABS(UR(I)).GE.VCUTIN(I)).AND.
     &       (ABS(UR(I)).LT.VCUTOUT(I)))THEN
            IF(ABS(UR(I)).LE.VRT(I)) THEN          
              PTEC(I) = CPTEC(I)*PI*RTEC(I)**2*0.5D0
     &                  *RHO0*UR(I)**2*ABS(UR(I))
            ELSE
              PTEC(I) = 1008102.0D0
            END IF
          ELSE IF(ABS(UR(I)).GE.VCUTOUT(I))THEN
	        PTEC(I) = 1008102.0D0
	  END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          PTEC(I)=0.D0            !!! delete
!          PTEC(I)= CTTEC(I)             !!! delete
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!		  
        END IF      
       END DO
!        IF((ABS(UR).GT.VCUTIN(I)).AND.
!     &       (ABS(UR).LT.VCUTOUT(I)))THEN
!          IF(ABS(UR).LT.VRT(I)) THEN          
!            PTEC(I) = CPTEC(I)*PI*RTEC(I)**2*0.5D0
!     &                *RHO0*UR**2*ABS(UR)  ! relocate it to under if for the plane (j)
!          ELSE
!            PTEC(I) = CPTEC(I)*PI*RTEC(I)**2*0.5D0
!     &                *RHO0*VRT(I)**3
!          END IF
!        END IF                              
!
      END DO
	  
!-----------------------------------------------------------------------
!

!----------------------------------------------------------
! WRITING THE POWER OUTPUT
!----------------------------------------------------------
! WRITING HEADER OF FILE
      IF((IPID.EQ.0).AND.(LT.EQ.1))THEN
!       FILE ID
        POWRES = T3D_FILES(T3DRFO)%LU
!       HEADER
        WRITE(POWRES,'(A)') "# TEC power result file:"
        WRITE(POWRES,'(A)') "# Power, theta and thetatec at time T"
        WRITE(POWRES,'(A)') "# for each TEC modelled"
!       VARIABLE NAMES
        WRITE(POWRES,'(A)',ADVANCE='NO') "T, "
        DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "P_",I,", "
        END DO
        WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "P_",NTEC,", "							
        DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Theta_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Theta_",NTEC,", "		!new
        DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "ThetaTEC_",I,", " 	!new
        END DO	
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "ThetaTEC_",NTEC,", "
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Cp_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Cp_",NTEC,", "		!new
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Ct_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Ct_",NTEC,", "		!new
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "UR_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "UR_",NTEC,", "		!new
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "URREL_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "URREL_",NTEC,", "		!new
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "DIAG_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "DIAG_",NTEC,", "		!new
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Cdsin_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Cdsin_",NTEC,", "		!new	
	DO I = 1,NTEC-1
          WRITE(POWRES,'(A,I0.4,A)',ADVANCE='NO') "Volume_",I,", " 	!new
        END DO
	WRITE(POWRES,'(A,I0.4)') "Volume_",NTEC		!new	
!       INITIAL RESULT
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0		
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0		
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0		
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0		
	DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') 0.D0
        END DO		
        WRITE(POWRES,'(F0.6,X)') 0.D0		
      END IF
!     WRITING RESULTS IN TIME
      IF((IPID.EQ.0).AND.(MOD(LT,GRAPRD).EQ.0))THEN
!       FILE ID
        POWRES = T3D_FILES(T3DRFO)%LU
!       CALCULATED RESULTS
!        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') AT
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') LT*DT
        DO I = 1,NTEC-1
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') PTEC(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') PTEC(NTEC)
        DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') THETA(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') THETA(NTEC)
        DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') THETATEC(I)
        END DO		
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') THETATEC(NTEC)	
	DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') CPTEC(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') CPTEC(NTEC)
	DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') CTTEC(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') CTTEC(NTEC)
	DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') UR(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') UR(NTEC)
	DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') URREL(I)
        END DO
        WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') URREL(NTEC)
	DO I = 1,NTEC-1												!new 
	  WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') DIAG_CUTIN(I)
        END DO
	WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') DIAG_CUTIN(NTEC)
	DO I = 1,NTEC-1												!new 
	  WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') HALFCDSSINTHETA(I)
        END DO
	WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') HALFCDSSINTHETA(NTEC)		
	DO I = 1,NTEC-1												!new 
          WRITE(POWRES,'(F0.6,X)',ADVANCE='NO') VOLUME_HYDR(I)
        END DO
        WRITE(POWRES,'(F0.6,X)') VOLUME_HYDR(NTEC)	
      END IF
!
      RETURN
      END  
