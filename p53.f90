!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   wav :    Periodic waves in an a nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)
      DOUBLE PRECISION  a, k1, r, d1, b, k2, d2, j, x, y
     ! DOUBLE PRECISION R

    a=PAR(1) 
	k1=PAR(2)
	r=PAR(3)
	d1=PAR(4)
	b=PAR(5)
	k2=PAR(6)
	d2=PAR(7)
	j=PAR(8)
	
	
	x=U(1)
    y=U(2)

	
	 
	F(1)= a*x**2/(k1+x**2)-r * x*y/(x+j)-d1*x
	F(2)= b*x**4/(k2+x**4)-d2*y
      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T



    

	 
	   PAR(1) = 0.1           !alpha  
       PAR(2) = 2.5           !k1  
       PAR(3) = 0.015         !r
       PAR(4) = 0.01          !d1
	   PAR(5) = 0.2           !b
	   PAR(6) = 4             !k2
	   PAR(7) = 0.03          !d2
	   PAR(8) = 0.0261        !j

         
		
		
		 U(1)=0.969925
	     U(2)=1.2078
		
		

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
