      SUBROUTINE CALCGR ( UL, UR, ULRINV)

        Use Blockc
        Implicit Real (KIND=8) ( A-G, O-Z )
        Implicit Integer ( H-N )

        COMPLEX  (Kind=8),  Dimension(:,:) :: UL, UR, ULRINV
        COMPLEX  (Kind=8),  Dimension(:,:), allocatable :: TEMP
        !GRUP (I,J) = < c_i c^+_j >
        !Green functions.
        allocate(TEMP(NDIM,NDIM))
        
        TEMP = DCMPLX(0.D0,0.D0)
        GRUP = DCMPLX(0.D0,0.D0)
        DO NL1 = 1,NE
            DO NL2 = 1,NE
                DO N = 1,Ndim
                    TEMP(N,NL1) = TEMP(N,NL1) + UR(N,NL2)*ULRINV(NL2,NL1)
                ENDDO
            ENDDO
        ENDDO
        
        DO NL = 1,NE
            DO I = 1,Ndim
                DO J = 1,Ndim
                    GRUP(I,J) = GRUP(I,J) + TEMP(J,NL)*UL(NL,I)
                ENDDO
            ENDDO
        ENDDO

        DO N1 = 1,Ndim
            DO N  = 1,Ndim
                TEMP(N,N1) =  ZKRON(N,N1) - GRUP(N1,N)
            ENDDO
        ENDDO
        DO N1 = 1,Ndim
            DO N  = 1,Ndim
                GRUP(N,N1) = TEMP(N,N1)
            ENDDO
        ENDDO
        DO N1 = 1,Ndim
            DO N  = 1,Ndim
                GRUPC(N,N1) =  ZKRON(N,N1) - GRUP(N1,N)
            ENDDO
        ENDDO
        
        deallocate(TEMP)

	RETURN
      END SUBROUTINE CALCGR
