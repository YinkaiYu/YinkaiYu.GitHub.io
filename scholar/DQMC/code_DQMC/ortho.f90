	SUBROUTINE  ORTHO(A,NCON)

	  Use MyMats

          Implicit Real (KIND=8) (A-G,O-Z)
          Implicit Integer (H-N)

          ! Arguments.
          COMPLEX (KIND=8), DIMENSION(:,:) :: A

          COMPLEX (KIND=8), DIMENSION(:,:), ALLOCATABLE :: AT, U,V
          COMPLEX (KIND=8), DIMENSION(:  ), ALLOCATABLE :: D

          ND1 = SIZE(A,1)
          ND2 = SIZE(A,2)

          !WRITE(6,*) 'In ortho: ', ND1, ND2
          IF (ND1.GT.ND2) THEN 
             ! Ortho of UR
             ALLOCATE(U(ND1,ND2) , D(ND2), V(ND2,ND2) )
             CALL UDV(A,U,D,V,NCON)
             A = U
             DEALLOCATE(U,D,V) 
          ELSE
             ! Ortho of UL
             ALLOCATE(AT(ND2,ND1), U(ND2,ND1) , D(ND1), V(ND1,ND1) )
             DO I = 1,ND1
                DO J = 1,ND2
                   AT(J,I) = A(I,J)
                ENDDO
             ENDDO
             CALL UDV(AT,U,D,V,NCON)
             DO I = 1,ND1
                DO J = 1,ND2
                   A(I,J) = U(J,I)
                ENDDO
             ENDDO
             DEALLOCATE(AT, U, D, V )
          ENDIF
         RETURN
	END SUBROUTINE ORTHO
