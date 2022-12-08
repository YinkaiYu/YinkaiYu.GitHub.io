      SUBROUTINE PRTAU(NOBST)
        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        COMPLEX (KIND =8) :: Znorm
        COMPLEX (KIND=8), Dimension(:,:,:,:), Allocatable :: Collect2
        Character(16) :: filek
        Interface
           Subroutine Fourier_Trans_tau(gr,filek)
             Complex (Kind=8), dimension(:,:,:,:) :: gr
             Character (16) :: filek
           end Subroutine Fourier_Trans_tau
        end Interface
!#define DEC
        INCLUDE 'mpif.h'
        INTEGER STATUS(MPI_STATUS_SIZE)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,ISIZE,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,IRANK,IERR)
        !write(6,*) ' In Prtau : ', NOBST
        ZNORM = CMPLX(1.D0,0.D0) / DCMPLX( DBLE(NOBST), 0.D0 )
        SPIN_tau = ZNORM* SPIN_tau
        SPINPM_tau = ZNORM* SPINPM_tau
        DEN_tau = ZNORM* DEN_tau
        GREEN_tau = ZNORM* GREEN_tau
         Allocate(Collect2(LQ,Norb,Norb,NTDM+1))
         N = LQ*Norb*Norb*(NTDM+1)
         Collect2 = CMPLX(0.D0,0.D0)
         CALL MPI_REDUCE(SPIN_tau,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
              & 0,MPI_COMM_WORLD,IERR)
         SPIN_tau = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         Collect2 = CMPLX(0.D0,0.D0)
         CALL MPI_REDUCE(SPINPM_tau,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
              & 0,MPI_COMM_WORLD,IERR)
         SPINPM_tau = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         Collect2 = CMPLX(0.D0,0.D0)
         CALL MPI_REDUCE(DEN_tau,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
              & 0,MPI_COMM_WORLD,IERR)
         DEN_tau = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         Collect2 = CMPLX(0.D0,0.D0)
         CALL MPI_REDUCE(GREEN_tau,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
              & 0,MPI_COMM_WORLD,IERR)
         GREEN_tau = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         Deallocate(Collect2)
         IF (IRANK.EQ.0) THEN
            filek = "dentau_tot"
            Call Fourier_Trans_tau(den_tau ,filek)
            filek = "spintau_tot"
            Call Fourier_Trans_tau(spin_tau,filek)
            filek = "spinpmtau_tot"
            Call Fourier_Trans_tau(spinpm_tau,filek)
            filek = "gtau_tot"
            Call Fourier_Trans_tau(green_tau,filek)
         ENDIF
       END SUBROUTINE PRTAU
       Subroutine Fourier_Trans_tau(gr,filek)
         Use Blockc
         Use Block_obs
         Implicit Real (KIND=8) (A-G,O-Z)
         Implicit Integer (H-N)
         Complex (Kind=8), dimension(:,:,:,:) :: gr
         Integer :: lp
         Character (16) :: filek
         Real (Kind=8) :: xk_p(2), aimj_p(2)
         Complex (Kind=8), allocatable , dimension(:,:,:,:) :: gk
         allocate (gk(LQ,norb,norb,ntdm+1))
         gk = cmplx(0.d0,0.d0)
         do imj = 1,LQ
            aimj_p = dble(nlist(imj,1))*a1_p + dble(nlist(imj,2))*a2_p
            do no = 1,norb
               do no1 = 1,norb
                  do nt = 1,ntdm + 1
                     do nk = 1,LQ
                        xk_p = dble(nlist(nk,1))*b1_p + dble(nlist(nk,2))*b2_p
                        gk(nk,no,no1,nt) = gk(nk,no,no1,nt) + &
                             & exp( cmplx( 0.d0, xk_p(1)*aimj_p(1)+xk_p(2)*aimj_p(2)) ) * gr(imj,no,no1,nt)
                     enddo
                  enddo
               enddo
            enddo
         enddo
         gk = gk/cmplx(LQ,0.d0)
         OPEN (UNIT=20,FILE=filek,STATUS='UNKNOWN', action="write", position="append")
            do nk = 1,LQ
            kx = nlist(nk,1)
            ky = nlist(nk,2)
            xk_p(1) = dble(kx - 1) * b1_p(1)/dble(NLX) + dble(ky - 1) * b2_p(1)/dble(NLY)
            xk_p(2) = dble(kx - 1) * b1_p(2)/dble(NLX) + dble(ky - 1) * b2_p(2)/dble(NLY)
!! convert FFA convention to correct convention
               write(20,*) xk_p(1), xk_p(2)
               do nt = 1,ntdm+1
                  do no1 = 1,norb
                     do no2 = 1,norb
                        write(20,*) gk(nk,no1,no2,nt)
                     enddo
                  enddo
               enddo
            enddo
         close(20)
         deallocate (gk)
       end Subroutine Fourier_Trans_tau
