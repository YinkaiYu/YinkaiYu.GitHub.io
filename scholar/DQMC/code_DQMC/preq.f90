    SUBROUTINE PREQ(NOBS)
        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        COMPLEX (KIND =8) :: Znorm
        COMPLEX (KIND=8), Dimension(:,:,:), Allocatable :: Collect2
        REAL( kind=8) :: collect4
        Character(16) :: filek,filek1, filek2
        INTEGER :: NOBS
        real(kind=8) :: momx, momy
        Interface
            Subroutine Fourier_Trans(gr,filek)
                Complex (Kind=8), dimension(:,:,:) :: gr
                Character (16) :: filek
            end Subroutine Fourier_Trans
            Subroutine orderparameter(gr1, file)
                real(kind=8) :: gr1
                Character (16) :: file
            end subroutine orderparameter
            Subroutine structurefactor(gr,filek,mom_x,mom_y,no1,no2)
                Complex (Kind=8), dimension(:,:,:) :: gr
                Character (16) :: filek
                real(kind=8) :: mom_x,mom_y
                integer :: no1, no2
            end subroutine structurefactor
            Subroutine realspace(gr,filek)
                Complex (Kind=8), dimension(:,:,:) :: gr
                Character (16) :: filek
            end subroutine realspace
        end Interface
!#define DEC
        INCLUDE 'mpif.h'
        INTEGER STATUS(MPI_STATUS_SIZE)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,ISIZE,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,IRANK,IERR)
        ZNORM = CMPLX(1.D0,0.D0)/ DCMPLX( DBLE(NOBS), 0.D0 )
    
        den = ZNORM * den
        spin = ZNORM * spin
        spinpm = ZNORM * spinpm
        onswave = ZNORM * onswave
        density = znorm * density
        double_ocupy = znorm * double_ocupy

        Allocate(Collect2(LQ,norb1,norb1))
        N = LQ*norb1*norb1
        Collect2 = CMPLX(0.D0,0.D0)
        CALL MPI_REDUCE(den,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
            & 0,MPI_COMM_WORLD,IERR)
        den = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         
        Collect2 = CMPLX(0.D0,0.D0)
        CALL MPI_REDUCE(spin,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
            & 0,MPI_COMM_WORLD,IERR)
        spin = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         
        Collect2 = CMPLX(0.D0,0.D0)
        CALL MPI_REDUCE(spinpm,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
            & 0,MPI_COMM_WORLD,IERR)
        spinpm = Collect2/CMPLX(DBLE(ISIZE),0.D0)
         
        Collect2 = CMPLX(0.D0,0.D0)
        CALL MPI_REDUCE(onswave,Collect2,N,MPI_COMPLEX16,MPI_SUM,&
            & 0,MPI_COMM_WORLD,IERR)
        onswave = Collect2/CMPLX(DBLE(ISIZE),0.D0)   
        
        collect4 = 0.0d0
        CALL MPI_REDUCE(density, Collect4, 1, MPI_REAL8, MPI_SUM, &
            &   0, MPI_COMM_WORLD, IERR)
        density = collect4/CMPLX(DBLE(ISIZE),0.D0)
        
        collect4 = 0.0d0
        CALL MPI_REDUCE(double_ocupy,Collect4,1,MPI_REAL8,MPI_SUM,&
            & 0,MPI_COMM_WORLD,IERR)
        double_ocupy = collect4/CMPLX(DBLE(ISIZE),0.D0)
     
        IF (IRANK.EQ.0) THEN    
            filek = "den_tot"
            Call Fourier_Trans(den,filek)
            filek = "spin_tot"
            Call Fourier_Trans(spin,filek)
            filek = "spinpm_tot"
            Call Fourier_Trans(spinpm,filek)
            filek = "onsite_tot"
            Call Fourier_Trans(onswave,filek)
            filek = "density"
            Call orderparameter(density, filek )
            filek = "double"
            Call orderparameter(double_ocupy,filek)
            
            filek = "AFMzz11"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spin,filek,momx,momy,1,1)
            filek = "AFMzz12"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spin,filek,momx,momy,1,2)
            filek = "AFMzz21"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spin,filek,momx,momy,2,1)
            filek = "AFMzz22"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spin,filek,momx,momy,2,2)
            filek = "AFMzz11deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spin,filek,momx,momy,1,1)
            filek = "AFMzz12deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spin,filek,momx,momy,1,2)
            filek = "AFMzz21deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spin,filek,momx,momy,2,1)
            filek = "AFMzz22deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spin,filek,momx,momy,2,2)
            filek = "AFMpm11"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spinpm,filek,momx,momy,1,1)
            filek = "AFMpm12"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spinpm,filek,momx,momy,1,2)
            filek = "AFMpm21"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spinpm,filek,momx,momy,2,1)
            filek = "AFMpm22"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(spinpm,filek,momx,momy,2,2)
            filek = "AFMpm11deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spinpm,filek,momx,momy,1,1)
            filek = "AFMpm12deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spinpm,filek,momx,momy,1,2)
            filek = "AFMpm21deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spinpm,filek,momx,momy,2,1)
            filek = "AFMpm22deltaq"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(spinpm,filek,momx,momy,2,2)
            filek = "spairm2"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(onswave,filek,momx,momy,1,1)
            filek = "spairm3"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(onswave,filek,momx,momy,1,1)
            filek = "cdw2"
            momx = 0.d0
            momy = 0.d0
            call structurefactor(den,filek,momx,momy,1,1)
            filek = "cdw3"
            momx = 1.0d0/dble(NLx)
            momy = 1.0d0/dble(NLy)
            call structurefactor(den,filek,momx,momy,1,1)
            filek = "afmzzreal"
            Call realspace(spin,filek)
            filek = "afmpmreal"
            Call realspace(spinpm,filek)
            
            

        ENDIF
    END SUBROUTINE PREQ
    
    Subroutine Fourier_Trans(gr,filek)
        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        Complex (Kind=8), dimension(:) :: gr
        Integer :: lp
        Character (16) :: filek
        Real (Kind=8) :: xk_p(2), aimj_p(2)
        Complex (Kind=8), allocatable , dimension(:) :: gk
        allocate (gk(LQ))
        gk = cmplx(0.d0,0.d0)
        do imj = 1,LQ
            lx = list(imj,1)
            ly = list(imj,2)
            aimj_p(1) = dble(lx)* a1_p(1) + dble(ly)* a2_p(1)
            aimj_p(2) = dble(lx)* a1_p(2) + dble(ly)* a2_p(2)
            do nk = 1,LQ
                kx = list(nk,1)
                ky = list(nk,2)
                xk_p(1) = dble(kx - 1) * b1_p(1)/dble(NLX) + dble(ky - 1) * b2_p(1)/dble(NLX)
                xk_p(2) = dble(kx - 1) * b1_p(2)/dble(NLX) + dble(ky - 1) * b2_p(2)/dble(NLY)
                gk(nk) = gk(nk) + &
                    & exp( cmplx( 0.d0, xk_p(1)*aimj_p(1) + xk_p(2)*aimj_p(2) ) ) * gr(imj)                
            enddo
        enddo
        gk = gk/cmplx(LQ,0.d0)
        OPEN (UNIT=20,FILE=filek,STATUS='UNKNOWN', action="write", position="append")
        do nk = 1,LQ
            kx = list(nk,1)
            ky = list(nk,2)
            !  write(20,*)  kx,ky
            xk_p(1) = dble(kx - 1) * b1_p(1)/dble(NLX) + dble(ky - 1) * b2_p(1)/dble(NLY)
            xk_p(2) = dble(kx - 1) * b1_p(2)/dble(NLX) + dble(ky - 1) * b2_p(2)/dble(NLY)
!! to convert the FFA convention to correct convention
            write(20,*) xk_p(1), xk_p(2)
            write(20,*) gk(nk)
        enddo
        close(20)
        deallocate (gk)
    end Subroutine Fourier_Trans
    
    Subroutine orderparameter(gr1, file)
        Use Blockc
        Use Block_obs
        real(kind=8) :: gr1
        Character (16) :: file
        OPEN (UNIT=20,FILE=file,STATUS='UNKNOWN', action="write", position="append")
        write(20,*) gr1      
        close(20)
    end Subroutine orderparameter
    
    Subroutine structurefactor(gr,filek,mom_x,mom_y,no1,no2)
         Use Blockc
         Use Block_obs
         Implicit Real (KIND=8) (A-G,O-Z)
         Implicit Integer (H-N)
         Complex (Kind=8), dimension(:,:,:) :: gr
         Integer :: lp
         Character (16) :: filek
         Real (Kind=8) :: mom_x,mom_y,xk_p(2), aimj_p(2)
         gk = cmplx(0.d0,0.d0)
         xk_p(1) = mom_x * b1_p(1) + mom_y * b2_p(1)
         xk_p(2) = mom_x * b1_p(2) + mom_y * b2_p(2)
         do imj = 1,LQ
             lx = list(imj,1)
             ly = list(imj,2)
            aimj_p(1) = dble(lx)* a1_p(1) + dble(ly)* a2_p(1)
            aimj_p(2) = dble(lx)* a1_p(2) + dble(ly)* a2_p(2)
            gk = gk + exp( cmplx( 0.d0, xk_p(1)*aimj_p(1)+xk_p(2)*aimj_p(2) ) ) * gr(imj,no1,no2)
         enddo
         gk = gk/cmplx(LQ,0.d0)
         OPEN (UNIT=20,FILE=filek,STATUS='UNKNOWN', action="write", position="append")
         write(20,*) real(gk)
         close(20)
    end subroutine structurefactor
    
    
        Subroutine realspace(gr,filek)
         Use Blockc
         Use Block_obs
         Implicit Real (KIND=8) (A-G,O-Z)
         Implicit Integer (H-N)
         Complex (Kind=8), dimension(:,:,:) :: gr
         Character (16) :: filek
    
         OPEN (UNIT=20,FILE=filek,STATUS='UNKNOWN', action="write", position="append")
         do imj = 1,LQ
             lx = list(imj,1)
             ly = list(imj,2)
             write(20,*) lx, ly
             do no1 = 1,norb
                 do no2 = 1, norb
                     write(20,*) gr(imj,no1,no2)
                 enddo
             enddo
         enddo
         close(20)
    end subroutine realspace