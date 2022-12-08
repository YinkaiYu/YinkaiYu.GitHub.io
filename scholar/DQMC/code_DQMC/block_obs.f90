    Module Block_obs

    Use Matrix

    ! For equal time
    COMPLEX  (Kind=8), Dimension(:,:,:), Allocatable, Save  ::  den,spin,spinpm,onswave
    REAL (Kind=8), save :: density,double_ocupy,kinetic
    ! For time displaced
    COMPLEX  (Kind=8), Dimension(:,:,:,:), Allocatable, Save  ::  Green_tau,Green1_tau,spin_tau,spinpm_tau,den_tau,onspair_tau

    Contains
    Subroutine Allocate_obs(LQ,Norb1,LTROT)
        Integer :: LQ, Norb1,Ltrot
        allocate ( den(LQ,norb1,norb1),spin(LQ,norb1,norb1),spinpm(LQ,norb1,norb1),onswave(LQ,norb1,norb1))
    end Subroutine Allocate_obs  

    Subroutine Allocate_obs_tau(LQ,Norb1,LTROT)
        Allocate ( green_tau(LQ,norb1,norb1, LTROT+1),green1_tau(LQ,norb1,norb1,LTROT+1) )
        allocate ( spin_tau(LQ,norb1,norb1, LTROT+1), spinpm_tau(LQ,norb1,norb1, LTROT+1), den_tau(LQ,norb1,norb1, LTROT+1), onspair_tau(LQ,norb1,norb1,LTROT+1) )
    end Subroutine Allocate_obs_tau
        
    Subroutine Init_obs
        den = CMPLX( 0.d0 , 0.d0 )
        spin = CMPLX( 0.d0 , 0.d0 )
        spinpm = CMPLX( 0.d0 , 0.d0 )
        density = 0.d0
        double_ocupy = 0.d0
        kinetic = 0.d0
    End Subroutine Init_obs

    Subroutine Init_obs_tau
        green_tau  = CMPLX(0.d0,0.d0 )
        green1_tau = cmplx(0.d0,0.d0 )
        spin_tau = cmplx(0.d0,0.d0)
        spinpm_tau = cmplx(0.d0,0.d0)
        den_tau = cmplx(0.d0,0.d0)
        onspair_tau = cmplx(0.d0,0.d0)
    End Subroutine Init_obs_tau
      
    end Module Block_obs
    
