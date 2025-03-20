submodule(stdlib_lapack_solve) stdlib_lapack_solve_ldl_comp2
  implicit none


  contains

     pure module subroutine stdlib_ssptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! SSPTRS solves a system of linear equations A*X = B with a real
     !! symmetric matrix A stored in packed format using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by SSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           real(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_sger( k-1, nrhs, -one, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / ap( kc+k-1 ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_sswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_sger( k-2, nrhs, -one, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_sger( k-2, nrhs, -one, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, 1_ilp &
                           ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, ap( kc ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, ap( kc ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb,ap( kc+k ), 1_ilp, one, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sger( n-k, nrhs, -one, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+1,&
                            1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / ap( kc ), b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_sswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_sger( n-k-1, nrhs, -one, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp )&
                              , ldb )
                    call stdlib_sger( n-k-1, nrhs, -one, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / akm1k
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( &
                           kc+1 ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( kc+1 ), &
                              1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( kc-( n-&
                              k ) ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_ssptrs

     pure module subroutine stdlib_dsptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! DSPTRS solves a system of linear equations A*X = B with a real
     !! symmetric matrix A stored in packed format using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by DSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           real(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_dger( k-1, nrhs, -one, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / ap( kc+k-1 ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_dswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_dger( k-2, nrhs, -one, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_dger( k-2, nrhs, -one, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, 1_ilp &
                           ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, ap( kc ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, ap( kc ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb,ap( kc+k ), 1_ilp, one, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dger( n-k, nrhs, -one, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+1,&
                            1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / ap( kc ), b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_dswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_dger( n-k-1, nrhs, -one, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp )&
                              , ldb )
                    call stdlib_dger( n-k-1, nrhs, -one, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / akm1k
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( &
                           kc+1 ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( kc+1 ), &
                              1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, ap( kc-( n-&
                              k ) ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_dsptrs


     pure module subroutine stdlib_csptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! CSPTRS solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A stored in packed format using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by CSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / ap( kc+k-1 ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_cgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_cgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, &
                           1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, ap( kc ),1_ilp, cone, b( k,&
                            1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, ap( kc ),1_ilp, cone, b( k,&
                            1_ilp ), ldb )
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb,ap( kc+k ), 1_ilp, cone, b( &
                           k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / ap( kc ), b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_cgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_cgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / akm1k
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( &
                           kc+1 ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( kc+1 ),&
                               1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( kc-( n-&
                              k ) ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_csptrs

     pure module subroutine stdlib_zsptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! ZSPTRS solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A stored in packed format using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by ZSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / ap( kc+k-1 ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_zgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_zgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, &
                           1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, ap( kc ),1_ilp, cone, b( k,&
                            1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, ap( kc ),1_ilp, cone, b( k,&
                            1_ilp ), ldb )
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb,ap( kc+k ), 1_ilp, cone, b( &
                           k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / ap( kc ), b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_zgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_zgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / akm1k
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( &
                           kc+1 ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( kc+1 ),&
                               1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, ap( kc-( n-&
                              k ) ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_zsptrs




     pure module subroutine stdlib_ssptri( uplo, n, ap, ipiv, work, info )
     !! SSPTRI computes the inverse of a real symmetric indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by SSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==zero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==zero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / ap( kc+k-1 )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, k-1, -one, ap, work, 1_ilp, zero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_sdot( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = ap( kc+k-1 ) / t
                 akp1 = ap( kcnext+k ) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, k-1, -one, ap, work, 1_ilp, zero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_sdot( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_sdot( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_scopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, k-1, -one, ap, work, 1_ilp, zero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -stdlib_sdot( k-1, work, 1_ilp, ap( kcnext ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_sswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = ap( kc+j-1 )
                    ap( kc+j-1 ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / ap( kc )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, n-k, -one, ap( kc+n-k+1 ), work, 1_ilp,zero, ap( kc+1 ), &
                              1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_sdot( n-k, work, 1_ilp, ap( kc+1 ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = ap( kcnext ) / t
                 akp1 = ap( kc ) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, n-k, -one, ap( kc+( n-k+1 ) ), work, 1_ilp,zero, ap( kc+&
                              1_ilp ), 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_sdot( n-k, work, 1_ilp, ap( kc+1 ), 1_ilp )
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_sdot( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+2 &
                              ), 1_ilp )
                    call stdlib_scopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_sspmv( uplo, n-k, -one, ap( kc+( n-k+1 ) ), work, 1_ilp,zero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -stdlib_sdot( n-k, work, 1_ilp, ap( kcnext+2 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_sswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = ap( kc+j-k )
                    ap( kc+j-k ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_ssptri

     pure module subroutine stdlib_dsptri( uplo, n, ap, ipiv, work, info )
     !! DSPTRI computes the inverse of a real symmetric indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by DSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==zero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==zero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / ap( kc+k-1 )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, k-1, -one, ap, work, 1_ilp, zero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_ddot( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = ap( kc+k-1 ) / t
                 akp1 = ap( kcnext+k ) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, k-1, -one, ap, work, 1_ilp, zero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_ddot( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_ddot( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_dcopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, k-1, -one, ap, work, 1_ilp, zero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -stdlib_ddot( k-1, work, 1_ilp, ap( kcnext ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_dswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = ap( kc+j-1 )
                    ap( kc+j-1 ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / ap( kc )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, n-k, -one, ap( kc+n-k+1 ), work, 1_ilp,zero, ap( kc+1 ), &
                              1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_ddot( n-k, work, 1_ilp, ap( kc+1 ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = ap( kcnext ) / t
                 akp1 = ap( kc ) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, n-k, -one, ap( kc+( n-k+1 ) ), work, 1_ilp,zero, ap( kc+&
                              1_ilp ), 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_ddot( n-k, work, 1_ilp, ap( kc+1 ), 1_ilp )
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_ddot( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+2 &
                              ), 1_ilp )
                    call stdlib_dcopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_dspmv( uplo, n-k, -one, ap( kc+( n-k+1 ) ), work, 1_ilp,zero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -stdlib_ddot( n-k, work, 1_ilp, ap( kcnext+2 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_dswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = ap( kc+j-k )
                    ap( kc+j-k ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_dsptri


     pure module subroutine stdlib_csptri( uplo, n, ap, ipiv, work, info )
     !! CSPTRI computes the inverse of a complex symmetric indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by CSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           complex(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = cone / ap( kc+k-1 )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_cdotu( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = ap( kcnext+k-1 )
                 ak = ap( kc+k-1 ) / t
                 akp1 = ap( kcnext+k ) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-cone )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_cdotu( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_cdotu( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_ccopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -stdlib_cdotu( k-1, work, 1_ilp, ap( kcnext ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_cswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = ap( kc+j-1 )
                    ap( kc+j-1 ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = cone / ap( kc )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp,czero, ap( kc+1 )&
                              , 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_cdotu( n-k, work, 1_ilp, ap( kc+1 ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = ap( kcnext+1 )
                 ak = ap( kcnext ) / t
                 akp1 = ap( kc ) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-cone )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work, 1_ilp,czero, ap( &
                              kc+1 ), 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_cdotu( n-k, work, 1_ilp, ap( kc+1 ),1_ilp )
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_cdotu( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+&
                              2_ilp ), 1_ilp )
                    call stdlib_ccopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_cspmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work, 1_ilp,czero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -stdlib_cdotu( n-k, work, 1_ilp, ap( kcnext+2 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_cswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = ap( kc+j-k )
                    ap( kc+j-k ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_csptri

     pure module subroutine stdlib_zsptri( uplo, n, ap, ipiv, work, info )
     !! ZSPTRI computes the inverse of a complex symmetric indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by ZSPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           complex(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = cone / ap( kc+k-1 )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_zdotu( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = ap( kcnext+k-1 )
                 ak = ap( kc+k-1 ) / t
                 akp1 = ap( kcnext+k ) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-cone )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero, ap( kc ),1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -stdlib_zdotu( k-1, work, 1_ilp, ap( kc ), 1_ilp )
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_zdotu( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_zcopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -stdlib_zdotu( k-1, work, 1_ilp, ap( kcnext ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_zswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = ap( kc+j-1 )
                    ap( kc+j-1 ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = cone / ap( kc )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp,czero, ap( kc+1 )&
                              , 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_zdotu( n-k, work, 1_ilp, ap( kc+1 ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = ap( kcnext+1 )
                 ak = ap( kcnext ) / t
                 akp1 = ap( kc ) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-cone )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work, 1_ilp,czero, ap( &
                              kc+1 ), 1_ilp )
                    ap( kc ) = ap( kc ) - stdlib_zdotu( n-k, work, 1_ilp, ap( kc+1 ),1_ilp )
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_zdotu( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+&
                              2_ilp ), 1_ilp )
                    call stdlib_zcopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_zspmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work, 1_ilp,czero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -stdlib_zdotu( n-k, work, 1_ilp, ap( kcnext+2 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_zswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = ap( kc+j-k )
                    ap( kc+j-k ) = ap( kx )
                    ap( kx ) = temp
                 end do
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_zsptri




     pure module subroutine stdlib_ssprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx,ferr, berr, work,&
     !! SSPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
                iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: afp(*), ap(*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_sspmv( uplo, n, -one, ap, x( 1_ilp, j ), 1_ilp, one, work( n+1 ),1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + abs( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_ssptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n, info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_ssptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_ssptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_ssprfs

     pure module subroutine stdlib_dsprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx,ferr, berr, work,&
     !! DSPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
                iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: afp(*), ap(*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dspmv( uplo, n, -one, ap, x( 1_ilp, j ), 1_ilp, one, work( n+1 ),1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + abs( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( ap( ik ) )*xk
                       s = s + abs( ap( ik ) )*abs( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    work( k ) = work( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dsptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n, info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_dsptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dsptrs( uplo, n, 1_ilp, afp, ipiv, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_dsprfs


     pure module subroutine stdlib_csprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx,ferr, berr, work,&
     !! CSPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
                rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: afp(*), ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_cspmv( uplo, n, -cone, ap, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + cabs1( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + cabs1( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_csptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_csptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_csptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_csprfs

     pure module subroutine stdlib_zsprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx,ferr, berr, work,&
     !! ZSPRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite
     !! and packed, and provides error bounds and backward error estimates
     !! for the solution.
                rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: afp(*), ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, ik, j, k, kase, kk, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zspmv( uplo, n, -cone, ap, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              kk = 1_ilp
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    ik = kk
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + cabs1( ap( kk+k-1 ) )*xk + s
                    kk = kk + k
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + cabs1( ap( kk ) )*xk
                    ik = kk + 1_ilp
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( ap( ik ) )*xk
                       s = s + cabs1( ap( ik ) )*cabs1( x( i, j ) )
                       ik = ik + 1_ilp
                    end do
                    rwork( k ) = rwork( k ) + s
                    kk = kk + ( n-k+1 )
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zsptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_zsptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zsptrs( uplo, n, 1_ilp, afp, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zsprfs




     pure module subroutine stdlib_ssycon_rook( uplo, n, a, lda, ipiv, anorm, rcond, work,iwork, info )
     !! SSYCON_ROOK estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by SSYTRF_ROOK.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYCON_ROOK', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_ssytrs_rook( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_ssycon_rook

     pure module subroutine stdlib_dsycon_rook( uplo, n, a, lda, ipiv, anorm, rcond, work,iwork, info )
     !! DSYCON_ROOK estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by DSYTRF_ROOK.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYCON_ROOK', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_dsytrs_rook( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_dsycon_rook


     pure module subroutine stdlib_csycon_rook( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! CSYCON_ROOK estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by CSYTRF_ROOK.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYCON_ROOK', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==czero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_csytrs_rook( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_csycon_rook

     pure module subroutine stdlib_zsycon_rook( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! ZSYCON_ROOK estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by ZSYTRF_ROOK.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYCON_ROOK', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==czero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_zsytrs_rook( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zsycon_rook




     pure module subroutine stdlib_ssytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
     !! SSYTRF_ROOK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.
     !! The form of the factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, n*nb )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRF_ROOK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SSYTRF_ROOK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf_rook;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_slasyf_rook( uplo, k, nb, kb, a, lda,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_ssytf2_rook( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf_rook;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_slasyf_rook( uplo, n-k+1, nb, kb, a( k, k ), lda,ipiv( k ), work, &
                           ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_ssytf2_rook( uplo, n-k+1, a( k, k ), lda, ipiv( k ),iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssytrf_rook

     pure module subroutine stdlib_dsytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
     !! DSYTRF_ROOK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.
     !! The form of the factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, n*nb )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRF_ROOK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DSYTRF_ROOK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf_rook;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_dlasyf_rook( uplo, k, nb, kb, a, lda,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_dsytf2_rook( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf_rook;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_dlasyf_rook( uplo, n-k+1, nb, kb, a( k, k ), lda,ipiv( k ), work, &
                           ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_dsytf2_rook( uplo, n-k+1, a( k, k ), lda, ipiv( k ),iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsytrf_rook


     pure module subroutine stdlib_csytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
     !! CSYTRF_ROOK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.
     !! The form of the factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'CSYTRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, n*nb )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRF_ROOK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CSYTRF_ROOK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf_rook;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_clasyf_rook( uplo, k, nb, kb, a, lda,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_csytf2_rook( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf_rook;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_clasyf_rook( uplo, n-k+1, nb, kb, a( k, k ), lda,ipiv( k ), work, &
                           ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_csytf2_rook( uplo, n-k+1, a( k, k ), lda, ipiv( k ),iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csytrf_rook

     pure module subroutine stdlib_zsytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
     !! ZSYTRF_ROOK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.
     !! The form of the factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'ZSYTRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, n*nb )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRF_ROOK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZSYTRF_ROOK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf_rook;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_zlasyf_rook( uplo, k, nb, kb, a, lda,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_zsytf2_rook( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf_rook;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_zlasyf_rook( uplo, n-k+1, nb, kb, a( k, k ), lda,ipiv( k ), work, &
                           ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_zsytf2_rook( uplo, n-k+1, a( k, k ), lda, ipiv( k ),iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsytrf_rook




     pure module subroutine stdlib_slasyf_rook( uplo, n, nb, kb, a, lda, ipiv, w, ldw,info )
     !! SLASYF_ROOK computes a partial factorization of a real symmetric
     !! matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! SLASYF_ROOK is an auxiliary routine called by SSYTRF_ROOK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, jp1, jp2, k, kk, kw, kkw, kp, kstep, p, &
                     ii
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, stemp, r1, rowmax, t, &
                     sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_scopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_scopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_scopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_scopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one,a( 1_ilp, k+1 ), lda, &
                                 w( imax, kw+1 ), ldw,one, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_isamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = abs( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_isamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          stemp = abs( w( itemp, kw-1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(abs( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_scopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_scopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_scopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_scopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_sswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_sswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_scopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_scopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_sswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_sswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_scopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_sscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = one / ( d11*d22-one )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -one, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ), ldw,one, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n
              j = k + 1_ilp
              60 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j + 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j + 1_ilp
                 if( jp2/=jj .and. j<=n )call stdlib_sswap( n-j+1, a( jp2, j ), lda, a( jj, j ), &
                           lda )
                 jj = j - 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_sswap( n-j+1, a( jp1, j ), lda, a( jj, j &
                           ), lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_scopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, one, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_scopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_scopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_scopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,one, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_isamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = abs( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_isamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          stemp = abs( w( itemp, k+1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( abs( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_scopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_scopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_scopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_scopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_sswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_sswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_scopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_scopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_sswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_sswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_scopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_sscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,one, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! in columns 1:k-1
              j = k - 1_ilp
              120 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j - 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j - 1_ilp
                 if( jp2/=jj .and. j>=1_ilp )call stdlib_sswap( j, a( jp2, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
                 jj = j + 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_sswap( j, a( jp1, 1_ilp ), lda, a( jj, 1_ilp ), &
                           lda )
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_slasyf_rook

     pure module subroutine stdlib_dlasyf_rook( uplo, n, nb, kb, a, lda, ipiv, w, ldw,info )
     !! DLASYF_ROOK computes a partial factorization of a real symmetric
     !! matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! DLASYF_ROOK is an auxiliary routine called by DSYTRF_ROOK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, jp1, jp2, k, kk, kw, kkw, kp, kstep, p, &
                     ii
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, dtemp, r1, rowmax, t, &
                     sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_dcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_dcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_dcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_dcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one,a( 1_ilp, k+1 ), lda, &
                                 w( imax, kw+1 ), ldw,one, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_idamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = abs( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_idamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          dtemp = abs( w( itemp, kw-1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(abs( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_dcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_dcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_dcopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_dcopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_dswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_dswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_dcopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_dcopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_dswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_dswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_dcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_dscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = one / ( d11*d22-one )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -one, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ), ldw,one, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n
              j = k + 1_ilp
              60 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j + 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j + 1_ilp
                 if( jp2/=jj .and. j<=n )call stdlib_dswap( n-j+1, a( jp2, j ), lda, a( jj, j ), &
                           lda )
                 jj = j - 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_dswap( n-j+1, a( jp1, j ), lda, a( jj, j &
                           ), lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_dcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, one, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_dcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_dcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_dcopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,one, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_idamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = abs( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_idamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          dtemp = abs( w( itemp, k+1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( abs( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_dcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_dcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_dcopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_dcopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_dswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_dswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_dcopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_dcopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_dswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_dswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_dcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_dscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,one, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! in columns 1:k-1
              j = k - 1_ilp
              120 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j - 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j - 1_ilp
                 if( jp2/=jj .and. j>=1_ilp )call stdlib_dswap( j, a( jp2, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
                 jj = j + 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_dswap( j, a( jp1, 1_ilp ), lda, a( jj, 1_ilp ), &
                           lda )
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_dlasyf_rook


     pure module subroutine stdlib_clasyf_rook( uplo, n, nb, kb, a, lda, ipiv, w, ldw,info )
     !! CLASYF_ROOK computes a partial factorization of a complex symmetric
     !! matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! CLASYF_ROOK is an auxiliary routine called by CSYTRF_ROOK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, jp1, jp2, k, kk, kw, kkw, kp, kstep, p, &
                     ii
           real(sp) :: absakk, alpha, colmax, rowmax, stemp, sfmin
           complex(sp) :: d11, d12, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_ccopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ),lda, w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_ccopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_ccopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda,&
                                  w( imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_icamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = cabs1( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_icamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          stemp = cabs1( w( itemp, kw-1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(cabs1( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_ccopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_ccopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_cswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_cswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_ccopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_ccopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_cswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_cswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_cscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -cone, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n
              j = k + 1_ilp
              60 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j + 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j + 1_ilp
                 if( jp2/=jj .and. j<=n )call stdlib_cswap( n-j+1, a( jp2, j ), lda, a( jj, j ), &
                           lda )
                 jj = j - 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_cswap( n-j+1, a( jp1, j ), lda, a( jj, j &
                           ), lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_ccopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, cone, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_ccopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_ccopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,cone, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_icamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = cabs1( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_icamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          stemp = cabs1( w( itemp, k+1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( cabs1( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_ccopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_ccopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_cswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_ccopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_ccopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_cswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_cscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! in columns 1:k-1
              j = k - 1_ilp
              120 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j - 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j - 1_ilp
                 if( jp2/=jj .and. j>=1_ilp )call stdlib_cswap( j, a( jp2, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
                 jj = j + 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_cswap( j, a( jp1, 1_ilp ), lda, a( jj, 1_ilp ), &
                           lda )
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_clasyf_rook

     pure module subroutine stdlib_zlasyf_rook( uplo, n, nb, kb, a, lda, ipiv, w, ldw,info )
     !! ZLASYF_ROOK computes a partial factorization of a complex symmetric
     !! matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! ZLASYF_ROOK is an auxiliary routine called by ZSYTRF_ROOK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, jp1, jp2, k, kk, kw, kkw, kp, kstep, p, &
                     ii
           real(dp) :: absakk, alpha, colmax, rowmax, dtemp, sfmin
           complex(dp) :: d11, d12, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_zcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ),lda, w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_zcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_zcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda,&
                                  w( imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_izamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = cabs1( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_izamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          dtemp = cabs1( w( itemp, kw-1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(cabs1( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_zcopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_zcopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_zswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_zswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_zcopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_zcopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_zswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_zswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_zscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -cone, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n
              j = k + 1_ilp
              60 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j + 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j + 1_ilp
                 if( jp2/=jj .and. j<=n )call stdlib_zswap( n-j+1, a( jp2, j ), lda, a( jj, j ), &
                           lda )
                 jj = j - 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_zswap( n-j+1, a( jp1, j ), lda, a( jj, j &
                           ), lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_zcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, cone, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_zcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_zcopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,cone, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_izamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = cabs1( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_izamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          dtemp = cabs1( w( itemp, k+1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( cabs1( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_zcopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_zcopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_zswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_zcopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_zcopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_zswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_zscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! in columns 1:k-1
              j = k - 1_ilp
              120 continue
                 kstep = 1_ilp
                 jp1 = 1_ilp
                 jj = j
                 jp2 = ipiv( j )
                 if( jp2<0_ilp ) then
                    jp2 = -jp2
                    j = j - 1_ilp
                    jp1 = -ipiv( j )
                    kstep = 2_ilp
                 end if
                 j = j - 1_ilp
                 if( jp2/=jj .and. j>=1_ilp )call stdlib_zswap( j, a( jp2, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
                 jj = j + 1_ilp
                 if( jp1/=jj .and. kstep==2_ilp )call stdlib_zswap( j, a( jp1, 1_ilp ), lda, a( jj, 1_ilp ), &
                           lda )
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_zlasyf_rook




     pure module subroutine stdlib_ssytf2_rook( uplo, n, a, lda, ipiv, info )
     !! SSYTF2_ROOK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, rowmax, stemp, t, wk, wkm1, &
                     wkp1, sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTF2_ROOK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_isamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_isamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          stemp = abs( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_sswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_sswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_sswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_sswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_ssyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_sscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_ssyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_isamax( imax-k, a( imax, k ), lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_isamax( n-imax, a( imax+1, imax ),1_ilp )
                          stemp = abs( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_sswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_sswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_sswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_ssyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_sscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_ssyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_ssytf2_rook

     pure module subroutine stdlib_dsytf2_rook( uplo, n, a, lda, ipiv, info )
     !! DSYTF2_ROOK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, rowmax, dtemp, t, wk, wkm1, &
                     wkp1, sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTF2_ROOK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_idamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_idamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          dtemp = abs( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_dswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_dswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_dswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_dswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_dsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_dscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_dsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_idamax( imax-k, a( imax, k ), lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_idamax( n-imax, a( imax+1, imax ),1_ilp )
                          dtemp = abs( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_dswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_dswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_dswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_dsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_dscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_dsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_dsytf2_rook


     pure module subroutine stdlib_csytf2_rook( uplo, n, a, lda, ipiv, info )
     !! CSYTF2_ROOK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, rowmax, stemp, sfmin
           complex(sp) :: d11, d12, d21, d22, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTF2_ROOK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_icamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_icamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          stemp = cabs1( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! cabs1( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1(a( imax, imax ))<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_cswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_cswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_cswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_cswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_csyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_cscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_csyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_icamax( imax-k, a( imax, k ), lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_icamax( n-imax, a( imax+1, imax ),1_ilp )
                          stemp = cabs1( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! cabs1( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1(a( imax, imax ))<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_cswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_cswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_cswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_csyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_cscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_csyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_csytf2_rook

     pure module subroutine stdlib_zsytf2_rook( uplo, n, a, lda, ipiv, info )
     !! ZSYTF2_ROOK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman ("rook") diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, rowmax, dtemp, sfmin
           complex(dp) :: d11, d12, d21, d22, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTF2_ROOK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_izamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_izamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          dtemp = cabs1( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! cabs1( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1(a( imax, imax ))<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_zswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_zswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_zswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_zswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_zsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_zscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_zsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_izamax( imax-k, a( imax, k ), lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_izamax( n-imax, a( imax+1, imax ),1_ilp )
                          dtemp = cabs1( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! cabs1( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1(a( imax, imax ))<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_zswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_zswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_zswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_zsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_zscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_zsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_zsytf2_rook




     pure module subroutine stdlib_ssytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb,info )
     !! SSYTRS_ROOK solves a system of linear equations A*X = B with
     !! a real symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by SSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRS_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_sger( k-1, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_sswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k>2_ilp ) then
                    call stdlib_sger( k-2, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                              ldb )
                    call stdlib_sger( k-2, nrhs, -one, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ),&
                               ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp )call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k ), 1_ilp, &
                           one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k ), 1_ilp, one, b( &
                              k, 1_ilp ), ldb )
                    call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k+1 ), 1_ilp, one, &
                              b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_sswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sger( n-k, nrhs, -one, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_sswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_sger( n-k-1, nrhs, -one, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp &
                              ), ldb )
                    call stdlib_sger( n-k-1, nrhs, -one, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k ),&
                               1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k-1 &
                              ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_sswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_ssytrs_rook

     pure module subroutine stdlib_dsytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb,info )
     !! DSYTRS_ROOK solves a system of linear equations A*X = B with
     !! a real symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by DSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRS_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_dger( k-1, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_dswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k>2_ilp ) then
                    call stdlib_dger( k-2, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                              ldb )
                    call stdlib_dger( k-2, nrhs, -one, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ),&
                               ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp )call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k ), 1_ilp, &
                           one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k ), 1_ilp, one, b( &
                              k, 1_ilp ), ldb )
                    call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b,ldb, a( 1_ilp, k+1 ), 1_ilp, one, &
                              b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_dswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dger( n-k, nrhs, -one, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_dswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_dger( n-k-1, nrhs, -one, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp &
                              ), ldb )
                    call stdlib_dger( n-k-1, nrhs, -one, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k ),&
                               1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k-1 &
                              ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_dswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_dsytrs_rook


     pure module subroutine stdlib_csytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb,info )
     !! CSYTRS_ROOK solves a system of linear equations A*X = B with
     !! a complex symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by CSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRS_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k>2_ilp ) then
                    call stdlib_cgeru( k-2, nrhs,-cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                              ldb )
                    call stdlib_cgeru( k-2, nrhs,-cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp )&
                              , ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp )call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), 1_ilp, &
                           cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), 1_ilp, cone, &
                              b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k+1 ), 1_ilp, cone,&
                               b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_cgeru( n-k-1, nrhs,-cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_cgeru( n-k-1, nrhs,-cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k )&
                              , 1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k-&
                              1_ilp ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_csytrs_rook

     pure module subroutine stdlib_zsytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb,info )
     !! ZSYTRS_ROOK solves a system of linear equations A*X = B with
     !! a complex symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by ZSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRS_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k>2_ilp ) then
                    call stdlib_zgeru( k-2, nrhs,-cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                              ldb )
                    call stdlib_zgeru( k-2, nrhs,-cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp )&
                              , ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp )call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), 1_ilp, &
                           cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), 1_ilp, cone, &
                              b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k+1 ), 1_ilp, cone,&
                               b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k) then k+1 and -ipiv(k+1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k+1 )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_zgeru( n-k-1, nrhs,-cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_zgeru( n-k-1, nrhs,-cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k )&
                              , 1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k-&
                              1_ilp ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k) then k-1 and -ipiv(k-1)
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kp = -ipiv( k-1 )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_zsytrs_rook




     pure module subroutine stdlib_ssytri_rook( uplo, n, a, lda, ipiv, work, info )
     !! SSYTRI_ROOK computes the inverse of a real symmetric
     !! matrix A using the factorization A = U*D*U**T or A = L*D*L**T
     !! computed by SSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           real(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRI_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_sdot( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_scopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_sswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_sswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k+1 with -ipiv(k) and
                 ! -ipiv(k+1)in the leading submatrix a(1:k+1,1:k+1)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_sswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_sswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
                 k = k + 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_sswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_sswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k + 1_ilp
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_sdot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_sdot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_sdot( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp )
                              
                    call stdlib_scopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_sdot( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_sswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k-1 with -ipiv(k) and
                 ! -ipiv(k-1) in the trailing submatrix a(k-1:n,k-1:n)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_sswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
                 k = k - 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_sswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k - 1_ilp
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_ssytri_rook

     pure module subroutine stdlib_dsytri_rook( uplo, n, a, lda, ipiv, work, info )
     !! DSYTRI_ROOK computes the inverse of a real symmetric
     !! matrix A using the factorization A = U*D*U**T or A = L*D*L**T
     !! computed by DSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           real(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRI_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_ddot( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_dcopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_dswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_dswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k+1 with -ipiv(k) and
                 ! -ipiv(k+1)in the leading submatrix a(1:k+1,1:k+1)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_dswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_dswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
                 k = k + 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_dswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_dswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k + 1_ilp
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_ddot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_ddot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_ddot( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp )
                              
                    call stdlib_dcopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_ddot( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_dswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k-1 with -ipiv(k) and
                 ! -ipiv(k-1) in the trailing submatrix a(k-1:n,k-1:n)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_dswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
                 k = k - 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_dswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k - 1_ilp
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_dsytri_rook


     pure module subroutine stdlib_csytri_rook( uplo, n, a, lda, ipiv, work, info )
     !! CSYTRI_ROOK computes the inverse of a complex symmetric
     !! matrix A using the factorization A = U*D*U**T or A = L*D*L**T
     !! computed by CSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           complex(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRI_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k+1 )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_cdotu( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_ccopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_cswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_cswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k+1 with -ipiv(k) and
                 ! -ipiv(k+1)in the leading submatrix a(1:k+1,1:k+1)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_cswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_cswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
                 k = k + 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_cswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_cswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k + 1_ilp
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k-1 )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_cdotu( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_ccopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_cswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k-1 with -ipiv(k) and
                 ! -ipiv(k-1) in the trailing submatrix a(k-1:n,k-1:n)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_cswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
                 k = k - 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_cswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k - 1_ilp
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_csytri_rook

     pure module subroutine stdlib_zsytri_rook( uplo, n, a, lda, ipiv, work, info )
     !! ZSYTRI_ROOK computes the inverse of a complex symmetric
     !! matrix A using the factorization A = U*D*U**T or A = L*D*L**T
     !! computed by ZSYTRF_ROOK.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           complex(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRI_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k+1 )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_zdotu( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_zcopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_zswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_zswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k+1 with -ipiv(k) and
                 ! -ipiv(k+1)in the leading submatrix a(1:k+1,1:k+1)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_zswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_zswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
                 k = k + 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp>1_ilp )call stdlib_zswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_zswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k + 1_ilp
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k-1 )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_zdotu( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_zcopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k,-cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+1,&
                               k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              if( kstep==1_ilp ) then
                 ! interchange rows and columns k and ipiv(k) in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kp = ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_zswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              else
                 ! interchange rows and columns k and k-1 with -ipiv(k) and
                 ! -ipiv(k-1) in the trailing submatrix a(k-1:n,k-1:n)
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_zswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
                 k = k - 1_ilp
                 kp = -ipiv( k )
                 if( kp/=k ) then
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                    call stdlib_zswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                    temp = a( k, k )
                    a( k, k ) = a( kp, kp )
                    a( kp, kp ) = temp
                 end if
              end if
              k = k - 1_ilp
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_zsytri_rook




     pure module subroutine stdlib_ssytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork,info )
     !! SSYTRF_RK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, ip, iws, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRF_RK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRF_RK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SSYTRF_RK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf_rk;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 15
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_slasyf_rk( uplo, k, nb, kb, a, lda, e,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_ssytf2_rk( uplo, k, a, lda, e, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k-kb+1:k and apply row permutations to the
              ! last k+1 colunms k+1:n after that block
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k<n ) then
                 do i = k, ( k - kb + 1 ), -1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_sswap( n-k, a( i, k+1 ), lda,a( ip, k+1 ), lda )
                    end if
                 end do
              end if
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
              ! this label is the exit from main loop over k decreasing
              ! from n to 1 in steps of kb
              15 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf_rk;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 35
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_slasyf_rk( uplo, n-k+1, nb, kb, a( k, k ), lda, e( k ),ipiv( k ), &
                           work, ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_ssytf2_rk( uplo, n-k+1, a( k, k ), lda, e( k ),ipiv( k ), iinfo )
                           
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do i = k, k + kb - 1
                 if( ipiv( i )>0_ilp ) then
                    ipiv( i ) = ipiv( i ) + k - 1_ilp
                 else
                    ipiv( i ) = ipiv( i ) - k + 1_ilp
                 end if
              end do
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k:k+kb-1 and apply row permutations to the
              ! first k-1 colunms 1:k-1 before that block
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k>1_ilp ) then
                 do i = k, ( k + kb - 1 ), 1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_sswap( k-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                    end if
                 end do
              end if
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
              ! this label is the exit from main loop over k increasing
              ! from 1 to n in steps of kb
              35 continue
           ! end lower
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssytrf_rk

     pure module subroutine stdlib_dsytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork,info )
     !! DSYTRF_RK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, ip, iws, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRF_RK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRF_RK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DSYTRF_RK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf_rk;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 15
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_dlasyf_rk( uplo, k, nb, kb, a, lda, e,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_dsytf2_rk( uplo, k, a, lda, e, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k-kb+1:k and apply row permutations to the
              ! last k+1 colunms k+1:n after that block
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k<n ) then
                 do i = k, ( k - kb + 1 ), -1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_dswap( n-k, a( i, k+1 ), lda,a( ip, k+1 ), lda )
                    end if
                 end do
              end if
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
              ! this label is the exit from main loop over k decreasing
              ! from n to 1 in steps of kb
              15 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf_rk;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 35
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_dlasyf_rk( uplo, n-k+1, nb, kb, a( k, k ), lda, e( k ),ipiv( k ), &
                           work, ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_dsytf2_rk( uplo, n-k+1, a( k, k ), lda, e( k ),ipiv( k ), iinfo )
                           
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do i = k, k + kb - 1
                 if( ipiv( i )>0_ilp ) then
                    ipiv( i ) = ipiv( i ) + k - 1_ilp
                 else
                    ipiv( i ) = ipiv( i ) - k + 1_ilp
                 end if
              end do
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k:k+kb-1 and apply row permutations to the
              ! first k-1 colunms 1:k-1 before that block
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k>1_ilp ) then
                 do i = k, ( k + kb - 1 ), 1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_dswap( k-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                    end if
                 end do
              end if
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
              ! this label is the exit from main loop over k increasing
              ! from 1 to n in steps of kb
              35 continue
           ! end lower
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsytrf_rk


     pure module subroutine stdlib_csytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork,info )
     !! CSYTRF_RK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, ip, iws, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'CSYTRF_RK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRF_RK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CSYTRF_RK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf_rk;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 15
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_clasyf_rk( uplo, k, nb, kb, a, lda, e,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_csytf2_rk( uplo, k, a, lda, e, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k-kb+1:k and apply row permutations to the
              ! last k+1 colunms k+1:n after that block
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k<n ) then
                 do i = k, ( k - kb + 1 ), -1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_cswap( n-k, a( i, k+1 ), lda,a( ip, k+1 ), lda )
                    end if
                 end do
              end if
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
              ! this label is the exit from main loop over k decreasing
              ! from n to 1 in steps of kb
              15 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf_rk;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 35
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_clasyf_rk( uplo, n-k+1, nb, kb, a( k, k ), lda, e( k ),ipiv( k ), &
                           work, ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_csytf2_rk( uplo, n-k+1, a( k, k ), lda, e( k ),ipiv( k ), iinfo )
                           
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do i = k, k + kb - 1
                 if( ipiv( i )>0_ilp ) then
                    ipiv( i ) = ipiv( i ) + k - 1_ilp
                 else
                    ipiv( i ) = ipiv( i ) - k + 1_ilp
                 end if
              end do
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k:k+kb-1 and apply row permutations to the
              ! first k-1 colunms 1:k-1 before that block
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k>1_ilp ) then
                 do i = k, ( k + kb - 1 ), 1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_cswap( k-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                    end if
                 end do
              end if
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
              ! this label is the exit from main loop over k increasing
              ! from 1 to n in steps of kb
              35 continue
           ! end lower
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csytrf_rk

     pure module subroutine stdlib_zsytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork,info )
     !! ZSYTRF_RK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, ip, iws, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'ZSYTRF_RK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRF_RK', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZSYTRF_RK',uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf_rk;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 15
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_zlasyf_rk( uplo, k, nb, kb, a, lda, e,ipiv, work, ldwork, iinfo )
                           
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_zsytf2_rk( uplo, k, a, lda, e, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! no need to adjust ipiv
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k-kb+1:k and apply row permutations to the
              ! last k+1 colunms k+1:n after that block
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k<n ) then
                 do i = k, ( k - kb + 1 ), -1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_zswap( n-k, a( i, k+1 ), lda,a( ip, k+1 ), lda )
                    end if
                 end do
              end if
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
              ! this label is the exit from main loop over k decreasing
              ! from n to 1 in steps of kb
              15 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf_rk;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 35
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_zlasyf_rk( uplo, n-k+1, nb, kb, a( k, k ), lda, e( k ),ipiv( k ), &
                           work, ldwork, iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_zsytf2_rk( uplo, n-k+1, a( k, k ), lda, e( k ),ipiv( k ), iinfo )
                           
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do i = k, k + kb - 1
                 if( ipiv( i )>0_ilp ) then
                    ipiv( i ) = ipiv( i ) + k - 1_ilp
                 else
                    ipiv( i ) = ipiv( i ) - k + 1_ilp
                 end if
              end do
              ! apply permutations to the leading panel 1:k-1
              ! read ipiv from the last block factored, i.e.
              ! indices  k:k+kb-1 and apply row permutations to the
              ! first k-1 colunms 1:k-1 before that block
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              if( k>1_ilp ) then
                 do i = k, ( k + kb - 1 ), 1
                    ip = abs( ipiv( i ) )
                    if( ip/=i ) then
                       call stdlib_zswap( k-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                    end if
                 end do
              end if
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
              ! this label is the exit from main loop over k increasing
              ! from 1 to n in steps of kb
              35 continue
           ! end lower
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsytrf_rk




     pure module subroutine stdlib_slasyf_rk( uplo, n, nb, kb, a, lda, e, ipiv, w, ldw,info )
     !! SLASYF_RK computes a partial factorization of a real symmetric
     !! matrix A using the bounded Bunch-Kaufman (rook) diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L',
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! SLASYF_RK is an auxiliary routine called by SSYTRF_RK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*), w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, k, kk, kw, kkw, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, stemp, r1, rowmax, t, &
                     sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = zero
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_scopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_scopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = zero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_scopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_scopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one,a( 1_ilp, k+1 ), lda, &
                                 w( imax, kw+1 ), ldw,one, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_isamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = abs( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_isamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          stemp = abs( w( itemp, kw-1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(abs( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_scopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_scopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_scopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_scopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_sswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_sswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_scopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_scopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_sswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_sswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_scopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_sscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = one / ( d11*d22-one )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy superdiagonal element of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = zero
                    a( k, k ) = w( k, kw )
                    e( k ) = w( k-1, kw )
                    e( k-1 ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -one, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ),ldw, one, a( 1_ilp, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = zero
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_scopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, one, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_scopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k<n )e( k ) = zero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_scopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_scopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,one, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_isamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = abs( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_isamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          stemp = abs( w( itemp, k+1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( abs( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_scopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_scopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_scopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_scopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_sswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_sswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_scopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_scopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_sswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_sswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_scopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_sscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy subdiagonal element of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = zero
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    e( k ) = w( k+1, k )
                    e( k+1 ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, one, a( j+jb, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_slasyf_rk

     pure module subroutine stdlib_dlasyf_rk( uplo, n, nb, kb, a, lda, e, ipiv, w, ldw,info )
     !! DLASYF_RK computes a partial factorization of a real symmetric
     !! matrix A using the bounded Bunch-Kaufman (rook) diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L',
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! DLASYF_RK is an auxiliary routine called by DSYTRF_RK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*), w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, k, kk, kw, kkw, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, dtemp, r1, rowmax, t, &
                     sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = zero
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_dcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_dcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = zero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_dcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_dcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one,a( 1_ilp, k+1 ), lda, &
                                 w( imax, kw+1 ), ldw,one, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_idamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = abs( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_idamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          dtemp = abs( w( itemp, kw-1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(abs( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_dcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_dcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_dcopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_dcopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_dswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_dswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_dcopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_dcopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_dswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_dswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_dcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_dscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = one / ( d11*d22-one )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy superdiagonal element of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = zero
                    a( k, k ) = w( k, kw )
                    e( k ) = w( k-1, kw )
                    e( k-1 ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -one, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ),ldw, one, a( 1_ilp, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = zero
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_dcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, one, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_dcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k<n )e( k ) = zero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_dcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_dcopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,one, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_idamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = abs( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_idamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          dtemp = abs( w( itemp, k+1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! abs( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( abs( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_dcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_dcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_dcopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_dcopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_dswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_dswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_dcopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_dcopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_dswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_dswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_dcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( abs( a( k, k ) )>=sfmin ) then
                          r1 = one / a( k, k )
                          call stdlib_dscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=zero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy subdiagonal element of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = zero
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    e( k ) = w( k+1, k )
                    e( k+1 ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, one, a( j+jb, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_dlasyf_rk


     pure module subroutine stdlib_clasyf_rk( uplo, n, nb, kb, a, lda, e, ipiv, w, ldw,info )
     !! CLASYF_RK computes a partial factorization of a complex symmetric
     !! matrix A using the bounded Bunch-Kaufman (rook) diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L',
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! CLASYF_RK is an auxiliary routine called by CSYTRF_RK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: e(*), w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, k, kk, kw, kkw, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, rowmax, sfmin, stemp
           complex(sp) :: d11, d12, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = czero
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_ccopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ),lda, w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = czero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_ccopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_ccopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda,&
                                  w( imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_icamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = cabs1( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_icamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          stemp = cabs1( w( itemp, kw-1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(cabs1( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_ccopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_ccopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_cswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_cswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_ccopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_ccopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_cswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_cswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_cscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy superdiagonal element of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = czero
                    a( k, k ) = w( k, kw )
                    e( k ) = w( k-1, kw )
                    e( k-1 ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -cone, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ),ldw, cone, a( 1_ilp, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = czero
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_ccopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, cone, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k<n )e( k ) = czero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_ccopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_ccopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,cone, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_icamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = cabs1( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_icamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          stemp = cabs1( w( itemp, k+1 ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( cabs1( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_ccopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_ccopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_cswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_ccopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_ccopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_cswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_cscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy subdiagonal element of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = czero
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    e( k ) = w( k+1, k )
                    e( k+1 ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_clasyf_rk

     pure module subroutine stdlib_zlasyf_rk( uplo, n, nb, kb, a, lda, e, ipiv, w, ldw,info )
     !! ZLASYF_RK computes a partial factorization of a complex symmetric
     !! matrix A using the bounded Bunch-Kaufman (rook) diagonal
     !! pivoting method. The partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L',
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! ZLASYF_RK is an auxiliary routine called by ZSYTRF_RK. It uses
     !! blocked code (calling Level 3 BLAS) to update the submatrix
     !! A11 (if UPLO = 'U') or A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: e(*), w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: imax, itemp, j, jb, jj, jmax, k, kk, kw, kkw, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, rowmax, sfmin, dtemp
           complex(dp) :: d11, d12, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = czero
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              p = k
              ! copy column k of a to column kw of w and update it
              call stdlib_zcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ),lda, w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = czero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! copy column imax to column kw-1 of w and update it
                       call stdlib_zcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                       call stdlib_zcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                                 
                       if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda,&
                                  w( imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_izamax( k-imax, w( imax+1, kw-1 ),1_ilp )
                          rowmax = cabs1( w( jmax, kw-1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_izamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                          dtemp = cabs1( w( itemp, kw-1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, kw-1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.(cabs1( w( imax, kw-1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column kw-1 of w to column kw of w
                          call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k-1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_zcopy( k-p, a( p+1, k ), 1_ilp, a( p, p+1 ), lda )
                    call stdlib_zcopy( p, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    ! interchange rows k and p in last n-k+1 columns of a
                    ! and last n-k+2 columns of w
                    call stdlib_zswap( n-k+1, a( k, k ), lda, a( p, k ), lda )
                    call stdlib_zswap( n-kk+1, w( k, kkw ), ldw, w( p, kkw ), ldw )
                 end if
                 ! updated column kp is already stored in column kkw of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_zcopy( k-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_zcopy( kp, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last n-kk+1 columns
                    ! of a and w
                    call stdlib_zswap( n-kk+1, a( kk, kk ), lda, a( kp, kk ), lda )
                    call stdlib_zswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! store u(k) in column k of a
                    call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_zscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now
                    ! hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    if( k>2_ilp ) then
                       ! store u(k) and u(k-1) in columns k and k-1 of a
                       d12 = w( k-1, kw )
                       d11 = w( k, kw ) / d12
                       d22 = w( k-1, kw-1 ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = 1, k - 2
                          a( j, k-1 ) = t*( (d11*w( j, kw-1 )-w( j, kw ) ) /d12 )
                          a( j, k ) = t*( ( d22*w( j, kw )-w( j, kw-1 ) ) /d12 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy superdiagonal element of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = czero
                    a( k, k ) = w( k, kw )
                    e( k ) = w( k-1, kw )
                    e( k-1 ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 if( j>=2_ilp )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb,n-k, -cone, a( &
                           1_ilp, k+1 ), lda, w( j, kw+1 ),ldw, cone, a( 1_ilp, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = czero
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              p = k
              ! copy column k of a to column k of w and update it
              call stdlib_zcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              if( k>1_ilp )call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( k, &
                        1_ilp ), ldw, cone, w( k, k ), 1_ilp )
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                 ! set e( k ) to zero
                 if( k<n )e( k ) = czero
              else
                 ! ============================================================
                 ! test for interchange
                 ! equivalent to testing for absakk>=alpha*colmax
                 ! (used to handle nan and inf)
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    72 continue
                       ! begin pivot search loop body
                       ! copy column imax to column k+1 of w and update it
                       call stdlib_zcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp)
                       call stdlib_zcopy( n-imax+1, a( imax, imax ), 1_ilp,w( imax, k+1 ), 1_ilp )
                       if( k>1_ilp )call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone,a( k, 1_ilp ), &
                                 lda, w( imax, 1_ilp ), ldw,cone, w( k, k+1 ), 1_ilp )
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_izamax( imax-k, w( k, k+1 ), 1_ilp )
                          rowmax = cabs1( w( jmax, k+1 ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_izamax( n-imax, w( imax+1, k+1 ), 1_ilp)
                          dtemp = cabs1( w( itemp, k+1 ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for
                       ! cabs1( w( imax, k+1 ) )>=alpha*rowmax
                       ! (used to handle nan and inf)
                       if( .not.( cabs1( w( imax, k+1 ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          ! copy column k+1 of w to column k of w
                          call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                          done = .true.
                       ! equivalent to testing for rowmax==colmax,
                       ! (used to handle nan and inf)
                       else if( ( p==jmax ) .or. ( rowmax<=colmax ) )then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found: set params and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                          ! copy updated jmaxth (next imaxth) column to kth of w
                          call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 72
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! copy non-updated column k to column p
                    call stdlib_zcopy( p-k, a( k, k ), 1_ilp, a( p, k ), lda )
                    call stdlib_zcopy( n-p+1, a( p, k ), 1_ilp, a( p, p ), 1_ilp )
                    ! interchange rows k and p in first k columns of a
                    ! and first k+1 columns of w
                    call stdlib_zswap( k, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( k, 1_ilp ), ldw, w( p, 1_ilp ), ldw )
                 end if
                 ! updated column kp is already stored in column kk of w
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp
                    a( kp, k ) = a( kk, k )
                    call stdlib_zcopy( kp-k-1, a( k+1, kk ), 1_ilp, a( kp, k+1 ), lda )
                    call stdlib_zcopy( n-kp+1, a( kp, kk ), 1_ilp, a( kp, kp ), 1_ilp )
                    ! interchange rows kk and kp in first kk columns of a and w
                    call stdlib_zswap( kk, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    ! store l(k) in column k of a
                    call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          r1 = cone / a( k, k )
                          call stdlib_zscal( n-k, r1, a( k+1, k ), 1_ilp )
                       else if( a( k, k )/=czero ) then
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / a( k, k )
                          end do
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! store l(k) and l(k+1) in columns k and k+1 of a
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          a( j, k ) = t*( ( d11*w( j, k )-w( j, k+1 ) ) /d21 )
                          a( j, k+1 ) = t*( ( d22*w( j, k+1 )-w( j, k ) ) /d21 )
                       end do
                    end if
                    ! copy diagonal elements of d(k) to a,
                    ! copy subdiagonal element of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = czero
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    e( k ) = w( k+1, k )
                    e( k+1 ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_zlasyf_rk




     pure module subroutine stdlib_ssytf2_rk( uplo, n, a, lda, e, ipiv, info )
     !! SSYTF2_RK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, rowmax, stemp, t, wk, wkm1, &
                     wkp1, sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTF2_RK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = zero
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 34
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = zero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_isamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_isamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          stemp = abs( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_sswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_sswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_sswap( n-k, a( k, k+1 ), lda, a( p, k+1 ), lda )
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_sswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_sswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_sswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_ssyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_sscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_ssyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                    ! copy superdiagonal elements of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    e( k ) = a( k-1, k )
                    e( k-1 ) = zero
                    a( k-1, k ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              34 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = zero
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 64
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k<n )e( k ) = zero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_isamax( imax-k, a( imax, k ), lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_isamax( n-imax, a( imax+1, imax ),1_ilp )
                          stemp = abs( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_sswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_sswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_sswap( k-1, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_sswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_sswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_ssyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_sscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_ssyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                    ! copy subdiagonal elements of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    e( k ) = a( k+1, k )
                    e( k+1 ) = zero
                    a( k+1, k ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
              64 continue
           end if
           return
     end subroutine stdlib_ssytf2_rk

     pure module subroutine stdlib_dsytf2_rk( uplo, n, a, lda, e, ipiv, info )
     !! DSYTF2_RK computes the factorization of a real symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, rowmax, dtemp, t, wk, wkm1, &
                     wkp1, sfmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTF2_RK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = zero
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 34
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = zero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_idamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_idamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          dtemp = abs( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_dswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_dswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_dswap( n-k, a( k, k+1 ), lda, a( p, k+1 ), lda )
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_dswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_dswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_dswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_dsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_dscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_dsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                    ! copy superdiagonal elements of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    e( k ) = a( k-1, k )
                    e( k-1 ) = zero
                    a( k-1, k ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              34 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = zero
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 64
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k<n )e( k ) = zero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_idamax( imax-k, a( imax, k ), lda )
                          rowmax = abs( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_idamax( n-imax, a( imax+1, imax ),1_ilp )
                          dtemp = abs( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( abs( a( imax, imax ) )<alpha*rowmax ) )then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_dswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_dswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_dswap( k-1, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_dswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_dswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( abs( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = one / a( k, k )
                          call stdlib_dsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_dscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_dsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = zero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                    ! copy subdiagonal elements of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    e( k ) = a( k+1, k )
                    e( k+1 ) = zero
                    a( k+1, k ) = zero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
              64 continue
           end if
           return
     end subroutine stdlib_dsytf2_rk


     pure module subroutine stdlib_csytf2_rk( uplo, n, a, lda, e, ipiv, info )
     !! CSYTF2_RK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(sp) :: absakk, alpha, colmax, rowmax, stemp, sfmin
           complex(sp) :: d11, d12, d21, d22, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTF2_RK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_slamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = czero
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 34
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = czero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_icamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_icamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          stemp = cabs1( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1( a( imax, imax ) )<alpha*rowmax ))then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_cswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_cswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_cswap( n-k, a( k, k+1 ), lda, a( p, k+1 ), lda )
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_cswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_cswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_cswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_csyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_cscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_csyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                    ! copy superdiagonal elements of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    e( k ) = a( k-1, k )
                    e( k-1 ) = czero
                    a( k-1, k ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              34 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = czero
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 64
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k<n )e( k ) = czero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_icamax( imax-k, a( imax, k ), lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_icamax( n-imax, a( imax+1, imax ),1_ilp )
                          stemp = cabs1( a( itemp, imax ) )
                          if( stemp>rowmax ) then
                             rowmax = stemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1( a( imax, imax ) )<alpha*rowmax ))then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_cswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_cswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_cswap( k-1, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_cswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_cswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_csyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_cscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_csyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                    ! copy subdiagonal elements of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    e( k ) = a( k+1, k )
                    e( k+1 ) = czero
                    a( k+1, k ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
              64 continue
           end if
           return
     end subroutine stdlib_csytf2_rk

     pure module subroutine stdlib_zsytf2_rk( uplo, n, a, lda, e, ipiv, info )
     !! ZSYTF2_RK computes the factorization of a complex symmetric matrix A
     !! using the bounded Bunch-Kaufman (rook) diagonal pivoting method:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
     !! For more information see Further Details section.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: e(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper, done
           integer(ilp) :: i, imax, j, jmax, itemp, k, kk, kp, kstep, p, ii
           real(dp) :: absakk, alpha, colmax, rowmax, dtemp, sfmin
           complex(dp) :: d11, d12, d21, d22, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTF2_RK', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           ! compute machine safe minimum
           sfmin = stdlib_dlamch( 'S' )
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! initialize the first entry of array e, where superdiagonal
              ! elements of d are stored
              e( 1_ilp ) = czero
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 34
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k>1_ilp )e( k ) = czero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange,
                    ! use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    12 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = imax + stdlib_izamax( k-imax, a( imax, imax+1 ),lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax>1_ilp ) then
                          itemp = stdlib_izamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                          dtemp = cabs1( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1( a( imax, imax ) )<alpha*rowmax ))then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 12
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the leading
                    ! submatrix a(1:k,1:k) if we have a 2-by-2 pivot
                    if( p>1_ilp )call stdlib_zswap( p-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, p ), 1_ilp )
                    if( p<(k-1) )call stdlib_zswap( k-p-1, a( p+1, k ), 1_ilp, a( p, p+1 ),lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_zswap( n-k, a( k, k+1 ), lda, a( p, k+1 ), lda )
                 end if
                 ! second swap
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    if( kp>1_ilp )call stdlib_zswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    if( ( kk>1_ilp ) .and. ( kp<(kk-1) ) )call stdlib_zswap( kk-kp-1, a( kp+1, kk ), &
                              1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert upper triangle of a into u form by applying
                    ! the interchanges in columns k+1:n.
                    if( k<n )call stdlib_zswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    if( k>1_ilp ) then
                       ! perform a rank-1 update of a(1:k-1,1:k-1) and
                       ! store u(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(1:k-1,1:k-1) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*1/d(k)*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_zsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                          ! store u(k) in column k
                          call stdlib_zscal( k-1, d11, a( 1_ilp, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = 1, k - 1
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - u(k)*d(k)*u(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_zsyr( uplo, k-1, -d11, a( 1_ilp, k ), 1_ilp, a, lda )
                       end if
                       ! store the superdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( ( a(k-1)a(k) )*inv(d(k)) ) * ( a(k-1)a(k) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       do j = k - 2, 1, -1
                          wkm1 = t*( d11*a( j, k-1 )-a( j, k ) )
                          wk = t*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - (a( i, k ) / d12 )*wk -( a( i, k-1 ) / d12 )&
                                       *wkm1
                          end do
                          ! store u(k) and u(k-1) in cols k and k-1 for row j
                          a( j, k ) = wk / d12
                          a( j, k-1 ) = wkm1 / d12
                       end do
                    end if
                    ! copy superdiagonal elements of d(k) to e(k) and
                    ! zero out superdiagonal entry of a
                    e( k ) = a( k-1, k )
                    e( k-1 ) = czero
                    a( k-1, k ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              34 continue
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! initialize the unused last entry of the subdiagonal array e.
              e( n ) = czero
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 64
              kstep = 1_ilp
              p = k
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( ( max( absakk, colmax )==zero ) ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ! set e( k ) to zero
                 if( k<n )e( k ) = czero
              else
                 ! test for interchange
                 ! equivalent to testing for (used to handle nan and inf)
                 ! absakk>=alpha*colmax
                 if( .not.( absakk<alpha*colmax ) ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    done = .false.
                    ! loop until pivot found
                    42 continue
                       ! begin pivot search loop body
                       ! jmax is the column-index of the largest off-diagonal
                       ! element in row imax, and rowmax is its absolute value.
                       ! determine both rowmax and jmax.
                       if( imax/=k ) then
                          jmax = k - 1_ilp + stdlib_izamax( imax-k, a( imax, k ), lda )
                          rowmax = cabs1( a( imax, jmax ) )
                       else
                          rowmax = zero
                       end if
                       if( imax<n ) then
                          itemp = imax + stdlib_izamax( n-imax, a( imax+1, imax ),1_ilp )
                          dtemp = cabs1( a( itemp, imax ) )
                          if( dtemp>rowmax ) then
                             rowmax = dtemp
                             jmax = itemp
                          end if
                       end if
                       ! equivalent to testing for (used to handle nan and inf)
                       ! abs( a( imax, imax ) )>=alpha*rowmax
                       if( .not.( cabs1( a( imax, imax ) )<alpha*rowmax ))then
                          ! interchange rows and columns k and imax,
                          ! use 1-by-1 pivot block
                          kp = imax
                          done = .true.
                       ! equivalent to testing for rowmax == colmax,
                       ! used to handle nan and inf
                       else if( ( p==jmax ).or.( rowmax<=colmax ) ) then
                          ! interchange rows and columns k+1 and imax,
                          ! use 2-by-2 pivot block
                          kp = imax
                          kstep = 2_ilp
                          done = .true.
                       else
                          ! pivot not found, set variables and repeat
                          p = imax
                          colmax = rowmax
                          imax = jmax
                       end if
                       ! end pivot search loop body
                    if( .not. done ) goto 42
                 end if
                 ! swap two rows and two columns
                 ! first swap
                 if( ( kstep==2_ilp ) .and. ( p/=k ) ) then
                    ! interchange rows and column k and p in the trailing
                    ! submatrix a(k:n,k:n) if we have a 2-by-2 pivot
                    if( p<n )call stdlib_zswap( n-p, a( p+1, k ), 1_ilp, a( p+1, p ), 1_ilp )
                    if( p>(k+1) )call stdlib_zswap( p-k-1, a( k+1, k ), 1_ilp, a( p, k+1 ), lda )
                              
                    t = a( k, k )
                    a( k, k ) = a( p, p )
                    a( p, p ) = t
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_zswap( k-1, a( k, 1_ilp ), lda, a( p, 1_ilp ), lda )
                 end if
                 ! second swap
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    if( ( kk<n ) .and. ( kp>(kk+1) ) )call stdlib_zswap( kp-kk-1, a( kk+1, kk ), &
                              1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                    ! convert lower triangle of a into l form by applying
                    ! the interchanges in columns 1:k-1.
                    if ( k>1_ilp )call stdlib_zswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                    ! perform a rank-1 update of a(k+1:n,k+1:n) and
                    ! store l(k) in column k
                       if( cabs1( a( k, k ) )>=sfmin ) then
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                          d11 = cone / a( k, k )
                          call stdlib_zsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                          ! store l(k) in column k
                          call stdlib_zscal( n-k, d11, a( k+1, k ), 1_ilp )
                       else
                          ! store l(k) in column k
                          d11 = a( k, k )
                          do ii = k + 1, n
                             a( ii, k ) = a( ii, k ) / d11
                          end do
                          ! perform a rank-1 update of a(k+1:n,k+1:n) as
                          ! a := a - l(k)*d(k)*l(k)**t
                             ! = a - w(k)*(1/d(k))*w(k)**t
                             ! = a - (w(k)/d(k))*(d(k))*(w(k)/d(k))**t
                          call stdlib_zsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                    
                       end if
                       ! store the subdiagonal element of d in array e
                       e( k ) = czero
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! perform a rank-2 update of a(k+2:n,k+2:n) as
                    ! a := a - ( l(k) l(k+1) ) * d(k) * ( l(k) l(k+1) )**t
                       ! = a - ( ( a(k)a(k+1) )*inv(d(k) ) * ( a(k)a(k+1) )**t
                    ! and store l(k) and l(k+1) in columns k and k+1
                    if( k<n-1 ) then
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       do j = k + 2, n
                          ! compute  d21 * ( w(k)w(k+1) ) * inv(d(k)) for row j
                          wk = t*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = t*( d22*a( j, k+1 )-a( j, k ) )
                          ! perform a rank-2 update of a(k+2:n,k+2:n)
                          do i = j, n
                             a( i, j ) = a( i, j ) - ( a( i, k ) / d21 )*wk -( a( i, k+1 ) / d21 )&
                                       *wkp1
                          end do
                          ! store l(k) and l(k+1) in cols k and k+1 for row j
                          a( j, k ) = wk / d21
                          a( j, k+1 ) = wkp1 / d21
                       end do
                    end if
                    ! copy subdiagonal elements of d(k) to e(k) and
                    ! zero out subdiagonal entry of a
                    e( k ) = a( k+1, k )
                    e( k+1 ) = czero
                    a( k+1, k ) = czero
                 end if
                 ! end column k is nonsingular
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -p
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
              64 continue
           end if
           return
     end subroutine stdlib_zsytf2_rk




     pure module subroutine stdlib_ssyconvf( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! SSYCONVF converts the factorization output format used in
     !! SSYTRF provided on entry in parameter A into the factorization
     !! output format used in SSYTRF_RK (or SSYTRF_BK) that is stored
     !! on exit in parameters A and E. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in SSYTRF into
     !! the format used in SSYTRF_RK (or SSYTRF_BK).
     !! If parameter WAY = 'R':
     !! SSYCONVF performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in SSYTRF_RK
     !! (or SSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in SSYTRF that is stored
     !! on exit in parameter A. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in SSYTRF_RK
     !! (or SSYTRF_BK) into the format used in SSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYCONVF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = zero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = zero
                       a( i-1, i ) = zero
                       i = i - 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_sswap( n-i, a( i-1, i+1 ), lda,a( ip, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_sswap( n-i, a( ip, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is one interchange of rows i-1 and ipiv(i-1),
                       ! so this should be recorded in two consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i-1 )
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = zero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = zero
                       a( i+1, i ) = zero
                       i = i + 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where k increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_sswap( i-1, a( i+1, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_sswap( i-1, a( ip, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is one interchange of rows i+1 and ipiv(i+1),
                       ! so this should be recorded in consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i+1 )
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_ssyconvf

     pure module subroutine stdlib_dsyconvf( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! DSYCONVF converts the factorization output format used in
     !! DSYTRF provided on entry in parameter A into the factorization
     !! output format used in DSYTRF_RK (or DSYTRF_BK) that is stored
     !! on exit in parameters A and E. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in DSYTRF into
     !! the format used in DSYTRF_RK (or DSYTRF_BK).
     !! If parameter WAY = 'R':
     !! DSYCONVF performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in DSYTRF_RK
     !! (or DSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in DSYTRF that is stored
     !! on exit in parameter A. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in DSYTRF_RK
     !! (or DSYTRF_BK) into the format used in DSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYCONVF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = zero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = zero
                       a( i-1, i ) = zero
                       i = i - 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_dswap( n-i, a( i-1, i+1 ), lda,a( ip, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_dswap( n-i, a( ip, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is one interchange of rows i-1 and ipiv(i-1),
                       ! so this should be recorded in two consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i-1 )
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = zero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = zero
                       a( i+1, i ) = zero
                       i = i + 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where k increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_dswap( i-1, a( i+1, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_dswap( i-1, a( ip, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is one interchange of rows i+1 and ipiv(i+1),
                       ! so this should be recorded in consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i+1 )
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_dsyconvf


     pure module subroutine stdlib_csyconvf( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! CSYCONVF converts the factorization output format used in
     !! CSYTRF provided on entry in parameter A into the factorization
     !! output format used in CSYTRF_RK (or CSYTRF_BK) that is stored
     !! on exit in parameters A and E. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in CSYTRF into
     !! the format used in CSYTRF_RK (or CSYTRF_BK).
     !! If parameter WAY = 'R':
     !! CSYCONVF performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in CSYTRF_RK
     !! (or CSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in CSYTRF that is stored
     !! on exit in parameter A. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in CSYTRF_RK
     !! (or CSYTRF_BK) into the format used in CSYTRF.
     !! CSYCONVF can also convert in Hermitian matrix case, i.e. between
     !! formats used in CHETRF and CHETRF_RK (or CHETRF_BK).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYCONVF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = czero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = czero
                       a( i-1, i ) = czero
                       i = i - 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_cswap( n-i, a( i-1, i+1 ), lda,a( ip, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_cswap( n-i, a( ip, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is cone interchange of rows i-1 and ipiv(i-1),
                       ! so this should be recorded in two consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i-1 )
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = czero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = czero
                       a( i+1, i ) = czero
                       i = i + 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where k increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_cswap( i-1, a( i+1, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_cswap( i-1, a( ip, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is cone interchange of rows i+1 and ipiv(i+1),
                       ! so this should be recorded in consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i+1 )
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_csyconvf

     pure module subroutine stdlib_zsyconvf( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! ZSYCONVF converts the factorization output format used in
     !! ZSYTRF provided on entry in parameter A into the factorization
     !! output format used in ZSYTRF_RK (or ZSYTRF_BK) that is stored
     !! on exit in parameters A and E. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in ZSYTRF into
     !! the format used in ZSYTRF_RK (or ZSYTRF_BK).
     !! If parameter WAY = 'R':
     !! ZSYCONVF performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in ZSYTRF_RK
     !! (or ZSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in ZSYTRF that is stored
     !! on exit in parameter A. It also converts in place details of
     !! the intechanges stored in IPIV from the format used in ZSYTRF_RK
     !! (or ZSYTRF_BK) into the format used in ZSYTRF.
     !! ZSYCONVF can also convert in Hermitian matrix case, i.e. between
     !! formats used in ZHETRF and ZHETRF_RK (or ZHETRF_BK).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYCONVF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = czero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = czero
                       a( i-1, i ) = czero
                       i = i - 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_zswap( n-i, a( i-1, i+1 ), lda,a( ip, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i) in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       if( i<n ) then
                          if( ip/=(i-1) ) then
                             call stdlib_zswap( n-i, a( ip, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                       end if
                       ! convert ipiv
                       ! there is cone interchange of rows i-1 and ipiv(i-1),
                       ! so this should be recorded in two consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i-1 )
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = czero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = czero
                       a( i+1, i ) = czero
                       i = i + 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where k increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_zswap( i-1, a( i+1, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is no interchnge of rows i and and ipiv(i),
                       ! so this should be reflected in ipiv format for
                       ! *sytrf_rk ( or *sytrf_bk)
                       ipiv( i ) = i
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations and ipiv
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i) in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=(i+1) ) then
                             call stdlib_zswap( i-1, a( ip, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                       end if
                       ! convert ipiv
                       ! there is cone interchange of rows i+1 and ipiv(i+1),
                       ! so this should be recorded in consecutive entries
                       ! in ipiv format for *sytrf
                       ipiv( i ) = ipiv( i+1 )
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_zsyconvf




     pure module subroutine stdlib_ssyconvf_rook( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! SSYCONVF_ROOK converts the factorization output format used in
     !! SSYTRF_ROOK provided on entry in parameter A into the factorization
     !! output format used in SSYTRF_RK (or SSYTRF_BK) that is stored
     !! on exit in parameters A and E. IPIV format for SSYTRF_ROOK and
     !! SSYTRF_RK (or SSYTRF_BK) is the same and is not converted.
     !! If parameter WAY = 'R':
     !! SSYCONVF_ROOK performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in SSYTRF_RK
     !! (or SSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in SSYTRF_ROOK that is stored
     !! on exit in parameter A. IPIV format for SSYTRF_ROOK and
     !! SSYTRF_RK (or SSYTRF_BK) is the same and is not converted.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, ip2
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYCONVF_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = zero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = zero
                       a( i-1, i ) = zero
                       i = i - 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i-1 and ipiv(i-1)
                       ! in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                          if( ip2/=(i-1) ) then
                             call stdlib_sswap( n-i, a( i-1, i+1 ), lda,a( ip2, i+1 ), lda )
                                       
                          end if
                       end if
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i-1) and i and ipiv(i)
                       ! in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip2/=(i-1) ) then
                             call stdlib_sswap( n-i, a( ip2, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                          if( ip/=i ) then
                             call stdlib_sswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = zero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = zero
                       a( i+1, i ) = zero
                       i = i + 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i+1 and ipiv(i+1)
                       ! in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                          if( ip2/=(i+1) ) then
                             call stdlib_sswap( i-1, a( i+1, 1_ilp ), lda,a( ip2, 1_ilp ), lda )
                          end if
                       end if
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i+1) and i and ipiv(i)
                       ! in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip2/=(i+1) ) then
                             call stdlib_sswap( i-1, a( ip2, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                          if( ip/=i ) then
                             call stdlib_sswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_ssyconvf_rook

     pure module subroutine stdlib_dsyconvf_rook( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! DSYCONVF_ROOK converts the factorization output format used in
     !! DSYTRF_ROOK provided on entry in parameter A into the factorization
     !! output format used in DSYTRF_RK (or DSYTRF_BK) that is stored
     !! on exit in parameters A and E. IPIV format for DSYTRF_ROOK and
     !! DSYTRF_RK (or DSYTRF_BK) is the same and is not converted.
     !! If parameter WAY = 'R':
     !! DSYCONVF_ROOK performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in DSYTRF_RK
     !! (or DSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in DSYTRF_ROOK that is stored
     !! on exit in parameter A. IPIV format for DSYTRF_ROOK and
     !! DSYTRF_RK (or DSYTRF_BK) is the same and is not converted.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, ip2
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYCONVF_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = zero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = zero
                       a( i-1, i ) = zero
                       i = i - 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i-1 and ipiv(i-1)
                       ! in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                          if( ip2/=(i-1) ) then
                             call stdlib_dswap( n-i, a( i-1, i+1 ), lda,a( ip2, i+1 ), lda )
                                       
                          end if
                       end if
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i-1) and i and ipiv(i)
                       ! in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip2/=(i-1) ) then
                             call stdlib_dswap( n-i, a( ip2, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                          if( ip/=i ) then
                             call stdlib_dswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and zero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = zero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = zero
                       a( i+1, i ) = zero
                       i = i + 1_ilp
                    else
                       e( i ) = zero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i+1 and ipiv(i+1)
                       ! in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                          if( ip2/=(i+1) ) then
                             call stdlib_dswap( i-1, a( i+1, 1_ilp ), lda,a( ip2, 1_ilp ), lda )
                          end if
                       end if
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i+1) and i and ipiv(i)
                       ! in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip2/=(i+1) ) then
                             call stdlib_dswap( i-1, a( ip2, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                          if( ip/=i ) then
                             call stdlib_dswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_dsyconvf_rook


     pure module subroutine stdlib_csyconvf_rook( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! CSYCONVF_ROOK converts the factorization output format used in
     !! CSYTRF_ROOK provided on entry in parameter A into the factorization
     !! output format used in CSYTRF_RK (or CSYTRF_BK) that is stored
     !! on exit in parameters A and E. IPIV format for CSYTRF_ROOK and
     !! CSYTRF_RK (or CSYTRF_BK) is the same and is not converted.
     !! If parameter WAY = 'R':
     !! CSYCONVF_ROOK performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in CSYTRF_RK
     !! (or CSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in CSYTRF_ROOK that is stored
     !! on exit in parameter A. IPIV format for CSYTRF_ROOK and
     !! CSYTRF_RK (or CSYTRF_BK) is the same and is not converted.
     !! CSYCONVF_ROOK can also convert in Hermitian matrix case, i.e. between
     !! formats used in CHETRF_ROOK and CHETRF_RK (or CHETRF_BK).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, ip2
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYCONVF_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = czero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = czero
                       a( i-1, i ) = czero
                       i = i - 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i-1 and ipiv(i-1)
                       ! in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                          if( ip2/=(i-1) ) then
                             call stdlib_cswap( n-i, a( i-1, i+1 ), lda,a( ip2, i+1 ), lda )
                                       
                          end if
                       end if
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i-1) and i and ipiv(i)
                       ! in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip2/=(i-1) ) then
                             call stdlib_cswap( n-i, a( ip2, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                          if( ip/=i ) then
                             call stdlib_cswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = czero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = czero
                       a( i+1, i ) = czero
                       i = i + 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i+1 and ipiv(i+1)
                       ! in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                          if( ip2/=(i+1) ) then
                             call stdlib_cswap( i-1, a( i+1, 1_ilp ), lda,a( ip2, 1_ilp ), lda )
                          end if
                       end if
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i+1) and i and ipiv(i)
                       ! in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip2/=(i+1) ) then
                             call stdlib_cswap( i-1, a( ip2, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                          if( ip/=i ) then
                             call stdlib_cswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_csyconvf_rook

     pure module subroutine stdlib_zsyconvf_rook( uplo, way, n, a, lda, e, ipiv, info )
     !! If parameter WAY = 'C':
     !! ZSYCONVF_ROOK converts the factorization output format used in
     !! ZSYTRF_ROOK provided on entry in parameter A into the factorization
     !! output format used in ZSYTRF_RK (or ZSYTRF_BK) that is stored
     !! on exit in parameters A and E. IPIV format for ZSYTRF_ROOK and
     !! ZSYTRF_RK (or ZSYTRF_BK) is the same and is not converted.
     !! If parameter WAY = 'R':
     !! ZSYCONVF_ROOK performs the conversion in reverse direction, i.e.
     !! converts the factorization output format used in ZSYTRF_RK
     !! (or ZSYTRF_BK) provided on entry in parameters A and E into
     !! the factorization output format used in ZSYTRF_ROOK that is stored
     !! on exit in parameter A. IPIV format for ZSYTRF_ROOK and
     !! ZSYTRF_RK (or ZSYTRF_BK) is the same and is not converted.
     !! ZSYCONVF_ROOK can also convert in Hermitian matrix case, i.e. between
     !! formats used in ZHETRF_ROOK and ZHETRF_RK (or ZHETRF_BK).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, ip2
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYCONVF_ROOK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! begin a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 ! assign superdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = n
                 e( 1_ilp ) = czero
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       e( i ) = a( i-1, i )
                       e( i-1 ) = czero
                       a( i-1, i ) = czero
                       i = i - 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i - 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i-1 and ipiv(i-1)
                       ! in a(1:i,n-i:n)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( i, i+1 ), lda,a( ip, i+1 ), lda )
                          end if
                          if( ip2/=(i-1) ) then
                             call stdlib_zswap( n-i, a( i-1, i+1 ), lda,a( ip2, i+1 ), lda )
                                       
                          end if
                       end if
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations
                 ! apply permutations to submatrices of upper part of a
                 ! in reverse factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(1:i,n-i:n)
                       ip = ipiv( i )
                       if( i<n ) then
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i-1 and ipiv(i-1) and i and ipiv(i)
                       ! in a(1:i,n-i:n)
                       i = i + 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i-1 )
                       if( i<n ) then
                          if( ip2/=(i-1) ) then
                             call stdlib_zswap( n-i, a( ip2, i+1 ), lda,a( i-1, i+1 ), lda )
                                       
                          end if
                          if( ip/=i ) then
                             call stdlib_zswap( n-i, a( ip, i+1 ), lda,a( i, i+1 ), lda )
                          end if
                       end if
                    end if
                    i = i + 1_ilp
                 end do
                 ! revert value
                 ! assign superdiagonal entries of d from array e to
                 ! superdiagonal entries of a.
                 i = n
                 do while ( i>1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i-1, i ) = e( i )
                       i = i - 1_ilp
                    end if
                    i = i - 1_ilp
                 end do
              ! end a is upper
              end if
           else
              ! begin a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 ! assign subdiagonal entries of d to array e and czero out
                 ! corresponding entries in input storage a
                 i = 1_ilp
                 e( n ) = czero
                 do while ( i<=n )
                    if( i<n .and. ipiv(i)<0_ilp ) then
                       e( i ) = a( i+1, i )
                       e( i+1 ) = czero
                       a( i+1, i ) = czero
                       i = i + 1_ilp
                    else
                       e( i ) = czero
                    end if
                    i = i + 1_ilp
                 end do
                 ! convert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in factorization order where i increases from 1 to n
                 i = 1_ilp
                 do while ( i<=n )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i and ipiv(i) and i+1 and ipiv(i+1)
                       ! in a(i:n,1:i-1)
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( i, 1_ilp ), lda,a( ip, 1_ilp ), lda )
                          end if
                          if( ip2/=(i+1) ) then
                             call stdlib_zswap( i-1, a( i+1, 1_ilp ), lda,a( ip2, 1_ilp ), lda )
                          end if
                       end if
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations
                 ! apply permutations to submatrices of lower part of a
                 ! in reverse factorization order where i decreases from n to 1
                 i = n
                 do while ( i>=1 )
                    if( ipiv( i )>0_ilp ) then
                       ! 1-by-1 pivot interchange
                       ! swap rows i and ipiv(i) in a(i:n,1:i-1)
                       ip = ipiv( i )
                       if ( i>1_ilp ) then
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    else
                       ! 2-by-2 pivot interchange
                       ! swap rows i+1 and ipiv(i+1) and i and ipiv(i)
                       ! in a(i:n,1:i-1)
                       i = i - 1_ilp
                       ip = -ipiv( i )
                       ip2 = -ipiv( i+1 )
                       if ( i>1_ilp ) then
                          if( ip2/=(i+1) ) then
                             call stdlib_zswap( i-1, a( ip2, 1_ilp ), lda,a( i+1, 1_ilp ), lda )
                          end if
                          if( ip/=i ) then
                             call stdlib_zswap( i-1, a( ip, 1_ilp ), lda,a( i, 1_ilp ), lda )
                          end if
                       end if
                    end if
                    i = i - 1_ilp
                 end do
                 ! revert value
                 ! assign subdiagonal entries of d from array e to
                 ! subgiagonal entries of a.
                 i = 1_ilp
                 do while ( i<=n-1 )
                    if( ipiv( i )<0_ilp ) then
                       a( i + 1_ilp, i ) = e( i )
                       i = i + 1_ilp
                    end if
                    i = i + 1_ilp
                 end do
              end if
              ! end a is lower
           end if
           return
     end subroutine stdlib_zsyconvf_rook




     pure module subroutine stdlib_ssytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info)
     !! SSYTRF_AA computes the factorization of a real symmetric matrix A
     !! using the Aasen's algorithm.  The form of the factorization is
     !! A = U**T*T*U  or  A = L*T*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is a symmetric tridiagonal matrix.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: j, lwkopt
           integer(ilp) :: nb, mj, nj, k1, k2, j1, j2, j3, jb
           real(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the block size
           nb = stdlib_ilaenv( 1_ilp, 'SSYTRF_AA', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = (nb+1)*n
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRF_AA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return
           if ( n==0_ilp ) then
               return
           endif
           ipiv( 1_ilp ) = 1_ilp
           if ( n==1_ilp ) then
              return
           end if
           ! adjust block size based on the workspace size
           if( lwork<((1_ilp+nb)*n) ) then
              nb = ( lwork-n ) / n
           end if
           if( upper ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              ! copy first row a(1, 1:n) into h(1:n) (stored in work(1:n))
              call stdlib_scopy( n, a( 1_ilp, 1_ilp ), lda, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_slasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              10 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j + 1_ilp
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_slasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( max(1_ilp, j), j+1 ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_sswap( j1-k1-2, a( 1_ilp, j2 ), 1_ilp,a( 1_ilp, ipiv(j2) ), 1_ilp )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
               ! the row a(j1-1, j2-1:n) stores u(j1, j2+1:n) and
               ! work stores the current block of the auxiriarly matrix h
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j, j+1 )
                    a( j, j+1 ) = one
                    call stdlib_scopy( n-j, a( j-1, j+1 ), lda,work( (j+1-j1+1)+jb*n ), 1_ilp )
                              
                    call stdlib_sscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_sgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_sgemv( 'NO TRANSPOSE', mj, jb+1,-one, work( j3-j1+1+k1*n ), &
                                    n,a( j1-k2, j3 ), 1_ilp,one, a( j3, j3 ), lda )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block of j2-th block row with stdlib_sgemm
                       call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE',nj, n-j3+1, jb+1,-one, a( j1-&
                                 k2, j2 ), lda,work( j3-j1+1+k1*n ), n,one, a( j2, j3 ), lda )
                    end do
                    ! recover t( j, j+1 )
                    a( j, j+1 ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_scopy( n-j, a( j+1, j+1 ), lda, work( 1_ilp ), 1_ilp )
              end if
              go to 10
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              ! copy first column a(1:n, 1) into h(1:n, 1)
               ! (stored in work(1:n))
              call stdlib_scopy( n, a( 1_ilp, 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_slasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              11 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j+1
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_slasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( j+1, max(1_ilp, j) ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_sswap( j1-k1-2, a( j2, 1_ilp ), lda,a( ipiv(j2), 1_ilp ), lda )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
                ! a(j2+1, j1-1) stores l(j2+1, j1) and
                ! work(j2+1, 1) stores h(j2+1, 1)
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j+1, j )
                    a( j+1, j ) = one
                    call stdlib_scopy( n-j, a( j+1, j-1 ), 1_ilp,work( (j+1-j1+1)+jb*n ), 1_ilp )
                    call stdlib_sscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_sgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_sgemv( 'NO TRANSPOSE', mj, jb+1,-one, work( j3-j1+1+k1*n ), &
                                    n,a( j3, j1-k2 ), lda,one, a( j3, j3 ), 1_ilp )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block in j2-th block column with stdlib_sgemm
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE',n-j3+1, nj, jb+1,-one, work(&
                                  j3-j1+1+k1*n ), n,a( j2, j1-k2 ), lda,one, a( j3, j2 ), lda )
                    end do
                    ! recover t( j+1, j )
                    a( j+1, j ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_scopy( n-j, a( j+1, j+1 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              go to 11
           end if
           20 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssytrf_aa

     pure module subroutine stdlib_dsytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info)
     !! DSYTRF_AA computes the factorization of a real symmetric matrix A
     !! using the Aasen's algorithm.  The form of the factorization is
     !! A = U**T*T*U  or  A = L*T*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is a symmetric tridiagonal matrix.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: j, lwkopt
           integer(ilp) :: nb, mj, nj, k1, k2, j1, j2, j3, jb
           real(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the block size
           nb = stdlib_ilaenv( 1_ilp, 'DSYTRF_AA', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = (nb+1)*n
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRF_AA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return
           if ( n==0_ilp ) then
               return
           endif
           ipiv( 1_ilp ) = 1_ilp
           if ( n==1_ilp ) then
              return
           end if
           ! adjust block size based on the workspace size
           if( lwork<((1_ilp+nb)*n) ) then
              nb = ( lwork-n ) / n
           end if
           if( upper ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              ! copy first row a(1, 1:n) into h(1:n) (stored in work(1:n))
              call stdlib_dcopy( n, a( 1_ilp, 1_ilp ), lda, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_dlasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              10 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j + 1_ilp
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_dlasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( max(1_ilp, j), j+1 ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_dswap( j1-k1-2, a( 1_ilp, j2 ), 1_ilp,a( 1_ilp, ipiv(j2) ), 1_ilp )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
               ! the row a(j1-1, j2-1:n) stores u(j1, j2+1:n) and
               ! work stores the current block of the auxiriarly matrix h
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j, j+1 )
                    a( j, j+1 ) = one
                    call stdlib_dcopy( n-j, a( j-1, j+1 ), lda,work( (j+1-j1+1)+jb*n ), 1_ilp )
                              
                    call stdlib_dscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_dgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_dgemv( 'NO TRANSPOSE', mj, jb+1,-one, work( j3-j1+1+k1*n ), &
                                    n,a( j1-k2, j3 ), 1_ilp,one, a( j3, j3 ), lda )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block of j2-th block row with stdlib_dgemm
                       call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE',nj, n-j3+1, jb+1,-one, a( j1-&
                                 k2, j2 ), lda,work( j3-j1+1+k1*n ), n,one, a( j2, j3 ), lda )
                    end do
                    ! recover t( j, j+1 )
                    a( j, j+1 ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_dcopy( n-j, a( j+1, j+1 ), lda, work( 1_ilp ), 1_ilp )
              end if
              go to 10
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              ! copy first column a(1:n, 1) into h(1:n, 1)
               ! (stored in work(1:n))
              call stdlib_dcopy( n, a( 1_ilp, 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_dlasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              11 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j+1
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_dlasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( j+1, max(1_ilp, j) ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_dswap( j1-k1-2, a( j2, 1_ilp ), lda,a( ipiv(j2), 1_ilp ), lda )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
                ! a(j2+1, j1-1) stores l(j2+1, j1) and
                ! work(j2+1, 1) stores h(j2+1, 1)
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j+1, j )
                    a( j+1, j ) = one
                    call stdlib_dcopy( n-j, a( j+1, j-1 ), 1_ilp,work( (j+1-j1+1)+jb*n ), 1_ilp )
                    call stdlib_dscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_dgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_dgemv( 'NO TRANSPOSE', mj, jb+1,-one, work( j3-j1+1+k1*n ), &
                                    n,a( j3, j1-k2 ), lda,one, a( j3, j3 ), 1_ilp )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block in j2-th block column with stdlib_dgemm
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE',n-j3+1, nj, jb+1,-one, work(&
                                  j3-j1+1+k1*n ), n,a( j2, j1-k2 ), lda,one, a( j3, j2 ), lda )
                    end do
                    ! recover t( j+1, j )
                    a( j+1, j ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_dcopy( n-j, a( j+1, j+1 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              go to 11
           end if
           20 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsytrf_aa


     pure module subroutine stdlib_csytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info)
     !! CSYTRF_AA computes the factorization of a complex symmetric matrix A
     !! using the Aasen's algorithm.  The form of the factorization is
     !! A = U**T*T*U  or  A = L*T*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is a complex symmetric tridiagonal matrix.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: j, lwkopt
           integer(ilp) :: nb, mj, nj, k1, k2, j1, j2, j3, jb
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the block size
           nb = stdlib_ilaenv( 1_ilp, 'CSYTRF_AA', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = (nb+1)*n
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRF_AA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return
           if ( n==0_ilp ) then
               return
           endif
           ipiv( 1_ilp ) = 1_ilp
           if ( n==1_ilp ) then
              return
           end if
           ! adjust block size based on the workspace size
           if( lwork<((1_ilp+nb)*n) ) then
              nb = ( lwork-n ) / n
           end if
           if( upper ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              ! copy first row a(1, 1:n) into h(1:n) (stored in work(1:n))
              call stdlib_ccopy( n, a( 1_ilp, 1_ilp ), lda, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_clasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              10 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j + 1_ilp
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_clasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( max(1_ilp, j), j+1 ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_cswap( j1-k1-2, a( 1_ilp, j2 ), 1_ilp,a( 1_ilp, ipiv(j2) ), 1_ilp )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
               ! the row a(j1-1, j2-1:n) stores u(j1, j2+1:n) and
               ! work stores the current block of the auxiriarly matrix h
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j, j+1 )
                    a( j, j+1 ) = cone
                    call stdlib_ccopy( n-j, a( j-1, j+1 ), lda,work( (j+1-j1+1)+jb*n ), 1_ilp )
                              
                    call stdlib_cscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_cgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_cgemv( 'NO TRANSPOSE', mj, jb+1,-cone, work( j3-j1+1+k1*n ),&
                                     n,a( j1-k2, j3 ), 1_ilp,cone, a( j3, j3 ), lda )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block of j2-th block row with stdlib_cgemm
                       call stdlib_cgemm( 'TRANSPOSE', 'TRANSPOSE',nj, n-j3+1, jb+1,-cone, a( j1-&
                                 k2, j2 ), lda,work( j3-j1+1+k1*n ), n,cone, a( j2, j3 ), lda )
                    end do
                    ! recover t( j, j+1 )
                    a( j, j+1 ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_ccopy( n-j, a( j+1, j+1 ), lda, work( 1_ilp ), 1_ilp )
              end if
              go to 10
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              ! copy first column a(1:n, 1) into h(1:n, 1)
               ! (stored in work(1:n))
              call stdlib_ccopy( n, a( 1_ilp, 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_clasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              11 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j+1
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_clasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( j+1, max(1_ilp, j) ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_cswap( j1-k1-2, a( j2, 1_ilp ), lda,a( ipiv(j2), 1_ilp ), lda )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
                ! a(j2+1, j1-1) stores l(j2+1, j1) and
                ! work(j2+1, 1) stores h(j2+1, 1)
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j+1, j )
                    a( j+1, j ) = cone
                    call stdlib_ccopy( n-j, a( j+1, j-1 ), 1_ilp,work( (j+1-j1+1)+jb*n ), 1_ilp )
                    call stdlib_cscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_cgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_cgemv( 'NO TRANSPOSE', mj, jb+1,-cone, work( j3-j1+1+k1*n ),&
                                     n,a( j3, j1-k2 ), lda,cone, a( j3, j3 ), 1_ilp )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block in j2-th block column with stdlib_cgemm
                       call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE',n-j3+1, nj, jb+1,-cone, &
                       work( j3-j1+1+k1*n ), n,a( j2, j1-k2 ), lda,cone, a( j3, j2 ), lda )
                                 
                    end do
                    ! recover t( j+1, j )
                    a( j+1, j ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_ccopy( n-j, a( j+1, j+1 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              go to 11
           end if
           20 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csytrf_aa

     pure module subroutine stdlib_zsytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info)
     !! ZSYTRF_AA computes the factorization of a complex symmetric matrix A
     !! using the Aasen's algorithm.  The form of the factorization is
     !! A = U**T*T*U  or  A = L*T*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is a complex symmetric tridiagonal matrix.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: j, lwkopt
           integer(ilp) :: nb, mj, nj, k1, k2, j1, j2, j3, jb
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the block size
           nb = stdlib_ilaenv( 1_ilp, 'ZSYTRF_AA', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = (nb+1)*n
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRF_AA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return
           if ( n==0_ilp ) then
               return
           endif
           ipiv( 1_ilp ) = 1_ilp
           if ( n==1_ilp ) then
              return
           end if
           ! adjust block size based on the workspace size
           if( lwork<((1_ilp+nb)*n) ) then
              nb = ( lwork-n ) / n
           end if
           if( upper ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              ! copy first row a(1, 1:n) into h(1:n) (stored in work(1:n))
              call stdlib_zcopy( n, a( 1_ilp, 1_ilp ), lda, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_zlasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              10 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j + 1_ilp
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_zlasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( max(1_ilp, j), j+1 ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_zswap( j1-k1-2, a( 1_ilp, j2 ), 1_ilp,a( 1_ilp, ipiv(j2) ), 1_ilp )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
               ! the row a(j1-1, j2-1:n) stores u(j1, j2+1:n) and
               ! work stores the current block of the auxiriarly matrix h
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j, j+1 )
                    a( j, j+1 ) = cone
                    call stdlib_zcopy( n-j, a( j-1, j+1 ), lda,work( (j+1-j1+1)+jb*n ), 1_ilp )
                              
                    call stdlib_zscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_zgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_zgemv( 'NO TRANSPOSE', mj, jb+1,-cone, work( j3-j1+1+k1*n ),&
                                     n,a( j1-k2, j3 ), 1_ilp,cone, a( j3, j3 ), lda )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block of j2-th block row with stdlib_zgemm
                       call stdlib_zgemm( 'TRANSPOSE', 'TRANSPOSE',nj, n-j3+1, jb+1,-cone, a( j1-&
                                 k2, j2 ), lda,work( j3-j1+1+k1*n ), n,cone, a( j2, j3 ), lda )
                    end do
                    ! recover t( j, j+1 )
                    a( j, j+1 ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_zcopy( n-j, a( j+1, j+1 ), lda, work( 1_ilp ), 1_ilp )
              end if
              go to 10
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              ! copy first column a(1:n, 1) into h(1:n, 1)
               ! (stored in work(1:n))
              call stdlib_zcopy( n, a( 1_ilp, 1_ilp ), 1_ilp, work( 1_ilp ), 1_ilp )
              ! j is the main loop index, increasing from 1 to n in steps of
              ! jb, where jb is the number of columns factorized by stdlib_zlasyf;
              ! jb is either nb, or n-j+1 for the last block
              j = 0_ilp
              11 continue
              if( j>=n )go to 20
              ! each step of the main loop
               ! j is the last column of the previous panel
               ! j1 is the first column of the current panel
               ! k1 identifies if the previous column of the panel has been
                ! explicitly stored, e.g., k1=1 for the first panel, and
                ! k1=0 for the rest
              j1 = j+1
              jb = min( n-j1+1, nb )
              k1 = max(1_ilp, j)-j
              ! panel factorization
              call stdlib_zlasyf_aa( uplo, 2_ilp-k1, n-j, jb,a( j+1, max(1_ilp, j) ), lda,ipiv( j+1 ), &
                        work, n, work( n*nb+1 ) )
              ! adjust ipiv and apply it back (j-th step picks (j+1)-th pivot)
              do j2 = j+2, min(n, j+jb+1)
                 ipiv( j2 ) = ipiv( j2 ) + j
                 if( (j2/=ipiv(j2)) .and. ((j1-k1)>2_ilp) ) then
                    call stdlib_zswap( j1-k1-2, a( j2, 1_ilp ), lda,a( ipiv(j2), 1_ilp ), lda )
                 end if
              end do
              j = j + jb
              ! trailing submatrix update, where
                ! a(j2+1, j1-1) stores l(j2+1, j1) and
                ! work(j2+1, 1) stores h(j2+1, 1)
              if( j<n ) then
                 ! if first panel and jb=1 (nb=1), then nothing to do
                 if( j1>1_ilp .or. jb>1_ilp ) then
                    ! merge rank-1 update with blas-3 update
                    alpha = a( j+1, j )
                    a( j+1, j ) = cone
                    call stdlib_zcopy( n-j, a( j+1, j-1 ), 1_ilp,work( (j+1-j1+1)+jb*n ), 1_ilp )
                    call stdlib_zscal( n-j, alpha, work( (j+1-j1+1)+jb*n ), 1_ilp )
                    ! k1 identifies if the previous column of the panel has been
                     ! explicitly stored, e.g., k1=1 and k2= 0 for the first panel,
                     ! while k1=0 and k2=1 for the rest
                    if( j1>1_ilp ) then
                       ! not first panel
                       k2 = 1_ilp
                    else
                       ! first panel
                       k2 = 0_ilp
                       ! first update skips the first column
                       jb = jb - 1_ilp
                    end if
                    do j2 = j+1, n, nb
                       nj = min( nb, n-j2+1 )
                       ! update (j2, j2) diagonal block with stdlib_zgemv
                       j3 = j2
                       do mj = nj-1, 1, -1
                          call stdlib_zgemv( 'NO TRANSPOSE', mj, jb+1,-cone, work( j3-j1+1+k1*n ),&
                                     n,a( j3, j1-k2 ), lda,cone, a( j3, j3 ), 1_ilp )
                          j3 = j3 + 1_ilp
                       end do
                       ! update off-diagonal block in j2-th block column with stdlib_zgemm
                       call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE',n-j3+1, nj, jb+1,-cone, &
                       work( j3-j1+1+k1*n ), n,a( j2, j1-k2 ), lda,cone, a( j3, j2 ), lda )
                                 
                    end do
                    ! recover t( j+1, j )
                    a( j+1, j ) = alpha
                 end if
                 ! work(j+1, 1) stores h(j+1, 1)
                 call stdlib_zcopy( n-j, a( j+1, j+1 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              go to 11
           end if
           20 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsytrf_aa




     pure module subroutine stdlib_slasyf_aa( uplo, j1, m, nb, a, lda, ipiv,h, ldh, work )
     !! DLATRF_AA factorizes a panel of a real symmetric matrix A using
     !! the Aasen's algorithm. The panel consists of a set of NB rows of A
     !! when UPLO is U, or a set of NB columns when UPLO is L.
     !! In order to factorize the panel, the Aasen's algorithm requires the
     !! last row, or column, of the previous panel. The first row, or column,
     !! of A is set to be the first row, or column, of an identity matrix,
     !! which is used to factorize the first panel.
     !! The resulting J-th row of U, or J-th column of L, is stored in the
     !! (J-1)-th row, or column, of A (without the unit diagonals), while
     !! the diagonal and subdiagonal of A are overwritten by those of T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: m, nb, j1, lda, ldh
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), h(ldh,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k, k1, i1, i2, mj
           real(sp) :: piv, alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           j = 1_ilp
           ! k1 is the first column of the panel to be factorized
           ! i.e.,  k1 is 2 for the first block column, and 1 for the rest of the blocks
           k1 = (2_ilp-j1)+1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              10 continue
              if ( j>min(m, nb) )go to 20
              ! k is the column to be factorized
               ! when being called from stdlib_ssytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j, j:m) - h(j:m, 1:(j-1)) * l(j1:(j-1), j),
               ! where h(j:m, j) has been initialized to be a(j, j:m)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_sgemv( 'NO TRANSPOSE', mj, j-k1,-one, h( j, k1 ), ldh,a( 1_ilp, j ), 1_ilp,&
                           one, h( j, j ), 1_ilp )
              end if
              ! copy h(i:m, i) into work
              call stdlib_scopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j-1, j:m) * t(j-1,j),
                  ! where a(j-1, j) stores t(j-1, j) and a(j-2, j:m) stores u(j-1, j:m)
                 alpha = -a( k-1, j )
                 call stdlib_saxpy( mj, alpha, a( k-2, j ), lda, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( k, j ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l(j, (j+1):m)
                  ! where a(j, j) stores t(j, j) and a(j-1, (j+1):m) stores u(j, (j+1):m)
                 if( k>1_ilp ) then
                    alpha = -a( k, j )
                    call stdlib_saxpy( m-j, alpha, a( k-1, j+1 ), lda,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_isamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1, i1+1:m) with a(i1+1:m, i2)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_sswap( i2-i1-1, a( j1+i1-1, i1+1 ), lda,a( j1+i1, i2 ), 1_ilp )
                              
                    ! swap a(i1, i2+1:m) with a(i2, i2+1:m)
                    if( i2<m )call stdlib_sswap( m-i2, a( j1+i1-1, i2+1 ), lda,a( j1+i2-1, i2+1 ),&
                               lda )
                    ! swap a(i1, i1) with a(i2,i2)
                    piv = a( i1+j1-1, i1 )
                    a( j1+i1-1, i1 ) = a( j1+i2-1, i2 )
                    a( j1+i2-1, i2 ) = piv
                    ! swap h(i1, 1:j1) with h(i2, 1:j1)
                    call stdlib_sswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_sswap( i1-k1+1, a( 1_ilp, i1 ), 1_ilp,a( 1_ilp, i2 ), 1_ilp )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j, j+1) = t(j, j+1)
                 a( k, j+1 ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j:m, j),
                    call stdlib_scopy( m-j, a( k+1, j+1 ), lda,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( k, j+1 )/=zero ) then
                       alpha = one / a( k, j+1 )
                       call stdlib_scopy( m-j-1, work( 3_ilp ), 1_ilp, a( k, j+2 ), lda )
                       call stdlib_sscal( m-j-1, alpha, a( k, j+2 ), lda )
                    else
                       call stdlib_slaset( 'FULL', 1_ilp, m-j-1, zero, zero,a( k, j+2 ), lda)
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 10
              20 continue
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              30 continue
              if( j>min( m, nb ) )go to 40
              ! k is the column to be factorized
               ! when being called from stdlib_ssytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j:m, j) - h(j:m, 1:(j-1)) * l(j, j1:(j-1))^t,
               ! where h(j:m, j) has been initialized to be a(j:m, j)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_sgemv( 'NO TRANSPOSE', mj, j-k1,-one, h( j, k1 ), ldh,a( j, 1_ilp ), lda,&
                           one, h( j, j ), 1_ilp )
              end if
              ! copy h(j:m, j) into work
              call stdlib_scopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j:m, j-1) * t(j-1,j),
                  ! where a(j-1, j) = t(j-1, j) and a(j, j-2) = l(j, j-1)
                 alpha = -a( j, k-1 )
                 call stdlib_saxpy( mj, alpha, a( j, k-2 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( j, k ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l((j+1):m, j)
                  ! where a(j, j) = t(j, j) and a((j+1):m, j-1) = l((j+1):m, j)
                 if( k>1_ilp ) then
                    alpha = -a( j, k )
                    call stdlib_saxpy( m-j, alpha, a( j+1, k-1 ), 1_ilp,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_isamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1+1:m, i1) with a(i2, i1+1:m)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_sswap( i2-i1-1, a( i1+1, j1+i1-1 ), 1_ilp,a( i2, j1+i1 ), lda )
                              
                    ! swap a(i2+1:m, i1) with a(i2+1:m, i2)
                    if( i2<m )call stdlib_sswap( m-i2, a( i2+1, j1+i1-1 ), 1_ilp,a( i2+1, j1+i2-1 ), &
                              1_ilp )
                    ! swap a(i1, i1) with a(i2, i2)
                    piv = a( i1, j1+i1-1 )
                    a( i1, j1+i1-1 ) = a( i2, j1+i2-1 )
                    a( i2, j1+i2-1 ) = piv
                    ! swap h(i1, i1:j1) with h(i2, i2:j1)
                    call stdlib_sswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_sswap( i1-k1+1, a( i1, 1_ilp ), lda,a( i2, 1_ilp ), lda )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j+1, j) = t(j+1, j)
                 a( j+1, k ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j+1:m, j),
                    call stdlib_scopy( m-j, a( j+1, k+1 ), 1_ilp,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( j+1, k )/=zero ) then
                       alpha = one / a( j+1, k )
                       call stdlib_scopy( m-j-1, work( 3_ilp ), 1_ilp, a( j+2, k ), 1_ilp )
                       call stdlib_sscal( m-j-1, alpha, a( j+2, k ), 1_ilp )
                    else
                       call stdlib_slaset( 'FULL', m-j-1, 1_ilp, zero, zero,a( j+2, k ), lda )
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 30
              40 continue
           end if
           return
     end subroutine stdlib_slasyf_aa

     pure module subroutine stdlib_dlasyf_aa( uplo, j1, m, nb, a, lda, ipiv,h, ldh, work )
     !! DLATRF_AA factorizes a panel of a real symmetric matrix A using
     !! the Aasen's algorithm. The panel consists of a set of NB rows of A
     !! when UPLO is U, or a set of NB columns when UPLO is L.
     !! In order to factorize the panel, the Aasen's algorithm requires the
     !! last row, or column, of the previous panel. The first row, or column,
     !! of A is set to be the first row, or column, of an identity matrix,
     !! which is used to factorize the first panel.
     !! The resulting J-th row of U, or J-th column of L, is stored in the
     !! (J-1)-th row, or column, of A (without the unit diagonals), while
     !! the diagonal and subdiagonal of A are overwritten by those of T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: m, nb, j1, lda, ldh
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), h(ldh,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k, k1, i1, i2, mj
           real(dp) :: piv, alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           j = 1_ilp
           ! k1 is the first column of the panel to be factorized
           ! i.e.,  k1 is 2 for the first block column, and 1 for the rest of the blocks
           k1 = (2_ilp-j1)+1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              10 continue
              if ( j>min(m, nb) )go to 20
              ! k is the column to be factorized
               ! when being called from stdlib_dsytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j, j:m) - h(j:m, 1:(j-1)) * l(j1:(j-1), j),
               ! where h(j:m, j) has been initialized to be a(j, j:m)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_dgemv( 'NO TRANSPOSE', mj, j-k1,-one, h( j, k1 ), ldh,a( 1_ilp, j ), 1_ilp,&
                           one, h( j, j ), 1_ilp )
              end if
              ! copy h(i:m, i) into work
              call stdlib_dcopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j-1, j:m) * t(j-1,j),
                  ! where a(j-1, j) stores t(j-1, j) and a(j-2, j:m) stores u(j-1, j:m)
                 alpha = -a( k-1, j )
                 call stdlib_daxpy( mj, alpha, a( k-2, j ), lda, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( k, j ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l(j, (j+1):m)
                  ! where a(j, j) stores t(j, j) and a(j-1, (j+1):m) stores u(j, (j+1):m)
                 if( k>1_ilp ) then
                    alpha = -a( k, j )
                    call stdlib_daxpy( m-j, alpha, a( k-1, j+1 ), lda,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_idamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1, i1+1:m) with a(i1+1:m, i2)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_dswap( i2-i1-1, a( j1+i1-1, i1+1 ), lda,a( j1+i1, i2 ), 1_ilp )
                              
                    ! swap a(i1, i2+1:m) with a(i2, i2+1:m)
                    if( i2<m )call stdlib_dswap( m-i2, a( j1+i1-1, i2+1 ), lda,a( j1+i2-1, i2+1 ),&
                               lda )
                    ! swap a(i1, i1) with a(i2,i2)
                    piv = a( i1+j1-1, i1 )
                    a( j1+i1-1, i1 ) = a( j1+i2-1, i2 )
                    a( j1+i2-1, i2 ) = piv
                    ! swap h(i1, 1:j1) with h(i2, 1:j1)
                    call stdlib_dswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_dswap( i1-k1+1, a( 1_ilp, i1 ), 1_ilp,a( 1_ilp, i2 ), 1_ilp )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j, j+1) = t(j, j+1)
                 a( k, j+1 ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j:m, j),
                    call stdlib_dcopy( m-j, a( k+1, j+1 ), lda,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( k, j+1 )/=zero ) then
                       alpha = one / a( k, j+1 )
                       call stdlib_dcopy( m-j-1, work( 3_ilp ), 1_ilp, a( k, j+2 ), lda )
                       call stdlib_dscal( m-j-1, alpha, a( k, j+2 ), lda )
                    else
                       call stdlib_dlaset( 'FULL', 1_ilp, m-j-1, zero, zero,a( k, j+2 ), lda)
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 10
              20 continue
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              30 continue
              if( j>min( m, nb ) )go to 40
              ! k is the column to be factorized
               ! when being called from stdlib_dsytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j:m, j) - h(j:m, 1:(j-1)) * l(j, j1:(j-1))^t,
               ! where h(j:m, j) has been initialized to be a(j:m, j)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_dgemv( 'NO TRANSPOSE', mj, j-k1,-one, h( j, k1 ), ldh,a( j, 1_ilp ), lda,&
                           one, h( j, j ), 1_ilp )
              end if
              ! copy h(j:m, j) into work
              call stdlib_dcopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j:m, j-1) * t(j-1,j),
                  ! where a(j-1, j) = t(j-1, j) and a(j, j-2) = l(j, j-1)
                 alpha = -a( j, k-1 )
                 call stdlib_daxpy( mj, alpha, a( j, k-2 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( j, k ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l((j+1):m, j)
                  ! where a(j, j) = t(j, j) and a((j+1):m, j-1) = l((j+1):m, j)
                 if( k>1_ilp ) then
                    alpha = -a( j, k )
                    call stdlib_daxpy( m-j, alpha, a( j+1, k-1 ), 1_ilp,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_idamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1+1:m, i1) with a(i2, i1+1:m)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_dswap( i2-i1-1, a( i1+1, j1+i1-1 ), 1_ilp,a( i2, j1+i1 ), lda )
                              
                    ! swap a(i2+1:m, i1) with a(i2+1:m, i2)
                    if( i2<m )call stdlib_dswap( m-i2, a( i2+1, j1+i1-1 ), 1_ilp,a( i2+1, j1+i2-1 ), &
                              1_ilp )
                    ! swap a(i1, i1) with a(i2, i2)
                    piv = a( i1, j1+i1-1 )
                    a( i1, j1+i1-1 ) = a( i2, j1+i2-1 )
                    a( i2, j1+i2-1 ) = piv
                    ! swap h(i1, i1:j1) with h(i2, i2:j1)
                    call stdlib_dswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_dswap( i1-k1+1, a( i1, 1_ilp ), lda,a( i2, 1_ilp ), lda )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j+1, j) = t(j+1, j)
                 a( j+1, k ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j+1:m, j),
                    call stdlib_dcopy( m-j, a( j+1, k+1 ), 1_ilp,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( j+1, k )/=zero ) then
                       alpha = one / a( j+1, k )
                       call stdlib_dcopy( m-j-1, work( 3_ilp ), 1_ilp, a( j+2, k ), 1_ilp )
                       call stdlib_dscal( m-j-1, alpha, a( j+2, k ), 1_ilp )
                    else
                       call stdlib_dlaset( 'FULL', m-j-1, 1_ilp, zero, zero,a( j+2, k ), lda )
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 30
              40 continue
           end if
           return
     end subroutine stdlib_dlasyf_aa


     pure module subroutine stdlib_clasyf_aa( uplo, j1, m, nb, a, lda, ipiv,h, ldh, work )
     !! DLATRF_AA factorizes a panel of a complex symmetric matrix A using
     !! the Aasen's algorithm. The panel consists of a set of NB rows of A
     !! when UPLO is U, or a set of NB columns when UPLO is L.
     !! In order to factorize the panel, the Aasen's algorithm requires the
     !! last row, or column, of the previous panel. The first row, or column,
     !! of A is set to be the first row, or column, of an identity matrix,
     !! which is used to factorize the first panel.
     !! The resulting J-th row of U, or J-th column of L, is stored in the
     !! (J-1)-th row, or column, of A (without the unit diagonals), while
     !! the diagonal and subdiagonal of A are overwritten by those of T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: m, nb, j1, lda, ldh
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), h(ldh,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k, k1, i1, i2, mj
           complex(sp) :: piv, alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           j = 1_ilp
           ! k1 is the first column of the panel to be factorized
           ! i.e.,  k1 is 2 for the first block column, and 1 for the rest of the blocks
           k1 = (2_ilp-j1)+1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              10 continue
              if ( j>min(m, nb) )go to 20
              ! k is the column to be factorized
               ! when being called from stdlib_csytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j, j:m) - h(j:m, 1:(j-1)) * l(j1:(j-1), j),
               ! where h(j:m, j) has been initialized to be a(j, j:m)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_cgemv( 'NO TRANSPOSE', mj, j-k1,-cone, h( j, k1 ), ldh,a( 1_ilp, j ), 1_ilp,&
                           cone, h( j, j ), 1_ilp )
              end if
              ! copy h(i:m, i) into work
              call stdlib_ccopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j-1, j:m) * t(j-1,j),
                  ! where a(j-1, j) stores t(j-1, j) and a(j-2, j:m) stores u(j-1, j:m)
                 alpha = -a( k-1, j )
                 call stdlib_caxpy( mj, alpha, a( k-2, j ), lda, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( k, j ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l(j, (j+1):m)
                  ! where a(j, j) stores t(j, j) and a(j-1, (j+1):m) stores u(j, (j+1):m)
                 if( k>1_ilp ) then
                    alpha = -a( k, j )
                    call stdlib_caxpy( m-j, alpha, a( k-1, j+1 ), lda,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_icamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1, i1+1:m) with a(i1+1:m, i2)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_cswap( i2-i1-1, a( j1+i1-1, i1+1 ), lda,a( j1+i1, i2 ), 1_ilp )
                              
                    ! swap a(i1, i2+1:m) with a(i2, i2+1:m)
                    if( i2<m )call stdlib_cswap( m-i2, a( j1+i1-1, i2+1 ), lda,a( j1+i2-1, i2+1 ),&
                               lda )
                    ! swap a(i1, i1) with a(i2,i2)
                    piv = a( i1+j1-1, i1 )
                    a( j1+i1-1, i1 ) = a( j1+i2-1, i2 )
                    a( j1+i2-1, i2 ) = piv
                    ! swap h(i1, 1:j1) with h(i2, 1:j1)
                    call stdlib_cswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_cswap( i1-k1+1, a( 1_ilp, i1 ), 1_ilp,a( 1_ilp, i2 ), 1_ilp )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j, j+1) = t(j, j+1)
                 a( k, j+1 ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j:m, j),
                    call stdlib_ccopy( m-j, a( k+1, j+1 ), lda,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( k, j+1 )/=czero ) then
                       alpha = cone / a( k, j+1 )
                       call stdlib_ccopy( m-j-1, work( 3_ilp ), 1_ilp, a( k, j+2 ), lda )
                       call stdlib_cscal( m-j-1, alpha, a( k, j+2 ), lda )
                    else
                       call stdlib_claset( 'FULL', 1_ilp, m-j-1, czero, czero,a( k, j+2 ), lda)
                                 
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 10
              20 continue
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              30 continue
              if( j>min( m, nb ) )go to 40
              ! k is the column to be factorized
               ! when being called from stdlib_csytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j:m, j) - h(j:m, 1:(j-1)) * l(j, j1:(j-1))^t,
               ! where h(j:m, j) has been initialized to be a(j:m, j)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_cgemv( 'NO TRANSPOSE', mj, j-k1,-cone, h( j, k1 ), ldh,a( j, 1_ilp ), &
                           lda,cone, h( j, j ), 1_ilp )
              end if
              ! copy h(j:m, j) into work
              call stdlib_ccopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j:m, j-1) * t(j-1,j),
                  ! where a(j-1, j) = t(j-1, j) and a(j, j-2) = l(j, j-1)
                 alpha = -a( j, k-1 )
                 call stdlib_caxpy( mj, alpha, a( j, k-2 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( j, k ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l((j+1):m, j)
                  ! where a(j, j) = t(j, j) and a((j+1):m, j-1) = l((j+1):m, j)
                 if( k>1_ilp ) then
                    alpha = -a( j, k )
                    call stdlib_caxpy( m-j, alpha, a( j+1, k-1 ), 1_ilp,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_icamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1+1:m, i1) with a(i2, i1+1:m)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_cswap( i2-i1-1, a( i1+1, j1+i1-1 ), 1_ilp,a( i2, j1+i1 ), lda )
                              
                    ! swap a(i2+1:m, i1) with a(i2+1:m, i2)
                    if( i2<m )call stdlib_cswap( m-i2, a( i2+1, j1+i1-1 ), 1_ilp,a( i2+1, j1+i2-1 ), &
                              1_ilp )
                    ! swap a(i1, i1) with a(i2, i2)
                    piv = a( i1, j1+i1-1 )
                    a( i1, j1+i1-1 ) = a( i2, j1+i2-1 )
                    a( i2, j1+i2-1 ) = piv
                    ! swap h(i1, i1:j1) with h(i2, i2:j1)
                    call stdlib_cswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_cswap( i1-k1+1, a( i1, 1_ilp ), lda,a( i2, 1_ilp ), lda )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j+1, j) = t(j+1, j)
                 a( j+1, k ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j+1:m, j),
                    call stdlib_ccopy( m-j, a( j+1, k+1 ), 1_ilp,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( j+1, k )/=czero ) then
                       alpha = cone / a( j+1, k )
                       call stdlib_ccopy( m-j-1, work( 3_ilp ), 1_ilp, a( j+2, k ), 1_ilp )
                       call stdlib_cscal( m-j-1, alpha, a( j+2, k ), 1_ilp )
                    else
                       call stdlib_claset( 'FULL', m-j-1, 1_ilp, czero, czero,a( j+2, k ), lda )
                                 
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 30
              40 continue
           end if
           return
     end subroutine stdlib_clasyf_aa

     pure module subroutine stdlib_zlasyf_aa( uplo, j1, m, nb, a, lda, ipiv,h, ldh, work )
     !! DLATRF_AA factorizes a panel of a complex symmetric matrix A using
     !! the Aasen's algorithm. The panel consists of a set of NB rows of A
     !! when UPLO is U, or a set of NB columns when UPLO is L.
     !! In order to factorize the panel, the Aasen's algorithm requires the
     !! last row, or column, of the previous panel. The first row, or column,
     !! of A is set to be the first row, or column, of an identity matrix,
     !! which is used to factorize the first panel.
     !! The resulting J-th row of U, or J-th column of L, is stored in the
     !! (J-1)-th row, or column, of A (without the unit diagonals), while
     !! the diagonal and subdiagonal of A are overwritten by those of T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: m, nb, j1, lda, ldh
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), h(ldh,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k, k1, i1, i2, mj
           complex(dp) :: piv, alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           j = 1_ilp
           ! k1 is the first column of the panel to be factorized
           ! i.e.,  k1 is 2 for the first block column, and 1 for the rest of the blocks
           k1 = (2_ilp-j1)+1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! .....................................................
              ! factorize a as u**t*d*u using the upper triangle of a
              ! .....................................................
              10 continue
              if ( j>min(m, nb) )go to 20
              ! k is the column to be factorized
               ! when being called from stdlib_zsytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j, j:m) - h(j:m, 1:(j-1)) * l(j1:(j-1), j),
               ! where h(j:m, j) has been initialized to be a(j, j:m)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_zgemv( 'NO TRANSPOSE', mj, j-k1,-cone, h( j, k1 ), ldh,a( 1_ilp, j ), 1_ilp,&
                           cone, h( j, j ), 1_ilp )
              end if
              ! copy h(i:m, i) into work
              call stdlib_zcopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j-1, j:m) * t(j-1,j),
                  ! where a(j-1, j) stores t(j-1, j) and a(j-2, j:m) stores u(j-1, j:m)
                 alpha = -a( k-1, j )
                 call stdlib_zaxpy( mj, alpha, a( k-2, j ), lda, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( k, j ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l(j, (j+1):m)
                  ! where a(j, j) stores t(j, j) and a(j-1, (j+1):m) stores u(j, (j+1):m)
                 if( k>1_ilp ) then
                    alpha = -a( k, j )
                    call stdlib_zaxpy( m-j, alpha, a( k-1, j+1 ), lda,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_izamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1, i1+1:m) with a(i1+1:m, i2)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_zswap( i2-i1-1, a( j1+i1-1, i1+1 ), lda,a( j1+i1, i2 ), 1_ilp )
                              
                    ! swap a(i1, i2+1:m) with a(i2, i2+1:m)
                    if( i2<m )call stdlib_zswap( m-i2, a( j1+i1-1, i2+1 ), lda,a( j1+i2-1, i2+1 ),&
                               lda )
                    ! swap a(i1, i1) with a(i2,i2)
                    piv = a( i1+j1-1, i1 )
                    a( j1+i1-1, i1 ) = a( j1+i2-1, i2 )
                    a( j1+i2-1, i2 ) = piv
                    ! swap h(i1, 1:j1) with h(i2, 1:j1)
                    call stdlib_zswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_zswap( i1-k1+1, a( 1_ilp, i1 ), 1_ilp,a( 1_ilp, i2 ), 1_ilp )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j, j+1) = t(j, j+1)
                 a( k, j+1 ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j:m, j),
                    call stdlib_zcopy( m-j, a( k+1, j+1 ), lda,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( k, j+1 )/=czero ) then
                       alpha = cone / a( k, j+1 )
                       call stdlib_zcopy( m-j-1, work( 3_ilp ), 1_ilp, a( k, j+2 ), lda )
                       call stdlib_zscal( m-j-1, alpha, a( k, j+2 ), lda )
                    else
                       call stdlib_zlaset( 'FULL', 1_ilp, m-j-1, czero, czero,a( k, j+2 ), lda)
                                 
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 10
              20 continue
           else
              ! .....................................................
              ! factorize a as l*d*l**t using the lower triangle of a
              ! .....................................................
              30 continue
              if( j>min( m, nb ) )go to 40
              ! k is the column to be factorized
               ! when being called from stdlib_zsytrf_aa,
               ! > for the first block column, j1 is 1, hence j1+j-1 is j,
               ! > for the rest of the columns, j1 is 2, and j1+j-1 is j+1,
              k = j1+j-1
              if( j==m ) then
                  ! only need to compute t(j, j)
                  mj = 1_ilp
              else
                  mj = m-j+1
              end if
              ! h(j:m, j) := a(j:m, j) - h(j:m, 1:(j-1)) * l(j, j1:(j-1))^t,
               ! where h(j:m, j) has been initialized to be a(j:m, j)
              if( k>2_ilp ) then
              ! k is the column to be factorized
               ! > for the first block column, k is j, skipping the first two
                 ! columns
               ! > for the rest of the columns, k is j+1, skipping only the
                 ! first column
                 call stdlib_zgemv( 'NO TRANSPOSE', mj, j-k1,-cone, h( j, k1 ), ldh,a( j, 1_ilp ), &
                           lda,cone, h( j, j ), 1_ilp )
              end if
              ! copy h(j:m, j) into work
              call stdlib_zcopy( mj, h( j, j ), 1_ilp, work( 1_ilp ), 1_ilp )
              if( j>k1 ) then
                 ! compute work := work - l(j:m, j-1) * t(j-1,j),
                  ! where a(j-1, j) = t(j-1, j) and a(j, j-2) = l(j, j-1)
                 alpha = -a( j, k-1 )
                 call stdlib_zaxpy( mj, alpha, a( j, k-2 ), 1_ilp, work( 1_ilp ), 1_ilp )
              end if
              ! set a(j, j) = t(j, j)
              a( j, k ) = work( 1_ilp )
              if( j<m ) then
                 ! compute work(2:m) = t(j, j) l((j+1):m, j)
                  ! where a(j, j) = t(j, j) and a((j+1):m, j-1) = l((j+1):m, j)
                 if( k>1_ilp ) then
                    alpha = -a( j, k )
                    call stdlib_zaxpy( m-j, alpha, a( j+1, k-1 ), 1_ilp,work( 2_ilp ), 1_ilp )
                 endif
                 ! find max(|work(2:m)|)
                 i2 = stdlib_izamax( m-j, work( 2_ilp ), 1_ilp ) + 1_ilp
                 piv = work( i2 )
                 ! apply symmetric pivot
                 if( (i2/=2_ilp) .and. (piv/=0_ilp) ) then
                    ! swap work(i1) and work(i2)
                    i1 = 2_ilp
                    work( i2 ) = work( i1 )
                    work( i1 ) = piv
                    ! swap a(i1+1:m, i1) with a(i2, i1+1:m)
                    i1 = i1+j-1
                    i2 = i2+j-1
                    call stdlib_zswap( i2-i1-1, a( i1+1, j1+i1-1 ), 1_ilp,a( i2, j1+i1 ), lda )
                              
                    ! swap a(i2+1:m, i1) with a(i2+1:m, i2)
                    if( i2<m )call stdlib_zswap( m-i2, a( i2+1, j1+i1-1 ), 1_ilp,a( i2+1, j1+i2-1 ), &
                              1_ilp )
                    ! swap a(i1, i1) with a(i2, i2)
                    piv = a( i1, j1+i1-1 )
                    a( i1, j1+i1-1 ) = a( i2, j1+i2-1 )
                    a( i2, j1+i2-1 ) = piv
                    ! swap h(i1, i1:j1) with h(i2, i2:j1)
                    call stdlib_zswap( i1-1, h( i1, 1_ilp ), ldh, h( i2, 1_ilp ), ldh )
                    ipiv( i1 ) = i2
                    if( i1>(k1-1) ) then
                       ! swap l(1:i1-1, i1) with l(1:i1-1, i2),
                        ! skipping the first column
                       call stdlib_zswap( i1-k1+1, a( i1, 1_ilp ), lda,a( i2, 1_ilp ), lda )
                    end if
                 else
                    ipiv( j+1 ) = j+1
                 endif
                 ! set a(j+1, j) = t(j+1, j)
                 a( j+1, k ) = work( 2_ilp )
                 if( j<nb ) then
                    ! copy a(j+1:m, j+1) into h(j+1:m, j),
                    call stdlib_zcopy( m-j, a( j+1, k+1 ), 1_ilp,h( j+1, j+1 ), 1_ilp )
                 end if
                 ! compute l(j+2, j+1) = work( 3:m ) / t(j, j+1),
                  ! where a(j, j+1) = t(j, j+1) and a(j+2:m, j) = l(j+2:m, j+1)
                 if( j<(m-1) ) then
                    if( a( j+1, k )/=czero ) then
                       alpha = cone / a( j+1, k )
                       call stdlib_zcopy( m-j-1, work( 3_ilp ), 1_ilp, a( j+2, k ), 1_ilp )
                       call stdlib_zscal( m-j-1, alpha, a( j+2, k ), 1_ilp )
                    else
                       call stdlib_zlaset( 'FULL', m-j-1, 1_ilp, czero, czero,a( j+2, k ), lda )
                                 
                    end if
                 end if
              end if
              j = j + 1_ilp
              go to 30
              40 continue
           end if
           return
     end subroutine stdlib_zlasyf_aa




     pure module subroutine stdlib_ssytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, lwork, info )
     !! SSYTRS_AA solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U**T*T*U or
     !! A = L*T*L**T computed by SSYTRF_AA.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, nrhs, lda, ldb, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           logical(lk) :: lquery, upper
           integer(ilp) :: k, kp, lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n-2 ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRS_AA', -info )
              return
           else if( lquery ) then
              lwkopt = (3_ilp*n-2)
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u**t*t*u.
              ! 1) forward substitution with u**t
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 k = 1_ilp
                 do while ( k<=n )
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                    k = k + 1_ilp
                 end do
                 ! compute u**t \ b -> b    [ (u**t \p**t * b) ]
                 call stdlib_strsm( 'L', 'U', 'T', 'U', n-1, nrhs, one, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (u**t \p**t * b) ]
              call stdlib_slacpy( 'F', 1_ilp, n, a(1_ilp, 1_ilp), lda+1, work(n), 1_ilp)
              if( n>1_ilp ) then
                  call stdlib_slacpy( 'F', 1_ilp, n-1, a(1_ilp, 2_ilp), lda+1, work(1_ilp), 1_ilp)
                  call stdlib_slacpy( 'F', 1_ilp, n-1, a(1_ilp, 2_ilp), lda+1, work(2_ilp*n), 1_ilp)
              end if
              call stdlib_sgtsv(n, nrhs, work(1_ilp), work(n), work(2_ilp*n), b, ldb,info)
              ! 3) backward substitution with u
              if( n>1_ilp ) then
                 ! compute u \ b -> b   [ u \ (t \ (u**t \p**t * b) ) ]
                 call stdlib_strsm( 'L', 'U', 'N', 'U', n-1, nrhs, one, a( 1_ilp, 2_ilp ),lda, b(2_ilp, 1_ilp), &
                           ldb)
                 ! pivot, p * b -> b  [ p * (u \ (t \ (u**t \p**t * b) )) ]
                 k = n
                 do while ( k>=1 )
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                    k = k - 1_ilp
                 end do
              end if
           else
              ! solve a*x = b, where a = l*t*l**t.
              ! 1) forward substitution with l
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 k = 1_ilp
                 do while ( k<=n )
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                    k = k + 1_ilp
                 end do
                 ! compute l \ b -> b    [ (l \p**t * b) ]
                 call stdlib_strsm( 'L', 'L', 'N', 'U', n-1, nrhs, one, a( 2_ilp, 1_ilp),lda, b(2_ilp, 1_ilp), &
                           ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (l \p**t * b) ]
              call stdlib_slacpy( 'F', 1_ilp, n, a(1_ilp, 1_ilp), lda+1, work(n), 1_ilp)
              if( n>1_ilp ) then
                  call stdlib_slacpy( 'F', 1_ilp, n-1, a(2_ilp, 1_ilp), lda+1, work(1_ilp), 1_ilp)
                  call stdlib_slacpy( 'F', 1_ilp, n-1, a(2_ilp, 1_ilp), lda+1, work(2_ilp*n), 1_ilp)
              end if
              call stdlib_sgtsv(n, nrhs, work(1_ilp), work(n), work(2_ilp*n), b, ldb,info)
              ! 3) backward substitution with l**t
              if( n>1_ilp ) then
                 ! compute l**t \ b -> b   [ l**t \ (t \ (l \p**t * b) ) ]
                 call stdlib_strsm( 'L', 'L', 'T', 'U', n-1, nrhs, one, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
                 ! pivot, p * b -> b  [ p * (l**t \ (t \ (l \p**t * b) )) ]
                 k = n
                 do while ( k>=1 )
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                    k = k - 1_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_ssytrs_aa

     pure module subroutine stdlib_dsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, lwork, info )
     !! DSYTRS_AA solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U**T*T*U or
     !! A = L*T*L**T computed by DSYTRF_AA.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, nrhs, lda, ldb, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           logical(lk) :: lquery, upper
           integer(ilp) :: k, kp, lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n-2 ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRS_AA', -info )
              return
           else if( lquery ) then
              lwkopt = (3_ilp*n-2)
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u**t*t*u.
              ! 1) forward substitution with u**t
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute u**t \ b -> b    [ (u**t \p**t * b) ]
                 call stdlib_dtrsm('L', 'U', 'T', 'U', n-1, nrhs, one, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (u**t \p**t * b) ]
              call stdlib_dlacpy( 'F', 1_ilp, n, a( 1_ilp, 1_ilp ), lda+1, work( n ), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_dlacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_dlacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_dgtsv( n, nrhs, work( 1_ilp ), work( n ), work( 2_ilp*n ), b, ldb,info )
              ! 3) backward substitution with u
              if( n>1_ilp ) then
                 ! compute u \ b -> b   [ u \ (t \ (u**t \p**t * b) ) ]
                 call stdlib_dtrsm( 'L', 'U', 'N', 'U', n-1, nrhs, one, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
                 ! pivot, p * b -> b  [ p * (u \ (t \ (u**t \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           else
              ! solve a*x = b, where a = l*t*l**t.
              ! 1) forward substitution with l
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute l \ b -> b    [ (l \p**t * b) ]
                 call stdlib_dtrsm( 'L', 'L', 'N', 'U', n-1, nrhs, one, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (l \p**t * b) ]
              call stdlib_dlacpy( 'F', 1_ilp, n, a(1_ilp, 1_ilp), lda+1, work(n), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_dlacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_dlacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_dgtsv( n, nrhs, work( 1_ilp ), work(n), work( 2_ilp*n ), b, ldb,info)
              ! 3) backward substitution with l**t
              if( n>1_ilp ) then
                 ! compute (l**t \ b) -> b   [ l**t \ (t \ (l \p**t * b) ) ]
                 call stdlib_dtrsm( 'L', 'L', 'T', 'U', n-1, nrhs, one, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ), &
                           ldb)
                 ! pivot, p * b -> b  [ p * (l**t \ (t \ (l \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_dsytrs_aa


     pure module subroutine stdlib_csytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, lwork, info )
     !! CSYTRS_AA solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U**T*T*U or
     !! A = L*T*L**T computed by CSYTRF_AA.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, nrhs, lda, ldb, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           logical(lk) :: lquery, upper
           integer(ilp) :: k, kp, lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n-2 ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRS_AA', -info )
              return
           else if( lquery ) then
              lwkopt = (3_ilp*n-2)
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u**t*t*u.
              ! 1) forward substitution with u**t
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute u**t \ b -> b    [ (u**t \p**t * b) ]
                 call stdlib_ctrsm( 'L', 'U', 'T', 'U', n-1, nrhs, cone, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (u**t \p**t * b) ]
              call stdlib_clacpy( 'F', 1_ilp, n, a( 1_ilp, 1_ilp ), lda+1, work( n ), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_clacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_clacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_cgtsv( n, nrhs, work( 1_ilp ), work( n ), work( 2_ilp*n ), b, ldb,info )
              ! 3) backward substitution with u
              if( n>1_ilp ) then
                 ! compute u \ b -> b   [ u \ (t \ (u**t \p**t * b) ) ]
                 call stdlib_ctrsm( 'L', 'U', 'N', 'U', n-1, nrhs, cone, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
                 ! pivot, p * b -> b  [ p * (u**t \ (t \ (u \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           else
              ! solve a*x = b, where a = l*t*l**t.
              ! 1) forward substitution with l
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute l \ b -> b    [ (l \p**t * b) ]
                 call stdlib_ctrsm( 'L', 'L', 'N', 'U', n-1, nrhs, cone, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (l \p**t * b) ]
              call stdlib_clacpy( 'F', 1_ilp, n, a(1_ilp, 1_ilp), lda+1, work(n), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_clacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_clacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_cgtsv( n, nrhs, work( 1_ilp ), work(n), work( 2_ilp*n ), b, ldb,info)
              ! 3) backward substitution with l**t
              if( n>1_ilp ) then
                 ! compute (l**t \ b) -> b   [ l**t \ (t \ (l \p**t * b) ) ]
                 call stdlib_ctrsm( 'L', 'L', 'T', 'U', n-1, nrhs, cone, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
                 ! pivot, p * b -> b  [ p * (l**t \ (t \ (l \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_csytrs_aa

     pure module subroutine stdlib_zsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, lwork, info )
     !! ZSYTRS_AA solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U**T*T*U or
     !! A = L*T*L**T computed by ZSYTRF_AA.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, nrhs, lda, ldb, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           logical(lk) :: lquery, upper
           integer(ilp) :: k, kp, lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n-2 ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRS_AA', -info )
              return
           else if( lquery ) then
              lwkopt = (3_ilp*n-2)
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u**t*t*u.
              ! 1) forward substitution with u**t
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute u**t \ b -> b    [ (u**t \p**t * b) ]
                 call stdlib_ztrsm( 'L', 'U', 'T', 'U', n-1, nrhs, cone, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (u**t \p**t * b) ]
              call stdlib_zlacpy( 'F', 1_ilp, n, a( 1_ilp, 1_ilp ), lda+1, work( n ), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_zlacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_zlacpy( 'F', 1_ilp, n-1, a( 1_ilp, 2_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_zgtsv( n, nrhs, work( 1_ilp ), work( n ), work( 2_ilp*n ), b, ldb,info )
              ! 3) backward substitution with u
              if( n>1_ilp ) then
                 ! compute u \ b -> b   [ u \ (t \ (u**t \p**t * b) ) ]
                 call stdlib_ztrsm( 'L', 'U', 'N', 'U', n-1, nrhs, cone, a( 1_ilp, 2_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
                 ! pivot, p * b -> b  [ p * (u \ (t \ (u**t \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           else
              ! solve a*x = b, where a = l*t*l**t.
              ! 1) forward substitution with l
              if( n>1_ilp ) then
                 ! pivot, p**t * b -> b
                 do k = 1, n
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
                 ! compute l \ b -> b    [ (l \p**t * b) ]
                 call stdlib_ztrsm( 'L', 'L', 'N', 'U', n-1, nrhs, cone, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
              end if
              ! 2) solve with triangular matrix t
              ! compute t \ b -> b   [ t \ (l \p**t * b) ]
              call stdlib_zlacpy( 'F', 1_ilp, n, a(1_ilp, 1_ilp), lda+1, work(n), 1_ilp)
              if( n>1_ilp ) then
                 call stdlib_zlacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 1_ilp ), 1_ilp )
                 call stdlib_zlacpy( 'F', 1_ilp, n-1, a( 2_ilp, 1_ilp ), lda+1, work( 2_ilp*n ), 1_ilp )
              end if
              call stdlib_zgtsv( n, nrhs, work( 1_ilp ), work(n), work( 2_ilp*n ), b, ldb,info)
              ! 3) backward substitution with l**t
              if( n>1_ilp ) then
                 ! compute (l**t \ b) -> b   [ l**t \ (t \ (l \p**t * b) ) ]
                 call stdlib_ztrsm( 'L', 'L', 'T', 'U', n-1, nrhs, cone, a( 2_ilp, 1_ilp ),lda, b( 2_ilp, 1_ilp ),&
                            ldb)
                 ! pivot, p * b -> b  [ p * (l**t \ (t \ (l \p**t * b) )) ]
                 do k = n, 1, -1
                    kp = ipiv( k )
                    if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end do
              end if
           end if
           return
     end subroutine stdlib_zsytrs_aa



end submodule stdlib_lapack_solve_ldl_comp2
