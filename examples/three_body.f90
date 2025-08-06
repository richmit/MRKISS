! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      three_body.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Arenstorf's classical periodic orbit three body problem.@EOL
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS/
!! @copyright 
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
!!  
!!  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
!!  conditions are met:
!!  
!!  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following
!!     disclaimer.
!!  
!!  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following
!!     disclaimer in the documentation and/or other materials provided with the distribution.
!!  
!!  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products
!!     derived from this software without specific prior written permission.
!!  
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!!  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!!  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
!!  OF THE POSSIBILITY OF SUCH DAMAGE.
!!  @endparblock
!! @filedetails   
!!
!!  This is one of my favorite IVP example problems.
!!
!!  The earth is at \((0, 0)\).  The moon is in a circular orbit of the earth at a distance of \(1\).  At the start of the
!!  simulation, the moon is located at \((1,0)\).  We have a small satellite starting out at \((0.99400, 0.0)\) with an initial
!!  velocity of \((0.0, -2.0015851063790825224)\).  With these initial conditions, the satellite will return to it's initial
!!  position after completing a three lobed orbit at \(t=17.06521656015796\).
!!
!!  This example demonstrates:
!!    - Fixed step size in t space using steps_fixed_stab_wt() 
!!    - Fixed step size in y space using steps_condy_stab_wt() 
!!    - Typical adaptive step use case via steps_adapt_etab_wt()
!!    - Exit after a maximum t value is reached via stepp_o = sp_max_t()
!!    - Rough scaling of t_delta in order to limit length of a sub-vector of y_delta via stepp_o = sp_sloppy_y_delta_len_max()
!!    - Find intersection with moon orbit via stepp_o = sp_cross_moon() & sdf_o = sdf_cross_moon() 
!!
!!  Interesting cases:
!!    - fiveLobe   y_iv: [0.87978_rk, 0.0_rk, 0.0_rk, -0.3797000000000000000_rk] t_iv: 0.0_rk t_end: 19.14045706162071_rk
!!    - threeLobe  y_iv: [0.99400_rk, 0.0_rk, 0.0_rk, -2.0015851063790825224_rk] t_iv: 0.0_rk t_end: 17.06521656015796_rk
!!
!!  References:
!!    Arenstorf (1963); Periodic Solutions of the Restricted Three Body Problem Representing Analytic Continuations 
!!        of Keplerian Elliptic Motions; American J. of Math. 85 (1); p27-35; zotero://select/items/0_JPDZ7KNL
!!    Butcher (2008); Numerical Methods for Ordinary Differential Equations; p29-30; zotero://select/items/0_8V2GY73E
!!    Butcher (2016); Numerical Methods for Ordinary Differential Equations; p28-31; zotero://select/items/0_V7UTIRPT
!!    Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p129-130; 
!!        zotero://select/items/0_VLZWN2CT
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program three_body
  use, intrinsic :: iso_fortran_env,                only: output_unit, error_unit
  use            :: mrkiss_config,                  only: rk, ik, bk, t_delta_tiny, istats_size
  use            :: mrkiss_solvers_wt,              only: steps_fixed_stab_wt, steps_condy_stab_wt, steps_adapt_etab_wt, steps_sloppy_condy_stab_wt
  use            :: mrkiss_utils,                   only: print_solution, seq, interpolate_solution
  use            :: mrkiss_eerk_verner_9_8,         only: a, b1, b2, c, p1, p2
  use            :: mrkiss_eerk_dormand_prince_5_4, only: dpa=>a, dpb=>b1, dpc=>c

  implicit none

  integer,          parameter :: deq_dim       = 4
  integer,          parameter :: num_points    = 4000
  real(kind=rk),    parameter :: t_iv          = 0.0_rk
  real(kind=rk),    parameter :: t_end         = 17.06521656015796_rk
  real(kind=rk),    parameter :: path_length   = 10.7068_rk 
  real(kind=rk),    parameter :: y_iv(deq_dim) = [0.994_rk, 0.0_rk, 0.0_rk, -2.0015851063790825224_rk]
  real(kind=rk),    parameter :: param(1)      = [1.0_rk / 81.45_rk]
  real(kind=rk),    parameter :: t_delta       = 17.06521656015796d0 / (num_points - 1 )

  real(kind=rk)               :: solution(1+2*deq_dim, num_points), isolution(1+deq_dim, num_points)
  integer(kind=ik)            :: status, istats(istats_size)
  integer                     :: c_beg, c_end, c_rate

  real(kind=rk)               :: dy(deq_dim)

  call system_clock(count_rate=c_rate)

  ! BEGIN: steps_fixed_stab_wt
  call system_clock(c_beg)
  call steps_fixed_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, c, t_end_o=t_end)
  call system_clock(c_end)
  print '(a)',       "Fixed t_delta run V(9): "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  call print_solution(status, solution, filename_o="tree_body_steps_fixed_stab_wt.csv", end_o=istats(1))
  ! END: steps_fixed_stab_wt


  ! BEGIN: steps_fixed_stab_wt-dp
  call system_clock(c_beg)
  call steps_fixed_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, dpa, dpb, dpc, t_end_o=t_end)
  call system_clock(c_end)
  print '(a)',       "Fixed t_delta run DP(5): "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  call print_solution(status, solution, filename_o="steps_fixed_stab_wt-dp.csv", end_o=istats(1))
  ! END: steps_fixed_stab_wt-dp

    ! BEGIN: steps_condy_stab_wt
  call system_clock(c_beg)
  call steps_condy_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, c, 0.0034_rk, .01_rk, &
                           y_delta_len_idxs_o=[1,2], y_sol_len_max_o=path_length, y_delta_len_tol_o=1.0e-5_rk)
  call system_clock(c_end)
  print '(a)',       "Fixed y_delta run: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-len Adjust one_step calls: ", istats(3)
  print '(a,i10)',   "              bisection limits: ", istats(7)
  print '(a,i10)',   "           bad bisection start: ", istats(8)
  call print_solution(status, solution, filename_o="three_body_steps_condy_stab_wt.csv", end_o=istats(1))
  ! END: steps_condy_stab_wt

  ! BEGIN: steps_sloppy_condy_stab_wt
  call system_clock(c_beg)
  call steps_sloppy_condy_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, c, 0.0034_rk, .01_rk, &
                                  y_delta_len_idxs_o=[1,2], y_sol_len_max_o=path_length)
  call system_clock(c_end)
  print '(a)',       "Sloppy Fixed y_delta run: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-len Adjust one_step calls: ", istats(3)
  call print_solution(status, solution, filename_o="steps_sloppy_condy_stab_wt.csv", end_o=istats(1))
  ! END: steps_sloppy_condy_stab_wt

  ! BEGIN: steps_adapt_etab_wt-std
  call system_clock(c_beg)
  call steps_adapt_etab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end);
  call system_clock(c_end)
  print '(a)',       "Adaptive run: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-err Adjust one_step calls: ", istats(4)
  call print_solution(status, solution, filename_o="three_body_steps_adapt_etab_wt-std.csv", end_o=istats(1))
  ! END: steps_adapt_etab_wt-std

  ! BEGIN: steps_adapt_int
  call system_clock(c_beg)
  isolution = 0
  call seq(status, isolution(1,:), from_o=0.0_rk, to_o=t_end);            ! Create new t values
  call interpolate_solution(status, isolution, solution, end_o=istats(1)) ! Preform the interpolation
  call system_clock(c_end)
  print '(a)',       "Adaptive hermite interpolation run: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  call print_solution(status, isolution, filename_o="three_body_steps_adapt_std_interpolated.csv", sol_w_dy_o=.false._bk)

  call system_clock(c_beg)
  isolution = 0
  call seq(status, isolution(1,:), from_o=0.0_rk, to_o=t_end);
  ! Note we must provide y_dim_o because solution really contains dy.  
  call interpolate_solution(status, isolution, solution, end_o=istats(1), y_dim_o=deq_dim, sol_w_dy_o=.false._bk)
  call system_clock(c_end)
  print '(a)',       "Adaptive linear interpolation run: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  call print_solution(status, isolution, filename_o="three_body_steps_adapt_std_interpolated_lin.csv", sol_w_dy_o=.false._bk)
  ! END: steps_adapt_int

  ! BEGIN: steps_adapt_etab_wt-fix-delta-steps
  call system_clock(c_beg)
  call steps_adapt_etab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_sloppy_y_delta_len_max);
  call system_clock(c_end)
  print '(a)',       "Adaptive run w max y_delta length: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-err Adjust one_step calls: ", istats(4)
  print '(a,i10)',   "  stepp t_delta one_step calls: ", istats(5)
  call print_solution(status, solution, filename_o="three_body_steps_adapt_etab_wt-fix-delta-steps.csv", end_o=istats(1))
  ! END: steps_adapt_etab_wt-fix-delta-steps

  ! BEGIN: steps_adapt_etab_wt-pho-t-max
  call system_clock(c_beg)
  call steps_adapt_etab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_max_t);
  call system_clock(c_end)
  print '(a)',       "Adaptive run w max t: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-err Adjust one_step calls: ", istats(4)
  call print_solution(status, solution, filename_o="three_body_steps_adapt_etab_wt-pho-t-max.csv", end_o=istats(1))
  ! END: steps_adapt_etab_wt-pho-t-max

  ! BEGIN: steps_adapt_etab_wt-isct
  call system_clock(c_beg)
  call steps_adapt_etab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_cross_moon, sdf_o=sdf_cross_moon);
  call system_clock(c_end)
  print '(a)',       "Adaptive run w moon orbit hit: "
  print '(a,f10.3)', "                  Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                        Status: ", status
  print '(a,i10)',   "               Solution Points: ", istats(1)
  print '(a,i10)',   "          Total one_step calls: ", istats(2)
  print '(a,i10)',   "   y-err Adjust one_step calls: ", istats(4)
  print '(a,i10)',   "              bisection limits: ", istats(7)
  print '(a,i10)',   "           bad bisection start: ", istats(8)
  call print_solution(status, solution, filename_o="three_body_steps_adapt_etab_wt-isct.csv", end_o=istats(1))
  ! END: steps_adapt_etab_wt-isct

contains
  
  subroutine eq(status, dydt, t, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    ! Vars
    real(kind=rk) x1,x2,v1,v2,mu,s1,s2,s3,x22,s12,s32,bf1,bf2
    ! Compute dydt
    x1  = y(1)                   ! y(1)     = Position x coordinate
    x2  = y(2)                   ! y(2)     = Position y coordinate
    v1  = y(3)                   ! y(3)     = Velocity x coordinate
    v2  = y(4)                   ! y(3)     = Velocity y coordinate
    s1  = x1 + param(1) - 1.0_rk ! param(1) = mu
    s2  = 1.0_rk - param(1)
    s3  = x1 + param(1)
    x22 = x2**2
    s12 = s1**2
    s32 = s3**2
    bf1 = (x22 + s12)**(3.0_rk/2.0_rk)
    bf2 = (x22 + s32)**(3.0_rk/2.0_rk)
    if (abs(bf1) < 0.0e-15) then
       status = 1
       return
    end if
    if (abs(bf2) < 0.0e-15) then
       status = 2
       return
    end if
    dydt(1) = v1
    dydt(2) = v2
    dydt(3) =   2 * v2 + x1 - (param(1) * s1) / bf1 - (s2 * s3) / bf2
    dydt(4) =  -2 * v1 + x2 - (param(1) * x2) / bf1 - (s2 * x2) / bf2
    status = 0
  end subroutine eq
  
  ! BEGIN: steps_adapt_etab_wt-pho-t-max-stepp
  ! Example subroutine replicateing the functionality of t_max_o in steps_adapt_etab_wt().
  subroutine sp_max_t(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer(kind=ik), intent(out) :: status
    integer(kind=ik), intent(out) :: end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer(kind=ik), intent(out) :: sdf_flags
    integer(kind=ik), intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),    parameter   :: t_max = 6.2_rk
    status    = 0_ik
    sdf_flags = 0_ik
    new_t_delta = -1.0_rk
    if ( solution(1, pnt_idx-1) + t_delta > t_max) then
       end_run = 1_ik
    else
       end_run = 0_ik
    end if
  end subroutine sp_max_t
  ! END: steps_adapt_etab_wt-pho-t-max-stepp

  ! BEGIN: steps_adapt_etab_wt-fix-delta-stepp
  ! Example subroutine to adjust t_delta in an atempt to keep y_delta under a maximum value.
  ! It is sloppy because we assume t_delta is linearly proportional to y_delta_len
  subroutine sp_sloppy_y_delta_len_max(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer(kind=ik), intent(out) :: status, end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer(kind=ik), intent(out) :: sdf_flags
    integer(kind=ik), intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),      parameter :: y_delta_len_max = 0.1_rk
    integer,            parameter :: y_delta_len_idxs(2) = [1, 2]
    real(kind=rk)                 :: y_delta_len
    status    = 0_ik
    end_run   = 0_ik
    sdf_flags = 0_ik
    y_delta_len = norm2(y_delta(y_delta_len_idxs))
    if ( y_delta_len > y_delta_len_max) then
       new_t_delta = t_delta * y_delta_len_max / y_delta_len
    else
       new_t_delta = -1.0_rk
    end if
  end subroutine sp_sloppy_y_delta_len_max
  ! END: steps_adapt_etab_wt-fix-delta-stepp

  ! BEGIN: steps_adapt_etab_wt-isct-stepp
  ! Example subroutine to find the first intersection of the satellite path and the moon's orbit.  It works in conjunction with
  ! sdf_cross_moon().
  subroutine sp_cross_moon(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer(kind=ik), intent(out) :: status, end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer(kind=ik), intent(out) :: sdf_flags
    integer(kind=ik), intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),    parameter   :: eps = 0.0001_rk
    real(kind=rk)                 :: lp_d, cp_d    
    status      = 0_ik
    sdf_flags   = 0_ik
    end_run     = 0_ik
    new_t_delta = -1.0_rk
    if (solution(1, pnt_idx-1) > 0.2_rk) then
       cp_d = norm2(solution(2:3, pnt_idx-1)+y_delta(1:2))
       if ( abs(cp_d-1.0_rk)  < eps) then
          end_run   = 1_ik
       else
          lp_d = norm2(solution(2:3, pnt_idx-1))
          if ((min(lp_d, cp_d) < 1.0_rk) .and. (max(lp_d, cp_d) > 1.0_rk)) then
             sdf_flags = 1_ik
             end_run   = 1_ik
          end if
       end if
    end if
  end subroutine sp_cross_moon
  ! END: steps_adapt_etab_wt-isct-stepp

  ! BEGIN: steps_adapt_etab_wt-isct-sdf
  ! Example SDF subroutine to isolate a point on a solution segment that crosses the unit circle.
  subroutine sdf_cross_moon(status, dist, sdf_flags, t, y)
    use mrkiss_config, only: rk, ik
    implicit none
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dist
    integer(kind=ik), intent(in)  :: sdf_flags
    real(kind=rk),    intent(in)  :: t, y(:)
    status = 0_ik
    dist = 1.0_rk - norm2(y(1:2))
  end subroutine sdf_cross_moon
  ! END: steps_adapt_etab_wt-isct-sdf

end program three_body
