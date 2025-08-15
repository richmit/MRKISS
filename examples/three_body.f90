! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
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
!!  This is a dimensionless, restricted three gravitational body problem.  The Earth is stationary at the origin.  The Moon
!!  orbits the Earth at a radius of $1$ and is at @f$(1,0)@f$ at @f$t=0@f$.  The mass ratio is @f$\frac{100}{8145}@f$.  The
!!  position, @f$(x_1,x_2)@f$, and velocity, @f$(v_1,v_2)@f$, are governed by the following differential equation:
!!  
!!   @f[\begin{align*}
!!       \frac{\mathrm{d}v_1}{\mathrm{d}t} & = v_1 \\
!!       \frac{\mathrm{d}v_2}{\mathrm{d}t} & = v_2 \\
!!       \frac{\mathrm{d}x_1}{\mathrm{d}t} & =   2  v_2 + x_1 - \frac{\mu (x_1 + \mu -1)}{D_1} - \frac{(1 - \mu)  (x_1 + \mu)}{D_2} \\
!!       \frac{\mathrm{d}x_2}{\mathrm{d}t} & =  -2  v_1 + x_2 - \frac{\mu  x_2}{D_1} - \frac{(1 - \mu) x_2}{D_2} 
!!   \end{align*}@f]
!!  
!!  Where @f$D_1@f$ and @f$D_2@f$ are defined as:
!!  
!!   @f[\begin{align*}
!!       D_1 & = \sqrt{\left(x_2^2 + (x_1 + \mu - 1)^2\right)^3} \\
!!       D_2 & = \sqrt{\left(x_2^2 + (x_1 + \mu)\right)^3}         
!!   \end{align*}@f]
!!  
!!  We solve the equations with the following initial values:
!!  
!!   @f[\begin{align*}
!!      (x_1,x_2) &=  \left(\frac{497}{500}, 0\right) \\
!!      (v_1,v_2) & = (0, -2.0015851063790825224)  
!!   \end{align*}@f]
!!  
!!  Under these conditions the satellite will return to it's initial position after completing a three lobed orbit at
!!  @f$t=17.06521656015796@f$.
!!
!! Another interesting set of initial conditions leads to a five lobe orbit at @f$t=19.14045706162071@f$:
!!
!!   @f[\begin{align*}
!!      (x_1,x_2) &=  \left(0.87978, 0\right) \\
!!      (v_1,v_2) & = (0, -0.3797)  
!!   \end{align*}@f]
!!
!!  References:
!!   - Arenstorf (1963); Periodic Solutions of the Restricted Three Body Problem Representing Analytic Continuations 
!!       of Keplerian Elliptic Motions; American J. of Math. 85 (1); p27-35; zotero://select/items/0_JPDZ7KNL
!!   - Butcher (2008); Numerical Methods for Ordinary Differential Equations; p29-30; zotero://select/items/0_8V2GY73E
!!   - Butcher (2016); Numerical Methods for Ordinary Differential Equations; p28-31; zotero://select/items/0_V7UTIRPT
!!   - Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p129-130; 
!!       zotero://select/items/0_VLZWN2CT
!!
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program three_body
  use, intrinsic :: iso_fortran_env,                only: output_unit, error_unit
  use            :: mrkiss_config,                  only: rk, istats_size
  use            :: mrkiss_solvers_wt,              only: steps_fixed_stab, steps_condy_stab, steps_adapt_etab, &
                                                          steps_sloppy_condy_stab, interpolate_solution
  use            :: mrkiss_utils,                   only: print_solution, seq, print_istats, status_to_message
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

  real(kind=rk)               :: sol1(1+2*deq_dim, num_points), sol2(1+2*deq_dim, num_points)
  integer                     :: status, istats1(istats_size), istats2(istats_size)

  print '(a)', repeat('*', 120)
  print '(a)', "Fixed t_delta run V(9)"
  ! SS-BEGIN:steps_fixed_stab:
  call steps_fixed_stab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, c, t_end_o=t_end)
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="tree_body_steps_fixed_stab.csv", end_o=istats1(1))
  ! SS-END:steps_fixed_stab:

  print '(a)', repeat('*', 120)
  print '(a)', "Fixed t_delta run DP(5)"
  ! SS-BEGIN:steps_fixed_stab-dp:
  call steps_fixed_stab(status, istats1, sol1, eq, t_iv, y_iv, param, dpa, dpb, dpc, t_end_o=t_end)
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="steps_fixed_stab-dp.csv", end_o=istats1(1))
  ! SS-END:steps_fixed_stab-dp:

  print '(a)', repeat('*', 120)
  print '(a)', "Fixed y_delta run"
  ! SS-BEGIN:steps_condy_stab:
  call steps_condy_stab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, c, 0.0034_rk, .01_rk, &
                           y_delta_len_idxs_o=[1,2], y_sol_len_max_o=path_length, y_delta_len_tol_o=1.0e-5_rk)
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)

  call print_solution(status, sol1, filename_o="three_body_steps_condy_stab.csv", end_o=istats1(1))
  ! SS-END:steps_condy_stab:

  print '(a)', repeat('*', 120)
  print '(a)', "Sloppy Fixed y_delta run"
  ! SS-BEGIN:steps_sloppy_condy_stab:
  call steps_sloppy_condy_stab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, c, 0.0034_rk, .01_rk, &
                                  y_delta_len_idxs_o=[1,2], y_sol_len_max_o=path_length)
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="steps_sloppy_condy_stab.csv", end_o=istats1(1))
  ! SS-END:steps_sloppy_condy_stab:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive run"
  ! SS-BEGIN:steps_adapt_etab-std:
  call steps_adapt_etab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end);
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="three_body_steps_adapt_etab-std.csv", end_o=istats1(1))
  ! SS-END:steps_adapt_etab-std:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive hermite interpolation run"
  sol2 = 0
  ! SS-BEGIN:steps_adapt_int_hermite:
  call seq(status, sol2(1,:), from_o=0.0_rk, to_o=t_end);                                     ! Create new t values
  print '(a)', status_to_message(status)
  call interpolate_solution(status, istats2, sol2, sol1, eq, param, num_src_pts_o=istats1(1)) ! Preform the interpolation
  call print_solution(status, sol2, filename_o="three_body_steps_adapt_std_interpolated.csv")
  ! SS-END:steps_adapt_int_hermite:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive linear interpolation run"
  sol2 = 0
  ! SS-BEGIN:steps_adapt_int_linear:
  call seq(status, sol2(1,:), from_o=0.0_rk, to_o=t_end);
  call interpolate_solution(status, istats2, sol2, sol1, eq, param, num_src_pts_o=istats1(1), linear_interp_o=.true.)
  print '(a)', status_to_message(status)
  call print_solution(status, sol2, filename_o="three_body_steps_adapt_std_interpolated_lin.csv")
  ! SS-END:steps_adapt_int_linear:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive run w max y_delta length"
  ! SS-BEGIN:steps_adapt_etab-fix-delta-steps:
  call steps_adapt_etab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_sloppy_y_delta_len_max);
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="three_body_steps_adapt_etab-fix-delta-steps.csv", end_o=istats1(1))
  ! SS-END:steps_adapt_etab-fix-delta-steps:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive run w max t"
  ! SS-BEGIN:steps_adapt_etab-pho-t-max:
  call steps_adapt_etab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_max_t);
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="three_body_steps_adapt_etab-pho-t-max.csv", end_o=istats1(1))
  ! SS-END:steps_adapt_etab-pho-t-max:

  print '(a)', repeat('*', 120)
  print '(a)', "Adaptive run w moon orbit hit"
  ! SS-BEGIN:steps_adapt_etab-isct:
  call steps_adapt_etab(status, istats1, sol1, eq, t_iv, y_iv, param, a, b1, b2, c, p1, p2, &
                           t_delta_max_o=t_delta*100, t_delta_ini_o=t_delta*20, error_tol_abs_o=[1.0e-9_rk], &
                           error_tol_rel_o=[1.0e-6_rk], t_max_o=t_end, t_end_o=t_end, &
                           stepp_o=sp_cross_moon, sdf_o=sdf_cross_moon);
  print '(a)', status_to_message(status)
  call print_istats(status, istats1)
  call print_solution(status, sol1, filename_o="three_body_steps_adapt_etab-isct.csv", end_o=istats1(1))
  ! SS-END:steps_adapt_etab-isct:

contains
  
  subroutine eq(status, dydt, t, y, param)
    integer,          intent(out) :: status
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
  
  ! SS-BEGIN:steps_adapt_etab-pho-t-max-stepp:
  ! Example subroutine replicateing the functionality of t_max_o in steps_adapt_etab().
  subroutine sp_max_t(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer,          intent(out) :: status
    integer,          intent(out) :: end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer,          intent(out) :: sdf_flags
    integer,          intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),    parameter   :: t_max = 6.2_rk
    status    = 0
    sdf_flags = 0
    new_t_delta = -1.0_rk
    if ( solution(1, pnt_idx-1) + t_delta > t_max) then
       end_run = 1
    else
       end_run = 0
    end if
  end subroutine sp_max_t
  ! SS-END:steps_adapt_etab-pho-t-max-stepp:

  ! SS-BEGIN:steps_adapt_etab-fix-delta-stepp:
  ! Example subroutine to adjust t_delta in an atempt to keep y_delta under a maximum value.
  ! It is sloppy because we assume t_delta is linearly proportional to y_delta_len
  subroutine sp_sloppy_y_delta_len_max(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer,          intent(out) :: status, end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer,          intent(out) :: sdf_flags
    integer,          intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),      parameter :: y_delta_len_max = 0.1_rk
    integer,            parameter :: y_delta_len_idxs(2) = [1, 2]
    real(kind=rk)                 :: y_delta_len
    status    = 0
    end_run   = 0
    sdf_flags = 0
    y_delta_len = norm2(y_delta(y_delta_len_idxs))
    if ( y_delta_len > y_delta_len_max) then
       new_t_delta = t_delta * y_delta_len_max / y_delta_len
    else
       new_t_delta = -1.0_rk
    end if
  end subroutine sp_sloppy_y_delta_len_max
  ! SS-END:steps_adapt_etab-fix-delta-stepp:

  ! SS-BEGIN:steps_adapt_etab-isct-stepp:
  ! Example subroutine to find the first intersection of the satellite path and the moon's orbit.  It works 
  ! in conjunction with sdf_cross_moon().
  subroutine sp_cross_moon(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta)
    integer,          intent(out) :: status, end_run
    real(kind=rk),    intent(out) :: new_t_delta
    integer,          intent(out) :: sdf_flags
    integer,          intent(in)  :: pnt_idx
    real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
    real(kind=rk),    parameter   :: eps = 0.0001_rk
    real(kind=rk)                 :: lp_d, cp_d    
    status      = 0
    sdf_flags   = 0
    end_run     = 0
    new_t_delta = -1.0_rk
    if (solution(1, pnt_idx-1) > 0.2_rk) then
       cp_d = norm2(solution(2:3, pnt_idx-1)+y_delta(1:2))
       if ( abs(cp_d-1.0_rk)  < eps) then
          end_run   = 1
       else
          lp_d = norm2(solution(2:3, pnt_idx-1))
          if ((min(lp_d, cp_d) < 1.0_rk) .and. (max(lp_d, cp_d) > 1.0_rk)) then
             sdf_flags = 1
             end_run   = 1
          end if
       end if
    end if
  end subroutine sp_cross_moon
  ! SS-END:steps_adapt_etab-isct-stepp:

  ! SS-BEGIN:steps_adapt_etab-isct-sdf:
  ! Example SDF subroutine to isolate a point on a solution segment that crosses the unit circle.
  subroutine sdf_cross_moon(status, dist, sdf_flags, t, y)
    use mrkiss_config, only: rk
    implicit none
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dist
    integer,          intent(in)  :: sdf_flags
    real(kind=rk),    intent(in)  :: t, y(:)
    status = 0
    dist = 1.0_rk - norm2(y(1:2))
  end subroutine sdf_cross_moon
  ! SS-END:steps_adapt_etab-isct-sdf:

end program three_body
