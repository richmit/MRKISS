! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_solvers_wt.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     RK Solvers.@EOL
!! @keywords  runge kutta embedded butcher tableau
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Solvers.
!!
module mrkiss_solvers_wt
  implicit none
  private

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Type for ODE dydt functions.
  !!
  !! @verbatim
  !! status .... Exit status
  !!              - -inf-0 .... Everything worked
  !!              - 1-255 ..... Error in this routine
  !!              - others .... No other values allowed
  !! t ......... Value for t.
  !! y(:) ...... Value for y.
  !! param(:) .. Data payload usually used for constants.
  !! @endverbatim
  !!
  abstract interface
     subroutine deq_iface(status, dydt, t, y, param) 
       use mrkiss_config, only: rk
       implicit none
       integer,          intent(out) :: status
       real(kind=rk),    intent(out) :: dydt(:)
       real(kind=rk),    intent(in)  :: t
       real(kind=rk),    intent(in)  :: y(:)
       real(kind=rk),    intent(in)  :: param(:)
     end subroutine deq_iface
  end interface

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Type step processing subroutine.
  !!
  !! @verbatim
  !! status .... Exit status
  !!              - -inf-0 .... Everything worked
  !!              - 256-511 ... Error in this routine
  !!              - others .... No other values allowed
  !! t ......... Value for t.
  !! y(:) ...... Value for y.
  !! param(:) .. Data payload usually used for constants.
  !! @endverbatim
  !!
  abstract interface
     subroutine stepp_iface(status, end_run, sdf_flags, new_t_delta, pnt_idx, solution, t_delta, y_delta) 
       use mrkiss_config, only: rk
       implicit none
       integer,          intent(out) :: status
       integer,          intent(out) :: end_run
       real(kind=rk),    intent(out) :: new_t_delta   ! If >0, then redo step with new t_delta
       integer,          intent(out) :: sdf_flags     ! If >0, then use bisection to solve SDF and use the result to redo step...
       integer,          intent(in)  :: pnt_idx
       real(kind=rk),    intent(in)  :: solution(:,:), t_delta, y_delta(:)
     end subroutine stepp_iface
  end interface

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Type SDF function on a solution point.
  !!
  !! @verbatim
  !! status ..... Exit status
  !!               - -inf-0 .... Everything worked
  !!               - 512-767 ... Error in this routine
  !!               - others .... No other values allowed
  !! dist ....... The distance value of the SDF funciton
  !! sdf_flags .. Flags passed from a stepp_iface routine
  !! t .......... The t value.
  !! y(:) ....... The y value.
  !! param(:) ... Data payload usually used for constants.
  !! @endverbatim
  !!
  abstract interface
     subroutine sdf_iface(status, dist, sdf_flags, t, y) 
       use mrkiss_config, only: rk
       implicit none
       integer,          intent(out) :: status
       real(kind=rk),    intent(out) :: dist
       integer,          intent(in)  :: sdf_flags
       real(kind=rk),    intent(in)  :: t, y(:)
     end subroutine sdf_iface
  end interface

  public :: one_step_rk4, one_step_rkf45, one_step_dp54                                         ! Test one step solvers
  public :: one_step_etab, one_step_stab, one_richardson_step_stab                              ! One step solvers
  public :: steps_fixed_stab, steps_condy_stab, steps_sloppy_condy_stab, steps_adapt_etab    ! Many step solvers
  public :: steps_points_stab, interpolate_solution                                                ! Meta-many step solvers
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name One Step Solvers

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of a embedded RK method expressed as a Butcher Tableau.
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1232-1247 .. Error in this routine
  !!                                - others ..... No other values allowed
  !! y1_delta(:) ................. Returned delta for b1 method
  !! y2_delta(:) ................. Returned delta for b2 method
  !! dy(:) ....................... Returned dy/dt value at t
  !! deq ......................... Equation subroutine
  !! t, y(:) ..................... Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b1(:), b2(:), c(:) .. The butcher tableau
  !! t_delta ..................... Delta t to use for the step.
  !! @endverbatim
  !!
  subroutine one_step_etab(status, y1_delta, y2_delta, dy, deq, t, y, param, a, b1, b2, c, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b1(:), c(:), t_delta, b2(:)
    ! Variables
    integer                       :: i, stage
    real(kind=rk)                 :: k(size(y, 1),size(b1, 1)+1)
    real(kind=rk)                 :: stage_t_delta
    real(kind=rk)                 :: y_tmp(size(y, 1)), stage_y_delta(size(y, 1))
    ! Compute k vectors
    do stage=1,size(b1, 1)
       stage_y_delta = 0.0_rk
       if (stage > 1) then
          do i=1,(stage-1)
             stage_y_delta = stage_y_delta + a(i,stage) * k(:,i)
          end do
       end if
       stage_t_delta = t_delta*c(stage)
       call deq(status, y_tmp, t+stage_t_delta, y+stage_y_delta, param)
       if (status > 0) return
       if (stage==1) dy = y_tmp
       k(:,stage) = y_tmp * t_delta
    end do
    ! Compute y_delta
    y1_delta = 0.0_rk
    y2_delta = 0.0_rk
    do i=1,size(b1, 1)
       y1_delta = y1_delta + k(:,i) * b1(i)
       y2_delta = y2_delta + k(:,i) * b2(i)
    end do
    status = 0
  end subroutine one_step_etab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of a non-embedded RK method expressed as a Butcher Tableau.
  !!
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 0-255 ...... Evaluation of deq failed
  !!                        - 1248-1263 .. Error in this routine
  !!                        - others ..... No other values allowed
  !! y_delta(:) .......... Returned delta for the method
  !! dy(:) ............... Returned dy/dt value at t
  !! deq ................. Equation subroutine
  !! t, y(:) ............. Initial conditions.  y is a column vector!
  !! param(:) ............ Data payload passed to deq
  !! a(:,:), b(:), c(:) .. The butcher tableau
  !!                       The number of stages is determined based on the length of b.  All of the methods in an EERK need not
  !!                       be the same number of stages.  When this occurs, the b1 or b2 pulled from the module can be shortened
  !!                       when passing it to this function.  This will improve performance by not executing an unnecessary
  !!                       stage.  See MRKISS/tests/short_b.f90 for an example.
  !! t_delta ............. Delta t to use for the step.
  !! @endverbatim
  !! 
  ! SHELLO: sed -n '/^  *subroutine one_step_etab(/,/end subroutine one_step_etab *$/p' mrkiss_solvers.f90 | sed 's/, y2_delta[^,]*//; s/, b2[^,]*//; s/_etab/_stab/; s/b1/b/g; s/y1/y/g; /y2_delta/d;'
  subroutine one_step_stab(status, y_delta, dy, deq, t, y, param, a, b, c, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), t_delta
    ! Variables
    integer                       :: i, stage
    real(kind=rk)                 :: k(size(y, 1),size(b, 1)+1)
    real(kind=rk)                 :: stage_t_delta
    real(kind=rk)                 :: y_tmp(size(y, 1)), stage_y_delta(size(y, 1))
    ! Compute k vectors
    do stage=1,size(b, 1)
       stage_y_delta = 0.0_rk
       if (stage > 1) then
          do i=1,(stage-1)
             stage_y_delta = stage_y_delta + a(i,stage) * k(:,i)
          end do
       end if
       stage_t_delta = t_delta*c(stage)
       call deq(status, y_tmp, t+stage_t_delta, y+stage_y_delta, param)
       if (status > 0) return
       if (stage==1) dy = y_tmp
       k(:,stage) = y_tmp * t_delta
    end do
    ! Compute y_delta
    y_delta = 0.0_rk
    do i=1,size(b, 1)
       y_delta = y_delta + k(:,i) * b(i)
    end do
    status = 0
  end subroutine one_step_stab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one Richardson Extrapolation Step.
  !!
  !! Uses Richardson extrapolation to produce an estimate of order one greater than the provided Runge-Kutta method.
  !!
  !! @verbatim
  !! status .............. Exit status
  !!                        - -inf-0 ..... Everything worked
  !!                        - 0-255 ...... Evaluation of deq failed
  !!                        - 1216-1231 .. Error in this routine
  !!                        - 1248-1263 .. Error from one_step_stab()
  !!                        - others ..... No other values allowed
  !! y_delta(:) .......... Returned delta for the method
  !! dy(:) ............... Returned dy/dt value at t
  !! deq ................. Equation subroutine
  !! t, y(:) ............. Initial conditions.  y is a column vector!
  !! param(:) ............ Data payload passed to deq
  !! a(:,:), b(:), c(:) .. The butcher tableau
  !! p ................... The order for the RK method in the butcher tableau
  !! t_delta ............. Delta t to use for the step.
  !! @endverbatim
  !! @see: mrkiss_solvers::one_step_stab(), 
  !!
  subroutine one_richardson_step_stab(status, y_delta, dy, deq, t, y, param, a, b, c, p, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b(:), c(:)
    integer,          intent(in)  :: p
    real(kind=rk),    intent(in)  :: t_delta
    ! Variables
    real(kind=rk)                 :: t_delta_tmp, t_tmp, y_tmp(size(y, 1))
    real(kind=rk)                 :: y_delta_small_1(size(y, 1)), y_delta_small_2(size(y, 1)), y_delta_big(size(y, 1))
    ! Compute y_delta_small
    t_delta_tmp = t_delta / 2.0_rk
    t_tmp       = t
    y_tmp       = y
    call one_step_stab(status, y_delta_small_1, dy, deq, t_tmp, y_tmp, param, a, b, c, t_delta_tmp)
    if (status > 0) return
    t_tmp = t_tmp + t_delta_tmp
    y_tmp = y_tmp + y_delta_small_1
    call one_step_stab(status, y_delta_small_2, dy, deq, t_tmp, y_tmp, param, a, b, c, t_delta_tmp)
    if (status > 0) return
    ! Compute y_delta_big
    call one_step_stab(status, y_delta_big, dy, deq, t, y, param, a, b, c, t_delta)
    if (status > 0) return
    ! Compute y_delta
    y_delta = y_delta_small_1 + y_delta_small_2 + (y_delta_small_1 + y_delta_small_2 - y_delta_big) / (2**p - 1)
    status = 0
  end subroutine one_richardson_step_stab

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name One Step Test Solvers

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of RK (mrkiss_erk_kutta_4)
  !!
  !! @verbatim
  !! status ...... Exit status
  !!                - -inf-0 ..... Everything worked
  !!                - 0-255 ...... Evaluation of deq failed
  !!                - 1200-1215 .. Error in this routine
  !!                - others ..... No other values allowed
  !! y_delta(:) .. Returned delta for the method
  !! dy(:) ....... Returned dy/dt value at t
  !! deq ......... Equation subroutine
  !! t, y(:) ..... Initial conditions.  y is a column vector!
  !! param(:) .... Data payload passed to deq
  !! t_delta ..... Delta t to use for the step.
  !! @endverbatim
  !!
  subroutine one_step_rk4(status, y_delta, dy, deq, t, y, param, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), t_delta
    ! Variables
    real(kind=rk)                 :: k1(size(y, 1)), k2(size(y, 1)), k3(size(y, 1)), k4(size(y, 1))
    ! Compute Step
    call deq(status, k1, t, y, param)
    if (status > 0) return
    dy = k1
    call deq(status, k2, t + 1.0_rk/2.0_rk * t_delta, y + 1.0_rk/2.0_rk * t_delta * k1, param)
    if (status > 0) return
    call deq(status, k3, t + 1.0_rk/2.0_rk * t_delta, y + 1.0_rk/2.0_rk * t_delta * k2, param)
    if (status > 0) return
    call deq(status, k4, t + t_delta, y + t_delta * k3, param)
    y_delta = t_delta * (k1 + 2.0_rk * k2 + 2.0_rk * k3 + k4) / 6.0_rk
    status = 0
  end subroutine one_step_rk4

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of RKF45 (mrkiss_eerk_fehlberg_4_5)
  !!
  !! @verbatim
  !! status ...... Exit status
  !!                - -inf-0 ..... Everything worked
  !!                - 0-255 ...... Evaluation of deq failed
  !!                - 1184-1199 .. Error in this routine
  !!                - others ..... No other values allowed
  !! y_delta(:) .. Returned delta for the method
  !! dy(:) ....... Returned dy/dt value at t
  !! deq ......... Equation subroutine
  !! t, y(:) ..... Initial conditions.  y is a column vector!
  !! param(:) .... Data payload passed to deq
  !! t_delta ..... Delta t to use for the step.
  !! @endverbatim
  !!
  subroutine one_step_rkf45(status, y1_delta, y2_delta, dy, deq, t, y, param, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), t_delta
    ! Variables
    real(kind=rk)                 :: k1(size(y, 1)), k2(size(y, 1)), k3(size(y, 1))
    real(kind=rk)                 :: k4(size(y, 1)), k5(size(y, 1)), k6(size(y, 1))
    ! Compute Step
    call deq(status, k1, t, y, param)
    if (status > 0) return
    dy = k1
    call deq(status, k2, t + 1.0_rk/4.0_rk * t_delta, y + 1.0_rk/4.0_rk * t_delta * k1, param)
    if (status > 0) return
    call deq(status, k3, t + 3.0_rk/8.0_rk * t_delta, y + t_delta * (3.0_rk/32.0_rk * k1 + 9.0_rk/32.0_rk * k2), param)
    if (status > 0) return
    call deq(status, k4, t + 12.0_rk/13.0_rk * t_delta, y + t_delta * (1932.0_rk/2197.0_rk * k1 - 7200.0_rk/2197.0_rk * k2 + 7296.0_rk/2197.0_rk * k3), param)
    if (status > 0) return
    call deq(status, k5, t + t_delta, y + t_delta * (439.0_rk/216.0_rk * k1 - 8.0_rk * k2 + 3680.0_rk/513.0_rk * k3 - 845.0_rk/4104.0_rk * k4), param)
    if (status > 0) return
    call deq(status, k6, t + 1.0_rk/2.0_rk * t_delta, y + t_delta * (-8.0_rk/27.0_rk * k1 + 2.0_rk * k2 - 3544.0_rk/2565.0_rk * k3 + 1859.0_rk/4104.0_rk * k4 - 11.0_rk/40.0_rk * k5), param)
    if (status > 0) return
    y1_delta = t_delta * (25.0_rk/216.0_rk * k1 + 1408.0_rk/2565.0_rk * k3 + 2197.0_rk/4104.0_rk * k4 - 1.0_rk/5.0_rk * k5)
    y2_delta = t_delta * (16.0_rk/135.0_rk * k1 + 6656.0_rk/12825.0_rk * k3 + 28561.0_rk/56430.0_rk * k4 - 9.0_rk/50.0_rk * k5 + 2.0_rk/55.0_rk * k6)
    status = 0
  end subroutine one_step_rkf45

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of DP45 (mrkiss_eerk_dormand_prince_5_4)
  !!
  !! @verbatim
  !! status ...... Exit status
  !!                - -inf-0 ..... Everything worked
  !!                - 0-255 ...... Evaluation of deq failed
  !!                - 1263-1279 .. Error in this routine
  !!                - others ..... No other values allowed
  !! y_delta(:) .. Returned delta for the method
  !! dy(:) ....... Returned dy/dt value at t
  !! deq ......... Equation subroutine
  !! t, y(:) ..... Initial conditions.  y is a column vector!
  !! param(:) .... Data payload passed to deq
  !! t_delta ..... Delta t to use for the step.
  !! @endverbatim
  !!
  subroutine one_step_dp54(status, y1_delta, y2_delta, dy, deq, t, y, param, t_delta) 
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)       :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), t_delta
    ! Variables
    real(kind=rk)                 :: k1(size(y, 1)), k2(size(y, 1)), k3(size(y, 1)), k4(size(y, 1))
    real(kind=rk)                 :: k5(size(y, 1)), k6(size(y, 1)), k7(size(y, 1))
    ! Compute Step
    call deq(status, k1, t, y, param)
    if (status > 0) return
    dy = k1;
    call deq(status, k2, t+t_delta/5.0_rk, y + t_delta * (k1/5.0_rk), param)
    if (status > 0) return
    call deq(status, k3, t+t_delta*3.0_rk/10.0_rk, y + t_delta * (k1*3.0_rk/40.0_rk + k2*9.0_rk/40.0_rk), param)
    if (status > 0) return
    call deq(status, k4, t+t_delta*4.0_rk/5.0_rk, y + t_delta * (k1*44.0_rk/45.0_rk - k2*56.0_rk/15.0_rk + k3*32.0_rk/9.0_rk), param)
    if (status > 0) return
    call deq(status, k5, t+t_delta*8.0_rk/9.0_rk, y + t_delta * (k1*19372.0_rk/6561.0_rk - k2*25360.0_rk/2187.0_rk + k3*64448.0_rk/6561.0_rk - k4*212.0_rk/729.0_rk), param)
    if (status > 0) return
    call deq(status, k6, t+t_delta, y + t_delta * (k1*9017.0_rk/3168.0_rk - k2*355.0_rk/33.0_rk + k3*46732.0_rk/5247.0_rk + k4*49.0_rk/176.0_rk - k5*5103.0_rk/18656.0_rk), param)
    if (status > 0) return
    call deq(status, k7, t+t_delta, y + t_delta * (k1*35.0_rk/384.0_rk + k3*500.0_rk/1113.0_rk + k4*125.0_rk/192.0_rk - k5*2187.0_rk/6784.0_rk + k6*11.0_rk/84.0_rk), param)
    if (status > 0) return
    y1_delta = t_delta * (5179.0_rk/57600.0_rk*k1 + 7571.0_rk/16695.0_rk*k3 + 393.0_rk/640.0_rk*k4 - 92097.0_rk/339200.0_rk*k5 + 187.0_rk/2100.0_rk*k6 + 1.0_rk/40.0_rk*k7)
    y2_delta = t_delta * (35.0_rk/384.0_rk*k1 + 500.0_rk/1113.0_rk*k3 + 125.0_rk/192.0_rk*k4 - 2187.0_rk/6784.0_rk*k5 + 11.0_rk/84.0_rk*k6)
    status = 0
  end subroutine one_step_dp54

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name Multistep Solvers

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take multiple fixed time steps with a simple RK method and store solutions in solution.  
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1120-1151 .. Error in this routine
  !!                                - 1216-1231 .. Error from one_richardson_step_stab()
  !!                                - 1248-1263 .. Error from one_step_stab()
  !!                                - others ..... No other values allowed
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates
  !!                                   - isi_num_pts
  !!                                   - isi_step_norm
  !! solution(:,:) ............... Array for solution.  
  !!                                Each COLUMN is a solution:
  !!                                 - First element is the t variable
  !!                                 - size(y, 1) elements starting with 2 have y values
  !!                                 - The next size(y, 1) elements have dy values
  !! deq ......................... Equation subroutine
  !! t, y(:) ..................... Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b(:), c(:) .......... The butcher tableau
  !! p_o ......................... The order for the RK method in the butcher tableau to enable Richardson extrapolation
  !! max_pts_o ................... Maximum number of solutions to put in solution.
  !!                               If max_pts_o>1 & size(solution, 2)==1, then max_pts_o-1 steps are taken and 
  !!                               only the last solution is stored in solution.
  !! t_delta_o ................... Step size to use.  
  !!                                If t_end_o is provided:  Default: (t_end - t) / (size(solution, 2) - 1)
  !!                                If t_end_o not provided: Default: mrkiss_config::t_delta_ai
  !! t_end_o ..................... End point for last step.  Silently ignored if t_delta_o is provided.
  !! t_max_o ..................... Maximum value for t
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_solvers::one_step_stab(), mrkiss_solvers::one_richardson_step_stab()
  !!
  subroutine steps_fixed_stab(status, istats, solution, deq, t, y, param, a, b, c, p_o, max_pts_o, t_delta_o, &
                                 t_end_o, t_max_o) 
    use mrkiss_config, only: rk, t_delta_ai, istats_size, isi_num_pts, isi_step_norm
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                 :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:)
    integer,          optional, intent(in)  :: p_o, max_pts_o
    real(kind=rk),    optional, intent(in)  :: t_delta_o, t_end_o, t_max_o
    ! Vars
    integer                                 :: cur_pnt_idx, y_dim, cur_step, max_steps, p
    real(kind=rk)                           :: t_cv, t_delta
    real(kind=rk)                           :: y_cv(size(y, 1)), y_delta(size(y, 1)), dy(size(y, 1))
    logical                                 :: lotsopnts 
    ! Process arguments
    max_steps = size(solution, 2) - 1
    lotsopnts = .true.
    if (present(max_pts_o)) then
       if ((max_steps == 0) .and. (max_pts_o > 1)) then
          lotsopnts = .false.
          max_steps = max_pts_o - 1
       else
          max_steps = min(max_pts_o - 1, max_steps)
       end if
    end if
    if (present(t_delta_o)) then
       t_delta = t_delta_o
    else 
       if (present(t_end_o)) then
          t_delta = (t_end_o - t) / max_steps
       else
          t_delta = t_delta_ai
       end if
    end if
    p = 0
    if (present(p_o)) p = p_o
    ! Compute Solution
    y_dim = size(y, 1)
    istats = 0
    t_cv = t
    y_cv = y
    cur_step = 0
    cur_pnt_idx = 1
    solution(1,  cur_pnt_idx) = t_cv
    solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
    do 
       cur_step = cur_step + 1
       if (p > 0) then
          call one_richardson_step_stab(status, y_delta, dy, deq, t_cv, y_cv, param, a, b, c, p, t_delta)
          istats(isi_step_norm) = istats(isi_step_norm) + 3
       else
          call one_step_stab(status, y_delta, dy, deq, t_cv, y_cv, param, a, b, c, t_delta)
          istats(isi_step_norm) = istats(isi_step_norm) + 1
       end if
       if (status > 0) return
       y_cv = y_cv + y_delta
       t_cv = t_cv + t_delta
       if (lotsopnts) then
          cur_pnt_idx = cur_pnt_idx + 1
          solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx-1) = dy
       end if
       solution(1, cur_pnt_idx) = t_cv
       solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
       istats(isi_num_pts) = istats(isi_num_pts) + 1
       status = 0
       if (present(t_max_o)) then
          if (t_cv > t_max_o) exit
       end if
       if (cur_step >= max_steps) exit
    end do
    ! Compute derivative for final solution point
    call deq(status, dy, t_cv, y_cv, param)  ! This sets return status
    if (status > 0) return
    solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx) = dy
    istats(isi_num_pts) = istats(isi_num_pts) + 1
  end subroutine steps_fixed_stab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take multiple adaptive steps with constant length y_delta using a simple RK method storing the solutions in solution.  
  !!
  !! The adaptive step size is controlled to result in steps that achieve a constant "length".  The "length" is defined to be the
  !! Euclidean norm of the subset of the solution vector specified by y_delta_len_idxs_o.  For each solution the routine will
  !! make two one_step_stab_* calls with t_delta_min_o and t_delta_max, and then use bisection (with additional calls to
  !! one_step_stab_*) to isolate a t_delta value that leads to a a length within y_delta_len_tol_o of y_delta_len_targ.
  !!     
  !! Note there is no mathematical guarantee that a RK step of size t_delta_min_o and t_delta_max will produce solutions that
  !! bracket y_delta_len_targ.  That said, for well behaved functions a t_delta_min_o and t_delta_max may always be found that
  !! work.  In practice finding good values are t_delta_min_o and t_delta_max isn't normally difficult.  One approach for harder
  !! problems is to use fixed step sizes over the interval in question, and then examine the y_delta lengths in the solution.
  !!
  !! My primary use case for this function is to create uniform sphere sweeps for constructive solid geometry applications.
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1024-1055 .. Error in this routine (no_bisect_error_o==.true.)
  !!                                                - 1024 .. t_delta_min yielded a longer step than t_delta_max
  !!                                                - 1025 .. no_bisect_error_o==0 not present and max_bisect_o violated
  !!                                - 1248-1263 .. Error from one_step_stab()
  !!                                - others ..... No other values allowed
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates
  !!                                   - isi_num_pts
  !!                                   - isi_step_norm
  !!                                   - isi_step_y_len
  !!                                   - isi_bic_fail_max
  !!                                   - isi_bic_fail_bnd
  !! solution .................... Array for solution.  
  !!                                Each COLUMN is a solution:
  !!                                 - First element is the t variable
  !!                                 - size(y, 1) elements starting with 2 have y values
  !!                                 - The next size(y, 1) elements have dy values if
  !! deq ......................... Equation subroutine
  !! t, y(:) ..................... Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b(:), c(:) .......... The butcher tableau
  !! y_delta_len_targ ............ Attempt to make all steps this long
  !! t_delta_max ................. Maximum t_delta
  !! t_delta_min_o ............... Minimum t_delta
  !! y_delta_len_tol_o ........... How close we have to get to y_delta_len_targ.  Default is y_delta_len_targ/100
  !! y_delta_len_idxs_o .......... Components of y_delta to use for y_delta length computation
  !! max_pts_o ................... Maximum number of solutions to put in solution.
  !! max_bisect_o ................ Maximum number of bisection iterations per each step.  Default: max_bisect_ai
  !! no_bisect_error_o ........... If .true., then do not exit on bisection errors
  !! y_sol_len_max_o ............. Maximum length of the solution curve
  !! t_max_o ..................... Maximum value for t
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_utils::status_to_message(), mrkiss_solvers::one_step_stab()
  !!
  subroutine steps_condy_stab(status, istats, solution, deq, t, y, param, a, b, c, y_delta_len_targ,          &
                                 t_delta_max, t_delta_min_o, y_delta_len_tol_o, max_bisect_o, no_bisect_error_o, &
                                 y_delta_len_idxs_o, max_pts_o, y_sol_len_max_o, t_max_o) 
    use mrkiss_config, only: rk, t_delta_tiny, max_bisect_ai, istats_size, isi_bic_fail_bnd, isi_bic_fail_max, isi_num_pts, isi_step_norm, isi_step_y_len
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                 :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), y_delta_len_targ, t_delta_max
    real(kind=rk),    optional, intent(in)  :: t_delta_min_o, y_delta_len_tol_o
    integer,          optional, intent(in)  :: max_pts_o, max_bisect_o, y_delta_len_idxs_o(:)
    logical,          optional, intent(in)  :: no_bisect_error_o
    real(kind=rk),    optional, intent(in)  :: y_sol_len_max_o, t_max_o
    ! Variables
    integer                                 :: max_bisect, max_pts, cur_pnt_idx, biter, y_dim
    logical                                 :: no_bisect_error
    real(kind=rk)                           :: y_delta_len_tol, t_delta_min, y_sol_len, bs_tmp1_y_delta_len
    real(kind=rk)                           :: bs_tmp1_t_delta, bs_tmp2_t_delta, t_cv, dy(size(y, 1))
    real(kind=rk)                           :: bs_tmpc_dy(size(y, 1)), bs_tmp1_dy(size(y, 1)), bs_tmp2_dy(size(y, 1))
    real(kind=rk)                           :: bs_tmp2_y_delta_len, bs_tmpc_y_delta_len, bs_tmpc_t_delta
    real(kind=rk)                           :: y_cv(size(y, 1)), bs_tmp1_y_delta(size(y, 1)) 
    real(kind=rk)                           :: bs_tmp2_y_delta(size(y, 1)), bs_tmpc_y_delta(size(y, 1))
    ! Process arguments
    max_pts = size(solution, 2)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o);
    t_delta_min = t_delta_tiny
    if (present(t_delta_min_o)) t_delta_min = t_delta_min_o
    max_bisect = max_bisect_ai
    if (present(max_bisect_o)) max_bisect = max_bisect_o
    y_delta_len_tol = y_delta_len_targ / 100.0_rk
    if (present(y_delta_len_tol_o)) y_delta_len_tol = y_delta_len_tol_o
    max_pts = size(solution, 2)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o);
    no_bisect_error = .false.
    if (present(no_bisect_error_o)) no_bisect_error = no_bisect_error_o
    ! Compute Solution
    y_dim = size(y, 1)
    y_sol_len = 0.0_rk
    istats = 0
    t_cv = t
    y_cv = y
    cur_pnt_idx = 1
    solution(1, cur_pnt_idx) = t_cv
    solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
    do 
       cur_pnt_idx = cur_pnt_idx  + 1
       ! Compute t_delta 1
       bs_tmp1_t_delta = t_delta_min
       call one_step_stab(status, bs_tmp1_y_delta, bs_tmp1_dy, deq, t_cv, y_cv, param, a, b, c, bs_tmp1_t_delta)
       istats(isi_step_norm) = istats(isi_step_norm) + 1
       if (status > 0) return
       if (present(y_delta_len_idxs_o)) then
          bs_tmp1_y_delta_len = norm2(bs_tmp1_y_delta(y_delta_len_idxs_o))
       else
          bs_tmp1_y_delta_len = norm2(bs_tmp1_y_delta)
       end if
       ! Compute upper t_delta
       bs_tmp2_t_delta = t_delta_max
       call one_step_stab(status, bs_tmp2_y_delta, bs_tmp2_dy, deq, t_cv, y_cv, param, a, b, c, bs_tmp2_t_delta)
       istats(isi_step_norm) = istats(isi_step_norm) + 1
       if (status > 0) return
       if (present(y_delta_len_idxs_o)) then
          bs_tmp2_y_delta_len = norm2(bs_tmp2_y_delta(y_delta_len_idxs_o))
       else
          bs_tmp2_y_delta_len = norm2(bs_tmp2_y_delta)
       end if
       ! Swap if required
       if (bs_tmp2_y_delta_len < bs_tmp1_y_delta_len) then
          bs_tmpc_t_delta     = bs_tmp1_t_delta
          bs_tmp1_t_delta     = bs_tmp2_t_delta
          bs_tmp2_t_delta     = bs_tmpc_t_delta
          bs_tmpc_y_delta_len = bs_tmp1_y_delta_len
          bs_tmp1_y_delta_len = bs_tmp2_y_delta_len
          bs_tmp2_y_delta_len = bs_tmpc_y_delta_len
          bs_tmpc_y_delta     = bs_tmp1_y_delta
          bs_tmp1_y_delta     = bs_tmp2_y_delta
          bs_tmp2_y_delta     = bs_tmpc_y_delta
          bs_tmpc_dy          = bs_tmp1_dy
          bs_tmp1_dy          = bs_tmp2_dy
          bs_tmp2_dy          = bs_tmpc_dy
       end if
       ! Initial "current" values for bisection and no-bisection
       bs_tmpc_t_delta     = bs_tmp2_t_delta
       bs_tmpc_y_delta_len = bs_tmp2_y_delta_len
       bs_tmpc_y_delta     = bs_tmp2_y_delta
       ! Bisect if required
       if ((bs_tmp1_y_delta_len < y_delta_len_targ) .and. (bs_tmp2_y_delta_len > y_delta_len_targ)) then
          biter = 1
          do while (abs(bs_tmpc_y_delta_len - y_delta_len_targ) > y_delta_len_tol)
             if (biter >  max_bisect) then
                istats(isi_bic_fail_max) = istats(isi_bic_fail_max) + 1
                if (no_bisect_error) then
                   exit
                else
                   status = 1025
                   return
                end if
             end if
             bs_tmpc_t_delta = (bs_tmp1_t_delta + bs_tmp2_t_delta) / 2.0_rk
             call one_step_stab(status, bs_tmpc_y_delta, bs_tmpc_dy, deq, t_cv, y_cv, param, a, b, c, bs_tmpc_t_delta)
             istats(isi_step_y_len) = istats(isi_step_y_len) + 1
             if (status > 0) return
             if (present(y_delta_len_idxs_o)) then
                bs_tmpc_y_delta_len = norm2(bs_tmpc_y_delta(y_delta_len_idxs_o))
             else
                bs_tmpc_y_delta_len = norm2(bs_tmpc_y_delta)
             end if
             if (bs_tmpc_y_delta_len < y_delta_len_targ) then
                bs_tmp1_y_delta_len = bs_tmpc_y_delta_len
                bs_tmp1_t_delta     = bs_tmpc_t_delta
             else
                bs_tmp2_y_delta_len = bs_tmpc_y_delta_len
                bs_tmp2_t_delta     = bs_tmpc_t_delta
             end if
             biter = biter + 1;
          end do
       else
          istats(isi_bic_fail_bnd) = istats(isi_bic_fail_bnd) + 1
          if (no_bisect_error) then
             status = 1024
             return
          end if
       end if
       ! Update solution
       y_cv = y_cv + bs_tmpc_y_delta
       t_cv = t_cv + bs_tmpc_t_delta
       solution(1, cur_pnt_idx) = t_cv
       solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx-1) = bs_tmpc_dy
       solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
       istats(isi_num_pts) = istats(isi_num_pts) + 1
       status = 0
       if (present(y_sol_len_max_o)) then
          y_sol_len = y_sol_len + bs_tmpc_y_delta_len
          if (y_sol_len > y_sol_len_max_o) exit
       end if
       if (present(t_max_o)) then
          if (t_cv > t_max_o) exit
       end if
       if (cur_pnt_idx >= max_pts) exit
    end do
    ! Compute derivative for final solution point
    call deq(status, dy, t_cv, y_cv, param)  ! This sets return status
    if (status > 0) return
    solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx) = dy
    istats(isi_num_pts) = istats(isi_num_pts) + 1
  end subroutine steps_condy_stab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take multiple adaptive steps adjusting for the length of y_delta using a simple RK method storing the solutions in solution.  
  !!
  !! This method attempts to control the length of y_delta.  At each solution step it takes a probing step at t_delta_ini and
  !! then takes another step with t_delta = t_delta_ini * y_delta_len_targ / y_delta_len.  If which will result in a y_delta
  !! approximately y_delta_len_targ when t_delta is proportional to y_delta.  By default this second step is only taken when the
  !! y_delta of the probe step is greater than y_delta_len_targ; however, it will be performed on shorter steps when adj_short_o
  !! is present -- in this mode it approximates the behavior steps_condy_stab() but is *much* faster.
  !!
  !! Note the assumption that t_delta is proportional to y_delta.  We have no mathematical guarantee for this assumption; however,
  !! it generally works in practice with well behaved functions when t_delta_ini is small and the.
  !!
  !! My primary use case for this function is to shrink down long steps for smoother curves/tubes in visualizations.
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1280-1296 .. Error in this routine
  !!                                - 1248-1263 .. Error from one_step_stab()
  !!                                - others ..... No other values allowed
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates:
  !!                                   - isi_num_pts
  !!                                   - isi_step_norm
  !!                                   - isi_step_y_len
  !! solution .................... Array for solution.  
  !!                                Each COLUMN is a solution:
  !!                                 - First element is the t variable
  !!                                 - size(y, 1) elements starting with 2 have y values
  !!                                 - The next size(y, 1) elements have dy values
  !! deq ......................... Equation subroutine
  !! t, y(:) ..................... Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b(:), c(:) .......... The butcher tableau
  !! y_delta_len_targ ............ Attempt to make all steps this long
  !! t_delta_ini ................. Test step t_delta
  !! t_delta_max_o ............... Maximum t_delta
  !! t_delta_min_o ............... Minimum t_delta
  !! y_delta_len_idxs_o .......... Components of y_delta to use for y_delta length computation
  !! adj_short_o ................. Adjust when t_delta is too short as well as when it's too long.
  !! max_pts_o ................... Maximum number of solutions to put in solution.
  !! y_sol_len_max_o ............. Maximum length of the solution curve
  !! t_max_o ..................... Maximum value for t
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_utils::status_to_message(), mrkiss_solvers::one_step_stab()
  !!
  subroutine steps_sloppy_condy_stab(status, istats, solution, deq, t, y, param, a, b, c, y_delta_len_targ, t_delta_ini, &
                                        t_delta_min_o, t_delta_max_o, y_delta_len_idxs_o, adj_short_o, max_pts_o,           &
                                        y_sol_len_max_o, t_max_o) 
    use mrkiss_config, only: rk, t_delta_tiny, istats_size, isi_num_pts, isi_step_norm, isi_step_y_len
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                 :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), y_delta_len_targ, t_delta_ini
    real(kind=rk),    optional, intent(in)  :: t_delta_min_o, t_delta_max_o
    integer,          optional, intent(in)  :: max_pts_o, y_delta_len_idxs_o(:), adj_short_o
    real(kind=rk),    optional, intent(in)  :: y_sol_len_max_o, t_max_o
    ! Variables
    integer                                 :: max_pts, cur_pnt_idx, y_dim
    real(kind=rk)                           :: t_delta_min, y_sol_len, t_cv, t_delta, y_delta_len
    real(kind=rk)                           :: y_cv(size(y, 1)), y_delta(size(y, 1)), dy(size(y, 1))
    ! Process arguments
    max_pts = size(solution, 2)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o);
    t_delta_min = t_delta_tiny
    if (present(t_delta_min_o)) t_delta_min = t_delta_min_o
    ! Compute Solution
    y_dim = size(y, 1)
    y_sol_len = 0.0_rk
    istats = 0
    t_cv = t
    y_cv = y
    cur_pnt_idx = 1
    solution(1, cur_pnt_idx) = t_cv
    solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
    do 
       cur_pnt_idx = cur_pnt_idx  + 1
       ! Compute Initial step
       t_delta = t_delta_ini
       call one_step_stab(status, y_delta, dy, deq, t_cv, y_cv, param, a, b, c, t_delta)
       istats(isi_step_norm) = istats(isi_step_norm) + 1
       if (status > 0) return
       if (present(y_delta_len_idxs_o)) then
          y_delta_len = norm2(y_delta(y_delta_len_idxs_o))
       else
          y_delta_len = norm2(y_delta)
       end if
       if ((y_delta_len > y_delta_len_targ) .or. present(adj_short_o)) then
          ! Adjust step
          t_delta = t_delta_ini * y_delta_len_targ / y_delta_len
          t_delta = max(t_delta_min, t_delta)
          if (present(t_delta_max_o)) then
             t_delta = min(t_delta_max_o, t_delta)
          end if
          ! Compute adjusted step
          call one_step_stab(status, y_delta, dy, deq, t_cv, y_cv, param, a, b, c, t_delta)
          istats(isi_step_y_len) = istats(isi_step_y_len) + 1
          if (status > 0) return
       end if
       ! Update state
       y_cv = y_cv + y_delta
       t_cv = t_cv + t_delta
       solution(1, cur_pnt_idx) = t_cv
       solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx-1) = dy
       solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
       istats(isi_num_pts) = istats(isi_num_pts) + 1
       status = 0
       ! Process solution length limit
       if (present(y_sol_len_max_o)) then
          if (present(y_delta_len_idxs_o)) then
             y_sol_len = y_sol_len + norm2(y_delta(y_delta_len_idxs_o))
          else
             y_sol_len = y_sol_len + norm2(y_delta)
          end if
          if (y_sol_len > y_sol_len_max_o) exit
       end if
       ! Process t size limit
       if (present(t_max_o)) then
          if (t_cv > t_max_o) exit
       end if
       ! Max points limit
       if (cur_pnt_idx >= max_pts) exit
    end do
    ! Compute derivative for final solution point
    call deq(status, dy, t_cv, y_cv, param)  ! This sets return status
    if (status > 0) return
    solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx) = dy
    istats(isi_num_pts) = istats(isi_num_pts) + 1
  end subroutine steps_sloppy_condy_stab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take multiple adaptive steps with an embedded RK method using relatively traditional step size controls.
  !!
  !! @warning I have not yet finalized the error estimate method.  /0 is possible when error_tol_abs_o = 0.
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 256-511 .... Error in stepp_o
  !!                                - 512-767 .... Error in sdf_o
  !!                                - 1056-1119 .. Error in this routine
  !!                                                - 1056 .. no_bisect_error_o=.false. and max_bisect_o violated
  !!                                - 1232-1247 .. Error from one_step_etab()
  !!                                - 1248-1263 .. Error from one_step_stab()
  !!                                - others ..... No other values allowed
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates:
  !!                                   - isi_num_pts        
  !!                                   - isi_step_norm      
  !!                                   - isi_step_y_err     
  !!                                   - isi_step_spp_td    
  !!                                   - isi_step_sdf_bic   
  !!                                   - isi_bic_fail_max   
  !!                                   - isi_bic_fail_bnd   
  !! solution(:,:) ............... Array for solution.
  !!                                Each COLUMN is a solution:
  !!                                 - First element is the t variable
  !!                                 - size(y, 1) elements starting with 2 have y values
  !!                                 - The next size(y, 1) elements have dy values
  !! deq ......................... Equation subroutine
  !! t, y(:) ..................... Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b1(:), b2(:), c(:) .. The butcher tableau
  !! p1, p2 ...................... The orders for the RK methods in the butcher tableaus
  !! t_max_o ..................... Stop if t>t_max_o.  Different from t_end_o! Default: NONE
  !! t_end_o ..................... Try to stop integration at t_end. Default: NONE
  !! t_delta_ini_o ............... Initial t_delta. Default: (t_delta_max_o+t_delta_min_o)/2 or t_delta_ai otherwise
  !! t_delta_min_o ............... Minimum allowed t_delta. Default: t_delta_tiny
  !! t_delta_max_o ............... Maximum allowed t_delta. Default: NONE
  !! t_delta_fac_min_o ........... Minimum t_delta adaption factor for all but the first step.  Default:  t_delta_fac_min_ai
  !! t_delta_fac_max_o ........... Maximum t_delta adaption factor.  Default:  2.0_rk
  !! t_delta_fac_fdg_o ........... Extra t_delta adaption factor when shrinking interval. Default: t_delta_fac_fdg_ai
  !! error_tol_abs_o(:) .......... Absolute error tolerance. Default: error_tol_abs_ai
  !! error_tol_rel_o(:) .......... Relative error tolerance. Default: error_tol_rel_ai
  !! max_pts_o ................... Maximum number of solutions to put in solution.
  !! max_bisect_o ................ Maximum number of bisection iterations to perform for each step. Default: max_bisect_ai
  !! no_bisect_error_o ........... If .true., then not exit on bisection errors
  !! sdf_o ....................... SDF function.  Used to set new t_delta for a step.  This subroutine may trigger one or
  !!                               more of the following actions:
  !!                                - status>0      => Immediately returns doing nothing else propagating status to caller.
  !!                                                   Positive status values must come from the interval [256, 511].
  !!                                - new_t_delta>0 => Recompute y_delta using new_t_delta.
  !!                                - sdf_flags>0   => Redo step with a t_delta derived from the sdf_o via bisection.
  !!                                - end_run>0     => routine returns after this step is complete.
  !! sdf_tol_o ................... How close we have to get to accept an sdf solution. Default: sdf_tol_ai
  !! stepp_o ..................... Step processing subroutine.  Called after each step.
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_utils::status_to_message(), mrkiss_solvers::one_step_etab(), mrkiss_solvers::one_step_stab()
  !! 
  subroutine steps_adapt_etab(status, istats, solution, deq, t, y, param, a, b1, b2, c, p1, p2, t_max_o, t_end_o, &
                                 t_delta_ini_o, t_delta_min_o, t_delta_max_o, t_delta_fac_min_o, t_delta_fac_max_o, &
                                 t_delta_fac_fdg_o, error_tol_abs_o, error_tol_rel_o, max_pts_o, max_bisect_o,      &
                                 no_bisect_error_o, sdf_o, sdf_tol_o, stepp_o) 
    use mrkiss_config
    implicit none
    ! Arguments
    integer,                             intent(out) :: status, istats(istats_size)
    real(kind=rk),                       intent(out) :: solution(:,:)
    procedure(deq_iface)                          :: deq
    real(kind=rk),                       intent(in)  :: t
    real(kind=rk),                       intent(in)  :: y(:), param(:), a(:,:), b1(:), b2(:), c(:)
    integer,                             intent(in)  :: p1, p2
    real(kind=rk),             optional, intent(in)  :: t_max_o, t_end_o, t_delta_ini_o, t_delta_min_o, t_delta_max_o
    real(kind=rk),             optional, intent(in)  :: t_delta_fac_min_o, t_delta_fac_max_o, t_delta_fac_fdg_o
    real(kind=rk),             optional, intent(in)  :: error_tol_abs_o(:), error_tol_rel_o(:)
    integer,                   optional, intent(in)  :: max_pts_o, max_bisect_o
    logical,                   optional, intent(in)  :: no_bisect_error_o
    procedure(sdf_iface),   optional              :: sdf_o
    real(kind=rk),             optional, intent(in)  :: sdf_tol_o
    procedure(stepp_iface), optional              :: stepp_o
    ! Variables
    integer                                          :: max_pts, cur_pnt_idx, adj_cnt, y_dim
    integer                                          :: max_bisect, sp_end_run, sp_sdf_flags, bs_itr
    logical                                          :: no_bisect_error
    real(kind=rk)                                    :: t_delta_fac, y_cv(size(y, 1)), y1_delta(size(y, 1)), dy(size(y, 1))
    real(kind=rk)                                    :: y2_delta(size(y, 1)), t_delta_ini, t_delta_min
    real(kind=rk)                                    :: y_delta_delta(size(y, 1)), t_delta_fac_max, t_delta_fac_min
    real(kind=rk)                                    :: t_delta_fac_fdg, t_delta_nxt, sdf_tol, error_tol_abs(size(y, 1))
    real(kind=rk)                                    :: error_tol_rel(size(y, 1)), comb_err_tol(size(y, 1)), t_cv, t_delta
    real(kind=rk)                                    :: sp_new_t_delta, bs_tmp1_t_delta, bs_tmp2_t_delta, bs_tmp_t_delta
    real(kind=rk)                                    :: bs_tmp_y_delta(size(y, 1)), bs_tmp1_dist, bs_tmp2_dist, bs_tmp_dist
    logical                                          :: t_delta_end_p
    ! Process arguments
    error_tol_abs = error_tol_abs_ai
    if (present(error_tol_abs_o)) then
       if (size(error_tol_abs_o, 1) < size(y, 1)) then
          error_tol_abs = error_tol_abs_o(1)
       else
          error_tol_abs = error_tol_abs_o
       end if
    end if
    error_tol_rel = error_tol_rel_ai
    if (present(error_tol_rel_o)) then
       if (size(error_tol_rel_o, 1) < size(y, 1)) then
          error_tol_rel = error_tol_rel_o(1)
       else
          error_tol_rel = error_tol_rel_o
       end if
    end if
    sdf_tol = sdf_tol_ai
    if (present(sdf_tol_o)) sdf_tol = sdf_tol_o
    t_delta_fac_fdg = t_delta_fac_fdg_ai
    if (present(t_delta_fac_fdg_o))  t_delta_fac_fdg = t_delta_fac_fdg_o
    max_bisect = max_bisect_ai
    if (present(max_bisect_o)) max_bisect = max_bisect_o
    t_delta_fac_max = t_delta_fac_max_ai
    if (present(t_delta_fac_max_o)) t_delta_fac_max = t_delta_fac_max_o
    t_delta_fac_min = t_delta_fac_min_ai
    if (present(t_delta_fac_min_o)) t_delta_fac_min = t_delta_fac_min_o
    t_delta_min = t_delta_tiny
    if (present(t_delta_min_o)) t_delta_min = t_delta_min_o
    if (present(t_delta_ini_o)) then
       t_delta_ini = t_delta_ini_o
    else 
       t_delta_ini = t_delta_ai
       if (present(t_delta_max_o))  t_delta_ini = (t_delta_max_o - t_delta_min) / 2
    end if
    max_pts = size(solution, 2)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o);
    no_bisect_error = .false.
    if (present(no_bisect_error_o)) no_bisect_error = no_bisect_error_o
    ! Compute solution
    y_dim = size(y, 1)
    istats = 0
    t_delta = t_delta_ini
    t_cv = t
    y_cv = y
    cur_pnt_idx = 1
    solution(1, cur_pnt_idx) = t_cv
    solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
    do 
       cur_pnt_idx = cur_pnt_idx  + 1
       ! If close to the end, adjust t_delta to hit t_end_o
       t_delta_end_p = .false.
       if (present(t_end_o)) then
          if (t_cv+t_delta*1.10_rk >= t_end_o) then
             t_delta = t_end_o - t_cv
             t_delta_end_p = .true.
          end if
       end if
       ! Do step and adaptive step
       do adj_cnt=1,2
          call one_step_etab(status, y1_delta, y2_delta, dy, deq, t_cv, y_cv, param, a, b1, b2, c, t_delta)
          if (adj_cnt > 1) then
             istats(isi_step_y_err) = istats(isi_step_y_err) + 1
          else
             istats(isi_step_norm) = istats(isi_step_norm) + 1
          end if
          if (status > 0) return
          ! Compute new t_delta_nxt based on error estimate.
          y_delta_delta = abs(y1_delta-y2_delta)
          comb_err_tol  = (error_tol_rel + max(abs(solution(2:, cur_pnt_idx-1)), abs(y_cv + y1_delta)) * error_tol_rel)
          ! MJR TODO NOTE steps_adapt_etab: Fix this for when comb_err_tol has a zero entry!
          t_delta_fac   = (1/sqrt(sum((y_delta_delta / comb_err_tol) ** 2) / size(y, 1))) ** (1 + 1/min(p1, p2))
          if (t_delta_fac < 1.0_rk)  t_delta_fac = t_delta_fac * t_delta_fac_fdg
          if (cur_pnt_idx /= 2)      t_delta_fac = max(t_delta_fac_min, t_delta_fac)
          t_delta_fac = min(t_delta_fac_max, t_delta_fac)
          t_delta_nxt = t_delta * t_delta_fac
          if (present(t_delta_min_o)) then
             t_delta_nxt = max(t_delta_min_o, t_delta_nxt)
          end if
          if (present(t_delta_max_o)) then
             t_delta_nxt = min(t_delta_max_o, t_delta_nxt)
          end if
          ! Break out of loop if all is good.
          if (all(y_delta_delta <= comb_err_tol)) exit
          ! Redoing step
          t_delta = t_delta_nxt
          t_delta_end_p = .false.
          ! When redoing a step close to t_end_o, we don't want t_delta to get us more than half way to t_end_o
          if (present(t_end_o)) then
             if (t_cv + t_delta * 2 >= t_end_o) then
                t_delta = (t_end_o - t_cv) / 2.0_rk
             end if
          end if
       end do
       if (present(stepp_o)) then
          call stepp_o(status, sp_end_run, sp_sdf_flags, sp_new_t_delta, cur_pnt_idx, solution, t_delta, y1_delta)
          if (sp_new_t_delta > 0) then
             if (present(t_delta_min_o)) then
                sp_new_t_delta = max(t_delta_min_o, sp_new_t_delta)
             end if
             if (present(t_delta_max_o)) then
                sp_new_t_delta = min(t_delta_max_o, sp_new_t_delta)
             end if
             t_delta = sp_new_t_delta ! Leave t_delta_nxt unchanged...
             call one_step_stab(status, y1_delta, dy, deq, t_cv, y_cv, param, a, b1, c, t_delta)
             istats(isi_step_spp_td) = istats(isi_step_spp_td) + 1
          end if
          if (sp_sdf_flags > 0) then
             bs_tmp1_t_delta = t_delta_min
             call one_step_stab(status, bs_tmp_y_delta, dy, deq, t_cv, y_cv, param, a, b1, c, bs_tmp1_t_delta)
             istats(isi_step_sdf_bic) = istats(isi_step_sdf_bic) + 1
             if (status > 0) return
             call sdf_o(status, bs_tmp1_dist, sp_sdf_flags, t_cv+bs_tmp1_t_delta, y_cv+bs_tmp_y_delta)
             if (status > 0) return
             bs_tmp2_t_delta = t_delta
             call one_step_stab(status, bs_tmp_y_delta, dy, deq, t_cv, y_cv, param, a, b1, c, bs_tmp2_t_delta)
             istats(isi_step_sdf_bic) = istats(isi_step_sdf_bic) + 1
             if (status > 0) return
             call sdf_o(status, bs_tmp2_dist, sp_sdf_flags, t_cv+bs_tmp2_t_delta, y_cv+bs_tmp_y_delta)
             if (status > 0) return
             if (bs_tmp2_dist < bs_tmp1_dist) then ! Swap if req. 
                bs_tmp_t_delta  = bs_tmp1_t_delta
                bs_tmp1_t_delta = bs_tmp2_t_delta
                bs_tmp2_t_delta = bs_tmp_t_delta
                bs_tmp_dist     = bs_tmp1_dist
                bs_tmp1_dist    = bs_tmp2_dist
                bs_tmp2_dist    = bs_tmp_dist
             end if
             if (bs_tmp2_dist >= 0) then ! We don't exit if we can't bisect, we just don't do it.
                bs_itr = 1
                do 
                   bs_tmp_t_delta = (bs_tmp1_t_delta + bs_tmp2_t_delta) / 2.0_rk
                   call one_step_stab(status, bs_tmp_y_delta, dy, deq, t_cv, y_cv, param, a, b1, c, bs_tmp_t_delta)
                   istats(isi_step_sdf_bic) = istats(isi_step_sdf_bic) + 1
                   if (status > 0) return
                   call sdf_o(status, bs_tmp_dist, sp_sdf_flags, t_cv+bs_tmp_t_delta, y_cv+bs_tmp_y_delta)
                   if (status > 0) return
                   if (abs(bs_tmp_dist) <= sdf_tol) exit
                   if (bs_tmp_dist < 0.0_rk) then
                      bs_tmp1_t_delta = bs_tmp_t_delta
                      bs_tmp1_dist    = bs_tmp_dist
                   else
                      bs_tmp2_t_delta = bs_tmp_t_delta
                      bs_tmp2_dist    = bs_tmp_dist
                   end if
                   bs_itr = bs_itr + 1;
                   if (bs_itr > max_bisect) then
                      istats(isi_bic_fail_max) = istats(isi_bic_fail_max) + 1
                      if (no_bisect_error) then
                         exit
                      else
                         status = 1025
                         return
                      end if
                   end if
                end do
                t_delta  = bs_tmp_t_delta
                y1_delta = bs_tmp_y_delta
             else
                istats(isi_bic_fail_bnd) = istats(isi_bic_fail_bnd) + 1
             end if
          end if
       end if
       y_cv = y_cv + y1_delta
       t_cv = t_cv + t_delta
       t_delta = t_delta_nxt
       solution(1, cur_pnt_idx) = t_cv
       solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx-1) = dy
       solution(2:(2+y_dim-1), cur_pnt_idx) = y_cv
       istats(isi_num_pts) = istats(isi_num_pts) + 1
       status = 0;
       if (present(stepp_o)) then
          if (sp_end_run > 0) exit ! If we get here, then status was set by stepp_o
       end if
       if (present(t_max_o)) then
          if (t_cv > t_max_o) exit
       end if
       if (t_delta_end_p) exit
    end do
    ! Compute derivative for final solution point
    call deq(status, dy, t_cv, y_cv, param)  ! This sets return status
    if (status > 0) return
    solution((2+y_dim):(2+2*y_dim-1), cur_pnt_idx) = dy
    istats(isi_num_pts) = istats(isi_num_pts) + 1
  end subroutine steps_adapt_etab

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name Multistep Meta Solvers

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take a solution with t values pre-populated, and take steps_per_pnt RK steps between each t value.
  !!
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1348-1364 .. Error in this routine
  !!                                - 1248-1263 .. Error from one_step_stab()
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates (via steps_fixed_stab() calls):
  !!                                   - isi_num_pts
  !!                                   - isi_step_norm
  !! solution(:,:) ............... Array for solution.  
  !!                                Each COLUMN is a solution:
  !!                                 - First element is the t variable if
  !!                                 - This array *must* have a populated t sequence in new_solution(1,:)
  !!                                 - size(y, 1) elements starting with 2 have y values
  !!                                 - The next size(y, 1) elements have dy values
  !! deq ......................... Equation subroutine
  !! y(:) ........................ Initial conditions.  y is a column vector!
  !! param(:) .................... Data payload passed to deq
  !! a(:,:), b(:), c(:) .......... The butcher tableau
  !! steps_per_pnt ............... Number of RK steps to reach each point
  !! p_o ......................... The order for the RK method in the butcher tableau to enable Richardson extrapolation
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_utils::status_to_message(), mrkiss_solvers::one_step_stab()
  !!
  subroutine steps_points_stab(status, istats, solution, deq, y, param, a, b, c, steps_per_pnt, p_o) 
    use mrkiss_config, only: rk, istats_size
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                 :: deq
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:)
    integer,                    intent(in)  :: steps_per_pnt
    integer,          optional, intent(in)  :: p_o
    ! Vars
    integer                                 :: cur_pnt_idx, y_dim, jstats(istats_size), p
    real(kind=rk)                           :: dy(size(y, 1))
    ! Process arguments
    p = 0
    if (present(p_o)) p = p_o
    ! Compute Solution
    y_dim = size(y, 1)
    istats = 0
    solution(2:(2+y_dim-1), 1) = y
    call deq(status, dy, &    ! wt2nt:IGNORE
         solution(1, 1), &    ! wt2nt:DELETE
         y, param)
    if (status > 0) return
    solution((2+y_dim):(2+2*y_dim-1), 1) = dy
    do cur_pnt_idx=2,size(solution, 2)
       call steps_fixed_stab(status, jstats, solution(:, cur_pnt_idx:cur_pnt_idx), deq, &   ! wt2nt:IGNORE
                                solution(1, cur_pnt_idx-1),                                &   ! wt2nt:DELETE
                                solution(2:(2+y_dim-1), cur_pnt_idx-1), param, a, b, c, p, &
                                t_end_o=solution(1,cur_pnt_idx), max_pts_o=steps_per_pnt+1)
       istats = istats + jstats
       if (status > 0) return
    end do
  end subroutine steps_points_stab

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Create an interpolated solution from a source colution.
  !! 
  !! @verbatim
  !! status ...................... Exit status
  !!                                - -inf-0 ..... Everything worked
  !!                                - 0-255 ...... Evaluation of deq failed
  !!                                - 1331:1347 .. Error in this routine
  !!                                                - 1331 ...... solution t value out of bounds
  !!                                - others ..... No other values allowed
  !! istats(:) ................... Integer statistics for run
  !!                                - See: mrkiss_utils::print_istats() for description of elements.
  !!                                - Elements this routine updates:
  !!                                   - isi_num_pts
  !! solution(:,:) ............... Array for new solution.  
  !!                                This array *must* have a populated t sequence in solution(1,:)
  !!                                It must also have room for a new y & dy coordinates.
  !!                                New y values are interpolated.  New dy values are coputed from deq.
  !! src_solution(:,:) ........... Array for old solution.  
  !!                                This array *must* have t!  It must have at least two solution points.
  !! num_src_pts_o ............... The number of solutions in src_solution.  Default infered from size of src_solution.
  !! linear_interp_o ............. If .true. do linear interpolation, and hermite otherwise.  Default: .false.
  !! @endverbatim
  !! @see mrkiss_utils::print_istats(), mrkiss_utils::status_to_message() 
  !!
  subroutine interpolate_solution(status, istats, solution, src_solution, deq, param, num_src_pts_o, linear_interp_o) 
    use :: mrkiss_config, only: rk, istats_size, isi_num_pts
    implicit none
    ! Arguments
    integer,                    intent(out)   :: status, istats(istats_size)
    real(kind=rk),              intent(inout) :: solution(:,:)
    real(kind=rk),              intent(in)    :: src_solution(:,:)
    integer,          optional, intent(in)    :: num_src_pts_o
    procedure(deq_iface)                   :: deq
    real(kind=rk),              intent(in)    :: param(:)
    logical,          optional, intent(in)    :: linear_interp_o
    ! Variables
    integer                                   :: new_sol_idx, old_sol_idx, max_idx, num_src_pts, y_dim
    logical                                   :: linear_interp
    real(kind=rk)                             :: t, t0, t1, tu, td
    real(kind=rk), allocatable                :: y0(:), y1(:), dy0(:), dy1(:), yat(:)
    ! Process Arguments
    linear_interp = .false.
    if (present(linear_interp_o)) linear_interp = linear_interp_o
    num_src_pts = size(src_solution, 2)
    if (present(num_src_pts_o)) num_src_pts = min(num_src_pts_o, size(src_solution, 2))
    ! Compute value
    istats = 0
    y_dim = (size(src_solution, 1) - 1) /2
    max_idx = size(solution, 2)
    old_sol_idx = 2
    do new_sol_idx=1, max_idx
       t = solution(1, new_sol_idx)
       do while (t > src_solution(1, old_sol_idx))
          old_sol_idx = old_sol_idx + 1
          if (old_sol_idx > num_src_pts) then
             status = 1331
             return
          end if
       end do
       ! If we get here, we are in the interval we want
       t0 = src_solution(1, old_sol_idx-1)
       t1 = src_solution(1, old_sol_idx)
       y0 = src_solution(2:(2+y_dim-1), old_sol_idx-1)
       y1 = src_solution(2:(2+y_dim-1), old_sol_idx)
       td = (t1 - t0)
       if (linear_interp) then
          yat = (y0 * (t1 - t) + y1 * (t - t0)) / td
       else
          dy0 = td * src_solution((2+y_dim):(2+2*y_dim-1), old_sol_idx-1)
          dy1 = td * src_solution((2+y_dim):(2+2*y_dim-1), old_sol_idx)
          tu  = (t - t0) / td
          yat = tu * (tu * (tu * (2 * y0 + dy0 - 2 * y1 + dy1) - 2 * dy0 - 3 * y0 + 3 * y1 - dy1) + dy0) + y0
       end if
       solution(2:(2+y_dim-1), new_sol_idx) = yat
       call deq(status, solution((2+y_dim):(2+2*y_dim-1), new_sol_idx), & ! wt2nt:IGNORE
                t,                                                      & ! wt2nt:DELETE
                yat, param)
       if (status > 0) return
       istats(isi_num_pts) = istats(isi_num_pts) + 1
    end do
    status = 0;
  end subroutine interpolate_solution
  
end module mrkiss_solvers_wt



