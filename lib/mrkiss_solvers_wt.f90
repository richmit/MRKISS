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
  !! @param status Exit status
  !!                 | Value     | Description
  !!                 |-----------|------------
  !!                 | -inf-0    | Everything worked
  !!                 | 1-255     | Error in this routine
  !! @param t      Value for @f$t@f$.
  !! @param y      Value for @f$\mathbf{y}@f$.
  !! @param param  Data payload usually used for constants.
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
  !! @param status      Exit status
  !!                      | Value     | Description
  !!                      |-----------|------------
  !!                      | -inf-0    | Everything worked
  !!                      | 256-511   | Error in this routine
  !! @param end_run     Return used to trigger the calling solver to stop the run when `end_run` is positive.
  !! @param sdf_flags   Return used to trigger SDF run in the calling solver, and pass information to `sdf_iface` function
  !! @param new_t_delta Returns a new value for @f$\Delta{t}@f$.  Used by calling solver if positive.
  !! @param pnt_idx     Index of current point in `solution`
  !! @param solution    Solution
  !! @param t_delta     Value from this step's @f$\Delta{t}@f$
  !! @param y_delta     Value from for this step's @f$\mathbf{\Delta{y}}@f$
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
  !! @param status     Exit status
  !!                     | Value     | Description
  !!                     |-----------|------------
  !!                     | -inf-0    | Everything worked
  !!                     | 512-767   | Error in this routine
  !! @param dist       The distance value of the SDF funciton
  !! @param sdf_flags  Flags passed from an `stepp_iface` routine
  !! @param t          Value for @f$t@f$.
  !! @param y          Value for @f$\mathbf{y}@f$.
  !! @param param      Data payload usually used for constants.
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
  !! @param status    Exit status
  !!                    | Value     | Description
  !!                    |-----------|------------
  !!                    | -inf-0    | Everything worked
  !!                    | 0-255     | Evaluation of @p deq failed
  !!                    | 1232-1247 | Error in this routine
  !! @param y1_delta  Returned @f$\mathbf{\Delta\check{y}}@f$ for the @f$\mathbf{\check{b}}@f$ (@p b1) method
  !! @param y2_delta  Returned @f$\mathbf{\Delta\hat{y}}@f$ for the @f$\mathbf{\hat{b}}@f$ (@p b2) method
  !! @param dy        Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq       The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t         Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y         Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param     Data payload passed to @p deq
  !! @param a         The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b1        The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param b2        The butcher tableau @f$\mathbf{\hat{b}}@f$ vector
  !! @param c         The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param t_delta   The @f$\Delta{t}@f$ value for this step.
  !!
  subroutine one_step_etab(status, y1_delta, y2_delta, dy, deq, t, y, param, a, b1, b2, c, t_delta)
    use mrkiss_config, only: rk, zero_epsilon
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)          :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b1(:), c(:), t_delta, b2(:)
    ! Variables
    integer                       :: i, stage
    real(kind=rk)                 :: k(size(y, 1),size(b1, 1)+1), stage_t_delta, y_tmp(size(y, 1)), stage_y_delta(size(y, 1))
    ! Compute k vectors
    call deq(status, dy, t, y, param)
    if (status > 0) return
    k(:,1) = dy * t_delta
    do stage=2,size(b1, 1)
       stage_y_delta = 0.0_rk
       do i=1,(stage-1)
          if (abs(a(i,stage)) > zero_epsilon) then
             stage_y_delta = stage_y_delta + a(i,stage) * k(:,i)
          end if
       end do
       stage_t_delta = t_delta*c(stage)
       call deq(status, y_tmp, t+stage_t_delta, y+stage_y_delta, param)
       if (status > 0) return
       k(:,stage) = y_tmp * t_delta
    end do
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
  !! @param status   Exit status
  !!                   | Value     | Description
  !!                   |-----------|------------
  !!                   | -inf-0    | Everything worked
  !!                   | 0-255     | Evaluation of @p deq failed
  !!                   | 1248-1263 | Error in this routine
  !! @param y_delta  Returned @f$\mathbf{\Delta{y}}@f$ for the method
  !! @param dy       Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq      The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t        Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y        Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param    Data payload passed to @p deq
  !! @param a        The butcher tableau
  !! @param b        The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !!                 The number of stages is determined based on the length of b.  All of the methods in an EERK need not
  !!                 be the same number of stages.  When this occurs, the b1 or b2 pulled from the module can be shortened
  !!                 when passing it to this function.  This will improve performance by not executing an unnecessary
  !!                 stage.
  !! @param c        The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param t_delta  The @f$\Delta{t}@f$ value for this step.
  !!
  ! SHELLO: sed -n '/^  *subroutine one_step_etab(/,/end subroutine one_step_etab *$/p' mrkiss_solvers_wt.f90 | sed 's/, y2_delta[^,]*//; s/, b2[^,]*//; s/_etab/_stab/; s/b1/b/g; s/y1/y/g; /y2_delta/d;'
  subroutine one_step_stab(status, y_delta, dy, deq, t, y, param, a, b, c, t_delta)
    use mrkiss_config, only: rk, zero_epsilon
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)          :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), t_delta
    ! Variables
    integer                       :: i, stage
    real(kind=rk)                 :: k(size(y, 1),size(b, 1)+1), stage_t_delta, y_tmp(size(y, 1)), stage_y_delta(size(y, 1))
    ! Compute k vectors
    call deq(status, dy, t, y, param)
    if (status > 0) return
    k(:,1) = dy * t_delta
    do stage=2,size(b, 1)
       stage_y_delta = 0.0_rk
       do i=1,(stage-1)
          if (abs(a(i,stage)) > zero_epsilon) then
             stage_y_delta = stage_y_delta + a(i,stage) * k(:,i)
          end if
       end do
       stage_t_delta = t_delta*c(stage)
       call deq(status, y_tmp, t+stage_t_delta, y+stage_y_delta, param)
       if (status > 0) return
       k(:,stage) = y_tmp * t_delta
    end do
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
  !! @param status   Exit status
  !!                   | Value     | Description
  !!                   |-----------|------------
  !!                   | -inf-0    | Everything worked
  !!                   | 0-255     | Evaluation of @p deq failed
  !!                   | 1216-1231 | Error in this routine
  !!                   | 1248-1263 | Error from one_step_stab()
  !! @param y_delta  Returned @f$\mathbf{\Delta{y}}@f$ for the method
  !! @param dy       Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq      The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t        Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y        Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param    Data payload passed to @p deq
  !! @param a        The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b        The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param c        The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param p        The order for the RK method in the butcher tableau
  !! @param t_delta  The @f$\Delta{t}@f$ value for this step.
  !!
  subroutine one_richardson_step_stab(status, y_delta, dy, deq, t, y, param, a, b, c, p, t_delta)
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)          :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), t_delta
    integer,          intent(in)  :: p
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
  !! @param status   Exit status
  !!                   | Value     | Description
  !!                   |-----------|------------
  !!                   | -inf-0    | Everything worked
  !!                   | 0-255     | Evaluation of @p deq failed
  !!                   | 1200-1215 | Error in this routine
  !! @param y_delta  Returned @f$\mathbf{\Delta{y}}@f$ for the method
  !! @param dy       Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq      The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t        Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y        Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param    Data payload passed to @p deq
  !! @param t_delta  The @f$\Delta{t}@f$ value for this step.
  !!
  subroutine one_step_rk4(status, y_delta, dy, deq, t, y, param, t_delta)
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y_delta(:), dy(:)
    procedure(deq_iface)          :: deq
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
  !! @param status    Exit status
  !!                    | Value     | Description
  !!                    |-----------|------------
  !!                    | -inf-0    | Everything worked
  !!                    | 0-255     |  Evaluation of @p deq failed
  !!                    | 1184-1199 | Unknown error in this routine
  !! @param y1_delta  Returned @f$\mathbf{\Delta\check{y}}@f$ for the @f$\mathbf{\check{b}}@f$ (@p b1) method with @f$\mathcal{O}(4)@f$.
  !! @param y2_delta  Returned @f$\mathbf{\Delta\hat{y}}@f$ for the @f$\mathbf{\hat{b}}@f$ (@p b2) method with @f$\mathcal{O}(5)@f$.
  !! @param dy        Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq       The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t         Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y         Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param     Data payload passed to @p deq
  !! @param t_delta   The @f$\Delta{t}@f$ value for this step.
  !!
  subroutine one_step_rkf45(status, y1_delta, y2_delta, dy, deq, t, y, param, t_delta)
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)          :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), t_delta
    ! Variables
    real(kind=rk)                 :: k1(size(y, 1)), k2(size(y, 1)), k3(size(y, 1))
    real(kind=rk)                 :: k4(size(y, 1)), k5(size(y, 1)), k6(size(y, 1))
    ! Compute Step
    call deq(status, k1, t,                             y, param)
    if (status > 0) return
    dy = k1
    call deq(status, k2, t + t_delta*1.0_rk/4.0_rk,   y + t_delta * ( k1*1.0_rk/4.0_rk), param)
    if (status > 0) return                            
    call deq(status, k3, t + t_delta*3.0_rk/8.0_rk,   y + t_delta * ( k1*3.0_rk/32.0_rk      + k2*9.0_rk/32.0_rk), param)
    if (status > 0) return
    call deq(status, k4, t + t_delta*12.0_rk/13.0_rk, y + t_delta * ( k1*1932.0_rk/2197.0_rk - k2*7200.0_rk/2197.0_rk + k3*7296.0_rk/2197.0_rk), param)
    if (status > 0) return
    call deq(status, k5, t + t_delta,                 y + t_delta * ( k1*439.0_rk/216.0_rk   - k2*8.0_rk              + k3*3680.0_rk/513.0_rk  - k4*845.0_rk/4104.0_rk), param)
    if (status > 0) return
    call deq(status, k6, t + t_delta*1.0_rk/2.0_rk,   y + t_delta * (-k1*8.0_rk/27.0_rk      + k2*2.0_rk              - k3*3544.0_rk/2565.0_rk + k4*1859.0_rk/4104.0_rk - k5*11.0_rk/40.0_rk), param)
    if (status > 0) return
    y1_delta = t_delta * (k1*25.0_rk/216.0_rk + k3*1408.0_rk/2565.0_rk  + k4*2197.0_rk/4104.0_rk   - k5*1.0_rk/5.0_rk)
    y2_delta = t_delta * (k1*16.0_rk/135.0_rk + k3*6656.0_rk/12825.0_rk + k4*28561.0_rk/56430.0_rk - k5*9.0_rk/50.0_rk + k6*2.0_rk/55.0_rk)
    status = 0
  end subroutine one_step_rkf45

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute one step of DP45 (mrkiss_eerk_dormand_prince_5_4)
  !!
  !! @param status    Exit status
  !!                    | Value     | Description
  !!                    |-----------|------------
  !!                    | -inf-0    | Everything worked
  !!                    | 0-255     |  Evaluation of @p deq failed
  !!                    | 1263-1279 | Unknown error in this routine
  !! @param y1_delta  Returned @f$\mathbf{\Delta\check{y}}@f$ for the @f$\mathbf{\check{b}}@f$ (@p b1) method with @f$\mathcal{O}(5)@f$.
  !! @param y2_delta  Returned @f$\mathbf{\Delta\hat{y}}@f$ for the @f$\mathbf{\hat{b}}@f$ (@p b2) method with @f$\mathcal{O}(4)@f$.
  !! @param dy        Returned @f$\mathbf{y}'@f$ value at @f$t@f$
  !! @param deq       The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t         Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y         Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param     Data payload passed to @p deq
  !! @param t_delta   The @f$\Delta{t}@f$ value for this step.
  !!
  subroutine one_step_dp54(status, y1_delta, y2_delta, dy, deq, t, y, param, t_delta)
    use mrkiss_config, only: rk
    implicit none
    ! Arguments
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y1_delta(:), y2_delta(:), dy(:)
    procedure(deq_iface)          :: deq
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:), param(:), t_delta
    ! Variables
    real(kind=rk)                 :: k1(size(y, 1)), k2(size(y, 1)), k3(size(y, 1)), k4(size(y, 1))
    real(kind=rk)                 :: k5(size(y, 1)), k6(size(y, 1)), k7(size(y, 1))
    ! Compute Step
    call deq(status, k1, t,                        y, param)
    if (status > 0) return
    dy = k1;
    call deq(status, k2, t+t_delta/5.0_rk,         y + t_delta * (k1/5.0_rk), param)
    if (status > 0) return
    call deq(status, k3, t+t_delta*3.0_rk/10.0_rk, y + t_delta * (k1*3.0_rk/40.0_rk       + k2*9.0_rk/40.0_rk), param)
    if (status > 0) return                                                                
    call deq(status, k4, t+t_delta*4.0_rk/5.0_rk,  y + t_delta * (k1*44.0_rk/45.0_rk      - k2*56.0_rk/15.0_rk      + k3*32.0_rk/9.0_rk), param)
    if (status > 0) return
    call deq(status, k5, t+t_delta*8.0_rk/9.0_rk,  y + t_delta * (k1*19372.0_rk/6561.0_rk - k2*25360.0_rk/2187.0_rk + k3*64448.0_rk/6561.0_rk - k4*212.0_rk/729.0_rk), param)
    if (status > 0) return
    call deq(status, k6, t+t_delta,                y + t_delta * (k1*9017.0_rk/3168.0_rk  - k2*355.0_rk/33.0_rk     + k3*46732.0_rk/5247.0_rk + k4*49.0_rk/176.0_rk    - k5*5103.0_rk/18656.0_rk), param)
    if (status > 0) return
    call deq(status, k7, t+t_delta,                y + t_delta * (k1*35.0_rk/384.0_rk                               + k3*500.0_rk/1113.0_rk   + k4*125.0_rk/192.0_rk   - k5*2187.0_rk/6784.0_rk + k6*11.0_rk/84.0_rk), param)
    if (status > 0) return
    y1_delta = t_delta * (k1*35.0_rk/384.0_rk     + k3*500.0_rk/1113.0_rk   + k4*125.0_rk/192.0_rk - k5*2187.0_rk/6784.0_rk    + k6*11.0_rk/84.0_rk)
    y2_delta = t_delta * (k1*5179.0_rk/57600.0_rk + k3*7571.0_rk/16695.0_rk + k4*393.0_rk/640.0_rk - k5*92097.0_rk/339200.0_rk + k6*187.0_rk/2100.0_rk + k7*1.0_rk/40.0_rk)
    status = 0
  end subroutine one_step_dp54

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @name Multistep Solvers

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Take multiple fixed time steps with a simple RK method and store solutions in solution.
  !!
  !! @param status     Exit status
  !!                   | Value     | Description
  !!                   |-----------|------------
  !!                   | -inf-0    | Everything worked
  !!                   | 0-255     |  Evaluation of @p deq failed
  !!                   | 1120-1151 | Unnecessary error in this routine
  !!                   | 1216-1231 | Error from one_richardson_step_stab()
  !!                   | 1248-1263 | Error from one_step_stab()
  !! @param istats     Integer statistics for run
  !!                    - See: mrkiss_utils::print_istats() for description of elements.
  !!                    - Elements this routine updates
  !!                       - mrkiss_config::isi_num_pts
  !!                       - mrkiss_config::isi_stab_norm
  !! @param solution   Array for solution.
  !!                   Each COLUMN is a solution:
  !!                    - First element is the @f$t@f$ variable
  !!                    - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                    - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param deq        The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t          Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y          Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param      Data payload passed to @p deq
  !! @param a          The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b          The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param c          The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param p_o        The order for the RK method in the butcher tableau to enable Richardson extrapolation
  !! @param max_pts_o  Maximum number of points to put in @p solution.
  !!                   If `max_pts_o>1` & `size(solution, 2)==1`, then `max_pts_o-1` steps are taken and
  !!                   only the last solution is stored in solution.
  !! @param t_delta_o  Step size to use.
  !!                    - Default when @p t_end_o provided: `(t_end - t) / (size(solution, 2) - 1)`
  !!                    - Default othewise: mrkiss_config::t_delta_ai
  !! @param t_end_o    End point for last step.  Silently ignored if @p t_delta_o is provided.
  !! @param t_max_o    Maximum value for @f$t@f$
  !!
  subroutine steps_fixed_stab(status, istats, solution, deq, t, y, param, a, b, c, p_o, max_pts_o, t_delta_o, &
                                 t_end_o, t_max_o)
    use mrkiss_config, only: rk, t_delta_ai, istats_size, isi_num_pts, isi_stab_norm
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                    :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:)
    integer,          optional, intent(in)  :: p_o, max_pts_o
    real(kind=rk),    optional, intent(in)  :: t_delta_o, t_end_o, t_max_o
    ! Vars
    integer                                 :: cur_pnt_idx, y_dim, cur_step, max_steps, p
    real(kind=rk)                           :: t_cv, t_delta, y_cv(size(y, 1)), y_delta(size(y, 1)), dy(size(y, 1))
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
          istats(isi_stab_norm) = istats(isi_stab_norm) + 3
       else
          call one_step_stab(status, y_delta, dy, deq, t_cv, y_cv, param, a, b, c, t_delta)
          istats(isi_stab_norm) = istats(isi_stab_norm) + 1
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
  !! This method attempts to precisely control the length of @f$\mathbf{\Delta{y}}@f$ which we denote by
  !! @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$.  This is done by finding a value for @f$\Delta{t}@f$ for which
  !! @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$ is equal to @p y_delta_len_targ with an accuracy of @p y_delta_len_tol_o.
  !!
  !! This value for @f$\Delta{t}@f$ is found via bisection.  For each solution point we first compute two RK steps with
  !! @f$\Delta{t}@f$ set to @p t_delta_min_o and @p t_delta_max.  We hope that the lengths of the two @f$\mathbf{\Delta{y}}@f$
  !! values thus obtained will bracket @p y_delta_len_targ.  If they don't then we can't isolate an appropriate value for
  !! @f$\Delta{t}@f$.  In this case we ignore the issue and continue to the next step if @p no_bisect_error_o is .TRUE.,
  !! otherwise we error out with a status code of 1024.  If the lengths of our @f$\mathbf{\Delta{y}}@f$ values bracket the
  !! target, then we use bisection to localize a value for @f$\Delta{t}@f$.
  !!
  !! The length, which we have denoted by @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$, is defined to be the Euclidean
  !! distance of the vector defined by the elements of @f$\mathbf{\Delta{y}}@f$ that are indexed by @p y_delta_len_idxs_o.
  !!
  !! Note there is no mathematical guarantee that a RK step of size @p t_delta_min_o and @p t_delta_max will produce solutions that
  !! bracket @p y_delta_len_targ.  That said, for well behaved functions a @p t_delta_min_o and @p t_delta_max may always be found
  !! that work.  Usually finding good values are @p t_delta_min_o and @p t_delta_max isn't difficult.  For challenging cases, a
  !! good approach is to use steps_fixed_stab() over the interval in question, and then examine the values of
  !! @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$ in the solution via analyze_solution().
  !!
  !! My primary use case for this function is to create uniform sphere sweeps for constructive solid geometry applications.
  !!
  !! @param status              Exit status
  !!                              | Value     | Description
  !!                              |-----------|------------
  !!                              | 0         | Everything worked
  !!                              | 0-255     | Evaluation of @p deq failed (see: deq_iface)
  !!                              | 1024      | bisection containment anomaly
  !!                              | 1025      | `no_bisect_error_o==0` not present and `max_bisect_o` violated
  !!                              | 1026-1055 | Unknown Error in this routine
  !!                              | 1248-1263 | Error from one_step_stab()
  !! @param istats              Integer statistics for run
  !!                             - See: mrkiss_config::istats_strs for description of elements.
  !!                             - Elements this routine updates
  !!                                - mrkiss_config::isi_num_pts
  !!                                - mrkiss_config::isi_stab_norm
  !!                                - mrkiss_config::isi_stab_y_len
  !!                                - mrkiss_config::isi_bic_fail_max
  !!                                - mrkiss_config::isi_bic_fail_bnd
  !! @param solution            Array for solution.
  !!                             Each COLUMN is a solution:
  !!                              - First element is the @f$t@f$ variable
  !!                              - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                              - The next `size(y, 1)` elements have dy values if
  !! @param deq                 The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t                   Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y                   Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param               Data payload passed to @p deq
  !! @param a                   The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b                   The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param c                   The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param y_delta_len_targ    Target length for @f$\mathbf{\Delta{y}}@f$ -- i.e. @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$
  !! @param t_delta_max         Maximum @f$\Delta{t}@f$
  !! @param t_delta_min_o       Minimum @f$\Delta{t}@f$
  !! @param y_delta_len_tol_o   How close we have to get to @p y_delta_len_targ.  Default is `y_delta_len_targ/100`
  !! @param y_delta_len_idxs_o  Components of @f$\mathbf{\Delta{y}}@f$ to use for length computation
  !! @param max_pts_o           Maximum number of points to put in @p solution.
  !! @param max_bisect_o        Maximum number of bisection iterations per each step.  Default: mrkiss_config::max_bisect_ai
  !! @param no_bisect_error_o   If `.TRUE.`, then do not exit on bisection errors
  !! @param y_sol_len_max_o     Maximum length of the solution curve
  !! @param t_max_o             Maximum value for @f$t@f$
  !!
  subroutine steps_condy_stab(status, istats, solution, deq, t, y, param, a, b, c, y_delta_len_targ,          &
                                 t_delta_max, t_delta_min_o, y_delta_len_tol_o, max_bisect_o, no_bisect_error_o, &
                                 y_delta_len_idxs_o, max_pts_o, y_sol_len_max_o, t_max_o)
    use mrkiss_config, only: rk, t_delta_min_ai, max_bisect_ai, istats_size, isi_bic_fail_bnd, isi_bic_fail_max, isi_num_pts, isi_stab_norm, isi_stab_y_len
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                    :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), y_delta_len_targ, t_delta_max
    real(kind=rk),    optional, intent(in)  :: t_delta_min_o, y_delta_len_tol_o, y_sol_len_max_o, t_max_o
    integer,          optional, intent(in)  :: max_pts_o, max_bisect_o, y_delta_len_idxs_o(:)
    logical,          optional, intent(in)  :: no_bisect_error_o
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
    t_delta_min = t_delta_min_ai
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
       istats(isi_stab_norm) = istats(isi_stab_norm) + 1
       if (status > 0) return
       if (present(y_delta_len_idxs_o)) then
          bs_tmp1_y_delta_len = norm2(bs_tmp1_y_delta(y_delta_len_idxs_o))
       else
          bs_tmp1_y_delta_len = norm2(bs_tmp1_y_delta)
       end if
       ! Compute upper t_delta
       bs_tmp2_t_delta = t_delta_max
       call one_step_stab(status, bs_tmp2_y_delta, bs_tmp2_dy, deq, t_cv, y_cv, param, a, b, c, bs_tmp2_t_delta)
       istats(isi_stab_norm) = istats(isi_stab_norm) + 1
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
             istats(isi_stab_y_len) = istats(isi_stab_y_len) + 1
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
  !! This method attempts to control the length of @f$\mathbf{\Delta{y}}@f$ which we denote by
  !! @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$.  At each solution step it takes a probing step with @f$\Delta{t}@f$ equal to
  !! @p t_delta_ini and then takes another step with @f$\Delta{t}@f$ equal to: `t_delta_ini * y_delta_len_targ / y_delta_len`
  !!
  !! If which will result in a @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$ approximately @p y_delta_len_targ when
  !! @f$\Delta{t}@f$ is proportional to @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$.  By default this second step is only
  !! taken when @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$ of the probe step is greater than @p y_delta_len_targ; however, it
  !! will be performed on shorter steps when @p adj_short_o is present -- in this mode it approximates the behavior
  !! @p steps_condy_stab() but is *much* faster.
  !!
  !! Note the assumption that @f$\Delta{t}@f$ is proportional to @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$.  We have no
  !! mathematical guarantee for this assumption; however, it is a fair approximation with well behaved functions when
  !! @p t_delta_ini is small enough.
  !!
  !! The length, which we have denoted by @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$, is defined to be the Euclidean
  !! distance of the vector defined by the elements of @f$\mathbf{\Delta{y}}@f$ that are indexed by @p y_delta_len_idxs_o.
  !!
  !! My primary use case for this function is to shrink down long steps for smoother curves/tubes in visualizations.
  !!
  !! @param status              Exit status
  !!                              | Value     | Description
  !!                              |-----------|------------
  !!                              | -inf-0    | Everything worked
  !!                              | 0-255     |  Evaluation of @p deq failed
  !!                              | 1280-1296 | Unknown error in this routine
  !!                              | 1248-1263 | Error from one_step_stab()
  !! @param istats              Integer statistics for run
  !!                             - See: mrkiss_utils::print_istats() for description of elements.
  !!                             - Elements this routine updates:
  !!                                - mrkiss_config::isi_num_pts
  !!                                - mrkiss_config::isi_stab_norm
  !!                                - mrkiss_config::isi_stab_y_len
  !! @param solution            Array for solution.
  !!                            Each COLUMN is a solution:
  !!                             - First element is the @f$t@f$ variable
  !!                             - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                             - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param deq                 The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t                   Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y                   Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param               Data payload passed to @p deq
  !! @param a                   The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b                   The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param c                   The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param y_delta_len_targ    Target length for @f$\mathbf{\Delta{y}}@f$ -- i.e. @f$\vert\vert\mathbf{\Delta{y}}\vert\vert_L@f$
  !! @param t_delta_ini         Test step @f$\Delta{t}@f$
  !! @param t_delta_max_o       Maximum @f$\Delta{t}@f$
  !! @param t_delta_min_o       Minimum @f$\Delta{t}@f$
  !! @param y_delta_len_idxs_o  Components of @f$\mathbf{\Delta{y}}@f$ to use for length computation
  !! @param adj_short_o         Adjust when @f$\Delta{t}@f$ is too short as well as when it's too long.
  !! @param max_pts_o           Maximum number of points to put in @p solution.
  !! @param y_sol_len_max_o     Maximum length of the solution curve
  !! @param t_max_o             Maximum value for @f$t@f$
  !!
  subroutine steps_sloppy_condy_stab(status, istats, solution, deq, t, y, param, a, b, c, y_delta_len_targ, t_delta_ini, &
                                        t_delta_min_o, t_delta_max_o, y_delta_len_idxs_o, adj_short_o, max_pts_o,        &
                                        y_sol_len_max_o, t_max_o)
    use mrkiss_config, only: rk, t_delta_min_ai, istats_size, isi_num_pts, isi_stab_norm, isi_stab_y_len
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                    :: deq
    real(kind=rk),              intent(in)  :: t
    real(kind=rk),              intent(in)  :: y(:), param(:), a(:,:), b(:), c(:), y_delta_len_targ, t_delta_ini
    real(kind=rk),    optional, intent(in)  :: t_delta_min_o, t_delta_max_o, y_sol_len_max_o, t_max_o
    integer,          optional, intent(in)  :: max_pts_o, y_delta_len_idxs_o(:), adj_short_o
    ! Variables
    integer                                 :: max_pts, cur_pnt_idx, y_dim
    real(kind=rk)                           :: t_delta_min, y_sol_len, t_cv, t_delta, y_delta_len
    real(kind=rk)                           :: y_cv(size(y, 1)), y_delta(size(y, 1)), dy(size(y, 1))
    ! Process arguments
    max_pts = size(solution, 2)
    if (present(max_pts_o)) max_pts = min(max_pts, max_pts_o);
    t_delta_min = t_delta_min_ai
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
       istats(isi_stab_norm) = istats(isi_stab_norm) + 1
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
          istats(isi_stab_y_len) = istats(isi_stab_y_len) + 1
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
  !! Method of controlling step size:
  !!
  !! First we compute a combined tolerance vector:
  !!    @f[ \mathbf{E} = [ A_i + R_i \max(\vert y_i\vert, \vert y_i+\Delta t\vert) ] @f]
  !! And a set containing the indexes of the non-zero elements of @f$\mathbf{E}@f$:
  !!    @f[ \mathbf{E_+} = \{i\vert\,E_i\ne0\} @f]
  !! When @f$\vert E_+\vert=0@f$, i.e. when @f$E_+=\emptyset@f$:
  !!   - We accept the current step
  !!   - Expand the next step by a factor of @p t_delta_fac_max_o
  !!
  !! Otherwise, we compute composite error:
  !!    @f[ \epsilon = \sqrt{\frac{1}{\vert E_+\vert} \sum_{E_+}\left(\frac{\vert\Delta\check{y}_i-\Delta\hat{y}_i\vert}{E_i}\right)^2}  @f]
  !! From this we compute the ideal step size adjustment ratio:
  !!    @f[ \left(\frac{1}{\epsilon}\right)^\frac{1}{1+\min(\hat{p}, \check{p})} @f]
  !! When @f$ \vert\Delta\check{y}_i-\Delta\hat{y}_i\vert < E_i\,\,\,\,\forall i@f$, we:
  !!   - We accept the current step
  !!   - Expand the next step by a factor of @f$m@f$
  !!
  !! Otherwise
  !!   - We recompute the current step with a @f$\Delta{t}@f$ reduced by @f$m@f$
  !!   - Recompute the error conditions
  !!   - Set the next step size based on the error conditions
  !!
  !! Notes:
  !!   - When @f$m<1@f$, the factor is adjusted by @p t_delta_fac_fdg_o
  !!   - The final value for @f$m<1@f$ is constrained by @p t_delta_fac_max_o & @p t_delta_fac_min_o.
  !!   - The final value for @f$\Delta{t}@f$ is always constrained by @p t_delta_min_o & @p t_delta_max_o.
  !!
  !! @param status             Exit status
  !!                             | Value     | Description
  !!                             |-----------|------------
  !!                             | -inf-0    | Everything worked
  !!                             | 0-255     |  Evaluation of @p deq failed
  !!                             | 256-511   | Error in @p stepp_o
  !!                             | 512-767   | Error in @p sdf_o
  !!                             | 1056      | @p no_bisect_error_o is `.FALSE`. and @p max_bisect_o violated.
  !!                             | 1057-1119 | Unknown error in this routine
  !!                             | 1232-1247 | Error from one_step_etab()
  !!                             | 1248-1263 | Error from one_step_stab()
  !! @param istats             Integer statistics for run
  !!                            - See: mrkiss_utils::print_istats() for description of elements.
  !!                            - Elements this routine updates:
  !!                               - mrkiss_config::isi_num_pts
  !!                               - mrkiss_config::isi_etab_norm
  !!                               - mrkiss_config::isi_etab_y_err
  !!                               - mrkiss_config::isi_stab_spp_td
  !!                               - mrkiss_config::isi_stab_sdf_bic
  !!                               - mrkiss_config::isi_bic_fail_max
  !!                               - mrkiss_config::isi_bic_fail_bnd
  !! @param solution           Array for solution.
  !!                           Each COLUMN is a solution:
  !!                            - First element is the @f$t@f$ variable
  !!                            - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                            - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param deq                The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param t                  Initial condition for @f$t@f$ -- i.e. @f$t_0@f$.
  !! @param y                  Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param              Data payload passed to @p deq
  !! @param a                  The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b1                 The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param b2                 The butcher tableau @f$\mathbf{\hat{b}}@f$ vector
  !! @param c                  The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param p1                 Order for the @f$\mathbf{\check{b}}@f$ (@p b1) RK method
  !! @param p2                 Order for the @f$\mathbf{\hat{b}}@f$ (@p b2) RK method
  !! @param t_max_o            Stop if @f$t@f$ becomes greater than @p t_max_o.  Different from @p t_end_o! Default: NONE
  !! @param t_end_o            Try to stop integration with @f$t@f$ equal to @p t_end. Default: NONE
  !! @param t_delta_ini_o      Initial @f$\Delta{t}@f$.
  !!                            - Default when @p t_delta_max provided: `(t_delta_max_o+t_delta_min_o)/2`
  !!                            - Default otherwise: mrkiss_config::t_delta_ai
  !! @param t_delta_min_o      Minimum allowed @f$\Delta{t}@f$. Default: mrkiss_config::t_delta_min_ai
  !! @param t_delta_max_o      Maximum allowed @f$\Delta{t}@f$. Default: NONE
  !! @param t_delta_fac_min_o  Minimum @f$\Delta{t}@f$ adaption factor for all but the first step.  Default: mrkiss_config::t_delta_fac_min_ai
  !! @param t_delta_fac_max_o  Maximum @f$\Delta{t}@f$ adaption factor.  Default:  2.0_rk
  !! @param t_delta_fac_fdg_o  Extra @f$\Delta{t}@f$ adaption factor when shrinking interval. Default: mrkiss_config::t_delta_fac_fdg_ai
  !! @param error_tol_abs_o    Absolute error tolerance. Default: mrkiss_config::error_tol_abs_ai
  !! @param error_tol_rel_o    Relative error tolerance. Default: mrkiss_config::error_tol_rel_ai
  !! @param max_pts_o          Maximum number of points to put in @p solution.
  !! @param max_bisect_o       Maximum number of bisection iterations to perform for each step. Default: mrkiss_config::max_bisect_ai
  !! @param no_bisect_error_o  If `.TRUE.`, then not exit on bisection errors
  !! @param sdf_o              SDF function.  Used to set new t_delta for a step.  This subroutine may trigger one or
  !!                           more of the following actions:
  !!                              | Return State    | Action
  !!                              |-----------------|-------
  !!                              | `status>0`      |  Immediately return doing nothing else and propagating status to caller.
  !!                              | `new_t_delta>0` |  Recompute step using @p new_t_delta for @f$\Delta{t}@f$
  !!                              | `sdf_flags>0`   |  Redo step with a @f$\Delta{t}@f$ derived from the @p sdf_o via bisection.
  !!                              | `end_run>0`     |  Routine returns after this step is complete.
  !! @param sdf_tol_o          How close we have to get to accept an sdf solution. Default: mrkiss_config::sdf_tol_ai
  !! @param stepp_o            Step processing subroutine.  Called after each step.
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
    procedure(deq_iface)                             :: deq
    real(kind=rk),                       intent(in)  :: t
    real(kind=rk),                       intent(in)  :: y(:), param(:), a(:,:), b1(:), b2(:), c(:)
    integer,                             intent(in)  :: p1, p2
    real(kind=rk),             optional, intent(in)  :: t_max_o, t_end_o, t_delta_ini_o, t_delta_min_o, t_delta_max_o
    real(kind=rk),             optional, intent(in)  :: t_delta_fac_min_o, t_delta_fac_max_o, t_delta_fac_fdg_o
    real(kind=rk),             optional, intent(in)  :: error_tol_abs_o(:), error_tol_rel_o(:), sdf_tol_o
    integer,                   optional, intent(in)  :: max_pts_o, max_bisect_o
    logical,                   optional, intent(in)  :: no_bisect_error_o
    procedure(sdf_iface),      optional              :: sdf_o
    procedure(stepp_iface),    optional              :: stepp_o
    ! Variables
    integer                                          :: max_pts, cur_pnt_idx, adj_cnt, y_dim, comb_err_nzc
    integer                                          :: max_bisect, sp_end_run, sp_sdf_flags, bs_itr
    logical                                          :: no_bisect_error, t_delta_end_p, comb_err_msk(size(y, 1))
    real(kind=rk)                                    :: t_delta_fac, y_cv(size(y, 1)), y1_delta(size(y, 1)), dy(size(y, 1))
    real(kind=rk)                                    :: y2_delta(size(y, 1)), t_delta_ini, t_delta_min
    real(kind=rk)                                    :: y_delta_delta(size(y, 1)), t_delta_fac_max, t_delta_fac_min
    real(kind=rk)                                    :: t_delta_fac_fdg, t_delta_nxt, sdf_tol, error_tol_abs(size(y, 1))
    real(kind=rk)                                    :: error_tol_rel(size(y, 1)), comb_err_tol(size(y, 1)), t_cv, t_delta
    real(kind=rk)                                    :: sp_new_t_delta, bs_tmp1_t_delta, bs_tmp2_t_delta, bs_tmp_t_delta
    real(kind=rk)                                    :: bs_tmp_y_delta(size(y, 1)), bs_tmp1_dist, bs_tmp2_dist, bs_tmp_dist
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
    t_delta_min = t_delta_min_ai
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
             istats(isi_etab_y_err) = istats(isi_etab_y_err) + 1
          else
             istats(isi_etab_norm) = istats(isi_etab_norm) + 1
          end if
          if (status > 0) return
          ! Compute new t_delta_nxt based on error estimate.
          y_delta_delta = abs(y1_delta-y2_delta)
          comb_err_tol  = (error_tol_abs + max(abs(solution(2:, cur_pnt_idx-1)), abs(y_cv + y1_delta)) * error_tol_rel)
          comb_err_msk  = abs(comb_err_tol) > zero_epsilon
          comb_err_nzc  = count(comb_err_msk)
          if (comb_err_nzc == 0) then
             t_delta_fac = t_delta_fac_max
          else
             t_delta_fac   = (1/sqrt(sum((y_delta_delta / comb_err_tol) ** 2, comb_err_msk) / (comb_err_nzc))) ** (1 + 1/min(p1, p2))
             if (t_delta_fac < 1.0_rk)  t_delta_fac = t_delta_fac * t_delta_fac_fdg
             if (cur_pnt_idx /= 2)      t_delta_fac = max(t_delta_fac_min, t_delta_fac)
             t_delta_fac = min(t_delta_fac_max, t_delta_fac)
          end if
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
             istats(isi_stab_spp_td) = istats(isi_stab_spp_td) + 1
          end if
          if (sp_sdf_flags > 0) then
             bs_tmp1_t_delta = t_delta_min
             call one_step_stab(status, bs_tmp_y_delta, dy, deq, t_cv, y_cv, param, a, b1, c, bs_tmp1_t_delta)
             istats(isi_stab_sdf_bic) = istats(isi_stab_sdf_bic) + 1
             if (status > 0) return
             call sdf_o(status, bs_tmp1_dist, sp_sdf_flags, t_cv+bs_tmp1_t_delta, y_cv+bs_tmp_y_delta)
             if (status > 0) return
             bs_tmp2_t_delta = t_delta
             call one_step_stab(status, bs_tmp_y_delta, dy, deq, t_cv, y_cv, param, a, b1, c, bs_tmp2_t_delta)
             istats(isi_stab_sdf_bic) = istats(isi_stab_sdf_bic) + 1
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
                   istats(isi_stab_sdf_bic) = istats(isi_stab_sdf_bic) + 1
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
  !! @param status         Exit status
  !!                         | Value     | Description
  !!                         |-----------|------------
  !!                         | -inf-0    | Everything worked
  !!                         | 0-255     | Evaluation of @p deq failed
  !!                         | 1348-1364 | Error in this routine
  !!                         | 1248-1263 | Error from one_step_stab()
  !! @param istats         Integer statistics for run
  !!                        - See: mrkiss_utils::print_istats() for description of elements.
  !!                        - Elements this routine updates (via steps_fixed_stab() calls):
  !!                           - mrkiss_config::isi_num_pts
  !!                           - mrkiss_config::isi_stab_norm
  !! @param solution       Array for solution.
  !!                        - This array *must* have a populated @f$t@f$ sequence in new_`solution(1,:)`.
  !!                        - Each COLUMN is a solution:
  !!                          - First element is the @f$t@f$ variable if
  !!                          - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                          - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param deq            The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param y              Initial condition for @f$\mathbf{y}@f$ -- i.e. @f$\mathbf{y_0}@f$.
  !! @param param          Data payload passed to @p deq
  !! @param a              The butcher tableau @f$\mathbf{a}@f$ matrix
  !! @param b              The butcher tableau @f$\mathbf{\check{b}}@f$ vector
  !! @param c              The butcher tableau @f$\mathbf{c}@f$ vector
  !! @param steps_per_pnt  Number of RK steps to reach each solution point
  !! @param p_o            The order for the RK method in the butcher tableau to enable Richardson extrapolation
  !!
  subroutine steps_points_stab(status, istats, solution, deq, y, param, a, b, c, steps_per_pnt, p_o)
    use mrkiss_config, only: rk, istats_size
    implicit none
    ! Arguments
    integer,                    intent(out) :: status, istats(istats_size)
    real(kind=rk),              intent(out) :: solution(:,:)
    procedure(deq_iface)                    :: deq
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
  !> Create an interpolated solution from a source solution.
  !!
  !! @param status           Exit status
  !!                           | Value     | Description
  !!                           |-----------|------------
  !!                           | -inf-0    | Everything worked
  !!                           | 0-255     | Evaluation of @p deq failed
  !!                           | 1331      | Solution @f$t@f$ value out of bounds
  !!                           | 1332:1347 | Unknown error in this routine
  !! @param istats           Integer statistics for run
  !!                          - See: mrkiss_utils::print_istats() for description of elements.
  !!                          - Elements this routine updates:
  !!                          - mrkiss_config::isi_num_pts
  !! @param solution         Array for new solution.
  !!                          - This array *must* have a populated @f$t@f$ sequence in solution(1,:)
  !!                          - New @f$\mathbf{y}@f$ values are interpolated.  New @f$\mathbf{y}'@f$ values are computed from @p deq.
  !!                          - Each COLUMN is a solution:
  !!                            - First element is the @f$t@f$ variable if
  !!                            - `size(y, 1)` elements starting with 2 have @f$\mathbf{y}@f$ values
  !!                            - The next `size(y, 1)` elements have @f$\mathbf{y}'@f$ values
  !! @param src_solution     Array for source solution.
  !!                          - This array *must* have at least two solution points.
  !! @param deq              The equation subroutine returning values for @f$\mathbf{y}'@f$ -- i.e. @f$\mathbf{f}(t, \mathbf{y})@f$
  !! @param param            Data payload passed to @p deq
  !! @param num_src_pts_o    The number of solutions in src_solution.  Default inferred from size of @p src_solution.
  !! @param linear_interp_o  If `.TRUE.` do linear interpolation, and Hermite otherwise.  Default: `.FALSE.`
  !!
  subroutine interpolate_solution(status, istats, solution, src_solution, deq, param, num_src_pts_o, linear_interp_o)
    use :: mrkiss_config, only: rk, istats_size, isi_num_pts
    implicit none
    ! Arguments
    integer,                    intent(out)   :: status, istats(istats_size)
    real(kind=rk),              intent(inout) :: solution(:,:)
    real(kind=rk),              intent(in)    :: src_solution(:,:)
    integer,          optional, intent(in)    :: num_src_pts_o
    procedure(deq_iface)                      :: deq
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
