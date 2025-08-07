! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_heun_euler_2_1.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Heun & Euler 2 step, order (2,1) Runge-Kutta method.@EOL
!! @keywords  ode ivp differential equation initial value problem rk
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
!> Butcher tableau for Heun & Euler 2 step, order (2,1) Runge-Kutta method
!!
!! IMO: Included for historical reasons.
!!
!! Known Aliases: 'RK21' (Butcher), 'ARKODE_HEUN_EULER_2_1_2' (SUNDIALS)
!! Known Aliases p1: 'the trapezoidal rule', 'explicit trapezoidal rule'
!! Known Aliases p2: 'mrkiss_erk_euler_1' (MRKISS)
!!
!! References:
!!  - Butcher (2016); Numerical Methods for Ordinary Differential Equations. 3rd Ed; Wiley; p98-99
!!
module mrkiss_eerk_heun_euler_2_1
  use mrkiss_config, only: rk, ik
  implicit none
  public
  !> The order of the overall method
  integer(kind=ik), parameter :: s      = 2
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: a(s,s) = reshape([  0.0_rk,  0.0_rk, &
                                                     1.0_rk,  0.0_rk], [s, s]) / 1.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: c(s)   = [          0.0_rk,  1.0_rk]          / 1.0_rk
  !> The order of the @f$\mathbf{b_1}@f$ method
  integer(kind=ik), parameter :: p1     = 2
  !> The @f$\mathbf{b_1}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: b1(s)  = [          1.0_rk,  1.0_rk]          / 2.0_rk
  !> The order of the @f$\mathbf{b_2}@f$ method
  integer(kind=ik), parameter :: p2     = 1
  !> Number of stages for the @f$\mathbf{b_2}@f$ method
  integer(kind=ik), parameter :: s2     = 1
  !> The @f$\mathbf{b_2}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: b2(s)  = [          1.0_rk,  0.0_rk]          / 1.0_rk
end module mrkiss_eerk_heun_euler_2_1
