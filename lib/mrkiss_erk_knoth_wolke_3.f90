! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_erk_knoth_wolke_3.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Knoth & wolke's 3 stage Runge-Kutta method of O(3).@EOL
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Butcher tableau for Knoth & wolke's 3 stage Runge-Kutta method of O(3)
!!
!! @image html erk_knoth_wolke_3-stab.png
!!
!! @par IMO
!! Included for historical reasons.
!!
!! @par Known Aliases
!! 'ARKODE_KNOTH_WOLKE_3_3' (SUNDIALS)
!!
!! @par Stability Image Links
!! <a href="erk_knoth_wolke_3-stab.png">  <img src="erk_knoth_wolke_3-stab.png"  width="256px"> </a>
!! <a href="erk_knoth_wolke_3-astab.png"> <img src="erk_knoth_wolke_3-astab.png" width="256px"> </a>
!! <a href="erk_knoth_wolke_3-star1.png"> <img src="erk_knoth_wolke_3-star1.png" width="256px"> </a>
!! <a href="erk_knoth_wolke_3-star2.png"> <img src="erk_knoth_wolke_3-star2.png" width="256px"> </a>
!!
!! @par References:
!!  - Knoth & Wolke (1988); Implicit-explicit runge-kutta methods for computiong atmospheric reactive flows; Appl. Num. Anal;
!!    28(2-4); p327-341
!!
module mrkiss_erk_knoth_wolke_3
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 3
  !> Number of methods
  integer,          parameter :: m      = 1
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([  0.0_rk,  0.0_rk, 0.0_rk, &
                                                    16.0_rk,  0.0_rk, 0.0_rk, &
                                                    -9.0_rk, 45.0_rk, 0.0_rk], [s, s]) / 48.0_rk
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b(s,m) = reshape([  5.0_rk, 9.0_rk, 16.0_rk], [s, m]) / 30.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: c(s)   = [          0.0_rk, 4.0_rk, 9.0_rk]           / 12.0_rk
  !> The method orders
  integer,          parameter :: p(m)   = [3]
  !> Number of stages for each method
  integer,          parameter :: se(m)  = [3]
end module mrkiss_erk_knoth_wolke_3
