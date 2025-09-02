! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_cash_karp_5_4.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Cash & Karp's 4 step, order (5,4) Runge-Kutta method.@EOL
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
!> Butcher tableau for Cash & Karp's 4 step, order (5,4) Runge-Kutta method
!!
!! IMO: I don't recommend this method as a 2 step embedded method for general use.  It was designed with five embedded methods
!!      for use with stiff applications.  It's an interesting method from both a historical and research perspective.  Note this
!!      module has all five embedded methods defined (b1/p1-b5/p5).
!!
!! Known Aliases: 'ARKODE_CASH_KARP_6_4_5' (SUNDIALS)
!!
!! @image html eerk_cash_karp_5_4-stab.png
!!
!! @par Stability Image Links
!! <a href="eerk_cash_karp_5_4-stab.png">  <img src="eerk_cash_karp_5_4-stab.png"  width="256px"> </a>
!! <a href="eerk_cash_karp_5_4-astab.png"> <img src="eerk_cash_karp_5_4-astab.png" width="256px"> </a><br>
!! <a href="eerk_cash_karp_5_4-star1.png"> <img src="eerk_cash_karp_5_4-star1.png" width="256px"> </a>
!! <a href="eerk_cash_karp_5_4-star2.png"> <img src="eerk_cash_karp_5_4-star2.png" width="256px"> </a>
!! <a href="eerk_cash_karp_5_4-star3.png"> <img src="eerk_cash_karp_5_4-star3.png" width="256px"> </a>
!! <a href="eerk_cash_karp_5_4-star4.png"> <img src="eerk_cash_karp_5_4-star4.png" width="256px"> </a>
!! <a href="eerk_cash_karp_5_4-star5.png"> <img src="eerk_cash_karp_5_4-star5.png" width="256px"> </a>
!!
!! @par References:
!!  - Cash & Karp(1990);  A variable order Runge-Kutta method for initial value problems with rapidly varying 
!!    right-hand sides; TOMS 16; zotero://select/items/0_2YSGGWSD
!!
module mrkiss_eerk_cash_karp_5_4
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 6
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([      0.0_rk,       0.0_rk,        0.0_rk,      0.0_rk,     0.0_rk,     0.0_rk, &
                                                    110592.0_rk,       0.0_rk,        0.0_rk,      0.0_rk,     0.0_rk,     0.0_rk, &
                                                     41472.0_rk,  124416.0_rk,        0.0_rk,      0.0_rk,     0.0_rk,     0.0_rk, &
                                                    165888.0_rk, -497664.0_rk,   663552.0_rk,      0.0_rk,     0.0_rk,     0.0_rk, &
                                                   -112640.0_rk, 1382400.0_rk, -1433600.0_rk, 716800.0_rk,     0.0_rk,     0.0_rk, &
                                                     16310.0_rk,  189000.0_rk,    23000.0_rk, 221375.0_rk, 34155.0_rk,     0.0_rk], [s, s]) / 552960.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: c(s)   = [              0.0_rk,       8.0_rk,       12.0_rk,     24.0_rk,    40.0_rk,    35.0_rk]          /     40.0_rk
  !> The order of the @f$\mathbf{b_1}@f$ method
  integer,          parameter :: p1     = 5
  !> The @f$\mathbf{b_1}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b1(s)  = [           9361.0_rk,       0.0_rk,    38500.0_rk,  20125.0_rk,     0.0_rk, 27648.0_rk]          /  95634.0_rk
  !> The order of the @f$\mathbf{b_2}@f$ method
  integer,          parameter :: p2     = 4
  !> The @f$\mathbf{b_2}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b2(s)  = [          39550.0_rk,       0.0_rk,   148600.0_rk,  94675.0_rk,  7479.0_rk, 96768.0_rk]          / 387072.0_rk
  !> The order of the @f$\mathbf{b_3}@f$ method
  integer,          parameter :: p3     = 3
  !> The @f$\mathbf{b_3}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b3(s)  = [             19.0_rk,       0.0_rk,      -20.0_rk,     55.0_rk,     0.0_rk,     0.0_rk]          /     54.0_rk
  !> The order of the @f$\mathbf{b_4}@f$ method
  integer,          parameter :: p4     = 2
  !> The @f$\mathbf{b_4}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b4(s)  = [             -3.0_rk,       5.0_rk,        0.0_rk,      0.0_rk,     0.0_rk,     0.0_rk]          /      2.0_rk
  !> The order of the @f$\mathbf{b_5}@f$ method
  integer,          parameter :: p5     = 1
  !> The @f$\mathbf{b_5}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b5(s)  = [              1.0_rk,       0.0_rk,        0.0_rk,      0.0_rk,     0.0_rk,     0.0_rk]          /      1.0_rk
end module mrkiss_eerk_cash_karp_5_4
