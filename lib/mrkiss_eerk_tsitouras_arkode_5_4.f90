! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_tsitouras_arkode_5_4.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for ARKODE's Tsitouras 7 stage Order (5,4) Runge-Kutta method.@EOL
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
!> Butcher tableau for ARKODE's Tsitouras 7 stage Order (5,4) Runge-Kutta method
!!
!! IMO: This tableau came directly from the ARKODE source.  It's *not* the same one from tsitouras' paper.  It is also not the
!!      same one used in OrdinaryDiffEq.jl.  So far my testing has shown to be a solid method.
!!
!! Known Aliases: 'ARKODE_TSITOURAS_7_4_5' (SUNDIALS)
!!
!! References:
!!  - Tsitouras (2011); Runge-Kutta Pairs of Order 5(4) Satisfying Only the First Column Simplifying Assumption; Comp. & Math. w/ Appl. 62 (2); p770-775; zotero://select/items/0_WHVVHHDH
!!
module mrkiss_eerk_tsitouras_arkode_5_4
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 7
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: a(s,s) = reshape([ 0.000000000000000000_rk,   0.000000000000000000_rk, 0.000000000000000000_rk,  0.000000000000000000_rk,  0.000000000000000000_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                    0.161000000000000000_rk,   0.000000000000000000_rk, 0.000000000000000000_rk,  0.000000000000000000_rk,  0.000000000000000000_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                   -0.008480655492356989_rk,   0.335480655492357000_rk, 0.000000000000000000_rk,  0.000000000000000000_rk,  0.000000000000000000_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                    2.897153057105493000_rk,  -6.359448489975075000_rk, 4.362295432869581000_rk,  0.000000000000000000_rk,  0.000000000000000000_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                    5.325864828439257000_rk, -11.748883564062830000_rk, 7.495539342889836000_rk, -0.092495066361755250_rk,  0.000000000000000000_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                    5.861455442946420000_rk, -12.920969317847110000_rk, 8.159367898576159000_rk, -0.071584973281401000_rk, -0.028269050394068380_rk, 0.000000000000000000_rk, 0.000000000000000000_rk, &
                                                    0.096460766818065230_rk,   0.010000000000000000_rk, 0.479889650414499600_rk,  1.379008574103742000_rk, -3.290069515436081000_rk, 2.324710524099774000_rk, 0.000000000000000000_rk], [s, s])
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: c(s)   = [         0.000000000000000000_rk,   0.161000000000000000_rk, 0.327000000000000000_rk,  0.900000000000000000_rk,  0.980025540904509700_rk, 1.000000000000000000_rk, 1.000000000000000000_rk]
  !> The order of the @f$\mathbf{b_1}@f$ method
  integer,          parameter :: p1     = 5
  !> The @f$\mathbf{b_1}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: b1(s)  = [         0.096460766818065230_rk,   0.010000000000000000_rk, 0.479889650414499600_rk,  1.379008574103742000_rk, -3.290069515436081000_rk, 2.324710524099774000_rk, 0.000000000000000000_rk]
  !> The order of the @f$\mathbf{b_2}@f$ method
  integer,          parameter :: p2     = 4
  !> The @f$\mathbf{b_2}@f$ matrix for the Butcher Tableau
  real(kind=rk),    parameter :: b2(s)  = [         0.093523748581892710_rk,   0.008652883141566368_rk, 0.492893099131431900_rk,  1.140235412267858000_rk, -2.329180192439365000_rk, 1.568875049316616000_rk, 0.025000000000000000_rk]
end module mrkiss_eerk_tsitouras_arkode_5_4
