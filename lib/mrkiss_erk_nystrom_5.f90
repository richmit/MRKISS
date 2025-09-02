! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_erk_nystrom_5.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for the classic 4 stage Runge-Kutta method of O(4).@EOL
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
!> Butcher tableau for Nystrom's 6 stage Runge-Kutta method of O(5).
!!
!! IMO: I have included this method mostly for historical and reference reasons.  It is frequently used for comparisons and
!!      experiments in the literature.
!!
!! Color commentary: This method was proposed in Kutta (1901), and later corrected by Nystrom (1925).  The original appears in
!!                   an exercise in Hairer et al. (2009).
!!
!! Known Aliases: 'RKN5', 'RK41' (Butcher), & 'The Runge-Kutta Method'.
!!
!! @image html erk_nystrom_5-stab.png
!!
!! @par Stability Image Links
!! <a href="erk_nystrom_5-stab.png">  <img src="erk_nystrom_5-stab.png"  width="256px"> </a>
!! <a href="erk_nystrom_5-astab.png"> <img src="erk_nystrom_5-astab.png" width="256px"> </a>
!! <a href="erk_nystrom_5-star1.png"> <img src="erk_nystrom_5-star1.png" width="256px"> </a>
!! <a href="erk_nystrom_5-star2.png"> <img src="erk_nystrom_5-star2.png" width="256px"> </a>
!!
!! @par References:
!!  - Kutta (1901); Beitrag Zur N\"herungsweisen Integration Totaler Differentialgleichungen; Z. Math. Phys. 46; p435-53
!!  - Nystr\"om (1925); Uber die numerische Integration von Differentialgleichungen; Acta Soc. Sci. Fennicae; 50(13); 
!!  - Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems; p155; 
!!    zotero://select/items/0_VLZWN2CT
!!  - Butcher (2016); Numerical Methods for Ordinary Differential Equations. 3ed; p206; zotero://select/items/0_V7UTIRPT
!!
module mrkiss_erk_nystrom_5
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 6
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([     0.0_rk,      0.0_rk,     0.0_rk,   0.0_rk,   0.0_rk,   0.0_rk, &
       &                                             2700.0_rk,      0.0_rk,     0.0_rk,   0.0_rk,   0.0_rk,   0.0_rk, &
       &                                             1296.0_rk,   1944.0_rk,     0.0_rk,   0.0_rk,   0.0_rk,   0.0_rk, &
       &                                             2025.0_rk, -24300.0_rk, 30375.0_rk,   0.0_rk,   0.0_rk,   0.0_rk, &
       &                                              600.0_rk,   9000.0_rk, -5000.0_rk, 800.0_rk,   0.0_rk,   0.0_rk, &
       &                                              648.0_rk,   3888.0_rk,  1080.0_rk, 864.0_rk,   0.0_rk,   0.0_rk], [s, s]) / 8100.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: c(s)   = [             0.0_rk,      5.0_rk,     6.0_rk,  15.0_rk,  10.0_rk,  12.0_rk]          /   15.0_rk
  !> The order of the method
  integer,          parameter :: p      = 5
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: b(s)   = [            23.0_rk,      0.0_rk,   125.0_rk,   0.0_rk, -81.0_rk, 125.0_rk]          /  192.0_rk
end module mrkiss_erk_nystrom_5
