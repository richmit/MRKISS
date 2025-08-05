! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      statuscodes.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test status_to_origin.@EOL
!! @keywords  
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program statuscodes
  use :: mrkiss_config, only: rk, ik
  use :: mrkiss_utils,  only: seq, status_to_origin, status_to_message

  implicit none
  integer(kind=ik) :: status
  real(kind=rk)    :: t(10)
  integer          :: out_io_unit

  open(newunit=out_io_unit, file="statuscodes.out", form='formatted', action='write')
 
  call seq(status, t, from_o=0.0_rk, to_o=1.0_rk)
  write(out_io_unit, fmt='(a25, i0)') "   status code: ", status
  write(out_io_unit, fmt='(a25, a)')  " status origin: ", status_to_origin(status)
  write(out_io_unit, fmt='(a25, a)')  "status message: ", status_to_message(status)
  write(out_io_unit, fmt='(a)')

  call seq(status, t, from_o=0.0_rk, to_o=1.0_rk, step_o=1.0_rk)
  write(out_io_unit, fmt='(a25, i0)') "   status code: ", status
  write(out_io_unit, fmt='(a25, a)')  " status origin: ", status_to_origin(status)
  write(out_io_unit, fmt='(a25, a)')  "status message: ", status_to_message(status)
  write(out_io_unit, fmt='(a)')

  status = 1248
  write(out_io_unit, fmt='(a25, i0)') "   status code: ", status
  write(out_io_unit, fmt='(a25, a)')  " status origin: ", status_to_origin(status)
  write(out_io_unit, fmt='(a25, a)')  "status message: ", status_to_message(status)
  write(out_io_unit, fmt='(a)')

  status = 10000
  write(out_io_unit, fmt='(a25, i0)') "   status code: ", status
  write(out_io_unit, fmt='(a25, a)')  " status origin: ", status_to_origin(status)
  write(out_io_unit, fmt='(a25, a)')  "status message: ", status_to_message(status)
  write(out_io_unit, fmt='(a)')

  close(unit=out_io_unit, status='keep')

end program statuscodes
