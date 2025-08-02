

!----------------------------------------------------------------------------------------------------------------------------------
program seqt
  use            :: mrkiss_config,      only: rk, ik
  use            :: mrkiss_utils,       only: seq

  implicit none
  integer(kind=ik),  parameter :: num_num   = 11
  real(kind=rk),     parameter :: in_from   = 0.0_rk
  real(kind=rk),     parameter :: in_to     = 1.0_rk
  real(kind=rk),     parameter :: in_step   = 0.1_rk
  real(kind=rk),     parameter :: out_step  = in_step + 1.0_rk
  real(kind=rk)                :: t(num_num)
  integer                      :: out_io_unit
  integer(kind=ik)             :: status

  open(newunit=out_io_unit, file="seqt.out", form='formatted', action='write')

  t = -1
  call seq(status, t, from_o=in_from, to_o=in_to)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, from_o=in_from, step_o=in_step)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, from_o=in_from)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=out_step, from_o=in_from)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  close(unit=out_io_unit, status='keep')
end program seqt
