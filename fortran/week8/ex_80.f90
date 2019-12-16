program ex_80
implicit none
real(8)::x(9),s,a
integer::i
open(1,file='8-0.dat')
 do i=1,9
  read(1,*)x(i)
 end do
close(1)
s=sum(x(:))
a=s/9d0
write(*,*)s,a
end program ex_80