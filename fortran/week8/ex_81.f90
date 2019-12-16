program ex_81
implicit none
real(8)::a,b,h,x,y,z,x0,y0,z0,c,f
integer::i,m,n
write(*,*)'a=','b='
read(*,*)a,b
open(1,file='ex_811.dat')
open(2,file='ex_812.dat')
open(3,file='ex_813.dat')
do n=2,4
 h=1d0/10d0**dble(n)
 m=nint((b-a)/h)
 x=0d0
 y=cos(1d0)
 z=-sin(1d0)
 write(*,*)n
 do i=1,m
  c=cos(exp(x))
  if(n==2) then
   write(1,*)x,c,y
  else
   if(n==4) then
    if(i-int(i/(m/200))*(m/200)==1) then
	 write(3,*)x,c,y
	else
	 
	end if
   else 
	if(n==3) then
     if(i-int(i/(m/200))*(m/200)==1) then
      write(2,*)x,c,y
     else
     
     end if
	 
	end if
	 
	 

	
   end if
  end if
  
  x0=x
  y0=y
  z0=z
  x=x0+h
  z=z0+f(x0,y0,z0)*h
  y=y0+h*z0
 end do
end do
close(1)
close(2)
close(3)

end program ex_81
 
function f(x,y,z)
implicit none
real(8)::x,y,z,f
f=-exp(2d0*x)*y+z
return
end function f