program ex_82
implicit none
real(8)::a,b,x,h,y,z,y0,y1,x1,y2,f,g,z0,x0,z1
integer::n,i

write(*,*)'a=','b='
read(*,*)a,b
open(1,file='ex_821.dat')
x0=a
h=1d0/10d0**(3d0)
y0=1d0
z0=-1d0/4d0 !y'!
n=nint(abs(b-a)/h)
do i=1,n
 call oira(x0,y0,z0,h,x,y,z)
 if(i-int(i/(n/200))*n/200==1) then
  write(1,*)x,y
 end if
 x0=x+h
 y0=y
 z0=z
end do
close(1)
open(2,file='ex_822.dat')
h=1d0/10d0**3d0
n=nint(abs(b-a)/h)
x1=a
y1=1d0
z1=-1d0/4d0 !y'!
y0=y1-h*z1+h**2d0/2d0*f(x1,y1)
do i=1,n
 call velre(x1,y1,y0,h,y2)
 if(i-int(i/(n/200))*n/200==1) then
  write(2,*)x1,y1,g(x1)
 end if
 x1=x1+h
 y0=y1
 y1=y2
end do
close(2)
end program ex_82
function f(x,y)
implicit none
real(8)::x,y,f
f=exp(-x/4d0)/2d0*(-2*cos(x)+100d0*cos(10d0*x)+sin(x)-5d0*sin(10d0*x))+1d0/16d0*y
return
end function f

function g(x)
implicit none
real(8)::x,g
g=exp(-x/4d0)*((sin(5d0*x))**2d0+cos(x))
return
end function g

subroutine oira(x0,y0,z0,h,x,y,z)
implicit none
real(8)::x,y,z,x0,y0,z0,f,h
z=z0+h*f(x0,y0)
y=y0+h*z0
x=x0

return
end subroutine oira

subroutine velre(x1,y1,y0,h,y2)
implicit none
real(8)::y2,y1,h,f,x1,y0
y2=2d0*y1-y0+(h**2d0)*f(x1,y1)


return
end subroutine velre

