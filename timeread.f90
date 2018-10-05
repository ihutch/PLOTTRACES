! Read back a timehistory file. Here just a working outline.

  real, allocatable, dimension(:) :: x
  real, allocatable, dimension(:,:,:) :: y

  open(13,file='timearray.dat',status='old')
  read(13,*)np,nf,nt,dt
  allocate (x(np))
  allocate (y(np,nf,nt))
  read(13,*)x
  read(13,*)y
  close(13)


  write(*,*)np,nf,nt,dt
  write(*,*)'        ifile=  1       2       3       4       5'
  write(*,*)'ip      x      y'
  do i=1,min(np,30)
     write(*,'(i3,6f8.3,'' ...'')')i,x(i),(y(i,j,1),j=1,min(5,nf))
  enddo
! Do with it what you want!

  call autocontour(y,np,np,nf)
  call pltend

  deallocate (x,y)
  end
