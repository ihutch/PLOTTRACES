c Maximum number of traces (and other things).
      integer ntmax,nfmax      
      parameter (ntmax=30,nfmax=2000)
c jj is the current trace pointer on entry to linereading
c it is updated when linereading is finished. j is the index within
c the current jj to jj+nt set of traces.
c ntroff is the nt offset such that ntraceno(j)=j-ntroff
      integer ntroff,nmask,jt1,ism,iticnum,markoff,jj,nannot,nf
      integer ixof,iyof,ixsc,iysc,nstycyc,ncolumns,numpf
      logical lx,mx,ly,my,la,lw,lp,lj,ll
      real xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh,dt
      character*256 ytitle,xtitle,linelabel(ntmax),cllabel
      dimension nmask(ntmax),xrange(2),yrange(2)
      real xrit(2,ntmax),yrit(2,ntmax)
      
      common /cmdlncom/ ntroff,nmask,jt1,ism,iticnum,markoff,jj,
     $     lx,mx,ly,my,la,lw,lp,lj,ll,nannot,nf,
     $     ixof,iyof,ixsc,iysc,nstycyc,ncolumns,
     $     xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh,
     $     ytitle,xtitle,linelabel,cllabel,xrit,yrit,dt,numpf
