
      integer ntmax      
      parameter (ntmax=100)
      integer ntroff,nmask,jt1,ism,iticnum,markoff,jj,nannot
      integer ixof,iyof,ixsc,iysc,nstycyc,ncolumns
      logical lx,mx,ly,my,la,lw,lp,lj,ll
      real xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh
      character*256 ytitle,xtitle
      dimension nmask(ntmax),xrange(2),yrange(2)
      real xrit(2,ntmax),yrit(2,ntmax)
      
      common /cmdlncom/ ntroff,nmask,jt1,ism,iticnum,markoff,jj,
     $     lx,mx,ly,my,la,lw,lp,lj,ll,nannot,
     $     ixof,iyof,ixsc,iysc,nstycyc,ncolumns,
     $     xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh,
     $     ytitle,xtitle,xrit,yrit
