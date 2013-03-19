
      integer ntmax      
      parameter (ntmax=100)
      integer ntroff,nmask,jt1,ism,iticnum,markoff,jj
      logical lx,mx,ly,my,la,lw,lp,lj
      real xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh
      character*256 ytitle,xtitle
      dimension nmask(ntmax),xrange(2),yrange(2)
      
      common /cmdlncom/ ntroff,nmask,jt1,ism,iticnum,markoff,jj,
     $     lx,mx,ly,my,la,lw,lp,lj,
     $     xlego,ylego,xrange,yrange,xsf,ysf,xof,yof,ysxp,cz,ybh,
     $     ytitle,xtitle
