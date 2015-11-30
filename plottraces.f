c      program plottraces
c Read in traces from files prescribed on the command line and plot them.
c The format of the files consists of one line with up to two integers
c np,nt followed by np lines of nt+1 floats which prescribe the x and nt
c y's for each of np points. If the second integer (nt) is absent, it is
c set to 1.  Any lines at the top of the file that do not start with an
c integer are skipped, as are lines at the end of the file. 

c Lines that start 'legend:' define a legend and increments the legend
c count. The legends are associated with the trace corresponding to the
c legend count.

c Lines that start 'Coordinate:' define a value of a coordinate
c associated with each trace, and should be placed after the data.
c The coordinate overrides the legend for that trace.

c Lines that start 'annotations:' define annotations at given position.

c Lines that start ':-' (colon minus) are considered to be additional
c arguments and are sent to the argument processor.

      include 'cmdlncom.f'

      character*256 argstr,charin,filename
      integer npmax
      parameter (npmax=5000)
      logical lpm(ntmax),ljm(ntmax)
      character*256 legends(ntmax),annotations(ntmax)
      real y(npmax,ntmax)
      real x(npmax,ntmax)
      real coordinate(ntmax)
      integer np(ntmax),ntraceno(ntmax),ifirst
      real yleginc
      data ifirst/1/
      data legends/ntmax*' '/
      data coordinate/ntmax*9.9e-20/
      data yleginc/0./

      il=0
      jj=1
      jt1=0
c---------------------------------------------
c Read command line arguments do loop.
      do i=1,iargc()
         charin=' '
         call getarg(i,argstr)
         if(lw)write(*,'(a,a)')'Argument:',argstr(1:lentrim(argstr))
c If argstr is blank, then we've exhausted the arguments. Exit.
         if(argstr(1:1) .eq. ' ')call exit(0)
         if(argstr(1:1) .ne. '-')then
c Non-switch argument is assumed to be a filename for reading.
            filename=argstr
            il=jj
            open(13,file=filename,status='old',err=106)
c Read lines from the data file. They can be of the form
c :-sx which acts just like cmdline switches.
            call linereading(il,lpm,ljm,legends,y,x,npmax,np,ntraceno,nt
     $           ,argstr,charin,filename,annotations,coordinate)
            close(13)
         else
c Intepret switch in cmdline.
            call cmdlineinterp(argstr)
         endif
      enddo
c---------------------------------------------
c End of reading command line arguments do loop.
      if(jj.eq.1)goto 106
      call pfset(3)
      if(iticnum.ne.0)call ticnumset(iticnum)
      if(cz.ne.1.)call charsize(cz*.015,cz*.015)
      write(*,*)'First 8 y-values of traces'
      if(jt1.gt.0)then
         i1=jt1
         i2=nt
      else
         i1=1
         i2=1
      endif
      itic=max(6,iticnum)
      icount=0
      ioldframe=0
      maxframe=0
      do it=i1,jj-1,i2
         maxframe=max(maxframe,nmask(it))
      enddo
      if(maxframe.gt.1)then
         if(ncolumns.gt.1)then
            call multiframe((maxframe+ncolumns-1)/ncolumns,ncolumns,1)
         else 
            call multiframe(maxframe,abs(ncolumns),1)
         endif
      endif
c Do over the traces:
      do it=i1,jj-1,i2
         icount=icount+1
         if(nmask(it).ne.0)then
            if(ioldframe.ne.nmask(it))then
               if(ioldframe.ne.0)call color(15)
               yleginc=(-(icount+markoff-1))*.05*cz
               if(ylego.eq.0..and.maxframe.gt.1)yleginc=yleginc+.05*cz
               if(ybh.ne.0.)call axregion(0.31,0.91,.1,.1+ybh)
c Possible individual frame plot ranges.
               if(yrit(1,nmask(it)).ne.yrit(2,nmask(it)))then
c Multi-frame ranges set separately.
                  call accisinit()
                  if(xrit(1,nmask(it)).ne.xrit(2,nmask(it)))then
                     call scalewn(xrit(1,nmask(it)),xrit(2,nmask(it))
     $                    ,yrit(1,nmask(it)),yrit(2,nmask(it)),lx,ly)
                  else
                     if(xrange(1).eq.0. .and. xrange(2).eq.0.)then
                        call minmax(x(1,it),np(1),xrange(1),xrange(2))
                        if(.not.lx)call fitrange(xrange(1),xrange(2)
     $                       ,itic,nxfac,xfac,xdelta,xrange(1)
     $                       ,xrange(2))
                     endif
                     call scalewn(xrange(1),xrange(2),yrit(1,nmask(it))
     $                    ,yrit(2,nmask(it)),lx,ly)
                  endif
               elseif(.not.(xrange(1).eq.0. .and. xrange(2).eq.0.))then
                  if(yrange(1).eq.0. .and. yrange(2).eq.0.)then
                     call minmax(y(1,it),np(1),ymin,ymax)
                     if(.not.ly)call fitrange(ymin,ymax,itic
     $                    ,nxfac,xfac,xdelta,ymin,ymax)
                  else
                     ymin=yrange(1)
                     ymax=yrange(2)
                  endif
                  if(la)then
                     call pltinaspect(xrange(1),xrange(2),ymin,ymax)
                  else
                     call accisinit()
                  endif
                  call scalewn(xrange(1),xrange(2),ymin,ymax,lx,ly)
               elseif(.not.(yrange(1).eq.0. .and. yrange(2).eq.0.))then
                  if(xrange(1).eq.0. .and. xrange(2).eq.0.)then
                     call minmax(x(1,it),np(1),xrange(1),xrange(2))
                     if(.not.lx)call fitrange(xrange(1),xrange(2),itic
     $                    ,nxfac,xfac,xdelta,xrange(1),xrange(2))
                  endif
                  if(la)then
                     call pltinaspect(xrange(1),xrange(2),yrange(1)
     $                    ,yrange(2))
                  else
                     call accisinit()
                  endif
                  call scalewn(xrange(1),xrange(2),yrange(1),yrange(2)
     $                 ,lx,ly)
               else
                  call lautoinit(x(1,it),y(1,it),np(it),lx,ly)
               endif
               if(cz.ne.1.)call charsize(cz*.015,cz*.015)
               call axis()
               call axis2()
               call winset(.true.)
               ifirst=0
               ioldframe=nmask(it)
            endif
c Plot the actual trace:
c            call color(mod((ntraceno(it)-1)/i2,nstycyc)+1)
            call color(mod(mod((ntraceno(it)-1)/i2,nstycyc),15)+1)
            call dashset(mod((ntraceno(it)-1)/i2,nstycyc))
            if(ljm(it))then
               ilablen=lentrim(linelabel(it))
               if(ilablen.le.0)then
                  call polyline(x(1,it),y(1,it),np(it))
               else
                  call labeline(x(1,it),y(1,it),np(it),
     $                 linelabel(it),ilablen)
               endif
            endif
            if(lpm(it))then
c               call color(mod((ntraceno(it+markoff)-1)/i2,nstycyc)+1)
               call color(mod(mod((ntraceno(it+markoff)-1)/i2,nstycyc),
     $              15)+1)
               call polymark(x(1,it),y(1,it),np(it),
     $              mod((ntraceno(it+markoff)-1)/i2,nstycyc)+1)
            endif
         endif
         write(*,'(i4,8f8.4)')it,(y(k,it),k=1,8)
c Legends: and Coordinates
         jl=lentrim(legends(icount))
         if(nmask(it).ne.0)then
            if(coordinate(icount).ne.9.9e-20)then
               call fwrite(coordinate(icount),iw,4,charin)
               legends(icount)=' '//charin(1:iw)
               jl=lentrim(legends(icount))
            elseif(jl.eq.0)then
               call iwrite(it,iw,argstr)
               call iwrite(np(it),iw2,charin)
               legends(icount)=argstr(1:iw)//' '//charin(1:iw2)
               jl=lentrim(legends(icount))
            endif
            if(jl.gt.1.and.ll)then
               call winset(.false.)
               if(lpm(it))then
                  ntr=mod((ntraceno(it+markoff)-1)/i2,nstycyc)+1
                  if(ljm(it))ntr=-ntr
                  call legendline(xlego,(icount+markoff)*.05*cz+ylego
     $                 +yleginc,ntr,legends(icount)(1:jl))
               else
                  call legendline(xlego,icount*.05*cz+ylego+yleginc,0,
     $                 legends(icount)(1:jl))
               endif
               call winset(.true.)
            endif
         else
            yleginc=yleginc-.05*cz
         endif
      enddo
      write(*,'(a,i3,a)')'Plotted',jj-1,'  traces'
      call color(15)
      if(xtitle(1:1).ne.' '.or.ytitle(1:1).ne.' ')then
         call winset(.false.)
         if(maxframe.gt.1)then
            call axlabels(xtitle(1:lentrim(xtitle)),' ')
            call charangl(90.)
            call jdrwstr(.03,.35,ytitle(1:lentrim(ytitle)),0.)
            call charangl(0.)
         else
            call axlabels(xtitle(1:lentrim(xtitle))
     $           ,ytitle(1:lentrim(ytitle)))
         endif
      endif
c Deal with annotations:
      do i=1,nannot
         read(annotations(i),*)xa,ya,argstr
         call drwstr(xa,ya,argstr(1:lentrim(argstr)))
      enddo

      call pltend
      call exit(0)

 106  call cmdlineinterp('-?')
      end
c******************************************************************
c Obtain the length of a string omitting trailing blanks.
      function lentrim(string)
      character*(*) string
      do i=len(string),1,-1
         if(string(i:i).ne.' ') goto 101
      enddo
      i=0
 101  lentrim=i
      end
c
c*********************************************************************
      subroutine cmdlineinterp(argstr)
c This is called successively for each commandline argument.
c It does whatever is dictated by the meaning of that switch.
c In this version the switch settings are passed back purely by
c the common cmdlinecom in cmdlinecom.f. Which must therefore be
c included in the calling routine appropriately.
c If those parameters need to be included in other commons, then
c a wrapper must be provided. In that case the common block passing
c can become problematic. 

      implicit none
      character*(*) argstr
      include 'cmdlncom.f'
      external cmdlnbd

      integer j,k

c Entry point of switch interpretation:
      if(argstr(1:3) .eq. '-lx') lx=.not.lx
      if(argstr(1:3) .eq. '-mx') mx=.not.mx
      if(argstr(1:3) .eq. '-ly') ly=.not.ly
      if(argstr(1:3) .eq. '-my') my=.not.my
      if(argstr(1:2) .eq. '-y') ytitle=argstr(3:)
      if(argstr(1:2) .eq. '-x') xtitle=argstr(3:)
      if(argstr(1:2) .eq. '-a') la=.true.
      if(argstr(1:2) .eq. '-v') lw=.true.
      if(argstr(1:2) .eq. '-p') lp=.not.lp
      if(argstr(1:2) .eq. '-j') lj=.not.lj
      if(argstr(1:3) .eq. '-nl') ll=.not.ll
      if(argstr(1:2) .eq. '-z')then
         read(argstr(3:),'(i5)',end=108,err=108)ntroff
         ntroff=-ntroff+jj-1
         goto 109
 108     ntroff=jj-1
 109     continue
         write(*,*)'ntroff=',ntroff
      endif
      if(argstr(1:2) .eq. '-t')then
         if(argstr(3:3).eq.'y')then
            read(argstr(4:),*,end=110,err=110)((yrit(k,j),k=1,2),j=1
     $           ,ntmax)
 110        continue
            write(*,'(a,6f8.4)')'Yranges',((yrit(k,j),k=1,2),j=1,3)
         elseif(argstr(3:3).eq.'n')then
            read(argstr(4:),*)ncolumns
         else
            read(argstr(3:),'(256i1)',end=107)nmask
 107        write(*,'(a,10i1)')'Mask starts ',(nmask(j),j=1,10)
         endif
      endif
      if(argstr(1:3) .eq. '-st')read(argstr(4:),*)jt1
      if(argstr(1:3) .eq. '-lo')read(argstr(4:),*)xlego,ylego
      if(argstr(1:3) .eq. '-rx') read(argstr(4:),*)xrange
      if(argstr(1:3) .eq. '-ry') read(argstr(4:),*)yrange
      if(argstr(1:3) .eq. '-sx') read(argstr(4:),*)xsf
      if(argstr(1:3) .eq. '-sy') read(argstr(4:),*)ysf
      if(argstr(1:3) .eq. '-sm') read(argstr(4:),*)ism
      if(argstr(1:3) .eq. '-ox') read(argstr(4:),*)xof
      if(argstr(1:3) .eq. '-oy') read(argstr(4:),*)yof
      if(argstr(1:3) .eq. '-oi') read(argstr(4:),*)ixof
      if(argstr(1:3) .eq. '-oj') read(argstr(4:),*)iyof
      if(argstr(1:3) .eq. '-si') read(argstr(4:),*)ixsc
      if(argstr(1:3) .eq. '-sj') read(argstr(4:),*)iysc
      if(argstr(1:5) .eq. '-spyx') read(argstr(6:),*)ysxp
      if(argstr(1:3) .eq. '-cz') read(argstr(4:),*)cz
      if(argstr(1:3) .eq. '-nt') read(argstr(4:),*)iticnum
      if(argstr(1:3) .eq. '-mo') read(argstr(4:),*)markoff
      if(argstr(1:3) .eq. '-ds') read(argstr(4:),*)nstycyc
      if(argstr(1:3) .eq. '-bh') read(argstr(4:),*)ybh
      if(argstr(1:3) .eq. '-ll') cllabel=argstr(4:)
      if(argstr(1:2) .eq. '-?') goto 106

      return
c-----------------------------------------------------------------
c Usage messages.
 106  write(*,*)'Usage: plottraces [-? -x -y ...] file1 [file2, ...]'
      write(*,*)' switches: -x"xtitle" -y"ytitle" -ly logarithmic-y',
     $     ' -my negate-y (toggles)'
      write(*,*)' -rx<min>,<max> -ry<min>,<max> specify plot range.'
     $     ,' -a retain aspect ratio.'
      write(*,*)' -sx<xsf> -sy<ysf> set scaling factors x and y'
      write(*,*)' -spyx<power> set power of x by which to divide y'
      write(*,*)' -st<num> set which one of multiple traces to plot'
      write(*,*)' -sm<num> smooth traces with triangular width <num>'
      write(*,*)' -ox<xof> -oy<yof> offset x or y by values.'
      write(*,*)' -oi<ixof> -oj<iyof> use x or y difference from'
     $     ,' their nth value.'
      write(*,*)' -si<ixsc> -oj<iysc> use x or y normalized to'
     $     ,' their nth value.'
      write(*,*)' -p toggle points, -j toggle line plotting.'
      write(*,*)' -nl toggle legends.'
      write(*,*)' -t1001... mask traces. -t112233... multiframe traces.'
      write(*,*)' -ty<y1n>,<y1x>,<y2n>,y2x>... set multiframe y-ranges'
      write(*,*)' -tn<num> multicolumns. If <0, just shrink plot width'
      write(*,*)' -v Verbose. Write out args, non-data lines read etc.'
      write(*,*)' -lo<xorig>,<yorig> box fraction origin of legends.'
     $     ,' -cz<size> Character scaling size.'
     $     ,' -nt<itics> set tic-number.'
      write(*,*)' -bh<height> Set height of plot box in normal units.'
      write(*,*)' -mo<int> offset count of marks vs lines/traces.'
      write(*,*)' -ds<int> set trace cycle number of line dash style.'
      write(*,*
     $     )' -z[ntroff] Offset trace count to 1 [or ntroff]' 
      write(*,*)' -ll.... set line label. -ll** says use filename.'
      write(*,*)'In data files,'
     $     ,' the first line starting with an integer is data start.'
      write(*,*)'Prior lines starting legend: define successive legends'
      write(*,*)'Legends remain aligned with traces if mask is used.'
      write(*,*)'Lines starting annotations: define annotations with '
     $     ,'  format: (2f10.4,a) x-y norm-position, string.'
      write(*,*)'Following lines starting coordinate: define values to'
     $     ,' label traces.'
      write(*,*)'Lines starting :- are interpreted as extra switches'

      call exit(0)

      end
c*********************************************************************
      block data cmdlnbd
c Separated block data program required by standard but not gnu.
      implicit none
      include 'cmdlncom.f'
      integer nrit
      parameter(nrit=2*ntmax)

      data ly/.false./my/.false./lx/.false./mx/.false./la/.false./
      data lp/.false./lj/.true./lw/.false./ll/.true./
      data xrange/0.,0./yrange/0.,0./
      data xtitle/' '/ytitle/' '/linelabel/ntmax*' '/cllabel/' '/
      data nmask/ntmax*1/ncolumns/1/
      data xlego/.2/ylego/0./cz/1./
      data xsf/1./ysf/1./ybh/0./ysxp/0./xof/0./yof/0./
      data ntroff/0/iticnum/0/
      data markoff/0/ism/0/
      data nannot/0/
      data ixof/0/iyof/0/ixsc/0/iysc/0/nstycyc/100/
      data xrit/nrit*0/yrit/nrit*0/

      end
c*********************************************************************
      subroutine linereading(il,lpm,ljm,legends,y,x,npmax,np,ntraceno,nt
     $     ,argstr,charin,filename,annotations,coordinate)
c This routine reads lines from an already opened file and interprets
c them either by calling the commandline interpreter if they are really
c switches, or else by its own logic as data lines etc.

      implicit none
      include 'cmdlncom.f' 

      character*(*) argstr,charin,filename,legends(ntmax)
     $     ,annotations(ntmax)
      integer npmax
      logical lpm(ntmax),ljm(ntmax)
      real y(npmax,ntmax)
      real x(npmax,ntmax)
      real coordinate(ntmax)
      integer np(ntmax),ntraceno(ntmax)
      integer il,nt
      real xxd,yyd

      integer j,k,ii
      
      integer lentrim
      external lentrim
      
c Loop till we run out of lines:
 101     read(13,'(a)',end=102)charin
         if(lw)write(*,*)charin(1:lentrim(charin))
         if(charin(1:11).eq.'annotation:')then
            nannot=nannot+1
            read(charin(12:),'(a)')annotations(nannot)
         elseif(charin(1:7).eq.'legend:')then
c Legend
            read(charin(8:),'(a)')legends(il)
            if(lentrim(legends(il)).eq.0)legends(il)=char(255)
            il=il+1
         endif
         if(charin(1:2).eq.':-')then
c Process a file-line switch
            argstr=charin(2:)
            if(lw)write(*,'(a,a)')
     $           'File Argument ',argstr(1:lentrim(argstr))
c by calling the processor.
            call cmdlineinterp(argstr)
         endif
         if(charin(1:11).eq.'Coordinate:')then
c Store real coordinate corresponding to each trace.
            read(charin(12:),*,end=111,err=111)(coordinate(j),j=1,nt)
 111        continue
c            write(*,*)(coordinate(j),j=1,20)
         endif

c Read the length of the next trace(s). 
c If there's no integer, then skip to the next line.
c If there's a second integer on this line, it's the number of traces.
         read(charin,*,err=101,end=101)np(jj)
         read(charin,*,err=103,end=103)np(jj),nt
         goto 104
 103     nt=1
c Store the lengths etc of these trace(s).
 104     do j=jj,jj+nt-1
            np(j)=np(jj)
            lpm(j)=lp
            ljm(j)=lj
            ntraceno(j)=j-ntroff
            if(cllabel(1:2).eq.'**')then
               linelabel(j)=filename
            else
               linelabel(j)=cllabel
            endif
         enddo
c Read and process the data for these trace(s). 
         do ii=1,np(jj)
            read(13,*,end=105,err=105)x(ii,jj),(y(ii,j),j=jj,jj+nt-1)
c Scaling x,y
            if(xsf.ne.1. .or. xof.ne.0.)then
               x(ii,jj)=xsf*(x(ii,jj)+xof)
            endif
            if(ysf.ne.1. .or. yof.ne.0.)then
               do j=jj,jj+nt-1
                  y(ii,j)=ysf*(y(ii,j)+yof)
               enddo
            endif
            if(ysxp.ne.0.)then
c Scaling y by a power of x.
               do j=jj,jj+nt-1
                  y(ii,j)=y(ii,j)/x(ii,j)**ysxp
               enddo
            endif
            if(my)then
c Negate y
               do j=jj,jj+nt-1
                  y(ii,j)=-y(ii,j)
               enddo
            endif
            do j=jj+1,jj+nt-1
c Set x-array the same for multiple y's
               x(ii,j)=x(ii,jj)
            enddo
         enddo
c Processing of entire traces starts here.
c Differencing from a particular position's value
         if(ixof.ne.0)then
            xxd=x(ixof,jj)
            do ii=1,np(jj)
               do j=jj+1,jj+nt-1
                  x(ii,j)=x(ii,j)-xxd
               enddo
            enddo
         endif
         if(iyof.ne.0)then
            do j=jj,jj+nt-1
               yyd=y(iyof,j)
               do ii=1,np(jj)
                  y(ii,j)=y(ii,j)-yyd
               enddo
            enddo
         endif
c Scaling (normalization) by a particular position's value
         if(ixsc.ne.0)then
            xxd=x(ixsc,jj)
            do ii=1,np(jj)
               do j=jj+1,jj+nt-1
                  x(ii,j)=x(ii,j)/xxd
               enddo
            enddo
         endif
         if(iysc.ne.0)then
            do j=jj,jj+nt-1
               yyd=y(iysc,j)
               do ii=1,np(jj)
                  y(ii,j)=y(ii,j)/yyd
               enddo
            enddo
         endif
         if(ism.ne.0)then
c Smooth traces
            do j=jj,jj+nt-1
c Average into an as yet unused trace
               call triangave(np(jj),ism,y(1,j),y(1,jj+nt))
c Copy back
               do ii=1,np(jj)
                  y(ii,j)=y(ii,jj+nt)
               enddo
            enddo
         endif
         write(*,'(a,i3,3a,30i4)') ' Read',nt, ' traces from '
     $        ,filename(1:lentrim(filename)),'. Length(s)'
     $        ,(np(jj+k),k=0,nt-1)
         jj=jj+nt
         goto 101
c End of looping over lines
c-------------------------------------------------------------
 105  write(*,*)'********* Error reading data at line',ii,' of',np(jj)
     $     ,nt,'Line=',argstr(1:lentrim(argstr))
      call exit()
 102  continue
      end
