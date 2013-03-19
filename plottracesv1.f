      program plottraces
c Read in traces from files prescribed on the command line and plot them.
c The format of the files consists of one line with up to two integers
c np,nt followed by np lines of nt+1 floats which prescribe the x and nt
c y's for each of np points. If the second integer (nt) is absent, it is
c set to 1.  Any lines at the top of the file that do not start with an
c integer are skipped, as are lines at the end of the file. 

c However each of those skipped lines that starts 'legend:' defines a
c legend and increments the legend count. The legends are associated
c with the trace corresponding to the legend count.  Also lines that
c start with colon minus :- are considered to be additional arguments and
c are sent to the argument processor.

      character*256 argstr,charin,filename,xtitle,ytitle
      integer npmax
      integer ntmax
      logical ly,my,lx,mx,la,lp,lj,lw

      parameter (npmax=5000,ntmax=100)
      logical lpm(ntmax),ljm(ntmax)

      character*256 legends(ntmax)
      real y(npmax,ntmax)
      real x(npmax,ntmax)
      integer np(ntmax),ntraceno(ntmax),ntroff
      integer nmask(ntmax),ifirst
      integer markoff,iticnum,jt1,ism
      real xrange(2),yrange(2)
      real xlego,ylego,cz
      real xsf,ysf,ybh,ysxp,xof,yof
      data ly/.false./my/.false./lx/.false./mx/.false./la/.false./
      data lp/.false./lj/.true./lw/.false./
      data xrange/0.,0./yrange/0.,0./
      data legends/ntmax*' '/
      data xtitle/' '/ytitle/' '/
      data nmask/ntmax*1/ifirst/1/
      data xlego/.2/ylego/0./cz/1./
      data xsf/1./ysf/1./ybh/0./ysxp/0./xof/0./yof/0./
      data ntroff/0/iticnum/0/
      data markoff/0/ism/0/

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
c Enter a subsidiary loop programmed below in spagetti.
c It reads lines from the data file. They can be of the form
c :-sx which acts just like cmdline switches.
            goto 101
         endif
c Entry point of switch interpretation:
 111     if(argstr(1:3) .eq. '-lx') lx=.not.lx
         if(argstr(1:3) .eq. '-mx') mx=.not.mx
         if(argstr(1:3) .eq. '-ly') ly=.not.ly
         if(argstr(1:3) .eq. '-my') my=.not.my
         if(argstr(1:2) .eq. '-y') ytitle=argstr(3:)
         if(argstr(1:2) .eq. '-x') xtitle=argstr(3:)
         if(argstr(1:2) .eq. '-a') la=.true.
         if(argstr(1:2) .eq. '-v') lw=.true.
         if(argstr(1:2) .eq. '-p') lp=.not.lp
         if(argstr(1:2) .eq. '-j') lj=.not.lj
         if(argstr(1:2) .eq. '-z')then
            read(argstr(3:),'(i5)',end=108,err=108)ntroff
            ntroff=-ntroff+jj-1
            goto 109
 108        ntroff=jj-1
 109        continue
            write(*,*)'ntroff=',ntroff
         endif
         if(argstr(1:2) .eq. '-t')then
            read(argstr(3:),'(256i1)',end=107)nmask
 107        write(*,'(a,10i1)')'Mask starts ',(nmask(j),j=1,10)
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
         if(argstr(1:5) .eq. '-spyx') read(argstr(6:),*)ysxp
         if(argstr(1:3) .eq. '-cz') read(argstr(4:),*)cz
         if(argstr(1:3) .eq. '-nt') read(argstr(4:),*)iticnum
         if(argstr(1:3) .eq. '-mo') read(argstr(4:),*)markoff
         if(argstr(1:3) .eq. '-bh') read(argstr(4:),*)ybh
         if(argstr(1:2) .eq. '-?') goto 106
         if(charin.eq.' ')goto 110
c If charin is blank, we are not reading a file. Skip the next.
c-----------------------------------------------------------
c Here when we have an open file and we are reading from it.
c We loop over the lines while there are still lines to read:
c This is thus "while not exhausted file 13"
 101     read(13,'(a)',end=102)charin
         if(lw)write(*,*)charin(1:lentrim(charin))
         if(charin(1:7).eq.'legend:')then
c Legend
            read(charin(8:),'(a)')legends(il)
            if(lentrim(legends(il)).eq.0)legends(il)=char(255)
            il=il+1
         endif
         if(charin(1:2).eq.':-')then
c Process a file-line switch
            argstr=charin(2:)
            if(lw)write(*,'(a,a)')
     $           'File Argument:',argstr(1:lentrim(argstr))
c by going to the switch-processing entry point.
            goto 111
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
         enddo
c Read and process the data for these trace(s). 
         do ii=1,np(jj)
            read(13,*,end=105,err=105)x(ii,jj),(y(ii,j),j=jj,jj+nt-1)
c Scaling x,y
            if(xsf.ne.1. .or. xof.ne.0.)then
               x(ii,jj)=xsf*x(ii,jj)+xof
            endif
            if(ysf.ne.1. .or. yof.ne.0.)then
               do j=jj,jj+nt-1
                  y(ii,j)=ysf*y(ii,j)+yof
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
 102     continue
c Finished reading file. Close it.
         close(13)
 110     continue
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
      icount=0
      do it=i1,jj-1,i2
         icount=icount+1
         if(ifirst.eq.1.and.nmask(it).ne.0)then
            if(ybh.ne.0.)call axregion(0.31,0.91,.1,.1+ybh)
            if(.not.(xrange(1).eq.0. .and. xrange(2).eq.0.))then
               if(yrange(1).eq.0. .and. yrange(2).eq.0.)then
                  call minmax(y(1,it),np(1),yrange(1),yrange(2))
               endif
               if(la)then
                  call pltinaspect(xrange(1),xrange(2),yrange(1)
     $                 ,yrange(2))
               else
                  call pltinit(xrange(1),xrange(2),yrange(1),yrange(2))
               endif
               call scalewn(xrange(1),xrange(2),yrange(1),yrange(2),lx
     $              ,ly)
               call axis()
               call winset(.true.)
               call color((ntraceno(it)-1)/i2+1)
               call dashset((ntraceno(it)-1)/i2)
               if(ljm(it))call polyline(x(1,it),y(1,it),np(it))
               if(lpm(it))then
                  call color((ntraceno(it+markoff)-1)/i2+1)
                  call polymark(x(1,it),y(1,it),np(it),
     $                 (ntraceno(it+markoff)-1)/i2+1)
               endif
            elseif(.not.(yrange(1).eq.0. .and. yrange(2).eq.0.))then
               if(xrange(1).eq.0. .and. xrange(2).eq.0.)then
                  call minmax(x(1,it),np(1),xrange(1),xrange(2))
               endif
               if(la)then
                  call pltinaspect(xrange(1),xrange(2),yrange(1)
     $                 ,yrange(2))
               else
                  call pltinit(xrange(1),xrange(2),yrange(1),yrange(2))
               endif
               call scalewn(xrange(1),xrange(2),yrange(1),yrange(2),lx
     $              ,ly)
               call axis()
               call winset(.true.)
               call color((ntraceno(it)-1)/i2+1)
               call dashset((ntraceno(it)-1)/i2)
               if(ljm(it))call polyline(x(1,it),y(1,it),np(it))
               if(lpm(it))then
                  call color((ntraceno(it+markoff)-1)/i2+1)
                  call polymark(x(1,it),y(1,it),np(it),
     $                 (ntraceno(it+markoff)-1)/i2+1)
               endif
            else
               call lautoinit(x(1,it),y(1,it),np(it),lx,ly)
               call axis()
               call color((ntraceno(it)-1)/i2+1)
               call dashset((ntraceno(it)-1)/i2)
               if(ljm(it))call polyline(x(1,it),y(1,it),np(it))
               if(lpm(it))then
                  call color((ntraceno(it+markoff)-1)/i2+1)
                  call polymark(x(1,it),y(1,it),np(it),
     $                 (ntraceno(it+markoff)-1)/i2+1)
               endif
            endif
            ifirst=0
         else
            call color((ntraceno(it)-1)/i2+1)
            call dashset((ntraceno(it)-1)/i2)
            if(nmask(it).ne.0)then
               if(ljm(it))call polyline(x(1,it),y(1,it),np(it))
               if(lpm(it))then
                  call color((ntraceno(it+markoff)-1)/i2+1)
                  call polymark(x(1,it),y(1,it),np(it),
     $                 (ntraceno(it+markoff)-1)/i2+1)
               endif
            endif
         endif
         write(*,'(i4,8f8.4)')it,(y(k,it),k=1,8)
         il=lentrim(legends(icount))
         if(nmask(it).ne.0)then
            if(il.eq.0)then
               call iwrite(it,iw,argstr)
               call iwrite(np(it),iw2,charin)
               legends(icount)=argstr(1:iw)//' '//charin(1:iw2)
               il=lentrim(legends(icount))
            endif
            if(il.gt.1)then
               call winset(.false.)
            if(lpm(it))then
               ntr=ntraceno(it+markoff)/i2
               if(ljm(it))ntr=-ntr
               call legendline(xlego,(icount+markoff)*.05*cz+ylego
     $              ,ntr,legends(icount)(1:il))
            else
               call legendline(xlego,icount*.05*cz+ylego,0,
     $              legends(icount)(1:il))
            endif
               call winset(.true.)
            endif
         endif
      enddo
      write(*,'(a,i3,a)')'Plotted',jj-1,'  traces'
      call color(15)
      if(xtitle(1:1).ne.' '.or.ytitle(1:1).ne.' ')then
         call winset(.false.)
         call axlabels(xtitle(1:lentrim(xtitle))
     $        ,ytitle(1:lentrim(ytitle)))
      endif
      call axis2()

      call pltend

 3    continue

      call exit(0)
c-----------------------------------------------------------------
c Usage messages.
 105  write(*,*)'********* Error reading data at line',ii,' of',np(jj)
     $     ,nt,'Line=',argstr(1:lentrim(argstr))
      call exit()
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
      write(*,*)' -p toggle points, -j toggle line plotting.'
     $     ,' -t1001... mask traces.'
      write(*,*)' -v Verbose. Write out args, non-data lines read etc.'
      write(*,*)' -lo<xorig>,<yorig> box fraction origin of legends.'
     $     ,' -cz<size> Character scaling size.'
     $     ,' -nt<itics> set tic-number.'
      write(*,*)' -bh<height> Set height of plot box in normal units.'
      write(*,*)' -mo<int> offset count of marks vs lines/traces.'
      write(*,*
     $     )' -z[ntroff] Offset trace count to 1 [or ntroff]' 
      write(*,*)'In data files,'
     $     ,' the first line starting with an integer is data start.'
      write(*,*)'Prior lines starting legend: define successive legends'
      write(*,*)'Lines starting :- are interpreted as extra switches'
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