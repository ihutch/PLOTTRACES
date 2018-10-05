c      program timehistory, based on plottraces code
c Read in traces from files prescribed on the command line.
c The files read are a time sequence of traces, and a history is to be
c stored in an array and subsequently plotted or manipulated.
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

c Each trace (of nt) is regarded as a different quantity.
c Each file is regarded as referring to a different time.
c Therefore the time history is a 3D array (np,nf,nt).
c Every x of every file must be the same, else things are inconsistent.
c Every nt of every file must be the same, else inconsistent.
c Every time we read a new file (and nf incremented), jj is reset to 1.
c Every trace of every file must have the same length np else we complain. 
c We truncate to the length of the first trace read. 

      include 'cmdlncom.f'

      character*256 argstr,charin,filename
      integer npmax
      parameter (npmax=2000)
      logical lpm(ntmax),ljm(ntmax)
      character*256 legends(ntmax),annotations(ntmax)
      real y(npmax,nfmax,ntmax)
      real x(npmax,ntmax)
      real coordinate(ntmax)
      integer np(ntmax),ntraceno(ntmax),ifirst
      real yleginc
      data ifirst/1/
      data legends/ntmax*' '/
      data coordinate/ntmax*9.9e-20/
      data yleginc/0./

      nf=0
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
            nf=nf+1
            jj=1  ! Reset for timehistory.
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
c---------------------------------------------
      if(jj.eq.1)goto 106
      write(*,*)np(1),nf,nt

      if(nf.lt.9)then
         do i=1,min(35,np(1))
            write(*,'(10f8.4)')x(i,1),(y(i,j,1),j=1,nf)
         enddo
      endif
! Plot a rendering of y.
      call autocontour(y(1,1,1),npmax,np(1),nf)
c      call autocolcont(y(1,1,1),npmax,np(1),nf)
      call pltend()
c      call pfset(3)
c      if(iticnum.ne.0)call ticnumset(iticnum)
c      if(cz.ne.1.)call charsize(cz*.015,cz*.015)
c Do over the files:
c Deal with annotations:
c      do i=1,nannot
c         read(annotations(i),*)xa,ya,argstr
c         call drwstr(xa,ya,argstr(1:lentrim(argstr)))
c      enddo
! Write out the result. dt=zero means don't use.
      open(14,file='timearray.dat',status='unknown',err=106)
      write(14,*)np(1),nf,nt,dt
      write(14,*)(x(i,1),i=1,np(1))
      do kk=1,nt
         write(14,*)((y(i,j,kk),i=1,np(1)),j=1,nf)
      enddo
      close(14)
      call exit(0)
 106  call cmdlineinterp('-?')
      end
c******************************************************************
c Now in accis
c$$$c Obtain the length of a string omitting trailing blanks.
c$$$      function lentrim(string)
c$$$      character*(*) string
c$$$      do i=len(string),1,-1
c$$$         if(string(i:i).ne.' ') goto 101
c$$$      enddo
c$$$      i=0
c$$$ 101  lentrim=i
c$$$      end
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
      if(argstr(1:3) .eq. '-dt') read(argstr(4:),*,end=106,err=106)dt
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
      write(*,*)' -dt<dt> set timestep length'
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
      data nannot/0/dt/0./
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
      real y(npmax,nfmax,ntmax)
      real x(npmax,ntmax)
      real coordinate(ntmax)
      integer np(ntmax),ntraceno(ntmax)
      integer il,nt
      real xxd,yyd

      integer j,k,ii
      integer npjj,ntff
      save npjj,ntff
      
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
         if(nf.eq.1)npjj=np(jj)
         if(np(jj).ne.npjj)then
            write(*,*)'Inconsistent trace length',npjj,nf,np(jj)
         endif
         read(charin,*,err=103,end=103)np(jj),nt
         goto 104
 103     nt=1
 104     continue
         if(nf.eq.1)ntff=nt
         if(ntff.ne.nt)write(*,*)'Inconsistent trace count',ntff,nf,nt
c Read and process the data for these trace(s). 
         do ii=1,np(jj)
            read(13,*,end=105,err=105)x(ii,jj),(y(ii,nf,j),j=jj,jj+nt-1)
c Scaling x,y
            if(xsf.ne.1. .or. xof.ne.0.)then
               x(ii,jj)=xsf*(x(ii,jj)+xof)
            endif
            if(ysf.ne.1. .or. yof.ne.0.)then
               do j=jj,jj+nt-1
                  y(ii,nf,j)=ysf*(y(ii,nf,j)+yof)
               enddo
            endif
            if(ysxp.ne.0.)then
c Scaling y by a power of x.
               do j=jj,jj+nt-1
                  y(ii,nf,j)=y(ii,nf,j)/x(ii,j)**ysxp
               enddo
            endif
            if(my)then
c Negate y
               do j=jj,jj+nt-1
                  y(ii,nf,j)=-y(ii,nf,j)
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
               yyd=y(iyof,nf,j)
               do ii=1,np(jj)
                  y(ii,nf,j)=y(ii,nf,j)-yyd
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
               yyd=y(iysc,nf,j)
               do ii=1,np(jj)
                  y(ii,nf,j)=y(ii,nf,j)/yyd
               enddo
            enddo
         endif
         if(ism.ne.0)then
c Smooth traces
            do j=jj,jj+nt-1
c Average into an as yet unused trace
               call triangave(np(jj),ism,y(1,nf,j),y(1,nf,jj+nt))
c Copy back
               do ii=1,np(jj)
                  y(ii,nf,j)=y(ii,nf,jj+nt)
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
