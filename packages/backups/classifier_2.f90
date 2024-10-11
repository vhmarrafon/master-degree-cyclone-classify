subroutine genese_classifier(file,nchar,nlin,sea)
    implicit none
    integer::i,j,nlin,ext_out,nchar,count_trop,count_phases
    integer,parameter::ncol=9
    character(nchar)::file
    character(3)::sea
    character(15)::cyclone_gen
    integer,dimension(3)::ext_in
    real,dimension(nlin,ncol)::cyc
    integer,dimension(nlin)::date, class_ax

    ! #################################
    ! CYCLONE GENESIS TAGS:
    !   :1 - EXTRATROPICAL
    !   :2 - TROPICAL
    !   :3 - SUBTROPICAL
    !   :4 - TO INDIVIDUAL ANALYSIS

    !!! B, VTu and VTl -> number of column in file - 1 !!!!!!!!
    ! #################################

    ! opening input and output files
    open(11,file=file,status='old')
    open(22,file='output_pure.txt',status='unknown')
    open(33,file='log.txt',status='unknown')
    write(33,*)file

    ! reading CPS of cyclone data
    do i=1,nlin 
        read(11,*)date(i),(cyc(i,j),j=1,ncol)
        class_ax(i)=4
    enddo

    ! count number of times in/out external threshold (Subtropical)
    ext_in=0
    ext_out=0
    count_trop=0


    ! classifing phase of each timestep
    cyclone_gen='ANALYSIS'

    !########## SUBTROPICAL CASES ------------------------------------------
    if(cyc(1,2)>=-40. .and. cyc(1,2)<=-20.)then

        do i=1,nlin

                !classifing subtropical phases
                if(sea=='DJF' .or. sea=='MAM')then

                    ! summer and autumn
                    call subtropicalDJF(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                elseif(sea=='JJA' .or. sea=='SON')then

                    ! winter and spring
                    call subtropicalJJA(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                endif
        enddo

        ! check if cyclone is a pure subtropical
        count_phases=0

        if(nlin>=6)then
            do i=1,6
                if(class_ax(i)==3)count_phases=count_phases+1
            enddo

            if(ext_out==0)then
            
                ! counting number of times into external threshold to subtropical cyclones
                if(sea=='DJF' .or. sea=='MAM')then

                    if(ext_in(1)<=2 .and. ext_in(2)<=3 .and. ext_in(3)<=2)then
                    
                        if(count_phases>=6)then
                            cyclone_gen='SUBTROPICAL'
                        endif

                    endif

                elseif(sea=='JJA' .or. sea=='SON')then

                    if(ext_in(1)<=2 .and. ext_in(2)<=2 .and. ext_in(3)<=2)then
                        if(count_phases>=6)then
                            cyclone_gen='SUBTROPICAL'

                        endif
                    endif
                endif
            endif
        endif
    endif
    if(cyclone_gen/='ANALYSIS')GO TO 200

    !########## EXTRATROPICAL CASES ------------------------------------------
    if(cyc(1,2)<=-20.)then
        do i=1,nlin   
            !classifing extratropical phases
            call extratropical(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i)
        enddo

        ! check if cyclone is a pure extratropical
        count_phases=0
        do i=1,5
            if(class_ax(i)==1)count_phases=count_phases+1
        enddo

        if(count_phases>=5)then
            cyclone_gen='EXTRATROPICAL'
        endif

    endif

    if(cyclone_gen/='ANALYSIS')GO TO 200

    !########## TROPICAL CASES ------------------------------------------
    if(cyc(1,2)<=-40.)then
        do i=1,nlin
            !classifing tropical phases
            if(class_ax(i)/=1)call tropical(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,count_trop)   
        enddo

        ! check if cyclone is a pure tropical
        count_phases=0

        do i=1,5
            if(class_ax(i)==2)count_phases=count_phases+1
        enddo

        if(count_phases>=4 .and. count_trop>=1)then
            cyclone_gen='TROPICAL'
        endif
        
    endif

    if(cyclone_gen/='ANALYSIS')GO TO 200

    

    200 CONTINUE
    do i=1,nlin
        write(22,"(i11,2x,7(f9.2,2x),i1)")date(i),(cyc(i,j),j=2,8),class_ax(i) 
    enddo

    ! call set_cyclone_genesis(class_ax,nlin,ext_in,ext_out,cyclone_gen,sea,count_trop,cyc,ncol)
    write(22,'(a15)')cyclone_gen
    write(33,*)cyclone_gen
    write(*,*)file,'  ',cyclone_gen

end subroutine

subroutine extratropical(lat,b,vtl,vtu,class_ax,nlin,time)
    implicit none
    integer::time,nlin 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    

    !! EXTRATROPICAL CRITERIA

    if(lat<=-20.)then
        ! if(time)
        if(time==1 .and. vtl<-0 .and. vtu<0)then
            class_ax(time)=1
        else
            if(abs(b)>25 .and. vtl<-0 .and. vtu<0)then
                class_ax(time)=1
            endif
        endif
    endif

end subroutine

subroutine tropical(lat,b,vtl,vtu,class_ax,nlin,time,count_trop)
    implicit none
    integer::time,nlin,count_trop 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax


    !! TROPICAL CRITERIA

    if(lat>=-40.)then
        if(time==1 .and. vtl>0 .and. vtu>-50)then
            class_ax(time)=2
    
        elseif(abs(b)<10 .and. vtl>0 .and. vtu>-50)then
            class_ax(time)=2
            if(vtu>0)then
                count_trop=count_trop+1
            endif
        endif
 
    endif

end subroutine

subroutine subtropicalDJF(lat,b,vtl,vtu,class_ax,nlin,time,ext_in,ext_out)
    implicit none
    integer::time,nlin,ext_out 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in


    !! SUBTROPICAL DJF/MAM CRITERIA

    if(lat>=-40. .and. lat<=-20.)then
        !! internal threshold

        if((abs(b)<=25 .or. time==1) .and. vtl>=-50 .and. vtu<=0.)then
            class_ax(time)=3 

        !! external threshold
        ! B
        elseif(abs(b)<=50. .and. vtl>=-50. .and. vtu<=0.)then
            if(time<=7)then
                ext_in(1)=ext_in(1)+1
            endif
            class_ax(time)=3 

        ! VTL
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-127.6 .and. vtu<=0.)then
            if(time<=7)then
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3 

        ! VTu
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-50. .and. vtu<=17.5)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
            endif
            class_ax(time)=3 

        ! B+ VTu
        elseif(abs(b)<=50. .and. vtl>=-50. .and. vtu<=17.5)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
                ext_in(1)=ext_in(1)+1
            endif
            class_ax(time)=3 

        ! B+ VTl
        elseif(abs(b)<=50. .and. vtl>=-127.6 .and. vtu<=0.)then
            if(time<=7)then
                ext_in(1)=ext_in(1)+1
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3

        ! VTu+ VTl
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-127.6 .and. vtu<=17.5)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3 

        !B+ VTu+ VTl
        elseif(abs(b)<=50. .and. vtl>=-127.6 .and. vtu<=17.5)then
            if(time<=7)then
                ext_in=ext_in+1            
            endif
            class_ax(time)=3  

        else
        !! out of external threshold
            if(time <= 7)then
                ext_out=ext_out+1
            endif
        endif
    endif

end subroutine


subroutine subtropicalJJA(lat,b,vtl,vtu,class_ax,nlin,time,ext_in,ext_out)
    implicit none
    integer::time,nlin,ext_out
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in


    !! SUBTROPICAL JJA/SON CRITERIA

    if(lat>=-40. .and. lat<=-20.)then
        !! internal threshold
        if((abs(b)<=25 .or. time==1) .and. vtl>=-50 .and. vtu<=-10.)then
            class_ax(time)=3 

        !! external threshold
        ! B
        elseif(abs(b)<=35. .and. vtl>=-50. .and. vtu<=-10.)then
            if(time<=7)then
                ext_in(1)=ext_in(1)+1
            endif
            class_ax(time)=3 

        ! VTL
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-75. .and. vtu<=-10.)then
            if(time<=7)then
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3 

        ! VTu
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-50. .and. vtu<=11.)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
            endif
            class_ax(time)=3 

        ! B+ VTu
        elseif(abs(b)<=35. .and. vtl>=-50. .and. vtu<=11.)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
                ext_in(1)=ext_in(1)+1
            endif
            class_ax(time)=3 

        ! B+ VTl
        elseif(abs(b)<=35. .and. vtl>=-75. .and. vtu<=-10.)then
            if(time<=7)then 
                ext_in(1)=ext_in(1)+1
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3

        ! VTu+ VTl
        elseif((abs(b)<=25. .or. time==1) .and. vtl>=-75. .and. vtu<=11.)then
            if(time<=7)then
                ext_in(3)=ext_in(3)+1
                ext_in(2)=ext_in(2)+1
            endif
            class_ax(time)=3 

        !B+ VTu+ VTl
        elseif(abs(b)<=35. .and. vtl>=-75. .and. vtu<=11.)then
            if(time<=7)then
                ext_in=ext_in+1            
            endif
            class_ax(time)=3         
        else

        !! out of external threshold
            if(time<=7)then
                ext_out=ext_out+1
            endif
        endif
    endif

end subroutine

subroutine set_cyclone_genesis(class_ax,nlin,ext_in,ext_out,cyclone_gen,sea,count_trop,cyc,ncol)
    implicit none
    integer::i,nlin,ext_out,n,count_trop,try_sub,ncol,j
    real::lat_i, lon_i
    character(3)::sea
    character(15)::cyclone_gen
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in
    integer,dimension(4)::count_times
    real,dimension(nlin,ncol)::cyc

    count_times=0
    try_sub=0
    lat_i=cyc(1,2)
    lon_i=cyc(1,3)

    if(nlin<5)then
        cyclone_gen='ANALYSIS'
        GO TO 100
    endif

    !! counting the number of times in each phase
    do i=1,nlin
        do n=1,4
            if(i==1 .and. n==3 .and. class_ax(i)==1)then
                count_times(3)=count_times(3)+1
            elseif(class_ax(i)==n)then
                count_times(n)=count_times(n)+1
            endif
        enddo

        !! checking phases after 24h
        if(i==5)then

            ! checking extratropical and tropical cyclogenesis
            if(count_times(1)>=5)then
                if(lat_i<-20. .and. lon_i <= 340)then
                    cyclone_gen='EXTRATROPICAL'
                    GO TO 100
                endif

            elseif(count_times(2)>=5)then
                if(count_trop>=1 .and. lat_i>-40. .and. lon_i <= 340)then
                    cyclone_gen='TROPICAL'
                    GO TO 100
                elseif(count_trop==0 .and. lat_i>-40 .and. lon_i <= 340)then
                    try_sub=1

                endif
                
            elseif(i==nlin)then
                cyclone_gen='ANALYSIS'
                GO TO 100
            endif

        endif

        !! checking phases after 36h
        if(i==7)then
            if(try_sub==1)then
                ! check if tropical phase are subtropical cyclone
                do j=1,i
                    if(sea=='DJF' .or. sea=='MAM')then

                        ! summer and autumn
                        call subtropicalDJF(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                    elseif(sea=='JJA' .or. sea=='SON')then

                        ! winter and spring
                        call subtropicalJJA(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                    endif
                enddo
                count_times(3)=0
                do n=1,7
                    if(class_ax(n)==3)then
                        count_times(3)=count_times(3)+1
                    endif
                enddo
            endif

            
            ! checking subtropical cyclogenesis
            if(ext_out>0)then
                cyclone_gen='ANALYSIS'
                GO TO 100

            ! counting number of times into external threshold to subtropical cyclones
            elseif(sea=='DJF' .or. sea=='MAM')then
                if(ext_in(1)>2 .or. ext_in(2)>3 .or. ext_in(3)>2)then
                    cyclone_gen='ANALYSIS'
                    GO TO 100
                else
                    if(count_times(3)>=3)then
                        if(lat_i>-40. .and. lat_i<-20. .and. lon_i <= 340)then
                            cyclone_gen='SUBTROPICAL'
                            GO TO 100
                        else
                            cyclone_gen='ANALYSIS'
                            GO TO 100
                        endif
                    else
                        cyclone_gen='ANALYSIS'
                        GO TO 100
                    endif
                endif

            elseif(sea=='JJA' .or. sea=='SON')then
                if(ext_in(1)>2 .or. ext_in(2)>2 .or. ext_in(3)>2)then
                    cyclone_gen='ANALYSIS'
                    GO TO 100
                else
                    if(count_times(3)>=3)then
                        if(lat_i>-40. .and. lat_i<-20. .and. lon_i <= 340)then
                            cyclone_gen='SUBTROPICAL'
                            GO TO 100
                        else
                            cyclone_gen='ANALYSIS'
                            GO TO 100
                        endif
                    else
                        cyclone_gen='ANALYSIS'
                        GO TO 100
                    endif
                endif
            endif

            if(i==nlin)then
                cyclone_gen='ANALYSIS'
                GO TO 100
            endif

        endif

        if(i==nlin)then
            cyclone_gen='ANALYSIS'
            GO TO 100
        endif
    enddo
    
    100 CONTINUE
end subroutine