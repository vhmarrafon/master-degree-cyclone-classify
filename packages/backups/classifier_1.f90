subroutine genese_classifier(file,nchar,nlin,sea)
    implicit none
    integer::i,j,nlin,ext_out,nchar
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
    enddo

    ! count number of times in/out external threshold (Subtropical)
    ext_in=0
    ext_out=0

    ! classifing phase of each timestep
    do i=1,nlin
        
        !classifing extratropical phases
        
        call extratropical(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i)
        
        if(class_ax(i)<0)then

            !classifing tropical phases
            call tropical(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i)

            if(class_ax(i)<0)then

                !classifing subtropical phases
                if(sea=='DJF' .or. sea=='MAM')then

                    ! summer and autumn
                    call subtropicalDJF(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                elseif(sea=='JJA' .or. sea=='SON')then

                    ! winter and spring
                    call subtropicalJJA(cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

                endif
                if(class_ax(i)<0)then

                    !to analysis phases
                    class_ax(i)=4

                endif

            endif

        endif
        write(33,*)cyc(i,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax(i)
    enddo
    

    do i=1,nlin
        write(22,"(i11,2x,7(f8.2,2x),i1)")date(i),(cyc(i,j),j=2,8),class_ax(i) 
    enddo

    call set_cyclone_genesis(class_ax,nlin,ext_in,ext_out,cyclone_gen, sea)
    write(22,'(a15)')cyclone_gen
    write(33,*)cyclone_gen

end subroutine

subroutine extratropical(lat,b,vtl,vtu,class_ax,nlin,time)
    implicit none
    integer::time,nlin 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    

    !! EXTRATROPICAL CRITERIA

    if(lat<-20)then
        if(b==-99999.)then
            class_ax(time)=-1
        else
            if(abs(b)>25 .and. vtl<-0 .and. vtu<0)then
                class_ax(time)=1
            else
                class_ax(time)=-1 
            endif
        endif
    else
        class_ax(time)=-1 
    endif

end subroutine

subroutine tropical(lat,b,vtl,vtu,class_ax,nlin,time)
    implicit none
    integer::time,nlin 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax


    !! TROPICAL CRITERIA

    if(lat>-20)then
        if(abs(b)<10 .and. vtl>0 .and. vtu>-50)then
            class_ax(time)=2
        else
            class_ax(time)=-1 
        endif
    else
        class_ax(time)=-1 
    endif

end subroutine

subroutine subtropicalDJF(lat,b,vtl,vtu,class_ax,nlin,time,ext_in,ext_out)
    implicit none
    integer::time,nlin,ext_out 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in


    !! SUBTROPICAL DJF/MAM CRITERIA

    if(lat>-40 .and. lat<-15)then
        !! internal threshold
        if(abs(b)<=25 .and. vtl>=-50 .and. vtu<=0)then
            class_ax(time)=3 

        !! external threshold
        ! B
        elseif(abs(b)<=50. .and. vtl>=-50. .and. vtu<=0.)then
            ext_in(1)=ext_in(1)+1
            class_ax(time)=3 
        ! VTL
        elseif(abs(b)<=25. .and. vtl>=-75. .and. vtu<=0.)then
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3 
        ! VTu
        elseif(abs(b)<=25. .and. vtl>=-50. .and. vtu<11.)then
            ext_in(3)=ext_in(3)+1
            class_ax(time)=3 
        ! B+ VTu
        elseif(abs(b)<=50. .and. vtl>=-50. .and. vtu<11.)then
            ext_in(3)=ext_in(3)+1
            ext_in(1)=ext_in(1)+1
            class_ax(time)=3 
        ! B+ VTl
        elseif(abs(b)<=50. .and. vtl>=-75. .and. vtu<0.)then
            ext_in(1)=ext_in(1)+1
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3
        ! VTu+ VTl
        elseif(abs(b)<=25. .and. vtl>=-75. .and. vtu<11.)then
            ext_in(3)=ext_in(3)+1
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3 
        !B+ VTu+ VTl
        elseif(abs(b)<=50. .and. vtl>=-75. .and. vtu<11.)then
            ext_in=ext_in+1            
            class_ax(time)=3         
        else
        !! out of external threshold
            if(b==-99999.)then
                class_ax(time)=-1
            else
                class_ax(time)=-1
                if(time <= 8)then
                    ext_out=ext_out+1
                endif
            endif
        endif
    else
        class_ax(time)=-1 
    endif

end subroutine


subroutine subtropicalJJA(lat,b,vtl,vtu,class_ax,nlin,time,ext_in,ext_out)
    implicit none
    integer::time,nlin,ext_out
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in


    !! SUBTROPICAL JJA/SON CRITERIA

    if(lat>-40 .and. lat<-15)then
        !! internal threshold
        if(abs(b)<=25 .and. vtl>=-50 .and. vtu<=-10)then
            class_ax(time)=3 

        !! external threshold
        ! B
        elseif(abs(b)<=35. .and. vtl>=-50. .and. vtu<=-10.)then
            ext_in(1)=ext_in(1)+1
            class_ax(time)=3 
        ! VTL
        elseif(abs(b)<=25. .and. vtl>=-75. .and. vtu<=-10.)then
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3 
        ! VTu
        elseif(abs(b)<=25. .and. vtl>=-50. .and. vtu<17.5)then
            ext_in(3)=ext_in(3)+1
            class_ax(time)=3 
        ! B+ VTu
        elseif(abs(b)<=35. .and. vtl>=-50. .and. vtu<17.5)then
            ext_in(3)=ext_in(3)+1
            ext_in(1)=ext_in(1)+1
            class_ax(time)=3 
        ! B+ VTl
        elseif(abs(b)<=35. .and. vtl>=-75. .and. vtu<=-10.)then
            ext_in(1)=ext_in(1)+1
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3
        ! VTu+ VTl
        elseif(abs(b)<=25. .and. vtl>=-75. .and. vtu<17.5)then
            ext_in(3)=ext_in(3)+1
            ext_in(2)=ext_in(2)+1
            class_ax(time)=3 
        !B+ VTu+ VTl
        elseif(abs(b)<=35. .and. vtl>=-75. .and. vtu<17.5)then
            ext_in=ext_in+1            
            class_ax(time)=3         
        else
        !! out of external threshold
            if(b==-99999.)then
                class_ax(time)=-1
   
            else
                class_ax(time)=-1
                ext_out=ext_out+1

            endif
        endif
    else
        class_ax(time)=-1 
    endif

end subroutine

subroutine set_cyclone_genesis(class_ax,nlin,ext_in,ext_out,cyclone_gen,sea)
    implicit none
    integer::i,nlin,ext_out,n
    character(3)::sea
    character(15)::cyclone_gen
    integer,dimension(nlin)::class_ax
    integer,dimension(3)::ext_in
    integer,dimension(4)::count_times

    count_times=0

    if(nlin<6)then
        cyclone_gen='ANALYSIS'
        GO TO 100
    endif

    !! counting the number of times in each phase
    do i=2,nlin
        do n=1,4
            if(class_ax(i)==n)then
                count_times(n)=count_times(n)+1
            endif
        enddo

        !! checking phases after 24h
        if(i==6)then

            ! checking extratropical and tropical cyclogenesis
            if(count_times(1)>=4)then
                cyclone_gen='EXTRATROPICAL'
                GO TO 100
            elseif(count_times(2)>=4)then
                cyclone_gen='TROPICAL'
                GO TO 100
            endif

        endif

        !! checking phases after 36h
        if(i==8)then

            ! checking subtropical cyclogenesis
            if(ext_out>1)then
                write(33,*)'EXT_OUT',ext_out
                cyclone_gen='ANALYSIS'
                GO TO 100

            ! counting number of times into external threshold to subtropical cyclones
            elseif(sea=='DJF' .or. sea=='MAM')then
                if(ext_in(1)>2 .or. ext_in(2)>3 .or. ext_in(3)>2)then
                    cyclone_gen='ANALYSIS'
                    GO TO 100
                else
                    if(count_times(3)>=3)then
                        cyclone_gen='SUBTROPICAL'
                        GO TO 100
                    else
                        cyclone_gen='ANALYSIS'
                        GO TO 100
                    endif
                endif
            elseif(sea=='JJA' .or. sea=='SON')then
                if(ext_in(1)>2 .or. ext_in(2)>2 .or. ext_in(3)>2)then
                    cyclone_gen='ANALYSIS'
                    write(33,*)ext_in
                    GO TO 100
                else
                    if(count_times(3)>=3)then
                        cyclone_gen='SUBTROPICAL'
                        GO TO 100
                    else
                        cyclone_gen='ANALYSIS'
                        GO TO 100
                    endif
                endif
            endif

        endif

        if(i==nlin)then
            cyclone_gen='ANALYSIS'
            GO TO 100
        endif
    enddo
    
    100 CONTINUE
end subroutine