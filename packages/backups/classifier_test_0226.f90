subroutine genese_classifier(file,nchar, nlin,sea)
    implicit none
    integer::i,j,nlin,ext_out,nchar,count_trop,count_phases,luiz_criteria, extrp_flex, ntimes_sub, o_ex, nex
    integer,parameter::ncol=9
    character(nchar)::file
    character(3)::sea
    character(15)::cyclone_gen
    integer,dimension(3)::ext_in
    real,dimension(nlin,ncol)::cyc
    real::i_vtl,i_vtu
    integer,dimension(nlin)::date,class_ax

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

    luiz_criteria=0
    extrp_flex=0
    o_ex = 0

    ! reading CPS of cyclone data
    do i=1,nlin 
        read(11,*)date(i),(cyc(i,j),j=1,ncol)
        class_ax(i)=4
        
        !Luiz's shell
        i_vtl=cyc(i,7)
        i_vtu=cyc(i,8)
        
        if(luiz_criteria < 6)then
            if(i_vtl>-49. .and. i_vtu<-11.)then
                luiz_criteria=luiz_criteria+1
            else
                luiz_criteria=0
            endif
        endif

    enddo

    if(nlin<5)then
        GO TO 100
        cyclone_gen='EXTRATROPICAL'
    endif

    ! count number of times in/out external threshold (Subtropical)
    ext_in=0
    ext_out=0
    count_trop=0

    ! classifing phase of each timestep
    cyclone_gen='ANALYSIS'

   
     ! !########## EXTRATROPICAL CASES ------------------------------------------
    if(cyc(1,2)<=-20.)then
        do i=1,nlin   
            !classifing extratropical phases
           call extratropical(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i, extrp_flex, o_ex)
        enddo

      ! check if cyclone is a pure extratropical
        if(nlin > 5)then
            nex=6
        else
            nex=5 
        endif

        count_phases=0
        do i=1,nex
            if(class_ax(i)==1)count_phases=count_phases+1
        enddo

        if(nex==6)then
            if(count_phases>=5 .and. extrp_flex<=3 .and. o_ex<=2 .and. cyclone_gen=='ANALYSIS')then
                cyclone_gen='EXTRATROPICAL'
            endif

        else
            if(count_phases>=4 .and. extrp_flex<=2 .and. o_ex<=1 .and. cyclone_gen=='ANALYSIS')then
                cyclone_gen='EXTRATROPICAL'
            endif
        endif

    endif

     !########## SUBTROPICAL CASES ------------------------------------------
    ntimes_sub = 7
    if(luiz_criteria==6)then

        do i=1,nlin

            !classifing subtropical phases
            if(sea=='DJF' .or. sea=='MAM')then

                ! summer and autumn
                call subtropicalDJF(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

            elseif(sea=='JJA' .or. sea=='SON')then

                ! winter and spring
                call subtropicalJJA(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,ext_in,ext_out)

            endif

            if(i==ntimes_sub)then
                if(ext_out==0 .and. cyclone_gen=='ANALYSIS')then

                    ! counting number of times into external threshold to subtropical cyclones
                    if(sea=='DJF' .or. sea=='MAM')then

                        ! checking number of times into internal threshold
                        if(ext_in(1)>=4 .and. ext_in(2)>=3 .and. ext_in(3)>=4)then
                            cyclone_gen='SUBTROPICAL'
                        endif

                    elseif(sea=='JJA' .or. sea=='SON')then
                        
                        ! checking number of times into internal threshold
                        if(ext_in(1)>=4 .and. ext_in(2)>=4 .and. ext_in(3)>=4)then
                            cyclone_gen='SUBTROPICAL'
                        endif
                    endif
                endif
            endif
        enddo
    endif

     !########## TROPICAL CASES ------------------------------------------
     
    do i=1,nlin

        !classifing tropical phases
        if((class_ax(i)/=1 .and. class_ax(i)/=3))then
            call tropical(cyc(1,2),cyc(i,6),cyc(i,7),cyc(i,8),class_ax,nlin,i,count_trop)   
        endif
    enddo

    if(cyc(1,2)>=-40. .and. cyclone_gen=='ANALYSIS')then

        ! check if cyclone is a pure tropical
        count_phases=0

        do i=1,5
            if(class_ax(i)==2)count_phases=count_phases+1
        enddo

        if(count_phases==5 .and. count_trop>=1)then
             cyclone_gen='TROPICAL'
        endif
        
    endif

    100 continue
    do i=1,nlin
        write(22,"(i10,2x,7(f11.2,2x),i2)")date(i),cyc(i,2),cyc(i,3),(cyc(i,j),j=4,8),class_ax(i) 
    enddo

    ! EXTRATROPICAL BY ELIMINATION
    if(cyclone_gen=='ANALYSIS' .and. cyc(1,2)<= -20.)then
        cyclone_gen='EXTRATROPICAL'
    endif

    write(22,'(a15)')cyclone_gen
    write(33,*)cyclone_gen

end subroutine

subroutine extratropical(lat,b,vtl,vtu,class_ax,nlin,time, extrp_flex, o_ex)
    implicit none
    integer::time,nlin, extrp_flex, o_ex 
    real::lat,b,vtl,vtu
    integer,dimension(nlin)::class_ax
    

    !! EXTRATROPICAL CRITERIA

    if(lat<=-20.)then
        ! if(time)

        if((abs(b)>=25. .or. time==1) .and. vtl<-0. .and. vtu<0.)then
            class_ax(time)=1
                
        elseif(abs(b)>=18. .and. vtl<0 .and. vtu<0 .and. time<=6)then
            class_ax(time)=1
            extrp_flex=extrp_flex+1

        elseif(time<=6)then
            o_ex=o_ex+1
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
        if((abs(b)<10 .or. time==1) .and. vtl>0 .and. vtu>-50)then
            class_ax(time)=2
            if(vtu>0 .and. time <=5)then
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

    if(lat>=-40.)then

        !! out of external check
    
        if((((abs(b) > 50. .and. time > 1) .or. vtl <-127.6 .or. vtu > 17.5 )).and. time <= 7)then
            ext_out = ext_out + 1
        else
            
            !! internal threshold
            if((abs(b)<=25. .or. time==1) .and. time <= 7)then
                ext_in(1) = ext_in(1)+1

            endif

            if(vtl >= -51. .and. time <= 7)then
                ext_in(2) = ext_in(2)+1

            endif

            if(vtu <= -11. .and. time <= 7)then
                ext_in(3) = ext_in(3)+1     
            endif

            if((abs(b)<=25. .or. time==1) .and. vtu <= -11. .and. vtl >= -51.)then
                ! Gozzo's subtropical cyclone
                class_ax(time) = 3
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
    if(lat>=-40. )then

        !! out of external check
        if(((abs(b) > 35. .and. time > 1) .or. vtl <-75. .or. vtu > 11. ).and. time <= 7)then

            ext_out = ext_out + 1
        else
            
            !! internal threshold
            if((abs(b)<=25. .or. time==1) .and. time<=7)then
                ext_in(1) = ext_in(1)+1
                
            endif

            if(vtl >= -51. .and. time<=7)then
                ext_in(2) = ext_in(2)+1
    
            endif

            if(vtu <= -11. .and. time<=7)then
                ext_in(3) = ext_in(3)+1
            endif

            if((abs(b)<=25. .or. time==1) .and. vtu <= -11. .and. vtl >= -51.)then
                ! Gozzo's subtropical cyclone
                class_ax(time) = 3
            endif
   
        endif
            
    endif

end subroutine
