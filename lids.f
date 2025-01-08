      subroutine lids(sb,j,k,kk,lid_prec)
!!    kk               |none          |LID index in *.lid files

      use parm
      implicit none
      
      integer :: sb, j, k,kk,li
      real*8 :: lid_prec,tot_lid_area,LID_inp
      
      lid_farea = 0.
      lid_farea_sum = 0.
      lid_qsurf_total = 0.
      lid_sw_add = 0.
      tot_lid_area=0. !total fraction of lids
      mlid_farea=0.
      LID_inp=0.
      
      if (gr_onoff(sb,kk)==1) then
        lid_farea(j,1) = gr_farea(sb,kk) 
        call lid_greenroof(sb,j,k,kk,lid_prec)
      end if
      
      if (rg_onoff(sb,kk)==1) then
        lid_farea(j,2) = rg_farea(sb,kk)
        call lid_raingarden(sb,j,k,kk,lid_prec)
      end if
      
      if (cs_onoff(sb,kk)==1) then
	  lid_farea(j,3) = cs_farea(sb,kk)
        if (cs_grcon(sb,kk)==0) then
          call lid_cistern(sb,j,k,kk,lid_prec)
        else
          call lid_cistern(sb,j,k,kk,lid_qsurf(j,1))
          lid_qsurf(j,1) = 0.
        end if
      end if
      
      if (pv_onoff(sb,kk)==1) then
        lid_farea(j,4) = pv_farea(sb,kk) 
        call lid_porpavement(sb,j,k,kk,lid_prec)
      end if
      
      !!MLGA LIDs
      
      !Biocell
      if (MLID_onoff(sb,1)==1) then
      LIDthick(j,1)=sum(Lthik(sb,1,1:2))+LID_FB(sb,1)
      mlid_farea(j,1)=LID_areaFr(sb,1)
      LIDr_fr(j,1)=LR_fr(sb,1)
      end if
      !rain garden
      if (MLID_onoff(sb,2)==1) then
      LIDthick(j,2)=Lthik(sb,2,1)+LID_FB(sb,2)
      mlid_farea(j,2)=LID_areaFr(sb,2)
      LIDr_fr(j,2)=LR_fr(sb,2)
      end if
       !roof garden
      if (MLID_onoff(sb,3)==1) then
      LIDthick(j,3)=sum(Lthik(sb,3,1:2))+LID_FB(sb,3)
      mlid_farea(j,3)=LID_areaFr(sb,3)
      LIDr_fr(j,3)=LR_fr(sb,3)
      end if
       !infiltration trench
      if (MLID_onoff(sb,4)==1) then
      LIDthick(j,4)=Lthik(sb,4,1)+LID_FB(sb,4)
      mlid_farea(j,4)=LID_areaFr(sb,4)
      LIDr_fr(j,4)=LR_fr(sb,4)
      end if
      !Permable pavement
      if (MLID_onoff(sb,5)==1) then
      LIDthick(j,5)=sum(Lthik(sb,5,1:2))+LID_FB(sb,5)+Lthik(sb,5,4)
      mlid_farea(j,5)=LID_areaFr(sb,5)
      LIDr_fr(j,5)=LR_fr(sb,5)
      end if
      
      do li=1,4
          tot_lid_area=tot_lid_area+lid_farea(j,li)
      end do
      do li=1,5
          tot_lid_area=tot_lid_area+mlid_farea(j,li)
      end do
      
      !Biocell
      if (MLID_onoff(sb,1)==1) then
          LID_inp=precipdt(k)+ (lid_prec*((1-tot_lid_area)/mlid_farea(j,1))* LIDr_fr(j,1))
          
      call MLGA_Biocell(sb,j,k-1,kk,LID_inp)
      endif
      
      !rain garden
        if (MLID_onoff(sb,2)==1) then
         LID_inp=precipdt(k)+ (lid_prec*((1-tot_lid_area)/mlid_farea(j,2))* LIDr_fr(j,2))
      call MLGA_raingarden(sb,j,k-1,kk,LID_inp)
      endif
      
      !roofgarden
        if (MLID_onoff(sb,3)==1) then
          LID_inp=precipdt(k)
      call MLGA_Roofgarden(sb,j,k-1,kk,LID_inp)
      endif
      
      !infiltration trench
        if (MLID_onoff(sb,4)==1) then
         LID_inp=precipdt(k)+ (lid_prec*((1-tot_lid_area)/mlid_farea(j,4))* LIDr_fr(j,4))
      call MLGA_infTrench(sb,j,k-1,kk,LID_inp)
      endif
      
      !permeable pavement
        if (MLID_onoff(sb,5)==1) then
          LID_inp=precipdt(k)+ (lid_prec*((1-tot_lid_area)/mlid_farea(j,5))* LIDr_fr(j,5))
      call MLGA_PermeablePav(sb,j,k-1,kk,LID_inp)
      end if
      
      

      
      return
      end subroutine