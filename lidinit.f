      subroutine lidinit
    
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine sets default values for LID parameters

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i                |none          |subbasin number
!!    msub             |none          |the number of subbasins
!!    mudb             |none          |the number of urban land uses
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gr_farea         |none          |fractional area of a green roof to the HRU
!!    gr_sol_k         |mm/hr         |saturated hydraulic conductivity of 
!!                                    |the amended soil layer
!!    gr_sol_por       |none          |total porosity of the amended soil layer
!!                                    |expressed as a fraction of the total volume
!!    gr_sol_fc        |mm/mm H2O     |amount of water held in the amended soil
!!                                    |profile at the field capacity
!!    gr_sol_wp        |mm/mm H2O     |amount of water held in the amended soil
!!                                    |profile at the wilting point
!!    gr_sol_sw(:)     |mm/mm H2O     |amount of water stored in the amended soil profile
!!    gr_sol_sw_last   |mm/mm H2O     |soil water content of the amended soil layer at the last time step in a day
!!    vgcl             |none          |van Genuchten equation's coefficient, l
!!    vgcm             |none          |van Genuchten equation's coefficient, m
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    kk               |none          |end of file flag


      use parm
      implicit none

      integer :: kk,m,mn
!      real*8 :: 
      real*8:: lid_thick
    
!!    Common Variables
!!    van Genuchten equation's coefficients
      lid_vgcl = 0.50
      lid_vgcm = 0.50
      lid_thick=0.
      lid_qsurf = 0.
      lid_qsurf_total = 0.
      lid_farea_sum = 0.
      lid_cuminf_last = 0.
      lid_sw_last = 0.00
      interval_last = 0.
      lid_f_last = 0.
      lid_cumr_last = 0.
      lid_str_last = 0.
      lid_cumqperc_last = 0.
      lid_cumirr_last = 0.
      lid_sw_add = 0.
	lid_str_curday = 0.
	lid_excum_last = 0.
      
      !MLGA-LID common variables
      LFcum = 0.
      Lsur = 0.
      Ldrain = 0.
      WFront = 1.
      Lexfil = 0.
      Lsol = 0.
      Lstor = 0.
      Lsurq = 0.
      Lpav=0.
      Lcum_surq=0.
      LFcum_last = 0.
      Lsur_last = 0.
      Ldrain_last = 0. 
      Lexfil_last = 0.
      Lsurq_last = 0.
      lid_runfr_sum=0.
      Lcum_exfil=0.
      Lpav_last=0.
     
     
!!    Green Roof
      do kk = 1, nlid(i)
        if (gr_onoff(i,kk)==1) then
          if (gr_imo(i,kk)<=0)         gr_imo(i,kk) = 1
          if (gr_iyr(i,kk)<=1000)      gr_iyr(i,kk) = iyr
          if (gr_farea(i,kk)<=0)       gr_farea(i,kk) = 0.1

		! take the chracteristics of the native HRU soil
          if (gr_solop(i,kk)==0) then
			do ihru=1,mhru
			  if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
			    if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
				  gr_fc(i,kk) = sol_fc(1,ihru)/10. !mm/mm
			      gr_wp(i,kk) = sol_wp(1,ihru)/10. !mm/mm
			      gr_ksat(i,kk) = sol_k(1,ihru) !mm/hr
			      gr_por(i,kk) = sol_por(1,ihru) !mm/mm
				endif
			  endif
			end do  
          end if
          if (gr_fc(i,kk)<=0)         gr_fc(i,kk) = 0.40
          if (gr_wp(i,kk)<=0)         gr_wp(i,kk) = 0.15
          if (gr_ksat(i,kk)<=0)       gr_ksat(i,kk) = 7.5
          if (gr_por(i,kk)<=0)        gr_por(i,kk) = 0.50
          if (gr_hydeff(i,kk)<=0)     gr_hydeff(i,kk) = 0.70
          if (gr_soldpt(i,kk)<=0)     gr_soldpt(i,kk) = 0.25
		if (gr_etcoef(i,kk)<=0)     gr_etcoef(i,kk) = 0.6
		
        end if
      end do
      
!!    Rain Garden
      do kk = 1, nlid(i)
        if (rg_onoff(i,kk)==1) then
          if (rg_imo(i,kk)<=0)          rg_imo(i,kk) = 1
          if (rg_iyr(i,kk)<=1000)       rg_iyr(i,kk) = iyr
          if (rg_farea(i,kk) <=0)       rg_farea(i,kk) = 0.1

		! take the chracteristics of the native HRU soil
          if (rg_solop(i,kk)==0) then
			do ihru=1,mhru
			  if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
			    if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
				  rg_fc(i,kk) = sol_fc(1,ihru)/10. !mm/mm
			      rg_wp(i,kk) = sol_wp(1,ihru)/10. !mm/mm
			      rg_ksat(i,kk) = sol_k(1,ihru) !mm/hr
			      rg_por(i,kk) = sol_por(1,ihru) !mm/mm
				  exit
				endif
			  endif
			end do  
		endif
          if (rg_fc(i,kk)<=0)         rg_fc(i,kk) = 0.40
          if (rg_wp(i,kk)<=0)         rg_wp(i,kk) = 0.15
          if (rg_ksat(i,kk)<=0)       rg_ksat(i,kk) = 7.5
          if (rg_por(i,kk)<=0)        rg_por(i,kk) = 0.50
          if (rg_hydeff(i,kk)<=0)     rg_hydeff(i,kk) = 0.10
          if (rg_soldpt(i,kk) <=0)    rg_soldpt(i,kk) = 0.25
		if (rg_etcoef(i,kk)<=0)       rg_etcoef(i,kk) = 0.6
          if (rg_vol(i,kk)<=0)       rg_vol(i,kk) = rg_farea(i,kk)
     & * 0.5 ! assuming the depth of 0.5 m
          if (rg_sarea(i,kk)<=0)      rg_sarea(i,kk) = 0.1
          if (rg_sth(i,kk)<=0)        rg_sth(i,kk) = 0.1
          if (rg_sdia(i,kk)<=0)       rg_sdia(i,kk) = 0.1
          if (rg_bdia(i,kk)<=0)       rg_bdia(i,kk) = 0.1
          if (rg_sts(i,kk)<=0)        rg_sts(i,kk) = 0.1
          if (rg_orifice(i,kk)==1) then
            if (rg_oheight(i,kk)<=0)    rg_oheight(i,kk) = 0.05
            if (rg_odia(i,kk)<=0)       rg_odia(i,kk) = 0.05
          end if
        end If
      end do
      
!!    CiStern
      do kk = 1, nlid(i)
        if (cs_onoff(i,kk)==1) then
          if (cs_imo(i,kk)<=0)          cs_imo(i,kk) = 1
          if (cs_iyr(i,kk)<=1000)       cs_iyr(i,kk) = iyr
          if (cs_farea(i,kk) <=0)       cs_farea(i,kk) = 0.1
          !if (cs_vol(i,kk) <=0)   cs_vol(i,kk) = 5
		do ihru=1,mhru
			if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
			if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
				if (cs_vol(i,kk) <= 0) then
					cs_vol(i,kk) = (cs_rdepth(i,kk) / 1000.) *
     &  (cs_farea(i,kk) * fcimp(urblu(ihru)) * hru_ha(ihru) * 10000.)         
				end if
			endif
			endif
		end do	
        end if
      end do

!!    Poropus paVement
      do kk = 1, nlid(i)
        pv_soldpt(i,kk) = 0.25
        if (pv_onoff(i,kk)==1) then
          if (pv_imo(i,kk)<=0)          pv_imo(i,kk) = 1
          if (pv_iyr(i,kk)<=1000)       pv_iyr(i,kk) = iyr
          if (pv_grvdep(i,kk)<=0)       pv_grvdep(i,kk) = 130
          if (pv_grvpor(i,kk)<=0)       pv_grvpor(i,kk) = 0.35
          if (pv_farea(i,kk) <=0)       pv_farea(i,kk) = 0.1
          if (pv_drcoef(i,kk) <=0)      pv_drcoef(i,kk) = 0.6
          if (pv_solop(i,kk)==0) then

		! take the chracteristics of the native HRU soil
          if (pv_solop(i,kk)==0) then
			do ihru=1,mhru
			  if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
			    if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
				  pv_fc(i,kk) = sol_fc(1,ihru)/10. !mm/mm
			      pv_wp(i,kk) = sol_wp(1,ihru)/10. !mm/mm
			      pv_ksat(i,kk) = sol_k(1,ihru) !mm/hr
			      pv_por(i,kk) = sol_por(1,ihru) !mm/mm
				  exit
				endif
			  endif
			end do  
		endif
            if (pv_fc(i,kk)<=0)         pv_fc(i,kk) = 0.40
            if (pv_wp(i,kk)<=0)         pv_wp(i,kk) = 0.15
            if (pv_ksat(i,kk)<=0)       pv_ksat(i,kk) = 7.5
            if (pv_por(i,kk)<=0)        pv_por(i,kk) = 0.50
            if (pv_hydeff(i,kk)<=0)     pv_hydeff(i,kk) = 0.10
          end if
        end if
      end do
      
      !! MLGA based LIDs
      do kk=1,nlid(i)
         do mn=1,5
           if (LmSat(i,mn,1)<0.)     LmSat(i,mn,1)=0.40 
           if (LmSat(i,mn,2)<0.)     LmSat(i,mn,2)=0.40 
           if (LmSat(i,mn,4)<0.)     LmSat(i,mn,4)=0.40 !for permeable pavement
           if (Lksat(i,mn,1)<0.)     Lksat(i,mn,1)=7.5
           if (Lksat(i,mn,2)<0.)     Lksat(i,mn,2)=7.5
           if (Lksat(i,mn,4)<0.)     Lksat(i,mn,4)=100 !for permeable pavement
           if (LmIni(i,mn,1)<0.)     LmIni(i,mn,1)=0.15
           if (LmIni(i,mn,2)<0.)     LmIni(i,mn,2)=0.15
           if (LsucH(i,mn,1)<0.)     LsucH(i,mn,1)=wfsh(ihru)
           if (LsucH(i,mn,2)<0.)     LsucH(i,mn,2)=wfsh(ihru)
           if (Lthta(i,mn,1)<0.)     Lthta(i,mn,1)=0.1
           if (Lthta(i,mn,2)<0.)     Lthta(i,mn,2)=0.1 
           if (LID_VoidFr(i,mn)<0.)      LID_VoidFr(i,mn)=0.5
            if (LD_drain(i,mn)>0.) then
               if (LN_drain(i,mn)<0.)      LN_drain(i,mn)=5.
               if (LD_height(i,mn)<0.)      LD_height(i,mn)=0.
            end if  
          
        end do
      end do
      
      
      
      !! Bio-retention cell
        do kk = 1, nlid(i)
         if (MLID_onoff(i,1)==1) then
          if (LID_areaFr(i,1) <=0)       LID_areafr(i,1) = 0.1
          if (LR_fr(i,1) <=0)       LR_fr(i,1) = 0.1
          if (LID_FB(i,1)<0.)      LID_FB(i,1)=0.
          ! take the chracteristics of the native HRU soil
           do ihru=1,mhru
               if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
                   if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
                       lid_thick=sum(Lthik(i,1,1:2))+LID_FB(i,1)
                         if (LD_height(i,1)>lid_thick)   LD_height(i,1)=0.25*Lthik(i,1,2)
                         if (sol_nly(ihru)==2) then
                          LmSat(i,1,3)=sol_por(2,ihru)
                          Lksat(i,1,3)=sol_k(2,ihru)
                          LmIni(i,1,3)=sol_fc(2,ihru)
                          LsucH(i,1,3)=wfsh(ihru)
                          
                             if (lid_thick>sol_z(2,ihru))  then
                                 Lthik(i,1,3)=10.  
                             else
                                 Lthik(i,1,3)=sol_z(2,ihru)-lid_thick
                             end if
                         else
                             do m=1,sol_nly(ihru)-1
                               if (lid_thick <=sol_z(m,ihru)) then
                                  LmSat(i,1,3)=sol_por(m,ihru)
                                 Lksat(i,1,3)=sol_k(m,ihru)
                                 LmIni(i,1,3)=sol_fc(m,ihru)
                                 Lthik(i,1,3)=sol_z(m,ihru)-lid_thick
                                 LsucH(i,1,3)=wfsh(ihru)
                                 exit
                                                              
                                elseif ((lid_thick> sol_z(m,ihru)).and.(lid_thick<=sol_z(m+1,ihru))) then
                                    LmSat(i,1,3)=sol_por(m+1,ihru)
                                    Lksat(i,1,3)=sol_k(m+1,ihru)
                                    LmIni(i,1,3)=sol_fc(m+1,ihru)
                                    Lthik(i,1,3)=sol_z(m+1,ihru)-lid_thick
                                    LsucH(i,1,3)=wfsh(ihru)
                                else
                                    LmSat(i,1,3)=0.5
                                    Lksat(i,1,3)=7.5
                                    LmIni(i,1,3)=0.1
                                    Lthik(i,1,3)=100
                                    LsucH(i,1,3)=wfsh(ihru) 
                  
                                endif
                             end do
                         end if  ! looping ends for soil layer
                   endif !for lunam
               endif  !for urblu
           end do    ! loop for hru
         end if  !for lid onoff
        end do  ! loop for nlid(sb)
        
        
         !! rain garden
        do kk = 1, nlid(i)
         if (MLID_onoff(i,2)==1) then
          if (LID_areaFr(i,2) <=0)       LID_areafr(i,2) = 0.1
          if (LR_fr(i,2) <=0)       LR_fr(i,2) = 0.1
          if (LID_FB(i,2)<0.)      LID_FB(i,2)=0.
          ! take the chracteristics of the native HRU soil
           do ihru=1,mhru
               if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
                   if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
                       lid_thick=0.
                       lid_thick= Lthik(i,2,1)+LID_FB(i,2)
                         if (LD_height(i,2)>lid_thick)   LD_height(i,2)=0.25*Lthik(i,2,1)
                         if (sol_nly(ihru)==2) then
                          LmSat(i,2,2)=sol_por(2,ihru)
                          Lksat(i,2,2)=sol_k(2,ihru)
                          LmIni(i,2,2)=sol_fc(2,ihru)
                          LsucH(i,2,2)=wfsh(ihru)
                          
                             if (lid_thick>sol_z(2,ihru))  then
                                 Lthik(i,2,2)=10.  
                             else
                                 Lthik(i,2,2)=sol_z(2,ihru)-lid_thick
                             end if
                         else
                             do m=1,sol_nly(ihru)-1
                               if (lid_thick <=sol_z(m,ihru)) then
                                  LmSat(i,2,2)=sol_por(m,ihru)
                                 Lksat(i,2,2)=sol_k(m,ihru)
                                 LmIni(i,2,2)=sol_fc(m,ihru)
                                 Lthik(i,2,2)=sol_z(m,ihru)-lid_thick
                                 LsucH(i,2,2)=wfsh(ihru)
                                 exit
                                                              
                                elseif ((lid_thick> sol_z(m,ihru)).and.(lid_thick<=sol_z(m+1,ihru))) then
                                    LmSat(i,2,2)=sol_por(m+1,ihru)
                                    Lksat(i,2,2)=sol_k(m+1,ihru)
                                    LmIni(i,2,2)=sol_fc(m+1,ihru)
                                    Lthik(i,2,2)=sol_z(m+1,ihru)-lid_thick
                                    LsucH(i,2,2)=wfsh(ihru)
                                else
                                    LmSat(i,2,2)=0.5
                                    Lksat(i,2,2)=7.5
                                    LmIni(i,2,2)=0.1
                                    Lthik(i,2,2)=100
                                    LsucH(i,2,2)=wfsh(ihru) 
                  
                                endif
                             end do
                         end if  ! looping ends for soil layer
                   endif !for lunam
               endif  !for urblu
           end do    ! loop for hru
         end if  !for lid onoff
        end do  ! loop for nlid(sb)
        
        
        
         !! roof garden
        do kk = 1, nlid(i)
         if (MLID_onoff(i,3)==1) then
          if (LID_areaFr(i,3) <=0)       LID_areafr(i,3) = 0.1
          if (LID_FB(i,3)<0.)      LID_FB(i,3)=0.
          if (LD_height(i,3)>Lthik(i,3,1))   LD_height(i,3)=0.25*Lthik(i,3,1)
         end if  !for lid onoff
        end do  ! loop for nlid(sb)
        
        
         !! Infiltrtaion trench
        do kk = 1, nlid(i)
        if (MLID_onoff(i,4)==1) then
          if (LID_areaFr(i,4) <=0)       LID_areafr(i,4) = 0.1
          if (LR_fr(i,4) <=0)       LR_fr(i,4) = 0.1
          if (LID_FB(i,4)<0.)      LID_FB(i,4)=0.
          ! take the chracteristics of the native HRU soil
           do ihru=1,mhru
               if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
                   if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
                       lid_thick=0.
                       lid_thick= Lthik(i,4,1)+LID_FB(i,4)
                        if (LD_height(i,4)>lid_thick)   LD_height(i,4)=0.25*Lthik(i,4,1)
                         if (sol_nly(ihru)==2) then
                          LmSat(i,4,2)=sol_por(2,ihru)
                          Lksat(i,4,2)=sol_k(2,ihru)
                          LmIni(i,4,2)=sol_fc(2,ihru)
                          LsucH(i,4,2)=wfsh(ihru)
                          
                             if (lid_thick>sol_z(2,ihru))  then
                                 Lthik(i,4,2)=10.  
                             else
                                 Lthik(i,4,2)=sol_z(2,ihru)-lid_thick
                             end if
                         else
                             do m=1,sol_nly(ihru)-1
                               if (lid_thick <=sol_z(m,ihru)) then
                                  LmSat(i,4,2)=sol_por(m,ihru)
                                 Lksat(i,4,2)=sol_k(m,ihru)
                                 LmIni(i,4,2)=sol_fc(m,ihru)
                                 Lthik(i,4,2)=sol_z(m,ihru)-lid_thick
                                 LsucH(i,4,2)=wfsh(ihru)
                                 exit
                                                              
                                elseif ((lid_thick> sol_z(m,ihru)).and.(lid_thick<=sol_z(m+1,ihru))) then
                                    LmSat(i,4,2)=sol_por(m+1,ihru)
                                    Lksat(i,4,2)=sol_k(m+1,ihru)
                                    LmIni(i,4,2)=sol_fc(m+1,ihru)
                                    Lthik(i,4,2)=sol_z(m+1,ihru)-lid_thick
                                    LsucH(i,4,2)=wfsh(ihru)
                                else
                                    LmSat(i,4,2)=0.5
                                    Lksat(i,4,2)=7.5
                                    LmIni(i,4,2)=0.1
                                    Lthik(i,4,2)=100
                                    LsucH(i,4,2)=wfsh(ihru) 
                  
                                endif
                             end do
                         end if  ! looping ends for soil layer
                   endif !for lunam
               endif  !for urblu
           end do    ! loop for hru
        end if  !for lid onoff
        end do  ! loop for nlid(sb)
       
     
      !! Permeable pavement
        do kk = 1, nlid(i)
         if (MLID_onoff(i,5)==1) then
          if (LID_areaFr(i,5) <=0)       LID_areafr(i,5) = 0.1
          if (LR_fr(i,5) <=0)       LR_fr(i,5) = 0.1
          if (LID_FB(i,5)<0.)      LID_FB(i,5)=0.
          ! take the chracteristics of the native HRU soil
           do ihru=1,mhru
               if (hru_sub(ihru)==i.and.urblu(ihru)>0) then
                   if (lid_lunam(i,kk)==urbname(urblu(ihru))) then
                       lid_thick=0.
                       lid_thick=sum(Lthik(i,5,1:2))+LID_FB(i,5)+Lthik(i,5,4)
                        if (LD_height(i,5)>lid_thick)   LD_height(i,5)=0.25*Lthik(i,5,2)
                         if (sol_nly(ihru)==2) then
                          LmSat(i,5,3)=sol_por(2,ihru)
                          Lksat(i,5,3)=sol_k(2,ihru)
                          LmIni(i,5,3)=sol_fc(2,ihru)
                          LsucH(i,5,3)=wfsh(ihru)
                          
                             if (lid_thick>sol_z(2,ihru))  then
                                 Lthik(i,5,3)=10.  
                             else
                                 Lthik(i,5,3)=sol_z(2,ihru)-lid_thick
                             end if
                         else
                             do m=1,sol_nly(ihru)-1
                               if (lid_thick <=sol_z(m,ihru)) then
                                  LmSat(i,5,3)=sol_por(m,ihru)
                                 Lksat(i,5,3)=sol_k(m,ihru)
                                 LmIni(i,5,3)=sol_fc(m,ihru)
                                 Lthik(i,5,3)=sol_z(m,ihru)-lid_thick
                                 LsucH(i,5,3)=wfsh(ihru)
                                 exit
                                                              
                                elseif ((lid_thick> sol_z(m,ihru)).and.(lid_thick<=sol_z(m+1,ihru))) then
                                    LmSat(i,5,3)=sol_por(m+1,ihru)
                                    Lksat(i,5,3)=sol_k(m+1,ihru)
                                    LmIni(i,5,3)=sol_fc(m+1,ihru)
                                    Lthik(i,5,3)=sol_z(m+1,ihru)-lid_thick
                                    LsucH(i,5,3)=wfsh(ihru)
                                else
                                    LmSat(i,5,3)=0.5
                                    Lksat(i,5,3)=7.5
                                    LmIni(i,5,3)=0.1
                                    Lthik(i,5,3)=100
                                    LsucH(i,5,3)=wfsh(ihru) 
                  
                                endif
                             end do
                         end if  ! looping ends for soil layer
                   endif !for lunam
               endif  !for urblu
           end do    ! loop for hru
         end if  !for lid onoff
        end do  ! loop for nlid(sb)
            
      do kk = 1, nlid(i)
        if ((MLID_onoff(i,1)==1).or.(MLID_onoff(i,2)==1).or.(MLID_onoff(i,3)==1).or.
     & (MLID_onoff(i,4)==1).or. (MLID_onoff(i,5)==1))  then
          MLGALID_onoff(i,kk)=1
       end if
      end do 
      
      
      
           
            

        
!!    Combine indices for the lids
      do kk = 1, nlid(i)
        if (gr_onoff(i,kk)==1 .or. rg_onoff(i,kk)==1 .or.
     & cs_onoff(i,kk)==1 .or. pv_onoff(i,kk)==1) then
          lid_onoff(i,kk)=1
       end if
      end do
            
      end subroutine