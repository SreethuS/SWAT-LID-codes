subroutine MLGA_GreenAmpt

    !INPUT VARIABLES

   !precipdt             |mm             |Total rainfall within the time duration idt
   !C_inf                |mm             |cumulative infiltration in the sub-basin at the end of a day
   !idt                  |minutes        |length of time step used to report
!!                                       |precipitation data for sub-daily modeling
   !ihru                 |none           |HRU number
   !delta_t              |hr             |discretization time step
   !del_t                |hr             |computational time step
   !mlyr                 |no.            |number of soil layers
   !nstep                |no.            | number of time step
   !newrti(:)            |mm/hr          |infiltration rate for last time step from the previous day
   !!swtrg(:)           |none          |rainfall event flag:
   !!                                   |  0: no rainfall event over midnight
   !!                                   |  1: rainfall event over midnight


   !! OUTPUT VARIABLES
   !cuminf               |mm             |cumulative infiltration for each time step
   !rateinf              |mm/h           |rate of infiltration


   !C_inf                |mm             |cumulative infiltration in the sub-basin at the end of a day
   !hhqday(:)            |mm             |surface runoff generated each hour of day in HRU
   !newrti(:)            |mm/hr          |infiltration rate for last time step from the previous day
   !surfq(:)             |mm             |surface runoff for the day in HRU
   !swtrg(:)             |none           |rainfall event flag:
!!                                       |  0: no rainfall event over midnight
!!                                       |  1: rainfall event over midnight



   !! INTERMEDIATE

   !rintns               |mm/h           | intensity of rainfall
   !theta_d              |mm/mm          | moisture deficit
   !SuctionHead          |mm             |suction head
   !soilw                |mm             |amount of water in the soil profile
   !exinc                |mm             | runoff for the time step


    use parm

        !! variables from SWAT
        integer::xxmylr,sb

        real*8, dimension (nstep+1) :: cuminf,rateinf
        real*8, dimension (nstep+1) :: rintns,exinc


         !! new variables for MLGA

        real*8, dimension (sol_nly(ihru)) :: theta_d,SuctionHead,soil_thick,sol_ksat


         !!intermediate variables
        real*8::soilw,urban_prec
        REAL*8::d,F_ini,delta_t,ks1,del_t,t
        REAL*8::f,T_out,F_cum,F_out,inf_out
        REAL*8::ia,Fp,tp,delta_F,Fs,F1,C,FF2,Kbar,cFp
        REAL*8:: f2(50),e(50)  !!for Newton-Raphson iteration
        REAL*8:: Er,Nr,Dr    !! for Newton-Raphson iteration
        REAL*8::A1,B1,A2,B2,delta_t1,L_A,L_AA,ABF  !! for layers beneath the first layer
        INTEGER ::j,ii,iter_num,jj,ts,kt,dez_t,kk
        real*8, dimension (:,:), allocatable ::T2_1 !! for storing the time when wetting front crosses a particular layer
        allocate (T2_1(nstep,sol_nly(ihru)))


        xxmylr = sol_nly(ihru)
        

        !!Initialization
        ABF=0.
        sb=hru_sub(ihru)
        d=0.
		T2_1 = 0.
        f2=0.
        e=0.
        theta_d = 0.
        iter_num=0.
        SuctionHead = 0.
        F_ini = 0.
        A2=0.
        B2=0.
        FF2=0.
        delta_t = 0.1 !! Discretization time step assumed as 5 minutes
        Er=0.1 !!iteration error
        iter_num=(size(f2)-5)

        !!Rainfall input
		rintns(1) =  60.* precipdt(2)/ dfloat(idt)


		do kt = 2, nstep+1
			rintns(kt) =  60. * precipdt(kt+1) / dfloat(idt)
		end do

        !soil moisture deficit at the start of the rainfall
            if (swtrg(ihru) == 1) then
               swtrg(ihru) = 0
               do jj=1,xxmylr
				theta_d(jj) = 0.001 * sol_por(jj,ihru) * 0.95
               end do
               F_ini=C_inf(ihru)
			   C_inf(ihru) = 0.
             else
               soilw = 0.
               if (sol_sw(ihru) >= sol_sumfc(ihru)) then
                 soilw = 0.999 * sol_sumfc(ihru)
               else
                 soilw = sol_sw(ihru)
               end if
               do jj=1,xxmylr
				 theta_d(jj) = (1. - soilw / sol_sumfc(ihru)) * sol_por(jj,ihru) * 0.95
               end do
               F_ini = 0.
			   C_inf(ihru) = 0.
             end if

        !! Determination of suction head
        do jj=1,xxmylr

        sol_sand(jj,ihru) = 100. - sol_clay(jj,ihru) - sol_silt(jj,ihru)

        SuctionHead(jj) = 10. * Exp(6.5309 - 7.32561 * sol_por(jj,ihru) +           &
          3.809479 * sol_por(jj,ihru) ** 2 + 0.001583 * sol_clay(jj,ihru) ** 2 +    &
          0.000344 * sol_sand(jj,ihru) * sol_clay(jj,ihru) - 0.049837 *             &
          sol_por(jj,ihru) * sol_sand(jj,ihru)                                      &
          + 0.001608 * sol_por(jj,ihru) ** 2 * sol_sand(jj,ihru) ** 2 +             &
          0.001602 * sol_por(jj,ihru) ** 2 * sol_clay(jj,ihru) ** 2 -               &
          0.0000136 * sol_sand(jj,ihru) ** 2 * sol_clay(jj,ihru) -                  &
          0.003479 * sol_clay(jj,ihru) ** 2 * sol_por(jj,ihru) -                    &
          0.000799 * sol_sand(jj,ihru) ** 2 * sol_por(jj,ihru))

        end do
        sol_ksat=0.

        do jj=1,xxmylr
            if (jj==1) then
                soil_thick(jj)=sol_z(jj,ihru)
            else
                soil_thick(jj)=sol_z(jj,ihru)-sol_z(jj-1,ihru)
            endif
            sol_ksat(jj)=(56.82 * sol_k(jj,ihru) ** 0.286) & 
                / (1. + 0.051 * Exp(0.062 * cnday(ihru))) - 2.
            if (sol_ksat(jj)<=0.)   sol_ksat(jj)=0.001
        end do
           Ks1=sol_ksat(1)

		  !Ks1 = sol_k(1,ihru)

		  del_t=float(idt)/60.

		  dez_t=del_t/delta_t

          j=1 !for the time steps wetting front crosses layers

	do ts=1,nstep

        !!Time to ponding in the first layer
        !F_cum = cumulative infiltration(F)

          ia = rintns(ts) + (d/del_t) !available rainfall rate || del_t should come as an input variable

        if (ts == 1) then
          F_cum=F_ini  !cumulative infiltration value from the previous time step
		ELSE
		  F_cum=cuminf(ts-1)  !cumulative infiltration value from the previous time step
		end if

        !!Fp = (SuctionHead(1)*Ks1*theta_d(1))/(ia-Ks1)
        !!tp = Fp/ia  !time to pond
        !!cFp = Ks1*(((SuctionHead(1)*theta_d(1))/F_cum) +1)


    do kk=1,dez_t  !!Additional time discretaization within MLGA

	do jj=2,xxmylr


        IF (F_cum < sol_z(1,ihru)*theta_d(1) .AND.(jj==2)) then
       !print*,"first layer"

            IF (ia==0) then
                f=0 !infiltration rate
                delta_F = 0.
                F_cum=F_cum+delta_F
            ELSEIF ((ia <= Ks1).OR.(delta_t<=tp)) then
                f=ia
                delta_F = ia*delta_t
                F_cum=F_cum+delta_F
            else
                Fs = (Ks1*SuctionHead(1)*theta_d(1))/(ia - Ks1)

                IF (F_cum>=Fs) then !saturated condition
                    !print*,"F_cum>Fs"
                    F1=F_cum
                    C = Ks1*delta_t + F1-(SuctionHead(1)*theta_d(1)*log(F1+(SuctionHead(1)*theta_d(1))))

                    f2(1)=ia*delta_t !initial guess
                   
                   

                    !!Newton -Raphson method
                    do ii=1,iter_num,1

                        f2(ii+1) =(f2(ii) - dble((C +(SuctionHead(1)*theta_d(1)*log(f2(ii)+(SuctionHead(1)*theta_d(1))))&
                         -f2(ii))/((SuctionHead(1)*theta_d(1)/(f2(ii)+SuctionHead(1)*theta_d(1)))-1)))

                        e(ii)= abs((f2(ii+1)-f2(ii))/f2(ii))

                        if (e(ii)<Er) EXIT

                    end do

                    FF2=f2(ii+1)
                    delta_F=FF2-F_cum
                    F_cum=FF2

                    f = delta_F/delta_t

                    IF (delta_F>(ia*delta_t)) then !change the condition to unsaturated

                        delta_F = ia*delta_t
                        F_cum=F_cum+delta_F

                        f= delta_F/delta_t
                    END IF

                ELSEIF (F_cum<(Fs-(ia*delta_t))) then !surface remains unsaturated
                   ! print*,"F_cum<Fs"

                           f = ia
                            delta_F = ia*delta_t
                            F_cum = F_cum+delta_F;


                ELSE  !surface gets saturate during the time step

                    !print*,"else condition"
                    F1=Fs
                    delta_t1 = delta_t - ((Fs-F_cum)/ia)
                    C = Ks1*delta_t1 + F1-(SuctionHead(1)*theta_d(1)*log(F1+(SuctionHead(1)*theta_d(1))))
                     !write(*,*),C

                    f2(1)=ia*delta_t !initial guess

                 
                    !!Newton Raphson method
                    do ii=1,iter_num
                      f2(ii+1) =(f2(ii) - dble((C+(SuctionHead(1)*theta_d(1)*log(f2(ii)+(SuctionHead(1)*theta_d(1))))&
                       -f2(ii))/((SuctionHead(1)*theta_d(1)/(f2(ii)+SuctionHead(1)*theta_d(1)))-1)))

                        e(ii) = abs((f2(ii+1)-f2(ii))/f2(ii))

                        if (e(ii)<Er) EXIT
                    end do
                     !write(*,*),C
                    FF2=f2(ii+1)

                    delta_F=FF2-F_cum
                    F_cum=FF2

                    f = delta_F/delta_t

                END IF


            END IF


        ELSEIF (F_cum>=sum(soil_thick(1:jj-1)*theta_d(1:jj-1))) then  !wetting front in the next layer
             !! advancing to jj th layer


            t=ts+ (kk*delta_t)
            T2_1(j,jj)=ts+(kk*delta_t) !time when wetting front crosses the layer
            j=j+1

            if (j>=nstep) then
                j=nstep
            end if

            Kbar=sum(soil_thick(1:jj))/sum(soil_thick(1:jj)/sol_ksat(1:jj))


                 if (ia ==0) then
                    f=0
                    F_cum = F_cum+f*delta_t
                 elseif (ia<=Kbar) then
                    f=ia
                    F_cum = F_cum+f*delta_t
                 else
                    L_A=sum(soil_thick(1:jj-1))
                    L_AA= sum(soil_thick(1:jj-1)*sol_ksat(jj)/sol_ksat(1:jj-1))
                    A2=real((L_A - L_AA + SuctionHead(jj))*theta_d(jj))
                    B2=real((L_AA*theta_d(jj)) - sum(soil_thick(1:jj-1)*theta_d(1:jj-1)))
                    FF2 = sum(soil_thick(1:jj-1)*theta_d(1:jj-1))
                    f2=0
                    e=0
                    f2(1)=Kbar*delta_t
                    ABF=A2+B2+FF2
            

                    !Newton Raphson
                    do ii=1,iter_num
                        
                        if (((A2+B2+f2(ii))/(ABF)) <=1) then
                            Nr = real(-f2(ii) + FF2 + (sol_ksat(jj)*(t-T2_1(1,jj))))
                        else
                       Nr = real(-f2(ii) + FF2 + (sol_ksat(jj)*(t-T2_1(1,jj))) + (A2*log((A2+B2+f2(ii))/(ABF))))
                        endif
                     
                          

                          Dr= -1+ real(A2/(A2+B2+f2(ii)))
                           f2(ii+1)=f2(ii)-(Nr/Dr)
                           e(ii) = abs((f2(ii+1)-f2(ii))/f2(ii))
                         if (e(ii)<Er) EXIT

                    end do
                    delta_F = F_cum-f2(ii+1)
                    f = sol_ksat(jj)*(1+ (A2/(B2+f2(ii+1))))
                    F_cum = F_cum+f*delta_t

                    if (delta_F > (ia*delta_t)) then
                        delta_F = Kbar*delta_t
                        F_cum = F_cum+delta_F
                        f= delta_F/delta_t
                    else
                        f = sol_ksat(jj)*(1+ (A2/(B2+f2(ii+1))))
                        F_cum = F_cum+f*delta_t
                        if (f<0.0) then
                          f=delta_F/delta_t
                          F_cum = F_cum+f*delta_t
                        end if
                    end if

                 end if




        END IF

    end do   !! layer loop ends here


    end do    !! discretization loop ends here



        rateinf(ts)=f

        if (ts==1)then
        cuminf(ts)=F_ini+rateinf(ts)*del_t
        else
         cuminf(ts)=cuminf(ts-1)+rateinf(ts)*del_t
        end if


!!        if (f > cFp) then
!!          print("There is a problem")
!!        end if


        exinc(ts)=(rintns(ts)-rateinf(ts))*del_t  !!runoff for the time step

        if (exinc(ts)<0.) then
            exinc(ts)=0.
        end if
        hhqday(ts)=exinc(ts)

        	   !! Urban Impervious cover
	   if (iurban(ihru)>0) then
	     !runoff from pervious area
	     hhqday(ts) = hhqday(ts) * (1.- fcimp(urblu(ihru)))
           ! add runoff from a LID and its upstream drainage areas (green roof, rain garden, cistern, and porous pavement)
           urban_prec = max(0.,precipdt(ts+1) - abstinit)
          
           do kk=1,nlid(sb)

		    if (((lid_onoff(sb,kk)==1).or.(MLGALID_onoff(sb,kk)==1)).and.&
       		          lid_lunam(sb,kk)==urbname(urblu(ihru))) then

                 call lids(sb,ihru,ts+1,kk,urban_prec)
			   exit
			endif
		 end do
     
           lid_qsurf_total = 0.
           lid_farea_sum = 0.
           lid_runfr_sum=0.
           do ii = 1, 4
             if (lid_farea(ihru,ii) > 0) then
               if (ii==1) then
                 if (cs_grcon(sb,kk)==0) then
                   lid_farea_sum = lid_farea_sum + lid_farea(ihru,ii)
				 lid_qsurf_total = lid_qsurf_total + lid_qsurf(ihru,ii) !jaehak 2021. lid_qsurf is mm normalized to the entire HRU.
                 end if
               else
                 lid_farea_sum = lid_farea_sum + lid_farea(ihru,ii)
                 lid_qsurf_total = lid_qsurf_total + lid_qsurf(ihru,ii)
               end if
		   end if
           end do

            do ii = 1, 5
             if (mlid_farea(ihru,ii) > 0) then
                 lid_farea_sum = lid_farea_sum + mlid_farea(ihru,ii)
                 lid_qsurf_total = lid_qsurf_total + Lsurq(ihru,ii,ts)
       


                 lid_runfr_sum = lid_runfr_sum + LIDr_fr(ihru,ii)
		   end if
           end do



		 	!adjustments for the fraction of runoff routed to MLGA LIDs
           ubnrunoff(ts) = urban_prec * fcimp(urblu(ihru)) &
            * (1 - lid_farea_sum)*(1-lid_runfr_sum) + lid_qsurf_total !lid_qsurf is mm normalized to the entire HRU.
         else
           ubnrunoff(ts) = 0.
         end if

         if (ubnrunoff(ts)<0)  ubnrunoff(ts) = 0.



        surfq(ihru)=surfq(ihru)+hhqday(ts) + ubnrunoff(ts) ! +ubnrunoff(ts)  (from SWAT)
        LID_surq_day(ihru)=LID_surq_day(ihru) + lid_qsurf_total
       
       

          !adjust initial abstraction for the next time steop. Jaehak 2021
	   if (precipdt(ts+1)>0) then
		   abstinit = max(abstinit - precipdt(ts+1),0.)
	   else
		   abstinit = min(iabstr,abstinit + pet_day / nstep)
	   endif



    end do  !! time step looping through a day ends here
    

        C_inf(ihru)=cuminf(nstep)
         newrti(ihru) = 0.
         if (sum(precipdt) > 12.) then
             swtrg(ihru) = 1
             newrti(ihru) = rateinf(nstep)
         end if

         deallocate (T2_1)
    return

    end


