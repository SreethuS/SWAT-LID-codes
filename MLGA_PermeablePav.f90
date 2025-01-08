

subroutine MLGA_PermeablePav (sb,j,k,kk,lid_prec)
    use parm
    IMPLICIT NONE



    !!INPUT PARAMETERS!!


   !lid_prec             |mm             |precipitation depth received by LID in the time step
   !idt                  |minutes        |simulation time interval for sub-daily modeling
   !k                    |none           |sub-daily time index
   !nstep                |none           |number of time steps in a day
   !LsucH                |mm             |suction head of the soil
   !LmSat                |mm3/mm3        |moisture content at saturation
   !Lksat                |mm/h           |saturated hydraulic conductivity
   !Lthik                |mm             |thickness of LID layer
   !Lthta                |mm3/mm3        |permanent wilting point
   !LmIni                |mm3/mm3        |initial  moisture content
   !ET                   |mm/h           |evapotranspiration rate
   !LFcum_last(:,:,:)    |mm             |cumulative infiltration in j
   !Lsur_last(:,:,:)     |mm             |water depth on top of surface layer
   !Lpav_last(:,:,:)     |mm             |water depth in pavement layer
   !layN                 |none           |number of LID layers
   !WFront(:,:,:)        |none           |index for storing time when wetting front crosses layer

   !LID_FB(:,:)          |mm             |freeboard thickness
   !LID_VoidFr(:,:)      |mm3/mm3        |surface void fraction
   !LD_drain(:,:)        |m              | diameter of drain openings
   !LN_drain(:,:)        |none           | number of drain openings
   !LD_height(:,:)       |mm             |height of drain opening from bottom
   !LID_area(:,:)        |m2             |area of LID in m2

   !!OUTPUTS!!

   !!for the next time step
   !LFcum                |mm             |cumulative infiltration
   !Lsur                 |mm             |ponded water depth
   !Ldrain               |mm/h           |under-drain flow
   !Lexfil               |mm/h           |exfiltration from the bottom of LID layer
   !Lsol                 |mm3/mm3        |moisture content in soil layer
   !Lstor                |mm3/mm3        |moisture content in storage layer
   !Lsurq                |mm/h           |runoff from LID
   !LmIni                |mm3/mm3        |initial moisture content (for the next rainfall event)
   !Lpav                 |mm             |depth of water in pavement layer



   !! for the j at the end of a day

   !LFcum_last           |mm             |cumulative infiltration
   !Lsur_last            |mm             |ponded water depth
   !Ldrain_last          |mm/h           |under-drain flow
   !Lexfil_last          |mm/h           |exfiltration from the bottom of LID layer
   !Lsurq_last           |mm/h           |runoff from LID
   !Wfront               |none           |for storing the index when wetting front crosses layers
   !Lpav_last            |mm             |depth of water in pavement layer



   !! INTERMEDIATE!!

   !delta_t              |hr             |computational time step
   !del_t                |hr             |time step of input flow
   !LRAIN                |mm/h           |precipitation input to LID

   !D1                   |mm             |freeboard thickness
   !phi1                 |mm3/mm3        |surface void fraction
   !D_drain              |m              | diameter of drain openings
   !N_drain              |               | number of drain openings
   !D3D                  |mm             |height of drain opening from bottom
   !h_open               |mm             |threshold for drain opening
   !h_close              |mm             |threshold for drain closing
   !A_LID                |m2             |area of LID in m2


   !!INPUTS!!
    INTEGER::sb,k,kk,j,LidN,layN
    REAL*8::lid_prec!input
    REAL*8:: D1,phi1,pervFrac,D3D,h_open,h_close,A_LID,D_drain,N_drain,ET


    !!OUTPUTS!!
    REAL*8::infi,C_infi,surf_outflow,drain_flow,exfil,evap,&
    soil_layer,stor_layer,surf_layer


    !!INTERMEDIATE!!
    REAL*8::LRAIN,inf_out,F_out,del_t,delta_t,d,F_ini! output from MLGA
    INTEGER::itr,dez_t,iteration,jL,kL !intermediate parameters used in iteration
    REAL*8::theta_WP2,theta_WP3,tol,D2,D3,D4,theta2,theta3,theta4,theta_FC2,theta_FC3,&
    phi2,phi3,q3_old,d_4,HCO1,HCO2,omega,x_new(4),x(4),G_new(4),G(4),err_mat(4) !intermediate
    REAL*8::C3D,availEvap,e1,e2,e3,e4,F_next,f2,f3,f4,rate,q3,head,q1,G1,G2,G3,G4 !intermediate
    REAL*8::layer1(5),layer2(5),layer3(5),layer4(2)


     q1=0.
     q3=0.
     rate=0.
     head=0.
     G1=0.
     G2=0.
     G3=0.
     e1=0.
     e2=0.
     e3=0.
     f2=0.
     f3=0.
     x_new=0.
     x=0.
     G_new=0.
     G=0.
     q3_old=0.

     sb = hru_sub(j)
     LidN=5
     LayN=3


    del_t=dfloat(idt)/60.

    !!time parameters
    delta_t=0.1     !computational time step
    dez_t=del_t/delta_t


    infi=0.
    C_infi=0.


    tol=1           ! tolerance


    !! Evapotranspiration input
    call etpot
    ET=pet_day/24.

    !! LID AREA
    A_LID=mlid_farea(j,LidN)*fcimp(urblu(j))*hru_km(j)*1000000.

    !! Rainfull input to LID

    LRAIN = lid_prec/del_t



    !! layer properties

    layer1(1)=LsucH(sb,LidN,1) !suction head in mm
    layer1(2)=LmSat(sb,LidN,1) ! moisture content at saturation
    layer1(3)=LmIni(sb,LidN,1)!initial moisture content
    layer1(4)=Lksat(sb,LidN,1)!saturated hydraulic conductivity in mm/hr
    layer1(5)=Lthik(sb,LidN,1) !layer thickness in mm
    !layer2 = storage layer
    layer2(1)=LsucH(sb,LidN,2) !suction head in mm
    layer2(2)=LmSat(sb,LidN,2) ! moisture content at saturation
    layer2(3)=LmIni(sb,LidN,2)!initial moisture content
    layer2(4)=Lksat(sb,LidN,2) !saturated hydraulic conductivity in mm/hr
    layer2(5)=Lthik(sb,LidN,2) !layer thickness in mm
    !layer3 = native soil
    layer3(1)=LsucH(sb,LidN,3) !suction head in mm
    layer3(2)=LmSat(sb,LidN,3)! moisture content at saturation
    layer3(3)=LmIni(sb,LidN,3)!initial moisture content
    layer3(4)=Lksat(sb,LidN,3)!saturated hydraulic conductivity in mm/hr
    layer3(5)=Lthik(sb,LidN,3)!layer thickness in mm
    !layer4=pavementlayer
    layer4(1)=Lksat(sb,LidN,4) !hydraulic conductivity in mm/hr
    layer4(2)=Lthik(sb,LidN,4)  ! thickness in mm
    pervFrac=Lmsat(sb,LidN,4) !pervious fraction in the pavement layer

    theta_WP2=Lthta(sb,LidN,1) !wilting point in soil layer
    theta_WP3=Lthta(sb,LidN,2) !wilting point in storage layer
 

    !!INITIAL PARAMETERS!! (to be read from SWAT input file)
    D1=LID_FB(sb,LidN)      !freeboard thickness in mm
    phi1 =LID_VoidFr(sb,LidN)  !surface void fraction



    !!underdrain parameters(input) (to be read from SWAT input file)
    D_drain=LD_drain(sb,LidN)
    N_drain=LN_drain(sb,LidN)
    D3D=LD_height(sb,LidN)
    h_open=D3D+D_drain*1000.
    h_close=D3D





    !!PARAMETER ASSIGNMNETS

    !LID layer thickness
    D2 = layer1(5)
    D3 = layer2(5)
    D4 = layer4(2)


    !LID layer Field capacity values
    theta_FC2=theta_WP2+0.02
    theta_FC3=theta_WP3+0.02
    !layer saturation moisture content
    phi2 = layer1(2)
    phi3=layer2(2)
    !LID Layer percolation factors
    HCO1=44
    HCO2=48
    !underdain (intermediate)
    C3D=0.6




    !!INITIALIZATION


    if (k==1) then
          d=Lsur_last(j,LidN)
          d_4=Lpav_last(j,LidN)
          !LID layer initial moisture contents
          theta2 = LmIni(sb,LidN,1)
          theta3 = LmIni(sb,LidN,2)
          F_ini=LFcum_last(j,LidN)
          q3_old=Ldrain_last(j,LidN)
           Lcum_exfil(j,LidN)=0.
           Lcum_surq(j,LidN)=0.
    else
           d=Lsur(j,LidN,k-1)
            d_4=Lpav(j,LidN,k-1)
           theta2=Lsol(j,LidN,k-1)
           theta3=Lstor(j,LidN,k-1)
          F_ini=LFcum(j,LidN,k-1)
          q3_old=LIDdrain(j,LidN,k-1)
    end if

    jL=WFront(j,LidN,1)
    kL=WFront(j,LidN,2)

    x_new=[0,0,0,0]
    G_new=[0,0,0,0]
    x=[phi1*d, D2*theta2, D3*theta3,d_4*pervFrac]
    G=G_new
    omega=0.5
    surf_layer =0.
    soil_layer = 0.
    stor_layer = 0.
    surf_outflow = 0.
    drain_flow= 0.
    exfil= 0.
    evap= 0.




!!computation


    do itr=1,dez_t,1

        do iteration=1,100,1

            availEvap=ET
            e1=min(availEvap,(d*phi1/delta_t))
            e1=max(0.0,e1)
            availEvap=max(0.0,(availEvap-e1))

            f4=min(layer4(1),(LRAIN+(d/delta_t)))

            CALL mlga_three(f4,d,layer1,layer2,layer3,F_ini,k+itr*delta_t,delta_t,dez_t,jL,kL,inf_out,F_out)

            F_next=F_out
            F_ini=F_next


            if (inf_out>0) then
                e2=0
                e3=0
                e4=0

                else
                    e4=min(availEvap,(D4*pervFrac/delta_t))
                    availEvap=max(0.0,(availEvap-e4))
                    e2=min(availEvap,(theta2*D2/delta_t))
                    availEvap=max(0.0,(availEvap-e2))

                    if (theta2<phi2) then
                        e3=min(availEvap,(theta3*D3/delta_t))
                        else
                            e3=0
                    end if
            end if

            !!!flux advancing to second layer!!!

            if (F_next>=D2*(phi2-layer1(3))) then
                f2=min(inf_out,layer2(4))
                if (inf_out==0) then
                    rate=layer1(4)*exp(-HCO1*(phi2-theta2))
                    f2=rate
                elseif ((inf_out==0).AND.(theta2<=theta_WP2)) then
                    f2=0
                end if
            else
                f2=0
            end if

            !!!flux advancing to third layer!!!

            if (F_next>=(D2*(phi2-layer1(3)))+(D3*(phi3-layer2(3)))) then
                f3=min(f2,layer3(4))
                if (inf_out==0) then
                    rate=layer2(4)*exp(-HCO2*(phi3-theta3))
                    f3=rate
                elseif ((inf_out==0).AND.(theta3<=theta_WP3)) then
                    f3=0

                end if
            else
                f3=0

            end if


            !!underdrain flow rate calculations

            if (D_drain==0) then
                q3=0
            elseif (D_drain>0) then
                head=D3*theta3

                if (theta3>=phi3) then !when storage layer is full
                    if(D2>0)then       !if storage layer exists
                        if(theta2>theta_FC2) then
                            head=head+((theta2-theta_FC2)/(phi2-theta_FC2))*D2
                            if(theta2>=phi2)then  !soil layer is saturated
                                if (D4>0)then
                                    head=head+d_4
                                    if (d_4>=D4)then
                                        head=head+d
                                        end if
                                end if

                            end if
                        end if

                    end if
                    else
                        head=0.

                end if


                if ((q3_old==0).AND.(head<=h_open)) then
                    q3=0
                end if

                if ((q3_old>0).AND.(head<=h_close)) then
                    q3=0
                end if


               if (head>D3D) then
                head=head-D3D
                q3=C3D*(3.1416*D_drain*D_drain/4)*sqrt(2*9.81*head/1000)*N_drain
                q3=(q3/A_LID)*1000*3600
                else
                    q3=0
               end if



            end if



            q1=max(0.0,(d-D1)/delta_t)

            G1=LRAIN-f4-e1-q1
            G2=inf_out-e2-f2


            if (D3==0)then
                G3=0
                else
                    G3 = f2-e3-f3-q3
            endif
            G4=f4-e4-inf_out

            G_new=[G1,G2,G3,G4]
            x_new = x + ((omega*G_new) + ((1-omega)*G))*delta_t

            d=x_new(1)/phi1
            if (d<0)then
                d=0
            end if
           x_new(1)=d*phi1


           theta2 = x_new(2)/D2
           if (theta2<theta_WP2) then
                theta2=theta_WP2
            elseif (theta2>phi2) then
                theta2=phi2
            endif
            x_new(2)=D2*theta2



            theta3 = x_new(3)/D3
           if (theta3<theta_WP3) then
                theta3=theta_WP3
            elseif (theta3>phi3) then
                theta3=phi3
            endif
            x_new(3)=D3*theta3

            d_4=x_new(4)/(pervFrac)
            if (d_4<0)then
                d4=0
                elseif (d_4>D4) then
                    d_4=D4
            end if
            x_new(4)=d_4*(pervFrac)


            err_mat=abs(x_new-x)
            x=x_new
            G=G_new

            if (sum(err_mat)<tol) exit


        end do  !!end of iteration
        infi=inf_out
        C_infi=F_out
        evap=e1+e2+e3
        surf_outflow=q1
        drain_flow=q3
        exfil=f3
        surf_layer=d
        soil_layer=theta2
        stor_layer=theta3

        x=x_new
        G=G_new


    end do  !! end of discretization



    LFcum(j,LidN,k)=C_infi   !cumulative infiltration at the end of discretization
    Lsur(j,LidN,k)=surf_layer  !ponded water on surface layer
    LIDdrain(j,LidN,k)=drain_flow!drainage flow
    Lexfil(j,LidN,k)=exfil     !exfiltration from the bottom of LID layer
    Lsol(j,LidN,k)=soil_layer !moisture content in soil layer
    Lstor(j,LidN,k)=stor_layer !moisture content in storage layer
    Lpav(j,LidN,k)=d_4                  !water depth in the pavement layer
    Lsurq(j,LidN,k)=(surf_outflow+drain_flow)*del_t*fcimp(urblu(j))*mlid_farea(j,LidN) !runoff from LID

    Lcum_surq(j,LidN)=Lcum_surq(j,LidN)+Lsurq(j,LidN,k) ! cumulative surface runoff in a day in mm
    Lcum_exfil(j,LidN)=Lcum_exfil(j,LidN) + exfil*del_t*fcimp(urblu(j))*mlid_farea(j,LidN)



    !!whether these variables should go as an output to SWAT??



    if (theta2<=theta_FC2) then
        LmIni(sb,LidN,1)=theta2
        Lsur(j,LidN,k)=0
        LFcum(j,LidN,k)=0
        jL=1
        kL=1

    end if

    if (theta3<=theta_FC3) then
         LmIni(sb,LidN,2)=theta3
    end if

    if (k==nstep)then
        LFcum_last(j,LidN)=LFcum(j,LidN,k)
        Lsur_last(j,LidN)=Lsur(j,LidN,k)
        Ldrain_last(j,LidN)=LIDdrain(j,LidN,k)
        Lexfil_last(j,LidN)=Lexfil(j,LidN,k)
        Lsurq_last(j,LidN)=Lsurq(j,LidN,k)
        Lpav_last(j,LidN)=Lpav(j,LidN,k)
        Lstor_last(j,LidN)=(Lsur(j,LidN,k)+Lsol(j,LidN,k)*layer1(5)+Lstor(j,LidN,k)*layer2(5)+Lpav(j,LidN,k))*fcimp(urblu(j))*mlid_farea(j,LidN)

    end if

    WFront(j,LidN,1)=jL
    WFront(j,LidN,2)=kL


return

END SUBROUTINE





