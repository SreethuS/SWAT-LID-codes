

subroutine MLGA_infTrench(sb,j,k,kk,lid_prec)

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
   !layN                 |none           |number of LID layers
   !WFront(:,:,:)        |none           |index for storing time when wetting front crosses layers


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

   !! for the j at the end of a day

   !LFcum_last           |mm             |cumulative infiltration
   !Lsur_last            |mm             |ponded water depth
   !Ldrain_last          |mm/h           |under-drain flow
   !Lexfil_last          |mm/h           |exfiltration from the bottom of LID layer
   !Lsurq_last           |mm/h           |runoff from LID
   !Wfront               |none           |for storing the index when wetting front crosses layers



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

    INTEGER::sb,j,k,kk,LidN,layN
    REAL*8::lid_prec!input
    REAL*8:: D1,phi1,D3D,h_open,h_close,A_LID,D_drain,N_drain,ET



    !!OUTPUTS!!
    REAL*8::infi,C_infi,surf_outflow,drain_flow,exfil,evap,&
    stor_layer,surf_layer


    !!INTERMEDIATE!!
    REAL*8::LRAIN,del_t,delta_t,inf_out,F_out,d,F_ini! output from MLGA
    INTEGER::itr,dez_t,iteration,jL !intermediate parameters used in iteration
    REAL*8::tol,D2,D3,theta2,theta3,theta_FC2,theta_WP2,&
    phi2,phi3,q3_old,HCO1,omega,x_new(3),x(3),G_new(3),G(3),err_mat(3) !intermediate
    REAL*8::C3D,availEvap,e1,e2,F_next,f2,rate,q3,head,q1,G1,G2,G3 !intermediate
    REAL*8::layer1(5),layer2(5)

       q1=0.
       q3=0.
       rate=0.
       head=0.
       G1=0.
       G2=0.
       G3=0.
       e1=0.
       e2=0.
       f2=0.
       x_new=0.
       x=0.
       G_new=0.
       G=0.
       q3_old=0.

        sb = hru_sub(j)
        LidN=4
        LayN=2

    del_t=dfloat(idt)/60.

    !!time parameters
    delta_t=0.1     !computational time step
    dez_t=del_t/delta_t


    infi=0.
    C_infi=0.


    tol=1           ! tolerance

    !! Evapotranspiration input
    call etpot
    ET = pet_day/24.

    !! LID AREA
    A_LID=mlid_farea(j,LidN)*fcimp(urblu(j))*hru_km(j)*1000000.

    !! Rainfull input to LID

    LRAIN = lid_prec/del_t


    !! layer properties
    !for infiltration trench, the first layer-soil layer is absent

    layer1(1)=LsucH(sb,LidN,1) !suction head in mm
    layer1(2)=LmSat(sb,LidN,1) ! moisture content at saturation
    layer1(3)=LmIni(sb,LidN,1)!initial moisture content
    layer1(4)=Lksat(sb,LidN,1)!saturated hydraulic conductivity in mm/hr
    layer1(5)=Lthik(sb,LidN,1) !layer thickness in mm
    !layer2 = native soil
    layer2(1)=LsucH(sb,LidN,2) !suction head in mm
    layer2(2)=LmSat(sb,LidN,2) ! moisture content at saturation
    layer2(3)=LmIni(sb,LidN,2)!initial moisture content
    layer2(4)=Lksat(sb,LidN,2) !saturated hydraulic conductivity in mm/hr
    layer2(5)=Lthik(sb,LidN,2) !layer thickness in mm
    theta3=0

    theta_WP2=Lthta(sb,LidN,1) !wilting point in storage layer



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


    !LID layer Field capacity values
    theta_FC2=theta_WP2+0.02
    !layer saturation moisture content
    phi2 = layer1(2)
    phi3=layer2(2)
    !LID Layer percolation factors
    HCO1=80
    !underdain (intermediate)
    C3D=0.6




    !!INITIALIZATION


    if (k==1) then
          d=Lsur_last(j,LidN)
          !LID layer initial moisture contents
          theta2 = LmIni(sb,LidN,1)
          F_ini=LFcum_last(j,LidN)
          q3_old=Ldrain_last(j,LidN)
           Lcum_exfil(j,LidN)=0.
           Lcum_surq(j,LidN)=0.
    else
           d=Lsur(j,LidN,k-1)
           theta2=Lstor(j,LidN,k-1)
          F_ini=LFcum(j,LidN,k-1)
          q3_old=LIDdrain(j,LidN,k-1)
    end if

    jL=WFront(j,LidN,1)


    x_new=[0,0,0]
    G_new=[0,0,0]
    x=[phi1*d, D2*theta2, D3*theta3]
    G=G_new
    omega=0.5
    surf_layer =0.
    stor_layer = 0.
    surf_outflow = 0.
    drain_flow= 0.
    exfil= 0.
    evap= 0.




!!computation


    do itr=1,dez_t

        do iteration=1,100,1

            availEvap=ET
            e1=min(availEvap,(d*phi1/delta_t))
            e1=max(0.0,e1)
            availEvap=max(0.0,(availEvap-e1))


            CALL mlga_two(LRAIN,d,layer1,layer2,F_ini,k+itr*delta_t,delta_t,dez_t,jL,inf_out,F_out)

            F_next=F_out
            F_ini=F_next


            if (inf_out>0) then
                e2=0

                else
                    e2=min(availEvap,(theta2*D2/delta_t))
                    availEvap=max(0.0,(availEvap-e2))

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




            !!underdrain flow rate calculations

            if (D_drain==0) then
                q3=0
            elseif (D_drain>0) then
                head=D2*theta2

                    if(D2>0)then
                         if(theta2>=phi2) then
                            head=(((theta2-theta_FC2)/(phi2-theta_FC2))*D2)+d
                            else
                                head=0.

                        end if

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

            G1=LRAIN-e1-inf_out-q1
            G2=inf_out-e2-f2-q3


            G3=0

            G_new=[G1,G2,G3]
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



            err_mat=abs(x_new-x)
            x=x_new
            G=G_new

            if (sum(err_mat)<tol) exit


        end do  !! end of iteration
        infi=inf_out
        C_infi=F_out
        evap=e1+e2
        surf_outflow=q1
        drain_flow=q3
        exfil=f2
        surf_layer=d
        stor_layer=theta2


        x=x_new
        G=G_new


    end do  !! end of discretization


    LFcum(j,LidN,k)=C_infi  !cumulative infiltration at the end of discretization
    Lsur(j,LidN,k)=surf_layer  !ponded water on surface layer
    LIDdrain(j,LidN,k)=drain_flow!drainage flow
    Lexfil(j,LidN,k)=exfil    !exfiltration from the bottom of LID layer
    Lstor(j,LidN,k)=stor_layer !moisture content in storage layer
    Lsurq(j,LidN,k)=(surf_outflow+drain_flow)*del_t*fcimp(urblu(j))*mlid_farea(j,LidN)!runoff from LID

    Lcum_surq(j,LidN)=Lcum_surq(j,LidN)+Lsurq(j,LidN,k) ! cumulative surface runoff in a day in mm
    Lcum_exfil(j,LidN)=Lcum_exfil(j,LidN) + exfil*del_t*fcimp(urblu(j))*mlid_farea(j,LidN)


    if (theta2<=theta_FC2) then
        LmIni(sb,LidN,1)=theta2
        Lsur(j,LidN,k)=0.
        LFcum(j,LidN,k)=0.
        jL=1


    end if



    if (k==nstep)then
        LFcum_last(j,LidN)=LFcum(j,LidN,k)
        Lsur_last(j,LidN)=Lsur(j,LidN,k)
        Ldrain_last(j,LidN)=LIDdrain(j,LidN,k)
        Lexfil_last(j,LidN)=Lexfil(j,LidN,k)
        Lsurq_last(j,LidN)=Lsurq(j,LidN,k)
        Lstor_last(j,LidN)=(Lsur(j,LidN,k)+Lstor(j,LidN,k)*layer1(5))*fcimp(urblu(j))*mlid_farea(j,LidN)

    end if

    WFront(j,LidN,1)=jL


return

END subroutine





