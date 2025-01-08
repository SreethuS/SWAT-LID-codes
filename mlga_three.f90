
    SUBROUTINE mlga_three(inflow,d,layer1,layer2,layer3,F_ini,t,delta_t,dez_t,jL,kL,inf_out,F_out) !t=time(i)
        IMPLICIT NONE
        INTEGER ::jL,kL,ii,iter_num,dez_t
        REAL*8::inflow,d,F_ini,delta_t,t
        REAL*8::f,F_cum,F_out,inf_out
        REAL*8::layer1(5),layer2(5),layer3(5)
        REAL*8::SuctionHead_1,thetaSat_1,thetaIni_1,Ks1,L1,theta_d1
        REAL*8::SuctionHead_2,thetaSat_2,thetaIni_2,Ks2,L2,theta_d2
        REAL*8::SuctionHead_3,thetaSat_3,thetaIni_3,Ks3,L3,theta_d3
        REAL*8::ia,delta_F,Fs,F1,C,FF2,Kbar
        REAL*8:: f2(50),e(50)  !!for Newton-Raphson iteration
        REAL*8:: Er,Nr,Dr    !! for Newton-Raphson iteration
        REAL*8::T2_1(dez_t),T3_1(dez_t) !! for storing time
        REAL*8::A1,B1,A2,B2,delta_t1,ABF  !! for layers beneath the first layer
    

         T2_1=0.
         T3_1=0.
         A1=0.
         B1=0.
         A2=0.
         B2=0.
         ABF=0.
         F1=0.
         FF2=0.

        SuctionHead_1 = layer1(1)
        thetaSat_1 = layer1(2)
        thetaIni_1 = layer1(3)
        Ks1 = layer1(4)
        L1=layer1(5)
        theta_d1= thetaSat_1-thetaIni_1 !soil moisture deficit at the start of the rainfall

        SuctionHead_2 = layer2(1)
        thetaSat_2 = layer2(2)
        thetaIni_2 = layer2(3)
        Ks2 = layer2(4)
        L2=layer2(5)
        theta_d2= thetaSat_2-thetaIni_2  !soil moisture deficit at the start of the rainfall

        SuctionHead_3 = layer3(1)
        thetaSat_3 = layer3(2)
        thetaIni_3 = layer3(3)
        Ks3 = layer3(4)
        L3=layer3(5)
        theta_d3= thetaSat_3-thetaIni_3   !soil moisture deficit at the start of the rainfall




        ia = inflow + (d/delta_t) !available rainfall rate
        F_cum=F_ini
        iter_num=(size(f2)-5)


        !!Time to ponding in the first layer
        !F_cum = cumulative infiltration(F)






        IF (F_cum<L1*theta_d1) then
       !print*,"first layer"





            IF (ia==0) then

                f=0 !infiltration rate
                delta_F = 0.
                F_cum=F_cum+delta_F


            ELSEIF (ia <= Ks1) then


                f=ia
                delta_F = ia*delta_t
                F_cum=F_cum+delta_F

            else

                Fs = (Ks1*SuctionHead_1*theta_d1)/(ia -Ks1)

                IF (F_cum>=Fs) then !saturated condition
                    !print*,"F_cum>Fs"

                    F1=F_cum
                    C = Ks1*delta_t + F1-(SuctionHead_1*theta_d1*log(F1+(SuctionHead_1*theta_d1)))
                    f2=0.
                    f2(1)=ia*delta_t !initial guess
                    Er=0.1 !error in iteartion
                    


                    !!Newton -Raphson method
                    do ii=1,iter_num,1


                        f2(ii+1) =(f2(ii) - dble((C +(SuctionHead_1*theta_d1*log(f2(ii)+(SuctionHead_1*theta_d1)))&
                         -f2(ii))/((SuctionHead_1*theta_d1/(f2(ii)+SuctionHead_1*theta_d1))-1)))
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
                    C = Ks1*delta_t1 + F1-(SuctionHead_1*theta_d1*log(F1+(SuctionHead_1*theta_d1)))
                     !write(*,*),C
                     f2=0.
                    f2(1)=ia*delta_t !initial guess

                    Er=0.1 !iteration error

                    !!Newton Raphson method
                    do ii=1,iter_num,1
                      f2(ii+1) =(f2(ii) - dble((C+(SuctionHead_1*theta_d1*log(f2(ii)+(SuctionHead_1*theta_d1)))&
                       -f2(ii))/((SuctionHead_1*theta_d1/(f2(ii)+SuctionHead_1*theta_d1))-1)))

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

        ELSEIF ((F_cum>=L1*theta_d1).AND.(F_cum< (L1*theta_d1)+(L2*theta_d2))) then
             ! print*," second layer"

            T2_1(jL)=t !time when wetting front crosses the second layer
            jL=jL+1
            if (jL>=dez_t) then
                jL=dez_t
            end if

            Kbar=(L1+L2)/((L1/Ks1)+(L2/Ks2))


            IF (ia==0) then
                f=0
                F_cum=F_cum+f*delta_t
            ELSEIF (ia<=Kbar) then
                f = ia
                F_cum = F_cum+f*delta_t
            else
                A1 = ((L1 -(L1*Ks2/Ks1) +SuctionHead_2)*theta_d2)
                      
                B1 = (((L1*Ks2/Ks1)*theta_d2)- L1*theta_d1)
                
                
                F1 = L1*theta_d1
            
                ABF=A1+B1+F1
                
                f2=0.
                e=0.
                f2(1)=Kbar*delta_t
                Er=0.1
                
            

                !Newton Raphson
                do ii=1,iter_num
                    
                    
                     if (((A1+B1+f2(ii))/(ABF)) <=1) then
                            Nr = real(-f2(ii) + F1 + (Ks2*(t-T2_1(1))))
                        else
                         Nr = real(-f2(ii) + F1 + (Ks2*(t-T2_1(1))) + (A1*log((A1+B1+f2(ii))/(ABF))))  
                        endif
                  
                  
                    Dr= -1+ real(A1/(A1+B1+f2(ii)))


                    f2(ii+1)=f2(ii)-(Nr/Dr)
                    e(ii) = abs((f2(ii+1)-f2(ii))/f2(ii))

                    if (e(ii)<Er) EXIT

                end do
              
               
                delta_F = F_cum-f2(ii+1)
                f = Ks2*(1+(A1/(B1+f2(ii+1))))
                F_cum = F_cum+f*delta_t


                IF (delta_F >(ia*delta_t)) then !condition to unsaturated

                    delta_F = Kbar*delta_t
                     F_cum = F_cum+delta_F

                     f= delta_F/delta_t

                 else
                    f = Ks2*(1+ (A1/(B1+f2(ii+1))))

                    F_cum = F_cum+f*delta_t

                    IF (f<0.0) then
                        f=delta_F/delta_t
                        F_cum = F_cum+f*delta_t

                    END IF


                END IF
            END IF

        ELSEIF ((F_cum>=(L2*theta_d2) + (L1*theta_d1)) .AND.(F_cum<(L2*theta_d2) + (L1*theta_d1)+(L3*theta_d3))) then
             ! print*," third layer"

                 T3_1(kL)=t
                 kL=kL+1
                if (kL>=dez_t) then
                kL=dez_t
              end if

                 Kbar = (L1+L2+L3)/((L1/Ks1)+(L2/Ks2)+(L3/Ks3))

                 if (ia ==0) then
                    f=0.
                    F_cum=F_cum+f*delta_t
                 elseif (ia<=Kbar) then
                    f=ia
                    F_cum=F_cum+f*delta_t
                 else
                    A2 = real(((L1+L2)- ((L1*Ks3/Ks1)+ (L2*Ks3/Ks2))+ SuctionHead_3)*theta_d3)
                   
                    
                    B2 = real(((L1*Ks3/Ks1)+(L2*Ks3/Ks2))*theta_d3 - ((L1*theta_d1) + (L2*theta_d2)))
                   
                    FF2 = (L1*theta_d1)+(L2*theta_d2)
                    ABF=A2+B2+FF2
                    f2=0.
                    e=0.
                    f2(1)=Kbar*delta_t
                    Er=0.1
                    
                
                    !Newton Raphson
                    do ii=1,iter_num
                        
                        if (((A2+B2+f2(ii))/(ABF)) <=1) then
                            Nr = real(-f2(ii) + FF2 + (Ks3*(t-T3_1(1))))
                        else
                        Nr = real(-f2(ii) + FF2 + (Ks3*(t-T3_1(1))) + (A2*log((A2+B2+f2(ii))/(ABF))))
                        endif
            
                          
                          Dr= -1+ real(A2/(A2+B2+f2(ii)))


                           f2(ii+1)=f2(ii)-(Nr/Dr)


                        e(ii) = abs((f2(ii+1)-f2(ii))/f2(ii))
                         if (e(ii)<Er) EXIT

                    end do
                    delta_F = F_cum-f2(ii+1)
                
                    f = Ks3*(1+ (A2/(B2+f2(ii+1))))
                     F_cum = F_cum+f*delta_t

                    if (delta_F > (ia*delta_t)) then
                        delta_F = Kbar*delta_t
                        F_cum = F_cum+delta_F
                        f= delta_F/delta_t
                    else
                       f =Ks3*(1+ (A2/(B2+f2(ii+1))))

                        F_cum = F_cum+f*delta_t

                        if (f<0.0) then
                            f=delta_F/delta_t
                          F_cum = F_cum+f*delta_t
                        end if
                    end if

                 end if

        ELSEIF (F_cum>(L1*theta_d1)+(L2*theta_d2)+(L3*theta_d3)) then !all layers are saturated
             !print*," alllayer"

            Kbar = (L1+L2+L3)/((L1/Ks1)+(L2/Ks2)+(L3/Ks3))
            if (ia ==0) then
                f=0.
                F_cum = F_cum+f*delta_t
            elseif (ia<=Kbar) then
                f = ia
                F_cum = F_cum+f*delta_t
                else
                    f=Kbar
                    F_cum = F_cum+f*delta_t

            end if



        END IF



        inf_out=f
        F_out=F_ini+inf_out*delta_t

        return

        END SUBROUTINE 
