	PROGRAM quadinter
		INTEGER:: j,k,l,points,i, stat, N
		REAL:: x0,x1,x2, v0, vc0,v1, vc1,v2, vc2
		REAL:: x,v,vc, xcount, l0,l1,l2


		
		Open (5, file='pot.dat')
		Open (6, file='Global1.DET')
		
! 		print*, "Number of points"
!       read(*,*)points
 
        step = 0.02
        N =  0.1/0.02
		xcount = 0.0
		x=0.01


		read(5,*) x2, v2, vc2		
		read(5,*) x1, v1, vc1			
		read(5,*) x0, v0, vc0

		stat = 0
	  	DO WHILE (stat.EQ.0) 
		
			DO i=0, (N - 1)

			
				l0 = (x-x1)*(x-x2)/((x0-x1)*(x0-x2))
				l1 = (x-x0)*(x-x2)/((x1-x0)*(x1-x2))
				l2 = (x-x0)*(x-x1)/((x2-x0)*(x2-x1))
				
				v = v0*l0 + v1*l1 + v2*l2
				vc = vc0*l0 + vc1*l1 + vc2*l2
			
				write(6,'(f5.2,x,f12.10,x,f12.10,3f12.10)') x, v, vc
				x = x + step
			ENDDO
		
			x3 = x2
			v3 = v2
			vc3 = vc2
			
			x2 = x1
			v2 = v1
			vc2 = vc1
		 	    
			read(5,*,IOSTAT=stat) x1, v1, vc1
			if (stat .ne. 0)  exit
	
	    Enddo
	    
	END PROGRAM
