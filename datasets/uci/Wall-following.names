1. Title of Database: Wall-Following navigation task with mobile robot SCITOS-G5

2. Sources:
   (a) Creators: 	Ananda Freire, Marcus Veloso and Guilherme Barreto
			Department of Teleinformatics Engineering
			Federal University of Ceará
			Fortaleza, Ceará, Brazil

   (b) Donors of database: Ananda Freire (anandalf@gmail.com)
			   Guilherme Barreto (guilherme@deti.ufc.br)	
	  
   (c) Date received: August, 2010

3. Past Usage:
   (a) 	Ananda L. Freire, Guilherme A. Barreto, Marcus Veloso and Antonio T. Varela (2009),
	"Short-Term Memory Mechanisms in Neural Network Learning of Robot Navigation
	Tasks: A Case Study". Proceedings of the 6th Latin American Robotics Symposium (LARS'2009),
	Valparaíso-Chile, pages 1-6, DOI: 10.1109/LARS.2009.5418323 

4. Relevant Information Paragraph:
   -- 	The data were collected as the SCITOS G5 navigates through the room following the wall in a clockwise
	direction, for 4 rounds. To navigate, the robot uses 24 ultrasound sensors arranged circularly around its "waist". 
	The numbering of the ultrasound sensors starts at the front of the robot and increases in clockwise direction.

   -- 	The provided files comprise three diferent data sets. The first one contains the raw values of the measurements 
	of all 24 ultrasound sensors and the corresponding class label (see Section 7). Sensor readings are sampled at a 
	rate of 9 samples per second.

	The second one contains four sensor readings named 'simplified distances' and the corresponding class label (see Section 7). 
	These simplified distances are referred to as the 'front distance', 'left distance', 'right distance' and 'back distance'. 
	They consist, respectively, of the minimum sensor readings among those within 60 degree arcs located at the front, left, 
	right and back parts of the robot.

	The third one contains only the front and left simplified distances and the corresponding class label (see Section 7). 
	
   --   It is worth mentioning that the 24 ultrasound readings and the simplified distances were collected at the same 		
	time step, so each file has the same number of rows (one for each sampling time step).                                                           

   --   The wall-following task and data gathering were designed to test the hypothesis that this apparently simple navigation task 
	is indeed a non-linearly separable classification task. Thus, linear classifiers, such as the Perceptron network, are not able 
	to learn the task and command the robot around the room without collisions. Nonlinear neural classifiers, such as the MLP network, 
	are able to learn the task and command the robot successfully without collisions. 

   --   If some kind of short-term memory mechanism is provided to the neural classifiers, their performances are improved in general. 
	For example, if past inputs are provided together with current sensor readings, even the Perceptron becomes able to 
	learn the task and command the robot succesfully. If a recurrent neural network, such as the Elman network, is used to 
	learn the task, the resulting dynamical classifier is able to learn the task using less hidden neurons than the MLP network.

   --   Files with different number of sensor readings were built in order to evaluate the performance of the classifiers 
	with respect to the number of inputs.

5. Number of Instances: 5456

6. Number of Attributes 
   -- sensor_readings_24.data: 24 numeric attributes and the class.
   -- sensor_readings_4.data:   4 numeric attributes and the class.
   -- sensor_readings_2.data:   2 numeric attributes and the class.

7. For Each Attribute: 
   -- File sensor_readings_24.data:
	 1. US1: ultrasound sensor at the front of the robot (reference angle: 180°) - (numeric: real)
	 2. US2: ultrasound reading (reference angle: -165°) - (numeric: real)
	 3. US3: ultrasound reading (reference angle: -150°) - (numeric: real)
	 4. US4: ultrasound reading (reference angle: -135°) - (numeric: real)
	 5. US5: ultrasound reading (reference angle: -120°) - (numeric: real)
	 6. US6: ultrasound reading (reference angle: -105°) - (numeric: real)
	 7. US7: ultrasound reading (reference angle: -90°) - (numeric: real)
	 8. US8: ultrasound reading (reference angle: -75°) - (numeric: real)
	 9. US9: ultrasound reading (reference angle: -60°) - (numeric: real)
	10. US10: ultrasound reading (reference angle: -45°) - (numeric: real)
	11. US11: ultrasound reading (reference angle: -30°) - (numeric: real)
	12. US12: ultrasound reading (reference angle: -15°) - (numeric: real)
	13. US13: reading of ultrasound sensor situated at the back of the robot (reference angle: 0°) - (numeric: real)
	14. US14: ultrasound reading (reference angle: 15°) - (numeric: real)
	15. US15: ultrasound reading (reference angle: 30°) - (numeric: real)
	16. US16: ultrasound reading (reference angle: 45°) - (numeric: real)
	17. US17: ultrasound reading (reference angle: 60°) - (numeric: real)
	18. US18: ultrasound reading (reference angle: 75°) - (numeric: real)
	19. US19: ultrasound reading (reference angle: 90°) - (numeric: real)
	20. US20: ultrasound reading (reference angle: 105°) - (numeric: real)
	21. US21: ultrasound reading (reference angle: 120°) - (numeric: real)
	22. US22: ultrasound reading (reference angle: 135°) - (numeric: real)
	23. US23: ultrasound reading (reference angle: 150°) - (numeric: real)
	24. US24: ultrasound reading (reference angle: 165°) - (numeric: real)
   	25. Class: 
      		-- Move-Forward
      		-- Slight-Right-Turn
      		-- Sharp-Right-Turn
      		-- Slight-Left-Turn

   -- File sensor_readings_4.data:
	1. SD_front: minimum sensor reading within a 60 degree arc located at the front of the robot - (numeric: real)
	2. SD_left:  minimum sensor reading within a 60 degree arc located at the left of the robot  - (numeric: real)
	3. SD_right: minimum sensor reading within a 60 degree arc located at the right of the robot - (numeric: real)
	4. SD_back:  minimum sensor reading within a 60 degree arc located at the back of the robot - (numeric: real)
   	5. Class: 
      		-- Move-Forward
      		-- Slight-Right-Turn
      		-- Sharp-Right-Turn
      		-- Slight-Left-Turn

   -- File sensor_readings_2.data:
	1. SD_front: minimum sensor reading within a 60 degree arc located at the front of the robot - (numeric: real)
	2. SD_left:  minimum sensor reading within a 60 degree arc located at the left of the robot - (numeric: real)
   	3. Class: 
      		-- Move-Forward
      		-- Slight-Right-Turn
      		-- Sharp-Right-Turn
      		-- Slight-Left-Turn

   -- Summary Statistics:
	-- File sensor_readings_24.data:
		 Max	  Min	  Mean	    SD		
	  US1	5.0000	0.40000	 1.47162  0.80280                                        			                     
	  US2 	5.0250	0.43700	 2.32704  1.41015
   	  US3	5.0290	0.47000  2.48935  1.24743
   	  US4	5.0170	0.83300  2.79650  1.30937
      	  US5	5.0000	1.12000  2.95855  1.33922
      	  US6	5.0050	1.11400  2.89307  1.28258
      	  US7	5.0080	1.12200  3.35111  1.41369
      	  US8	5.0870	0.85900  2.54040  1.11155
      	  US9	5.0000	0.83600  3.12562  1.35697                                                          
      	  US10	5.0220	0.81000  2.83239  1.30784
      	  US11	5.0190	0.78300  2.54940  1.38203
      	  US12	5.0000	0.77800  2.07778  1.24930
      	  US13	5.0030	0.77000  2.12578  1.40717
     	  US14	5.0000	0.75600  2.19049  1.57687
      	  US15	5.0000	0.49500  2.20577  1.71543
      	  US16	5.0000	0.42400  1.20211  1.09857
      	  US17	5.0000	0.37300  0.98983  0.94207                                                          
      	  US18	5.0000	0.35400  0.91027  0.88953
      	  US19	5.0000	0.34000  1.05811  1.14463
      	  US20	5.0000	0.35500  1.07632  1.14150
      	  US21	5.0000	0.38000  1.01592  0.88744
      	  US22	5.0000	0.37000  1.77803  1.57169
      	  US23	5.0000	0.36700  1.55505  1.29145
      	  US24	5.0000	0.37700  1.57851  1.15048

	-- File sensor_readings_4.data:
		       Max    Min       Mean	    SD	
	   SD_front	5   0.49500   1.29031	  0.62670                     
	   SD_left	5   0.34000   0.68127	  0.34259
	   SD_right	5   0.83600   1.88182	  0.56253
	   SD_back	5   0.36700   1.27369	  0.82175

	-- File sensor_readings_2.data:
		       Max    Min       Mean	    SD	
	   SD_front	5   0.49500   1.29031	  0.62670                     
	   SD_left	5   0.34000   0.68127	  0.34259

 
8. Missing Attribute Values: none
 
9. Class Distribution: 
	-- Move-Forward: 2205 samples (40.41%).
      	-- Slight-Right-Turn: 826 samples (15.13%).
      	-- Sharp-Right-Turn: 2097 samples (38.43%).
      	-- Slight-Left-Turn: 328 samples (6.01%).

