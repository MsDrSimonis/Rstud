###########################
#
#	An example of how to use the Rstud code to conduct inbreeding analyses
#
#		Written by Ms. Dr. Joseph L. Simonis while working at the Lincoln Park Zoo, Chicago, IL, 2013 - 
#	
#
#
#


#########################################
#
#	Table of Contents
#
#########################################
#
#	1. Preliminaries
#	2. Measure inbreeding and summarize over time
#	3. Analyze inbreeding depression




######################################
#
#	1. Preliminaries 
#


	#
	# source the R script with all of the functions


		source("Rstud_Functions.R")


	#
	# load the necessary packages
	#

		PackageLoad()



	#
	# all of the input variables for the analyses	
	#


		StudbookName ="NubianIbexStudbook_14Nov14";
		OverlayToUse="Overlay for PVA_MLT";

		startingDate="1988-01-01";
		endingDate="2012-01-22";

		institutions="AZA";
		ADDinstitutions = NULL;
		DROPinstitutions = NULL;

		UDF_FieldName = NULL;
		UDF_Levels = NULL;

		mortalityWindow=365;
		birthingWindow=10;
		censusDate="01-01";

		GraphResolution=100;
		Tables=TRUE;
		Graphs=TRUE;
		modelsOut=TRUE;



	#
	# get the studbook
	#	-in order for this to work, you need to have opened the studbook in poplink on your computer


		SB<-SBsetup(DBname = StudbookName, DBtablenames = c("Master", "Event", "Sex"), Overlay=TRUE, UDF=TRUE, verbose=FALSE, silent=TRUE)


	#
	# apply the overlay indicated 
	#


		SBwithOL<-overlayApply(Studbook = SB, OverlayToUse = OverlayToUse, verbose=FALSE, Remove = TRUE, AddIsHypothetical=TRUE)





######################################
#
#	2. Measure inbreeding and summarize over time
#


	#
	# Measure inbreeding in the population
	#	-uses the calcInbreeding function in the pedigree package


		Inbreeding<-SBinbreeding(Studbook = SBwithOL, verbose=FALSE, silent=TRUE)


	#
	# Write out the inbreeding table
	#


		tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Inbreeding_Table.csv", sep="_")
		write.table(Inbreeding, tname, sep=",", row.names=F)


	#
	# Summarize the inbreeding in the population over time 
	#


		#
		# By births/hatches
		#

			# Birth or hatch?

				lab<-as.character(unique(SBwithOL$Event$TranCode)[which(unique(SBwithOL$Event$TranCode) %in% c("Birth", "Hatch"))])
				lab[lab=="Birth"]<-"Births"
				lab[lab=="Hatch"]<-"Hatches"

			# restricting the births to those within the institution and time windows, and based on UDFs

				BHin<-specificEvents(Studbook = SBwithOL, eventType = c("Birth", "Hatch"), startingDate=startingDate, endingDate=endingDate, 
							institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels)

			# trim and format the data

				BHin<-BHin[,which(colnames(BHin) %in% c("StudbookID", "TranDate"))]
				BHin<-data.frame(BHin, Year=format(BHin$TranDate, "%Y"), Fval=Inbreeding$Fval[which(Inbreeding$StudbookID %in% BHin$StudbookID)])

			# doing by year, so summarize Fvals by year
					
				birthFvals<-summarizeFoverTime(Ftable=BHin, startingDate=startingDate, endingDate=endingDate)

			# write to table 

				tname<-paste(paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Historical_Inbreeding_", sep="_"), lab, ".csv", sep="")
				write.table(birthFvals, tname, sep=",", row.names=F)

			# write to graph

				graphFoverTime(Studbook=SBwithOL, FsummaryTable=birthFvals, GraphResolution=GraphResolution, type="BH")


		#
		# Whole population [NOTE: this runs slowly]
		#
				

			# census the population to figure out who was in it each year on the census date

				censusPop<-censusPopulation(Studbook=SBwithOL,startingDate=startingDate, endingDate=endingDate, institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, 
					UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels, censusDate=censusDate)


			# tack on their F values 

				Fval<-rep(NA, nrow(censusPop))
				for(i in 1:nrow(censusPop))
					Fval[i]<-Inbreeding$Fval[which(as.character(Inbreeding$StudbookID)==as.character(censusPop$StudbookID[i]))]

				Fvals<-data.frame(censusPop, Fval)


			# summarize Fvals by year
					
				popFvals<-summarizeFoverTime(Ftable=Fvals	, startingDate=startingDate, endingDate=endingDate)


			# write to table if desired

				tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Historical_Inbreeding_Population.csv", sep="_")
				write.table(popFvals, tname, sep=",", row.names=F)
				

			# write to graph
	
				graphFoverTime(Studbook=SBwithOL, FsummaryTable=popFvals, GraphResolution=GraphResolution, type="Pop")




######################################
#
#	3. Analyze inbreeding depression
#


	#
	# Infant Mortality
	#

		# Measure the lifespan of each individual and note whether they count for mortality or not and whether they lived/died within the window

			BH<-as.character(unique(SBwithOL$Event$TranCode)[which(unique(SBwithOL$Event$TranCode) %in% c("Birth", "Hatch"))])
			LS<-rep(NA, nrow(BHin))
			Status<-rep(NA, nrow(BHin))
			InfMort<-rep(NA, nrow(BHin))		

			for(i in 1:nrow(BHin)){
				xx<-lifespan(Studbook=SBwithOL, StudbookID=BHin$StudbookID[i], endingDate=endingDate, BH=BH)
				LS[i]<-as.numeric(xx[2])
				Status[i]<-xx[1]
				InfMort[i]<-mortWindow(LifeSpan = LS[i], Status = Status[i], mortalityWindow=mortalityWindow)
			}
	

		# determine everyone's sex

			Sex<-rep(NA, nrow(BHin))

			for(i in 1:nrow(BHin)){
				xx<-SBwithOL$Sex$Sex[which(SBwithOL$Sex$StudbookID==BHin$StudbookID[i])]
				Sex[i]<-as.character(xx[length(xx)]	)
			}


		# combine into one dataset, reduce to just those that can be used (lived at least the mortWindow/died before it)

			InfantMortalities<-data.frame(BHin, LS, Status, InfMort, Sex)
			InfantMortalities<-InfantMortalities[which(is.na(InfantMortalities$InfMort)==FALSE),]
		


		# summarize data to see if it's sufficient for analyses

			# F values:

				summary(InfantMortalities$Fval)
		
			# number of infant mortalities:
		
				sum(InfantMortalities$InfMort)
				length(InfantMortalities$InfMort)


		# models

			IMtable<-InfantMortalities


			# check if sex is significant
			#	requires pulling out unknown and other sex animals


				IMtable2 <-IMtable[-which(IMtable$Sex=="Unknown" | IMtable$Sex=="Other"),]
	
				IMmod1<-glm(InfMort~Sex, data=IMtable2, family='binomial')
				summary(IMmod1)


				# if sex is significant, check F value
			
					IMmod2<-glm(InfMort~Sex+Fval, data=IMtable2, family='binomial')
					summary(IMmod2)


				# if sex and F value are significant, check interaction

					IMmod3<-glm(InfMort~Sex*Fval, data=IMtable2, family='binomial')
					summary(IMmod3)



				# if sex is NOT significant, check F value
						
					IMmod4<-glm(InfMort~Fval, data=IMtable, family='binomial')
					summary(IMmod4)



				# whichever model you would like to write out, put into the "summary()" call

					mname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Inbreeding_Infant_Mortality_Model.txt", sep="_")

					sink(file=mname)
					summary(	)	#put the name of the model in there, like IMmod4 or whatnot
					sink()
			


		# graphing

			gname<-paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "Infant_Mortality_Inbreeding_Analysis_Graph.tif", sep="_")


			# point type
		
				pnts<-as.character(IMtable$Sex)
				pnts[which(pnts!="Male" & pnts!="Female")]<-1
				pnts[which(pnts=="Female")]<-2
				pnts[which(pnts=="Male")]<-5
				pnts<-as.numeric(pnts)


			# the locations of the points to be graphed (Fvals and InfMorts, plus some jittering noise)

				yvs<-rnorm(n=length(IMtable$InfMort), mean=IMtable$InfMort, sd=.04)
				xvs<-rnorm(n=length(IMtable$Fval), mean=IMtable$Fval, sd=.003)

			
			# model-predicted line(s), which depends on the best-fit model 
			#	-but only do this if F is significant


				xpred<-seq(0, max(IMtable$Fval), length.out=100)


				# if sex is signif, no interaction				
					
					yMpred<-predict.glm(IMmod2, newdata=list(Fval=xpred, Sex=rep("Male", length(xpred))),type='response')
					yFpred<-predict.glm(IMmod2, newdata=list(Fval=xpred, Sex=rep("Female", length(xpred))),type='response')

				# if sex is signif, yes interaction				
					
					yMpred<-predict.glm(IMmod3, newdata=list(Fval=xpred, Sex=rep("Male", length(xpred))),type='response')
					yFpred<-predict.glm(IMmod3, newdata=list(Fval=xpred, Sex=rep("Female", length(xpred))),type='response')


				# if sex isnt signif				
					
					ypred<-predict.glm(IMmod4, newdata=list(Fval=xpred),type='response')


			# actual graphing
					
				tiff(file = gname, width=14, height = 8, units = "in", res=200)
				par(mar=c(6,7,1,6.5))

				plot(1~1, ylim=c(min(yvs)-.01,max(yvs)+.01), xlim=c(min(xvs)-.01,max(xvs)+.01), ylab="", xlab="", yaxt='n', xaxt='n', type='n')
				points(xvs, yvs, lwd=2, cex=1.5, pch=pnts)

				axis(1, cex=1.25, at=seq(0, max(IMtable$Fval),.05),cex.axis=1.25)
				mtext(side=1, "Inbreeding Coefficient", line=3.75, cex=1.75)

				axis(2, at=c(0,1), labels=c("Survived", "Died"), las=1, cex.axis=1.25)
				mtext(side=2, "Infant Mortality (points, jittered)", line=5, cex=1.75)
		
				axis(4, at=seq(0,1,.2), las=1, cex.axis=1.25)
				mtext(side=4, "Probability of Infant Mortality (line)", line=4, cex=1.75)
	

				# line depends on which (if any) models are significant

					points(xpred, yMpred, type='l', lwd=3, lty=2)
					points(xpred, yFpred, type='l', lwd=3, lty=3)

					points(xpred, ypred, type='l', lwd=3, lty=1)
	

				dev.off()





	#
	# Clutch/Litter Size
	#


		# measure litter/clutch sizes

			LCsizes<-litterSize(Studbook = SBwithOL, startingDate=startingDate, endingDate=endingDate, institutions=institutions, ADDinstitutions=ADDinstitutions, 
				DROPinstitutions=DROPinstitutions, UDF_FieldName=UDF_FieldName, UDF_Levels=UDF_Levels, birthingWindow=birthingWindow)

	
		# save a table of litter/clutch sizes, and a summary table

			tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Specific_LitterClutch_Sizes.csv", sep="_")
			write.table(LCsizes, tname, sep=",", row.names=F)

			SummaryTable<-count(LCsizes, "LitterSize")
			tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Summary_LitterClutch_Sizes.csv", sep="_")
			write.table(SummaryTable, tname, sep=",", row.names=F)


		# get the inbreeding values for the mom, dad, and babies in each litter

			LSize<-LCsizes
			InbreedingTable<-Inbreeding

			MomF<-rep(NA, nrow(LSize))
			DadF<-rep(NA, nrow(LSize))
			BabyF<-rep(NA, nrow(LSize))

			for(i in 1:nrow(LSize)){

				BabyF[i]<-InbreedingTable$Fval[as.character(InbreedingTable$StudbookID)==as.character(LSize$ReferenceOffspring[i])]

				if(length(which(as.character(InbreedingTable$StudbookID)==as.character(LSize$Sire[i])))>0)
					DadF[i]<-InbreedingTable$Fval[as.character(InbreedingTable$StudbookID)==as.character(LSize$Sire[i])]

				if(length(which(as.character(InbreedingTable$StudbookID)==as.character(LSize$Dam[i])))>0)	
					MomF[i]<-InbreedingTable$Fval[as.character(InbreedingTable$StudbookID)==as.character(LSize$Dam[i])]		

			}

			LSize<-data.frame(LSize, MomF, DadF, BabyF)
			naD<-c(which(is.na(LSize$DadF)), which(is.na(LSize$MomF)))
			LSize2<-LSize
			if(length(naD)>0)
				LSize2<-LSize2[-naD,]	# currently removing all entries with NA parent(s)


		# models!

			# for now, using standard poisson model, probably should switch to positive poisson via vglm
			# also doenes't including blocking by parents or pedigree

			# just intercept
				CLmod0<-glm(LitterSize~1, data=LSize2, family='poisson')

			# MomF
				CLmod1<-glm(LitterSize~MomF, data=LSize2, family='poisson')

			# DadF
				CLmod2<-glm(LitterSize~DadF, data=LSize2, family='poisson')

			# BabyF
				CLmod3<-glm(LitterSize~BabyF, data=LSize2, family='poisson')

			summary(CLmod1)
			summary(CLmod2)
			summary(CLmod3)


			# complicate model from here following basic principles


			
			# write it out

			mname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Inbreeding_Brood_Size_Model.txt", sep="_")

			sink(file=mname)
			summary(	)		#put the name of the model in there, like CLmod4 or whatnot
			sink()


			
		# need to add graphics





