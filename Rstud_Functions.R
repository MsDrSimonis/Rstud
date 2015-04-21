######################################################################################################################
######################################################################################################################
######################################################################################################################
########
########		This script contains functions for reading, editing, analyzing, and writing studbooks in R
########		
########		This code has been developed by Ms. Dr. Joseph L. Simonis while working at the Lincoln Park Zoo,
########			Chicago, IL, 2013 -
########
########		This code is still in development and not at all adequately tested
########			It will be in the future, though!
########
########		An example of how to use these functions is given in the Rstud_Example.R script
########
########
########		Direct comments and feedback to jsimonis@lpzoo.org
########
######################################################################################################################
######################################################################################################################
######################################################################################################################


#
#	Table of Contents (use Cntr + F to get to specific functions quickly)
#
#	1. Package Loading
#	2. Reading in Studbook
#	3. Applying Overlays to the Studbook
#	4. Read in, ammend, and apply institutional windows
#	5. Construct and apply date windows
#	6. Examine/apply UDFs
#	7. Determine the pedigree and genetic metrics		[have things to eventually add here]


#
#	To be coded/included:
#
#		Editing studbooks, including adding new entries
#		Writing edited studbooks back out
#		
#		Analyses: 
#			Infant mortality
#			Litter/clutch size
#			Inbreeding
#			Infant mortality ~ inbreeding
#			Litter/clutch size ~ inbreeding
#




#################################
#
#	1. Package Loading
#
#	
#		This set of functions is dependent on a suite of packages	
#		When this set of functions becomes its own package, these will be defined as dependencies, but for now, auto-load
#
#		Functions involved: PackageLoad
#


PackageLoad<-function(Packages = c("RODBC", "pedigree", "plyr", "MASS", "VGAM"), quiet=TRUE, verbose=FALSE, warn.conflicts=FALSE){

		# download required packages if they're not already

		pkgsToDownload<- Packages[!(Packages  %in% installed.packages()[,"Package"])]
		if(length(pkgsToDownload)>0)
			install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", quiet=quiet, verbose=verbose)

		# then load them
		for(i in 1:length(Packages))
			require(Packages[i], character.only=T, quietly=quiet, warn.conflicts=warn.conflicts)
}



#################################
#
#	2. Reading in Studbook
#
#	
#		Read in the studbook, print basic info re: UDFs and Overlays	
#		Currently assuming it's in SQL/PopLink and has been opened
#			will need to relax that assumption in the future
#
#		Functions involved: SBsetup [sub functions: overlayExamine]
#			Need to make the output prettier in the future
#			


#
#	Function for interacting with the database to read in the data

SBsetup<-function(DBname = NULL, DBtablenames = c("Master", "Event"), Overlay=TRUE, UDF=TRUE, verbose=TRUE, silent=FALSE){

	if(length(DBname)==0)
		return(print("No studbook name given!"))
	
	# connect to the studbook

	# determine the name of the computer

	CompName <-Sys.info()['nodename']

	# paste together the calls to the ODBC driver connection
	xx <- paste("driver={SQL Server};server=", CompName, "\\SQLEXPRESS;database=", DBname, ";trusted_connection=true", sep="")

	# then set up the connection
	dbhandle <- odbcDriverConnect(xx)

	if(UDF ==TRUE)
		DBtablenames<-c(DBtablenames, "UserDefinedField", "UserDefinedFieldValue")

	# grab all of the main tables, save them as named elements in a list

	output<-vector("list", length(DBtablenames))

	for(i in 1:length(DBtablenames))
		output[[i]]<-sqlQuery(dbhandle, paste("select * from ", DBtablenames[i], sep=""))	

	nn<-DBtablenames	# will be used to name the list elements

	# add in the overlay tables if wanted (by default they are, even if they don't exist!)

	if(Overlay==TRUE){

		for(i in 1:length(DBtablenames))
			output[[length(DBtablenames)+i]]<-sqlQuery(dbhandle, paste("select * from Overlay", DBtablenames[i], sep=""))

		output[[length(output)+1]]<-sqlQuery(dbhandle, "select * from OverlayInformation")
		nn<-c(nn, paste("Overlay", DBtablenames, sep=""), "OverlayInformation")

	}


	
	Details<-sqlQuery(dbhandle, "select * from Overview")
	output[[length(output)+1]]<-Details	

	names(output)<-c(nn, "DatabaseDetails")

	# print the details about the database if requested

	if(verbose==TRUE){
		print(paste(Details$CommonName, "studbook version", Details$Version, ", current to", Details$CurrentnessDate, "and kept by",
			Details$StudbookKeeper, "at", Details$StudbookKeeperInstitution))
	}


	# close the ODBC handle

	odbcCloseAll()

	# if Overlays were requested, examine them (using overlayExamine) and print the simple results
	
	if(Overlay==TRUE	& nrow(output$OverlayInformation)>0 & silent==FALSE	)
		print(overlayExamine(output)$OverlaySummary)

	# return the studbook 
	return(output)

}


#
#	Function for describing overlays...has options for printing the whole overlay or just the details about it


overlayExamine<-function(Studbook = NULL, verbose=FALSE, fullReturn=FALSE){

	if(length(Studbook)==0)
		return(print("No studbook given!"))

	# find which elements in the input are overlay components (based on the names)

	OLcomponents<-grep("Overlay", names(Studbook))

	# if there aren't any overlay components, say so and end the function

	if(length(OLcomponents)==0)
		return(print("There are no overlay components in the studbook."))

	# how many overlays exist?
	nOL<-nrow(Studbook$OverlayInformation)

	# if no overlays exist, say so and end function

	if(nOL == 0){
		if(verbose==TRUE)
			print("There are no overlays.")
		return()
	}	

	# if overlays exist, say how many

	if(verbose==TRUE)
		print(paste("Number of overlays in the studbook: ", nOL, sep=""))
	
	OLsummary<-Studbook$OverlayInformation
	OLsummary<-	OLsummary[,which(colnames(OLsummary) %in% c("Name","Description", "DateCreated", "DateEdited", "UserCreated", "UserEdited" ))]

	output<-vector("list", 1)
	output[[1]]<-OLsummary
	names(output)<-"OverlaySummary"

	if(fullReturn==TRUE){

		overlayUIDs<-(Studbook$OverlayInformation)$GeneratedGUID

		# create a list of lists for the overlays  (each overlay will be a list of its components, and then there will be a list of the overlay lists)

		OLlist<-vector("list", length=nOL)
		names(OLlist)<-OLsummary$Name

		for(i in 1:nOL){

			sub_OLlist<-vector("list", length=length(OLcomponents))
			names(sub_OLlist)<-names(Studbook)[OLcomponents]
			entries<-rep(0, length(sub_OLlist))	
		
			for(j in 1:length(sub_OLlist)){
				ff<-Studbook[[OLcomponents[j]]]
				ff<-ff[which(as.character(ff$GeneratedGUID)==as.character((Studbook$OverlayInformation)$GeneratedGUID[i])),]
				ff<-ff[,-which(colnames(ff) %in% c("UniqueID", "GeneratedGUID", "IndividualGUID"))]
				sub_OLlist[[j]]<-ff
				entries[j]<-nrow(ff)
			}

			tablesWith<-names(sub_OLlist)[which(entries!=0)]
			
			sub_OLlist2<-vector("list", length=length(tablesWith))
			
			for(j in 1:length(tablesWith)){
				sub_OLlist2[[j]]<-sub_OLlist[[which(names(sub_OLlist) %in% tablesWith[j])]]
			}

			names(sub_OLlist2)<-names(sub_OLlist)[which(names(sub_OLlist) %in% tablesWith)]
			output[[length(output)+1]]<-sub_OLlist2

		}

		names(output)[2:length(output)]<-as.character(OLsummary$Name)
	}
		
	return(output)

}





#################################
#
#	3. Applying Overlays to the Studbook
#
#	
#		If overlays are to be applied to create the analytical studbook
#
#		Functions involved: OverlayApply [subfunctions: overlayMasterApply, overlayEventApply, overlaySexApply]
#


overlayApply<-function(Studbook = NULL, OverlayToUse = NULL, verbose=TRUE, Remove=TRUE, AddIsHypothetical=TRUE){

	OLcomponents<-grep("Overlay", names(Studbook))
	REGcomponents<-(1:length(names(Studbook)))[-OLcomponents]

	output<-Studbook
	OLnames<-NULL


	# check the requested OverlayToUse against the overlays that exist

	OverlayDetails<-overlayExamine(Studbook = Studbook , fullReturn = FALSE)

	# trying to come up with a stop-gap to deal with "none"...  this will need to be improved on in the future, for sure

	if(class(OverlayDetails) != "character"){
		OLnames<-as.character(OverlayDetails$OverlaySummary$Name)
	}

	if(length(OverlayToUse)==0 & length(OLnames)>0){
			cat(cat("No overlay selected, yet overlay(s) exist(s): \n \t"), cat(OLnames, sep="\n \t"), cat("Which overlay would you like to apply? If no overlay is desired, enter the word None. \n"))
			OverlayToUse <-readline(" ")			
	}
	
	if(length(OLnames)==0 & length(OverlayToUse )==0)		
		OverlayToUse <-"None"

	OL<-which(OLnames==OverlayToUse)


	# if the user requested an overlay that doesn't exist, say so and end the function [in future, make this be so there can be a talk back and give them an option, rather than just error/end]

	if(length(OL)==0 & OverlayToUse!="None")
		return(print("Requested overlay does not exist."))




	# if the user wants to not apply any overlays, add IsHypothetical (with values =0) to all tables (if desired)

	if(OverlayToUse=="None"){
		if(verbose==TRUE)
			print("No overlay applied.")

		if(AddIsHypothetical==TRUE){
			for(i in 1:length(REGcomponents)){
				tt<-which(names(Studbook)==names(Studbook)[REGcomponents[i]])
				output[[i]]<-data.frame(Studbook[[tt]], IsHypothetical=rep(0, nrow(Studbook[[tt]])))
			}
		}
	}




	#
	#	now run the sub functions, depending on which tables are part of the database
	#		only written in the ones I want/need now.  should probably code up others at some point

	if(OverlayToUse !="None"){

		overlayUID_toApply<-(Studbook$OverlayInformation)$GeneratedGUID[OL]

		DBcomponents<-names(Studbook)[-OLcomponents]

		if(length(which(DBcomponents %in% "Master"))>0)
			output$Master<-overlayMasterApply(Studbook, overlayUID_toApply, AddIsHypothetical=AddIsHypothetical)
	
		if(length(which(DBcomponents %in% "Event"))>0)
			output$Event<-overlayEventApply(Studbook, overlayUID_toApply, AddIsHypothetical=AddIsHypothetical)

		if(length(which(DBcomponents %in% "Sex"))>0)
			output$Sex<-overlaySexApply(Studbook, overlayUID_toApply, AddIsHypothetical=AddIsHypothetical)

		if(verbose==TRUE)
			print(paste("Overlay [", OverlayToUse, "] applied.", sep=""))


	}


	# if wanted, remove the overlay components from the output

	if(Remove==TRUE){
		output2<-output
		output<-vector("list", length(REGcomponents))
	
		for(i in 1:length(REGcomponents))
			output[[i]]<-output2[[REGcomponents[i]]]

		names(output)<-names(output2)[REGcomponents]	
	}
	
	return(output)

}
		





#
#	SUBFUNCTIONS TO APPLY THE OVERLAYS
#
#		these functions fit into the overlayApply function and apply the overlay(s) specified to the relevant table.
#		I've only currently written functions for Master, Event, and Sex tables
#
#	the calls for each are the DB and the UIDs for the overlays, as determined via the "OverlayToUse" input in the "overlayApply" function
#
#		so it's nothing special that needs to get addressed via passed-through calls to a higher function
#
#		except for whether or not to add the IsHypothetical column 

#
#	function to apply the master overlay
#

overlayMasterApply<-function(Studbook=Studbook, overlayUID_toApply = NULL, AddIsHypothetical=TRUE){

	# pull out the regular table and overlay table

	reg<-Studbook$Master

	if(AddIsHypothetical==TRUE)
		reg<-data.frame(reg, IsHypothetical=rep(0, nrow(Studbook$Master)))
	
	ov<-Studbook$OverlayMaster

	# subset the overlay table to which ever overlay to apply

	ov<-ov[which(ov$GeneratedGUID %in% overlayUID_toApply),]

	ov$StudbookID<-as.character(ov$StudbookID)
	ov$Sire<-as.character(ov$Sire)
	ov$Dam<-as.character(ov$Dam)

	reg$StudbookID<-as.character(reg$StudbookID)
	reg$Sire<-as.character(reg$Sire)
	reg$Dam<-as.character(reg$Dam)


	for(i in 1:nrow(ov)){	# go through each individual in the overlay

		if(length(which(as.character(reg$StudbookID)==ov$StudbookID[i]))>0){

			xx<-reg[which(as.character(reg$StudbookID)==ov$StudbookID[i]),]		# the studbook data
			yy<-ov[which(ov$StudbookID==ov$StudbookID[i]),]					# the overlay data
			
			# in the case that there are two entries for the same individual in the overlay (or set of overlays), take the first one, and print a warning

			if(nrow(yy)>1){
				print(paste("Warning: multiple entries for individual ", ov$StudbookID[i], " in the Overlay Master table.  Using first entry as default.", sep=""))
				yy<-yy[1,]
			}

			if(is.na(yy$Sire)==F)
				xx$Sire<-yy$Sire
			if(is.na(yy$Dam)==F)
				xx$Dam<-yy$Dam
			if(is.na(yy$BirthDate)==F)
				xx$BirthDate<-yy$BirthDate
			if(is.na(yy$BDateEst)==F)
				xx$BDateEst<-yy$BDateEst
			if(is.na(yy$BirthType)==F)
				xx$BirthType<-yy$BirthType
			if(is.na(yy$IsHypothetical)==F & AddIsHypothetical==TRUE)
				xx$IsHypothetical<-yy$IsHypothetical
		
			reg[which(as.character(reg$StudbookID)==ov$StudbookID[i]),]<-xx
		}

	
		# if an individual isn't already in the studbook (they are most likely hypothetical), then add them

		if(length(which(as.character(reg$StudbookID)==ov$StudbookID[i]))==0){

			#	grab the first row in mast as a template, replace all data with NAs, then fill in stuff from the overlay

			xx<-reg[1,]
			xx[1,1:ncol(xx)]<-NA
			yy<-ov[which(ov$StudbookID==ov$StudbookID[i]),]
			
			xx$StudbookID<-yy$StudbookID		
			xx$Sire<-yy$Sire		
			xx$Dam<-yy$Dam
			xx$BirthDate<-yy$BirthDate	
			xx$BDateEst<-yy$BDateEst
			xx$BirthType<-yy$BirthType

			if(AddIsHypothetical==TRUE)
				xx$IsHypothetical<-yy$IsHypothetical

			reg<-rbind(reg, xx)
		}

	}

	return(reg)
}


#
#	function to apply the event overlay  
#

overlayEventApply<-function(Studbook=Studbook, overlayUID_toApply = NULL, AddIsHypothetical=TRUE){

	# pull out the regular table and overlay table

	reg<-Studbook$Event

	if(AddIsHypothetical==TRUE)
		reg<-data.frame(reg, IsHypothetical=rep(0, nrow(Studbook$Event)))

	ov<-Studbook$OverlayEvent

	# subset the overlay table to which ever overlay to apply
	ov<-ov[which(ov$GeneratedGUID %in% overlayUID_toApply),]

	ov$StudbookID<-as.character(ov$StudbookID)
	reg$StudbookID<-as.character(reg$StudbookID)

	# split the data into "new" and "related" records

	ovR<-ov[-which(is.na(ov$RelatedRecord)),]
	ovN<-ov[which(is.na(ov$RelatedRecord)),]

	# only edit entries if there are "related" records

	if(nrow(ovR)>0){

		for(i in 1:length(nrow(ovR))){
			#find the matching record
			spot<-which(as.character(reg$GeneratedGUID)==as.character(ovR$RelatedRecord[i]))

			# if it doesn't exist, bounce that error!
			if(length(spot)==0)
				return(print("Event table overlay references record not in main table."))

			xx<-reg[spot,]
			yy<-ovR[i,]
	
			# if it's the wrong individual or event, bounce that error

			if(length(na.omit(yy$StudbookID))>0){
				if(as.character(xx$StudbookID) != as.character(yy$StudbookID))
					return(print("Event table overlay references the wrong Studbook ID."))
			}

			if(length(na.omit(yy$TranCode))>0){
				if(as.character(xx$TranCode) != as.character(yy$TranCode))
					return(print("Event table overlay references the wrong event type."))
			}

			if(is.na(yy$Location)==F)
				xx$Location<-yy$Location
			if(is.na(yy$TranDate)==F)
				xx$TranDate<-yy$TranDate
			if(is.na(yy$TDateEst)==F)
				xx$TDateEst<-yy$TDateEst
			
			reg[spot,]<-xx
		}
	}


	# only add the new entries if there are new entries to add!

	if(nrow(ovN)>0){

		# grab the first row in reg as a template, replace all data with NAs, then fill in stuff from the overlay

		xx<-reg[1,]
		xx[1,1:ncol(xx)]<-NA

		for(i in 1:nrow(ov)){	# go through each event in the overlay

			yy<-ov[i,]
			xx$StudbookID<-yy$StudbookID		
			xx$TranCode<-yy$TranCode		
			xx$Location<-yy$Location
			xx$TranDate<-yy$TranDate	
			xx$TDateEst<-yy$TDateEst

			if(AddIsHypothetical==TRUE)
				xx$IsHypothetical<-yy$IsHypothetical

			reg<-rbind(reg, xx)
		}

	}

	return(reg)
}



#
#	Function to apply the sex overlay
#

overlaySexApply<-function(Studbook=Studbook, overlayUID_toApply = NULL, AddIsHypothetical=TRUE){

	# pull out the regular table and overlay table

	reg<-Studbook$Sex

	if(AddIsHypothetical==TRUE)
		reg<-data.frame(reg, IsHypothetical=rep(0, nrow(Studbook$Sex)))

	ov<-Studbook$OverlaySex

	# subset the overlay table to which ever overlay to apply
	ov<-ov[which(ov$GeneratedGUID %in% overlayUID_toApply),]

	ov$StudbookID<-as.character(ov$StudbookID)
	reg$StudbookID<-as.character(reg$StudbookID)
	
	# split the data into "new" and "related" records

	ovR<-ov[-which(is.na(ov$RelatedRecord)),]
	ovN<-ov[which(is.na(ov$RelatedRecord)),]

	# only edit entries if there are "related" records

	if(nrow(ovR)>0){

		#find the matching record
		spot<-which(as.character(reg$GeneratedGUID)==as.character(ovR$RelatedRecord[i]))

		# if it doesn't exist, bounce that error!
		if(length(spot)==0)
			return(print("Event table overlay references record not in main table."))

		xx<-reg[spot,]
		yy<-ovR[i,]
	
		# if it's the wrong individual bounce that error

		if(as.character(xx$StudbookID) != as.character(yy$StudbookID))
			return(print("Event table overlay references the wrong Studbook ID."))

		if(is.na(yy$Sex)==F)
			xx$Sex<-yy$Sex
		if(is.na(yy$EventDate)==F)
			xx$EventDate<-yy$EventDate
		if(is.na(yy$EDateEst)==F)
			xx$EDateEst<-yy$EDateEst
			
		reg[spot,]<-xx

	}

	# only add the new entries if there are new entries to add!

	if(nrow(ovN)>0){


		# grab the first row in reg as a template, replace all data with NAs, then fill in stuff from the overlay

		xx<-reg[1,]
		xx[1,1:ncol(xx)]<-NA

		for(i in 1:nrow(ov)){	# go through each event in the overlay

			yy<-ov[i,]
			xx$StudbookID<-yy$StudbookID		
			xx$Sex<-yy$Sex		
			xx$EventDate<-yy$EventDate	
			xx$EDateEst<-yy$EDateEst

			if(AddIsHypothetical==TRUE)
				xx$IsHypothetical<-yy$IsHypothetical

			reg<-rbind(reg, xx)
		}

	}
	return(reg)
}






#################################
#
#	4. Read in, ammend, and apply institutional windows
#
#	
#		Functions needed for applying the institutional window to any set of data
#
#		Functions involved: institutionList, inOutInstitution
#
# 		institutionWindow reads in the FED file(s) of interest and makes any ammendments based on user definitions (adding or removing specific institutions)
#		inOutInstitution applies the institutional window to a vector of locations
#
#			possible idea to combine these or make iW a sub function in iOI


institutionWindow<-function(institutions = NULL, ADDinstitutions = NULL, DROPinstitutions = NULL){

	if(length(institutions)==0)
		return(print("No FED file given."))		# in the future, will be more refined here (should allow, for example, "None" or something?)

	insts<-NULL
	for(i in 1:length(institutions)){
		insts<-c(insts, scan(file=paste(gsub("[\\]", "/", Sys.getenv("HOME")), "/PopLink/Federation Files/", institutions[i], ".FED" , sep=""), what="character", sep="\n", quiet=TRUE))
	}
	

	if(length(ADDinstitutions)>0)
		insts<-c(insts, ADDinstitutions)

	if(length(DROPinstitutions)>0)
		insts<-insts[-which( insts %in% DROPinstitutions)]

	insts<-sort(unique(insts))

	return(insts)

}



inOutInstitution<-function(Locations=NULL, locationWindow= NULL){

	if(length(locationWindow)==0)
		return(NA)		

	InOut<-Locations %in% locationWindow
	
	return(InOut)
}




#################################
#
#	5. Construct and apply date windows
#	
#		Functions needed for constructing and applying the date window to any set of dates
#
#		Functions involved: institutionList, inOutInstitution
#
#		including a "buffer" on each end of the window, which allows for things like "hasn't yet lived for a full year"
#		currently only allowing the buffers to be in days, not, for example, years (which is a little more to deal with, but not hard...just need to do it)


dateWindow<-function(startingDate=NULL, endingDate=NULL, startingBuffer=0, endingBuffer=0, bufferUnits="days"){

	if(length(endingDate)==0){
		print(paste("No ending date input.  Today's date (", Sys.Date() ,") is being used.", sep=""))
		endingDate<-Sys.Date()
	}

	endingDate<-as.Date(endingDate)

	if(length(startingDate)==0){
		print("No starting date input.  1980-01-01 is being used.")
		startingDate <-"1980-01-01"
	}

	startingDate <-as.Date(startingDate)

	if(bufferUnits!="days")
		return(print("Joe hasn't coded up non-day units for the buffer yet!"))

	output<-c(startingDate+startingBuffer, endingDate-endingBuffer)

}



inOutDate<-function(Dates=NULL, timeWindow= NULL){

	if(length(timeWindow)==0)
		return(print("No date window defined!"))		

	InOut<-as.Date(Dates) >= timeWindow[1] & as.Date(Dates) <= timeWindow[2]
	
	return(InOut)
}



#################################
#
#	6. Examine/apply UDFs
#
#		Print out UDFs with their levels, create a vector applying them
#	
#		Functions involved: UDFexamine, UDFapply
#
#		UDFapply can either print out a vector of which SBIDs match that UDF or match it to an inputted vector of IDs and return a TF vector


UDFexamine<-function(Studbook=NULL){

	UDFs<-as.character((Studbook$UserDefinedField)$FieldName)

	if(length(which(UDFs!="Studbook ID"))==0)
		return(print("There are no UDF data in the Studbook you gave the function."))

	UDFs<-UDFs[which(UDFs %in% "Studbook ID" ==FALSE)]


	for(i in 1:length(UDFs)){
		
		spot<-which(as.character((Studbook$UserDefinedField)$FieldName)==UDFs[i])
		levs<-as.character(unique((Studbook$UserDefinedFieldValue)$Value[(Studbook$UserDefinedFieldValue)$FieldUniqueID==spot]))
	
		print(paste( "UDF: [", UDFs[i], "] with levels: [", paste0(levs, collapse="], ["), "]", sep=""))
	}
	
}


	#	retain allows you to either retain or drop the ones that match Levels

UDFapply<-function(Studbook = NULL, FieldName = NULL, Levels = NULL, retain=TRUE, SBIDmatch=NULL){

	if(length(which(names(Studbook)=="UserDefinedField"))==0)
		return(print("There is not a UDF explanation in the Studbook you gave the function."))

	if(length(which(names(Studbook)=="UserDefinedFieldValue"))==0)
		return(print("There are not UDF data in the Studbook you gave the function."))

	UDFs<-as.character((Studbook$UserDefinedField)$FieldName)
	whichFields<-which(UDFs %in% FieldName ==TRUE)

	if(length(whichFields)==0)
		return(print("That UDF does not exist."))

	temp1<-Studbook$UserDefinedFieldValue[which((Studbook$UserDefinedFieldValue)$FieldUniqueID ==whichFields),]

	indivs<-temp1$StudbookID[which(temp1$Value %in% Levels == retain)]
	
	if(length(SBIDmatch)==0)
		return(indivs)

	InOut<- SBIDmatch %in% indivs
	return(InOut)
	
}











#################################
#
#	7. Determine the pedigree and genetic metrics
#
#	
#		Map out the pedigree and calculate inbreeding
#
#		Functions involved: SBpedig, SBinbreeding, inbreedingHistory
#
#			SBpedig uses any hypotheticals to calculate relatedness, 
#			Now currently 
#			currently replacing all MULTs, UNKs, and WILDs that weren't altered by the overlay with NAs
#				verbose tells you who got replaced with NAs (probably want ability to write that out to a file, too, and perhaps more detailed output)
#
#		this is also where i'll want to include measures of GD and MK and all that stuff.  haven't included them yet.

SBpedig<-function(Studbook = NULL, verbose=FALSE, silent=FALSE){

	if(length(Studbook)==0)
		return(print("No studbook given!"))

	ind<-(Studbook$Master)$StudbookID
	dam<-(Studbook$Master)$Dam
	sire<-(Studbook$Master)$Sire

	# if a dam or sire isn't listed as an individual in the studbook (with an ID), it gets replaced with NA
	#	this ends up replacing MULTs UNKs and WILDs that weren't altered via the overlay, also

	dam2<-dam
	sire2<-sire
	whichdropD<-NULL
	whichdropS<-NULL

	for(i in 1:length(ind)){

		if(length(ind[which(ind %in% dam[i])])==0){
			dam2[i]<-NA
			whichdropD<-c(whichdropD, i)
		}
		if(length(ind[which(ind %in% sire[i])])==0){
			sire2[i]<-NA
			whichdropS<-c(whichdropS, i)
		}

	}

	DamReplace<-data.frame(StudbookID=ind[whichdropD], Dam=dam[whichdropD], ReplacedWith=rep("NA", length(whichdropD)))
	SireReplace<-data.frame(StudbookID=ind[whichdropS], Sire=sire[whichdropS], ReplacedWith=rep("NA", length(whichdropS)))

	if(verbose==TRUE){
		print(DamReplace)
		print(SireReplace)
	}

	if(silent==FALSE){
		print(paste(length(whichdropD), " unresolved dams and ", length(whichdropS), " unresolved sires replaced with NAs (out of ", length(ind), " total entries)", sep=""))
	
	}	

	pedig<-as.data.frame(cbind(Indiv=as.character(ind), Dam=as.character(dam2), Sire=as.character(sire2)))
	Ped<-pedig[,c(1,2,3)]
	for(i in 1:3)
		Ped[,i]<-as.factor(pedig[,i])	

	ord<-orderPed(Ped)
	Ped2<-Ped[order(ord),]

	return(Ped2)
}


SBinbreeding<-function(Studbook = NULL, verbose=FALSE, silent=FALSE){

	pedigree<-SBpedig(Studbook = Studbook , verbose=verbose, silent=silent)

	ind<-(Studbook$Master)$StudbookID

	hypos<-NULL
	if(length(which(names(Studbook$Master)=="IsHypothetical"))==1)
		hypos<-(Studbook$Master)$IsHypothetical
	

	F1<-round(calcInbreeding(pedigree),4)

	# order F1 to match ind
	F1x<-rep(NA, length(F1))
	F1x<-F1[match(ind, pedigree$Indiv)]

	# then cut out the hypotheticals from the Inbreeding Table

	if(length(hypos)>0){
		ind<-ind[which(hypos==0)]
		F1x<-F1x[which(hypos==0)]
	}	

	output<-data.frame(StudbookID= ind, Fval=F1x)
	return(output)

}












##################################################################
#
#	OTHER FUNCTIONS THAT need to be categorized, etc.










#################################
#
#	Restrict the event table 
#
#		currently based on event type, institution window, date window, udf, and hypothetical (remove them) if wanted
#			will eventually want to generalize this function and make it more primitive, but it's good for now



specificEvents<-function(Studbook = NULL, eventType = NULL, startingDate=NULL, endingDate=NULL, institutions = NULL, UDF_FieldName = NULL, UDF_Levels = NULL, includeHypotheticals = FALSE, 
		ADDinstitutions =NULL, DROPinstitutions = NULL, ...){

	# set up the windows

		# institution
		iW<-institutionWindow(institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions)	

		# date
		dW<-dateWindow(startingDate = startingDate, endingDate = endingDate)

		# UDFs
		if(length(UDF_FieldName)>0)
			uW<-UDFapply(Studbook = Studbook, FieldName = UDF_FieldName, Levels = UDF_Levels)


	# restrict the event table 
	
		specEvent<-Studbook$Event	

		# based on Event Type
		specEvent<-specEvent[which(specEvent$TranCode %in% eventType),]

		# based on location
		specEvent<-specEvent[which(specEvent$Location %in% iW),]

		# based on date
		specEvent<-specEvent[which(as.Date(specEvent$TranDate) >= dW[1] & as.Date(specEvent$TranDate) <= dW[2]),]

		# based on UDFs
		if(length(UDF_FieldName)>0)
			specEvent<-specEvent[which(specEvent$StudbookID %in% uW),]

		# based on Hypotheticals
		if(includeHypotheticals == FALSE & length(which(names(specEvent)=="IsHypothetical"))>0)
			specEvent<-specEvent[which(specEvent$IsHypothetical==0),]

	return(specEvent)
}




#
#	this function determines if an individual's events within a time window are all within an institution window 
#
#		can totally make this better int he future


stayIn<-function(Studbook=NULL, StudbookID=NULL, institutions=NULL, startDate=NULL, timeWindow=NULL,...){

	# find all events the individual was at for the first X days of their life
	events<-Studbook$Event[which((Studbook$Event)$StudbookID==StudbookID &  as.Date((Studbook$Event)$TranDate) < as.Date(startDate)+timeWindow ),]
	
	# grab their locations
	locs<-events$Location

	locsIn<-inOutInstitution(Locations=locs, locationWindow= institutionWindow(institutions))
	output<-length(locsIn[locsIn==FALSE])
	output[output>0]<-"FALSE"
	output[output==0]<-"TRUE"

	# if there were any LTFs within the window, it's automatically turned to FALSE

	if(length(which(events$TranCode=="Go LTF"))>0)
		output<-"FALSE"
	return(output)	
}




#
#	summarize inbreeding over time
#
#


summarizeFoverTime<-function(Ftable=NULL, startingDate=NULL, endingDate=NULL, ...){
	
		yrs<-seq(as.numeric(format(as.Date(startingDate), "%Y")), as.numeric(format(as.Date(endingDate), "%Y")), 1)
		
		avF<-rep(NA, length(yrs))
		sdF<-rep(NA, length(yrs))
		minF<-rep(NA, length(yrs))
		maxF<-rep(NA, length(yrs))
		N<-rep(NA, length(yrs))

		for(i in 1:length(yrs)){

			xx<-Ftable$Fval[Ftable$Year==yrs[i]]
			N[i]<-length(xx)

			if(length(xx) > 0){
				avF[i]<-round(mean(xx),4)
				minF[i]<-min(xx)
				maxF[i]<-max(xx)
			}
			if(length(xx) > 1){
					sdF[i]<-round(sd(xx),4)
			}
		}

		output<-data.frame(Year=yrs, Total=N,AverageF=avF, StDevF=sdF, MinF=minF, MaxF=maxF)
		return(output)

}



#
#	graph inbreeding over time
#
#


graphFoverTime<-function(Studbook=NULL, FsummaryTable=NULL, GraphResolution=200, type=NULL, ...){
	
		if(type=="BH"){
			lab<-as.character(unique(Studbook$Event$TranCode)[which(unique(Studbook$Event$TranCode) %in% c("Birth", "Hatch"))])
			lab[lab=="Birth"]<-"Births"
			lab[lab=="Hatch"]<-"Hatches"
	
			lab2<-"# Births "
		}

		if(type=="Pop"){
			lab<-"Population"
			lab2<-"Pop Size "
		}

		gname<-paste(paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "Historical_Inbreeding_", sep="_"), lab, ".tif", sep="")

		tiff(file = gname, width=15, height = 8, units = "in", res=GraphResolution)
		par(mar=c(4,7,1.5,1.5))

		plot(1~1, ylim=c(-.025,max(na.omit(MaxF))+.075), xlim=c(Year[1], Year[length(Year)]), ylab="", xlab="", yaxt='n', xaxt='n', type='n', data=FsummaryTable)
		points(FsummaryTable$AverageF~Year, pch=16, cex=1.5, data=FsummaryTable)

		for(i in 1:length(FsummaryTable$Year)){
			points(x=rep(FsummaryTable$Year[i],2), y= c(max(0, FsummaryTable$AverageF[i]-FsummaryTable$StDevF[i]), FsummaryTable$AverageF[i]+FsummaryTable$StDevF[i]), type='l', lwd=3)
			points(x=rep(FsummaryTable$Year[i],2), y=c( FsummaryTable$MinF[i], FsummaryTable$MaxF[i]), type='l', lwd=1)
		}

		axis(1, cex.axis=1.25)
		axis(1, at=FsummaryTable$Year[1]:FsummaryTable$Year[length(FsummaryTable$Year)], labels=F, tck=-.0075)
		axis(2, at=seq(0,max(na.omit(FsummaryTable$MaxF))+.1,.05 ), las=1, cex.axis=1.25)

		mtext(side=2, paste("Inbreeding Coefficient of ", lab, sep=""), line=4.75, cex=1.75)
		mtext(side=2, at=-.03, lab2, las=1, cex=1.125)
		text(x=FsummaryTable$Year, y=rep(-0.03, length(FsummaryTable$Year)), FsummaryTable$Total, cex=.9)

		dev.off()
}



#
#	Determine the population that exists on each census date
#
#
# this function is pretty darn slow
#  it needs work to speed up the whole "check who's in the population on day X" thing
#
#	indeed, this whole process could be improved a ton, but this works for now


censusPopulation<-function(Studbook=NULL,startingDate=startingDate, endingDate=endingDate, institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, 
					UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels, censusDate="01-01", ...){



	# institution Window
		iW<-institutionWindow(institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions)	

	# apply to the events table
		EventsIO<- Studbook$Event$Location %in% iW
		Events<-data.frame(Studbook$Event, IO=EventsIO)
	
	#	
	#	need to include UDF filtration
	#

	# use the census date and the beginning and end of the time window to determine when the censuses should be happening

		Censuses<-paste(seq(as.numeric(format(as.Date(startingDate), "%Y")), as.numeric(format(as.Date(endingDate), "%Y")),1), censusDate, sep="-")
		Censuses<-Censuses[which(Censuses>=startingDate & Censuses<=endingDate)]	

	# for each individual in the studbook, determine if they were in the population on the census date each year

		yy<-NULL
		ii<-NULL

		for(i in 1:length(unique(Events$StudbookID))){
			specEvents<-Events[which(Events$StudbookID==unique(Events$StudbookID)[i]),]
			specEvents<-specEvents[order(specEvents$TranDate),]	

			InOutEachCensus<-rep(NA, length(Censuses))

			for(j in 1:length(Censuses)){
			
				# find the individual's most recent event prior to the census date, check to make sure it's not a death or go ltf, and that it's in the institution window
				beforeCensus<-specEvents[which(as.Date(specEvents$TranDate)<=Censuses[j]),]
				mostRecent<-beforeCensus[nrow(beforeCensus),]
				InOutEachCensus[j]<-length(which(mostRecent$IO==TRUE & mostRecent$TranCode!= "Death" & mostRecent$TranCode!= "Go LTF"))

			}
			yy<-c(yy, format(as.Date(Censuses[which(InOutEachCensus==1)]), "%Y"))
			ii<-c(ii, rep(unique(Events$StudbookID)[i], length(which(InOutEachCensus==1))))
		}

		return(data.frame(Year=yy, StudbookID=ii))
}





#
#
#	measure individuals' life spans
#
#



lifespan<-function(Studbook=NULL, StudbookID=NULL, endingDate=NULL, BH=NULL...){

	# find all events the individual was at for the first X days of their life
	events<-Studbook$Event[which((Studbook$Event)$StudbookID==StudbookID ),]

	Status<-"Alive"
	Span<-as.numeric(as.Date(endingDate) - as.Date(events$TranDate[which(events$TranCode==BH)]))

	if(events$TranCode[length(events$TranCode)]=="Go LTF"){
		Status<-"LTF"
		Span<-as.numeric(as.Date(events$TranDate[length(events$TranCode)]) - as.Date(events$TranDate[which(events$TranCode==BH)]))
	}
	if(events$TranCode[length(events$TranCode)]=="Death"){
		Status<-"Dead"	
		Span<-as.numeric(as.Date(events$TranDate[length(events$TranCode)]) - as.Date(events$TranDate[which(events$TranCode==BH)]))
	}
	
	return(c(Status,Span))
}




#
#
#	apply the mort window
#

mortWindow<-function(LifeSpan = NULL, Status = NULL, mortalityWindow=0,...){

		output<-NA

		if(LifeSpan>=mortalityWindow)
			output<-0

		if(LifeSpan < mortalityWindow & Status == "Dead")
			output<-1

		return(output)
}









#
#	INBREEDING INFANT MORTALITY ANALYSIS
#
#
#	this function needs a lot of work, but it's good enough for now
#
#		some things:	
#			subfunction to check if there's enough data
#			improve model selection and summarizing...also figure out how to deal with non-sig, but retained terms (like with A oryx)
#			make output a word doc!
#			break up analyses and coding into subfunctions?



infMortInbreedingAnalysis<-function(IMtable = NULL, Studbook = NULL, SpeciesName=NULL, modelOut=TRUE, Graphs=TRUE){


	# for the models with sex included, we drop the unknown and other sex animals
	IMtable2 <-IMtable[-which(IMtable$Sex=="Unknown" | IMtable$Sex=="Other"),]
	
	#
	#	build in catches here for insufficient data... like what to do if there aren't enough mortalities or enough inbreeding
	#		note that 'not enough mortalities' may only apply to the aTss dataset [in which case you can analyze the F effect but not the sex effect], or it could be both
	#	

	

	pSex<-NA
	pFval<-NA

	#	see if sex is significant
	#

	modSex<-glm(InfMort~Sex, data=IMtable2, family='binomial')
	modSex<-stepAIC(modSex, trace=0)

	
	#	if Sex is significant, keep it in the model and proceed with this data set.  if not, drop it and go to the other data set

	if(length(grep("Sex", rownames(coef(summary(modSex))), ignore.case=TRUE ))>0	){
		
		modFull<-glm(InfMort~Sex*Fval, data=IMtable2, family='binomial')
		modFinal<-stepAIC(modFull, trace=0)

		# in the case that sex ended up getting dropped after Fval was added
	
		if(length(grep("Sex", rownames(coef(summary(modFinal))), ignore.case=TRUE))==0){

			pSex<-add1(modFinal, ~.+Sex , test="Chisq")$"Pr(>Chi)"[2]
			modFull<-glm(InfMort~Fval, data=IMtable, family='binomial')
			modFinal<-stepAIC(modFull, trace=0)
		}
	}

	if(length(grep("Sex", rownames(coef(summary(modSex))), ignore.case=TRUE))==0	){
		
		pSex<-anova(glm(InfMort~Sex, data=IMtable2, family='binomial'), glm(InfMort~1, data=IMtable2, family='binomial'), test="Chisq")$"Pr(>Chi)"[2]
		modFull<-glm(InfMort~Fval, data=IMtable, family='binomial')
		modFinal<-stepAIC(modFull, trace=0)
	}


	# if inbreeding isn't in the final model, get it's p val

	if(length(grep("Fval", rownames(coef(summary(modFinal))), ignore.case=TRUE ))==0	){
		pFval<-add1(modFinal, ~.+Fval , test="Chisq")$"Pr(>Chi)"[2]
	}

	#
	# summarize the model fit, ammending the NS terms
	#

	s1<-summary(modFinal)$coefficients
	s2<-matrix(NA, nrow=nrow(s1), ncol=ncol(s1)+1)
	s2[,2:ncol(s2)]<-round(s1[,],4)
	s2[,1]<-rownames(s1)
	colnames(s2)[2:ncol(s2)]<-colnames(s1)
	colnames(s2)[1]<-"Parameter"

	if(length(grep("Sex", rownames(s1), ignore.case=TRUE ))==0){
		s2<-rbind(s2, rep(NA, ncol(s2)))
		s2[nrow(s2), which(colnames(s2)=="Parameter")]<-"Sex"
		s2[nrow(s2), which(colnames(s2)=="Pr(>|z|)")]<-round(pSex,4)
	}

	if(length(grep("Fval", rownames(s1), ignore.case=TRUE ))==0){
		s2<-rbind(s2, rep(NA, ncol(s2)))
		s2[nrow(s2), which(colnames(s2)=="Parameter")]<-"Fval"
		s2[nrow(s2), which(colnames(s2)=="Pr(>|z|)")]<-round(pFval,4)
	}

	s2[which(s2[,which(colnames(s2)=="Pr(>|z|)")]=="0"),which(colnames(s2)=="Pr(>|z|)")] <- "<0.0001"


	OddsRatio<-rep(NA, nrow(s2))
	whichOR<-which(s2[,which(colnames(s2)=="Parameter")]!="(Intercept)" & is.na(s2[,which(colnames(s2)=="Estimate")])==FALSE)

	if(length(whichOR)>0)
		OddsRatio[whichOR]<-round(exp(as.numeric(s2[whichOR,which(colnames(s2)=="Estimate")])),4)

	s2<-cbind(s2, OddsRatio)
	s2[,which(colnames(s2)=="Parameter")]<-gsub("SexMale", "Sex", s2[,which(colnames(s2)=="Parameter")])

	# write it out if desired

	if(modelOut==TRUE){

		tname<-paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "Infant_Mortality_Inbreeding_Analysis_Stats_Table.txt", sep="_")
		write.table(s2, tname, row.names=F, sep="\t")
	}

	# model summary

	Terms<-names(modFinal$coefficients)
	Terms<-Terms[-which(Terms=="(Intercept)")]
	Terms<-gsub("SexMale", "Sex", Terms)
		
	xpred<-seq(0, max(IMtable$Fval), length.out=100)

	# if just intercept

	if(length(Terms)==0){	
		modSum<-c(	"All individuals included in analysis (including Unknown/Other sexes)",
				paste("N = ", nrow(IMtable), sep=""),
				paste("No terms retained in model (Inbreeding p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="Pr(>|z|)")], 
						", Sex p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="Pr(>|z|)")], ")", sep=""),
				paste("Fitted infant mortality rate = ", round(predict.glm(modFinal, type='response')[1],3)*100, "%", sep=""))
	}

	# if just Fval

	if(length(Terms)==1 & Terms[1]=="Fval"){
		modSum<-c(	"All individuals included in analysis (including Unknown/Other sexes)",
				paste("N = ", nrow(IMtable), sep=""),
				paste("Inbreeding is retained (p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="Pr(>|z|)")], ", odds-ratio = ",
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="OddsRatio")],
						"), Sex is not retained (p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="Pr(>|z|)")], ")", sep=""),
				paste("Fitted infant mortality rate = ",  round(predict.glm(modFinal, newdata=list(Fval=0), type='response')[1],3)*100, "% when F = 0.0 and ",  
					round(predict.glm(modFinal, newdata=list(Fval=max(IMtable$Fval)), type='response')[1],3)*100, "% when F = ", max(IMtable$Fval), sep=""))
	}		

	# if just Sex
		
	if(length(Terms)==1 & Terms[1]=="Sex"){		
		modSum<-c(	"Only males and females included in analysis (excluding Unknown/Other sexes)",
				paste("N = ", nrow(IMtable2), sep=""),
				paste("Inbreeding is not retained (p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="Pr(>|z|)")], "), Sex is retained (p = ", 
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="Pr(>|z|)")], 
					", odds-ratio (Males) = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="OddsRatio")]	, ")", sep=""),
				paste("Fitted infant mortality rate = ", round(predict.glm(modFinal, newdata=list(Sex="Male"), type='response'),3)*100, "% for Males and ", 
					round(predict.glm(modFinal, newdata=list(Sex="Female"), type='response'),3)*100, "% for Females", sep=""))
	}	

	# if both Sex and Fval (no interaction)

	if(length(Terms)==2){		
		modSum<-c(	"Only males and females included in analysis (excluding Unknown/Other sexes)",
				paste("N = ", nrow(IMtable2), sep=""),
				paste("Inbreeding is retained (p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="Pr(>|z|)")], ", odds-ratio = ",
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="OddsRatio")],
					"), Sex is retained (p = ", 
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="Pr(>|z|)")], 
					", odds-ratio (Males) = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="OddsRatio")]	, ")", sep=""),
				paste("Fitted Male infant mortality rate = ", round(predict.glm(modFinal, newdata=list(Sex="Male", Fval=0), type='response'),3)*100, "% when F = 0.0 and ", 
					round(predict.glm(modFinal, newdata=list(Sex="Male", Fval=max(IMtable$Fval)), type='response'),3)*100, "% when F = ", max(IMtable$Fval),
					".  Fitted Female infant mortality rate = ", round(predict.glm(modFinal, newdata=list(Sex="Female", Fval=0), type='response'),3)*100, "% when F = 0.0 and ", 
					round(predict.glm(modFinal, newdata=list(Sex="Female", Fval=max(IMtable$Fval)), type='response'),3)*100, "% when F = ", max(IMtable$Fval),sep=""))
	}

	# if Sex and Fval and interaction

	if(length(Terms)==3){		
		modSum<-c(	"Only males and females included in analysis (excluding Unknown/Other sexes)",
				paste("N = ", nrow(IMtable2), sep=""),
				paste("Inbreeding is retained (p = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="Pr(>|z|)")], ", odds-ratio = ",
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Fval"),which(colnames(s2)=="OddsRatio")],
					"), Sex is retained (p = ", 
					s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="Pr(>|z|)")], 
					", odds-ratio (Males) = ", s2[which(s2[,which(colnames(s2)=="Parameter")]=="Sex"),which(colnames(s2)=="OddsRatio")]	, ") and there is an interaction (p = ", 
					s2[grep(":", s2[,which(colnames(s2)=="Parameter")]),which(colnames(s2)=="Pr(>|z|)")],
					", odds-ratio (Males) = ", s2[grep(":", s2[,which(colnames(s2)=="Parameter")]),which(colnames(s2)=="OddsRatio")],")",sep=""),
				paste("Fitted Male infant mortality rate = ", round(predict.glm(modFinal, newdata=list(Sex="Male", Fval=0), type='response'),3)*100, "% when F = 0.0 and ", 
					round(predict.glm(modFinal, newdata=list(Sex="Male", Fval=max(IMtable$Fval)), type='response'),3)*100, "% when F = ", max(IMtable$Fval),
					".  Fitted Female infant mortality rate = ", round(predict.glm(modFinal, newdata=list(Sex="Female", Fval=0), type='response'),3)*100, "% when F = 0.0 and ", 
					round(predict.glm(modFinal, newdata=list(Sex="Female", Fval=max(IMtable$Fval)), type='response'),3)*100, "% when F = ", max(IMtable$Fval),sep=""))
	}

	# write it out if desired

	if(modelOut==TRUE){

		tname<-paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "Infant_Mortality_Inbreeding_Analysis_Summary.txt", sep="_")
		write.table(modSum, tname, row.names=F, sep="\t")
	}




	#
	#	graphing! (if desired)

	if(Graphs==TRUE){

		gname<-paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "Infant_Mortality_Inbreeding_Analysis_Graph.tif", sep="_")


		# points!
		
		pnts<-rep(1, nrow(IMtable))	# all the same points

		# unless sex is significant

		if(length(grep("Sex", rownames(coef(summary(modFinal))), ignore.case=TRUE ))>0){
		
			pnts[which(IMtable$Sex=="Female")]<-2
			pnts[which(IMtable$Sex!="Female" & IMtable$Sex!="Male")]<-5
		}

		# the locations of the points to be graphed (Fvals and InfMorts, plus some jittering noise)

		yvs<-rnorm(n=length(IMtable$InfMort), mean=IMtable$InfMort, sd=.04)
		xvs<-rnorm(n=length(IMtable$Fval), mean=IMtable$Fval, sd=.003)

		tiff(file = gname, width=14, height = 8, units = "in", res=200)
		par(mar=c(6,7,1,6.5))

		plot(1~1, ylim=c(min(yvs)-.01,max(yvs)+.01), xlim=c(min(xvs)-.01,max(xvs)+.01), ylab="", xlab="", yaxt='n', xaxt='n', type='n')

		points(xvs, yvs, lwd=2, cex=1.5, pch=pnts)


		# model predicted line!  which totally depends on which terms are in the final model!

		Terms<-names(modFinal$coefficients)
		Terms<-Terms[-which(Terms=="(Intercept)")]
		
		xpred<-seq(0, max(IMtable$Fval), length.out=100)

		# if just intercept

		if(length(Terms)==0){	
			ypred<-rep(predict.glm(modFinal, type='response')[1], length(xpred))
			points(xpred, ypred, type='l', lwd=3, lty=1)
			textOut<-"All sexes are circles and fitted with one line."
		}

		# if just Fval

		if(length(Terms)==1 & Terms[1]=="Fval"){
			ypred<-predict.glm(modFinal, newdata=list(Fval=xpred),type='response')
			points(xpred, ypred, type='l', lwd=3, lty=1)
			textOut<-"All sexes are circles and fitted with one line."
		}		

		# if just Sex
		
		if(length(Terms)==1 & length(grep("Sex",Terms[1], ignore.case=TRUE))==1){		# that sex match allows for the SexMale (or whatever it wants to write it as)
			ypredM<-rep(predict.glm(modFinal, newdata=list(Sex="Male"),type='response'), length(xpred))
			ypredF<-rep(predict.glm(modFinal, newdata=list(Sex="Female"),type='response'), length(xpred))
			points(xpred, ypredM, type='l', lwd=3, lty=2)
			points(xpred, ypredF, type='l', lwd=3, lty=3)
			textOut<-"Males are circles and dashed line, females are triangles and dotted line, unknowns/others are diamonds and no line."
		}	

		# if both Sex and Fval (regardless of interaction, that gets done auto)

		if(length(Terms)>1){		
			ypredM<-rep(predict.glm(modFinal, newdata=list(Sex=rep("Male", length(xpred)), Fval=xpred),type='response'))
			ypredF<-rep(predict.glm(modFinal, newdata=list(Sex=rep("Female", length(xpred)), Fval=xpred),type='response'))
			points(xpred, ypredM, type='l', lwd=3, lty=2)
			points(xpred, ypredF, type='l', lwd=3, lty=3)
			textOut<-"Males are circles and dashed line, females are triangles and dotted line, unknowns/others are diamonds and no line."
		}	

		axis(1, cex=1.25, at=seq(0, max(IMtable$Fval),.05),cex.axis=1.25)
		mtext(side=1, "Inbreeding Coefficient", line=3.75, cex=1.75)

		axis(2, at=c(0,1), labels=c("Survived", "Died"), las=1, cex.axis=1.25)
		mtext(side=2, "Infant Mortality (points, jittered)", line=5, cex=1.75)
		
		axis(4, at=seq(0,1,.2), las=1, cex.axis=1.25)
		mtext(side=4, "Probability of Infant Mortality (line)", line=4, cex=1.75)
	
		dev.off()
		write.table(data.frame(Legend=textOut), "InfMortInbreedingAnalysisFigureLegend.txt", row.names=F, sep="\t")

	}

	output<-vector("list", 2)
	output[[1]]<-s2
	output[[2]]<-modSum
	names(output)<-c("Model Table", "Model Summary")
	return(output)
}



#
#	measuring clutch/litter size
#
#	limited function to start:
#		grouping is based on shared moms, locations, and falling within the birthing window...will probably want to relax these going forward!

litterSize<-function(Studbook = NULL, startingDate=NULL, endingDate=NULL, institutions=NULL, ADDinstitutions=NULL, DROPinstitutions=NULL, UDF_FieldName=NULL, UDF_Levels=NULL, birthingWindow=NULL, ...){

	# BHtable is basically the same as the BHin output from specificEvents, which applies the date, institution, and UDF windows to the dataset

	BHtable<-specificEvents(Studbook = Studbook , eventType = c("Birth", "Hatch"), startingDate=startingDate, endingDate=endingDate, 
					institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels)

	# trim, then ammend the data table with the parents

	BHtable<-BHtable[,which(colnames(BHtable) %in% c("StudbookID", "TranDate", "Location"))]
	Dam<-rep(NA, nrow(BHtable))
	Sire<-rep(NA, nrow(BHtable))

	for(i in 1:nrow(BHtable)){
		Dam[i]<-as.character(Studbook$Master$Dam[which(Studbook$Master$StudbookID == BHtable$StudbookID[i])])
		Sire[i]<-as.character(Studbook$Master$Sire[which(Studbook$Master$StudbookID == BHtable$StudbookID[i])])
	}	

	BHtable<-data.frame(BHtable, Dam, Sire)
	BHtable$TranDate<-as.Date(BHtable$TranDate)

	# remove any that have unk moms (unk dads are ok...although may not be really...sort of depends on how we're grouping animals, here assuming it's by mom)

	if(length(which(BHtable$Dam=="UNK"))>0)
		BHtable<-BHtable[-which(BHtable$Dam=="UNK"),]

	# now group the animals by clutch/litter
	#	start at the top, and roll through the BH table, assigning animals to Litters
	#		for animal X that has not yet been assigned a Litter, give it Litter A, then find any animals that have the same location and mom, and fit the birth window...assign them Litter A also
		

	Litter<-rep(NA, nrow(BHtable))
	indic<-1	#counter variable for counting the # of Litters

	for(i in 1:nrow(BHtable)){
		if(is.na(Litter[i])==TRUE){
				
			xx<-which(BHtable$Dam == BHtable$Dam[i] & BHtable$Location == BHtable$Location[i] & BHtable$TranDate >= BHtable$TranDate[i] - birthingWindow & BHtable$TranDate <= BHtable$TranDate[i] + birthingWindow) 
			Litter[xx]<-indic
			indic<-indic+1
		}
	}

	BHtable<-data.frame(BHtable, Litter)

	# then condense down into one row per Litter and count how many entries per litter

	mom<-rep(NA, length(unique(Litter)))
	dad<-rep(NA, length(unique(Litter)))
	date<-rep(NA, length(unique(Litter)))
	location<-rep(NA, length(unique(Litter)))
	noffspring<-rep(NA, length(unique(Litter)))
	referenceKid<-rep(NA, length(unique(Litter)))

	for(i in 1:length(unique(Litter))){
		mom[i]<-as.character(BHtable$Dam[which(BHtable$Litter==unique(Litter)[i])][1])
		dad[i]<-as.character(BHtable$Sire[which(BHtable$Litter==unique(Litter)[i])][1])
		date[i]<-as.character(BHtable$TranDate[which(BHtable$Litter==unique(Litter)[i])][1])
		location[i]<-as.character(BHtable$Location[which(BHtable$Litter==unique(Litter)[i])][1])
		referenceKid[i]<-as.character(BHtable$StudbookID[which(BHtable$Litter==unique(Litter)[i])][1])
		noffspring[i]<-length(which(BHtable$Litter==unique(Litter)[i]))
	}

	output<-data.frame(Sire=dad, Dam=mom, Date=date, Location=location, ReferenceOffspring=referenceKid, LitterSize=noffspring)

	return(output)
}




#
#
#	litterSizeInbreedingAnalysis
#
#		lots to work on with this...it's very early and not well refined yet
#			definitely need to add graphing capabilities
#			make output more pretty

litterSizeInbreedingAnalysis<-function(Studbook=NULL, LSize=NULL, InbreedingTable=NULL, modelOut=NULL,... ){
	
	#
	# get the inbreeding values for the mom, dad, and babies in each litter
	#

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
		LSize2<-LSize2[-naD,]
	
	# model fitting
	#	doesn't include blocking by parents or pedigreed relationship
	#	also doesn't implement a proper distribution...should use function vglm and family = pospoisson(), but I have to build model selection/term evaluation stuff for that

	#	first do it without the NA dads and moms

	mm1<-glm(LitterSize~MomF*DadF*BabyF, data=LSize2, family='poisson')
	m1<-stepAIC(mm1, trace=0)	#trace=0 silences the output

	#
	#	if DadF isn't in the model, re-run with the NA dads included....need to re-write this to handle the NA moms too.  for now we just don't add either back in.

#	if(length(which(names(m1[[1]])=="DadF"))==0){
#		mm1<-glm(LitterSize~MomF*BabyF, data=LSize, family='poisson')
#		m1<-stepAIC(mm1, trace=0)
#	}


	#
	# summarize the model fit, ammending the NS terms
	#

	s1<-summary(m1)$coefficients
	s2<-matrix(NA, nrow=nrow(s1), ncol=ncol(s1)+1)
	s2[,2:ncol(s2)]<-round(s1[,],4)
	s2[,1]<-rownames(s1)
	colnames(s2)[2:ncol(s2)]<-colnames(s1)
	colnames(s2)[1]<-"Parameter"

	if(length(grep("MomF", rownames(s1), ignore.case=TRUE ))==0){
		s2<-rbind(s2, rep(NA, ncol(s2)))
		s2[nrow(s2), which(colnames(s2)=="Parameter")]<-"MomF"
		pval<-add1(m1, ~.+MomF , test="Chisq")$"Pr(>Chi)"[2]
		s2[nrow(s2), which(colnames(s2)=="Pr(>|z|)")]<-round(pval,4)
	}

	if(length(grep("DadF", rownames(s1), ignore.case=TRUE ))==0){
		s2<-rbind(s2, rep(NA, ncol(s2)))
		s2[nrow(s2), which(colnames(s2)=="Parameter")]<-"DadF"
		mm1<-update(m1, data=LSize2)
		pval<-add1(mm1, ~.+DadF , test="Chisq")$"Pr(>Chi)"[2]
		s2[nrow(s2), which(colnames(s2)=="Pr(>|z|)")]<-round(pval,4)
	}

	if(length(grep("BabyF", rownames(s1), ignore.case=TRUE ))==0){
		s2<-rbind(s2, rep(NA, ncol(s2)))
		s2[nrow(s2), which(colnames(s2)=="Parameter")]<-"BabyF"
		pval<-add1(m1, ~.+BabyF , test="Chisq")$"Pr(>Chi)"[2]
		s2[nrow(s2), which(colnames(s2)=="Pr(>|z|)")]<-round(pval,4)
	}

	# write it out if desired

	if(modelOut==TRUE){

		tname<-paste(sub(" ", "_", as.character(Studbook$DatabaseDetails$CommonName)), "LitterClutch_Size_Inbreeding_Analysis_Stats_Table.txt", sep="_")
		write.table(s2, tname, row.names=F, sep="\t")
	}

	return(summary(m1))

}
















#
#
#	wrapper function for inbreeding analysis
#		still in development, obv
#






inbreedingAnalysis<-function(StudbookName = NULL, OverlayToUse=NULL, startingDate=NULL, endingDate=NULL, mortalityWindow=0, institutions=NULL, 	ADDinstitutions = NULL, DROPinstitutions = NULL,	
					UDF_FieldName = NULL, UDF_Levels = NULL, FSummary=c("Births", "Population"), Analyses=c("InfMort", "CLsize"),
					Tables=NULL, Graphs=NULL, birthingWindow=0, GraphResolution=100, modelsOut=NULL...){

	# load the necessary packages

		PackageLoad()


	# grab the studbook

		SB<-SBsetup(DBname = StudbookName, DBtablenames = c("Master", "Event", "Sex"), Overlay=TRUE, UDF=TRUE, verbose=FALSE, silent=TRUE)


	# apply the overlay indicated (or if not indicated, it will prompt for which, or to put in None)

		SBwithOL<-overlayApply(Studbook = SB, OverlayToUse = OverlayToUse, verbose=FALSE, Remove = TRUE, AddIsHypothetical=TRUE)


	# Measure inbreeding in the population

		Inbreeding<-SBinbreeding(Studbook = SBwithOL, verbose=FALSE, silent=TRUE)


	# Summarize the inbreeding in the population over time (as desired)

		# 1. Births/hatches only

		if(length(which(FSummary %in% "Births")) + length(which(FSummary %in% "Hatch")) >0){

			# restricting the births to those within the institution and time windows, and based on UDFs

				BHin<-specificEvents(Studbook = SBwithOL, eventType = c("Birth", "Hatch"), startingDate=startingDate, endingDate=endingDate, 
								institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels)

			# trim and format the data

				BHin<-BHin[,which(colnames(BHin) %in% c("StudbookID", "TranDate"))]
				BHin<-data.frame(BHin, Year=format(BHin$TranDate, "%Y"), Fval=Inbreeding$Fval[which(Inbreeding$StudbookID %in% BHin$StudbookID)])

			# doing by year, so summarize Fvals by year
					
				birthFvals<-summarizeFoverTime(Ftable=BHin, startingDate=startingDate, endingDate=endingDate)

			# write to table if desired  (need to make that code changable from Births to Hatches)

				if(Tables == TRUE){
					tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Historical_Inbreeding_Births.csv", sep="_")
					write.table(birthFvals, tname, sep=",", row.names=F)
				}

			# graph if desired
	
				if(Graphs == TRUE)					
					graphFoverTime(Studbook=SBwithOL, FsummaryTable=birthFvals, GraphResolution=GraphResolution, type="BH")

		}


		# 2. Whole population

		if(length(which(FSummary %in% "Population")) >0){
		

			# census the population to figure out who was in it each year on 01/01...this is uber slow...

				censusPop<-censusPopulation(Studbook=SBwithOL,startingDate=startingDate, endingDate=endingDate, institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, 
					UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels, censusDate="01-01")

			# then tack on their F values (could prolly be done smoother/slightly faster, but is ok for now)

				Fval<-rep(NA, nrow(censusPop))
				for(i in 1:nrow(censusPop))
					Fval[i]<-Inbreeding$Fval[which(as.character(Inbreeding$StudbookID)==as.character(censusPop$StudbookID[i]))]

				Fvals<-data.frame(censusPop, Fval)

			# doing by year, so summarize Fvals by year
					
				popFvals<-summarizeFoverTime(Ftable=Fvals	, startingDate=startingDate, endingDate=endingDate)

			# write to table if desired

				if(Tables == TRUE){
					tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Historical_Inbreeding_Population.csv", sep="_")
					write.table(popFvals, tname, sep=",", row.names=F)
				}

			# graph if desired
	
				if(Graphs == TRUE)					
					graphFoverTime(Studbook=SBwithOL, FsummaryTable=popFvals, GraphResolution=GraphResolution, type="Pop")

		}


	# Analyses (as desired)

		# 1. Infant Mortality

		if(length(which(Analyses %in% "InfMort")) >0){

			# if we didn't do BHin yet, do now!

			if(exists("BHin")==FALSE){

				# restricting the births to those within the institution and time windows, and based on UDFs

					BHin<-specificEvents(Studbook = SBwithOL, eventType = c("Birth", "Hatch"), startingDate=startingDate, endingDate=endingDate, 
								institutions = institutions, ADDinstitutions = ADDinstitutions, DROPinstitutions = DROPinstitutions, UDF_FieldName = UDF_FieldName, UDF_Levels = UDF_Levels)

				# trim and format the data

					BHin<-BHin[,which(colnames(BHin) %in% c("StudbookID", "TranDate"))]
					BHin<-data.frame(BHin, Year=format(BHin$TranDate, "%Y"), Fval=Inbreeding$Fval[which(Inbreeding$StudbookID %in% BHin$StudbookID)])
			}


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
		
			# infant mortality as function of inbreeding analysis

			InfMortFAnalysis<-infMortInbreedingAnalysis(IMtable = InfantMortalities, Studbook=SBwithOL, modelOut=modelsOut, Graphs=Graphs	)
	
		}

		# 2. Clutch/Litter Size

		if(length(which(Analyses %in% "CLsize")) >0){

			# measure litter/clutch sizes

				LCsizes<-litterSize(Studbook = SBwithOL, startingDate=startingDate, endingDate=endingDate, institutions=institutions, ADDinstitutions=ADDinstitutions, 
					DROPinstitutions=DROPinstitutions, UDF_FieldName=UDF_FieldName, UDF_Levels=UDF_Levels, birthingWindow=birthingWindow)

			# if wanted, save a table of litter/clutch sizes

				if(Tables==TRUE){
					tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Specific_LitterClutch_Sizes.csv", sep="_")
					write.table(LCsizes, tname, sep=",", row.names=F)

					SummaryTable<-count(LCsizes, "LitterSize")
					tname<-paste(sub(" ", "_", as.character(SBwithOL$DatabaseDetails$CommonName)), "Summary_LitterClutch_Sizes.csv", sep="_")
					write.table(SummaryTable, tname, sep=",", row.names=F)
				}
	
			# analyze litter/clutch size as a function of inbreeding

				LCsizeFAnalysis<-litterSizeInbreedingAnalysis(LSize=LCsizes,InbreedingTable=Inbreeding,Studbook=SBwithOL, modelOut=TRUE)

			# to add: graphing litter/clutch size
		}

}






