LoadPackage("DESIGN");




#######################################################################################################################################
#######################################################################################################################################
#   PART I
#   initializing all functions to read in and create block designs from the processed output of the program genreg
########################################################################################################################################
########################################################################################################################################
# The following functions are needed to read in and convert adjacency lists of a graph and then search for a corresponding design. The
# adjacency lists are saved as a single .txt file that contains v rows listing all adjacent vertices to
# the respective vertex. ReadGENREGString reads these files ( with input format for example "{2,3,4,5}")
# and returns a list of length v whose entries are the adjacency list per vertex
#########################################################################################################################################

Substring:=function( str, start, length )
	local result, i,s;
	result:="";
	s:=Size(str);
	for i in [start..Minimum(start+length, s)] do
		Add(result, str[i]);
	od;
	return result;
end;

SubstringIndexInString:=function( haystack, needle, offset )
	local h,n,k, sh, sn;
	sh:=Size(haystack);
	sn:=Size(needle);
	for h in [1+offset..sh - sn+1] do
		k:=0;
		for n in [1..sn] do
			if needle[n] = haystack[h+n-1] then
				k:=k+1;
			fi;
		od;
		if k = sn then
			return h;
		fi;
	od;
	return -1;
end;
ReadGENREGString:=function(path)
	local inputStream, input,start,finish, substring, results, afterFirstColon, i, results2,tmp;
	inputStream:=InputTextFile(path);
	input:=ReadAll(inputStream);
	CloseStream(inputStream);
	results:=[];
	while( Size(input)>0 ) do
        start:=SubstringIndexInString(input, "{", 0)+1;
		if start > 0 then
			afterFirstColon:=Substring(input, start, Size(input));
			finish:=SubstringIndexInString(afterFirstColon, "}", 0)-1;
            substring:=Substring(afterFirstColon,1,finish-1);
			input:=Substring(afterFirstColon,finish+2,Size(afterFirstColon));
            Add(results, substring);
		else
            input:= "";
		fi;
    od;
    

    results2:=[];
	for i in results do
		start:=SubstringIndexInString(i, ",", 0);
		tmp:=[];
		while ( start >=0 ) do
			substring:=EvalString(Substring(i, 1, start-2));
            Add(tmp, substring);
			i:=Substring(i, start+1, Size(i));
			start:=SubstringIndexInString(i, ",", 0);
		od;
		if start >0 then
		Add(tmp,i);	
		else 	Add(tmp, EvalString(i));
		fi;
		Add(results2, tmp);
	od;

	return results2;

end;
######################################################################################################################
# ConvertAdjacencyListsToParitionLists converts the adjacency lists read in by ReadGENREGString into the partition lists 
# used for generating a design with the package DESIGN
######################################################################################################################

ConvertAdjacencyListsToPartitionLists:=function( adjacencyListList )
	local i,j, lambda1List, lambda0List, thingsToReturn;

	lambda1List:=[];
	for i in [1..Size(adjacencyListList)] do
		for j in [1..Size(adjacencyListList[i])] do
			if i < adjacencyListList[i][j] then
				Add(lambda1List, [i, adjacencyListList[i][j]]);
			fi;
		od;
	od;
 
	thingsToReturn:=[lambda1List];
		

	
	lambda0List:=Difference( Combinations([1..Size(adjacencyListList)],2),lambda1List);
	
	if Size(lambda0List)>0 then
		Add(thingsToReturn, lambda0List);
	fi;

	
	return thingsToReturn;
end;


#####################################################################################################################
# MakeBIBD returns a BIBD with given v,k,lamda if it exists
#####################################################################################################################

MakeBIBD:=function(v,blockSize,lambda)
	local myDesign, parts;
	myDesign:=rec();
	myDesign.v:=v;
	#myDesign.blocks:=[];
	myDesign.blockSizes:=[blockSize];
	
	myDesign.tSubsetStructure:=rec( t:=2,  lambdas:=[lambda]);
	
	myDesign.isoLevel:=0;
	myDesign.isoGroup:=Group([()]);
	return BlockDesigns(myDesign);
end;

#####################################################################################################################
# MakeBlockDesignFromAdjacencyListList constructs a block design with the function BlockDesings() in the package DESIGN.
# Input are the partitions created with ConvertAdjacencyListsToPartitionLists
#####################################################################################################################

MakeBlockDesignFromAdjacencyListList:=function( adjacencyListList, blockSize,lambda )
	local myDesign, parts;
	myDesign:=rec();
	myDesign.v:=Size(adjacencyListList);
	myDesign.blockSizes:=[blockSize];
	parts:=ConvertAdjacencyListsToPartitionLists(adjacencyListList);
    myDesign.tSubsetStructure:=rec( t:=2, partition:= parts, lambdas:=[lambda+1]);
	if Size(parts)>1 then
		if lambda=0 then
		Add(myDesign.tSubsetStructure.lambdas, 0);
		else
		Add(myDesign.tSubsetStructure.lambdas, lambda);
        fi;
	fi;
	myDesign.isoLevel:=0;
	myDesign.isoGroup:=Group([()]);
	return BlockDesigns(myDesign);
end;
#####################################################################################################################
# MakeBlockDesignFromFile calls MakeBlockDesignFromAdjacenyListList on a .txt file with the adjacency lists stored of 
# respective graph.
#####################################################################################################################

MakeBlockDesignFromFile:=function( path, blockSize,lambda)
	return MakeBlockDesignFromAdjacencyListList(ReadGENREGString(path), blockSize,lambda);
end;

#####################################################################################################################
# IsBD checks whether a graph stored as adjacency lists in a .txt file gives a design with desired blocksize and lambda
#####################################################################################################################

IsBD:=function( input, blockSize,lambda)
local BlockDes;
  BlockDes:=MakeBlockDesignFromAdjacencyListList( input, blockSize,lambda);
	 		
		return BlockDes;
	
end;


#######################################################################################################################################
#######################################################################################################################################
#   PART 2
#   Setting directories and the main function
########################################################################################################################################
########################################################################################################################################
# Setting all directories where the files containing the adjacency lists can be found and where the log-files of the GAP session should be saved. 
path := DirectoriesSystemPrograms();;
date := Filename( path, "date" );
str := "";; a := OutputTextString(str,true);;
Process( DirectoryCurrent(), date, InputTextNone(), a, [] );
CloseStream(a);
LogTo(Concatenation("~/Documents/OptimalRGDS/Programs and Data/DesignLog",str,".txt"));

########################################################################################################################################
# For details on the following function see Manual.
########################################################################################################################################


TestForBlockSize:=function(path,v,degree,optimality,max_rank,blockSize,minLambda,maxLambda)

	local files, i,j,input,prefix, file,blockSizesChecked,BlockDes,ranking,max_blocksize,lambda,newdegree,b,tdesignlist,D, dir, file_prefix,printdir;
    


    if v<10 then
    dir:=Concatenation(path,String(0),String(v),"/",String(0),String(v),"_",String(degree),"/");
    printdir:=Concatenation(path,String(0),String(v),"/");
    else
    dir:=Concatenation(path,String(v),"/",String(v),"_",String(degree),"/");
    printdir:=Concatenation(path,String(v),"/");
    fi;
    

    file_prefix:=Concatenation("Graphs/",String(optimality),"BestGraph");

	Print("\n Checking which files exist...\n");

	files:=[];
		
		for i in [1..max_rank] do
			file:=Concatenation(dir,file_prefix,String(i),".txt");
			if IsExistingFile(file) then
				Add(files, [file, i]);
				else break;
				fi;
	od;
	blockSizesChecked:=[];

	lambda:=minLambda;
    Print("We've got ",Size(files)," files to check...\n\n");

    while Length(blockSizesChecked)=0 and lambda<maxLambda+1 do
        newdegree:=(v-1)*lambda+degree;
		
		if (Quotient(newdegree, (blockSize-1)) in Integers) then
            Print("\n \n Checking for Lambda= ",lambda,"\n\n");

            for i in files do

                input:=ReadGENREGString(i[1]);

                if Length(blockSizesChecked)=0 then
                    Print("Trying to find design for graph number ",i[2],"\n\n");
                    BlockDes:=MakeBlockDesignFromFile(i[1],blockSize,lambda);
							if Size(BlockDes)>0 then
								Print(Concatenation(optimality,"BestGraph"), i[2]," was successful for block size ", blockSize," and replication ",ReplicationNumber(BlockDes[1]),"!\n\n");
								PrintTo(Concatenation(printdir,optimality,"_BestRGD_",String(v),"_",String(blockSize),"_",String(ReplicationNumber(BlockDes[1])),"_",String(lambda),".txt"),BlockDes[1]);
                                PrintTo(Concatenation(printdir,optimality,"_Rank_",String(v),"_",String(blockSize),"_",String(ReplicationNumber(BlockDes[1])),"_",String(lambda),".txt"),i[2]);
                                
                                    BlockDesignsToXMLFile(Concatenation(printdir,optimality,"_BestRGD_",String(v),"_",String(blockSize),"_",String(ReplicationNumber(BlockDes[1])),"_",String(lambda),".xml"),[BlockDes[1]]);

                                
                                Print("Result Printed to Directory ",printdir,"\n");
                                Add(blockSizesChecked, blockSize);
							fi;
                fi;
						
            od;
        fi;
        lambda:=lambda+1;

	od;

end;






