


pathToDir:="/Users/cakiro01/Documents/OptimalRGDS/Programs and Data/";

Read(Concatenation(pathToDir,"OptimalRGDs.g"));

GetDesigns:=function(v,optimality,minLambda,maxLambda)
    local d,i;
    for d in [3..9]do
        for i in [2..9] do
        Print("Checking for v=",v," degree=", d," and block size ",i);
        TestForBlockSize(pathToDir,v,d,optimality,10,i,minLambda,maxLambda);

        od;
    od;

end;



