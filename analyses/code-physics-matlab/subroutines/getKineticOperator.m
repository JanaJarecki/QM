% find the eigen states of a potential

%Input
%xArray: evenly spaced vector
%potential: vector of same size as xArray
%kinematicFactor: number
%higherOrderBoolean: true/false

%Output
%eigVals: eigenstates of the potential. Vector of size(xArray)
%eigVecs: eigenstates of potential. Matrix of size(xArray) x size(xArray).
%         Each column is an eigenstate normalized as sum(eigenstate)*dx = 1. 

function T=getKineticOperator(xArray,kinematicFactor, higherOrderBoolean)
        
        numSpatialPts=length(xArray);
        spaceStep=xArray(2)-xArray(1);
        
        % construct the kinetic matrix
        kineticMatrix=zeros(numSpatialPts,numSpatialPts);
                
        if higherOrderBoolean
            %%%% the following is correct to O(deltax^5)
            for j=1:numSpatialPts-2
                kineticMatrix(j,j)=30/12;
                kineticMatrix(j,j+1)=-16/12;
                kineticMatrix(j+1,j)=-16/12;
                kineticMatrix(j,j+2)=1/12;
                kineticMatrix(j+2,j)=1/12;
                
            end
            kineticMatrix(numSpatialPts,numSpatialPts)=30/12;
            kineticMatrix(numSpatialPts-1,numSpatialPts-1)=30/12;
            kineticMatrix(numSpatialPts-1,numSpatialPts)=-16/12;
            kineticMatrix(numSpatialPts,numSpatialPts-1)=-16/12;
        else
            %%%% the following is correct to O(deltax^4)
            for j=1:numSpatialPts-1
                kineticMatrix(j,j)=2;
                kineticMatrix(j,j+1)=-1;
                kineticMatrix(j+1,j)=-1;
            end
            kineticMatrix(numSpatialPts,numSpatialPts)=2;
        end
        
        T=kineticMatrix*kinematicFactor/spaceStep^2;
        
        % add the potential to the diagonal
        %for j=1:numSpatialPts
        %    kineticMatrixNew(j,j)=kineticMatrixNew(j,j)+potential(j); % this is now your Hamilton
        %end
end
    