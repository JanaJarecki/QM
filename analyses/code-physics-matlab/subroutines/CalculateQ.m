function [Q_T,Qs,F,eta] = CalculateQ(levelObject,path)
% Calculates the Q factor - see Mirecks article.

% Level stuff
timeStep = levelObject.normalDt;
xArray = levelObject.x;
dx = levelObject.dx;
kinematicFactor = levelObject.kin;
V = @(u) levelObject.potential(u);

%%% path guess
% Make path one element longer if it is outside the goal area:
u = path;


lenT = size(u,1);
lenU = size(u,2);
%%% wave function states
psi0 = levelObject.startState;
target = levelObject.targetState; %% target state ALWAYS needs input from path!!
lenX = length(psi0);

%% Generation of states
% Generate the forwards evolved states
psi = zeros(lenX,lenT+1);
psi(:,1) = psi0;
for n=1:lenT
    psi(:,n+1) = propegateInTime(psi(:,n),xArray,timeStep,kinematicFactor,V(u(n,:)).',true,true);
end
psiT=psi(:,end);
% Generate the backwards evolved states 
chi = zeros(lenX,lenT+1);
chi(:,end)=target(u(end,:));
for n = lenT:-1:1
    chi(:,n) = propegateInTime(chi(:,n+1),xArray,timeStep,kinematicFactor,V(u(n,:)).',false,true); % this is only correct for linear Schr�dinger equation
end
% Fidelity
F=abs(psiT'*chi(:,end)*dx).^2;
% Calculate the xi vectors
xi=zeros(lenX,lenT);
for n=1:lenT
    xi(:,n)=1/sqrt(F*(1-F))*(dx*chi(:,n)*chi(:,n)'*psi(:,n)-F*psi(:,n));
end
% Check the properties of xi
 %for n=1:lenT
 %  disp(['The fidelity of psi and xi is ',num2str(abs(psi(:,n)'*xi(:,n)*dx).^2), '.The norm of xi is ',num2str(abs(xi(:,n)'*xi(:,n)*dx))])
 %end

%% Initialize Hamiltonian constructor code
T=getKineticOperator(xArray,kinematicFactor,true);
%% Calculate of Q
Qs=zeros(lenT,1);
for n=1:lenT
   Qs(n)=imag(xi(:,n)'*(T+diag(levelObject.potential(u(n,:))))*psi(:,n)*dx);
end
Q_T=timeStep/(lenT*timeStep)*sum(Qs);

% Start calculation of Mirek's eta parameter
eta=std(Qs)/Q_T;
