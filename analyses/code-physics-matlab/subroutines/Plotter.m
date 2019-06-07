classdef Plotter
    %PATHPLOTTER Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        level;                  % The current level being plotted
        pathFigure;             % The figure, which shows the individual steps
        
        potentialHandleStep;
       
        obstacleHandlesIterations = [];
        targetHandleIteration = [];
        pointsHandleIterations
        
        wavefunctionHandleStep;
        pointsHandleStep
    end
    
    methods
        function obj = Plotter(level)
  
            obj.level = level;

            obj.pathFigure = figure('position',[600 50 600 500]);
            axis([level.xMin level.xMax level.yMin level.yMax])
            title('Step Plot')
            ylabel('Energy')
            xlabel('Position')
            hold on;
            if (~isempty(obj.level.targets))
                  for p=1:size(obj.level.targets,1)
                        obj.targetHandleIteration(end+1) = rectangle('Position',obj.level.targets(p,:), 'FaceColor','g');
                  end
            end
            if (~isempty(obj.level.obstacles))
                for p=1:size(obj.level.obstacles,1)
                    obj.obstacleHandlesIterations(end+1) = rectangle('Position',obj.level.obstacles(p,:), 'FaceColor','r');
                end
            end
            if (~isempty(obj.level.points))
                if size(obj.level.points,1)>0
                    obj.pointsHandleIterations = plot(obj.level.points(:,1),obj.level.points(:,2),'.', 'markersize',20);
                end
            end
        end
        
        function self=iteration(self, u,extraInfo)
             set(0,'CurrentFigure',self.pathFigure)
             if size(u,2)~=2
                    u=[u, zeros(size(u,1),1)];
             end
             pathHandleIteration = plot(u(:,1),u(:,2),'.b','markersize',4);
             titleString = [extraInfo{2,1} '=' num2str(extraInfo{1,1}) ', ' extraInfo{2,2} '=', num2str(extraInfo{1,2})];
             if (size(extraInfo,2)>2)
                 for i=3:size(extraInfo,2)
                     titleString = [titleString ', ' extraInfo{2,i} '=' num2str(extraInfo{1,i})] ;
                 end
             end
             title(titleString)
             uicontrol('Style', 'pushbutton', 'String', 'Run current Path',...
                'Position', [20 20 100 20],...
                'Callback', {@self.runResultPath,u});        
             drawnow
             delete(pathHandleIteration)
        end

        function runResultPath(self,foo,foofoo,u)
            resultpathfigure = figure;
            propagator = Propagator(self.level.x, self.level.normalDt, self.level.kin);
            psi = self.level.startState;
            for k= 1:size(u,1)
                pause(0.005)
                
                potentialk = self.level.potential(u(k,:));
                psi = propagator.propagateInTime(psi, potentialk);
                
                theWavePlusPot = self.level.waveScale.*abs(psi').^2 + potentialk;
                
                set(0,'CurrentFigure',resultpathfigure)
                clf
                hold on
                
                if (~isempty(self.level.obstacles))
                    for p=1:size(self.level.obstacles,1)
                        rectangle('Position',self.level.obstacles(p,:), 'FaceColor','r')
                    end
                end
                if (~isempty(self.level.targets))
                    for p=1:size(self.level.targets,1)
                        rectangle('Position',self.level.targets(p,:), 'FaceColor','g')
                    end
                end
                if (~isempty(self.level.points))
                    if size(self.level.points,1) > 0
                        self.pointsHandleStep = plot(self.level.points(:,1),self.level.points(:,2),'.', 'markersize',30);
                    end
                end
                
                plot(self.level.x,potentialk,'k');
                plot(self.level.x,theWavePlusPot,'r');
                plot(u(:,1),u(:,2),'.b','markersize',4);
                drawnow;
            end
            close(resultpathfigure);
       end
    end
end


