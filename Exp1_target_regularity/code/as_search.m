 function  as_search
% Additional singleton search task: 
% Strongway 26.01.2018

try
   
    exp = CExp(1,[840, 1, 1, 1],'blockRepetition',1); % repitition, factors
    N=size(exp.seq,1);   
    
    exp.subInfo('group');  % acquire subject information 

    seq = csvread(['sequences/seq_' num2str(exp.sPara) '.csv'],1);
    exp.seq=seq;
    
    n=exp.sPara;
       
    exp.maxTrls = size(exp.seq,1);
        
    blk_trials = 60;
    
    v = CDisplay('skipSync',1,'monitorSize',20,'viewDistance',60, 'Color',[192 192 192],'fontSize', 20, 'fullWindow',1);
    
    % create an instance of response device
    kb = CInput('k',[1,2],{'LeftArrow','UpArrow'});
    %  1 - horizontal (Left), 2 - vertical (Up)
    
    circle_diam = 2;
    box_size = 2*sqrt(pi/4);
    % create stimuli (Square-Green, Sq-Red, Circle-Green, Circle-red)
    container(1) = v.createShape('rectangle', box_size, box_size, 'fill',0,'color',[0,196,0]);
    container(2) = v.createShape('rectangle', box_size, box_size,'fill',0, 'color',[196,0,0]);
    container(3) = v.createShape('circle', circle_diam, circle_diam, 'fill',0,'color',[0,196,0]);
    container(4) = v.createShape('circle', circle_diam, circle_diam, 'fill',0,'color',[196,0,0]);
    
    % orientation lines -  horizontal, vertical
    line_length = 1.5;
    line_thick = 0.3;
    v_line(1) = v.createShape('rectangle',line_length,line_thick, 'fill',1,'color',[128,128,128]);
    v_line(2) = v.createShape('rectangle',line_thick,line_length, 'fill',1,'color',[128,128,128]);
    
    % prepare the locations of the presentations x,y
    ecc = 4;
    x = cos(linspace(0,360,9)/180*pi) * ecc;
    y = sin(linspace(0,360,9)/180*pi) * ecc;
    xy = [x(:), y(:)];
    xy(end,:) = [];
    rects = ones(8,2)*box_size;     % sizes of each container
    
    % start the experiment
    v.dispText(['Visual Search \n', ...
        'In this experiment, you will see a display with eight objects (circle/diamond). And one of the shapes is different from the other seven. \n', ...
        'The task is to find the different one as target and indicate whether the line segment inside the target was vertical or horizontal. \n', ...
        'If the line is vertical, please press the UpArrow on the keyboard; \n', ...
        'If the line is horizontal, please press the LeftArrow on the keyboard.\n',... 
        'Please try to respond as quickly and accurately as possible.\n', ...
        '\n\n when you are ready, press any key to continue']);
    kb.wait;
    
    % formal experiment
    for iTrl=1: exp.maxTrls
        
        % get experiment condition: display 1 gabor or 4?
        cond = exp.getCondition;
        
        tarPos = cond(1); % target location
        % note, due to y-axis is downward, the 1-8 is starting from east,
        % clockwise. Thus 2,3,4 are located at bottom side.
        
        tarContainer = cond(2); % target container Diamond / Circle
        distColor = cond(4); % additional singleton color
        disPos=0; % No distractor
        
        % initialize all distractors
        if tarContainer == 1 % Diamond
            if distColor == 1 
                boxes = repmat(container(3), 1,8); % distractor as circle
            else
                boxes = repmat(container(4), 1,8); % distractor as circle
            end
            % change the target container
            if distColor == 1    
                boxes(tarPos) = container(1);
            else
                boxes(tarPos) = container(2);
            end
            % if additional singleton is present, change it
            if disPos > 0
                if distColor == 1 
                    boxes(disPos) = container(4); % red circle
                else
                    boxes(disPos) = container(3); % green circle
                end
            end
        else
           if distColor == 1 
               boxes = repmat(container(1), 1,8); % distractors as diamond
           else
               boxes = repmat(container(2), 1,8); % distractors as diamond
           end

           if distColor == 1
                boxes(tarPos) = container(3);
           else
                boxes(tarPos) = container(4);
           end
            % if additional singleton is present, change it
            if disPos > 0
                if distColor == 1
                    boxes(disPos) = container(2); % red diamond
                else                    
                    boxes(disPos) = container(1); % green diamond
                end
            end
            
        end
        
        % orientation target and distractor
        tarOrientation = cond(3); % target orientation 1 hori / 2 vert.
        line_ori = repmat([1,2], 1,4);
        line_ori = Shuffle(line_ori); % randomize
        while(line_ori(tarPos) ~= tarOrientation)
            line_ori = Shuffle(line_ori); % randomize again
        end
        % not prepare lines, rectangles
        l_rects = repmat([line_length,line_thick], 8,1);
        for ipos = 1:8
            v_lines(ipos) = v_line(line_ori(ipos));
            if line_ori(ipos) == 2 % vertical
                l_rects(ipos,:) = [line_thick,line_length]; % replace horizontal one
            end
        end
        
        % container rotation 45 degree
        rotations = repmat(45,1,8); % change square to diamond
        
        % change sizes to match requirements of the shapes on this trial
        if tarContainer==2
            rects=ones(8,2)*box_size;
            rects(tarPos,:)=circle_diam;
        else
            rects=ones(8,2)*circle_diam;
            rects(tarPos,:)=box_size;
        end
        
        % trial starts with a fixation cross
        v.dispFixation(30);
        WaitSecs(0.5);
        
        v.dispFixation(30, 1,0);
        % display containers
        v.dispItems(xy, boxes, rects, rotations, 0);
        % display lines
        v.dispItems(xy, v_lines, l_rects);
        
        % acquire response
        initTime = GetSecs;
        [key, rTime] = kb.response;
        
        % blank screen
        v.dispFixation(30);
%        v.flip(1);
        
        % store the response
        % note: for UML, you need to set two values
        % which condition, and response key
        exp.setResp([key, rTime-initTime]);
        
        % error feedback
        if (tarOrientation ~= key)
            v.dispText('Error!');
            WaitSecs(0.5);
        end
        
        v.dispFixation(30);
%        v.flip(1);
        
        % block break or sometime feedback
        if mod(iTrl,blk_trials) == 0 && iTrl < exp.maxTrls
            if iTrl == (exp.maxTrls+blk_trials)/2
                v.dispText(['You have finished half of the experiment\n', ...
                         'Please press any key to continue.']); % Add halfway message here
            else
                v.dispText('Block break. Please press any key to continue.');
            end
            kb.wait;
            WaitSecs(0.5);
        end
        
        % inter-trial interval
        v.dispFixation(30);
        WaitSecs(0.5+rand*0.25);
        
        % debugging needs 'stop' function
        if kb.wantStop %stop for debugging
            break;
        end
        
    end %end of trials
    
      % awareness question
  kb2 = CInput('k',[1,2],{'1!','2@'});
  v.dispText(['You have finished the experiment.\n', ...
      '\n\n Please answer following questions.\n', ...
      '\n\n When you are ready, press any key to continue.']);
  kb2.wait;
  
  v.dispText(['Did you notice any pattern to how the position of the target changed\n', ...
    'from trial to trial, or do you think the changes were completely random?\n', ...
    '\n1) The target position changed following a regular pattern from trial to trial. \n', ...
    '2) The target position changed randomly from trial to trial.']);
       
  aware_1 = kb2.response(1);   
   WaitSecs(0.3);
    
  kb3 = CInput('k',[1,2,3,4,5,6,7],{'1!','2@', '3#','4$', '5%','6^','7&',});

  v.dispText(['Which of the following changes of target position from one trial to\n', ...
      'the next do you think occurred the most often?\n', ...
    '1) Target moved to opposite end of circle\n', ...
    '2) Target moved one step clockwise\n', ...
    '3) Target moved one step counterclockwise\n', ...
    '4) Target moved two steps clockwise\n', ...
    '5) Target moved two steps counterclockwise\n', ...
    '6) Target moved three steps clockwise\n', ...
    '7) Target moved three steps counterclockwise']);
    aware_2 = kb3.response(1);
    WaitSecs(0.3)

    exp.u=[aware_1 aware_2];
  
    %closing the experiment
    exp.saveData;   %save data
    
    % now the adaptive uml results are stored in u cells.
    % estimations: u{i}.phi
    % plot the estimation results:
    % u{i}.plotP;
    
    v.dispText('You have finished the experiment. Thank you. Have a nice day!');
    kb.wait;
    v.close;
    
catch ME
    
    disp(ME.message);
    disp(ME.stack);
    for iTrl=1:length(ME.stack)
        disp(ME.stack(iTrl).name);
        disp(ME.stack(iTrl).line);
    end
    v.close;
end
end

