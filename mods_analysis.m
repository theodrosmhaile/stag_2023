
% retrieve responses and span digits

function  [acc_serial_position, acc_total_span] = mods_analysis(Record)
% ------------ serial position
acc_serial_pos = nan(32,6);
acc_total      = nan(32,4);
for i=1:length(Record)
    %results.span_length(i) = length(Record(i).span);
    span_len_temp     = length(Record(i).span);
    response_len_temp = length(Record(i).response);
    
    span_temp = mat2str(Record(i).span);
    this_span = span_temp(regexp(mat2str(Record(i).span), '[0-9]'));
    
    %this_regexp(mat2str(Record(3).span), '[0-9]');
    if span_len_temp == response_len_temp
        
        this_resp = Record(i).response;
        
        
    else
        % response was too short, repair by adding a dash and hope for the
        % best
        this_resp = strcat(Record(i).response, ...
            repmat('-',1,abs(span_len_temp - response_len_temp)));
        
    end
    acc_serial_pos(i, 1:span_len_temp) = this_span == this_resp;
    
    % span_temp = mat2str(Record(i).span);
    
    % span_temp(regexp(span_temp), '[0-9]')
    
    
    if all(this_span == this_resp)
    acc_total(i,span_len_temp-2) = 1;
    
    else
    acc_total(i,span_len_temp-2) = 0;    
    end
    
end

acc_serial_position = mean(acc_serial_pos, 'omitnan');
acc_total_span      = mean(acc_total,'omitnan');

plot(1:6,acc_serial_position, ...
    '--*', ...
    'LineWidth', 2,...
    'MarkerSize',15, ...
    'MarkerFaceColor','red')
ylim([0 1])
xlabel('span positions')
ylabel('accuracy')
title(['subject ', num2str(Record(1).subject_ID) ])





end


