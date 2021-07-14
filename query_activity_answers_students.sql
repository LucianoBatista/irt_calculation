select qn.examId, users.systemUserId, group_concat(ifnull(sutr.grade,0) order by qn.number) results
 from (
      select  examId, questionId, number
		from  studos.exam 
		join  studos.exam_question eq on
              (     eq.examId  = exam.id 
		        and eq.enabled = 1
		        and eq.deleted = 0
              )
		where 1 = 1 
		    [[ AND  {{examId}} ]] 
		 order by number
       ) qn 		
 join (  select distinct sutr.systemUserId, sutr.examId
           from studos.exam 
           join studos.system_user_training_result sutr on sutr.examId = exam.id
           where  1 = 1
		       [[ AND  {{examId}} ]] 
	  ) users on users.examId = qn.examId
 left join studos.system_user_training_result sutr on 
     (       sutr.examId = qn.examId 
		and  sutr.systemUserId = users.systemUserId
        and  sutr.questionId = qn.questionId
        and  sutr.enabled    = 1
        and  sutr.deleted    = 0
	 ) 
 group by qn.examId, users.systemUserId
 order by qn.examId, users.systemUserId, qn.number
 ;