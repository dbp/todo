<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>

  <dfForm method="POST">
    <dfChildErrorList ref=""/>
    <dfInputTextArea ref="description"/>
    <br/>
    by
    <dfSubView ref="deadline_at">
      date:
      <dfInput class="small date" ref="date"/>
        
      time:
      <dfInput class="small time" ref="time"/>
      
    </dfSubView>
    repeat: <dfInput class="small" ref="repeat_at"/>
    snooze:
    <a class="button" href="/todos/${id}/snooze?t=D&acnt=${account}">D</a> |
    <a class="button" href="/todos/${id}/snooze?t=W&acnt=${account}">W</a> |
    <a class="button" href="/todos/${id}/snooze?t=M&acnt=${account}">M</a> 
    <br/>
    <dfInputSubmit value="Update"/>
  </dfForm>

</apply>
