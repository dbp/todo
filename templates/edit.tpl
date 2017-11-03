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
    <todo>
      <snooze_till>
        <span class="deadline"><timestamp/><a class="button" href="/todos/${id}/unsnooze?acnt=${account}">X</a></span> 
      </snooze_till>
      <not_snooze_till>
        <a class="button" href="/todos/${id}/snooze?t=D&acnt=${account}">D</a> |
        <a class="button" href="/todos/${id}/snooze?t=W&acnt=${account}">W</a> |
        <a class="button" href="/todos/${id}/snooze?t=M&acnt=${account}">M</a>
      </not_snooze_till>
    </todo>
      <br/>
      <dfInputSubmit value="Update"/>
      <br/>
      <ul>
        <dones>
          <li>done at <timestamp/></li>
        </dones>
      </ul>
  </dfForm>

</apply>
