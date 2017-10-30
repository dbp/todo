<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>

  <dfForm method="POST">
    <dfChildErrorList ref=""/>
    <dfInputTextArea ref="description"/>
    by
    <dfSubView ref="deadline_at">
      <dfLabel ref="date">date:
        <dfInput class="date" ref="date"/>
      </dfLabel>
      <dfLabel ref="time">time:
        <dfInput class="time" ref="time"/>
      </dfLabel>
    </dfSubView>
    <br/>
    <dfInputSubmit value="Update"/>
  </dfForm>

</apply>
