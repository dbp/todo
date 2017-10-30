<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>

  <dfForm method="POST">
    <dfChildErrorList ref=""/>
    <dfInputTextArea ref="description"/>
    Deadline:
    <dfSubView ref="deadline_at">
      <dfInput class="date" ref="date"/>
      <dfInput class="time" ref="time"/>
    </dfSubView>
    <br/>
    <dfInputSubmit value="Update"/>
  </dfForm>

</apply>
