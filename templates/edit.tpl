<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>

  <dfForm method="POST">
    <dfChildErrorList ref=""/>
    <dfInputTextArea ref="description"/>
    Deadline: <dfInput class="date" ref="deadline_at"/>
    <br/>
    <dfInputSubmit value="Update"/>
  </dfForm>

</apply>
