<apply template="_base">

  <a href="/?acnt=${account}" class="close">X</a>

  <dfForm method="POST">
    <dfChildErrorList ref=""/>
    <dfInputTextArea ref="description"/>
    <dfInput class="date" ref="deadline_at"/>
    <br/>
    <dfInputSubmit value="Update"/>
  </dfForm>

</apply>
