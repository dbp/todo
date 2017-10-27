<apply template="_base">

  <dfForm method="POST">
    <dfInputTextArea ref="description"/>
    <dfInputHidden ref="deadline_at"/>

    <br/>
    <dfInputSubmit value="Add"/>
  </dfForm>

  <ul>
    <todos>
      <bind tag="cls"><is-done>done</is-done></bind>
      <li class="${cls}">
        <is-done><description/></is-done>
        <not-done>
          <a href="/todos/${id}/edit?acnt=${account}"><description/></a>
          <deadline><timestamp/></deadline>
          <a onclick="return confirm('Are you sure?');" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
        </not-done>
        <div class="clearfix"></div>
      </li>
    </todos>
  </ul>
</apply>


