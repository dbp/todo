<apply template="_base">

  <dfForm class="create" method="POST">
    <dfInputTextArea ref="description" placeholder="Write something here..."/>
    <dfInputHidden ref="deadline_at"/>
    <dfInputSubmit value="+"/>
  </dfForm>

  <ul class="todos">
    <todos>
      <bind tag="cls"><is-done>done</is-done></bind>
      <li class="${cls}">
        <is-done>
          <a class="button" href="/todos/${id}/undone?acnt=${account}">&#10008;</a>
          <description/>
        </is-done>
        <not-done>
          <a class="button" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
          <a href="/todos/${id}/edit?acnt=${account}"><description/></a>
          <deadline><timestamp/></deadline>
        </not-done>
        <div class="clearfix"></div>
      </li>
    </todos>
  </ul>
</apply>


