<apply template="_base">

  <dfForm class="create" method="POST">
    <dfInputTextArea ref="description" placeholder="Write something here..."/>
    <dfSubView ref="deadline_at">
      <dfInputHidden ref="date"/>
      <dfInputHidden ref="time"/>
    </dfSubView>
    <dfInputHidden ref="repeat_at"/>
    <button type="submit">+</button>
  </dfForm>

  <ul class="todos">
    <todos>
      <bind tag="cls"><is-done>done</is-done></bind>
      <li class="${cls}">
        <a class="button" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
        <a class="description" href="/todos/${id}/edit?acnt=${account}"><description/></a>
        <deadline><span class="deadline"><timestamp/><repeat_at> then every <interval/></repeat></span></deadline>
        <div class="clearfix"></div>
      </li>
    </todos>
  </ul>

  <a class="done" href="/snoozed?acnt=${account}">View Snoozed</a>

  <a class="done" href="/archive?acnt=${account}">View Archive</a>
</apply>
