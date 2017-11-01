<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>

  <ul class="todos">
    <todos>
      <bind tag="cls"><is-done>done</is-done></bind>
      <li class="${cls}">
        <a class="button" href="/todos/${id}/undone?acnt=${account}">&#10008;</a>
        <span class="description">
          <description/>
        </span>
        <done_at><span class="deadline">done at <timestamp/></span></done_at>
      </li>
    </todos>
  </ul>

</apply>
