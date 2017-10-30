<apply template="_base">

  <dfForm class="create" method="POST">
    <dfInputTextArea ref="description" placeholder="Write something here..."/>
    <dfSubView ref="deadline_at">
      <dfInputHidden ref="date"/>
      <dfInputHidden ref="time"/>
    </dfSubView>
    <button type="submit">+</button>
  </dfForm>

  <ul class="todos">
    <todos>
      <bind tag="cls"><is-done>done</is-done></bind>
      <li class="${cls}">
        <is-done>
          <a class="button" href="/todos/${id}/undone?acnt=${account}">&#10008;</a>
          <span class="description">
            <description/>
          </span>
        </is-done>
        <not-done>
          <a class="button" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
          <a class="description" href="/todos/${id}/edit?acnt=${account}"><description/></a>
          <deadline><span class="deadline"><timestamp/></span></deadline>
        </not-done>
        <div class="clearfix"></div>
      </li>
    </todos>
  </ul>
</apply>
