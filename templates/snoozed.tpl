<apply template="_base">

  <a href="/?acnt=${account}" class="close">&#8592;</a>


  <ul class="todos">
    <todos>
      <li class="done">
        <a class="button" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
        <a class="description" href="/todos/${id}/edit?acnt=${account}"><description/></a>
        <span class="deadline"><snooze_till> snoozed till <timestamp/></snooze_till></span>
        <div class="clearfix"></div>
      </li>
    </todos>
  </ul>


</apply>
