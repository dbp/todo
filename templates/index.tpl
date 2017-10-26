<html>
  <head>
    <link rel="stylesheet" href="/static/css/main.css"/>
  </head>
  <body>

    <dfForm method="POST">
      <dfInput ref="description"/>
      <dfInputSubmit/>
    </dfForm>

    <ul>
      <todos>
        <bind tag="cls"><is-done>done</is-done></bind>
        <li class="${cls}">
          <is-done><description/></is-done>
          <not-done>
            <span class="description" data-acnt="${account}" data-id="${id}"><description/></span>
            <a onclick="return confirm('Are you sure?');" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
          </not-done>
        </li> 
      </todos>
    </ul>
  </body>
  <script type="text/javascript" src="/static/app.js"></script>
</html>
