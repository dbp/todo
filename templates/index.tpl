<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1, minimum-scale=1">
    <link rel="stylesheet" href="/static/css/main.css"/>
  </head>
  <body>

    <dfForm method="POST" class="create">
      <dfInputTextArea ref="description"/>
      <dfInputHidden class="when" ref="when" value="0"/>
      <br/>
      <dfInputSubmit value="Any" data-when="0"/>
      <dfInputSubmit value="1D" data-when="1"/>
      <dfInputSubmit value="1W" data-when="7"/>
      <dfInputSubmit value="1M" data-when="30"/>
      <dfInputSubmit value="1Y" data-when="365"/>
    </dfForm>

    <ul>
      <todos>
        <bind tag="cls"><is-done>done</is-done></bind>
        <li class="${cls}">
          <is-done><description/></is-done>
          <not-done>
            <span class="description" data-acnt="${account}" data-id="${id}"><description/></span>
            <deadline><timestamp/></deadline>
            <a onclick="return confirm('Are you sure?');" href="/todos/${id}/done?acnt=${account}">&#10004;</a>
          </not-done>
        </li> 
      </todos>
    </ul>
  </body>
  <script type="text/javascript" src="/static/app.js"></script>
</html>



