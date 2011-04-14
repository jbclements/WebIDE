function runLab(){
    // all the logic that should go in script.js once that's available...
    $(".answerfield").submit(function(event) {
      event.preventDefault();
      
      var childuns = $(this).children("textarea");
      
      childuns.each(function(index) {
        var kid = $(this);
        
        var studentResponse = $(this).val();
        
        console.log(studentResponse);
        
        var datatosend = JSON.stringify({id: "doesntmatter", 
          args: {}, //{pattern:"4"},
          textfields: {somefield: studentResponse}}
        );
        
        // submit the evaluation request
        var studentResponse = kid.val();
        $.post('/bountsy/any-c-int', 
          {request: datatosend},
          function(data){
            var parseddata = JSON.parse(data);
            // HACK
            if (parseddata === null) data = {};
            
            if (parseddata.status == "success") {
              kid.qtip({content:"Success!", show: { ready: true, when: 'never' }, hide: { when: 'never', fixed: true }, position: {corner: { target: 'rightMiddle', tooltip: 'leftMiddle'}}, style: {name: "green", tip: "leftMiddle", width: { max: 400 }}});
            } else {
              kid.qtip({content:parseddata.message, show: { ready: true, when: 'never' }, hide: { when: 'never', fixed: true }, position: {corner: { target: 'rightMiddle', tooltip: 'leftMiddle'}}, style: {name: "red", tip: "leftMiddle", width: { max: 400 }}});
            }
        });
      });
      
      // Stop the form from submitting normally
      return false;
    });
}
