var currentQuestion = 0;

function runLab(){
    // easy form submit shortcut - currently broken
    // $(".answerfield textarea").keydown(function(key) {
    //   if (key.which == 13) {
    //     console.dir("Pressed enter");
    //     console.dir($(this).parent("form"));
    //     // $(this).parent("form")[0].submit();
    //   }
    // });
    
    // hide the steps except for the first
    $("[step=" + '1' + "]").hide();
    // and the next button
    $('#nextButtonHolder').hide();
    
    
    // override form submit and submit async with tooltips for displaying the response.
    // currently super hacky
    $(".answerfield").submit(function(event) {
      event.preventDefault();
      
      var childuns = $(this).children("textarea");
      
      childuns.each(function(index) {
        var studentAnswerField = $(this);
        
        // disable the textfield until the result is back        
        studentAnswerField.attr('readonly', 'true').css('background-color', '#DDD');
        // HACK so we can destroy the tooltip the next time the form is submitted
        $('.tooltip_'+ studentAnswerField.attr('id')).qtip('destroy');
        
        var studentResponse = $(this).val();
        
        var datatosend = JSON.stringify({id: "doesntmatter", 
          args: {}, //{pattern:"4"},
          textfields: {somefield: studentResponse}}
        );
        
        // submit the evaluation request
        var studentResponse = studentAnswerField.val();
        var evaluator = studentAnswerField.attr('evaluator') ? studentAnswerField.attr('evaluator') : 'any-c-int';
        $.post('/bountsy/' + evaluator, 
          {request: datatosend},
          function(data){
            // Allow editing of the field again
            studentAnswerField.removeAttr('readonly').css('background-color','#FFF');
            
            // separate var for debugging for now
            var parseddata = JSON.parse(data);
            // HACK
            if (parseddata === null) parseddata = {};
            console.log(studentAnswerField.parents('form')[0]);
            
            
            // HACK
            var toolTipID = "tooltip_" + studentAnswerField.attr('id');
            var parentform = studentAnswerField.parents('form')[0];
            var qTipWithSettings = {
              content: "Success!",
              show: {
                ready: true,
                when: 'never'
              },
              hide: {
                // doesn't currently work - the target is fine but its not grabbing the submit
                target: parentform,
                when: 'submit',
                fixed: true
              },
              position: {
                corner: {
                  target: 'leftMiddle',
                  tooltip: 'rightMiddle'
                }
              },
              style: {
                name: "green",
                tip: "rightMiddle",
                width: {
                  max: 250
                },
                // HACK so we can destroy the tooltip the next time the form is submitted
                classes: {
                  tooltip: toolTipID
                }
              }
            }            

            if (parseddata.status == "success") {
              qTipWithSettings.content = "Success!";
              qTipWithSettings.style.name = "green";
              studentAnswerField.qtip(qTipWithSettings);
              $('#nextButtonHolder').slideDown();
            } else {
              qTipWithSettings.content = parseddata.message;
              qTipWithSettings.style.name = "red";
              studentAnswerField.qtip(qTipWithSettings);
            }
        });
      });
      
      // Stop the form from submitting normally
      return false;
    });

    $("#nextButton").click(function(event){
      // hide button
      $("#nextButtonHolder").hide();
      // destroy all tooltips - really need a better way to do tooltips
      $('[class *= tooltip]').each(function(index) {
        var item = $(this);
        item.qtip('destroy');
      });
      
      
      // animate in the next part
      $("[step=" + currentQuestion + "]").fadeOut(function() {
        // superhack!!!!!! -- should be currentQuestion++ but there are only 2 atm
        currentQuestion = 1;
        $("[step=" + currentQuestion + "]").fadeIn();        
      });
    });

}
