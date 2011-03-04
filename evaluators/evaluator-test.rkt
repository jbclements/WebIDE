#lang racket

(require (planet dherman/json:3:0)
         net/url)

(define sample-sample 
 (string->bytes/utf-8 "request=%7B%22id%22%3A%22Facebook%3A+Selection%22%2C%22args%22%3A%7B%22functionCall%22%3A%22assignGroup%2818%29%22%2C%22fmessage%22%3A%22I%27m+sorry%2C+your+if+statement+for+group+C+is+incorrect.%22%2C%22expectedOutput%22%3A%22%27C%27%22%2C%22smessage%22%3A%22Good+Job%21%22%2C%22function%22%3A%22%5Cn++++++++++++++public+char+assignGroup%28int+age%29+%7B%5Cn++++++++++++++++char+group+%3D+%27x%27%3B%5Cn++++++++++++++++%40groupC%5Cn++++++++++++++++return+group%3B%5Cn++++++++++++++%7D%5Cn++++++++++%22%7D%2C%22textfields%22%3A%7B%22groupC%22%3A%22abba+zabba%22%7D%7D%0A"))

(define sample-json
  "{\"id\":\"Facebook: Selection\",\"args\":{\"functionCall\":\"assignGroup(18)\",\"fmessage\":\"I'm sorry, your if statement for group C is incorrect.\",\"expectedOutput\":\"'C'\",\"smessage\":\"Good Job!\",\"function\":\"\\n              public char assignGroup(int age) {\\n                char group = 'x';\\n                @groupC\\n                return group;\\n              }\\n          \"},\"textfields\":{\"groupC\":\"abba zabba\"}}\n"

  #;(jsexpr->json 
   #hasheq((id . "Facebook: Selection")
           (args . #hasheq((functionCall . "assignGroup(18)")
                           (fmessage . "I'm sorry, your if statement for group C is incorrect.")
                           (expectedOutput . "'C'")
                           (smessage . "Good Job!")
                           (function . "public char assignGroup(int age) { char group = 'x'; @groupC return group;} ")))
           (textfields . #hasheq((groupC . "abba zabba"))))))

sample-json

(define (path->path/params los)
  (map (lambda (s) (path/param s '())) los))

(define the-url
  (url "http" #f "184.73.238.21" #f #t 
       (path->path/params '("webide""evaluators""JavaEvaluator" "JavaEvaluator.php")) 
       `()
       #f))

(url->string the-url)

(define port (post-impure-port the-url sample-sample
                               '("Content-Type: application/x-www-form-urlencoded")))
(regexp-match #px".*" port)
(close-input-port port)

