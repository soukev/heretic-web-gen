
module GenHF where

gHeader :: String -> String
gHeader title = "<!doctype html>\
\\n<html class=\"no-js\" lang=\"en\">\
\\n     <head>\
\\n         <meta charset=\"utf-8\">\
\\n         <meta http-equiv=\"x-ua-compatible\" content=\"ie=edge\">\
\\n         <title>" ++ title ++ "</title>\
\\n        <meta name=\"description\" content=\"\">\
\\n        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\
\\n          <link rel=\"stylesheet\" href=\"css/style.css\">\
\\n    </head>\
\\n    <body>\
\\n    <div class=\"wrap\">"

gFooter :: String
gFooter = "    </div>\n    </body>\n</html>\n"

gCSS :: String
gCSS = "body{\
\\n     font-family: serif;\
\\n     font-size: 1.2em;\
\\n}\
\\n\
\\nh1, h2, h3 {\
\\n     font-family: sans-serif;\
\\n}\
\\nh1 {\
\\n     font-size: 2em;\
\\n}\
\\n\
\\np {\
\\n     padding-top: .5em;\
\\n     line-height: 1.6;\
\\n     text-align: left;\
\\n}\
\\n\
\\nimg {\
\\n     max-width: 90%;\
\\n     text-align: center;\
\\n}\
\\n.articles {\
\\n     padding-top: 2em;\
\\n     line-height: 1.6;\
\\n}\
\\n.articlelink{\
\\n     font-family: sans-serif;\
\\n     font-weight: lighter;\
\\n     letter-spacing: .05em;\
\\n     text-underline-offset: .3em;\
\\n     font-size: 1.4em;\
\\n}\
\\na{\
\\n     color: inherit;\
\\n}\
\\n.date{\
\\n     font-family: sans-serif;\
\\n     font-size: .8em;\
\\n}\
\\n\
\\n@media (min-width: 42em) {\
\\nbody > .wrap > article, body > .wrap > .articles {\
\\n     width: 42em;\
\\n     margin: 0 auto;\
\\n}}"
