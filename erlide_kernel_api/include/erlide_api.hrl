
-record(form, {
               name :: string(),
               text :: binary(),
               ast
              }).
-type form() :: #form{}.

-record(unit, {
               name :: string(),
               kind=erl :: 'erl'|'hrl'|'beam',
               forms=[] :: [form()]
              }).
-type unit() :: #unit{}.

-record(folder, {
                 name :: string(),
                 units=[] :: [unit()]
                }).
-type folder() :: #folder{}.

-record(application, {
                      name :: string(),
                      sources=[] :: [folder()],
                      includes=[] :: [folder()],
                      ebins=[] :: [folder()],
                      tests=[] :: [folder()],
                      priv :: folder(),
                      doc :: folder()
                     }).
-type application():: #application{}.

-record(library, {
                  location,
                  applications=[] :: [application()]
                 }).
-type library() :: #library{}.

-record(project, {
                  name :: string(),
                  otp_version :: string(),
                  applications :: library(),
                  dependencies :: library(),
                  otp :: library()
                 }).
-type project() :: #project{}.

-record(model, {
                projects=[] :: [project()]
               }).
-type model() :: #model{}.
