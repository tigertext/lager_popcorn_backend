Enables lager to publish live logging data to popcorn.

### Configuration

In the app.config add a lager_popcorn_backend entry to the lager config.

Hostname is determined at runtime.

The app version number should be passed in so log messages on bootup are recorded correctly.

Example:

```
{lager, [
         {handlers, [
            {lager_console_backend, none},
            {lager_file_backend,
             [
              {"log/error.log", error, 104857600, "$D0", 5},
             ]},
             {lager_popcorn_backend, [
                 {level,            debug},
                 {popcorn_host,     "hostname"},
                 {popcorn_port,     9125},
                 {node_role,        "webserver"},
                 {node_version,     "0.0.1"}
             ]}
  ]}
 ]},
```
