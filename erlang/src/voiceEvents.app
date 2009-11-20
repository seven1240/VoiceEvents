{application, voiceEvents,
 [{description, "voiceEvents"},
  {vsn, "0.01"},
  {modules, [
    voiceEvents,
    voiceEvents_app,
    voiceEvents_sup,
    voiceEvents_web,
    voiceEvents_deps
  ]},
  {registered, []},
  {mod, {voiceEvents_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
