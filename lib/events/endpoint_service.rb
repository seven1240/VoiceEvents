class EndpointService < ActiveResource::Base
  self.site = APP_CONFIG["erlang_web_server"] || "http://voip1.lan:8000"
  self.format = :json
end
