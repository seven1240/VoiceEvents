puts "ADDING VOICE EVENTS"
module Telegraph
  class Events

  include ActionController::UrlWriter
  
  def self.add_handler(event, params={})
     host = APP_CONFIG['url'] || "www.lan"
     url = self.new.url_for(params[:url].merge(:host=>host))
     puts url
     event = event.to_s.upcase

     endpoint = EndpointService.new(:url=>url, :event=>event, :app=> "#{APP_CONFIG['app']}_#{params[:url][:controller]}", :domain=>APP_CONFIG["domain"])
     endpoint.save
  end
end
end