#Mime::Type.register "telegraph/voice", :voice

telegraph_config_file = File.join(RAILS_ROOT || '', 'config/telegraph.yml' )
telegraph_config_file = File.readable?(telegraph_config_file) ? telegraph_config_file : File.join(File.dirname(__FILE__), 'telegraph.yml')

TELEGRAPH_CONFIG = YAML.load_file(telegraph_config_file)[Rails.env]

module Telegraph
  def self.config
    TELEGRAPH_CONFIG
  end
  def self.logger 
    RAILS_DEFAULT_LOGGER
  end
  
  def self.log(txt)
    puts "#{Time.now.utc.to_s(:long)}: #{txt}\n"
  end
end

require 'events/endpoint_service'
require 'events/voice_events'


require 'model/voip_destination'
require 'model/voip_dialstring'
require 'model/domain_region_service'

require 'model/voice_connector'
require 'model/voice_channel_model'
require 'model/voice_sip_model'
require 'model/voice_conference_model'

require 'model/rails_channel'

require 'helpers/view_helpers'

ActionView::Base.send(:include, Telegraph::ViewHelpers)
ActionController::Base.send(:include, Telegraph::Dialer)


Telegraph::Model::Connector.load_configuration_and_connect!

ActiveRecord::Base.send(:include, Telegraph::ActsAsVoiceChannel)

