module Telegraph
  module ActsAsVoiceChannel #:nodoc:
    def self.included(base)
      base.extend(ClassMethods)
    end

    module ClassMethods
      def has_a_voice_channel
        include Telegraph::ActsAsVoiceChannel::InstanceMethods
      end
      
      def find_by_voice_channel(params)
        model_id = params["variable_app_model_id"] ? params["variable_app_model_id"].to_i : params["app_model_id"].to_i
        return nil unless model_id > 0
        self.find_by_id(model_id)
      end
      
      def get_channel_uuid(params)
        params[:origination_uuid] || params[:variable_bridge_uuid] || params[:channel_uuid] 
        
      end
      
    end

    module InstanceMethods
      
      def create_channel(destination, opts={})
        destination = VoipDestinationService.create(:destination => destination).for_fs unless destination =~ /^(sofia|user)\//
        Telegraph::Model::ChannelBase.create(destination, opts.merge({:origination_uuid=>voice_channel_uuid,:app_model_id=>self.id, :app_model_name=>self.class_name}))
      end
      
      def voice_channel_uuid
        Digest::MD5.hexdigest("#{APP_CONFIG["domain"]}_#{APP_CONFIG["app"]}_#{self.class.to_s}_#{self.id}")
      end
      
      def voice_channel
         Telegraph::Model::ChannelBase.find(voice_channel_uuid)
      end   

      def incoming_voice_channel
        Telegraph::Model::ChannelBase.find(self.uuid)
      end         

      def blind_voice_channel(uuid=nil)
        # find is kind of waste, when we get an event, the channel probably be there
        # Telegraph::Model::ChannelBase.new(:uuid => uuid || self.uuid)

        Telegraph::Model::ChannelBase.new(:uuid => uuid || self.uuid)
      end     
      
      
  
    end
  end
end
        
      
