module Telegraph
  module Model
    class SipBase
      attr_accessor :profile
      attr_accessor :gateway
      attr_accessor :params
      def initialize
        @@connector ||= Connector.new
        @params = {}
      end
      
      def self.find_by_gateway(gateway)
        klass = self.new
        klass.gateway = gateway
        klass.profile = nil
        klass
      end
      
      def self.find_by_profile(profile)
        klass = self.new
        klass.profile = profile
        klass.gateway = nil
        klass
      end
      
      def self.find_by_username(username, profile="default")
          registrations = self.find_all
          registrations.detect{|r| r.params[:user] =~ /^#{username}\@/}
      end
      
      def self.find_all(profile="default")
        @@connector ||=VoiceConnector.new
        @@data_cache ||= Hash.new
        if @@data_cache[:set_at].nil? or @@data_cache[:set_at] < 3.second.ago.utc
          data = @@connector.send_command("sofia", "status profile #{SiteConfig['global']['domain']}")
          @@data_cache[:data] = data
          @@data_cache[:set_at] = Time.now.utc
        else
          data = @@data_cache[:data]
        end
        
        registrations = data.split("Registrations:\n")[1]
        registrations = registrations.split("\n\n")
        registrations = registrations.find_all{|r| r.split("\n").size > 2} 
        registrations = registrations.map do |r| 
          klass = self.new
          klass.profile = profile
          regs = r.split("\n")
          regs.each do |p|
            p = p.split(" \t")
            klass.params[p[0].gsub(' ', '').downcase.gsub('-', '_').to_sym] = p[1] if p.size > 1
          end
          klass
        end
        return registrations
      end
      
      def flush
        re= @@connector.send_command("sofia", "profile #{profile} flush_inbound_reg #{@params[:call_id]}")
        @params = {} if re=~ /OK/
      end
      
      def restart_and_reload
        @@connector.send_command("sofia", "profile #{profile} restart all reloadxml")
      end
      
      def status
        if @gateway
          @@connector.send_command("sofia", "status gateway #{@gateway}")
        elsif
          @@connector.send_command("sofia", "status profile #{profile}")
        end
      end
      
      def self.status
        @@connector ||= Connector.new
        @@connector.send_command("sofia", "status")
      end
    end
  end
end
        
      
      
