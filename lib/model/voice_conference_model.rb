module Telegraph
  module Model
    class ConferenceBase
      attr_accessor :name
      attr_accessor :members
      @name = nil
      @members = nil
      @@connector = nil
      def initialize(xml, blank=false)
        @@connector ||= Connector.new
        @members = VoiceConferenceMemberCollection.new

        if blank
          @name = xml
        else
          @name = xml.attributes['name']
          xml.elements.each('members/member') do |member|
            @members << VoiceConferenceMember.new(member, @name, @@connector)
          end
        end
      end
    #  d.elements.each('conferences/conference'){|i| i.elements.each('members/member'){|m| puts m.elements['flags'].elements['talking'].text}}
    
      def start_recording(filename)
        @@connector.send_command('conference', "#{@name} record #{filename}")
      end

      def stop_recording(filename)
        @@connector.send_command('conference', "#{@name} norecord #{filename}")
      end
      
      def play(filename)
        @@connector.send_command('conference', "#{@name} play #{filename}")
      end
      
      def stop(arg='all')  # <[current|all|async|last]>
        @@connector.send_command('conference', "#{@name} stop #{arg}")
      end
      
      def dial_in(path)
        @@connector.send_command('conference', "bgdial #{path}")
      end
      
      def self.find(id)
         @@connector ||= Connector.new

          if id == :all
            data = @@connector.send_command('conference', 'xml_list')
          else
            data = @@connector.send_command('conference', "#{id} xml_list")
          end
          xml =REXML::Document.new(data)
          
          conferences = []
          
          xml.elements.each('conferences/conference'){|conf|
            conferences << self.new(conf)
          }
          if id == :all
            conferences
          else
            conferences.first.nil? ? self.new(id.to_s, true) : conferences.first
          end
      end
    end
    
    class VoiceConferenceMemberCollection < Array
      def find(id)
        self.detect{|o| o.id.to_s == id.to_s}
      end
    end
    class VoiceConferenceMember
      attr_accessor :id, :channel, :uuid, :deaf, :mute, :volume_in, :volume_out, :energy, :conf_name, :cid_name, :cid_number
      
      def initialize(data, conf_name, voice_connector)
  
        @id = data.elements['id'].text
        @uuid = data.elements['uuid'].text
        @mute = data.elements['flags'].elements['can_speak'].text == "false"
        @deaf = data.elements['flags'].elements['can_hear'].text == "false"
        @cid_name = data.elements['caller_id_name'].text
        @cid_number = data.elements['caller_id_number'].text

        @conf_name = conf_name
        @connector = voice_connector
      end
      
      def mute?
        @mute
      end
      
      def deaf?
        @deaf
      end
      
      def update_attributes(params)
        puts "UPDATE"
        pp params
        params.each do |k,v|
          puts "param"
          puts k.to_sym
          if [:mute, :deaf].include?(k.to_sym)
            tru = (v and not v == "0")
            prefix = tru ? '' : 'un'
            unless tru == self.send(k.to_sym)
              if @connector.send_command('conference', "#{conf_name} #{prefix}#{k.to_s} #{id}")
                self.send("#{k.to_s}=".to_sym, v)
              end
            end
          elsif [:volume_in, :volume_out, :energy].include?(k.to_sym)
            puts "in valume"
            if @connector.send_command('conference', "#{conf_name} #{k.to_s} #{id} #{v}")
              self.send("#{k}=", v)
            end
          end
        end
      end
            
    end
  end
end