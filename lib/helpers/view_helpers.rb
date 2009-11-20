module Telegraph
  module ViewHelpers
    def dialer_form(name, pars = nil)
        pars = VoipDialstringService.create_hash_from_user(current_user) if pars.nil?

#      @regions = []
       re = %{<div width="300px">
       	<div style="width:100px; float:left">
       	Use:<br/>
         #{select_tag "#{name}[connect_type]", options_for_select([['Phone', :pstn],['Skype', 'skype'], ['Gtalk', 'gtalk']], pars[:connect_type]), :onchange=>"#{name}ChangeConnectType()"}
       	</div>
       	<div id="#{name}_skype" style="display: none">
       		Skype Id:<br/>
       		 #{text_field_tag "#{name}[skype]", pars[:skype]}
       	</div>
       	<div id="#{name}_gtalk" style="display: none">
       		Gtalk Username:<br/>
       		#{text_field_tag "#{name}[gtalk]", pars[:gtalk]}
       	</div>
       	<div id="#{name}_pstn" style="display: none">
       		Phone Number:<br/>
       	</div>
          #{pstn_select(name, pars)}
       	</div>

       	<script>
       		#{name}ConnectType = "#{name}_#{pars[:connect_type]}";
       		$(#{name}ConnectType).show();
       		function #{name}ChangeConnectType(){
       			$(#{name}ConnectType).hide();
       			#{name}ConnectType = '#{name}_' + $('#{name}_connect_type').value;
       			$(#{name}ConnectType).show();
       		}

       	</script>}
       end
       
       def pstn_select(name, pars = nil)
         pars = VoipDialstringService.create_hash_from_user(current_user) if pars.nil?
         
         @regions = DomainRegionService.find(:all,:params=>{ :domain=>"eqenglish"})
         	"#{select_tag name + "[region_id]", options_for_select(@regions.map{|i| ["#{i.region.name}(+#{i.region.code})",i.region.id]}, pars[:region_id])} - #{text_field_tag "#{name}[pstn]", pars[:pstn], :size=>10}"
      	end
  end
end


module Telegraph
  module Dialer #:nodoc:
    def self.included(base)
      base.send(:include,InstanceMethods)
    end
    
    module InstanceMethods
          ## This probably shouldn't be here.  Need to decide if there is a better place
          def update_user_by_dial_params(user, params)
            user.connect_type = params[:connect_type]
            if params[:connect_type] == "skype"
              user.skype = params[:skype]
             elsif params[:connect_type] == "gtalk"
               user.gtalk = params[:gtalk]
             else
               user.connect_number = params[:pstn]
               user.region_id = params[:region_id]
             end
             user.save
          end
      end
  end
end


      


    
    
