class VoipDialstringService < ActiveResource::Base
  # self.site = "http://a.veecue.com/fs_control"
  self.site = :fs_control
    
    # @returns a valid FreeSWITCH dial string
    # pstn://CN/86/0108260xxxx
    # skype://CN/skype_name
    # pstn://CN/skype/skype_name
    # gtalk://CN/gtalk_name
    # voip://US/eqenglish/trainer39-55
    # sip3p://CN/1000@yt.idapted.com
    # sip3p://CN.yt.idapted.com/1000 ?
    
  def self.create(number, args = {})
    
    region_code = args[:code] || '86' 
    type = args[:type] || 'pstn'
    iso3166_code = args[:code] || 'CN'
    
    number.gsub!(/[()]/, '')  #let's remove () from numbers since it's invalid URI
    
    return self.new({:dialstring => "error/NO_ROUTE_DESTINATION"}) if type != "pstn" && iso3166_code !~ /^[A-Z]/
    
    if type == 'pstn'
      uri = "#{type}://CN/#{region_code}/#{number}"
    else
      uri = "#{type}://#{iso3166_code}/#{number}"
    end
    begin
	super(:uri => uri)
    rescue
	return self.new({:dialstring => "error/NETWORK_OUT_OF_ORDER"})
    end
  end
  def self.create_hash_from_user(user)
    begin #hack here
      region_id = DomainRegionService.find(:all,:params=>{ :domain => APP_CONFIG["domain"] }).find{|i| i.region.id == user.region_id.to_i}.region.code
    rescue
      region_id = 1
    end
    {:connect_type => user.connect_type, :gtalk=>user.gtalk, :skype=>user.skype, :region_id => region_id, :pstn => user.connect_number || user.mobile || user.home_phone}  

  end
  
  def self.create_from_form(params)
    code = nil
    type = params[:connect_type]
    if type == 'pstn' || type.nil?
      number = params[:pstn]
      #This is definitely not the best way to do this..
      #code = params[:region_id] #DomainRegionService.find(:all,:params=>{ :domain=>"eqenglish"}).find{|i| i.region.code == params[:region_id].to_i}.region.code
      code = DomainRegionService.find(:all,:params=>{ :domain=>"eqenglish"}).find{|i| i.region.id == params[:region_id].to_i}.region.code
      formatted_number = "+#{code}#{number}"
    elsif type == 'skype'
      number = params[:skype]
      formatted_number = "skype:#{number}"
    else
      number = params[:gtalk]
      formatted_number = "gtalk:#{number}"
    end
    
    return self.new({:dialstring=>self.create(number, :type=>params[:connect_type], :code=>code).dialstring, :formatted_number => formatted_number})
      
  end
end
