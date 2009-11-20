class DomainRegionService < ActiveResource::Base
 self.site= :system
 self.site= "#{self.site}/:domain"
 self.format= :json
end