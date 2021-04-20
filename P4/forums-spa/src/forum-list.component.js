
angular.module('forumsApp').component('forumList', {
  templateUrl: 'forum-list.template.html',

  controller: ['forumsApiSrv','breadcrumbSrv', function(forumsApiSrv, breadcrumbSrv) {
    var self = this;
    //-----------------------------------------
    // Data
    self.forums = [];
    self.openedNewForum = false;
  
    //-----------------------------------------
    // Operations
    
    self.maybeUser = forumsApiSrv.maybeUser;
    
    self.openNewForum = function() {
        self.openedNewForum = true;
    };

    self.closeNewForum = function() {
        self.openedNewForum = false;
    };

    self.newForum = function(formData) {
        forumsApiSrv.postForums(formData).then(
            function() {
                self.closeNewForum();
                reloadForums();
            }
        );
    };
    
    self.deleteForum = function(fid) {
        forumsApiSrv.deleteForum(fid).then(
            function() {
                reloadForums();
            }
        );
    };

    function reloadForums() {
        forumsApiSrv.getForums().then(function(data) {
            self.forums = data.items;
        });
    };

    //-----------------------------------------
    // Initial load
    reloadForums();
    
    breadcrumbSrv.set([
        { label:  'Home', url: '#!/' }
    ]);
  }]
});

