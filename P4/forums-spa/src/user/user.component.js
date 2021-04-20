
angular.module('forumsApp').component('user', {
  templateUrl: 'user/user.template.html',

  controller: ['forumsApiSrv', function(forumsApiSrv) {
    var self = this;

    self.maybeUser = forumsApiSrv.maybeUser;

    self.login = function() {
        forumsApiSrv.getUser();
    };
  }]
});

