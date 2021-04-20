
angular.module('forumsApp').component('breadcrumb', {
  templateUrl: 'breadcrumb/breadcrumb.template.html',

  controller: ['breadcrumbSrv', function(breadcrumbSrv) {
    var self = this;

    self.getPop = function() {
        return breadcrumbSrv.getPop();
    };
    self.getTop = function() {
        return breadcrumbSrv.getTop();
    };
  }]
});

