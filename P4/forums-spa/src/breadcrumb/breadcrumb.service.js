
angular.module('forumsApp').service('breadcrumbSrv', [function() {
    var self = this;

    var breadcrumbs = [];

    self.set = function(bs) {
        breadcrumbs = bs;
    };

    self.push = function(label, url) {
        breadcrumbs.push({label: label, url: url});
    };

    self.getAll = function() {
        return breadcrumbs;
    };

    self.getPop = function() {
        return breadcrumbs.slice(0, breadcrumbs.length - 1);
    };

    self.getTop = function() {
        return breadcrumbs[breadcrumbs.length - 1];
    };

}]);

