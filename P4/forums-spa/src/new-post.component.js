
angular.module('forumsApp').component('newPost', {
  templateUrl: 'new-post.template.html',

  bindings: {
    onSubmit: '&',
    onCancel: '&',
  },

  controller: [function() {
    var self = this;

    //self.model = { message: '' };

    self.submit = function() {
        self.onSubmit({data: self.model});
        self.model = { };
    }

  }]
});
