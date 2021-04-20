
angular.module('forumsApp').component('newTopic', {
  templateUrl: 'new-topic.template.html',

  bindings: {
    onSubmit: '&',
    onCancel: '&',
  },

  controller: [function() {
    var self = this;

    //self.model = { title: '', message: '' };

    self.submit = function() {
        self.onSubmit({data: self.model});
        self.model = { };
    }

  }]
});

