 
angular.module('forumsApp').component('topic', {
  templateUrl: 'topic.template.html',

  controller: ['$routeParams','forumsApiSrv','breadcrumbSrv', function($routeParams, forumsApiSrv, breadcrumbSrv) {
    var self = this;
    //-----------------------------------------
    // Data
    self.forum = null;
    self.topic = null;
    self.posts = []; // array of posts
                     // post rep.: { user: String, posted: String, message: String, ... }
    self.reversed = true;
    self.openedNewPost = false;
    self.isMod = false;

    //-----------------------------------------
    // Operations

    self.maybeUser = forumsApiSrv.maybeUser;

    self.toggleReversed = function() {
        self.reversed = ! self.reversed;
    };

    self.openNewPost = function() {
        self.openedNewPost = true;
    };

    self.closeNewPost = function() {
        self.openedNewPost = false;
    };

    self.newPost = function(formData) {
        forumsApiSrv.postTopicPosts(self.topic.id, formData).then(
            function() {
                self.closeNewPost();
                reloadPosts();
            }
        );
    };
    
    self.deletePost = function(pid) {
        forumsApiSrv.deletePost(pid).then(
            function() {
                reloadPosts();
            }
        );
    };
    
    function reloadPosts() {
        forumsApiSrv.getTopicPosts($routeParams.topicId).then(
            function(data) {
                self.posts = data.items;
            }
        );
    }

    //-----------------------------------------
    // Initial load
    forumsApiSrv.getTopic($routeParams.topicId).then(
        function(data) {
            self.topic = data;
            forumsApiSrv.getForum(self.topic.forumId).then(
                function(data) {
                    self.forum = data;
                    var user = self.maybeUser();
                    if (user != null) {
                        self.isMod = (user.name == data.moderator);
                    }
                    
                    breadcrumbSrv.set([
                       { label:  'Home', url: '#!/' },
                       { label: self.forum.title, url: '#!/forum-' + self.forum.id },
                       { label: self.topic.title, url: '#!/topic-' + self.topic.id }
                    ]);
                }
            )
        }
    );
    reloadPosts();
  }]
});