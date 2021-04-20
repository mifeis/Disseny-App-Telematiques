
angular.module('forumsApp').service('oauth2Srv',
  ['config','$window','$rootScope','$q', function(config, $window, $rootScope, $q) {
        var self = this;

        /*
         * Helper function to build the Authorization URL
         *
         * @param   scopes {array}  An array of all scopes needed (optional)
         * @param   state  {string} The state string (optional)
         *
         * @return   {string}  The authorization URL
         */
        function buildAuthorizationUrl(scopes, state) {
            var scopeL = convertScopeArrayToList(scopes);
            var url1 = config.serverSettings.authorizationEndpoint
	        + '?response_type=token&scope=' + scopeL + '&client_id=' + config.clientSettings.client_id
	        + '&redirect_uri=' + config.clientSettings.callback_url;
	    if (state) {
	        return url1 + '&state=' + state;
	    } else {
	        return url1;
	    }
        }

        function convertScopeArrayToList(scopes) {
                var scopeList = '';
                if (scopes) {
                    if (scopes.length > 1) {
                        scopeList = scopes.join(' ');
                    } else {
                        scopeList = scopes[0];
                    }
                }
                return scopeList;
        }

        /*
         * Helper function to build and open auth url in a new window
         * and receive the access token from the callback script
         *
         * @param  scopes {array}  An array of all scopes needed (optional)
         * @param  state  {string} The state string (optional)
         * @param  popup  {bool}   Bool to indicate if auth url should be opened
         *                         in a new window or a popup. True if popup required
         *
         * @return    {Promise}  The resolved or rejected Promise containing the
         *                       access token or error message
         */
        function doGetToken(scopes, state, popup) {
            var url = buildAuthorizationUrl(scopes, state);
            if(popup) {
                $window.open(url, 'Authorize', 'width=650, height=550');
            } else {
                $window.open(url);
            }

            var deferred = $q.defer();

            angular.element($window).bind('message', function(event) {
                // Use JQuery originalEvent if present
                event = event.originalEvent || event;
                if (event.origin === $window.location.origin) {
                  $rootScope.$apply(function() {
                    if (event.data.access_token) {
                        deferred.resolve(event.data);
                    } else {
                        deferred.reject(event.data);
                    }
                  });
                }
            });

            return deferred.promise;
        }

        var token_response = null;
        var token_expiry = null;

        /*
         * Calls the function to get the access token in a new window
         * and stores it in the cache
         *
         * @param  scopes {array}   An array of all scopes needed (optional)
         * @param  state  {string}  The state string (optional)
         * @param  popup  {bool}    Bool to indicate if auth url should be opened
         *                          in a new window or a popup. True if popup required
         *
         * @return    {Promise}  The resolved or rejected Promise containing the
         *                       access token or error message
         */
        self.getToken = function(scopes, state, popup) {
            var atoken = self.checkToken();
            if (atoken) {
                return $q.resolve(atoken);
            }
            return doGetToken(scopes, state, popup).then(
                function(tokres) {
                    token_response = tokres;
                    if (token_response.expires_in) {
                        var now = Date.now() / 1000; // number of seconds elapsed since January 1, 1970 00:00:00 UTC
                        token_expiry = now + token_response.expires_in;
                    }
                    return tokres.access_token;
                }
            );
        };

        /*
         * Checks if access token is saved and valid
         *
         * @return    {string}  The access token or null
         */
        self.checkToken = function() {
            var now = Date.now() / 1000; // number of seconds elapsed since January 1, 1970 00:00:00 UTC
            if (token_expiry && now <= token_expiry) {
                // Invalidate token cache
                self.clearToken();
            }
            return token_response ? token_response.access_token : null;
        };

        /*
         * Invalidates token cache
         */
        self.clearToken = function() {
            token_response = null;
            token_expiry = null;
        };

  }]
);

