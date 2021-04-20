
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    password TEXT NOT NULL,
    isAdmin INTEGER NOT NULL
);

CREATE TABLE forums (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category TEXT NOT NULL, title TEXT NOT NULL, description TEXT NOT NULL, moderatorId INTEGER REFERENCES users(id) NOT NULL,
    created DATE NOT NULL, topicCount INTEGER NOT NULL, postCount INTEGER NOT NULL, lastPostId INTEGER REFERENCES posts(id)
);

CREATE TABLE topics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    forumId INTEGER REFERENCES forums(id) NOT NULL,
    subject TEXT NOT NULL, userId INTEGER REFERENCES users(id) NOT NULL, started DATE NOT NULL, postCount INTEGER NOT NULL,
    firstPostId INTEGER REFERENCES posts(id), lastPostId INTEGER REFERENCES posts(id)
);

CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    topicId INTEGER REFERENCES topics(id) NOT NULL,
    userId INTEGER REFERENCES users(id) NOT NULL, posted DATE NOT NULL, message TEXT NOT NULL
);

INSERT INTO users (id, name, password, isAdmin)
    VALUES (1, 'admin', '1234', 1),
           (2, 'usuari1', '1234', 0),
           (3, 'usuari2', '1234', 0);

INSERT INTO forums (id, category, title, description, moderatorId, created, topicCount, postCount, lastPostId)
  VALUES ( 1, '', 'Fòrum de prova'
         , 'Fòrum inicial en el que plantejar qüestions i respostes de prova.'
         , 1, '2020-11-01 12:00:00'
         , 1, 2, 1
         );

INSERT INTO topics (id, forumId, subject, userId, started, postCount, firstPostId, lastPostId)
  VALUES ( 1, 1, 'Pregunta de prova'
         , 1, '2020-11-01 12:00:00'
         , 2, 1, 2
         );

INSERT INTO posts (id, topicId, userId, posted, message)
  VALUES ( 1, 1, 1, '2020-11-01 12:00:00'
         , 'Missatge de la pregunta de prova.'
         ),
         ( 2, 1, 2, '2020-11-01 12:00:00'
         , 'Missatge de resposta de prova.'
         );

