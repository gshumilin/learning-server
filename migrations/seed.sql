CREATE TABLE "users" (
"id" SERIAL PRIMARY KEY,
"name" VARCHAR(255),
"login" VARCHAR(30),
"password" VARCHAR(30),
"create_date" TIMESTAMP WITH TIME ZONE,
"is_admin" BOOLEAN,
"is_able_to_create_news" BOOLEAN);

CREATE TABLE "categories" (
"id" SERIAL PRIMARY KEY,
"title" TEXT,
"parent_category_id" INTEGER REFERENCES categories(id));

CREATE TABLE "news" (
"id" SERIAL PRIMARY KEY,
"title" TEXT,
"create_date" TIMESTAMP WITH TIME ZONE,
"creator_id" INTEGER REFERENCES users(id),
"category_id" INTEGER REFERENCES categories(id),
"text_content" TEXT,
"is_published" BOOLEAN);

CREATE TABLE "pictures" (
"id" SERIAL PRIMARY KEY,
"base64" TEXT);

CREATE TABLE "news_pictures" (
"news_id" INTEGER REFERENCES news(id),
"picture_id" INTEGER REFERENCES pictures(id));

INSERT INTO "users" 
("name", "login", "password", "create_date", "is_admin", "is_able_to_create_news")
VALUES
('admin', 'admin', 'pleasedonthackme', CURRENT_TIMESTAMP, True, True);