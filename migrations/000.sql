CREATE TABLE IF NOT EXISTS "users" (
"id" SERIAL PRIMARY KEY,
"name" VARCHAR(255) NOT NULL,
"login" VARCHAR(30) NOT NULL UNIQUE,
"password" VARCHAR(30) NOT NULL,
"create_date" TIMESTAMP WITH TIME ZONE NOT NULL,
"is_admin" BOOLEAN NOT NULL,
"is_able_to_create_news" BOOLEAN NOT NULL);

CREATE TABLE IF NOT EXISTS "categories" (
"id" SERIAL PRIMARY KEY,
"title" TEXT NOT NULL UNIQUE,
"parent_category_id" INTEGER REFERENCES categories(id));

CREATE TABLE IF NOT EXISTS "news" (
"id" SERIAL PRIMARY KEY,
"title" TEXT NOT NULL UNIQUE,
"create_date" TIMESTAMP WITH TIME ZONE,
"creator_id" INTEGER REFERENCES users(id),
"category_id" INTEGER REFERENCES categories(id),
"text_content" TEXT,
"is_published" BOOLEAN);

CREATE TABLE IF NOT EXISTS "pictures" (
"id" SERIAL PRIMARY KEY,
"base64" TEXT);

CREATE TABLE IF NOT EXISTS "news_pictures" (
"news_id" INTEGER REFERENCES news(id),
"picture_id" INTEGER REFERENCES pictures(id));
