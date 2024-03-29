# learning-server
This is a learning web news server project with a REST API that accepts HTTP requests and responds in JSON format. You can imagine that this is a server for a mobile application.
# Deployment
1. Clone the repository;
2. Create a database. For simplicity, you can use database info from the `config-TEMPLATE.json` file;
3. Fill in the `config-TEMPLATE.json` file and **change it's name to `config.json`**. Pay special attention to database info: host, port, user name, password, database name. Values of these fields must match the values you specified when you created your database. `logDescType` field accepts either "stderr", "stdout" or file-name/file-path
4. It's recommended to start server for the first time with the `stack run m f` command from the terminal opened in the project folder. Command has two flags:

    `m` - create `schema_migrations` table in your database and run migrations;

    `f` - run fixtures. Function create for you admin user;
    
Server is usually started by the `stack run` command.

# Testing
If you ran `fixtures.sql`, then you have the opportunity to test the server using files in the `.test-requests`. File names match to name of the endpoint. Files come in pairs: one with a .sh extension and one with a .json extension. They contain curl scripts and the request body necessary for executing, respectively.
Start the server, open a terminal in the `.test-requests` folder and make requests to server with command “bash <endpoint_name.sh>”
The optimal sequence of requests to the first launched server:
1. `bash getNews.sh` — asking for news. We get an empty array.
2. `bash createCategory.sh` — as an admin, create the first category.

    2.1. `bash getCategories.sh` — optionally, check the list of categories.

3. `bash editCategory.sh` — modify the newly created category.

    3.1. `bash getCategories.sh` — optional.

4. `bash createUser.sh` — as an admin, we create a second user who is not an admin, but can create news (his name is Oleg).

6. `bash createNews.sh` — on behalf of Oleg we create the first news, in the first category and a picture (Haskell logo)

    5.1. `bash getNews.sh` — optional. Thus, we will check that the news is written to the database.

7. `bash editNews.sh` — on behalf of Oleg we change the news

    6.1. `bash getNews.sh` — optional. 

8. In the browser, you can check the link to which picture is returned in the news we recorded. Paste the link to the picture from the received news into the browser. It will look like this: “localhost:3000/getPicture?id=1" 
* There is **logins-passwords-base64.txt** file in which it is convenient to store encoded logins and passwords for testing endpoints with authorization.

# Architecture
main reads the config, creates an environment, and runs application function from the Routing module in it. application is written in the ReaderT monad. Environment contains information for logging and Connection type for contacting the database.
The application function is a standard routing function. It receives a request from the client and, depending on the passed endpoint, calls one of the functions from the Endpoints folder with this request.

## Endpoints folder
Almost all endpoints are written using the handle pattern. A separate file is allocated for each such endpoint.

## Types
`Types.API` — specifically designed to process the body of a request from a client.
`Types.Domain` —  fully describe an entity.
`Types.DB` — types into which the response from the database is parsed.

## Hashing
The server supports password hashing. The hashing uses the **Blake2b_256** algorithm. The functions for hashing are implemented in the **Hash** module.
When a user registers, it is not the transmitted password that is written to the database, but its hash. During authorization, the server will compare the hash of the transmitted password with the hash in the database.

## Authorization
Authorization is implemented using **Basic Auth**. To authorize, the client must send the header “Authorization: ...” in the request. In the body of the header, you need to specify a base64-encoded string containing the username and password, which are separated by a colon (login:password).

The Auth.hs file implements functions for authorization. The main authorization function takes a request from the client, finds the header there, decodes it from base64 and calls the function from DatabaseQueries.Auth to query the database and get the user data. At the output, the function will provide user data with information about all permissions, or an error.

Most of the endpoints in the application function are called with the higher-order function **withAuthAndParsedRequest**. It takes endpoint function and request, makes API-type-request and User-info from database and return result of endpoint function.

## Logging
The server supports three-level logging:
* DEBUG - logging of all events during debugging.
* WARNING - logging errors and warnings.
* RELEASE - logging only total errors (for example, "there is no connection to the database")
The logging level is specified in the config.json file. Enabling one level means that all messages will be logged, including those logged at a higher level. Logging levels are described in Types.Domain.Log.
The logging function itself is implemented in the Log module.

# SERVER API / Endpoint

## GET - requests
Responsible for getting entities.

### /categories
GET-request. Returns categories list.

### /users
GET-request. Returns users list.

### /picture
GET-request. Returns picture.

Parameters:

`id` - Int - Required;

Example: "http://localhost:3000/getPicture?id=42"

### /news
GET request. Returns news list.

Optional Text Parameters for filtering:

`creator_login` 

`category_title`

`created_at` - example: 2020-12-21;

`created_until`

`created_since`

`title`

`content`

Other oprtional parameters:

`limit` - Int value for pagination. 10 by default;

`offset` - Int value for offset;

`sort_by` - Text. The following options are available:

  `creator_login`
    
   `category_title`
    
  `create_date`
    
  `number_of_pictures`
  

## POST - requests
Responsible for creating entities.

### /category
POST-request. Creates Category. Returns id of the created category or error message. Available to authorized admin-users.

Parameters:

`title` - Text - Required;

`parentCategoryId` - Int - Optional;

### /user
POST-request. Creates user. Available to authorized admin-users.

Parameters:

`name` - Text - Required;

`login` - Text - Required;

`password` - Text - Required;

`isAdmin` - Bool - Required;

`isAbleToCreateNews` - Bool - Required;

### /news
POST-request. Creates news. Returns news id or error message. Available to able to create news users.

Parameters:

`title` - Text - Required;

`categoryId` - Int - Required;

`textContent` - Text - Required;

`pictures` - Array of pictures- Required;
    Request body for picture:
```
{
"image": 
    { 
    "mime: <Text. Image format. Example: "image/png">,
    "data": <Text. Image encoded in base64>
    }
}

```

## PUT - requests
Responsible for editing entities.

### /category
POST-request. Available to authorized admin-users

Parameters:

`processedCategoryId` - Int - Required;

`newTitle` - Text - Optional;
      
`newParentCategoryId` - Int - Optional - specify "0" to set a null parent category;
    
### /news
PUT-request. Edits news. Available to author.

`newsId` - Int - Required;

`newTitle` - Text - Optional;

`newCategoryId` - Text - Optional;

`newTextContent` - Text - Optional;

`newPictures` - Array of pictures. See "picture" section in createNews endpoint - Optional;

`newPublishStatus` - Bool - Optional;
