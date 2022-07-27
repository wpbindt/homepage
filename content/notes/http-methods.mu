** 27-7-2022

The HTTP methods used in RESTful APIs are POST, GET, PUT, PATCH, and DELETE. There are other HTTP methods, like OPTIONS and TRACE, but they tend not to play a role in REST APIs. 

GET and DELETE are relatively easy to grok. One gets the data of a resource (if it exists), and the other removes it.

POST is used for creation of a new, resource that doesn't exist yet. It should return a status code 201 and an endpoint pointing to the newly created resource. 
It is not for updating pre-existing resources, that's what PUT and PATCH are for.

PUT is used for updating a given resource or creating one. The difference between it and POST is that PUT is meant to be idempotent. If used for updating a resource,
the request should contain the resource in its entirety.

PATCH is only used for updating a given resource. The difference between it and PUT is that PATCH can be used to update only part of a given resource.
This means that a PATCH request could contain only part of a resource. PATCH is not specified to be idempotent.
