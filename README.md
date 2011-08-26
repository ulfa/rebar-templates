install

In your home directory create the dir .rebar.Inside the .rebar dir create the link
ln -s <path to the template> templates

Create a REST service :

rebar create template=wm_service appid=test port=8888 entity=user

appid = name of the service and init resource
port = port for the webserver
entity = the name of the entity to store