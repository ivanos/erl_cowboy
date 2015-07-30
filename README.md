# erl_cowboy
erl_cowboy is a thin wrapper around cowboy which makes it easier
to share a single cowboy listener port across applications on a
single node that want to use cowboy. The wrapper initializes cowboy
and dynamically updates cowboy's routing table as applications
install their application specific routing tables.

# Usage
Add erl_cowboy as a dependency to the application using cowboy. In
sys.config for the node, add environment variables for erl_cowboy.

Variable | Description
-------- | -----------
port | cowboy listener port for http
listeners | number of listeners

Applications using cowboy call `erl_cowboy:routing/2` with a unique
token and the path list part of the routing table. The unique token
can be the application name or the name of the module installing
the routes. Applications may update their routing in which case the
old route entries for the appliation are replaced with the new ones.
