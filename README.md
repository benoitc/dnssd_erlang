#dnssd_erlang

dnssd_erlang is an interface to Apple's Bonjour DNS Service Discovery implementation. Bonjour allows applications to browse, resolve and register network services via link-local multicast DNS on the local network and via unicast DNS over the internet. In the later case if the service is running behind a NAT gateway Bonjour will only advertise it if a port forward can be negotiated via NAT-PMP or uPNP (which is attempted automatically).

## Prerequisites

Apple Bonjour or compatible API with the appropriate development files available. If you are not running OS X or Linux with Avahi you will likely have to massage the build flags.

If you are running Windows and know how to make the build a no-brainer please send me a patch.
