#dnssd_erlang

dnssd_erlang is an interface to Apple's Bonjour DNS Service Discovery implementation. Bonjour allows applications to browse, resolve and register network services via link-local multicast DNS on the local network and via unicast DNS over the internet. In the later case if the service is running behind a NAT gateway Bonjour will only advertise it if a port forward can be negotiated via NAT-PMP or uPNP (which is attempted automatically).

####Development Status
The API and functionality provided aren't yet set in stone. If you use this or would like to, I'd like your [feedback](andrew.tj.id.au/email).

## Prerequisites

Apple Bonjour or compatible API with the appropriate development files available. If you are not running OS X or Linux with Avahi you will likely have to massage the build flags.

If you are running Windows and know how to make the build a no-brainer please send me a patch.

## Example use

    Eshell V5.8.2  (abort with ^G)
    1> dnssd:start().
    ok

First start the application via dnssd:start/1 or application:start(dnssd).

### Browsing for Services

    2> dnssd:browse("_http._tcp").
    {ok,#Ref<0.0.0.197>,[]}

In the success case, all functions return a tuple of the form `{ok, Reference, InitialResults}`. Reference should be retained to pass to dnssd:stop/1 when no further results are required. InitialResults will generally be empty unless another process has done the same operation and some results are already known.

    3> flush().
    Shell got {dnssd,#Ref<0.0.0.197>,
                     {browse,add,
                             {<<"dnsndnsweb">>,<<"_http._tcp.">>,
                              <<"bonjour.tj.id.au.">>}}}
    Shell got {dnssd,#Ref<0.0.0.197>,
                     {browse,add,{<<"TIVO">>,<<"_http._tcp.">>,<<"local.">>}}}
    ok

Subsequent results will be sent in tuples of the form `{dnssd, Reference, {Operation, Change, Result}}`.

    4> dnssd:browse(<<"_http._tcp">>, <<"dns-sd.org">>).
    {ok,#Ref<0.0.0.488>,[]}
    5> flush().
    Shell got {dnssd,#Ref<0.0.0.488>,
                     {browse,add,
                             {<<" * Apple, makers of the iPod">>,
                              <<"_http._tcp.">>,<<"dns-sd.org.">>}}}
    Shell got {dnssd,#Ref<0.0.0.488>,
                     {browse,add,
                             {<<" * Google, searching the Web">>,
                              <<"_http._tcp.">>,<<"dns-sd.org.">>}}}
    %% snipped %%
    ok

Browsing can be limited to a specific domain by specifying the domain as argument two. Both domains and service types may be specified as lists or binaries.

### Resolving a Service Instance

    6> dnssd:resolve(<<" * DNS Service Discovery">>, <<"_http._tcp.">>, <<"dns-sd.org.">>). 
    {ok,#Ref<0.0.0.6628>,[]}

To resolve a service, supply it's name, registration type and domain to the resolve function.

    7> flush().
    Shell got {dnssd,#Ref<0.0.0.6628>,
                     {resolve,add,
                              {<<"\\032*\\032DNS\\032Service\\032Discovery._http._tcp.dns-sd.org.">>,
                               <<"dns-sd.org.">>,80,
                               [<<"txtvers=1">>,<<"path=/">>]}}}
    Shell got {dnssd,#Ref<0.0.0.6628>,nomorecoming}
    ok

Unlike the other operations the resolve function will not continue indefinitely. The atom `'nomorecoming'` indicates when the operation has stopped.

### Registering Services

    8> dnssd:register("_answer._udp",42).
    {ok,#Ref<0.0.0.10006>,[]}
    9> flush().
    Shell got {dnssd,#Ref<0.0.0.10006>,
                     {register,add,
                               {<<"atj-mbp">>,<<"_answer._udp.">>,<<"local.">>}}}
    ok

The minimum arguments needed to register a service are the service type and port. If no service name is supplied, the machines name is used (in the example above, that's `<<"atj-mbp">>`).

For brevity, the alternative invocations of register are:

    dnssd:register(Name, Type, Port)
    dnssd:register(Type, Port, Txt)
    dnssd:register(Name, Type, Port, Txt)
    dnssd:register(Name, Type, Port, Txt, Host, Domain)

Wherein:

 * `Txt` is a TXT record data in either binary form (a sequence of `<<Size, String:Size/binary>>`), a list of atoms, strings or binaries or tuples of the form {Key,Value} where Key and Value are atoms, strings or binaries.
 * `Host` is the hostname of the machine running the service. Pass an empty string or binary for the local machine.
 * `Domain` is the domain to register the service within. Pass an empty string or binary for all domains.

### Enumerating Domains

    10> dnssd:enumerate(browse).
    {ok,#Ref<0.0.0.15448>,[]}
    11> flush().
    Shell got {dnssd,#Ref<0.0.0.15448>,{enumerate,add,<<"local.">>}}
    Shell got {dnssd,#Ref<0.0.0.15448>,{enumerate,add,<<"bonjour.tj.id.au.">>}}
    ok
    12> dnssd:enumerate(reg).
    {ok,#Ref<0.0.0.15529>,[]}
    13> flush().
    Shell got {dnssd,#Ref<0.0.0.15529>,{enumerate,add,<<"local.">>}}
    Shell got {dnssd,#Ref<0.0.0.15529>,{enumerate,add,<<"bonjour.tj.id.au.">>}}
    ok

### Stopping Operations

It's important to stop operations when no more results are needed to avoid generating needless network traffic. To stop an operation pass the Reference returned when you started the operation to dnssd:stop/1. Operations will also be stopped if your process exits.