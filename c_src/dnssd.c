/* -------------------------------------------------------------------

   Copyright (c) 2011 Andrew Tunnell-Jones. All Rights Reserved.

   This file is provided to you under the Apache License,
   Version 2.0 (the "License"); you may not use this file
   except in compliance with the License.  You may obtain
   a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing,
   software distributed under the License is distributed on an
   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
   KIND, either express or implied.  See the License for the
   specific language governing permissions and limitations
   under the License.

   -------------------------------------------------------------------- */

#if defined(_WIN32)
#define __WIN32__
#endif

#include <erl_driver.h>
#include <erl_interface.h>
#include <string.h>
#include <dns_sd.h>

#define DNSSD_CMD_ENUM     0
#define DNSSD_CMD_BROWSE   1
#define DNSSD_CMD_RESOLVE  2
#define DNSSD_CMD_REGISTER 3

typedef struct _dnssd_drv_t {
  DNSServiceRef sd_ref;
  ErlDrvPort erl_port;
  ErlDrvTermData term_port;
  ErlDrvTermData term_ok;
  ErlDrvTermData term_error;
  ErlDrvTermData term_enumerate;
  ErlDrvTermData term_browse;
  ErlDrvTermData term_resolve;
  ErlDrvTermData term_register;
#ifdef __WIN32__
  WSAEVENT event;
#endif
} dnssd_drv_t;

/* Driver Callbacks */
static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static int call(ErlDrvData drv_data,
		unsigned int command,
		char *buf,
		int len,
		char **rbuf,
		int rlen,
		unsigned int *flags);
static void process(ErlDrvData handle, ErlIOVec *ev);
static void ready_io(ErlDrvData handle, ErlDrvEvent ev);
static void send_error(ErlDrvData edd, signed int errno);

/* DNSSD API Callbacks */
static void DNSSD_API EnumReply(DNSServiceRef sd_ref,
				DNSServiceFlags flags,
				uint32_t ifIndex,
				DNSServiceErrorType err,
				const char * domain,
				void * context);

static void DNSSD_API BrowseReply(DNSServiceRef sd_ref,
				  DNSServiceFlags flags,
				  uint32_t ifIndex,
				  DNSServiceErrorType err,
				  const char * name,
				  const char * regtype,
				  const char * domain,
				  void * context);

static void DNSSD_API ResolveReply(DNSServiceRef sd_ref,
				   DNSServiceFlags flags,
				   uint32_t ifIndex,
				   DNSServiceErrorType err,
				   const char * fullname,
				   const char * hosttarget,
				   uint16_t port,
				   uint16_t txtLen,
				   const unsigned char * txtRecord,
				   void * context);

static void DNSSD_API RegisterReply (DNSServiceRef sd_ref,
				     DNSServiceFlags flags,
				     DNSServiceErrorType err,
				     const char * name,
				     const char * regtype,
				     const char * domain,
				     void * context);

static ErlDrvEntry dnssd_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    ready_io,                         /* ready_input */
    ready_io,                         /* ready_output */
    "dnssd_drv",                      /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    process,                          /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    call,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(dnssd_driver) {
  return &dnssd_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char* cmd) {
  dnssd_drv_t* retval = (dnssd_drv_t*) driver_alloc(sizeof(dnssd_drv_t));
  retval->sd_ref = NULL;
  retval->erl_port = port;
  retval->term_port = driver_mk_port(port);
  retval->term_ok = driver_mk_atom("ok");
  retval->term_error = driver_mk_atom("error");
  retval->term_enumerate = driver_mk_atom("enumerate");
  retval->term_browse = driver_mk_atom("browse");
  retval->term_resolve = driver_mk_atom("resolve");
  retval->term_register = driver_mk_atom("register");
  return (ErlDrvData) retval;
}

static void stop(ErlDrvData edd) {
  dnssd_drv_t* dd = (dnssd_drv_t*) edd;
#ifdef __WIN32__
  if (dd->event) {
    driver_select(dd->erl_port, dd->event, DO_READ, 0);
    WSAEventSelect(DNSServiceRefSockFD(dd->sd_ref), NULL, 0);
  }
  if (dd->sd_ref) {
    DNSServiceRefDeallocate(dd->sd_ref);
  }
#else
  if (dd->sd_ref) {
    driver_select(dd->erl_port,
		  (ErlDrvEvent) DNSServiceRefSockFD(dd->sd_ref),
		  DO_READ,
		  0);
    DNSServiceRefDeallocate(dd->sd_ref);
  }
#endif
  driver_free(dd);
}

static int call(ErlDrvData edd, unsigned int cmd, char *buf, int len,
		char **rbuf, int rlen, unsigned int *flags) {
  dnssd_drv_t* dd = (dnssd_drv_t*) edd;
  int version, out_len, index, rindex;
  DNSServiceErrorType err;
  char* out_atom_text;
  ei_term arg, name, type, domain, txt, host, hostport;
  char *name_tmp, *type_tmp, *domain_tmp, *txt_tmp, *host_tmp;

  /* don't allow reuse */
  if (dd->sd_ref) return -1;

  index = 0;
  dd->sd_ref = NULL;

  ei_decode_version(buf, &index, &version);
  ei_decode_ei_term(buf, &index, &arg);

  if (cmd == DNSSD_CMD_ENUM)  {
    if (arg.ei_type == ERL_ATOM_EXT) {
      if (strncmp(arg.value.atom_name, "browse", 6) == 0) {
	// init for enum browse
	err = DNSServiceEnumerateDomains(&dd->sd_ref,
					 kDNSServiceFlagsBrowseDomains,
					 kDNSServiceInterfaceIndexAny,
					 (DNSServiceDomainEnumReply) EnumReply,
					 dd);
      } else if (strncmp(arg.value.atom_name,"reg", 3) == 0) {
	// init for enum reg
	err = DNSServiceEnumerateDomains(&dd->sd_ref,
					 kDNSServiceFlagsRegistrationDomains,
					 kDNSServiceInterfaceIndexAny,
					 (DNSServiceDomainEnumReply) EnumReply,
					 dd);
      } else {
	goto badarg;
      }
    } else {
      goto badarg;
    }
  } else if (cmd == DNSSD_CMD_BROWSE) {
    if (!arg.ei_type == ERL_TUPLE || arg.arity != 2) goto badarg;
    /* decode type */
    ei_decode_ei_term(buf, &index, &type);
    if (type.ei_type != ERL_BINARY_EXT) goto badarg;
    index += 5; // skip tag + 4 byte size
    type_tmp = (char*)driver_alloc(type.size + 1);
    memset(type_tmp, 0, type.size + 1);
    memcpy(type_tmp, buf + index, type.size);
    index += type.size;
    /* decode domain */
    ei_decode_ei_term(buf, &index, &domain);
    if (domain.ei_type != ERL_BINARY_EXT) {
      driver_free(type_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    domain_tmp = (char *) driver_alloc(domain.size + 1);
    memset(domain_tmp, 0, domain.size + 1);
    memcpy(domain_tmp, buf + index, domain.size);
    err = DNSServiceBrowse(&dd->sd_ref,
			   0, // Flags
			   kDNSServiceInterfaceIndexAny,
			   type_tmp,
			   domain_tmp,
			   (DNSServiceBrowseReply) BrowseReply,
			   dd);
    driver_free(type_tmp);
    driver_free(domain_tmp);
  } else if (cmd == DNSSD_CMD_RESOLVE) {
    if (!arg.ei_type == ERL_TUPLE || arg.arity != 3) goto badarg;
    /* decode name */
    ei_decode_ei_term(buf, &index, &name);
    if (name.ei_type != ERL_BINARY_EXT) goto badarg;
    index += 5; // skip tag + 4 byte size
    name_tmp = (char *) driver_alloc(name.size + 1);
    memset(name_tmp, 0, name.size + 1);
    memcpy(name_tmp, buf + index, name.size);
    index += name.size;
    /* decode type */
    ei_decode_ei_term(buf, &index, &type);
    if (type.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    type_tmp = (char *) driver_alloc(type.size + 1);
    memset(type_tmp, 0, type.size + 1);
    memcpy(type_tmp, buf + index, type.size);
    index += type.size;
    /* decode domain */
    ei_decode_ei_term(buf, &index, &domain);
    if (domain.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      driver_free(type_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    domain_tmp = (char *) driver_alloc(domain.size + 1);
    memset(domain_tmp, 0, domain.size + 1);
    memcpy(domain_tmp, buf + index, domain.size);
    /* start op */
    err = DNSServiceResolve(&dd->sd_ref,
			    0, // Flags
			    kDNSServiceInterfaceIndexAny,
			    name_tmp,
			    type_tmp,
			    domain_tmp,
			    (DNSServiceResolveReply) ResolveReply,
			    dd);
    driver_free(name_tmp);
    driver_free(type_tmp);
    driver_free(domain_tmp);
  } else if (cmd == DNSSD_CMD_REGISTER) {
    if (!arg.ei_type == ERL_TUPLE || arg.arity != 6) goto badarg;
    /* decode name */
    ei_decode_ei_term(buf, &index, &name);
    if (name.ei_type != ERL_BINARY_EXT) goto badarg;
    index += 5; // skip tag + 4 byte size
    name_tmp = (char *) driver_alloc(name.size + 1);
    memset(name_tmp, 0, name.size + 1);
    memcpy(name_tmp, buf + index, name.size);
    index += name.size;
    /* decode type */
    ei_decode_ei_term(buf, &index, &type);
    if (type.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    type_tmp = (char *) driver_alloc(type.size + 1);
    memset(type_tmp, 0, type.size + 1);
    memcpy(type_tmp, buf + index, type.size);
    index += type.size;
    /* decode domain */
    ei_decode_ei_term(buf, &index, &domain);
    if (domain.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      driver_free(type_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    domain_tmp = (char *) driver_alloc(domain.size + 1);
    memset(domain_tmp, 0, domain.size + 1);
    memcpy(domain_tmp, buf + index, domain.size);
    index += domain.size;
    /* decode host */
    ei_decode_ei_term(buf, &index, &host);
    if (host.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      driver_free(type_tmp);
      driver_free(domain_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    host_tmp = (char *) driver_alloc(host.size + 1);
    memset(host_tmp, 0, host.size + 1);
    memcpy(host_tmp, buf + index, host.size);
    index += host.size;
    /* decode port */
    ei_decode_ei_term(buf, &index, &hostport);
    if (hostport.ei_type != ERL_INTEGER_EXT &&
	hostport.ei_type != ERL_SMALL_INTEGER_EXT) {
      driver_free(name_tmp);
      driver_free(type_tmp);
      driver_free(domain_tmp);
      driver_free(host_tmp);
      goto badarg;
    }
    /* decode txt */
    ei_decode_ei_term(buf, &index, &txt);
    if (txt.ei_type != ERL_BINARY_EXT) {
      driver_free(name_tmp);
      driver_free(type_tmp);
      driver_free(domain_tmp);
      driver_free(host_tmp);
      goto badarg;
    }
    index += 5; // skip tag + 4 byte size
    txt_tmp = (char *) driver_alloc(txt.size + 1);
    memset(txt_tmp, 0, txt.size + 1);
    memcpy(txt_tmp, buf + index, txt.size);
    /* start op */
    err = DNSServiceRegister(&dd->sd_ref,
			     kDNSServiceInterfaceIndexAny,
			     0, // Flags
			     name_tmp,
			     type_tmp,
			     domain_tmp,
			     host_tmp,
			     htons(hostport.value.i_val),
			     txt.size,
			     txt_tmp,
			     (DNSServiceRegisterReply) RegisterReply,
			     dd);
    driver_free(name_tmp);
    driver_free(type_tmp);
    driver_free(domain_tmp);
    driver_free(host_tmp);
    driver_free(txt_tmp);
  } else {
    goto badarg;
  }
  rindex = 0;
  out_len = 0;
  ei_encode_version(NULL, &out_len);
  if (err == kDNSServiceErr_NoError) {
#ifdef __WIN32__
    dd->event = WSACreateEvent();
    WSAEventSelect(DNSServiceRefSockFD(dd->sd_ref), dd->event, FD_READ);
    driver_select(dd->erl_port, dd->event, ERL_DRV_READ, 1);
#else
    driver_select(dd->erl_port,
		  (ErlDrvEvent) DNSServiceRefSockFD(dd->sd_ref),
		  ERL_DRV_READ,
		  1);
#endif
    out_atom_text = "ok";
    ei_encode_atom(NULL, &out_len, out_atom_text);
    if(rlen < out_len) {
      *rbuf = driver_alloc(out_len);
      rlen = out_len;
    }
    ei_encode_version(*rbuf, &rindex);
    ei_encode_atom(*rbuf, &rindex, out_atom_text);
    return out_len;
  } else {
    out_atom_text = "error";
    ei_encode_tuple_header(NULL, &out_len, 2);
    ei_encode_atom(NULL, &out_len, out_atom_text);
    ei_encode_long(NULL, &out_len, 1337);
    if(rlen < out_len) {
      *rbuf = driver_alloc(out_len);
      rlen = out_len;
    }
    ei_encode_version(*rbuf, &rindex);
    ei_encode_tuple_header(*rbuf, &rindex, 2);
    ei_encode_atom(*rbuf, &rindex, out_atom_text);
    ei_encode_long(*rbuf, &rindex, (long) err);
    return out_len;
  }
 badarg:
  return -1;
}

static void process(ErlDrvData handle, ErlIOVec *ev) {
  /* Todo: Update a registered service's text record */
}

static void ready_io(ErlDrvData edd, ErlDrvEvent ev)
{
  dnssd_drv_t* dd = (dnssd_drv_t*) edd;
  DNSServiceErrorType err;
#ifdef __WIN32__
  WSAResetEvent(dd->event);
#endif
  err = DNSServiceProcessResult(dd->sd_ref);
  if (err != kDNSServiceErr_NoError) {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_ATOM, dd->term_error,
			     ERL_DRV_INT, err,
			     ERL_DRV_TUPLE, 3};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

static void DNSSD_API EnumReply(DNSServiceRef sd_ref,
				DNSServiceFlags flags,
				uint32_t ifIndex,
				DNSServiceErrorType err,
				const char * domain,
				void * context)
{
  dnssd_drv_t* dd = (dnssd_drv_t*) context;
  if (err != kDNSServiceErr_NoError) {
    send_error(context, err);
  } else {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_ATOM, dd->term_enumerate,
			     ERL_DRV_INT, flags,
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) domain, strlen(domain),
			     ERL_DRV_TUPLE, 2,
			     ERL_DRV_TUPLE, 3};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

static void DNSSD_API BrowseReply(DNSServiceRef sd_ref,
				  DNSServiceFlags flags,
				  uint32_t ifIndex,
				  DNSServiceErrorType err,
				  const char * name,
				  const char * regtype,
				  const char * domain,
				  void * context
				  )
{
  dnssd_drv_t* dd = (dnssd_drv_t*) context;
  if (err != kDNSServiceErr_NoError) {
    send_error(context, err);
  } else {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_ATOM, dd->term_browse,
			     ERL_DRV_INT, flags,
			     ERL_DRV_INT, ifIndex,
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) name, strlen(name),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) regtype, strlen(regtype),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) domain, strlen(domain),
			     ERL_DRV_TUPLE, 5,
			     ERL_DRV_TUPLE, 3};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

static void DNSSD_API ResolveReply(DNSServiceRef sd_ref,
				   DNSServiceFlags flags,
				   uint32_t ifIndex,
				   DNSServiceErrorType err,
				   const char * fullname,
				   const char * hosttarget,
				   uint16_t port,
				   uint16_t txtLen,
				   const unsigned char * txtRecord,
				   void * context)
{
  dnssd_drv_t* dd = (dnssd_drv_t*) context;
  if (err != kDNSServiceErr_NoError) {
    send_error(context, err);
  } else {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_ATOM, dd->term_resolve,
			     ERL_DRV_INT, flags,
			     ERL_DRV_INT, ifIndex,
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) fullname, strlen(fullname),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) hosttarget, strlen(hosttarget),
			     ERL_DRV_INT, ntohs(port),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) txtRecord, txtLen,
			     ERL_DRV_TUPLE, 6,
			     ERL_DRV_TUPLE, 3};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

static void DNSSD_API RegisterReply (DNSServiceRef sd_ref,
				     DNSServiceFlags flags,
				     DNSServiceErrorType err,
				     const char * name,
				     const char * regtype,
				     const char * domain,
				     void * context)
{
  dnssd_drv_t* dd = (dnssd_drv_t*) context;
  if (err != kDNSServiceErr_NoError) {
    send_error(context, err);
  } else {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_ATOM, dd->term_register,
			     ERL_DRV_INT, flags,
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) name, strlen(name),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) regtype, strlen(regtype),
			     ERL_DRV_BUF2BINARY, (ErlDrvTermData) domain, strlen(domain),
			     ERL_DRV_TUPLE, 4,
			     ERL_DRV_TUPLE, 3};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

void send_error(ErlDrvData edd, signed int errno) {
  dnssd_drv_t* dd = (dnssd_drv_t*) edd;
  ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			   ERL_DRV_ATOM, dd->term_error,
			   ERL_DRV_INT, (ErlDrvTermData) errno,
			   ERL_DRV_TUPLE, 3};
  driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
}
