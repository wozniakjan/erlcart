%%  coding: latin-1
%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotification_EventHeader
%% Source: /var/lib/openshift/556f9860e0b8cdaf36000016/diy/otp/lib/cosNotification/src/CosNotification.idl
%% IC vsn: 4.4
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotification_EventHeader').
-ic_compiled("4_4").


-include("CosNotification.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/CosNotification/EventHeader:1.0",
            "EventHeader",
            [{"fixed_header",
              {tk_struct,"IDL:omg.org/CosNotification/FixedEventHeader:1.0",
                  "FixedEventHeader",
                  [{"event_type",
                    {tk_struct,"IDL:omg.org/CosNotification/EventType:1.0",
                        "EventType",
                        [{"domain_name",{tk_string,0}},
                         {"type_name",{tk_string,0}}]}},
                   {"event_name",{tk_string,0}}]}},
             {"variable_header",
              {tk_sequence,
                  {tk_struct,"IDL:omg.org/CosNotification/Property:1.0",
                      "Property",
                      [{"name",{tk_string,0}},{"value",tk_any}]},
                  0}}]}.

%% returns id
id() -> "IDL:omg.org/CosNotification/EventHeader:1.0".

%% returns name
name() -> "CosNotification_EventHeader".


