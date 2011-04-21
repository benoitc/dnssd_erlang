
main([]) ->
    NoCl = os:find_executable("cl.exe") =:= false,
    NoBonjourSDK = os:getenv("BONJOUR_SDK_HOME") =:= false,
    if NoCl ->
	    io:fwrite("cl.exe not found.~n"),
	    erlang:halt(1);
       NoBonjourSDK ->
	    io:fwrite("BONJOUR_SDK_HOME variable is not set.~n"),
	    erlang:halt(1);
       true ->
	    {ok, Cwd} = file:get_cwd(),
	    Arch = erlang:system_info(system_architecture),
	    ErlInc = fix_path(erl_inc_dir()),
	    EiInc = fix_path(code:lib_dir(erl_interface, include)),
	    EiLib = fix_path(code:lib_dir(erl_interface, lib)),
	    BSdk = os:getenv("BONJOUR_SDK_HOME"),
	    BSdkInc = BSdk ++ "include",
	    BSdkLib = BSdk ++ "lib\\" ++ Arch,
	    Cmd = flat_format(
		    "cl /nologo /LD /MT /Fednssd_drv.dll "
		    "/I \"~s\" /I \"~s\" /I \"~s\" "
		    "/Tc ..\\c_src\\dnssd.c "
		    "/link /LIBPATH:\"~s\" "
		    "/DLL /DYNAMICBASE:NO "
		    "ws2_32.lib ei.lib \"~s\\dnssd.lib\"",
		    [ErlInc, EiInc, BSdkInc, EiLib, BSdkLib]
		   ),
	    ok = file:set_cwd("priv"),
	    CmdResult = os:cmd(Cmd),
	    ok = file:set_cwd(Cwd),
	    io:fwrite("~s~n~n~s~n", [Cmd, CmdResult]),
	    case filelib:is_file("priv/dnssd_drv.dll") of
		true -> erlang:halt(0);
		false -> erlang:halt(1)
	    end
    end;
main(_) ->
    io:write("This script does not accept arguments.~n"),
    erlang:halt(1).

fix_path(Path) ->
    re:replace(Path, "/", "\\", [global, {return, binary}]).

erl_inc_dir() ->
    [_,_,_|Base] = lists:reverse(filename:split(code:lib_dir(erts, include))),
    filename:join([lists:reverse(Base), "usr", "include"]).

flat_format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
