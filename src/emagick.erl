%% -----------------------------------------------------------------------------
%%
%% emagick: Wrapper for Graphics/ImageMagick command line tool.
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(emagick).
-author('Per Andersson').

-export([convert/3, convert/4]).


%% -----------------------------------------------------------------------------
-spec convert(InData, From, To) -> {ok, OutData}
    when InData  :: binary(),
         From    :: atom(), %% pdf | png | jpg | gif | ...
         To      :: atom(), %% same as To
         OutData :: binary().
-spec convert(InData, From, To, Opts) -> {ok, OutData}
    when InData  :: binary(),
         From    :: atom(),
         To      :: atom(),
         Opts    :: list(tuple()),
         OutData :: binary().
%%
%% @doc
%%      Convert indata with *Magick.
%% @end
%% -----------------------------------------------------------------------------
convert(InData, From, To) -> convert(InData, From, To, []).
convert(InData, From, To, Opts) ->
    run(convert, [{indata, InData}, {from, From}, {to, To}, {opts, Opts}]).



%% =============================================================================
%%
%% INTERNAL FUNCTIONS
%%
%% =============================================================================

%% -----------------------------------------------------------------------------
-spec run(Command, Opts) -> {ok, Result}
    when Command :: atom(),
         Opts :: list(tuple()),
         Result :: binary().
%%
%% @doc
%%      Format and execute the supplied *magick command.
%% @end
%% -----------------------------------------------------------------------------
run(Command, Opts) ->
    %% create working directory if it does not exist already
    Workdir =
        case application:get_env(emagick, working_directory) of
            {ok, Dir} -> Dir;
            undefined -> "/tmp/emagick"
        end,
    %% add trailing slash to ensure path is dir
    ok = filelib:ensure_dir(Workdir ++ "/"),

    %% get opts
    InData  = proplists:get_value(indata, Opts),
    From    = proplists:get_value(from, Opts),
    To      = proplists:get_value(to, Opts),
    CmdOpts = proplists:get_value(opts, Opts, ""),

    %% write input file to temporary location
    Filename = uuid:to_string(uuid:uuid4()),
    InFile  = Workdir ++ "/" ++ Filename ++ "." ++ atom_to_list(From),
    OutFile = Workdir ++ "/" ++ Filename ++ "." ++ atom_to_list(To),

    %% dump indata to file to be consumed by command
    ok = file:write_file(InFile, InData),

    %% convert magick
    MagickPrefix =
        case application:get_env(emagick, magick_prefix) of
            {ok, Prefix} -> Prefix;
            undefined ->    ""
        end,
    PortCommand = string:join([MagickPrefix, atom_to_list(Command),
                               format_opts(CmdOpts), InFile, OutFile], " "),

    %% execute as port
    PortOpts = [stream, use_stdio, exit_status, binary],
    Port = erlang:open_port({spawn, PortCommand}, PortOpts),

    %% crash upon non-zero exit status
    {ok, _Data, 0} = receive_until_exit(Port, []),
    case erlang:port_info(Port) of
        undefined -> ok;
        _ ->         true = erlang:port_close(Port)
    end,

    %% read converted file
    Result = {ok, _} = file:read_file(OutFile),

    %% cleanup
    file:delete(InFile),
    file:delete(OutFile),

    Result.


%% -----------------------------------------------------------------------------
-spec format_opts(Opts) -> Result
    when Opts   :: list(tuple(atom(), list(term))),
         Result :: string().
%%
%% @doc
%%      Format option proplist as string.
%% @end
%% -----------------------------------------------------------------------------
format_opts(Opts) -> format_opts(Opts, []).
format_opts([], Res) -> string:join(Res, " ");
format_opts([Opt|Opts], Res) ->
    ArgStr =
        "-" ++ string:join([to_string(Arg) || Arg <- tuple_to_list(Opt)], " "),
    format_opts(Opts, Res ++ [ArgStr]).

-spec to_string(term()) -> string().
to_string(E) when is_atom(E) ->    atom_to_list(E);
to_string(E) when is_binary(E) ->  binary_to_list(E);
to_string(E) when is_integer(E) -> integer_to_list(E);
to_string(E) when is_list(E) ->    E.


%% -----------------------------------------------------------------------------
-spec receive_until_exit(Port, ReverseBuffer) -> {ok, Data, Status}
    when Port          :: port(),
         ReverseBuffer :: list(iolist()),
         Data          :: binary(),
         Status        :: pos_integer().
%%
%% @doc
%%      Receives until the given port exits.
%% @end
%% -----------------------------------------------------------------------------
receive_until_exit(Port, ReverseBuffer) ->
    receive
        {Port, {exit_status, Status}} ->
            Data = iolist_to_binary(lists:reverse(ReverseBuffer)),
            {ok, Data, Status};
        {Port, {data, Data}} ->
            receive_until_exit(Port, [Data | ReverseBuffer])
    end.
