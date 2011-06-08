-module({{projectid}}_cowboy_default).

-behaviour(cowboy_http_handler).

%% 
%% The streaming code was initially based on misultin:misultin_req.erl
%% but has been modified quite a bit.  The content type function is
%% lifted from misultin:misultin_utility.erl
%%

%%
%% API
%%
-export([
	 init/3,
	 handle/2,
	 terminate/2
	]).

%%
%% Dev API
%%
-export([
	 do_test/2
	]).


-record(state, {docroot, filereadbuffer}).


%%
%% API Implementation
%%
init({tcp, http}, Req, [DocRoot, FileReadBuffer]) ->
    {ok, Req, #state{docroot = DocRoot, filereadbuffer = FileReadBuffer}}.

handle(Req, S) ->
    {FullPath, Req1} = get_full_path(Req, S),
    case open_file(FullPath) of
	{ok, IoDevice} -> reply_full_file(Req1, FullPath, IoDevice, S);
	error          -> reply_error(Req1, S)
    end.

terminate(_Req, _State) ->
    ok.

%% Dev API

do_test(FullPath, FileReadBuffer) ->
    {ok, IoDevice} = open_file(FullPath),
    ok = send_file(IoDevice, 0, fun(Data) -> io:format("~s", [Data]) end, FileReadBuffer).

%% Internal Functions

reply_full_file(Req, Filename, IoDevice, S) ->
    {ok, Req1} = send_headers(Req, Filename),
    Chunkfun = fun(Data) -> cowboy_http_req:chunk(Data, Req1) end,
    ok = send_file(IoDevice, 0, Chunkfun, S#state.filereadbuffer),
    {ok,  Req1, S}.

reply_error(Req, S) ->
    {ok, Req1} = cowboy_http_req:reply(404, [], <<"File not found.">>, Req),
    {ok, Req1, S}.

get_full_path(Req, S) ->
    case cowboy_http_req:raw_path(Req) of
	{<< $/, Filename>>, Req1} -> true;
	{Filename, Req1} -> true
    end,
    Suffix = case binary:last(Filename) of
		 $/ -> << "index.html" >>;
		 _ -> << "" >>
	     end,
    Docroot = S#state.docroot,
    {<< Docroot/bits, Filename/bits, Suffix/bits >>, Req1}.

open_file(FullPath) ->
    error_logger:info_msg("~nOpening... '~s'~n", [FullPath]),
    file:open(FullPath, [read, binary]).

send_headers(Req, Filename) ->
    ContentType = get_content_type(Filename),
    cowboy_http_req:chunked_reply(200, [{<<"Content-Type">>, ContentType}], Req).

send_file(IoDevice, Position, StreamFun, FileReadBuffer) ->
    case file:pread(IoDevice, Position, FileReadBuffer) of
	{ok, Data} -> 
	    StreamFun(Data),
	    send_file(IoDevice, Position + FileReadBuffer, StreamFun, FileReadBuffer);
	eof -> ok
    end.
	
get_content_type(FileName) ->
    case filename:extension(FileName) of
	%% most common first
	<<".doc">> -> <<"application/msword">>;
	<<".exe">> -> <<"application/octet-stream">>;
	<<".pdf">> -> <<"application/pdf">>;
	<<".rtf">> -> <<"application/rtf">>;
	<<".ppt">> -> <<"application/vnd.ms-powerpoint">>;
	<<".tgz">> -> <<"application/x-compressed">>;
	<<".tar">> -> <<"application/x-tar">>;
	<<".zip">> -> <<"application/zip">>;
	<<".mp3">> -> <<"audio/mpeg">>;
	<<".wav">> -> <<"audio/x-wav">>;
	<<".bmp">> -> <<"image/bmp">>;
	<<".ram">> -> <<"audio/x-pn-realaudio">>;
	<<".gif">> -> <<"image/gif">>;
	<<".jpe">> -> <<"image/jpeg">>;
	<<".jpeg">> -> <<"image/jpeg">>;
	<<".jpg">> -> <<"image/jpeg">>;
	<<".tif">> -> <<"image/tiff">>;
	<<".tiff">> -> <<"image/tiff">>;
	<<".png">> -> <<"image/png">>;
	<<".htm">> -> <<"text/html">>;
	<<".html">> -> <<"text/html">>;
	<<".txt">> -> <<"text/plain">>;
	<<".mp2">> -> <<"video/mpeg">>;
	<<".mpa">> -> <<"video/mpeg">>;
	<<".mpe">> -> <<"video/mpeg">>;
	<<".mpeg">> -> <<"video/mpeg">>;
	<<".mpg">> -> <<"video/mpeg">>;
	<<".mov">> -> <<"video/quicktime">>;
	<<".avi">> -> <<"video/x-msvideo">>;
	%% less common last
	<<".evy">> -> <<"application/envoy">>;
	<<".fif">> -> <<"application/fractals">>;
	<<".spl">> -> <<"application/futuresplash">>;
	<<".hta">> -> <<"application/hta">>;
	<<".acx">> -> <<"application/internet-property-stream">>;
	<<".hqx">> -> <<"application/mac-binhex40">>;
	<<".dot">> -> <<"application/msword">>;
	<<".bin">> -> <<"application/octet-stream">>;
	<<".class">> -> <<"application/octet-stream">>;
	<<".dms">> -> <<"application/octet-stream">>;
	<<".lha">> -> <<"application/octet-stream">>;
	<<".lzh">> -> <<"application/octet-stream">>;
	<<".oda">> -> <<"application/oda">>;
	<<".axs">> -> <<"application/olescript">>;
	<<".prf">> -> <<"application/pics-rules">>;
	<<".p10">> -> <<"application/pkcs10">>;
	<<".crl">> -> <<"application/pkix-crl">>;
	<<".ai">> -> <<"application/postscript">>;
	<<".eps">> -> <<"application/postscript">>;
	<<".ps">> -> <<"application/postscript">>;
	<<".setpay">> -> <<"application/set-payment-initiation">>;
	<<".setreg">> -> <<"application/set-registration-initiation">>;
	<<".xla">> -> <<"application/vnd.ms-excel">>;
	<<".xlc">> -> <<"application/vnd.ms-excel">>;
	<<".xlm">> -> <<"application/vnd.ms-excel">>;
	<<".xls">> -> <<"application/vnd.ms-excel">>;
	<<".xlt">> -> <<"application/vnd.ms-excel">>;
	<<".xlw">> -> <<"application/vnd.ms-excel">>;
	<<".msg">> -> <<"application/vnd.ms-outlook">>;
	<<".sst">> -> <<"application/vnd.ms-pkicertstore">>;
	<<".cat">> -> <<"application/vnd.ms-pkiseccat">>;
	<<".stl">> -> <<"application/vnd.ms-pkistl">>;
	<<".pot">> -> <<"application/vnd.ms-powerpoint">>;
	<<".pps">> -> <<"application/vnd.ms-powerpoint">>;
	<<".mpp">> -> <<"application/vnd.ms-project">>;
	<<".wcm">> -> <<"application/vnd.ms-works">>;
	<<".wdb">> -> <<"application/vnd.ms-works">>;
	<<".wks">> -> <<"application/vnd.ms-works">>;
	<<".wps">> -> <<"application/vnd.ms-works">>;
	<<".hlp">> -> <<"application/winhlp">>;
	<<".bcpio">> -> <<"application/x-bcpio">>;
	<<".cdf">> -> <<"application/x-cdf">>;
	<<".z">> -> <<"application/x-compress">>;
	<<".cpio">> -> <<"application/x-cpio">>;
	<<".csh">> -> <<"application/x-csh">>;
	<<".dcr">> -> <<"application/x-director">>;
	<<".dir">> -> <<"application/x-director">>;
	<<".dxr">> -> <<"application/x-director">>;
	<<".dvi">> -> <<"application/x-dvi">>;
	<<".gtar">> -> <<"application/x-gtar">>;
	<<".gz">> -> <<"application/x-gzip">>;
	<<".hdf">> -> <<"application/x-hdf">>;
	<<".ins">> -> <<"application/x-internet-signup">>;
	<<".isp">> -> <<"application/x-internet-signup">>;
	<<".iii">> -> <<"application/x-iphone">>;
	<<".js">> -> <<"application/x-javascript">>;
	<<".latex">> -> <<"application/x-latex">>;
	<<".mdb">> -> <<"application/x-msaccess">>;
	<<".crd">> -> <<"application/x-mscardfile">>;
	<<".clp">> -> <<"application/x-msclip">>;
	<<".dll">> -> <<"application/x-msdownload">>;
	<<".m13">> -> <<"application/x-msmediaview">>;
	<<".m14">> -> <<"application/x-msmediaview">>;
	<<".mvb">> -> <<"application/x-msmediaview">>;
	<<".wmf">> -> <<"application/x-msmetafile">>;
	<<".mny">> -> <<"application/x-msmoney">>;
	<<".pub">> -> <<"application/x-mspublisher">>;
	<<".scd">> -> <<"application/x-msschedule">>;
	<<".trm">> -> <<"application/x-msterminal">>;
	<<".wri">> -> <<"application/x-mswrite">>;
	<<".nc">> -> <<"application/x-netcdf">>;
	<<".pma">> -> <<"application/x-perfmon">>;
	<<".pmc">> -> <<"application/x-perfmon">>;
	<<".pml">> -> <<"application/x-perfmon">>;
	<<".pmr">> -> <<"application/x-perfmon">>;
	<<".pmw">> -> <<"application/x-perfmon">>;
	<<".p12">> -> <<"application/x-pkcs12">>;
	<<".pfx">> -> <<"application/x-pkcs12">>;
	<<".p7b">> -> <<"application/x-pkcs7-certificates">>;
	<<".spc">> -> <<"application/x-pkcs7-certificates">>;
	<<".p7r">> -> <<"application/x-pkcs7-certreqresp">>;
	<<".p7c">> -> <<"application/x-pkcs7-mime">>;
	<<".p7m">> -> <<"application/x-pkcs7-mime">>;
	<<".p7s">> -> <<"application/x-pkcs7-signature">>;
	<<".sh">> -> <<"application/x-sh">>;
	<<".shar">> -> <<"application/x-shar">>;
	<<".swf">> -> <<"application/x-shockwave-flash">>;
	<<".sit">> -> <<"application/x-stuffit">>;
	<<".sv4cpio">> -> <<"application/x-sv4cpio">>;
	<<".sv4crc">> -> <<"application/x-sv4crc">>;
	<<".tcl">> -> <<"application/x-tcl">>;
	<<".tex">> -> <<"application/x-tex">>;
	<<".texi">> -> <<"application/x-texinfo">>;
	<<".texinfo">> -> <<"application/x-texinfo">>;
	<<".roff">> -> <<"application/x-troff">>;
	<<".t">> -> <<"application/x-troff">>;
	<<".tr">> -> <<"application/x-troff">>;
	<<".man">> -> <<"application/x-troff-man">>;
	<<".me">> -> <<"application/x-troff-me">>;
	<<".ms">> -> <<"application/x-troff-ms">>;
	<<".ustar">> -> <<"application/x-ustar">>;
	<<".src">> -> <<"application/x-wais-source">>;
	<<".cer">> -> <<"application/x-x509-ca-cert">>;
	<<".crt">> -> <<"application/x-x509-ca-cert">>;
	<<".der">> -> <<"application/x-x509-ca-cert">>;
	<<".pko">> -> <<"application/ynd.ms-pkipko">>;
	<<".au">> -> <<"audio/basic">>;
	<<".snd">> -> <<"audio/basic">>;
	<<".mid">> -> <<"audio/mid">>;
	<<".rmi">> -> <<"audio/mid">>;
	<<".aif">> -> <<"audio/x-aiff">>;
	<<".aifc">> -> <<"audio/x-aiff">>;
	<<".aiff">> -> <<"audio/x-aiff">>;
	<<".m3u">> -> <<"audio/x-mpegurl">>;
	<<".ra">> -> <<"audio/x-pn-realaudio">>;
	<<".cod">> -> <<"image/cis-cod">>;
	<<".ief">> -> <<"image/ief">>;
	<<".jfif">> -> <<"image/pipeg">>;
	<<".svg">> -> <<"image/svg+xml">>;
	<<".ras">> -> <<"image/x-cmu-raster">>;
	<<".cmx">> -> <<"image/x-cmx">>;
	<<".ico">> -> <<"image/x-icon">>;
	<<".pnm">> -> <<"image/x-portable-anymap">>;
	<<".pbm">> -> <<"image/x-portable-bitmap">>;
	<<".pgm">> -> <<"image/x-portable-graymap">>;
	<<".ppm">> -> <<"image/x-portable-pixmap">>;
	<<".rgb">> -> <<"image/x-rgb">>;
	<<".xbm">> -> <<"image/x-xbitmap">>;
	<<".xpm">> -> <<"image/x-xpixmap">>;
	<<".xwd">> -> <<"image/x-xwindowdump">>;
	<<".mht">> -> <<"message/rfc822">>;
	<<".mhtml">> -> <<"message/rfc822">>;
	<<".nws">> -> <<"message/rfc822">>;
	<<".css">> -> <<"text/css">>;
	<<".323">> -> <<"text/h323">>;
	<<".stm">> -> <<"text/html">>;
	<<".uls">> -> <<"text/iuls">>;
	<<".bas">> -> <<"text/plain">>;
	<<".c">> -> <<"text/plain">>;
	<<".h">> -> <<"text/plain">>;
	<<".rtx">> -> <<"text/richtext">>;
	<<".sct">> -> <<"text/scriptlet">>;
	<<".tsv">> -> <<"text/tab-separated-values">>;
	<<".htt">> -> <<"text/webviewhtml">>;
	<<".htc">> -> <<"text/x-component">>;
	<<".etx">> -> <<"text/x-setext">>;
	<<".vcf">> -> <<"text/x-vcard">>;
	<<".mpv2">> -> <<"video/mpeg">>;
	<<".qt">> -> <<"video/quicktime">>;
	<<".lsf">> -> <<"video/x-la-asf">>;
	<<".lsx">> -> <<"video/x-la-asf">>;
	<<".asf">> -> <<"video/x-ms-asf">>;
	<<".asr">> -> <<"video/x-ms-asf">>;
	<<".asx">> -> <<"video/x-ms-asf">>;
	<<".movie">> -> <<"video/x-sgi-movie">>;
	<<".flr">> -> <<"x-world/x-vrml">>;
	<<".vrml">> -> <<"x-world/x-vrml">>;
	<<".wrl">> -> <<"x-world/x-vrml">>;
	<<".wrz">> -> <<"x-world/x-vrml">>;
	<<".xaf">> -> <<"x-world/x-vrml">>;
	<<".xof">> -> <<"x-world/x-vrml">>;
	_ -> <<"application/octet-stream">>
    end.
