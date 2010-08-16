-module(erozja_gui).
-author('baryluk@smp.if.uj.edu.pl').


-include_lib("wx/include/wx.hrl").

% we are using wxWidgets here
% for Erlang, beyond wx, there is also etk (Erlang Tk from Tcl/Tk), gs, x11, ex11.
%


-behaviour(wx_object).

-export([start/0, start_link/0, stop/0]).
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_call/3, handle_event/2]).

-record(state, {win, tree, preview, adder}).

start() ->
	Debug = [],
	wx_object:start(?MODULE, Debug, []).

start_link() ->
	Debug = [],
	wx_object:start_link(?MODULE, Debug, []).

stop() ->
	wx_object:cast(?MODULE, stop).

init(Options) ->
    wx:new(Options),

	process_flag(trap_exit, true),

	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erozja", [{size, {800, 500}}]),

	Icons = wxIconBundle:new(),
	Icon1 = wxIcon:new("icon1.png"),
	wxIconBundle:addIcon(Icons, Icon1),
	wxFrame:setIcons(Frame, Icons),

	%wxFrame:setTitle(Frame, "5 new feeds - Erozja"),
	wxFrame:centreOnScreen(Frame),
	%wxFrame:maximize(Frame, [{maximize, true}]),

	MB = wxMenuBar:new(),
	Sub = wxMenu:new(),
	wxMenu:append(Sub, ?wxID_NEW, "New subscription...", [{help, "Add new RSS feed to the Erozja reader"}]),
	wxMenu:append(Sub, 10002, "New folder..."),
	wxMenu:append(Sub, 10003, "New source...", [{help, "Add meta-sources like Planets/Blogrolls, live OMPL file, Google Reader account or Bloglines account"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:append(Sub, 10004, "Import subscriptions...", [{help, "Read subscriptions from OMPL file"}]),
	wxMenu:append(Sub, 10005, "Export subscriptions...", [{help, "Save all subscriptions to OMPL file"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:append(Sub, ?wxID_EXIT, "&Quit"),
	Help = wxMenu:new(),
	wxMenu:append(Help, ?wxID_ABOUT, "About"),
	wxMenuBar:append(MB, Sub, "&Subscriptions"),
	wxMenuBar:append(MB, Help, "&Help"),
	wxFrame:setMenuBar(Frame, MB),

	wxFrame:connect(Frame, command_menu_selected),

	_SB = wxFrame:createStatusBar(Frame),

	Panel = wxPanel:new(Frame),

	Tree = wxTreeCtrl:new(Panel),
	wxTreeCtrl:setMinSize(Tree, {200, -1}),
	I1 = wxTreeCtrl:addRoot(Tree, "Nauka", [{data, nauka}]),
	wxTreeCtrl:appendItem(Tree, I1, "Bash", [{data, n1}]),
	wxTreeCtrl:appendItem(Tree, I1, "BBC", [{data, n2}]),
	wxTreeCtrl:appendItem(Tree, I1, "LV", [{data, n3}]),

	Preview = wxHtmlWindow:new(Panel, [{style, ?wxHW_SCROLLBAR_AUTO}]),
	wxTreeCtrl:setMinSize(Preview, {400, -1}),

	wxHtmlWindow:setPage(Preview, "<b>kuku</b> <a href='s'>lll</a> ok!"),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(MainSizer, Tree, [{flag, ?wxALL bor ?wxEXPAND}]),
	wxSizer:add(MainSizer, Preview, [{flag, ?wxALL bor ?wxEXPAND}]),
	wxPanel:setSizer(Panel, MainSizer),

	wxFrame:show(Frame),


	wxTreeCtrl:connect(Tree, command_tree_item_activated),
	wxHtmlWindow:connect(Preview, command_html_link_clicked),

	State = #state{win=Frame,tree=Tree,preview=Preview},

io:format("State = ~p~n", [State]),

	wxToolTip:enable(true),
	wxToolTip:setDelay(500),

	{Frame, State#state{}}.

%% Handled as in normal gen_server callbacks
handle_info({Adder, W}, State = #state{adder=Adder}) when is_pid(Adder) ->
	case W of
		{add, URL} ->
			io:format("Adding ~p~n", [URL]);
		cancel ->
			ok
	end,
	{noreply, State#state{adder=undefined}};
handle_info({'EXIT', _, wx_deleted}, State) ->
	{noreply, State};
handle_info({'EXIT', _, normal}, State) ->
	{noreply, State};
handle_info(Msg, State) ->
	io:format("~p: Unknown msg ~p~n", [?MODULE, Msg]),
	{noreply, State}.

handle_call(Msg, From, State) ->
	io:format("~p: Unknown call ~p from ~p~n", [?MODULE, Msg, From]),
	{noreply, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event=#wxCommand{type=command_listbox_selected, cmdString=Ex}}, State = #state{}) ->
	case Ex of
		[] ->
			{noreply, State};
		_ ->
			ok,
			{noreply, State}
	end;
handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}}, State = #state{}) ->
	case Id of
		?wxID_ABOUT ->
			AboutString =
				"Erozja - RSS client and proxy.\nUsing Erlang and wxWidgets as GUI\n"
				"Authors: Witold Baryluk",
			wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
							[{style, ?wxOK bor ?wxICON_INFORMATION bor ?wxSTAY_ON_TOP},
							 {caption, "About"}])),
			{noreply, State};
		?wxID_EXIT ->
			{stop, normal, State};
		?wxID_NEW ->
			Parent = self(),
			Env = wx:get_env(),
			AdderPid = spawn_link(fun() ->
				wx:set_env(Env),
				String = "Enter URL of website to automatically detect available feeds or direct RSS feed address if you know it",
				Dialog = wxTextEntryDialog:new(State#state.win, String,
								[{style, ?wxOK bor ?wxCANCEL bor ?wxSTAY_ON_TOP},
								 {caption, "New subscription"}]),
				R = fun(F) ->
					case wxTextEntryDialog:showModal(Dialog) of
						?wxID_OK ->
							NewURL = wxTextEntryDialog:getValue(Dialog),
							case NewURL of
								[] ->
									F(F); % self recursion
								_ ->
									Parent ! {self(), {add, NewURL}}
							end;
						?wxID_CANCEL ->
							Parent ! {self(), cancel}
					end
				end,
				R(R)
			end),
			{noreply, State#state{adder=AdderPid}};
		_ ->
			{noreply, State}
	end;
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
	ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	{stop, normal, State};

handle_event(#wx{id = Id, event = #wxTree{type = command_tree_item_activated, item = ItemNum}}, State = #state{tree=Tree}) ->
	%io:format("Selected item ~p~n", [ItemNum]),
	Data = wxTreeCtrl:getItemData(Tree, ItemNum),
	io:format("  data ~p~n", [Data]),
	wxTreeCtrl:setItemBold(Tree, ItemNum, [{bold, true}]),
	{noreply, State};

	%wxTreeCtrl:getSelection(Tree)
	%wxTreeCtrl:setItemData(Tree, ItemNumber, Data),


handle_event(#wx{id = Id, event = #wxHtmlLink{type = command_html_link_clicked, linkInfo = LinkInfo}}, State = #state{}) ->
	#wxHtmlLinkInfo{href = Href} = LinkInfo,
	io:format("Clicked link ~p~n", [Href]),
	{noreply, State};

handle_event(Ev, State) ->
	io:format("~p: Unknown event ~p~n", [?MODULE, Ev]),
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
	wx:destroy().
