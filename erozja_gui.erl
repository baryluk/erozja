-module(erozja_gui).
-author('baryluk@smp.if.uj.edu.pl').

-include_lib("wx/include/wx.hrl").

% we are using wxWidgets here
% for Erlang, beyond wx, there is also etk (Erlang Tk from Tcl/Tk), gs, x11, ex11.
%


-behaviour(wx_object).

-export([start/0, start_link/0, stop/0]).
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_call/3, handle_event/2]).

-record(state, {win, tree, preview, adder, sb}).

-define(deb_enable, 1).
-include("erozja.hrl").

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
	wxMenu:append(Sub, 10001, "Update all subscription", [{help, "Fetch all subscriped RSS feeds from Internet, and update any new items"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:append(Sub, ?wxID_NEW, "New subscription...", [{help, "Add new RSS feed to the Erozja reader"}]),
	wxMenu:append(Sub, 10002, "New folder..."),
	wxMenu:append(Sub, 10003, "New source...", [{help, "Add meta-sources like Planets/Blogrolls, live OMPL file, Google Reader account or Bloglines account"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:append(Sub, 10004, "Import subscriptions...", [{help, "Read subscriptions from OMPL file"}]),
	wxMenu:append(Sub, 10005, "Export subscriptions...", [{help, "Save all subscriptions to OMPL file"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:appendCheckItem(Sub, 10006, "Work offline", [{help, "When checked Erozja will not update automatically any subscripion"}]),
	wxMenu:appendSeparator(Sub),
	wxMenu:append(Sub, ?wxID_EXIT, "&Quit"),

	View = wxMenu:new(),
	wxMenu:append(View, 10021, "Smaller font size"),
	wxMenu:append(View, 10022, "Bigger font size"),
	wxMenu:append(View, 10023, "Normal font size"),
	wxMenu:appendSeparator(View),
	wxMenu:appendCheckItem(View, 10013, "Reduced tree view", [{help, "Show only this tree items, which have unreaded entries"}]),
	wxMenu:appendSeparator(View),
	wxMenu:appendRadioItem(View, 10014, "Normal view", [{help, "Two panels: list of entries (title, date, status) on top, and entry view at bottom"}]),
	wxMenu:appendRadioItem(View, 10015, "Wide view", [{help, "Two panels: list of entries (title, date, status) on left, and entry view at right - good for wide screens"}]),
	wxMenu:appendRadioItem(View, 10016, "Condensated view", [{help, "Single panel: Rendered as list with both headers (title, date), and content. Status rendered using background"}]),
	wxMenu:appendRadioItem(View, 10017, "List view", [{help, "Single panel: Show only list (title, date, status), like in the Normal view, but without entry view"}]),
	wxMenu:appendRadioItem(View, 10018, "Deep tree view", [{help, "Single panel: Show only tree (title, date, status), similar like in the Normal view, but now it is a tree of all feeds"}]),
	wxMenu:appendSeparator(View),
	wxMenu:appendCheckItem(View, 10019, "Show subresources", [{help, "Show additional feeds for feed entry, like comments"}]),
	wxMenu:appendSeparator(View),
	wxMenu:appendRadioItem(View, 10011, "Tree on left"),
	wxMenu:appendRadioItem(View, 10012, "Tree on right"),

	Help = wxMenu:new(),
	wxMenu:append(Help, ?wxID_ABOUT, "About"),

	wxMenuBar:append(MB, Sub, "&Subscriptions"),
	wxMenuBar:append(MB, View, "&View"),
	wxMenuBar:append(MB, Help, "&Help"),
	wxFrame:setMenuBar(Frame, MB),

	wxFrame:connect(Frame, command_menu_selected),

	SB = wxFrame:createStatusBar(Frame),

	MainPanel = wxPanel:new(Frame),

	Splitter = wxSplitterWindow:new(MainPanel),

	LeftPanel = wxPanel:new(Splitter),

	Tree = wxTreeCtrl:new(LeftPanel),
	wxTreeCtrl:setMinSize(Tree, {200, -1}),

	SubTree = wxTreeCtrl:addRoot(Tree, "Main", [{data, main}]),

	Feeds = erozja_manager:list_feeds(),
	lists:foreach(fun({URL, QueuePid}) ->
		Item = wxTreeCtrl:appendItem(Tree, SubTree, "URL"),
		Data = Item,
		AllStat = erozja_queue:subscribe(QueuePid, Data),
		{Title, Favicon, CountTotal, CountUnreaded, LastUpdate} = AllStat,
		Title1 = if
			CountUnreaded =:= 0 -> Title;
			true -> Title ++ " (" ++ integer_to_list(CountUnreaded) ++ ")"
		end,
		wxTreeCtrl:setItemText(Tree, Item, Title1),
		Data2 = {{URL, QueuePid}, AllStat},
		wxTreeCtrl:setItemData(Tree, Item, Data2),
		if
			CountUnreaded =/= 0 ->
				wxTreeCtrl:setItemBold(Tree, Item);
			true ->
				ok
		end
	end, Feeds),
	wxTreeCtrl:expand(Tree, SubTree),

	RightPanel = wxPanel:new(Splitter),

	Preview = wxHtmlWindow:new(RightPanel, [{style, ?wxHW_SCROLLBAR_AUTO}]),
	wxTreeCtrl:setMinSize(Preview, {400, -1}),

	wxHtmlWindow:setPage(Preview, "Erozja preview window! Select feed on left to show all entries for given feed."),

	Vbox = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(Vbox, Tree, [{flag, ?wxEXPAND}]),
	wxPanel:setSizer(LeftPanel, Vbox),

	Vbox2 = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(Vbox2, Preview, [{flag, ?wxEXPAND}]),
	wxPanel:setSizer(RightPanel, Vbox2),

	wxSplitterWindow:splitVertically(Splitter, LeftPanel, RightPanel, [{sashPosition, 150}]),
	wxSplitterWindow:setMinimumPaneSize(Splitter, 200),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

	%wxSizer:add(MainSizer, Tree, [{flag, ?wxALL bor ?wxEXPAND}]),
	%wxSizer:add(MainSizer, Preview, [{flag, ?wxALL bor ?wxEXPAND}]),

	wxSizer:add(MainSizer, Splitter, [{flag, ?wxEXPAND}]),

	wxPanel:setSizer(MainPanel, MainSizer),

	wxFrame:show(Frame),

	wxTreeCtrl:connect(Tree, command_tree_item_activated),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed),
	wxTreeCtrl:connect(Tree, command_right_click),

	wxHtmlWindow:connect(Preview, command_html_link_clicked),
	wxHtmlWindow:connect(Preview, command_left_click),
	wxHtmlWindow:connect(Preview, command_right_click),

	State = #state{win=Frame,tree=Tree,preview=Preview,sb=SB},

	wxToolTip:enable(true),
	wxToolTip:setDelay(500),

	{Frame, State#state{}}.

%% Handled as in normal gen_server callbacks
handle_info({Adder, W}, State = #state{adder=Adder}) when is_pid(Adder) ->
	case W of
		{add, URL} ->
			erozja_manager:add_feed(URL);
		cancel ->
			ok
	end,
	{noreply, State#state{adder=undefined}};
handle_info({erozja_queue, _From, Data, Msg}, State = #state{sb=SB,tree=Tree}) ->
%	?deb("msg from queue:~n  ~p~n", [Msg]),
	case Msg of
		{add_item, FeedItem} ->
			ok;
		{update_started, _LoaderPid} ->
			TreeItemNum = Data,
			FeedTitle = from_tree_title_or_url(TreeItemNum, Tree),
			wxStatusBar:setStatusText(SB, "Update started for "++FeedTitle),
			ok;
		{update_done, _LoaderPid, Status} ->
			TreeItemNum = Data,
			FeedTitle = from_tree_title_or_url(TreeItemNum, Tree),
			wxStatusBar:setStatusText(SB, "Update done for "++FeedTitle++" with status "++io_lib:format("~p", [Status]))
	end,
	{noreply, State};
handle_info({'EXIT', _, wx_deleted}, State) ->
	{noreply, State};
handle_info({'EXIT', _, normal}, State) ->
	{noreply, State};
handle_info(Msg, State) ->
	?deb("Unknown msg ~p~n", [Msg]),
	{noreply, State}.

handle_call(Msg, From, State) ->
	?deb("Unknown call ~p from ~p~n", [Msg, From]),
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
				"Erozja - Atom and RSS client and proxy.\nUsing Erlang and wxWidgets as GUI\n"
				"Authors: Witold Baryluk",
			wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
							[{style, ?wxOK bor ?wxICON_INFORMATION bor ?wxSTAY_ON_TOP},
							 {caption, "About"}])),
			{noreply, State};
		?wxID_EXIT ->
			{stop, normal, State};
		?wxID_NEW ->
			AdderPid = worker(fun(Parent) ->
				String = "Enter URL of website to automatically detect available feeds or direct RSS feed address if you know it",
				Dialog = wxTextEntryDialog:new(State#state.win, String,
								[{style, ?wxOK bor ?wxCANCEL bor ?wxSTAY_ON_TOP},
								 {caption, "New subscription"}]),
				R = fun(F, InitText) ->
					wxTextEntryDialog:setValue(Dialog, InitText),
					case wxTextEntryDialog:showModal(Dialog) of
						?wxID_OK ->
							NewURL = wxTextEntryDialog:getValue(Dialog),
							case NewURL of
								[] ->
									F(F, NewURL); % self recursion
								_ ->
									Parent ! {self(), {add, NewURL}}
							end;
						?wxID_CANCEL ->
							Parent ! {self(), cancel}
					end
				end,
				R(R, "")
			end),
			{noreply, State#state{adder=AdderPid}};
		10002 -> % new folder
			{noreply, State};
		10004 -> % import ompl
			{noreply, State};
		10005 -> % export ompl
			{noreply, State};
		_ ->
			?deb("Clicked menu item ~p~n", [Id]),
			{noreply, State}
	end;
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
	ok = wxFrame:setStatusText(Frame, "Closing...", []),
	{stop, normal, State};

handle_event(#wx{id = Id, event = #wxTree{type = command_tree_item_activated, item = ItemNum}}, State = #state{tree=Tree}) ->
	Data = wxTreeCtrl:getItemData(Tree, ItemNum),
%	?deb("  double click on item ~p ~n    with data ~p~n", [ItemNum, Data]),
	% something like forced update will be better,
	% for internal nodes in tree, update whole subtree
	wxTreeCtrl:setItemBold(Tree, ItemNum, [{bold, true}]),
	{noreply, State};

handle_event(#wx{id = Id, event = #wxTree{type = command_tree_sel_changed, item = ItemNum}}, State = #state{tree=Tree, preview=Preview, win=Frame}) ->
	ItemNum = wxTreeCtrl:getSelection(Tree),
	Data = wxTreeCtrl:getItemData(Tree, ItemNum),
	% show in Status Bar, number of items, URL, when lastly updated, and when will be next update, and status (ok/error) of last update
%	?deb("  selection on item ~p ~n    with data ~p~n", [ItemNum, Data]),
	case Data of
		{{URL, QueuePid}, {_, FeedTitle, _, _, _}} ->
			_WorkerPid = worker(fun(_Parent) ->
				Title = case FeedTitle of
					undefined -> URL;
					_ -> FeedTitle
				end,
				wxFrame:setTitle(Frame, Title ++ " - Erozja"),
				wxHtmlWindow:setPage(Preview, "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body><h1>"++Title++"</h1><br/>"),
				Items = erozja_queue:get_items(QueuePid),
				lists:foldl(fun(Item = #rss_item{title=ItemTitle, desc=ItemDesc}, _) ->
					wxHtmlWindow:appendToPage(Preview, "<h2>"++totext(ItemTitle)++"</h2><p>"++totext(ItemDesc)++"</p><br>")
				end, [], Items),
				wxHtmlWindow:appendToPage(Preview, "</body></html>")
			end);
		_ ->
			ignore % probably internal node of tree, not leaf
	end,
	{noreply, State};

	%wxTreeCtrl:selectItem(Tree, ItemNum)
	%wxTreeCtrl:setItemData(Tree, ItemNumber, Data)

handle_event(#wx{id = Id, event = #wxHtmlLink{type = command_html_link_clicked, linkInfo = LinkInfo}}, State = #state{}) ->
	#wxHtmlLinkInfo{href = Href} = LinkInfo,
	?deb("Clicked link ~p~n", [Href]),
	% IMPORTANT TODO: sanitize Href!
	spawn(fun() -> os:cmd("/usr/bin/sensible-browser '"++Href++"'") end),
	{noreply, State};

handle_event(#wx{id = Id, event = #wxCommand{type = command_left_click}}, State = #state{preview=Preview}) ->
	Text = wxHtmlWindow:selectionToText(Preview),
	?deb("Preview left clicked? text=~p~n", [Text]),
	{nreply, State};

handle_event(#wx{id = Id, event = #wxCommand{type = command_right_click}}, State = #state{preview=Preview}) ->
	Text = wxHtmlWindow:selectionToText(Preview),
	?deb("Preview right clicked? text=~p~n", [Text]),
	{nreply, State};


handle_event(Ev, State) ->
	?deb("Unknown event ~p~n", [Ev]),
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
	wx:destroy().


worker(Fun) ->
	Parent = self(),
	Env = wx:get_env(),
	Pid = spawn_link(fun() ->
		wx:set_env(Env),
		Fun(Parent)
	end),
	Pid.


totext(undefined) ->
	"";
totext(L) when is_list(L) ->
	L.

from_tree_title(ItemNum, Tree) ->
	Data = wxTreeCtrl:getItemData(Tree, ItemNum),
	case Data of
		{{URL, QueuePid}, {_, FeedTitle, _, _, _}} ->
			FeedTitle;
		_ -> undefined
	end.

from_tree_title_or_url(ItemNum, Tree) ->
	Data = wxTreeCtrl:getItemData(Tree, ItemNum),
	case Data of
		{{URL, QueuePid}, {_, FeedTitle, _, _, _}} ->
			case FeedTitle of
				undefined -> URL;
				_ -> FeedTitle
			end;
		_ -> undefined
	end.
