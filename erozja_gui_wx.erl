-module(erozja_gui_wx).
-author('baryluk@smp.if.uj.edu.pl').

-include_lib("wx/include/wx.hrl").

% we are using wxWidgets here
% for Erlang, beyond wx, there is also etk (Erlang Tk from Tcl/Tk), gs, x11, ex11.
%


-behaviour(wx_object).

-export([start/0, start_link/0, stop/0]).
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_call/3, handle_event/2]).

-record(state, {win, tree, subtree, preview, adder, sb}).

-record(data, {url, pid, stats, last_status}).

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
	wxMenu:append(Sub, 10002, "New folder...", [{help, "Create new folder, in which you can place feeds or other folders, to organize your feeds into tree hierarchy"}]),
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
	wxMenu:appendRadioItem(View, 10016, "Condensated view", [{help, "Single panel: Rendered as list with both headers (title, date), and content. Status rendered using background"}]),
	wxMenu:appendRadioItem(View, 10014, "Normal view", [{help, "Two panels: list of entries (title, date, status) on top, and entry view at bottom"}]),
	wxMenu:appendRadioItem(View, 10015, "Wide view", [{help, "Two panels: list of entries (title, date, status) on left, and entry view at right - good for wide screens"}]),
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
	%wxTreeCtrl:setMinSize(Tree, {200, -1}),

	SubTree = wxTreeCtrl:addRoot(Tree, "Main", [{data, main}]),

	Feeds = erozja_manager:list_feeds(),
	lists:foreach(fun({URL, QueuePid}) ->
		add_to_tree(Tree, SubTree, {URL, QueuePid})
	end, Feeds),
	wxTreeCtrl:expand(Tree, SubTree),

	RightPanel = wxPanel:new(Splitter),

	Preview = wxHtmlWindow:new(RightPanel, [{style, ?wxHW_SCROLLBAR_AUTO}]),
	%wxTreeCtrl:setMinSize(Preview, {400, -1}),

	wxHtmlWindow:setPage(Preview, "Erozja preview window! Select feed on left to show all entries for given feed. You can double click on tree elements to force update of given feed (or all subfeeds). When updating tree items are show in bold. Red color indicate there was some kind of error."),

	Vbox = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:add(Vbox, Tree, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxPanel:setSizer(LeftPanel, Vbox),

	Vbox2 = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(Vbox2, Preview, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxPanel:setSizer(RightPanel, Vbox2),

	wxSplitterWindow:splitVertically(Splitter, LeftPanel, RightPanel, [{sashPosition, 150}]),
	wxSplitterWindow:setMinimumPaneSize(Splitter, 150),

	wxSplitterWindow:setSashGravity(Splitter, 0.1),

	MainSizer = wxBoxSizer:new(?wxVERTICAL),

	%wxSizer:add(MainSizer, Tree, [{flag, ?wxALL bor ?wxEXPAND}]),
	%wxSizer:add(MainSizer, Preview, [{flag, ?wxALL bor ?wxEXPAND}]),

	wxSizer:add(MainSizer, Splitter, [{flag, ?wxEXPAND}, {proportion, 1}]),

	wxPanel:setSizer(MainPanel, MainSizer),

	wxFrame:show(Frame),

	wxTreeCtrl:connect(Tree, command_tree_item_activated),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed),
	wxTreeCtrl:connect(Tree, command_right_click),

	wxHtmlWindow:connect(Preview, command_html_link_clicked),
	wxHtmlWindow:connect(Preview, command_left_click),
	wxHtmlWindow:connect(Preview, command_right_click),

	State = #state{win=Frame,tree=Tree,subtree=SubTree,preview=Preview,sb=SB},

	wxToolTip:enable(true),
	wxToolTip:setDelay(500),

	{Frame, State#state{}}.

add_to_tree(Tree, SubTree, {URL, QueuePid}) ->
	TreeItemNum = wxTreeCtrl:appendItem(Tree, SubTree, "URL"),
	Data = TreeItemNum,
	AllStat = erozja_queue:subscribe(QueuePid, Data),
	{Title, Favicon, CountTotal, CountUnreaded, LastUpdate} = AllStat,
	Title1 = if
		CountUnreaded =:= 0 -> Title;
		true -> Title ++ " (" ++ integer_to_list(CountUnreaded) ++ ")"
	end,
	wxTreeCtrl:setItemText(Tree, TreeItemNum, Title1),
	Data2 = {{URL, QueuePid}, AllStat, ""},
	wxTreeCtrl:setItemData(Tree, TreeItemNum, Data2),
	if
		CountUnreaded =/= 0 ->
			wxTreeCtrl:setItemBold(Tree, TreeItemNum);
		true ->
			ok
	end,
	Img1 = wxImage:new("icon1.png"),
	%wxTreeCtrl:setItemImage(Tree, TreeItemNum, Img1),
	ok.

%% Handled as in normal gen_server callbacks
handle_info({Adder, W}, State = #state{adder=Adder,tree=Tree,subtree=SubTree}) when is_pid(Adder) ->
	case W of
		{add, URL} ->
			case erozja_manager:add_feed(URL) of
				{ok, QueuePid} ->
					add_to_tree(Tree, SubTree, {URL, QueuePid})
			end;
		cancel ->
			ok
	end,
	{noreply, State#state{adder=undefined}};
handle_info({erozja_queue, _From, Data, Msg}, State = #state{sb=SB,tree=Tree}) ->
%	?deb("msg from queue:~n  ~p~n", [Msg]),
	case Msg of
		{new_item, FeedItem} ->
			ok;
		{update_started, _LoaderPid} ->
			TreeItemNum = Data,
			FeedTitle = from_tree_title_or_url(TreeItemNum, Tree),
			wxTreeCtrl:setItemBold(Tree, TreeItemNum, [{bold, true}]),
			wxStatusBar:setStatusText(SB, "Update started for "++FeedTitle),
			ok;
		{update_done, _LoaderPid, Diff, Status} ->
			TreeItemNum = Data,
			FeedTitle = from_tree_title_or_url(TreeItemNum, Tree),
			wxTreeCtrl:setItemBold(Tree, TreeItemNum, [{bold, false}]),
			case Status of
				{ok, _} ->
					wxTreeCtrl:setItemTextColour(Tree, TreeItemNum, {255,255,255});
				_ ->
					wxTreeCtrl:setItemTextColour(Tree, TreeItemNum, {255,0,0})
			end,
			wxStatusBar:setStatusText(SB, "Update done for "++FeedTitle++" with status "++lists:flatten(io_lib:fwrite("~10000p", [Status]))++" after "++integer_to_list(Diff)++"ms"),
			% save/update status of last update in treeitem data
			% 
			ok
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
		10001 ->
			erozja_manager:force_update_all(),
			{noreply, State};
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

handle_event(#wx{id = _Id, event = #wxTree{type = command_tree_item_activated, item = TreeItemNum}}, State = #state{tree=Tree}) ->
	maybe_update(TreeItemNum, Tree),
	{noreply, State};

handle_event(#wx{id = _Id, event = #wxTree{type = command_tree_sel_changed, item = TreeItemNum}}, State = #state{tree=Tree, preview=Preview, win=Frame}) ->
	%TreeItemNum = wxTreeCtrl:getSelection(Tree),
	Data = wxTreeCtrl:getItemData(Tree, TreeItemNum),
	% show in Status Bar, number of items, URL, when lastly updated, and when will be next update, and status (ok/error) of last update
%	?deb("  selection on item ~p ~n    with data ~p~n", [ItemNum, Data]),
	case Data of
		{{FeedURL, QueuePid}, {_, FeedTitle, _, _, _}, _} ->
			_WorkerPid = worker(fun(_Parent) ->
				Title = case FeedTitle of
					undefined -> FeedURL;
					_ -> FeedTitle
				end,
				wxFrame:setTitle(Frame, Title ++ " - Erozja"),
				wxHtmlWindow:setPage(Preview, ["<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body><h2><a href='",FeedURL,"'>",Title,"</a></h2><br/>"]),
				wxHtmlWindow:appendToPage(Preview, "<ul>"),
				ShowDesc = false,
				Items = erozja_queue:get_items(QueuePid),
				{_, Content0} = wx:foldl(fun(Item = #rss_item{title=ItemTitle, desc=ItemDesc, link=ItemURL, pubdate=ItemPubDate, author=ItemAuthor}, {C, Acc}) ->
					if
					  C < 100 ->
					    %Text = ["<h5><a href='",totext(ItemURL),"'>",totext(ItemTitle),"</a></h5><div>at ",datetotext(ItemPubDate)," by ",totext(ItemAuthor),"</div><div>",totext(ItemDesc),"</div>"];
					    Text = ["<li><a href='",totext(ItemURL),"'>"
					    	,totext(ItemTitle),"</a>"
					    	,case ItemPubDate of undefined -> ""; _ -> [" submitted ",datetotext(ItemPubDate)] end
					    	,case ItemAuthor of undefined -> ""; _ -> [" by ",totext(ItemAuthor)] end
					    	,if ShowDesc -> [" <div>",totext(ItemDesc),"</div>"]; true -> "" end
					    	,"</li>"
					    	],
					    %wxHtmlWindow:appendToPage(Preview, Text),
					    {C+1, [Text | Acc]};
					  true -> {C+1, Acc}
					end
				end, {0, []}, Items),
				Content = lists:reverse(Content0),
				wxHtmlWindow:appendToPage(Preview, ["<ul>",Content,"</ul>"]),
				%wxHtmlWindow:appendToPage(Preview, "</ul>"),
				wxHtmlWindow:appendToPage(Preview, "</body></html>")
			end);
		_ ->
			ignore % probably internal node of tree, not leaf
	end,
	{noreply, State};

	%wxTreeCtrl:selectItem(Tree, ItemNum)
	%wxTreeCtrl:setItemData(Tree, ItemNumber, Data)

handle_event(#wx{id = _Id, event = #wxHtmlLink{type = command_html_link_clicked, linkInfo = LinkInfo}}, State = #state{}) ->
	#wxHtmlLinkInfo{href = Href} = LinkInfo,
	?deb("Clicked link ~p~n", [Href]),
	% IMPORTANT TODO: sanitize Href!
	spawn(fun() -> os:cmd("/usr/bin/sensible-browser '"++Href++"'") end),
	{noreply, State};

handle_event(#wx{id = _Id, event = #wxCommand{type = command_left_click}}, State = #state{preview=Preview}) ->
	Text = wxHtmlWindow:selectionToText(Preview),
	?deb("Preview left clicked? text=~p~n", [Text]),
	{noreply, State};

handle_event(#wx{id = _Id, event = #wxCommand{type = command_right_click}}, State = #state{preview=Preview}) ->
	Text = wxHtmlWindow:selectionToText(Preview),
	?deb("Preview right clicked? text=~p~n", [Text]),
	{noreply, State};


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
	L;
totext(B) when is_binary(B) ->
	B.

datetotext(undefined) ->
	"";
datetotext(Timestamp) when is_integer(Timestamp), Timestamp > 0 ->
	DT = calendar:gregorian_seconds_to_datetime(Timestamp),
	io_lib:format("~p", [DT]);
datetotext(Timestamp) ->
	"?DATE_PARSING_ERROR".

% gets title for given tree item or undefined
from_tree_title(TreeItemNum, Tree) ->
	Data = wxTreeCtrl:getItemData(Tree, TreeItemNum),
	case Data of
		{{_URL, _QueuePid}, {_, FeedTitle, _, _, _}, _} ->
			FeedTitle;
		_ -> undefined
	end.

% gets title or url for given tree item or undefined
from_tree_title_or_url(TreeItemNum, Tree) ->
	Data = wxTreeCtrl:getItemData(Tree, TreeItemNum),
	case Data of
		{{URL, _QueuePid}, {_, FeedTitle, _, _, _}, _} ->
			case FeedTitle of
				undefined -> URL;
				_ -> FeedTitle
			end;
		_ -> undefined
	end.

% gets queue pid for given tree item or undefined
from_tree_queuepid(TreeItemNum, Tree) ->
	Data = wxTreeCtrl:getItemData(Tree, TreeItemNum),
	case Data of
		{{_URL, QueuePid}, {_, _FeedTitle, _, _, _}, _} ->
			QueuePid;
		_ -> undefined
	end.

% updates all feeds for given tree item (possibly recursivly)
maybe_update(TreeItemNum, Tree) ->
	case from_tree_queuepid(TreeItemNum, Tree) of
		QueuePid when is_pid(QueuePid) ->
			erozja_queue:force_update(QueuePid);
		_ ->
			case wxTreeCtrl:itemHasChildren(Tree, TreeItemNum) of
				true ->
					Count = wxTreeCtrl:getChildrenCount(Tree, TreeItemNum),
					{TreeItemNum2, Cookie2} = wxTreeCtrl:getFirstChild(Tree, TreeItemNum),
					maybe_update(TreeItemNum2, Tree),
					lists:foldl(fun(_, {PrevTreeItemNum, PrevCookie}) ->
						R = wxTreeCtrl:getNextChild(Tree, TreeItemNum, PrevCookie),
						{TreeItemNum3, Cookie3} = R,
						maybe_update(TreeItemNum3, Tree),
						R
					end, {TreeItemNum2, Cookie2}, lists:seq(1, Count-1));
				false ->
					ok
			end
	end.
