#=============================================================================
# sidebar plugin
#=============================================================================
sidebar =
  _create: ->
    return false


  _init: ->
    @options.display = 'client'
    @_bind_sidebar_events()
    if @options.data?
      $(document).trigger("ui.update_sideBar", [@options.data])
    else
      @element.find("#sidebar-refresh").click()
    return false


  _bind_sidebar_events: ->
    $(document).on "ui.update_sideBar", (e, Data) =>
      @element.find("#active-metrics").empty()
      @options.data = Data
      @_add_sidebar_elements(Data)
      @_bind_select_metric()

    $(document).on "selectionDone", =>
      @element.find(".selected").find("a").css("color", "")
      @element.find(".selected").effect("highlight", {color: "#ffcdd2"}, 1000)
      Selected = []
      for ele in  @element.find(".selected").closest("li")
        MetricData =
          metric_name: $(ele).attr('data-metric')
          client_name: $(ele).attr('data-client')
        Selected.push(MetricData)
        #Selected.push($(ele).attr('id'))

      @element.find(".selected").removeClass("selected")
      @options.selected = Selected
      @_unbind_multi_selection()
      @_bind_select_metric()

    $(document).on "selectionStart", =>
      # cancel any previous selection gonig on
      $.event.trigger('selectionCancel')
      @_unbind_select_metric()
      @_unbind_multi_selection()
      @_multi_selection()

    @element.find('#sidebar-refresh').on "click", =>
        $.ajax(
          method: "GET"
          url   : "/metric/list"
          success: (data) ->
            #console.log data
            $.event.trigger('ui.update_sideBar', [data.metric_list])
        )

    @element.find("#sb-disp-client").on "click", (e) =>
      @options.display = 'client'
      $(document).trigger("ui.update_sideBar", [@options.data])
    

    @element.find("#sb-disp-metric").on "click", (e) =>
      @options.display = 'metric'
      $(document).trigger("ui.update_sideBar", [@options.data])


    SideBarRefresh = =>
      @element.find("#sidebar-refresh").click()
    setInterval(SideBarRefresh, 60000);


  get_selected_metric: ->
    return @options.selected


  _bind_select_metric: ->
    @element.find('li.client').on "click.select", (e) =>
      Metric = $(e.currentTarget).attr('data-metric')
      Client = $(e.currentTarget).attr('data-client')
      Data = {}
      Data[Metric] = {}
      Data[Metric][Client] = {data: []}
      graph_utils.add_display({data: Data})
      # TODO trigger to update with display with the selected metric


  _unbind_select_metric: ->
    @element.find('li.client').unbind("click.select")


  _unbind_multi_selection: ->
    @element.find("li").unbind("click.multi_select")
    @element.find(".multi-select").remove()
    @element.find(".multi-selected").remove()


  _add_sidebar_elements: (Data) ->
    List = @element.find("#active-metrics")
    Data2 = {}

    if @options.display == "client"
      # convert from metric -> client to client -> metric
      for Metric, Clients of Data
        for Client in Clients
          if Data2[Client]? then Data2[Client].push(Metric) else Data2[Client] = [Metric]

      # $.each Data2, (Client, Metrics) =>
      Clients = Object.keys(Data2).sort()
      for Client in Clients
        List.append("""
          <li title="#{Client}" class="metric disabled">
            <a href="#" style="text-align: center;">#{Client}</a>
          </li> """)
        Metrics = Data2[Client].sort()
        for Metric in Metrics
          List.append(UI.sideBar_li(Metric, Client, Metric))

    else
      #$.each Data, (Metric, Value) =>
      Metrics = Object.keys(Data).sort()
      for Metric in Metrics
        List.append("""
          <li title="#{Metric}" class="metric disabled">
            <a href="#" style="text-align: center;">#{Metric}</a>
          </li> """)
        Value = Data[Metric].sort()
        for Client in Value
          List.append(UI.sideBar_li(Metric, Client, Client))


  _multi_selection: ->
    @options.selected = []
    #@element.find("li a").prepend("""<i class="fa fa-square-o multi-select"></i> """)
    @element.find("li").on "click.multi_select", (e) =>
      Clicked_li = @element.find("li##{e.currentTarget.id}")
      if Clicked_li.hasClass("selected") == false
        Clicked_li.find("a").css("color", "#f44336")
        Clicked_li.addClass("selected")
      else
        Clicked_li.removeClass("selected")
        Clicked_li.find("a").css("color", "")



#=============================================================================
# bind events to global buttons
#=============================================================================
init_global_buttons = ->
  GlobalToolbar = UI.globalToolbar()
  GlobalToolbar.find("#addDisplay").on "click", =>
    graph_utils.add_display()

  GlobalToolbar.find("#addSplitDisplay").on "click", =>
    graph_utils.add_split_display()

  GlobalToolbar.find("#bookmarkWindow").on "click", =>
    graph_utils.save_display()

  $(UI.graphDiv()).find("#load-bookmark").on "change", (e) =>
    Bookmark = e.target.files[0]
    if Bookmark
      fd = new FileReader()
      fd.onload = (e) ->
        graph_utils.load_display(JSON.parse(e.target.result))
      fd.readAsText(Bookmark)
