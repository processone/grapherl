#=============================================================================
# add graph using c3.js
#=============================================================================

c3_chartify =
  _create : ->
    @_state = {}


  _init: ->
    @_super()
    @options.moreYaxis = false
    @options.max_x_labels = if @options.split == true then 5 else 10
    @append_configrations()

  append_configrations: ->

    Li_element = """
      <li data-placement="bottom" data-toggle="config-chart-popover" data-title="Configure chart"
          data-container="body" type="button" data-html="true">
        <a href="#"> <i class="fa fa-bars"></i> </a></li> """

    Toolbar = @options.toolbar
    Toolbar.find(".chartify_hook").replaceWith(Li_element)
    Toolbar.find("[data-toggle=config-chart-popover]").popover({
      html: true
      placement: 'right'
      container: @element
      content: =>
        return $(c3_utils.chart_config(@options.data)).html()
    })

    Toolbar.find("[data-toggle=config-chart-popover]").on "shown.bs.popover", =>
      Form = @element.find(".popover").find("#chart-config")

      # check if the additional Y axis was added or not
      if @options.moreYaxis == false
        Form.find("#metric_select").prop('disabled', true)
      else
        Form.find("#add_axis").prop("checked", true)
        Form.find("#metric_select").find("##{@options.moreYaxis}").attr('selected', true)

      if @options.xgrid == true then Form.find("#add_grid_x").prop("checked", true)
      if @options.ygrid == true then Form.find("#add_grid_y").prop("checked", true)
      if @options.rotateAxis == true then Form.find("#rotate_axis").prop("checked", true)
      if @options.subchart == true then Form.find("#subchart").prop("checked", true)

      Form.find("#max_x_labels").val(@options.max_x_labels)

      # allow user to enable additional Y axis
      Form.find("#add_axis").on "click", (e) =>
        if Form.find("#add_axis").is(":checked")
          Form.find("#metric_select").prop('disabled', false)
        else
          Form.find("#metric_select").prop('disabled', true)

      Form.on "submit", (e) =>
        e.preventDefault()
        Toolbar.find("[data-toggle=config-chart-popover]").popover('hide')

        if Form.find("#add_grid_x").is(":checked") then @options.xgrid = true else @options.xgrid = false
        if Form.find("#add_grid_y").is(":checked") then @options.ygrid = true else @options.ygrid = false
        if Form.find("#rotate_axis").is(":checked") then @options.rotateAxis = true else @options.rotateAxis = false
        if Form.find("#subchart").is(":checked") then @options.subchart = true else @options.subchart = false

        Val = parseInt(Form.find("#max_x_labels").val())
        if Number.isInteger(Val) == true then @options.max_x_labels = Val

        if Form.find("#add_axis").is(":checked")
          Id = Form.find("#metric_select").children(":selected").attr("id")
          if Id == undefined
            @options.moreYaxis = false
          else
            @options.moreYaxis = Id
            @render_chart()
        else
          @options.moreYaxis = false
          @render_chart()



  render_chart : (Type = "line") ->
    # clear canvas before rendering
    if @_state.chart? then @_state.chart.destroy()

    Columns = []
    Xs      = {}

    for Metric, Clients of @options.data
      for Client, Data of Clients
        [Metric, X, D] = c3_utils.data_to_c3(Metric, Client, Data.data)
        Xs[D]          = X
        Columns.push(Metric.data)
        Columns.push(Metric.label)

    Options =
      xs: Xs,
      columns: Columns,
      type: 'spline'
      axes: {}

    if @options.moreYaxis != false then Options.axes[@options.moreYaxis] = 'y2'

    Id = @element.find(".chart").attr('id')
    Args =
      bindto: "##{Id}"
      data  : Options 
      # transition:
      #   duration: 0
      padding:
        right: 50
      legend:
        position: 'bottom'
      axis:
        x:
          type: 'timeseries',
          tick:
            format: '%m-%d %H:%M:%S'
            culling:
              max  : @options.max_x_labels

        rotated: false


      grid:
        x:
          show: false
        y:
          show: false

      subchart:
        show: false

    if @options.moreYaxis != false then Args.axis['y2'] = {show: true}

    if @options.xgrid == true then Args.grid.x.show = true
    if @options.ygrid == true then Args.grid.y.show = true
    if @options.rotateAxis == true then Args.axis.rotated = true
    if @options.subchart == true then Args.subchart.show = true

    # console.log "generating chart", Args
    # display chart 
    chart         = c3.generate(Args)
    @_state.chart = chart
    # if @options.moreYaxis != false
    #   Dict = {}
    #   Dict[@options.moreYaxis] = 'y2'
    #   @_state.chart.data.axes(Dict)

    return false


  # add a new metrics to the chart
  add_metric_data: (Metric, Client, Data) ->
    if !@_state.chart?
      @render_chart()
      return false

    Xs      = {}
    Columns = []

    [Metric, X, D] = c3_utils.data_to_c3(Metric, Client, Data)
    Xs[D]          = X
    Columns.push(Metric.data)
    Columns.push(Metric.label)

    @_state.chart.load({
        xs: Xs
        columns: Columns
        length: 0
      });

    return false


  update_metric_data: (Metric, Client, Data) ->
    Xs      = {}
    Columns = []

    [Metric, X, D] = c3_utils.data_to_c3(Metric, Client, Data)
    Xs[D]          = X
    Columns.push(Metric.data)
    Columns.push(Metric.label)

    @_state.chart.load({
      columns: Columns
    });
    return false


  removeMetric: (Metric, Client) ->
    @_state.chart.unload({
      ids: [Client + "-" + Metric]
    })

  # change chart display method
  transform_chart:(Type = "spline", Client, Metric) ->
    if Client == "all"
      @_state.chart.transform(Type);
    else
      @_state.chart.transform(Type, c3_utils.to_data_label(Client, Metric));

  saveDisplay: ->
    return false

  udpateChart: ->
    return false



c3_utils =
  to_data_label: (Client, Metric) ->
    return Client + "-" + Metric

  data_to_c3: (Metric, Client, Data) ->
    X      = "x-" + Metric + '-' + Client
    #D      = Client + "-" + Metric
    D      = c3_utils.to_data_label(Client, Metric)
    Label  = [X]
    Value  = [D]

    $.each Data, (Key, Val) =>
      Label.push(moment(Key * 1000).toDate())
      Value.push(parseFloat(Val))

    NewData =
      label : Label
      data  : Value

    return [NewData, X, D]

  chart_config: (Data) ->
    List = ""
    for Metric, Clients of Data
      for Client, Val of Clients
        Id = c3_utils.to_data_label(Client, Metric)
        Option = """ <option id="#{Id}">#{Metric} #{Client}</option> """
        List = List.concat(Option)

    return """
      <div class="hide">
        <form class="form" role="form" id="chart-config">
          <div class="form-group">
            <div class="checkbox">
              <label><input id="add_axis" type="checkbox" value="">
                Additional Y axis for
              </label>
            </div>
            <select class="form-control" id="metric_select">
              #{List}
            </select>
          </div>

          <div class="form-group">
            <div> <label> Grids :</label>
              <input id="add_grid_x" type="checkbox" value=""> X grid 
              <input id="add_grid_y" type="checkbox" value=""> Y grid
            </div>
          </div>

          <div class="form-group">
            <div class="checkbox">
              <label>
                <input id="rotate_axis" type="checkbox" value="">
                Rotate axis
              </label>
            </div>
          </div>

          <div class="form-group">
            <div class="checkbox">
              <label><input id="subchart" type="checkbox" value="">
                Subchart display
              </label>
            </div>
          </div>

          <div class="form-group">
            <div class="checkbox">
              <label> Max x-axis labels
                <input id="max_x_labels" type="number" min=1>
              </label>
            </div>
          </div>

          <div class="form-group">
            <button type="submit" class="btn btn-primary">Submit »</button>
          </div>
        </form>
      </div> """

