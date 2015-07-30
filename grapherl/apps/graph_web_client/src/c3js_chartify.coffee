#=============================================================================
# add graph using c3.js
#=============================================================================

c3_chartify =
  _create : ->
    @_state = {}


  _init: ->
    @_super()
    @options.moreYaxis = false
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

      # allow user to enable additional Y axis
      Form.find("#add_axis").on "click", (e) =>
        if Form.find("#add_axis").is(":checked")
          Form.find("#metric_select").prop('disabled', false)
        else
          Form.find("#metric_select").prop('disabled', true)

      Form.on "submit", (e) =>
        e.preventDefault()
        Toolbar.find("[data-toggle=config-chart-popover]").popover('hide')

        if Form.find("#add_axis").is(":checked")
          Id = Form.find("#metric_select").children(":selected").attr("id")
          console.log Id
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
        # y2:
        #   show: true

    if @options.moreYaxis != false then Args.axis['y2'] = {show: true}

    # display chart 
    chart = c3.generate(Args)
    # chart = c3.generate({
    #   bindto: "##{Id}"
    #   data  : Options 
    #   # transition:
    #   #   duration: 0
    #   padding:
    #     right: 50
    #   legend:
    #     position: 'bottom'
    #   axis:
    #     x:
    #       type: 'timeseries',
    #       tick:
    #         format: '%m-%d %H:%M:%S'
    #     y2:
    #       show: true
    # })
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
  transform_chart:(Type = "spline") ->
    @_state.chart.transform(Type);

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

    console.log List

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
            <button type="submit" class="btn btn-primary">Submit »</button>
          </div>
        </form>
      </div> """

