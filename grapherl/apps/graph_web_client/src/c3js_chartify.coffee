#=============================================================================
# add graph using c3.js
#=============================================================================

c3_chartify =
  _create : ->
    @_state = {}


  _init: ->
    @_super()


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

    Id = @element.find(".chart").attr('id')
    # display chart 
    chart = c3.generate({
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
    })
    @_state.chart = chart

    # AddMore = () =>
    #   Data =
    #     metric3:
    #       label: ['2013-01-01', '2013-01-02', '2013-01-03', '2013-01-04', '2013-01-05', '2013-01-07'],
    #       data : [ 400, 500, 450, 700, 600, 500]
    #   @addMetric(Data)

    #setTimeout(AddMore, 1000)
    # AppendData = () =>
    #   NewVal = Math.floor(Math.random() * 200)
    #   Next   = new Date()
    #   @options.data.metric1.data.push(NewVal)
    #   @options.data.metric1.label.push(Next)

    #   @_state.chart.load({
    #     columns: [
    #       ["x"    + @options.data.metric1.id].concat(@options.data.metric1.label),
    #       ["data" + @options.data.metric1.id].concat(@options.data.metric1.data)
    #     ]
    #   });

    # @options.data.metric1.data = []
    # @options.data.metric1.label = []
    #setInterval(AppendData, 1000)
    #@element.trigger("display_update")
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
  data_to_c3: (Metric, Client, Data) ->
    X      = "x-" + Metric + '-' + Client
    D      = Client + "-" + Metric
    Label  = [X]
    Value  = [D]

    $.each Data, (Key, Val) =>
      Label.push(moment(Key * 1000).toDate())
      Value.push(Val)

    NewData =
      label : Label
      data  : Value

    return [NewData, X, D]
