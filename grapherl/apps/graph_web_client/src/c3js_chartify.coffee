#=============================================================================
# add graph using c3.js
#=============================================================================
c3_chartify =
  _create : ->
    @_state = {}


  _init: ->
    @_super()
    if !@options.config?           then @options.config = {}
    if !@options.config.moreYaxis? then @options.config.moreYaxis = false
    if !@options.config.y_format?  then @options.config.y_format = ""
    if !@options.config.max_x_labels?
      @options.config.max_x_labels = if @options.split == true then 5 else 10
    if !@options.config.disp_points? then @options.config.disp_points = false

    @append_configrations()


  # add configrations specific to c3.js to display toolbar
  append_configrations: ->

    Li_element = """
      <li data-placement="bottom" data-toggle="config-chart-popover" data-title="Configure chart"
          data-container="body" type="button" data-html="true">
        <a href="#"> <i class="fa fa-bars"></i> </a></li> """

    # create popover element to display configration form
    Toolbar = @options.toolbar
    Toolbar.find(".chartify_hook").replaceWith(Li_element)
    Toolbar.find("[data-toggle=config-chart-popover]").popover({
      html: true
      placement: 'right'
      container: @element
      content: =>
        return $(c3_utils.chart_config(@options)).html()
    })

    # display popover
    Toolbar.find("[data-toggle=config-chart-popover]").on "shown.bs.popover", =>
      Form = @element.find(".popover").find("#chart-config")

      #
      # load previous config when popover is displayed
      if @options.config.moreYaxis == false
        Form.find("#metric_select").prop('disabled', true)
      else
        Form.find("#add_axis").prop("checked", true)
        Form.find("#metric_select").find("##{@options.config.moreYaxis}").attr('selected', true)

      if @options.config.xgrid == true then Form.find("#add_grid_x").prop("checked", true)
      if @options.config.ygrid == true then Form.find("#add_grid_y").prop("checked", true)
      if @options.config.rotateAxis == true then Form.find("#rotate_axis").prop("checked", true)
      if @options.config.subchart == true then Form.find("#subchart").prop("checked", true)
      if @options.config.subchart == true then Form.find("#subchart").prop("checked", true)
      if @options.config.disp_points == true  then Form.find("#disp_points").prop("checked", true)

      Form.find("#max_x_labels").val(@options.config.max_x_labels)

      # allow user to enable additional Y axis
      Form.find("#add_axis").on "click", (e) =>
        if Form.find("#add_axis").is(":checked")
          Form.find("#metric_select").prop('disabled', false)
        else
          Form.find("#metric_select").prop('disabled', true)

      Form.find("#format_y_axis").on "change", (e) =>
        if Form.find("#format_y_axis").children(":selected").attr("value") == 'custom'
          Form.find("#custom_y_format").css('display', 'inline-block')
        else          
          Form.find("#custom_y_format").hide()

      # set new config values on form submit
      Form.on "submit", (e) =>
        e.preventDefault()
        # extract format for y-axis
        Val = Form.find("#format_y_axis").children(":selected").attr("value")
        if Val == "custom"
          CustomFormat = Form.find("#custom_y_format").val()
          @options.config.y_format = CustomFormat
        else
          @options.config.y_format = Val


        Toolbar.find("[data-toggle=config-chart-popover]").popover('hide')

        if Form.find("#add_grid_x").is(":checked") then @options.config.xgrid = true else @options.config.xgrid = false
        if Form.find("#add_grid_y").is(":checked") then @options.config.ygrid = true else @options.config.ygrid = false
        if Form.find("#rotate_axis").is(":checked") then @options.config.rotateAxis = true else @options.config.rotateAxis = false
        if Form.find("#subchart").is(":checked") then @options.config.subchart = true else @options.config.subchart = false
        if Form.find("#disp_points").is(":checked") then @options.config.disp_points = true else @options.config.disp_points = false

        Val = parseInt(Form.find("#max_x_labels").val())
        if Number.isInteger(Val) == true then @options.config.max_x_labels = Val

        if Form.find("#add_axis").is(":checked")
          Id = Form.find("#metric_select").children(":selected").attr("id")
          if Id == undefined
            @options.config.moreYaxis = false
          else
            @options.config.moreYaxis = Id
            @render_chart()
        else
          @options.config.moreYaxis = false
          @render_chart()


  # generate chart
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
      type: 'line'
      axes: {}

    if @options.config.moreYaxis != false then Options.axes[@options.config.moreYaxis] = 'y2'
    Id = @element.find(".chart").attr('id')


    YFormat =
      if @options.config.y_format == "data_size"
        c3_utils.bytesToString
      else
        d3.format(@options.config.y_format)
      

    Args =
      bindto: "##{Id}"
      data  : Options 
      point :
        show: @options.config.disp_points
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
              max  : @options.config.max_x_labels

        y:
          tick:
            format: (d) =>
              return YFormat(d)

        rotated: false


      grid:
        x:
          show: false
        y:
          show: false

      subchart:
        show: false

    if @options.config.moreYaxis != false then Args.axis['y2'] = {show: true}

    if @options.config.xgrid == true then Args.grid.x.show = true
    if @options.config.ygrid == true then Args.grid.y.show = true
    if @options.config.rotateAxis == true then Args.axis.rotated = true
    if @options.config.subchart == true then Args.subchart.show = true

    # console.log "generating chart", Args
    # display chart 
    chart         = c3.generate(Args)
    @_state.chart = chart

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


  # load new data for metrics
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


  # delete metrics from display
  removeMetric: (Metric, Client) ->
    Id = c3_utils.to_data_label(Client, Metric)
    @_state.chart.unload({
      ids: [Id]
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
    return Client + " (" + Metric + ")"
    #return Client + "-" + Metric

  # taken from stackoverflow (muscially_ut)
  bytesToString: (bytes) ->
    fmt = d3.format('.0f')
    if bytes < 1024
      return fmt(bytes) + 'B'
    else if bytes < 1024 * 1024
      return fmt(bytes / 1024) + 'kB'
    else if bytes < 1024 * 1024 * 1024
      return fmt(bytes / 1024 / 1024) + 'MB'
    else if bytes < 1024 * 1024 * 1024 * 1024
      return fmt(bytes / 1024 / 1024 / 1024) + 'GB'
    else
      return fmt(bytes / 1024 / 1024 / 1024 /1024 ) + 'TB'



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


  # generate config form for chart
  chart_config: (Opts) ->
    Data = Opts.data
    List = ""
    for Metric, Clients of Data
      for Client, Val of Clients
        Id = c3_utils.to_data_label(Client, Metric)
        Option = """ <option id="#{Id}">#{Metric} #{Client}</option> """
        List = List.concat(Option)

    FS = ""
    FormatStyles =
      "None"      : ""
      "Currency"  : "$,"
      "Percentage": ".2%"
      "Data size" : "data_size"
      "Float"     : ".3g"

    for Key, Val of FormatStyles 
      if Opts.config.y_format == Val
        FS = FS.concat(""" <option value="#{Val}" selected> #{Key} </option> """)
      else
        FS = FS.concat(""" <option value="#{Val}"> #{Key}</option> """)

    FS = FS.concat(""" <option value="custom"> Custom </option> """)

    return """
      <div class="hide">
        <form class="form" role="form" id="chart-config">
          <div class="form-group">
            <div class="checkbox">
              <label><input id="add_axis" type="checkbox" value="">
                Additional Y axis for
              </label>
            </div>
            <select class="form-control" id="metric_select" style="width: 90%; margin-left: 20px;">
              #{List}
            </select>
          </div>

          <div class="form-group">
            <div class="checkbox"> <label> Grids :</label>
              <span> X grid: </span>
              <span> <input id="add_grid_x" type="checkbox" value=""
               style="margin-left: 0px;"> </span>

              <span style="margin-left: 20px;"> Y grid: </span>
              <span> <input id="add_grid_y" type="checkbox" value=""
               style="margin-left: 5px;"> </span>
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
              <label><input id="disp_points" type="checkbox" value="">
                Circle points
              </label>
            </div>
          </div>

          <div class="form-group">
            <div class="checkbox">
              <label> Max X axis labels </label>
                <input id="max_x_labels" class="form-control" type="number" min=1
                style="display: inline; margin-left: 10px; width: 30%;">

            </div>
          </div>

          <div class="form-group">
            <div class="checkbox">
              <label> Y axis label format </label>
            </div>
            <div class="checkbox" style="padding-left: 20px;">
              <select class="form-control" id="format_y_axis"
              style="display: inline; width: 40%;">
                #{FS}
              </select>
              <input id="custom_y_format" class="form-control" placeholder="Format"
              style="display: none; max-width: 40%;">
              <a onclick="window.open('http://koaning.s3-website-us-west-2.amazonaws.com/html/d3format.html', '_blank').focus()"
               style="font-size: small; cursor: pointer;"> Help </a>
            </div>


          </div>

          <div class="form-group" style="padding-left: 20px;">
            <button type="submit" class="btn btn-primary">Submit »</button>
          </div>
        </form>
      </div> """

