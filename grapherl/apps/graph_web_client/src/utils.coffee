graph_utils =
  generate_id: ->
    $("<div></div>").uniqueId().attr('id')

  add_display: (MetricData) ->
    GraphFrame   = $(UI.graphDiv())
    NewDisplay   = $(UI.graphNew())
    NewDisplay.find("#display").chartify({data: MetricData})
    GraphFrame.append(NewDisplay)

  add_split_display: ->
    GraphFrame   = $(UI.graphDiv())
    NewDisplay   = $(UI.graphNewSplit())
    NewDisplay.find("#display").chartify()
    GraphFrame.append(NewDisplay)
    return GraphFrame

  get_interval: (Granularity) ->
    if Granularity == graph_utils.granularity.sec
      return 15000
    else if Granularity == graph_utils.granularity.min
      return 60000
    else if Granularity == graph_utils.granularity.hour
      return 3600000
    else
      return 3600000


  granularity:
    sec  : "sec"
    min  : "min"
    hour : "hour"
    day  : "day"
    week : "week"
    month: "month"
    

## contains all functions that genereate UI elements
UI =
  # global button
  globalToolbar : ->
    return $("#global-nav")


  # global sidebar
  sideBar: ->
    return $("#sidebar")


  sideBar_metricList : ->
    return $("#active-metrics")


  # elements in global sidebar
  sideBar_li : (Metric, Client) ->
    Id = graph_utils.generate_id()
    return """
      <li title="#{Client}" data-metric="#{Metric}" data-client="#{Client}"
        class="client" id="#{Id}">
        <a href="#" style="padding: 5px;">  #{Client}</a>
      </li> """

  graphDiv : ->
    return $("#graphDiv")


  graphNew : ->
    return """
      <div class="row">
        <div class="col-md-12" id="display" ></div>
      </div> """


  graphNewSplit : ->
    return """
      <div class="row" >
        <div class="col-md-6" > <div id="display"></div> </div>
        <div class="col-md-6" > <div id="display"></div> </div>
      </div> """

  graphDisplayC3: (DispId, ChartId) ->
    Frame = $(UI.graphFrame(DispId))
    Id    = graph_utils.generate_id()
    Frame.append("""<div class="chart" id="#{Id}"></div>""")
    Frame.append(UI.graphToolbar())
    return Frame


  graphDisplayCharjs: (DispId, ChartId) ->
    Frame = $(UI.graphFrame(DispId))
    Frame.append(UI.graphCanvas(ChartId))
    Frame.append(UI.graphToolbar())
    return Frame

    
  graphFrame: (Id="display1") ->
    return """<div class="well graphDisplay" id="#{Id}"></div> """


  graphCanvas: (Id="chart") ->
    return """<div> <canvas id="#{Id}"></canvas> </div> """


  graphToolbar_intervalFrom: (Value) ->
    return """
      <div id="popover-content" class="hide">
        <form class="form-inline" role="form" id="update-interval">
          <div class="form-group">
            <input id="interval" value="#{Value}" type="number"
             placeholder="Seconds" class="form-control" maxlength="5"
              type="text" style="max-width:120px;">
            <button type="submit" class="btn btn-primary">Update »</button>
          </div>
        </form>
      </div> """


  graphToolbar: ->
    return """
      <hr style="border-color:rgba(179, 179, 179, 0.6); margin:0px;">

      <nav class="navbar navbar-fixed-bottom" style="position:relative;">
        <div class="container-fluid">


        <!-- Collect the nav links, forms, and other content for toggling -->
          <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">

            <ul class="nav navbar-nav">
              <li class="dropdown" id="chart_type" >
                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true"
                  aria-expanded="false"><i class="fa fa-gear"></i></a>
                <ul class="dropdown-menu">
                  <li id="bar"><a href="#"><i class="fa fa-bar-chart"></i> Bar chart</a></li>
                  <li id="line"><a href="#"><i class="fa fa-line-chart"></i> Line chart</a></li>
                  <li id="spline"><a href="#"><i class="fa fa-line-chart"></i> Spline chart</a></li>
                  <!-- <li><a href="#"><i class="fa fa-pie-chart"></i> Pie chart</a></li> -->
                </ul>
              </li>
              <li title="Specify interval" id="range_picker" ><a href="#"><i class="fa fa-clock-o"></i></a></li>

              <!-- <li title="Duplicate display" id="duplicate"><a href="#"><i class="fa fa-copy"></i></a></li> -->
              <li title="Add more metric" id="addMetric"><a href="#"><i class="fa fa-plus-square-o"></i></a></li>
              <li title="Add more metric" id="selectionDone" style="display:none;"><a href="#"><i class="fa fa-square-o"></i></a></li>

              <li class="dropdown" title="Remove metric" id="removeMetric">
                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true"
                  aria-expanded="false"><i class="fa fa-minus-square-o"></i></a>
                <ul class="dropdown-menu" id="currentDispMetric">
                  <li><a href="#"><i class="fa fa-pie-chart"></i> Pie chart</a></li>
                </ul>
              </li>

              <li title="Go live" id="update_metrics" >
                 <a href="#">
                    <i class="fa fa-refresh live"></i>

                    <span  data-placement="bottom" data-toggle="update-interval-popover" data-title="Change udpate interval"
                      data-container="body" type="button" data-html="true" href="#">
                      <i class="fa fa-angle-down"></i>
                    </span>
                </a>
              </li>


              <li class="dropdown" title="Granularity">
                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true"
                  aria-expanded="false"><i class="fa fa-sitemap"></i></a>
                <ul class="dropdown-menu" id="granularity">
                  <li id="#{graph_utils.granularity.sec}" ><a href="#">Seconds</a></li>
                  <li id="#{graph_utils.granularity.min}" ><a href="#">Minutes</a></li>
                  <li id="#{graph_utils.granularity.hour}" ><a href="#">Hours</a></li>
                  <li id="#{graph_utils.granularity.day}" ><a href="#">Days</a></li>
                  <li id="#{graph_utils.granularity.week}" ><a href="#">Weeks</a></li>
                  <li id="#{graph_utils.granularity.month}" ><a href="#">Months</a></li>
                </ul>
              </li>

            </ul>

            <ul class="nav navbar-nav navbar-right">
              <li title="Remove display" id="delDisplay"><a href="#"><i class="fa fa-trash-o"></i></a></li>
              <!-- <li title="Save" id="saveDisplay"><a href="#"><i class="fa fa-save"></i></a></li> -->
            </ul>

          </div><!-- /.navbar-collapse -->
        </div><!-- /.container-fluid -->
      </nav>
    """
