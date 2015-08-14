#=============================================================================
# load Grapherl client
#=============================================================================
$(document).ready ->

  # init plugins
  $.widget "grapherl.dashboard", dashboard
  #$.widget "grapherl.chartjs_chartify", $.grapherl.dashboard, chartjs_chartify
  $.widget "grapherl.c3_chartify", $.grapherl.dashboard, c3_chartify
  $.widget "grapherl.chartify", $.grapherl.dashboard, c3_chartify
  $.widget "grapherl.chartDaemon", chartDaemon
  $.widget "grapherl.sidebar", sidebar

  $(document).chartDaemon()
  init_app()
  #create_test_graph()

  console.log "Grapherl client loaded !"
  return false


init_app = ->
  #Chart.defaults.global = chartJs.config.global
  #Chart.defaults.Line   = chartJs.config.lineChart
  get_active_metric()
  init_global_buttons()


get_active_metric = ->
  # TODO send ajax request to server get list of active metric
  # Metrics = [
  #   "Metric 1",
  #   "Metric 2",
  #   "Metric 3",
  #   "Metric 4",
  #   "Metric 5",
  #   "Metric 6",
  #   "Metric 7",
  #   "Metric 8",
  #   "Metric 9"
  #   ]
  # $.ajax(
  #   method: "GET"
  #   url   : "/metric/list"
  #   #contentType: "application/json"
  #   success: (data) ->
  #     console.log data
  #     $.event.trigger('ui.update_sideBar', [data.metric_list])
  # )

  #MetricSideBar = UI.sideBar_metricList()
  MetricSideBar = UI.sideBar()
  MetricSideBar.sidebar()
  #MetricSideBar.sidebar({data: Metrics})
