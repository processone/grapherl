(function() {
  var UI, c3_chartify, c3_utils, chartDaemon, chartJs, chartjs_chartify, create_test_graph, dashboard, get_active_metric, graph_utils, init_app, init_global_buttons, sidebar;

  c3_chartify = {
    _create: function() {
      return this._state = {};
    },
    _init: function() {
      this._super();
      if (this.options.config == null) {
        this.options.config = {};
      }
      if (this.options.config.moreYaxis == null) {
        this.options.config.moreYaxis = false;
      }
      if (this.options.config.y_format == null) {
        this.options.config.y_format = "";
      }
      if (this.options.config.max_x_labels == null) {
        this.options.config.max_x_labels = this.options.split === true ? 5 : 10;
      }
      if (this.options.config.disp_points == null) {
        this.options.config.disp_points = false;
      }
      return this.append_configrations();
    },
    append_configrations: function() {
      var Li_element, Toolbar;
      Li_element = "<li data-placement=\"bottom\" data-toggle=\"config-chart-popover\" data-title=\"Configure chart\"\n    data-container=\"body\" type=\"button\" data-html=\"true\">\n  <a href=\"#\"> <i class=\"fa fa-bars\"></i> </a></li> ";
      Toolbar = this.options.toolbar;
      Toolbar.find(".chartify_hook").replaceWith(Li_element);
      Toolbar.find("[data-toggle=config-chart-popover]").popover({
        html: true,
        placement: 'right',
        container: this.element,
        content: (function(_this) {
          return function() {
            return $(c3_utils.chart_config(_this.options)).html();
          };
        })(this)
      });
      return Toolbar.find("[data-toggle=config-chart-popover]").on("shown.bs.popover", (function(_this) {
        return function() {
          var Form;
          Form = _this.element.find(".popover").find("#chart-config");
          if (_this.options.config.moreYaxis === false) {
            Form.find("#metric_select").prop('disabled', true);
          } else {
            Form.find("#add_axis").prop("checked", true);
            Form.find("#metric_select").find("#" + _this.options.config.moreYaxis).attr('selected', true);
          }
          if (_this.options.config.xgrid === true) {
            Form.find("#add_grid_x").prop("checked", true);
          }
          if (_this.options.config.ygrid === true) {
            Form.find("#add_grid_y").prop("checked", true);
          }
          if (_this.options.config.rotateAxis === true) {
            Form.find("#rotate_axis").prop("checked", true);
          }
          if (_this.options.config.subchart === true) {
            Form.find("#subchart").prop("checked", true);
          }
          if (_this.options.config.subchart === true) {
            Form.find("#subchart").prop("checked", true);
          }
          if (_this.options.config.disp_points === true) {
            Form.find("#disp_points").prop("checked", true);
          }
          Form.find("#max_x_labels").val(_this.options.config.max_x_labels);
          Form.find("#add_axis").on("click", function(e) {
            if (Form.find("#add_axis").is(":checked")) {
              return Form.find("#metric_select").prop('disabled', false);
            } else {
              return Form.find("#metric_select").prop('disabled', true);
            }
          });
          Form.find("#format_y_axis").on("change", function(e) {
            if (Form.find("#format_y_axis").children(":selected").attr("value") === 'custom') {
              return Form.find("#custom_y_format").css('display', 'inline-block');
            } else {
              return Form.find("#custom_y_format").hide();
            }
          });
          return Form.on("submit", function(e) {
            var CustomFormat, Id, Val;
            e.preventDefault();
            Val = Form.find("#format_y_axis").children(":selected").attr("value");
            if (Val === "custom") {
              CustomFormat = Form.find("#custom_y_format").val();
              _this.options.config.y_format = CustomFormat;
            } else {
              _this.options.config.y_format = Val;
            }
            Toolbar.find("[data-toggle=config-chart-popover]").popover('hide');
            if (Form.find("#add_grid_x").is(":checked")) {
              _this.options.config.xgrid = true;
            } else {
              _this.options.config.xgrid = false;
            }
            if (Form.find("#add_grid_y").is(":checked")) {
              _this.options.config.ygrid = true;
            } else {
              _this.options.config.ygrid = false;
            }
            if (Form.find("#rotate_axis").is(":checked")) {
              _this.options.config.rotateAxis = true;
            } else {
              _this.options.config.rotateAxis = false;
            }
            if (Form.find("#subchart").is(":checked")) {
              _this.options.config.subchart = true;
            } else {
              _this.options.config.subchart = false;
            }
            if (Form.find("#disp_points").is(":checked")) {
              _this.options.config.disp_points = true;
            } else {
              _this.options.config.disp_points = false;
            }
            Val = parseInt(Form.find("#max_x_labels").val());
            if (Number.isInteger(Val) === true) {
              _this.options.config.max_x_labels = Val;
            }
            if (Form.find("#add_axis").is(":checked")) {
              Id = Form.find("#metric_select").children(":selected").attr("id");
              if (Id === void 0) {
                return _this.options.config.moreYaxis = false;
              } else {
                _this.options.config.moreYaxis = Id;
                return _this.render_chart();
              }
            } else {
              _this.options.config.moreYaxis = false;
              return _this.render_chart();
            }
          });
        };
      })(this));
    },
    render_chart: function(Type) {
      var Args, Client, Clients, Columns, D, Data, Id, Metric, Options, X, Xs, YFormat, chart, ref, ref1;
      if (Type == null) {
        Type = "line";
      }
      if (this._state.chart != null) {
        this._state.chart.destroy();
      }
      Columns = [];
      Xs = {};
      ref = this.options.data;
      for (Metric in ref) {
        Clients = ref[Metric];
        for (Client in Clients) {
          Data = Clients[Client];
          ref1 = c3_utils.data_to_c3(Metric, Client, Data.data), Metric = ref1[0], X = ref1[1], D = ref1[2];
          Xs[D] = X;
          Columns.push(Metric.data);
          Columns.push(Metric.label);
        }
      }
      Options = {
        xs: Xs,
        columns: Columns,
        type: 'line',
        axes: {}
      };
      if (this.options.config.moreYaxis !== false) {
        Options.axes[this.options.config.moreYaxis] = 'y2';
      }
      Id = this.element.find(".chart").attr('id');
      YFormat = this.options.config.y_format === "data_size" ? c3_utils.bytesToString : d3.format(this.options.config.y_format);
      Args = {
        bindto: "#" + Id,
        data: Options,
        point: {
          show: this.options.config.disp_points
        },
        padding: {
          right: 50
        },
        legend: {
          position: 'bottom'
        },
        axis: {
          x: {
            type: 'timeseries',
            tick: {
              format: '%m-%d %H:%M:%S',
              culling: {
                max: this.options.config.max_x_labels
              }
            }
          },
          y: {
            tick: {
              format: (function(_this) {
                return function(d) {
                  return YFormat(d);
                };
              })(this)
            }
          },
          rotated: false
        },
        grid: {
          x: {
            show: false
          },
          y: {
            show: false
          }
        },
        subchart: {
          show: false
        }
      };
      if (this.options.config.moreYaxis !== false) {
        Args.axis['y2'] = {
          show: true
        };
      }
      if (this.options.config.xgrid === true) {
        Args.grid.x.show = true;
      }
      if (this.options.config.ygrid === true) {
        Args.grid.y.show = true;
      }
      if (this.options.config.rotateAxis === true) {
        Args.axis.rotated = true;
      }
      if (this.options.config.subchart === true) {
        Args.subchart.show = true;
      }
      chart = c3.generate(Args);
      this._state.chart = chart;
      return false;
    },
    add_metric_data: function(Metric, Client, Data) {
      var Columns, D, X, Xs, ref;
      if (this._state.chart == null) {
        this.render_chart();
        return false;
      }
      Xs = {};
      Columns = [];
      ref = c3_utils.data_to_c3(Metric, Client, Data), Metric = ref[0], X = ref[1], D = ref[2];
      Xs[D] = X;
      Columns.push(Metric.data);
      Columns.push(Metric.label);
      this._state.chart.load({
        xs: Xs,
        columns: Columns,
        length: 0
      });
      return false;
    },
    update_metric_data: function(Metric, Client, Data) {
      var Columns, D, X, Xs, ref;
      Xs = {};
      Columns = [];
      ref = c3_utils.data_to_c3(Metric, Client, Data), Metric = ref[0], X = ref[1], D = ref[2];
      Xs[D] = X;
      Columns.push(Metric.data);
      Columns.push(Metric.label);
      this._state.chart.load({
        columns: Columns
      });
      return false;
    },
    removeMetric: function(Metric, Client) {
      var Id;
      Id = c3_utils.to_data_label(Client, Metric);
      return this._state.chart.unload({
        ids: [Id]
      });
    },
    transform_chart: function(Type, Client, Metric) {
      if (Type == null) {
        Type = "spline";
      }
      if (Client === "all") {
        return this._state.chart.transform(Type);
      } else {
        return this._state.chart.transform(Type, c3_utils.to_data_label(Client, Metric));
      }
    },
    saveDisplay: function() {
      return false;
    },
    udpateChart: function() {
      return false;
    }
  };

  c3_utils = {
    to_data_label: function(Client, Metric) {
      return Client + " (" + Metric + ")";
    },
    bytesToString: function(bytes) {
      var fmt;
      fmt = d3.format('.0f');
      if (bytes < 1024) {
        return fmt(bytes) + 'B';
      } else if (bytes < 1024 * 1024) {
        return fmt(bytes / 1024) + 'kB';
      } else if (bytes < 1024 * 1024 * 1024) {
        return fmt(bytes / 1024 / 1024) + 'MB';
      } else if (bytes < 1024 * 1024 * 1024 * 1024) {
        return fmt(bytes / 1024 / 1024 / 1024) + 'GB';
      } else {
        return fmt(bytes / 1024 / 1024 / 1024 / 1024) + 'TB';
      }
    },
    data_to_c3: function(Metric, Client, Data) {
      var D, Label, NewData, Value, X;
      X = "x-" + Metric + '-' + Client;
      D = c3_utils.to_data_label(Client, Metric);
      Label = [X];
      Value = [D];
      $.each(Data, (function(_this) {
        return function(Key, Val) {
          Label.push(moment(Key * 1000).toDate());
          return Value.push(parseFloat(Val));
        };
      })(this));
      NewData = {
        label: Label,
        data: Value
      };
      return [NewData, X, D];
    },
    chart_config: function(Opts) {
      var Client, Clients, Data, FS, FormatStyles, Id, Key, List, Metric, Option, Val;
      Data = Opts.data;
      List = "";
      for (Metric in Data) {
        Clients = Data[Metric];
        for (Client in Clients) {
          Val = Clients[Client];
          Id = c3_utils.to_data_label(Client, Metric);
          Option = " <option id=\"" + Id + "\">" + Metric + " " + Client + "</option> ";
          List = List.concat(Option);
        }
      }
      FS = "";
      FormatStyles = {
        "None": "",
        "Currency": "$,",
        "Percentage": ".2%",
        "Data size": "data_size",
        "Float": ".3g"
      };
      for (Key in FormatStyles) {
        Val = FormatStyles[Key];
        if (Opts.config.y_format === Val) {
          FS = FS.concat(" <option value=\"" + Val + "\" selected> " + Key + " </option> ");
        } else {
          FS = FS.concat(" <option value=\"" + Val + "\"> " + Key + "</option> ");
        }
      }
      FS = FS.concat(" <option value=\"custom\"> Custom </option> ");
      return "<div class=\"hide\">\n  <form class=\"form\" role=\"form\" id=\"chart-config\">\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label><input id=\"add_axis\" type=\"checkbox\" value=\"\">\n          Additional Y axis for\n        </label>\n      </div>\n      <select class=\"form-control\" id=\"metric_select\" style=\"width: 90%; margin-left: 20px;\">\n        " + List + "\n      </select>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\"> <label> Grids :</label>\n        <span> X grid: </span>\n        <span> <input id=\"add_grid_x\" type=\"checkbox\" value=\"\"\n         style=\"margin-left: 0px;\"> </span>\n\n        <span style=\"margin-left: 20px;\"> Y grid: </span>\n        <span> <input id=\"add_grid_y\" type=\"checkbox\" value=\"\"\n         style=\"margin-left: 5px;\"> </span>\n      </div>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label>\n          <input id=\"rotate_axis\" type=\"checkbox\" value=\"\">\n          Rotate axis\n        </label>\n      </div>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label><input id=\"subchart\" type=\"checkbox\" value=\"\">\n          Subchart display\n        </label>\n      </div>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label><input id=\"disp_points\" type=\"checkbox\" value=\"\">\n          Circle points\n        </label>\n      </div>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label> Max X axis labels </label>\n          <input id=\"max_x_labels\" class=\"form-control\" type=\"number\" min=1\n          style=\"display: inline; margin-left: 10px; width: 30%;\">\n\n      </div>\n    </div>\n\n    <div class=\"form-group\">\n      <div class=\"checkbox\">\n        <label> Y axis label format </label>\n      </div>\n      <div class=\"checkbox\" style=\"padding-left: 20px;\">\n        <select class=\"form-control\" id=\"format_y_axis\"\n        style=\"display: inline; width: 40%;\">\n          " + FS + "\n        </select>\n        <input id=\"custom_y_format\" class=\"form-control\" placeholder=\"Format\"\n        style=\"display: none; max-width: 40%;\">\n        <a onclick=\"window.open('http://koaning.s3-website-us-west-2.amazonaws.com/html/d3format.html', '_blank').focus()\"\n         style=\"font-size: small; cursor: pointer;\"> Help </a>\n      </div>\n\n\n    </div>\n\n    <div class=\"form-group\" style=\"padding-left: 20px;\">\n      <button type=\"submit\" class=\"btn btn-primary\">Submit »</button>\n    </div>\n  </form>\n</div> ";
    }
  };

  dashboard = {
    _create: function() {
      return this._state = {};
    },
    _init: function() {
      this._init_ui();
      this._init_toolbar();
      this._toolbar_events();
      if (this.options.live == null) {
        this.options.live = false;
      }
      if (this.options.granularity == null) {
        this.options.granularity = graph_utils.granularity.min;
      }
      if (!this.options.interval) {
        this.options.interval = 60000;
      }
      if (this.options.live === true) {
        this._go_live();
      }
      this.options.toolbar.trigger("update_info_time");
      this.options.toolbar.trigger("update_info_granularity");
      if (this.options.data != null) {
        return $.each(this.options.data, (function(_this) {
          return function(Metric, Clients) {
            return $.each(Clients, function(Client, Data) {
              _this.get_data_from_daemon(Metric, Client);
              return delete _this.options.data[Metric][Client];
            });
          };
        })(this));
      } else {
        return this.options.data = {};
      }
    },
    _init_ui: function() {
      var Display;
      Display = UI.graphDisplayC3("c3_display1", "chart");
      this.element.append(Display);
      return this.element.append("<div id=\"disp_info\" style=\"margin-bottom: 30px; padding-left: 5px;\n  padding-top: 10px;\">\n    <i class=\"fa fa-clock-o\" ></i>\n    <span id=\"selected-time-interval\" style=\"margin-right: 20px;\"> time </span>\n    <i class=\"fa fa-sitemap\" ></i>\n    <span id=\"selected-granularity\"> sec </span>\n  </div>");
    },
    _init_toolbar: function() {
      var CurrDispMetric, RangePicker, Toolbar;
      Toolbar = this.element.find('nav');
      Toolbar.find("#delDisplay").on("click", (function(_this) {
        return function() {
          return _this.element.remove();
        };
      })(this));
      CurrDispMetric = Toolbar.find("#currentDispMetric");
      this.element.on("display_update", (function(_this) {
        return function(e) {
          e.stopImmediatePropagation();
          CurrDispMetric.empty();
          $.each(_this.options.data, function(Metric, Clients) {
            return $.each(Clients, function(Client, Data) {
              var Id;
              Id = graph_utils.generate_id();
              return CurrDispMetric.append("<li id=\"" + Id + "\" data-metric=\"" + Metric + "\" data-client=\"" + Client + "\">\n  <a href=\"#\">" + Metric + "\n   <span style=\"font-size: 12px; color:gray;\">" + Client + "</span>\n  </a>\n</li> ");
            });
          });
          return _this._toolbar_events();
        };
      })(this));
      Toolbar.find("#chart_type li").on("click", (function(_this) {
        return function(e) {
          e.stopImmediatePropagation();
          if (Toolbar.find("#chart_type li#" + e.currentTarget.id).hasClass("active") === false) {
            Toolbar.find("#chart_type li#" + e.currentTarget.id).find(".fa-angle-down").removeClass("fa-angle-down").addClass("fa-angle-up");
            Toolbar.find("#chart_type li#" + e.currentTarget.id).addClass("active");
            return _this.show_avail_charts(e.currentTarget.id);
          } else {
            Toolbar.find("#chart_type li#" + e.currentTarget.id).find(".fa-angle-up").removeClass("fa-angle-up").addClass("fa-angle-down");
            Toolbar.find("#chart_type li#" + e.currentTarget.id).removeClass("active");
            return _this.options.toolbar.find("#chart_type").find("." + e.currentTarget.id).remove();
          }
        };
      })(this));
      this.inti_daterangepicker();
      RangePicker = Toolbar.find("#range_picker");
      RangePicker.unbind("apply.daterangepicker");
      RangePicker.on("apply.daterangepicker", (function(_this) {
        return function(e) {
          var EndDate, StartDate;
          StartDate = RangePicker.data('daterangepicker').startDate;
          EndDate = RangePicker.data('daterangepicker').endDate;
          console.log(StartDate.unix(), EndDate.unix());
          Toolbar.trigger("udpate_info_time");
          return _this.update_all_metrics();
        };
      })(this));
      Toolbar.on("update_info_time", (function(_this) {
        return function() {
          var Ed, EndDate, Sd, StartDate;
          StartDate = RangePicker.data('daterangepicker').startDate;
          EndDate = RangePicker.data('daterangepicker').endDate;
          Sd = StartDate.format("MMM D YYYY, h:mm:ss a");
          Ed = EndDate.format("MMM D YYYY, h:mm:ss a");
          return _this.element.find("#selected-time-interval").html(Sd + " - " + Ed);
        };
      })(this));
      Toolbar.on("update_info_granularity", (function(_this) {
        return function() {
          return _this.element.find("#selected-granularity").html(_this.options.granularity);
        };
      })(this));
      Toolbar.find("#addMetric").on("click", (function(_this) {
        return function() {
          $.event.trigger('selectionStart');
          _this.toggle_add_button(true);
          return false;
        };
      })(this));
      Toolbar.find("#selectionDone").on("click", (function(_this) {
        return function() {
          var Cname, Metric, MetricSideBar, Metrics, Mname, j, len;
          $.event.trigger('selectionDone');
          MetricSideBar = UI.sideBar();
          Metrics = MetricSideBar.sidebar("get_selected_metric");
          for (j = 0, len = Metrics.length; j < len; j++) {
            Metric = Metrics[j];
            Mname = Metric.metric_name;
            Cname = Metric.client_name;
            if (_this.options.data[Mname] != null) {
              if (_this.options.data[Mname][Cname] == null) {
                _this.get_data_from_daemon(Mname, Cname);
              }
            } else {
              _this.get_data_from_daemon(Mname, Cname);
            }
          }
          return _this.toggle_add_button(false);
        };
      })(this));
      $(document).on("selectionCancel", (function(_this) {
        return function() {
          return _this.toggle_add_button(false);
        };
      })(this));
      Toolbar.find("#update_metrics i.live").on("click.update", (function(_this) {
        return function() {
          if (_this.options.live === false) {
            return _this._go_live();
          } else {
            clearInterval(_this.options.live);
            _this.options.live = false;
            return Toolbar.find("#update_metrics").find("a i.live").css("color", "");
          }
        };
      })(this));
      Toolbar.find("#granularity").find('li').on("click", (function(_this) {
        return function(e) {
          var Id;
          Id = e.currentTarget.id;
          _this.options.granularity = Id;
          _this.options.toolbar.trigger("update_info_granularity");
          return console.log(_this.options.granularity);
        };
      })(this));
      Toolbar.find("[data-toggle=update-interval-popover]").popover({
        html: true,
        placement: 'right',
        container: this.element,
        content: (function(_this) {
          return function() {
            return $(UI.graphToolbar_intervalFrom(_this.options.interval / 1000)).html();
          };
        })(this)
      });
      Toolbar.find("[data-toggle=update-interval-popover]").on("shown.bs.popover", (function(_this) {
        return function() {
          var Form;
          Form = _this.element.find(".popover").find("#update-interval");
          return Form.on("submit", function(e) {
            var Val;
            e.preventDefault();
            Val = Form.find("#interval").val();
            _this.options.interval = Val * 1000;
            console.log("new interval", Val);
            Toolbar.find("[data-toggle=update-interval-popover]").popover('hide');
            if (!(_this.options.live === false)) {
              Toolbar.find("#update_metrics i.live").trigger("click.update");
              return Toolbar.find("#update_metrics i.live").trigger("click.update");
            }
          });
        };
      })(this));
      return this.options.toolbar = Toolbar;
    },
    _go_live: function() {
      var Fun, Interval;
      Fun = (function(_this) {
        return function() {
          _this.element.find("#range_picker").data('daterangepicker').setEndDate(moment());
          return _this.update_all_metrics();
        };
      })(this);
      Interval = setInterval(Fun, this.options.interval);
      this.options.live = Interval;
      this.options.toolbar.find("#update_metrics").find("a i.live").css("color", "#f44336");
      return false;
    },
    bookmark: function() {
      var Client, Clients, Data, Metric, State, d, ref;
      Data = {};
      ref = this.options.data;
      for (Metric in ref) {
        Clients = ref[Metric];
        Data[Metric] = {};
        for (Client in Clients) {
          d = Clients[Client];
          Data[Metric][Client] = {
            data: []
          };
        }
      }
      State = {
        config: this.options.config,
        granularity: this.options.granularity,
        interval: this.options.interval,
        live: this.options.live,
        data: Data
      };
      return State;
    },
    toggle_add_button: function(State) {
      if (State == null) {
        State = false;
      }
      if (State === false) {
        this.options.toolbar.find("#selectionDone").hide();
        return this.options.toolbar.find("#addMetric").show();
      } else {
        this.options.toolbar.find("#addMetric").hide();
        return this.options.toolbar.find("#selectionDone").show();
      }
    },
    _toolbar_events: function() {
      var MetricList, Toolbar;
      Toolbar = this.element.find('nav');
      MetricList = this.options.toolbar.find("#removeMetric");
      MetricList.find("li").unbind("click.remove");
      MetricList.find("li").on("click.remove", (function(_this) {
        return function(e) {
          var Client, Metric;
          Metric = MetricList.find("#" + e.currentTarget.id).attr('data-metric');
          Client = MetricList.find("#" + e.currentTarget.id).attr('data-client');
          _this.removeMetric(Metric, Client);
          delete _this.options.data[Metric][Client];
          return _this.element.trigger("display_update");
        };
      })(this));
      this.element.on("metric_data", (function(_this) {
        return function(e, Metric, Client, Data) {
          e.stopImmediatePropagation();
          if (_this.options.data[Metric] != null) {
            if (_this.options.data[Metric][Client] != null) {
              _this.options.data[Metric][Client] = {
                data: Data.metric_data
              };
              _this.update_metric_data(Metric, Client, Data.metric_data);
            } else {
              _this.options.data[Metric][Client] = {
                data: Data.metric_data
              };
              _this.add_metric_data(Metric, Client, Data.metric_data);
            }
          } else {
            _this.options.data[Metric] = {};
            _this.options.data[Metric][Client] = {
              data: Data.metric_data
            };
            _this.add_metric_data(Metric, Client, Data.metric_data);
          }
          return _this.element.trigger("display_update");
        };
      })(this));
      return false;
    },
    update_all_metrics: function() {
      var Client, Clients, Data, Metric, ref, results;
      ref = this.options.data;
      results = [];
      for (Metric in ref) {
        Clients = ref[Metric];
        results.push((function() {
          var results1;
          results1 = [];
          for (Client in Clients) {
            Data = Clients[Client];
            results1.push(this.get_data_from_daemon(Metric, Client));
          }
          return results1;
        }).call(this));
      }
      return results;
    },
    show_avail_charts: function(Id) {
      var Client, Clients, D, Ele, Metric, Option, Options, ref;
      this.options.toolbar.find("#chart_type").find("." + Id).remove();
      Options = " <li class=\"" + Id + "\" data-client=\"all\" data-metric=\"" + Metric + "\">\n<a href=\"#\" style=\"font-size: x-small; color: gray;\"> All </a></li>";
      ref = this.options.data;
      for (Metric in ref) {
        Clients = ref[Metric];
        for (Client in Clients) {
          D = Clients[Client];
          Option = "<li class=\"" + Id + "\" data-client=\"" + Client + "\" data-metric=\"" + Metric + "\">\n<a href=\"#\" style=\"font-size: 11px; color: gray;\"> " + Metric + " (" + Client + ")</a></li>";
          Options = Options.concat(Option);
        }
      }
      Options = Options.concat(" <li role=\"separator\" class=\"divider " + Id + "\"></li> ");
      Ele = this.options.toolbar.find("#chart_type").find("#" + Id);
      $(Options).insertAfter(Ele);
      return this.options.toolbar.find("#chart_type").find("." + Id).on("click", (function(_this) {
        return function(e) {
          Client = e.currentTarget.dataset.client;
          Metric = e.currentTarget.dataset.metric;
          return _this.transform_chart(Id, Client, Metric);
        };
      })(this));
    },
    get_data_from_daemon: function(Metric, Client) {
      var End, RangePicker, Start;
      RangePicker = this.options.toolbar.find("#range_picker");
      Start = RangePicker.data('daterangepicker').startDate.unix();
      End = RangePicker.data('daterangepicker').endDate.unix();
      console.log("getting data range: ", Start, End);
      return $(document).chartDaemon("get_metric_data", this.element, Metric, Client, [Start, End], this.options.granularity);
    },
    inti_daterangepicker: function() {
      var RangePicker;
      RangePicker = this.element.find("#range_picker");
      return RangePicker.daterangepicker({
        parentEl: "#graphDiv",
        format: 'MM/DD/YYYY',
        startDate: moment().subtract(3, 'hours'),
        endDate: moment(),
        minDate: '01/01/2015',
        maxDate: '12/31/2015',
        showDropdowns: true,
        showWeekNumbers: true,
        timePicker: true,
        timePickerIncrement: 1,
        timePicker12Hour: true,
        ranges: {
          'Today': [moment(), moment()],
          'Yesterday': [moment().subtract(1, 'days'), moment().subtract(1, 'days')],
          'Last 7 Days': [moment().subtract(6, 'days'), moment()],
          'Last 30 Days': [moment().subtract(29, 'days'), moment()],
          'This Month': [moment().startOf('month'), moment().endOf('month')],
          'Last Month': [moment().subtract(1, 'month').startOf('month'), moment().subtract(1, 'month').endOf('month')]
        },
        opens: 'center',
        drops: 'down',
        buttonClasses: ['btn', 'btn-sm'],
        applyClass: 'btn-primary',
        cancelClass: 'btn-default',
        separator: ' to ',
        locale: {
          applyLabel: 'Submit',
          cancelLabel: 'Cancel',
          fromLabel: 'From',
          toLabel: 'To',
          customRangeLabel: 'Custom',
          daysOfWeek: ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'],
          monthNames: ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
          firstDay: 1
        }
      });
    }
  };

  chartDaemon = {
    _cache: {},
    _create: function() {
      return this._bind_events();
    },
    _init: function() {
      return false;
    },
    get_metric_data: function(element, Metric, Client, Range, Granularity) {
      var Cache, End, Start, Status, Url, ref;
      ref = this._lookup_cache(Metric, Client, Range, Granularity), Status = ref[0], Cache = ref[1];
      if (Status === false) {
        Start = Range[0], End = Range[1];
        Url = "/metric/data/" + Metric + "/" + Client + "/" + Start.toString() + ":" + End.toString() + "/" + Granularity;
        $.ajax({
          method: "GET",
          url: Url,
          success: (function(_this) {
            return function(data) {
              element.trigger("metric_data", [Metric, Client, data]);
              return _this._insert_cache(Metric, Client, Granularity, data.metric_data);
            };
          })(this)
        });
      } else {
        element.trigger("metric_data", [
          Metric, Client, {
            metric_data: Cache
          }
        ]);
      }
      return false;
    },
    _insert_cache: function(Metric, Client, Granularity, Data) {
      var Key, Val, ref, ref1, ref2;
      if (((ref = this._cache[Metric]) != null ? (ref1 = ref[Client]) != null ? ref1[Granularity] : void 0 : void 0) != null) {
        for (Key in Data) {
          Val = Data[Key];
          if (this._cache[Metric][Client][Granularity][Key] == null) {
            this._cache[Metric][Client][Granularity][Key] = Val;
          }
        }
        this._sort_data(Metric, Client, Granularity);
      } else if (((ref2 = this._cache[Metric]) != null ? ref2[Client] : void 0) != null) {
        this._cache[Metric][Client][Granularity] = {};
        this._insert_cache(Metric, Client, Granularity, Data);
      } else if (this._cache[Metric] != null) {
        this._cache[Metric][Client] = {};
        this._insert_cache(Metric, Client, Granularity, Data);
      } else {
        this._cache[Metric] = {};
        this._cache[Metric][Client] = {};
        this._cache[Metric][Client][Granularity] = Data;
      }
      return false;
    },
    _sort_data: function(Metric, Client, Granularity) {
      var Data, Key, Keys, j, len;
      Keys = Object.keys(this._cache[Metric][Client][Granularity]).sort();
      Data = {};
      for (j = 0, len = Keys.length; j < len; j++) {
        Key = Keys[j];
        Data[Key] = this._cache[Metric][Client][Granularity][Key];
      }
      this._cache[Metric][Client][Granularity] = Data;
      return false;
    },
    _lookup_cache: function(Metric, Client, Range, Granularity) {
      var Data, End, Key, Keys, Len, Rend, Rstart, Start, Val, ref, ref1, ref2;
      if (((ref = this._cache[Metric]) != null ? (ref1 = ref[Client]) != null ? ref1[Granularity] : void 0 : void 0) != null) {
        Rstart = Range[0], Rend = Range[1];
        Data = {};
        Keys = Object.keys(this._cache[Metric][Client][Granularity]);
        Len = Keys.length;
        Start = parseInt(Keys[0]);
        End = parseInt(Keys[Keys.length - 1]);
        if (Start <= Rstart && End >= Rend) {
          ref2 = this._cache[Metric][Client][Granularity];
          for (Key in ref2) {
            Val = ref2[Key];
            if (Key >= Rstart && Key <= Rend) {
              Data[Key] = Val;
            }
          }
          return [true, Data];
        } else {
          return [false, []];
        }
      } else {
        return [false, []];
      }
    },
    _bind_events: function() {
      return $(document).on("metricData", function(e, Data) {
        return this._cache[Data.metric] = Data.points;
      });
    }
  };

  create_test_graph = function() {
    var data, data2;
    data = {
      labels: ["January", "February", "March", "April", "May", "June", "July"],
      datasets: [
        {
          label: "Metric 1",
          fillColor: "rgba(220,220,220,0.2)",
          strokeColor: "rgba(220,220,220,1)",
          pointColor: "rgba(220,220,220,1)",
          pointStrokeColor: "#fff",
          pointHighlightFill: "#fff",
          pointHighlightStroke: "rgba(220,220,220,1)",
          data: [65, 59, 80, 81, 56, 55, 40]
        }, {
          label: "Metric 2",
          fillColor: "rgba(151,187,205,0.2)",
          strokeColor: "rgba(151,187,205,1)",
          pointColor: "rgba(151,187,205,1)",
          pointStrokeColor: "#fff",
          pointHighlightFill: "#fff",
          pointHighlightStroke: "rgba(151,187,205,1)",
          data: [28, 48, 40, 19, 86, 27, 90]
        }
      ]
    };
    data2 = {
      metric1: {
        client1: {
          data: [
            {
              '2013-01-01': 300
            }, {
              '2013-01-02': 200
            }, {
              '2013-01-03': 100
            }, {
              '2013-01-04': 400
            }, {
              '2013-01-05': 150
            }, {
              '2013-01-06': 250
            }
          ]
        }
      },
      metric2: {
        client2: {
          data: [
            {
              '2013-01-01': 130
            }, {
              '2013-01-02': 340
            }, {
              '2013-01-03': 200
            }, {
              '2013-01-04': 500
            }, {
              '2013-01-05': 250
            }, {
              '2013-01-07': 350
            }
          ]
        }
      }
    };
    return false;
  };

  chartjs_chartify = {
    _create: function() {
      return this._state = {};
    },
    _init: function() {
      return this._super();
    },
    _init_ui: function() {
      return this.element.append(UI.graphDisplayCharjs("c3_display1", "chart"));
    },
    render_chart: function(Type) {
      var Canvas, D, Data, Key, L, Length, NewChart, Points, Val, ctx, i, j, ref, ref1;
      if (Type == null) {
        Type = "line";
      }
      if (this._state.chart != null) {
        this._state.chart.destroy();
      }
      Data = [];
      ref = this.options.data;
      for (Key in ref) {
        Val = ref[Key];
        L = this.options.data[Key].label;
        D = this.options.data[Key].data;
        Length = L.length;
        Points = [];
        for (i = j = 0, ref1 = Length - 1; 0 <= ref1 ? j <= ref1 : j >= ref1; i = 0 <= ref1 ? ++j : --j) {
          Points.push({
            x: new Date(L[i]),
            y: D[i]
          });
        }
        Data.push({
          label: Key,
          data: Points
        });
      }
      Canvas = this.element.find('canvas');
      ctx = Canvas.get(0).getContext("2d");
      NewChart = new Chart(ctx).Scatter(Data, {
        bezierCurve: true,
        datasetStrokeWidth: 1,
        pointDotRadius: 3,
        showTooltips: true,
        scaleShowLabels: true,
        scaleType: "date",
        scaleShowHorizontalLines: false,
        scaleShowVerticalLines: false
      });
      this._state.chart = NewChart;
      this.options.toolbar.find("#currentDispMetric").trigger("display_update");
      return false;
    },
    addChart: function(Data) {
      return false;
    },
    udpateChart: function() {
      return false;
    },
    _bind_events: function() {
      var Toolbar;
      Toolbar = this.element.find('nav');
      this.options.toolbar.find("#removeMetric li").unbind("click.remove");
      this.options.toolbar.find("#removeMetric li").on("click.remove", (function(_this) {
        return function(e) {
          var Index;
          Index = e.currentTarget.id;
          _this.options.data.datasets.splice(Index, 1);
          return _this.render_chart();
        };
      })(this));
      return false;
    }
  };

  chartJs = {
    config: {
      lineChart: {
        scaleShowGridLines: true,
        scaleGridLineColor: "rgba(0,0,0,.05)",
        scaleGridLineWidth: 1,
        scaleShowHorizontalLines: true,
        scaleShowVerticalLines: true,
        bezierCurve: true,
        bezierCurveTension: 0.4,
        pointDot: true,
        pointDotRadius: 3,
        pointDotStrokeWidth: 1,
        pointHitDetectionRadius: 20,
        datasetStroke: true,
        datasetStrokeWidth: 1,
        datasetFill: true,
        legendTemplate: "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li><span style=\"background-color:<%=datasets[i].strokeColor%>\"></span><%if(datasets[i].label){%><%=datasets[i].label%><%}%></li><%}%></ul>"
      },
      global: {
        animation: true,
        animationSteps: 30,
        animationEasing: "easeOutQuart",
        showScale: true,
        scaleOverride: false,
        scaleSteps: null,
        scaleStepWidth: null,
        scaleStartValue: null,
        scaleLineColor: "#000000",
        scaleLineWidth: 1,
        scaleShowLabels: true,
        scaleLabel: "<%=value%>",
        scaleIntegersOnly: true,
        scaleBeginAtZero: false,
        scaleFontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
        scaleFontSize: 12,
        scaleFontStyle: "normal",
        scaleFontColor: "#666",
        responsive: true,
        maintainAspectRatio: true,
        showTooltips: true,
        customTooltips: false,
        tooltipEvents: ["mousemove", "touchstart", "touchmove"],
        tooltipFillColor: "rgba(0,0,0,0.8)",
        tooltipFontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
        tooltipFontSize: 14,
        tooltipFontStyle: "normal",
        tooltipFontColor: "#fff",
        tooltipTitleFontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
        tooltipTitleFontSize: 14,
        tooltipTitleFontStyle: "bold",
        tooltipTitleFontColor: "#fff",
        tooltipYPadding: 6,
        tooltipXPadding: 6,
        tooltipCaretSize: 8,
        tooltipCornerRadius: 6,
        tooltipXOffset: 10,
        tooltipTemplate: "<%if (label){%><%=label%>: <%}%><%= value %>",
        multiTooltipTemplate: "<%= value %>",
        onAnimationProgress: function() {},
        onAnimationComplete: function() {}
      }
    }
  };

  $(document).ready(function() {
    $.widget("grapherl.dashboard", dashboard);
    $.widget("grapherl.c3_chartify", $.grapherl.dashboard, c3_chartify);
    $.widget("grapherl.chartify", $.grapherl.dashboard, c3_chartify);
    $.widget("grapherl.chartDaemon", chartDaemon);
    $.widget("grapherl.sidebar", sidebar);
    $(document).chartDaemon();
    init_app();
    console.log("Grapherl client loaded !");
    return false;
  });

  init_app = function() {
    get_active_metric();
    return init_global_buttons();
  };

  get_active_metric = function() {
    var MetricSideBar;
    MetricSideBar = UI.sideBar();
    return MetricSideBar.sidebar();
  };

  graph_utils = {
    generate_id: function() {
      return $("<div></div>").uniqueId().attr('id');
    },
    add_display: function(MetricData) {
      var GraphFrame, NewDisplay;
      GraphFrame = $(UI.graphDiv());
      NewDisplay = $(UI.graphNew());
      NewDisplay.find("#display").chartify(MetricData);
      return GraphFrame.append(NewDisplay);
    },
    add_split_display: function(Data) {
      var Disp, GraphFrame, NewDisplay;
      if (Data == null) {
        Data = void 0;
      }
      GraphFrame = $(UI.graphDiv());
      NewDisplay = $(UI.graphNewSplit());
      if (Data === void 0) {
        NewDisplay.find("#display").chartify({
          split: true
        });
      } else {
        Disp = NewDisplay.find("#display");
        Data[0].split = true;
        Data[1].split = true;
        $(Disp[0]).chartify(Data[0]);
        $(Disp[1]).chartify(Data[1]);
      }
      GraphFrame.append(NewDisplay);
      return GraphFrame;
    },
    save_display: function() {
      var Block, Display, Displays, Ele, Encoded, Layout, Obj, Row, Rows, j, k, len, len1;
      Rows = $(UI.graphDiv()).find(".row");
      Layout = [];
      for (j = 0, len = Rows.length; j < len; j++) {
        Row = Rows[j];
        Displays = $(Row).find(".chartified");
        Block = [];
        for (k = 0, len1 = Displays.length; k < len1; k++) {
          Display = Displays[k];
          Block.push($(Display).chartify("bookmark"));
        }
        if (Block[0] !== void 0) {
          Layout.push(Block);
        }
      }
      Obj = JSON.stringify(Layout);
      Encoded = window.btoa(Obj);
      $("#bookmark_download").remove();
      Ele = $("<a id='bookmark_download' download='grapherl_display_bookmark.dat' href='data:application/octet-stream;charset=utf-8;base64," + Encoded + "' style='display:none;'></a>");
      $(UI.graphDiv()).append(Ele);
      return document.getElementById('bookmark_download').click();
    },
    load_display: function(Layout) {
      var Row, j, len, results;
      console.log(Layout);
      results = [];
      for (j = 0, len = Layout.length; j < len; j++) {
        Row = Layout[j];
        if (Row.length === 1) {
          results.push(graph_utils.add_display(Row[0]));
        } else if (Row.length === 2) {
          results.push(graph_utils.add_split_display(Row));
        } else {
          results.push(void 0);
        }
      }
      return results;
    },
    get_interval: function(Granularity) {
      if (Granularity === graph_utils.granularity.sec) {
        return 15000;
      } else if (Granularity === graph_utils.granularity.min) {
        return 60000;
      } else if (Granularity === graph_utils.granularity.hour) {
        return 3600000;
      } else {
        return 3600000;
      }
    },
    granularity: {
      sec: "sec",
      min: "min",
      hour: "hour",
      day: "day",
      week: "week",
      month: "month"
    }
  };

  UI = {
    globalToolbar: function() {
      return $("#global-nav");
    },
    sideBar: function() {
      return $("#sidebar");
    },
    sideBar_metricList: function() {
      return $("#active-metrics");
    },
    sideBar_li: function(Metric, Client, Disp) {
      var Id;
      Id = graph_utils.generate_id();
      return "<li title=\"" + Client + "\" data-metric=\"" + Metric + "\" data-client=\"" + Client + "\"\n  class=\"client\" id=\"" + Id + "\">\n  <a href=\"#\" style=\"padding: 5px;\">  " + Disp + "</a>\n</li> ";
    },
    graphDiv: function() {
      return $("#graphDiv");
    },
    graphNew: function() {
      var Id;
      Id = graph_utils.generate_id();
      return "<div class=\"row\">\n  <div class=\"col-md-12 chartified\" data-chart-name=\"" + Id + "\" id=\"display\" >\n  </div>\n</div> ";
    },
    graphNewSplit: function() {
      var Id1, Id2;
      Id1 = graph_utils.generate_id();
      Id2 = graph_utils.generate_id();
      return "<div class=\"row\" >\n  <div class=\"col-md-6 chartified\" data-chart-name=\"" + Id1 + "\" id=\"display\" > </div>\n  <div class=\"col-md-6 chartified\" data-chart-name=\"" + Id2 + "\" id=\"display\" > </div>\n</div> ";
    },
    graphDisplayC3: function(DispId, ChartId) {
      var Frame, Id;
      Frame = $(UI.graphFrame(DispId));
      Id = graph_utils.generate_id();
      Frame.append("<div class=\"chart\" id=\"" + Id + "\"></div>");
      Frame.append(UI.graphToolbar());
      return Frame;
    },
    graphDisplayCharjs: function(DispId, ChartId) {
      var Frame;
      Frame = $(UI.graphFrame(DispId));
      Frame.append(UI.graphCanvas(ChartId));
      Frame.append(UI.graphToolbar());
      return Frame;
    },
    graphFrame: function(Id) {
      if (Id == null) {
        Id = "display1";
      }
      return "<div class=\"well graphDisplay\" id=\"" + Id + "\" style=\"margin-bottom: 0px;\"> </div>";
    },
    graphCanvas: function(Id) {
      if (Id == null) {
        Id = "chart";
      }
      return "<div> <canvas id=\"" + Id + "\"></canvas> </div> ";
    },
    graphToolbar_intervalFrom: function(Value) {
      return "<div id=\"popover-content\" class=\"hide\">\n  <form class=\"form-inline\" role=\"form\" id=\"update-interval\">\n    <div class=\"form-group\">\n      <input id=\"interval\" value=\"" + Value + "\" type=\"number\"\n       placeholder=\"Seconds\" class=\"form-control\" maxlength=\"5\"\n        type=\"text\" style=\"max-width:120px;\">\n      <button type=\"submit\" class=\"btn btn-primary\">Update »</button>\n    </div>\n  </form>\n</div> ";
    },
    graphToolbar: function() {
      return "<hr style=\"border-color:rgba(179, 179, 179, 0.6); margin:0px;\">\n\n<nav class=\"navbar navbar-fixed-bottom\" style=\"position:relative;\">\n  <div class=\"container-fluid\">\n\n\n  <!-- Collect the nav links, forms, and other content for toggling -->\n    <div class=\"collapse navbar-collapse\" id=\"bs-example-navbar-collapse-1\">\n\n      <ul class=\"nav navbar-nav\">\n        <li class=\"dropdown\" id=\"chart_type\" >\n          <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\"\n            aria-expanded=\"false\"><i class=\"fa fa-gear\"></i></a>\n          <ul class=\"dropdown-menu\">\n            <li id=\"bar\"> <a href=\"#\"><i class=\"fa fa-bar-chart\"></i> Bar chart <i class=\"fa fa-angle-down pull-right\"></i></a></li>\n            <li id=\"line\"> <a href=\"#\"><i class=\"fa fa-line-chart\"></i> Line chart <i class=\"fa fa-angle-down pull-right\"></i></a></li>\n            <li id=\"spline\"> <a href=\"#\"><i class=\"fa fa-line-chart\"></i> Spline chart <i class=\"fa fa-angle-down pull-right\"></i></a></li>\n          </ul>\n        </li>\n\n\n        <li title=\"Specify interval\" id=\"range_picker\" ><a href=\"#\"><i class=\"fa fa-clock-o\"></i></a></li>\n\n        <!-- <li title=\"Duplicate display\" id=\"duplicate\"><a href=\"#\"><i class=\"fa fa-copy\"></i></a></li> -->\n        <li title=\"Add more metric\" id=\"addMetric\"><a href=\"#\"><i class=\"fa fa-plus-square-o\"></i></a></li>\n        <li title=\"Add more metric\" id=\"selectionDone\" style=\"display:none;\"><a href=\"#\"><i class=\"fa fa-square-o\"></i></a></li>\n\n        <li class=\"dropdown\" title=\"Remove metric\" id=\"removeMetric\">\n          <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\"\n            aria-expanded=\"false\"><i class=\"fa fa-minus-square-o\"></i></a>\n          <ul class=\"dropdown-menu\" id=\"currentDispMetric\">\n            <li><a href=\"#\"><i class=\"fa fa-pie-chart\"></i> Pie chart</a></li>\n          </ul>\n        </li>\n\n        <li title=\"Go live\" id=\"update_metrics\" >\n           <a href=\"#\">\n              <i class=\"fa fa-refresh live\"></i>\n\n              <span  data-placement=\"bottom\" data-toggle=\"update-interval-popover\" data-title=\"Change udpate interval\"\n                data-container=\"body\" type=\"button\" data-html=\"true\" href=\"#\">\n                <i class=\"fa fa-angle-down\"></i>\n              </span>\n          </a>\n        </li>\n\n\n\n        <li class=\"dropdown\" title=\"Granularity\">\n          <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\"\n            aria-expanded=\"false\"><i class=\"fa fa-sitemap\"></i></a>\n          <ul class=\"dropdown-menu\" id=\"granularity\">\n            <li id=\"" + graph_utils.granularity.sec + "\" ><a href=\"#\">Seconds</a></li>\n            <li id=\"" + graph_utils.granularity.min + "\" ><a href=\"#\">Minutes</a></li>\n            <li id=\"" + graph_utils.granularity.hour + "\" ><a href=\"#\">Hours</a></li>\n            <li id=\"" + graph_utils.granularity.day + "\" ><a href=\"#\">Days</a></li>\n            <li id=\"" + graph_utils.granularity.week + "\" ><a href=\"#\">Weeks</a></li>\n            <li id=\"" + graph_utils.granularity.month + "\" ><a href=\"#\">Months</a></li>\n          </ul>\n        </li>\n\n      <li class=\"chartify_hook\"></li>\n\n\n      </ul>\n\n      <ul class=\"nav navbar-nav navbar-right\">\n        <li title=\"Remove display\" id=\"delDisplay\"><a href=\"#\"><i class=\"fa fa-trash-o\"></i></a></li>\n        <!-- <li title=\"Save\" id=\"saveDisplay\"><a href=\"#\"><i class=\"fa fa-save\"></i></a></li> -->\n      </ul>\n\n    </div><!-- /.navbar-collapse -->\n  </div><!-- /.container-fluid -->\n</nav>";
    }
  };

  sidebar = {
    _create: function() {
      return false;
    },
    _init: function() {
      this.options.display = 'client';
      this._bind_sidebar_events();
      if (this.options.data != null) {
        $(document).trigger("ui.update_sideBar", [this.options.data]);
      } else {
        this.element.find("#sidebar-refresh").click();
      }
      return false;
    },
    _bind_sidebar_events: function() {
      var SideBarRefresh;
      $(document).on("ui.update_sideBar", (function(_this) {
        return function(e, Data) {
          _this.element.find("#active-metrics").empty();
          _this.options.data = Data;
          _this._add_sidebar_elements(Data);
          return _this._bind_select_metric();
        };
      })(this));
      $(document).on("selectionDone", (function(_this) {
        return function() {
          var MetricData, Selected, ele, j, len, ref;
          _this.element.find(".selected").find("a").css("color", "");
          _this.element.find(".selected").effect("highlight", {
            color: "#ffcdd2"
          }, 1000);
          Selected = [];
          ref = _this.element.find(".selected").closest("li");
          for (j = 0, len = ref.length; j < len; j++) {
            ele = ref[j];
            MetricData = {
              metric_name: $(ele).attr('data-metric'),
              client_name: $(ele).attr('data-client')
            };
            Selected.push(MetricData);
          }
          _this.element.find(".selected").removeClass("selected");
          _this.options.selected = Selected;
          _this._unbind_multi_selection();
          return _this._bind_select_metric();
        };
      })(this));
      $(document).on("selectionStart", (function(_this) {
        return function() {
          $.event.trigger('selectionCancel');
          _this._unbind_select_metric();
          _this._unbind_multi_selection();
          return _this._multi_selection();
        };
      })(this));
      this.element.find('#sidebar-refresh').on("click", (function(_this) {
        return function() {
          return $.ajax({
            method: "GET",
            url: "/metric/list",
            success: function(data) {
              console.log(data);
              return $.event.trigger('ui.update_sideBar', [data.metric_list]);
            }
          });
        };
      })(this));
      this.element.find("#sb-disp-client").on("click", (function(_this) {
        return function(e) {
          _this.options.display = 'client';
          return $(document).trigger("ui.update_sideBar", [_this.options.data]);
        };
      })(this));
      this.element.find("#sb-disp-metric").on("click", (function(_this) {
        return function(e) {
          _this.options.display = 'metric';
          return $(document).trigger("ui.update_sideBar", [_this.options.data]);
        };
      })(this));
      SideBarRefresh = (function(_this) {
        return function() {
          return _this.element.find("#sidebar-refresh").click();
        };
      })(this);
      return setInterval(SideBarRefresh, 60000);
    },
    get_selected_metric: function() {
      return this.options.selected;
    },
    _bind_select_metric: function() {
      return this.element.find('li.client').on("click.select", (function(_this) {
        return function(e) {
          var Client, Data, Metric;
          Metric = $(e.currentTarget).attr('data-metric');
          Client = $(e.currentTarget).attr('data-client');
          Data = {};
          Data[Metric] = {};
          Data[Metric][Client] = {
            data: []
          };
          return graph_utils.add_display({
            data: Data
          });
        };
      })(this));
    },
    _unbind_select_metric: function() {
      return this.element.find('li.client').unbind("click.select");
    },
    _unbind_multi_selection: function() {
      this.element.find("li").unbind("click.multi_select");
      this.element.find(".multi-select").remove();
      return this.element.find(".multi-selected").remove();
    },
    _add_sidebar_elements: function(Data) {
      var Client, Clients, Data2, List, Metric, Metrics, Value, j, k, l, len, len1, len2, results, results1;
      List = this.element.find("#active-metrics");
      Data2 = {};
      if (this.options.display === "client") {
        for (Metric in Data) {
          Clients = Data[Metric];
          for (j = 0, len = Clients.length; j < len; j++) {
            Client = Clients[j];
            if (Data2[Client] != null) {
              Data2[Client].push(Metric);
            } else {
              Data2[Client] = [Metric];
            }
          }
        }
        Clients = Object.keys(Data2).sort();
        results = [];
        for (k = 0, len1 = Clients.length; k < len1; k++) {
          Client = Clients[k];
          List.append("<li title=\"" + Client + "\" class=\"metric disabled\">\n  <a href=\"#\" style=\"text-align: center;\">" + Client + "</a>\n</li> ");
          Metrics = Data2[Client].sort();
          results.push((function() {
            var l, len2, results1;
            results1 = [];
            for (l = 0, len2 = Metrics.length; l < len2; l++) {
              Metric = Metrics[l];
              results1.push(List.append(UI.sideBar_li(Metric, Client, Metric)));
            }
            return results1;
          })());
        }
        return results;
      } else {
        Metrics = Object.keys(Data).sort();
        results1 = [];
        for (l = 0, len2 = Metrics.length; l < len2; l++) {
          Metric = Metrics[l];
          List.append("<li title=\"" + Metric + "\" class=\"metric disabled\">\n  <a href=\"#\" style=\"text-align: center;\">" + Metric + "</a>\n</li> ");
          Value = Data[Metric].sort();
          results1.push((function() {
            var len3, m, results2;
            results2 = [];
            for (m = 0, len3 = Value.length; m < len3; m++) {
              Client = Value[m];
              results2.push(List.append(UI.sideBar_li(Metric, Client, Client)));
            }
            return results2;
          })());
        }
        return results1;
      }
    },
    _multi_selection: function() {
      this.options.selected = [];
      return this.element.find("li").on("click.multi_select", (function(_this) {
        return function(e) {
          var Clicked_li;
          Clicked_li = _this.element.find("li#" + e.currentTarget.id);
          if (Clicked_li.hasClass("selected") === false) {
            Clicked_li.find("a").css("color", "#f44336");
            return Clicked_li.addClass("selected");
          } else {
            Clicked_li.removeClass("selected");
            return Clicked_li.find("a").css("color", "");
          }
        };
      })(this));
    }
  };

  init_global_buttons = function() {
    var GlobalToolbar;
    GlobalToolbar = UI.globalToolbar();
    GlobalToolbar.find("#addDisplay").on("click", (function(_this) {
      return function() {
        return graph_utils.add_display();
      };
    })(this));
    GlobalToolbar.find("#addSplitDisplay").on("click", (function(_this) {
      return function() {
        return graph_utils.add_split_display();
      };
    })(this));
    GlobalToolbar.find("#bookmarkWindow").on("click", (function(_this) {
      return function() {
        return graph_utils.save_display();
      };
    })(this));
    return $(UI.graphDiv()).find("#load-bookmark").on("change", (function(_this) {
      return function(e) {
        var Bookmark, fd;
        Bookmark = e.target.files[0];
        if (Bookmark) {
          fd = new FileReader();
          fd.onload = function(e) {
            return graph_utils.load_display(JSON.parse(e.target.result));
          };
          return fd.readAsText(Bookmark);
        }
      };
    })(this));
  };

}).call(this);
