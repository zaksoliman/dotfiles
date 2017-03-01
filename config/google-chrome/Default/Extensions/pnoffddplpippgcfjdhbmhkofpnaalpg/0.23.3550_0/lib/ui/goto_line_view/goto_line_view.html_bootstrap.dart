library app_bootstrap;

import 'package:polymer/polymer.dart';

import 'package:spark_widgets/common/spark_widget.dart' as i0;
import 'package:spark_widgets/spark_button/spark_button.dart' as i1;
import 'goto_line_view.dart' as i2;
import 'package:smoke/smoke.dart' show Declaration, PROPERTY, METHOD;
import 'package:smoke/static.dart' show useGeneratedCode, StaticConfiguration;
import 'package:spark_widgets/common/spark_widget.dart' as smoke_0;
import 'package:polymer/polymer.dart' as smoke_1;
import 'package:spark_widgets/spark_button/spark_button.dart' as smoke_2;
import 'dart:html' as smoke_3;
import 'goto_line_view.dart' as smoke_4;
abstract class _M0 {} // HtmlElement & Polymer
abstract class _M1 {} // _M0 & ChangeNotifier

void main() {
  useGeneratedCode(new StaticConfiguration(
      checkedMode: false,
      getters: {
        #active: (o) => o.active,
        #command: (o) => o.command,
        #disabled: (o) => o.disabled,
        #flat: (o) => o.flat,
        #handleClick: (o) => o.handleClick,
        #handleKeyDown: (o) => o.handleKeyDown,
        #handleKeyPress: (o) => o.handleKeyPress,
        #hide: (o) => o.hide,
        #hoverStyle: (o) => o.hoverStyle,
        #padding: (o) => o.padding,
        #primary: (o) => o.primary,
        #raised: (o) => o.raised,
        #round: (o) => o.round,
        #tooltip: (o) => o.tooltip,
      },
      setters: {
        #active: (o, v) { o.active = v; },
        #command: (o, v) { o.command = v; },
        #disabled: (o, v) { o.disabled = v; },
        #flat: (o, v) { o.flat = v; },
        #hoverStyle: (o, v) { o.hoverStyle = v; },
        #padding: (o, v) { o.padding = v; },
        #primary: (o, v) { o.primary = v; },
        #raised: (o, v) { o.raised = v; },
        #round: (o, v) { o.round = v; },
        #tooltip: (o, v) { o.tooltip = v; },
      },
      parents: {
        smoke_4.GotoLineView: smoke_0.SparkWidget,
        smoke_1.PolymerElement: _M1,
        smoke_0.SparkWidget: smoke_1.PolymerElement,
        smoke_2.SparkButton: smoke_0.SparkWidget,
        _M0: smoke_3.HtmlElement,
        _M1: _M0,
      },
      declarations: {
        smoke_4.GotoLineView: {},
        smoke_1.PolymerElement: {},
        smoke_0.SparkWidget: {},
        smoke_2.SparkButton: {
          #active: const Declaration(#active, bool, annotations: const [smoke_1.published]),
          #command: const Declaration(#command, String, annotations: const [smoke_1.published]),
          #disabled: const Declaration(#disabled, bool, kind: PROPERTY, annotations: const [smoke_1.published]),
          #flat: const Declaration(#flat, bool, annotations: const [smoke_1.published]),
          #hoverStyle: const Declaration(#hoverStyle, String, annotations: const [smoke_0.published_reflected]),
          #padding: const Declaration(#padding, String, annotations: const [smoke_0.published_reflected]),
          #primary: const Declaration(#primary, bool, annotations: const [smoke_1.published]),
          #raised: const Declaration(#raised, bool, annotations: const [smoke_1.published]),
          #round: const Declaration(#round, bool, annotations: const [smoke_1.published]),
          #tooltip: const Declaration(#tooltip, String, annotations: const [smoke_1.published]),
        },
      },
      names: {
        #active: r'active',
        #command: r'command',
        #disabled: r'disabled',
        #flat: r'flat',
        #handleClick: r'handleClick',
        #handleKeyDown: r'handleKeyDown',
        #handleKeyPress: r'handleKeyPress',
        #hide: r'hide',
        #hoverStyle: r'hoverStyle',
        #padding: r'padding',
        #primary: r'primary',
        #raised: r'raised',
        #round: r'round',
        #tooltip: r'tooltip',
      }));
  configureForDeployment([
      () => Polymer.register('spark-widget', i0.SparkWidget),
      () => Polymer.register('spark-button', i1.SparkButton),
      () => Polymer.register('goto-line-view', i2.GotoLineView),
    ]);
  i2.main();
}
