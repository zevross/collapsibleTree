HTMLWidgets.widget({
  name: 'collapsibleTree',
  type: 'output',
  factory: function(el, width, height) {
    var i = 0,
      duration = 750,
      scale = 1,
      root = {},
      options = {},
      newnest = {},
      treemap;
      // transform = d3.zoomIdentity;

    // var transform = d3.zoomIdentity;

    // Optionally enable zooming, and limit to 1/5x or 5x of the original viewport
    var zoom = d3
      .zoom()
      .interpolate(d3.interpolate)
      .scaleExtent([1 / 5, 5])
      .on("zoom", zoomed);
    // .on('zoom', function () {
    //     if (
    //       d3.event.sourceEvent &&
    //       d3.event.sourceEvent.type == "wheel" &&
    //       d3.event.sourceEvent.target.nodeName == "circle"
    //     ) {
    //       event.stopPropagation();
    //     } else {

    //       // debugger;

    //       // svg.attr("transform", d3.event.transform);
    //       g.attr("transform", d3.event.transform);
    //     }
    //   });

    // create our tree object and bind it to the element
    var svg = d3
      .select(el)
      .append("svg")
      .call(zoom)
      // .on('dblclick.zoom', null)
      .attr("width", width)
      .attr("height", height);
      // .call(zoom);
    // .append('g');

    var zoomer = svg.append("rect")
      .attr("width", width)
      .attr("height", height)
      .style("fill", "none")
      .style("pointer-events", "all")
      .call(zoom);


    var g = svg.append("g");
      // .attr("transform", function(d) {
      //   return "translate(" +
      //     (widthMargin) + "," + (heightMargin) + ") scale(" + scale + ")";
      // });

    // zoom.scaleTo(svg, 2);

    function zoomed() {

          if (
            d3.event.sourceEvent &&
            d3.event.sourceEvent.type == "wheel" &&
            d3.event.sourceEvent.target.nodeName == "circle"
          ) {
            event.stopPropagation();
          } else {
            g.attr("transform", d3.event.transform);
          }

      // // console(d3.event);

      // g.attr("transform", d3.event.transform);
    }

    function centerNode(source) {
      t = d3.zoomTransform(zoomer.node());
      // // console("t:");
      // // console(t);

      // // var t = d3.zoomTransform(svg.node());
      // var x = ;
      // var y = ;
      x = -source.y0 * t.k + width / 6;
      y = -source.x0 * t.k + height / 2;

      // debugger;

      // x = -t.x;
      // y = source.x0;
      // y = (-y * t.k) / 2;

      // y = -t.y; // * t.k + height / 2;

      g.transition()
        .duration(duration)
        .attr("transform", "translate(" + x + "," + y + ")scale(" + t.k + ")")
        .on("end", function () {
          zoomer.call(
            zoom.transform,
            d3.zoomIdentity.translate(x, y).scale(t.k)
          );
        });
    }


    // .on("mousedown.zoom", // console('Zoom 1'))
    // .on("touchstart.zoom", // console('Zoom 2'))
    // .on("touchmove.zoom", // console('Zoom 3'))
    // .on("touchend.zoom", // console('Zoom 4'));

    d3.select(el)
      .select("svg")
      .on("mousedown", () => {
        // Without this, clicking the circle will never trigger a mouseup, because zoom will handle mouseup
        // d3.event.stopPropagation();
        // console("G TRANSFORM mousedown");
        // d3.event.stopImmediatePropagation();
        d3.event.stopPropagation();
      })
      .on("click", () => {
        // console("G TRANSFORM click");
        // debugger;
        // d3.event.stopImmediatePropagation();
      })
      .on("mouseup", () => {
        // d3.event.stopPropagation();
        // console("G TRANSFORM mouseup");
      })
      .on("dblclick", () => {
        // console("G TRANSFORM dblclick");
        // d3.event.stopImmediatePropagation();
        // d3.event.stopPropagation();
      })
      .on("zoom", () => {
        // d3.event.stopPropagation();
        // console("G TRANSFORM 1st zoom");
        d3.event.stopImmediatePropagation();
      });

    function center() {
      return d3.zoomIdentity.scale(0.5);
    }

    function dragged(d) {
      d3.select(this)
        .attr("cx", (d.x = d3.event.x))
        .attr("cy", (d.y = d3.event.y));
    }

    // svg.call(zoom.transform, center);

    if (options.zoomable) {
      // svg.call(zoom.transform, d3.zoomIdentity.scale(1));
      // svg.call(zoom.transform, center);

      // d3.select(el).select("svg").call(zoom);

      // d3.select(el).select("svg").call(zoom, center);
      svg.call(zoom).call(zoom.transform, d3.zoomIdentity);
    }

    d3.select("svg")
      .select("g")
      .on("mousedown", () => {
        // Without this, clicking the circle will never trigger a mouseup, because zoom will handle mouseup
        // d3.event.stopPropagation();
        // console("mousedown");
      })
      .on("click", () => {
        // d3.event.stopPropagation();
        // console("click");
      })
      .on("mouseup", () => {
        // d3.event.stopPropagation();
        // console("mouseup");
      })
      .on("dblclick", () => {
        // d3.event.stopPropagation();
        // console("dblclick");
      })
      .on("zoom", () => {
        // console("OTHER zoom");
        // d3.event.stopPropagation();
      });

    // Define the div for the tooltip
    var tooltip = d3
      .select(el)
      .append("div")
      .attr("class", "tooltip")
      .style("opacity", 0);

    function update(source, animate) {
      // Assigns the x and y position for the nodes
      var treeData = treemap(root);

      // Compute the new tree layout.
      var nodes = treeData.descendants(),
        links = treeData.descendants().slice(1);

      // Normalize for fixed-depth.
      nodes.forEach(function (d) {
        d.y = d.depth * options.linkLength;
      });

      // ****************** Nodes section ***************************

      // Update the nodes...
      var node = g
        .selectAll("g.node")
        // var node = svg.selectAll('g.node')
        .data(nodes, function (d) {
          if (d.depth > 1) {
            d.root_id = d.parent.root_id;
          } else {
            d.root_id = d.id;
          }

          return d.id || (d.id = ++i);
        });
        // .call(d3.drag()
        //   .on('drag', dragged));

      // Enter any new nodes at the parent's previous position.
      var nodeEnter = node
        .enter()
        .append("g")
        .attr("class", "node")
        .attr("transform", function (d) {
          // // console('d:');
          // // console(d);
          return "translate(" + source.y0 + "," + source.x0 + ")";
        })
        .on("mousedown", function (d) {
          // console("MOUSE DOWN (CLICK)");
          event.stopPropagation();
        })
        .on("click", function (d) {
          // console("CLICK (CLICK)");
          click(d);
          event.stopImmediatePropagation();
        }) // ;
        .on("mouseup", function (d) {
          // console("MOUSE UP (CLICK)");
          // event.stopImmediatePropagation();
        })
        .on("dblclick", function (d) {
          // console("DBL CLICK");
          event.stopImmediatePropagation();
        }) // ;
        .on("zoom", function (d) {
          // console("OTHER ZOOM");
        });

      // Add tooltips, if specified in options
      if (options.tooltip) {
        nodeEnter = nodeEnter
          .on("mouseover", mouseover)
          .on("mouseout", mouseout);
      }

      // Add Circle for the nodes
      nodeEnter
        .append("circle")
        .attr("class", "node")
        .attr("r", 1e-6)
        .attr("r", function (d) {
          //return Math.log(d.data.SizeOfNode) || 5; // default radius was 10, reduced to 5
          return Math.sqrt(d.data.SizeOfNode); // default radius was 10, reduced to 5
        })
        .style("stroke-width", function (d) {
          return d._children ? 1 : 1;
        });

      // Add labels for the nodes
      nodeEnter
        .append("text")
        .attr("class", "node-text")
        .attr("dy", ".35em")
        .attr("x", function (d) {
          // Scale padding for label to the size of node
          //var padding = (Math.log(d.data.SizeOfNode) || 5) + 3;
          var padding = Math.sqrt(d.data.SizeOfNode) + 3;
          return d.children || d._children ? padding : padding;
        })
        .style("font-size", options.fontSize + "px")
        .text(function (d) {
          return d.data.name;
        });

      // UPDATE
      var nodeUpdate = nodeEnter.merge(node);

      // Transition to the proper position for the node
      nodeUpdate
        .transition()
        .duration(animate ? duration : 0)
        .attr("transform", function (d) {
          return "translate(" + d.y + "," + d.x + ")";
        });

      // Update the node attributes and style
      nodeUpdate
        .select("circle.node")
        .style("fill", function (d) {
          if (d._isSelected === true) {
            return options.fill;
          } else {
            return "#FFF";
          }
        })
        .attr("cursor", "pointer");

      // Update the node-text attributes and style
      nodeUpdate
        .select("text.node-text")
        .attr("text-anchor", function (d) {
          if (d.children) {
            return "end";
          } else {
            return "start";
          }
        })
        .attr("x", function (d) {
          //var padding = (Math.log(d.data.SizeOfNode) || 5) + 3;
          var padding = Math.sqrt(d.data.SizeOfNode) + 3;
          if (d.children) {
            return -1 * padding;
          } else {
            return padding;
          }
        })
        .style("font-size", function (d) {
          if (d._isSelected === true) {
            return options.fontSize + 1 + "px";
          } else {
            return options.fontSize + "px";
          }
        })
        .style("font-weight", function (d) {
          if (d._isSelected === true) {
            return "bolder";
          } else {
            return "lighter";
          }
        });

      // Remove any exiting nodes
      var nodeExit = node
        .exit()
        .transition()
        .duration(duration)
        .attr("transform", function (d) {
          return "translate(" + source.y + "," + source.x + ")";
        })
        .remove();

      // On exit reduce the node circles size to 0
      nodeExit.select("circle").attr("r", 1e-6);

      // On exit reduce the opacity of text labels
      nodeExit.select("text").style("fill-opacity", 1e-6);

      // ****************** links section ***************************

      // Update the links...
      var link = g
        .selectAll("path.link")
        // var link = svg.selectAll('path.link')
        .data(links, function (d) {
          return d.id;
        });

      // Enter any new links at the parent's previous position.
      var linkEnter = link
        .enter()
        .insert("path", "g")
        .attr("class", "link")
        // Potentially, this may one day be mappable
        // .style('stroke-width', function(d) { return d.data.linkWidth || 1 })
        .attr("d", function (d) {
          var o = { x: source.x0, y: source.y0 };
          return diagonal(o, o);
        });

      // UPDATE
      var linkUpdate = linkEnter.merge(link);

      // Transition back to the parent element position
      linkUpdate
        .transition()
        .duration(animate ? duration : 0)
        .attr("d", function (d) {
          return diagonal(d, d.parent);
        });

      // Remove any exiting links
      link
        .exit()
        .transition()
        // var linkExit = link.exit().transition()
        .duration(duration)
        .attr("d", function (d) {
          var o = { x: source.x, y: source.y };
          return diagonal(o, o);
        })
        .remove();

      // Store the old positions for transition.
      nodes.forEach(function (d) {
        d.x0 = d.x;
        d.y0 = d.y;
      });

      // Creates a curved (diagonal) path from parent to the child nodes
      function diagonal(s, d) {
        path =
          `M ${ s.y } ${ s.x } C ${ (s.y + d.y) / 2 } ${ s.x }, ${ (s.y + d.y) / 2 } ${ d.x }, ${ d.y } ${ d.x }`;

        return path;
      }

      newnest = nodes
        .filter(
          (nodes) =>
            {
              return nodes.depth > 0 &&
                (nodes._isSelected === true || nodes.data.collapsed === false);
            }
        )
        .map(function (nd) {
          return {
            id: nd.root_id,
            parent: nd.parent.data.name,
            level: options.hierarchy[nd.depth - 1],
            value: nd.data.name,
          };
        });

      // Toggle children on click.
      function click(d) {
        // toggle _isselected
        if (
          d._isSelected === false ||
          d._isSelected === null ||
          d._isSelected === undefined
        ) {
          d._isSelected = true;
        } else {
          d._isSelected = false;
        }

        // // console("`click(d) & d:");
        // // console(d);

        if (d.height > 0) {
          // toggle children
          if (d.children) {
            d._children = d.children;
            d.children = null;
          } else {
            d.children = d._children;
            d._children = null;
          }

          // toggle `collapsed`
          if (d.data.collapsed === false) {
            // if (d.data.collapsed === false || d.data.collapsed === null) {
            d.data.collapsed = true;
          } else if (d.data.collapsed === true) {
            d.data.collapsed = false;
          } else {
            d.data.collapsed = true;
          }

          // debugger;

          // var t = d3.zoomTransform(g.node());
          // // var t = d3.zoomTransform(svg.node());
          // var x = -source.y0;
          // var y = -source.x0;
          // var new_x = x * t.k + width / 6;
          // var new_y = y * t.k + height / 2;

          // // console(d3.event);

          // g.transition()
          //   .duration(750)
          //   // .attr("transform", `translate(${ new_x },${ new_y })scale(${ t.k })`);
          //   .attr("transform", `translate(${-d.x},${-d.y / 2})`);

          // zoom.scaleTo(svg, t.k);

          // svg.call(zoom.translateBy, -d.x / 2, 0);


          // svg.transition().duration(750).call(zoom.translateTo, d.x, d.y);

          centerNode(source);
        }

        update(d, true);

        var nodes = d3.selectAll("circle.node").size();

        if (d._isSelected === false) {
          if (d._children) {
            nodes = nodes - d._children.length;
          }
        }

        // width and height, corrected for margins
        var heightMargin = height - options.margin.top - options.margin.bottom,
          widthMargin = width - options.margin.left - options.margin.right;

        if (nodes < 35) {
          // Update the treemap to fit the new canvas size
          treemap = d3
            .tree()
            .size([heightMargin, widthMargin])
            .separation(separationFun);
        } else if (nodes >= 35 && nodes < 130) {
          // Update the treemap to fit the new canvas size
          treemap = d3
            .tree()
            .size([heightMargin * 2, widthMargin])
            .separation(separationFun);
        } else if (nodes >= 130 && nodes < 160) {
          // Update the treemap to fit the new canvas size
          treemap = d3
            .tree()
            .size([heightMargin * 3, widthMargin])
            .separation(separationFun);
        } else if (nodes > 160) {
          // Update the treemap to fit the new canvas size
          treemap = d3
            .tree()
            .size([heightMargin * 4, widthMargin])
            .separation(separationFun);
        }

        update(root, true);
        // update(treemap, true);

        // Hide the tooltip after clicking
        // tooltip.transition()
        // .duration(100)
        // .style('opacity', 0);

        // Update Shiny inputs, if applicable
        if (options.input) {
          Shiny.setInputValue(options.input, JSON.stringify(newnest), {
            priority: "event",
          });
        }
      }

      // Show tooltip on mouseover
      function mouseover(d, i) {
        if (
          d._isSelected === false ||
          d._isSelected === null ||
          d._isSelected === undefined
        ) {
          // // console(this);
          d3.select(this)
            .select("text.node-text")
            .style("font-size", "12px")
            .style("font-weight", "bolder");
        }

        // tooltip.transition()
        // .duration(750)
        // .style('opacity', 0.9);

        // Show either a constructed tooltip, or override with one from the data
        tooltip
          .html(
            d.data.tooltip ||
              d.data.name +
                "<br>" +
                options.attribute +
                ": " +
                d.data.WeightOfNode
          )
          // Make the tooltip font size just a little bit bigger
          .style("font-size", options.fontSize + 1 + "px")
          .style("left", d3.event.layerX + "px")
          .style("top", d3.event.layerY - 10 + "px");
      }

      // Hide tooltip on mouseout
      function mouseout(d, i) {
        if (
          d._isSelected === false ||
          d._isSelected === null ||
          d._isSelected === undefined
        ) {
          d3.select(this)
            .select("text.node-text")
            .style("font-size", "11px")
            .style("font-weight", "lighter");
        }

        // tooltip.transition()
        // .duration(750)
        // .style('opacity', 0);
      }
    }

    return {
      renderValue: function (x) {
        // Assigns parent, children, height, depth
        root = d3.hierarchy(x.data, function (d) {
          return d.children;
        });
        root.x0 = height / 2;
        // root.x0 = 0;
        root.y0 = 0;
        // root.y0 = width / 2;
        root._isSelected = true;
        root.collapsed = true;

        // Attach options as a property of the instance
        options = x.options;

        // Update the canvas with the new dimensions
        // g = g.attr(
        //   // g = g.attr(
        //   // svg = svg.attr(
        //   "transform",
        //   "translate(" + options.margin.left + "," + options.margin.top + ")"
        // );

        tooltip = tooltip.attr("class", "tooltip").style("opacity", 0);

        // width and height, corrected for margins
        (heightMargin = height - options.margin.top - options.margin.bottom),
          (widthMargin = width - options.margin.left - options.margin.right);

        // declares a tree layout and assigns the size
        treemap = d3
          .tree()
          .size([heightMargin, widthMargin])
          .separation(separationFun);
        // update(root);

        // Calculate a reasonable link length, if not otherwise specified
        if (!options.linkLength) {
          options.linkResponsive = true;
          options.linkLength = 2 * (widthMargin / options.hierarchy.length);
          if (options.linkLength < 175) {
            options.linkLength = 175; // Offscreen or too short
          }
        }

        // Optionally collapse after the second level
        if (options.collapsed) root.children.forEach(collapse);
        // update(root);

        // Collapse the node and all it's children
        function collapse(d) {
          if (d.depth > 1) {
            d.root_id = d.parent.root_id;
          } else {
            d.root_id = d.id;
          }

          // A collapsed data value was specified and is true
          if (
            d.children &&
            options.collapsed in d.data &&
            !d.data[options.collapsed]
          ) {
            d._isSelected = true;

            d.children.forEach(collapse);
          } else if (d.children) {
            d._isSelected = false;

            d._children = d.children;
            d._children.forEach(collapse);
            d.children = null;
          }

          // update(d);
        }

        Shiny.setInputValue(options.input, JSON.stringify(newnest), {
          priority: "event",
        });

        // d3.select("svg").call(zoom);
        if (options.zoomable) {
          // d3.select(el)
          // .select("svg")
          // .call(zoom, center);

          // d3.select(el).select("svg").call(zoom);

          // svg.call(zoom.transform, center);

          // svg.call(zoom).call(zoom.transform, d3.zoomIdentity.translate(width/2, 0).scale(1));

          svg.call(zoom).call(zoom.transform, d3.zoomIdentity);
        }

        centerNode(root);

        update(root, false);
      },

      resize: function (width, height) {
        // // console("RESIZE");

        // Resize the canvas
        d3.select(el)
          .select("svg")
          .select("g")
          .attr("width", width)
          .attr("height", height);

        // width and height, corrected for margins
        var heightMargin = height - options.margin.top - options.margin.bottom,
          widthMargin = width - options.margin.left - options.margin.right;

        // Calculate a reasonable link length, if not originally specified
        if (options.linkResponsive) {
          options.linkLength = 2 * (widthMargin / options.hierarchy.length);
          if (options.linkLength < 175) {
            options.linkLength = 175; // Offscreen or too short
          }
        }

        // Update the treemap to fit the new canvas size
        treemap = d3
          .tree()
          .size([heightMargin, widthMargin])
          .separation(separationFun);
        update(root, false);
      },
      // Make the instance properties available as a property of the widget
      svg: svg,
      root: root,
      options: options,
    };
  }
});

function separationFun(a, b) {

  // // console("SeparationFun\n");

  var height = Math.sqrt(a.data.SizeOfNode) + Math.sqrt(b.data.SizeOfNode),
  // Scale distance to SizeOfNode, if defined
  distance = (height)/25; // increase denominator for better spacing in DEAP app
  if (distance < .4) {
    distance = .4
  }
  if (distance > 1.5) {
    distance = 1.5
  }
  //// console(distance);
  return (a.parent === b.parent ? distance : 1);
};
