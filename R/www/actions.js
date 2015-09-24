shinyjs.setEvents = function setEvents(){
  d3.selectAll(".node").on("click",function(){
    d3.select(this)
    .select("circle")
    .transition()
    .duration(200)
    .attr("r",5)
    .transition()
    .duration(200)
    .attr("r",9);
    Shiny.onInputChange("selnode",d3.select(this).data()[0].name);
  });
};

shinyjs.fire = function fire(){
  document.querySelector("#tabset").children[3].querySelector("a").onmouseout = function(){shinyjs.setEvents()};
};

