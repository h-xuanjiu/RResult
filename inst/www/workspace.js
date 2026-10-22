(function () {
  function viewport() {
    if (window.Shiny) Shiny.setInputValue("rr_viewport_height", window.innerHeight, {priority: "event"});
  }
  function resizeChart() {
    window.dispatchEvent(new Event("resize"));
  }
  var focusPreview = false;
  function focusPreviewPlot() {
    if (!focusPreview) return;
    var plot = document.getElementById("preview_plot");
    if (!plot || plot.offsetParent === null || !plot.querySelector("img")) return;
    focusPreview = false;
    plot.scrollIntoView({behavior: "smooth", block: "start"});
  }
  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-rr-action]");
    if (!button) return;
    if (button.dataset.rrAction === "style-open") {
      var dialog = document.getElementById(button.dataset.rrTarget);
      if (dialog && !dialog.open) {
        dialog.showModal();
        // Inputs in hidden tabs/dialogs need their slider widths recalculated.
        resizeChart();
      }
      return;
    }
    if (button.dataset.rrAction === "style-close") {
      var current = button.closest("dialog");
      if (current) current.close();
      return;
    }
    var panel = button.closest(".rr-chart-panel");
    if (!panel) return;
    if (button.dataset.rrAction === "settings") {
      var hidden = panel.classList.toggle("rr-controls-hidden");
      button.textContent = hidden ? "Show settings" : "Hide settings";
      button.setAttribute("aria-expanded", String(!hidden));
    } else {
      var expanded = panel.classList.toggle("rr-expanded");
      document.body.classList.toggle("rr-chart-open", expanded);
      button.textContent = expanded ? "Exit expanded view" : "Expand chart";
      button.setAttribute("aria-expanded", String(expanded));
      if (window.Shiny) Shiny.setInputValue("chart_expanded", expanded, {priority: "event"});
    }
    resizeChart();
  });
  document.addEventListener("keydown", function (event) {
    if (event.key !== "Escape" || document.querySelector(".rr-style-dialog[open]")) return;
    var button = document.querySelector(".rr-expanded [data-rr-action='expand']");
    if (button) button.click();
  });
  document.addEventListener("toggle", function (event) {
    if (event.target.id === "explorer_table" && window.Shiny) {
      Shiny.setInputValue("explorer_table_open", event.target.open, {priority: "event"});
    }
  }, true);
  window.addEventListener("resize", viewport);
  if (window.jQuery) {
    window.jQuery(document).on("shiny:connected", viewport);
    window.jQuery(document).on("shiny:value", function (event) {
      if (event.name === "preview_plot" || event.name === "preview_ready") window.requestAnimationFrame(focusPreviewPlot);
    });
    window.jQuery(document).on("shiny:idle", function () { window.requestAnimationFrame(focusPreviewPlot); });
  }
  function register() {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler("rresult-theme", function (dark) {
      document.documentElement.setAttribute("data-rresult-theme", dark ? "dark" : "light");
    });
    Shiny.addCustomMessageHandler("rresult-preview-focus", function (message) {
      focusPreview = true;
      window.requestAnimationFrame(focusPreviewPlot);
    });
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", register);
  else register();
})();

