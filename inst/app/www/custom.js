// Hide the "no results yet" empty-state placeholder on a step's Overview
// tab once additional output tabs have been appended (appendTab() adds new
// <li> nav-links to the same tabsetPanel). Purely cosmetic — does not touch
// any Shiny inputs/outputs.
(function () {
  function refresh(navTabs) {
    var pane = navTabs.closest('.tab-content, .bslib-page');
    if (!pane) pane = document;
    var hasExtraTabs = navTabs.querySelectorAll('a.nav-link').length > 1;
    var container = navTabs.parentElement.querySelector('.tab-content') ||
      document.querySelector('.tab-content');
    if (!container) return;
    var emptyStates = container.querySelectorAll(':scope > .tab-pane .empty-state');
    emptyStates.forEach(function (el) {
      el.style.display = hasExtraTabs ? 'none' : '';
    });
  }

  // CR-PERF-14: one scan per animation frame, however many mutations fire.
  var scheduled = false;
  function scan() {
    if (scheduled) return;
    scheduled = true;
    window.requestAnimationFrame(function () {
      scheduled = false;
      document.querySelectorAll('ul.nav-tabs').forEach(refresh);
    });
  }

  document.addEventListener('DOMContentLoaded', function () {
    scan();
    var observer = new MutationObserver(scan);
    observer.observe(document.body, { childList: true, subtree: true });
  });
})();

// Stop an automatic configuration run when its modal is dismissed via Escape
// or the backdrop. The server cannot interrupt a synchronous stage, but it can
// prevent later stages from starting.
(function () {
  var seq = 0;
  document.addEventListener('hidden.bs.modal', function (e) {
    if (!e.target || e.target.id !== 'shiny-modal') return;
    if (!window.Shiny || !Shiny.setInputValue) return;
    seq += 1;
    Shiny.setInputValue('import_dismissed', seq, { priority: 'event' });
  }, true);
})();

// Submit the overview connection form with Enter from a single-line field.
// Scope this to the Data card so other module inputs keep their own behavior.
(function () {
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Enter' || e.isComposing) return;
    var field = e.target;
    if (!field.matches('.connect-card input[type="text"], .connect-card input[type="password"], .connect-card select')) return;
    if (field.closest('textarea, [contenteditable="true"]')) return;
    var button = field.closest('.connect-card').querySelector('.connection-action-row .btn');
    if (!button || button.disabled) return;
    e.preventDefault();
    button.click();
  });
})();

// ---- Config flyouts (UI-02) --------------------------------------------------
// Shared behavior for the .config-flyout disclosure panels built by
// config_flyout_block() (utils_ui.R):
//   - exactly one flyout open at a time,
//   - aria-expanded kept in sync on the toggle buttons,
//   - focus moved into the flyout on open and back to its toggle on close,
//   - Escape closes the open flyout,
//   - the panel is positioned beside its own toggle (not a shared viewport
//     spot) and follows it while scrolling.
// Panels are marked data-flyout-for="<toggle id>" and get a stable
// "<toggle id>_panel" id; the toggle button carries aria-expanded/controls.
(function () {
  var wasOpen = new WeakMap();
  // Clicks we dispatch ourselves to close other flyouts must not re-trigger
  // the close-others scan (HTMLElement.click() dispatches synchronously).
  var synthetic = new WeakSet();

  function isVisible(el) {
    return el.getClientRects().length > 0;
  }

  function isFixed(panel) {
    return getComputedStyle(panel).position === 'fixed';
  }

  function positionPanel(panel) {
    var anchor = panel.closest('.config-flyout-anchor');
    if (!anchor) return;
    var r = anchor.getBoundingClientRect();
    if (r.width === 0 && r.height === 0) return;
    var w = panel.offsetWidth;
    var h = panel.offsetHeight;
    var left = r.right + 10;
    if (left + w > window.innerWidth - 10) {
      left = Math.max(10, window.innerWidth - w - 10);
    }
    var top = r.top;
    if (top + h > window.innerHeight - 10) {
      top = Math.max(10, window.innerHeight - h - 10);
    }
    panel.style.left = left + 'px';
    panel.style.top = top + 'px';
  }

  // Re-apply aria-expanded / position; optionally move focus for a
  // hidden->visible (focus the panel) or visible->hidden (focus the toggle)
  // transition. When both happen in one pass, the opened panel wins.
  function sync(moveFocus) {
    var opened = null;
    var closed = null;
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      var open = isVisible(panel);
      var toggle = document.getElementById(panel.getAttribute('data-flyout-for'));
      if (toggle) toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (open && isFixed(panel)) positionPanel(panel);
      var prev = wasOpen.get(panel) || false;
      if (open && !prev && opened === null) opened = panel;
      if (!open && prev) closed = panel;
      wasOpen.set(panel, open);
    });
    if (!moveFocus) return;
    if (opened) {
      opened.setAttribute('tabindex', '-1');
      opened.focus({ preventScroll: true });
    } else if (closed) {
      var t = document.getElementById(closed.getAttribute('data-flyout-for'));
      if (t) t.focus({ preventScroll: true });
    }
  }

  function closePanel(panel) {
    var owner = document.getElementById(panel.getAttribute('data-flyout-for'));
    if (owner) {
      synthetic.add(owner);
      owner.click();
    }
  }

  document.addEventListener('click', function (e) {
    var btn = e.target.closest('.config-flyout-toggle');
    if (!btn) return;
    if (synthetic.has(btn)) {
      synthetic.delete(btn);
      return;
    }
    // One open at a time: close every other visible flyout via its toggle.
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (!isVisible(panel)) return;
      if (panel.getAttribute('data-flyout-for') !== btn.id) closePanel(panel);
    });
    setTimeout(function () { sync(true); }, 60);
  });

  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Escape') return;
    var lastToggle = null;
    var anyClosed = false;
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (!isVisible(panel)) return;
      closePanel(panel);
      lastToggle = document.getElementById(panel.getAttribute('data-flyout-for'));
      anyClosed = true;
    });
    if (anyClosed) {
      e.preventDefault();
      setTimeout(function () {
        sync(false);
        if (lastToggle) lastToggle.focus({ preventScroll: true });
      }, 60);
    }
  });

  // Keep an open fixed-position flyout beside its toggle while the page or
  // any container scrolls.
  function repositionOpen() {
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (isVisible(panel) && isFixed(panel)) positionPanel(panel);
    });
  }
  window.addEventListener('scroll', repositionOpen, { passive: true, capture: true });
  window.addEventListener('resize', repositionOpen);

  document.addEventListener('DOMContentLoaded', function () {
    sync(false);
  });
})();

// Info popover icons without a title (info_popover() in utils_ui.R) get the
// generic name "More information". Give each a specific name from the heading
// or label it sits in, e.g. "More information: Poverty line" (WCAG 2.4.6).
(function () {
  var GENERIC = 'More information';
  var HOST = 'h1,h2,h3,h4,h5,h6,label,legend,.headline-card-label,.card-header';
  function nameOf(icon) {
    var pop = icon.closest('bslib-popover') || icon;
    var host = pop.closest(HOST) || pop.parentElement;
    if (!host) return '';
    var clone = host.cloneNode(true);
    clone.querySelectorAll('.wise-info-icon,bslib-popover,script,style,template')
      .forEach(function (n) { n.remove(); });
    var text = (clone.textContent || '').replace(/\s+/g, ' ').trim();
    return text.length > 60 ? text.slice(0, 57) + '...' : text;
  }
  function nameIcons() {
    document.querySelectorAll('.wise-info-icon').forEach(function (icon) {
      if (icon.getAttribute('aria-label') !== GENERIC) return;
      var text = nameOf(icon);
      if (text) icon.setAttribute('aria-label', GENERIC + ': ' + text);
    });
  }
  // axe fixes for markup generated by Font Awesome and bslib (CR-A11Y-09).
  function fixGenerated() {
    // Decorative icons: shiny::icon() writes role="presentation" together with
    // an aria-label, which ARIA prohibits.
    document.querySelectorAll('i[role="presentation"][aria-label]').forEach(function (i) {
      i.removeAttribute('aria-label');
      i.setAttribute('aria-hidden', 'true');
    });
    // The navbar is a tablist, but also holds a dropdown, the spacer and
    // plain nav items; only tabs may be children of a tablist.
    document.querySelectorAll('ul.navbar-nav[role="tablist"] > li').forEach(function (li) {
      if (!li.querySelector(':scope > [role="tab"]') && li.getAttribute('role') !== 'presentation') {
        li.setAttribute('role', 'presentation');
      }
    });
    // A focusable separator needs its current value: the sidebar's share of
    // the layout width, in percent.
    document.querySelectorAll('.bslib-sidebar-resize-handle[role="separator"]').forEach(function (h) {
      var layout = h.closest('.bslib-sidebar-layout');
      var side = layout && layout.querySelector(':scope > .sidebar');
      if (!side || !layout.clientWidth) return;
      h.setAttribute('aria-valuemin', '0');
      h.setAttribute('aria-valuemax', '100');
      h.setAttribute('aria-valuenow', String(Math.round(100 * side.offsetWidth / layout.clientWidth)));
    });
  }
  var queued = false;
  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(function () { queued = false; nameIcons(); fixGenerated(); });
  }
  ['pointerup', 'keyup'].forEach(function (ev) { document.addEventListener(ev, schedule); });
  document.addEventListener('DOMContentLoaded', function () {
    nameIcons();
    fixGenerated();
    new MutationObserver(schedule)
      .observe(document.body, { childList: true, subtree: true });
  });
})();

// Map legend info markers (.wx-tip, wx_info_marker() in fct_weatherstats.R):
// Escape dismisses the tooltip while it is hovered or focused (WCAG 1.4.13).
// The dismissal lasts until the pointer leaves or focus moves away.
(function () {
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Escape') return;
    document.querySelectorAll('.wx-tip').forEach(function (tip) {
      if (tip.matches(':hover') || tip.matches(':focus')) {
        tip.classList.add('wx-tip-dismissed');
      }
    });
  });
  function reset(e) {
    var tip = e.target.closest && e.target.closest('.wx-tip');
    if (tip && !tip.contains(e.relatedTarget)) {
      tip.classList.remove('wx-tip-dismissed');
    }
  }
  document.addEventListener('mouseout', reset);
  document.addEventListener('focusout', reset);
})();

// Server-driven disabled state for pill_toggle() radios and plain inputs
// (update_pill_toggle_disabled() / update_input_disabled() in utils_ui.R).
// Inserted controls may not be in the DOM yet when the message arrives, so
// the handler retries briefly.
(function () {
  function apply(msg) {
    if (msg.kind === 'input') {
      var el = document.getElementById(msg.id);
      if (!el) return false;
      el.disabled = !!msg.all;
      if (msg.all && msg.tooltip) el.title = msg.tooltip; else el.removeAttribute('title');
      return true;
    }
    var radios = document.querySelectorAll('input[type="radio"][name="' + msg.id + '"]');
    if (!radios.length) return false;
    var values = [].concat(msg.values || []);
    radios.forEach(function (r) {
      var off = !!msg.all || values.indexOf(r.value) > -1;
      r.disabled = off;
      if (off) r.setAttribute('aria-disabled', 'true'); else r.removeAttribute('aria-disabled');
      var wrap = r.closest('.radio-inline, .form-check-inline, .radio, .form-check') || r.parentElement;
      if (!wrap) return;
      wrap.classList.toggle('pill-disabled', off);
      if (off && msg.tooltip) wrap.title = msg.tooltip; else wrap.removeAttribute('title');
    });
    return true;
  }
  function run(msg, tries) {
    if (apply(msg) || tries <= 0) return;
    setTimeout(function () { run(msg, tries - 1); }, 100);
  }
  function register() {
    Shiny.addCustomMessageHandler('wise_set_disabled', function (msg) { run(msg, 30); });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) register();
  else document.addEventListener('shiny:connected', register, { once: true });
})();

// ---- Slider accessible names (CR-A11Y-04) ------------------------------------
// ionRangeSlider's keyboard focus target is the .irs-line span, which has no
// role or name. Expose it as a slider named by the input's visible label (or
// Shiny's own label) and keep its value in sync. jQuery events because Shiny
// and ionRangeSlider fire shiny:bound / change through jQuery.
(function () {
  if (!window.jQuery) return;
  function sync(input) {
    var wrap = input.parentElement;
    var line = wrap && wrap.querySelector('.irs-line');
    if (!line || !input.id) return;
    line.setAttribute('role', 'slider');
    var lbl = Array.prototype.find.call(
      document.querySelectorAll('label[for="' + input.id + '"]'),
      function (l) { return l.textContent.trim() !== ''; }
    );
    if (lbl) {
      if (!lbl.id) lbl.id = input.id + '-a11y-label';
      line.setAttribute('aria-labelledby', lbl.id);
    }
    if (input.dataset.min) line.setAttribute('aria-valuemin', input.dataset.min);
    if (input.dataset.max) line.setAttribute('aria-valuemax', input.dataset.max);
    var v = String(input.value);
    if (v.indexOf(';') === -1) {
      line.setAttribute('aria-valuenow', v);
    } else {
      line.setAttribute('aria-valuetext', v.replace(';', ' to '));
    }
  }
  jQuery(document).on('shiny:bound change', 'input.js-range-slider', function () {
    sync(this);
  });
})();
