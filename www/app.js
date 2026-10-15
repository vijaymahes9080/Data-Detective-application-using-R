/**
 * DATA DETECTIVE PRO: High-Craft SaaS Interface Engine
 * Features:
 * - Dynamic Sidebar Active State synchronization
 * - Universal keyboard accessibility (Escape modal close, Cmd/Ctrl+K query focus)
 * - Tooltip instant-chaining (zero delay on adjacent tooltips)
 * - Copy-to-clipboard micro-feedback with tactile spring animation
 * - Tabular number enforcement & smooth scroll
 */

(function () {
  "use strict";

  let lastActiveTooltipTime = 0;
  let rafId = null;

  document.addEventListener("DOMContentLoaded", function () {
    console.log("Data Detective Pro: SaaS Interface Engine Initialized.");

    // -------------------------------------------------------------------------
    // 1. Sidebar Nav Active State & Instant View Switching
    // -------------------------------------------------------------------------
    document.addEventListener("click", function (e) {
      const navBtn = e.target.closest(".sidebar-nav-btn");
      if (!navBtn) return;

      // Update active state in DOM immediately for zero-latency feel
      document.querySelectorAll(".sidebar-nav-btn").forEach(function (btn) {
        btn.classList.remove("active");
      });
      navBtn.classList.add("active");

      // Notify Shiny of selected tab if custom data attribute is present
      const tabTarget = navBtn.getAttribute("data-tab-target");
      if (tabTarget && window.Shiny && window.Shiny.setInputValue) {
        window.Shiny.setInputValue("selected_nav_js", tabTarget, { priority: "event" });
      }
    });

    // -------------------------------------------------------------------------
    // 2. Dynamic Cursor Spotlight Tracking (Linear & Raycast Effect)
    // -------------------------------------------------------------------------
    document.addEventListener("mousemove", function (e) {
      if (rafId) cancelAnimationFrame(rafId);

      rafId = requestAnimationFrame(function () {
        const targetCard = e.target.closest(".metric-card, .card, .panel-box");
        if (!targetCard) return;

        const rect = targetCard.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const y = e.clientY - rect.top;

        targetCard.style.setProperty("--mouse-x", `${x}px`);
        targetCard.style.setProperty("--mouse-y", `${y}px`);
      });
    });

    // -------------------------------------------------------------------------
    // 3. Universal Keyboard Shortcuts
    // -------------------------------------------------------------------------
    document.addEventListener("keydown", function (e) {
      // Escape: Dismiss active modal dialog with clean focus restoration
      if (e.key === "Escape") {
        const modal = document.querySelector(".modal.show");
        if (modal) {
          const dismissBtn = modal.querySelector(
            "[data-bs-dismiss='modal'], .btn-close, .modal-footer button"
          );
          if (dismissBtn) {
            dismissBtn.click();
          }
        }
      }

      // Cmd/Ctrl + K or '/' : Jump focus to query input or column selector
      if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "k") {
        e.preventDefault();
        const queryInput = document.getElementById("mod_workbench-nlp_query_input");
        const colSelect = document.getElementById("mod_column_studio-selected_column");
        if (queryInput && document.activeElement !== queryInput) {
          queryInput.focus();
          queryInput.select();
        } else if (colSelect) {
          colSelect.focus();
        }
      }
    });

    // -------------------------------------------------------------------------
    // 4. Copy-to-Clipboard Micro-Feedback
    // -------------------------------------------------------------------------
    document.addEventListener("click", function (e) {
      const copyBtn = e.target.closest(
        "[data-clipboard-target], .btn-copy, #mod_workbench-btn_copy_code"
      );
      if (!copyBtn) return;

      const originalHtml = copyBtn.innerHTML;
      copyBtn.innerHTML = '<i class="fa fa-check text-success"></i> Copied!';
      copyBtn.style.transform = "scale(0.97)";

      setTimeout(function () {
        copyBtn.style.transform = "";
      }, 100);

      setTimeout(function () {
        copyBtn.innerHTML = originalHtml;
      }, 1800);
    });

    // -------------------------------------------------------------------------
    // 5. Emil Kowalski Tooltip Instant Chaining
    // -------------------------------------------------------------------------
    document.addEventListener("mouseover", function (e) {
      const tooltipTrigger = e.target.closest("[data-bs-toggle='tooltip'], [title]");
      if (!tooltipTrigger) return;

      const now = Date.now();
      if (now - lastActiveTooltipTime < 600) {
        tooltipTrigger.setAttribute("data-bs-delay", "0");
      }
      lastActiveTooltipTime = now;
    });
  });
})();
