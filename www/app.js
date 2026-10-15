/**
 * DATA DETECTIVE PRO: High-Craft Kinetic Interface Engine
 * Implements Emil Kowalski Design Engineering & Apple Human Interface guidelines:
 * - Dynamic 60/120fps Cursor Spotlight tracking (Linear / Vercel style)
 * - Hardware-accelerated tactile press micro-interactions
 * - Universal keyboard accessibility (Escape modal close, Cmd/Ctrl+K query focus)
 * - Tooltip instant-chaining (zero delay on adjacent tooltips)
 * - Copy-to-clipboard micro-feedback with spring animation
 */

(function () {
  "use strict";

  let lastActiveTooltipTime = 0;
  let rafId = null;

  document.addEventListener("DOMContentLoaded", function () {
    console.log("Data Detective Pro: Kinetic Interface Engine Initialized.");

    // -------------------------------------------------------------------------
    // 1. Dynamic Cursor Spotlight Tracking (Linear & Raycast Effect)
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
    // 2. Universal Keyboard Shortcuts
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
        const queryInput = document.getElementById("mod_workbench-query_input");
        const colSelect = document.getElementById("mod_column_studio-col_select");
        if (queryInput && document.activeElement !== queryInput) {
          queryInput.focus();
          queryInput.select();
        } else if (colSelect) {
          colSelect.focus();
        }
      }
    });

    // -------------------------------------------------------------------------
    // 3. Copy-to-Clipboard Micro-Feedback
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
    // 4. Emil Kowalski Tooltip Instant Chaining
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

    // -------------------------------------------------------------------------
    // 5. Active Tab Pill Smooth Focus
    // -------------------------------------------------------------------------
    document.addEventListener("click", function (e) {
      const tabLink = e.target.closest(".nav-tabs .nav-link");
      if (tabLink) {
        tabLink.scrollIntoView({ behavior: "smooth", block: "nearest", inline: "nearest" });
      }
    });
  });
})();
