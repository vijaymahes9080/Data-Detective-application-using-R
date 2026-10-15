// Data Detective Pro: High-Craft Client-Side UI & Accessibility Enhancements
document.addEventListener("DOMContentLoaded", function () {
  console.log("Data Detective Pro: High-Craft UI Initialized.");

  // Keyboard accessibility: ESC closes any active modal dialog
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") {
      const modal = document.querySelector(".modal.show");
      if (modal) {
        const dismissBtn = modal.querySelector("[data-bs-dismiss='modal'], .btn-close, .modal-footer button");
        if (dismissBtn) dismissBtn.click();
      }
    }
  });

  // Tactile button micro-feedback
  document.addEventListener("click", function (e) {
    const btn = e.target.closest(".btn");
    if (btn) {
      btn.style.transition = "transform 80ms cubic-bezier(0.16, 1, 0.3, 1)";
      btn.style.transform = "scale(0.96)";
      setTimeout(() => {
        btn.style.transform = "";
      }, 120);
    }
  });
});
