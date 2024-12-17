document.addEventListener("DOMContentLoaded", function () {
  const switcher = document.createElement("div");
  switcher.innerHTML = `
    <div id="theme-switcher" style="position: fixed; bottom: 20px; right: 20px; z-index: 1000;">
      <label style="cursor: pointer; color: var(--bs-body-color);">
        <input type="checkbox" id="theme-toggle" style="margin-right: 5px;"> Light Mode
      </label>
    </div>
  `;
  document.body.appendChild(switcher);

  const toggle = document.getElementById("theme-toggle");
  const savedTheme = localStorage.getItem("theme") || "dark"; // Default is dark
  applyTheme(savedTheme);

  toggle.checked = savedTheme === "light";

  toggle.addEventListener("change", function () {
    const theme = toggle.checked ? "light" : "dark";
    applyTheme(theme);
    localStorage.setItem("theme", theme);
  });

  function applyTheme(theme) {
    document.documentElement.setAttribute("data-bs-theme", theme);
  }
});
