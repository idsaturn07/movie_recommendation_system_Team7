if (!Array.isArray(CM_TITLES)) CM_TITLES = [];
var _st = null;

/* FILTER */
function cmFilter(q) {
  var clr  = document.getElementById("cm-clr");
  var drop = document.getElementById("cm-drop");
  if (clr) clr.style.display = q.length > 0 ? "block" : "none";
  if (!q || q.length < 1) { drop.style.display = "none"; return; }
  clearTimeout(_st);
  _st = setTimeout(function () { cmBuildDrop(q); }, 120);
}

/*  BUILD DROPDOWN  */
function cmBuildDrop(q) {
  var ql = q.toLowerCase();
  var starts = [], contains = [];
  for (var i = 0; i < CM_TITLES.length; i++) {
    var tl = CM_TITLES[i].toLowerCase();
    if (tl.startsWith(ql))          starts.push(CM_TITLES[i]);
    else if (tl.indexOf(ql) !== -1) contains.push(CM_TITLES[i]);
    if (starts.length >= 6 && contains.length >= 4) break;
  }
  starts   = starts.slice(0, 6).sort(function (a, b) { return a.length - b.length; });
  contains = contains.slice(0, 4).sort(function (a, b) { return a.length - b.length; });

  var drop = document.getElementById("cm-drop");
  if (starts.length === 0 && contains.length === 0) {
    drop.innerHTML = '<div class="drop-empty">No movies found</div>';
    drop.style.display = "block";
    return;
  }

  var re = new RegExp("(" + ql.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + ")", "gi");
  function esc(t) {
    return t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }
  function mkItem(t) {
    var safe = esc(t);
    return '<div class="drop-item" data-title="' + safe + '">&#127916; ' +
           safe.replace(re, "<b>$1</b>") + '</div>';
  }

  var html = "";
  if (starts.length > 0) {
    html += '<div class="drop-label">Best match</div>';
    starts.forEach(function (t) { html += mkItem(t); });
  }
  if (contains.length > 0) {
    html += '<div class="drop-label">Also contains</div>';
    contains.forEach(function (t) { html += mkItem(t); });
  }

  drop.innerHTML   = html;
  drop.style.display = "block";

  drop.querySelectorAll(".drop-item[data-title]").forEach(function (el) {
    el.addEventListener("click", function (e) {
      e.stopPropagation();
      cmPick(el.getAttribute("data-title"));
    });
  });
}

/*  PICK  */
function cmPick(title) {
  var inp = document.getElementById("cm-inp");
  var clr = document.getElementById("cm-clr");
  if (inp) inp.value = title;
  if (clr) clr.style.display = "block";
  document.getElementById("cm-drop").style.display = "none";
  Shiny.setInputValue("picked_movie", title, { priority: "event" });
}

/*  CLEAR  */
function cmClear() {
  var inp = document.getElementById("cm-inp");
  var clr = document.getElementById("cm-clr");
  var drop = document.getElementById("cm-drop");
  if (inp) { inp.value = ""; inp.focus(); }
  if (clr) clr.style.display = "none";
  if (drop) drop.style.display = "none";
  /* reset both inputs so server clears search panel */
  Shiny.setInputValue("picked_movie", "",          { priority: "event" });
  Shiny.setInputValue("clear_search", Date.now(),  { priority: "event" });
}

/*  MODAL  */
function openMovie(title) {
  var m = document.getElementById("nf-modal");
  if (!m) { console.error("nf-modal not found"); return; }
  m.classList.add("open");
  document.body.style.overflow = "hidden";
  Shiny.setInputValue("modal_movie", title, { priority: "event" });
}

function closeModal() {
  var m = document.getElementById("nf-modal");
  if (m) m.classList.remove("open");
  document.body.style.overflow = "";
}

/*  RATING (cards + modal)  */
function cmRate(title, r) {
  Shiny.setInputValue("rated_movie", { title: title, rating: r }, { priority: "event" });

  /* update every star-row with matching data-movie */
  document.querySelectorAll(".star-row, .modal-star-row").forEach(function (row) {
    if (row.getAttribute("data-movie") === title) {
      row.querySelectorAll(".nf-star, .modal-star").forEach(function (s, i) {
        s.style.color = i < r ? "#e50914" : "rgba(255,255,255,0.25)";
      });
    }
  });
}

/*  HERO BG  */
Shiny.addCustomMessageHandler("setHeroBg", function (data) {
  var el = document.getElementById("hero-bg");
  if (el && data.url) el.style.backgroundImage = "url(" + data.url + ")";
});

/*  SCROLL NAVBAR  */
window.addEventListener("scroll", function () {
  var nav = document.getElementById("nf-nav");
  if (!nav) return;
  if (window.scrollY > 10) nav.classList.add("scrolled");
  else nav.classList.remove("scrolled");
});

/* GLOBAL CLICKS & KEYS */
document.addEventListener("click", function (e) {
  if (!e.target.closest("#nf-search-wrap"))
    document.getElementById("cm-drop").style.display = "none";
  if (e.target.id === "nf-modal") closeModal();
});

document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") closeModal();
  if (e.key === "Enter") {
    var f = document.querySelector(".drop-item[data-title]");
    if (f) f.click();
  }
  /* Arrow key navigation in dropdown */
  if (e.key === "ArrowDown" || e.key === "ArrowUp") {
    var items = Array.from(document.querySelectorAll(".drop-item[data-title]"));
    if (!items.length) return;
    var cur = document.querySelector(".drop-item.focused");
    var idx = cur ? items.indexOf(cur) : -1;
    if (cur) cur.classList.remove("focused");
    idx = e.key === "ArrowDown"
          ? Math.min(idx + 1, items.length - 1)
          : Math.max(idx - 1, 0);
    items[idx].classList.add("focused");
    items[idx].scrollIntoView({ block: "nearest" });
    e.preventDefault();
  }
});