// Client-side glue: page links, row action buttons, demo account buttons and the active menu item.
window.FMS = {
  // Help pages: hide the FAQ questions or glossary terms that do not match the search box.
  filterHelp(el) {
    const page = el.closest('.shiny-html-output') || document;
    const words = el.value.toLowerCase().trim().split(/\s+/).filter(Boolean);
    let shown = 0;
    page.querySelectorAll('.fms-help-item').forEach((it) => {
      const ok = words.every((w) => it.textContent.toLowerCase().includes(w));
      it.style.display = ok ? '' : 'none';
      if (ok) { shown++; if (words.length && it.tagName === 'DETAILS') it.open = true; }
    });
    page.querySelectorAll('.fms-help-group').forEach((g) => {
      g.style.display = g.querySelector('.fms-help-item:not([style*="none"])') ? '' : 'none';
    });
    const none = page.querySelector('.fms-help-none');
    if (none) none.style.display = shown ? 'none' : '';
  },
  setActive(page) {
    const key = page === 'fund' ? 'funds' : page;
    document.querySelectorAll('.fms-nav-item[data-page]').forEach((a) => a.classList.toggle('active', a.dataset.page === key));
    document.querySelectorAll('.fms-drop').forEach((d) => d.querySelector('.dropdown-toggle').classList.toggle('active', !!d.querySelector('.dropdown-item.active')));
  },
  closePopovers() {
    document.querySelectorAll('bslib-popover').forEach((p) => { try { if (p.visible) p.hide(); } catch (e) { /* ignore */ } });
  },
};

$(document).on('click', '[data-action]', function (e) {
  e.preventDefault();
  e.stopPropagation();
  Shiny.setInputValue('act', { action: this.dataset.action, id: this.dataset.id, n: Date.now() }, { priority: 'event' });
});

$(document).on('click', '[data-nav]', function (e) {
  e.preventDefault();
  FMS.closePopovers();
  Shiny.setInputValue('nav', { page: this.dataset.nav, fund: this.dataset.fund || null, tab: this.dataset.tab || null, n: Date.now() }, { priority: 'event' });
  window.scrollTo({ top: 0 });
  document.body.classList.remove('fms-menu-open');
});

// Profile photo: crop to a centred square, resize to 256 px in the browser and send it as a small data URL.
$(document).on('change', '#pf_avatar_file', function () {
  const file = this.files && this.files[0];
  this.value = '';
  if (!file) return;
  if (!/^image\/(png|jpeg|webp)$/.test(file.type)) { Shiny.setInputValue('pf_avatar_data', { data: '', n: Date.now() }, { priority: 'event' }); return; }
  const img = new Image();
  img.onload = () => {
    const size = 256, side = Math.min(img.width, img.height);
    const canvas = document.createElement('canvas');
    canvas.width = size; canvas.height = size;
    canvas.getContext('2d').drawImage(img, (img.width - side) / 2, (img.height - side) / 2, side, side, 0, 0, size, size);
    URL.revokeObjectURL(img.src);
    Shiny.setInputValue('pf_avatar_data', { data: canvas.toDataURL('image/jpeg', 0.86), n: Date.now() }, { priority: 'event' });
  };
  img.onerror = () => Shiny.setInputValue('pf_avatar_data', { data: '', n: Date.now() }, { priority: 'event' });
  img.src = URL.createObjectURL(file);
});

$(document).on('click', '.fms-demo', function (e) {
  e.preventDefault();
  $('#login_email').val(this.dataset.email).trigger('change');
  $('#login_password').val(this.dataset.password).trigger('change');
  setTimeout(() => $('#login_btn').trigger('click'), 50);
});

$(document).on('keydown', '#login_password, #login_email', (e) => { if (e.key === 'Enter') setTimeout(() => $('#login_btn').trigger('click'), 50); });

// Options: mode (dark or light) and language. The browser remembers both for the sign-in screen;
// once signed in, the server saves them on the user and sends them back at each sign-in.
const fmsStore = {
  get: (k) => { try { return localStorage.getItem(k); } catch (e) { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch (e) { /* private mode */ } },
};
FMS.applyTheme = (theme) => {
  if (theme !== 'dark' && theme !== 'light') return;
  document.documentElement.setAttribute('data-bs-theme', theme);
  fmsStore.set('fms-theme', theme);
  // Charts read their colours from CSS, so a resize event is enough to redraw them.
  window.dispatchEvent(new Event('resize'));
};

$(document).on('click', '[data-opt]', function (e) {
  e.preventDefault();
  const key = this.dataset.opt, value = this.dataset.value;
  if (key === 'theme') FMS.applyTheme(value);
  if (key === 'lang') { fmsStore.set('fms-lang', value); $(`[data-opt="lang"]`).removeClass('on').filter(`[data-value="${value}"]`).addClass('on'); }
  Shiny.setInputValue('opt', { key, value, n: Date.now() }, { priority: 'event' });
  Shiny.setInputValue('client_prefs', { lang: fmsStore.get('fms-lang') || 'en', theme: fmsStore.get('fms-theme') || 'dark' });
});

$(document).on('shiny:connected', () => {
  Shiny.addCustomMessageHandler('fms-active', (page) => { FMS.setActive(page); fmsStore.set('fms-page', page); });
  Shiny.addCustomMessageHandler('fms-reset', (_x) => { FMS.leaving = true; window.location.reload(); });
  // Stay signed in: keep the token the server gives at sign-in, send it back after a reload or reconnection.
  // Remember me: the token goes to localStorage (kept after the browser closes); otherwise to sessionStorage (this tab only).
  Shiny.addCustomMessageHandler('fms-token', (t) => {
    try {
      localStorage.removeItem('fms-token'); sessionStorage.removeItem('fms-token');
      if (t && t.token) (t.remember ? localStorage : sessionStorage).setItem('fms-token', t.token);
    } catch (e) { /* storage blocked */ }
  });
  Shiny.addCustomMessageHandler('fms-resumed', (ok) => {
    document.documentElement.classList.remove('fms-resuming');
    // Signed back in: reopen the page the user was on (a fund page needs its fund, so it falls back to the fund list).
    const p = fmsStore.get('fms-page');
    if (ok && p && p !== 'dashboard') setTimeout(() => Shiny.setInputValue('nav', { page: p === 'fund' ? 'funds' : p, n: Date.now() }, { priority: 'event' }), 50);
  });
  let token = fmsStore.get('fms-token');
  try { token = token || sessionStorage.getItem('fms-token'); } catch (e) { /* storage blocked */ }
  if (token) Shiny.setInputValue('resume', { token, n: Date.now() }, { priority: 'event' });
  setTimeout(() => document.documentElement.classList.remove('fms-resuming'), 6000);
  Shiny.addCustomMessageHandler('fms-prefs', (p) => {
    if (p.theme) FMS.applyTheme(p.theme);
    if (p.lang) { fmsStore.set('fms-lang', p.lang); document.documentElement.lang = p.lang; }
    const l = p.labels || {};
    $('.fms-burger').attr('title', l.menu); $('.fms-hnav').attr('aria-label', l.main);
    $('.fms-top-icons .fa-bell').closest('button').attr('title', l.notifications);
    $('.fms-top-icons .fa-sliders').closest('button').attr('title', l.options);
    $('.fms-avatar-btn').attr('title', l.account);
    const sel = $('#search')[0] && $('#search')[0].selectize;
    if (sel && l.search) { sel.settings.placeholder = l.search; sel.updatePlaceholder(); }
  });
  Shiny.setInputValue('client_prefs', { lang: fmsStore.get('fms-lang') || 'en', theme: fmsStore.get('fms-theme') || 'dark' });
});

// Native date fields (input type="date"), so no date-picker library is needed.
const fmsDate = new Shiny.InputBinding();
$.extend(fmsDate, {
  find: (scope) => $(scope).find('input.fms-date'),
  getValue: (el) => el.value || null,
  setValue: (el, v) => { el.value = v || ''; },
  subscribe: (el, cb) => $(el).on('change.fmsDate', () => cb()),
  unsubscribe: (el) => $(el).off('.fmsDate'),
  receiveMessage: (el, data) => { if ('value' in data) el.value = data.value || ''; $(el).trigger('change'); },
});
Shiny.inputBindings.register(fmsDate, 'fms.date');

// Sign-in background: twinkling stars with a slow drift and the odd shooting star.
// It pauses while the sign-in screen is hidden, and draws a still sky when the user prefers reduced motion.
(function () {
  const still = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  let canvas, ctx, stars = [], shot = null, w = 0, h = 0, dpr = 1, last = 0;
  const resize = () => {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    w = canvas.clientWidth; h = canvas.clientHeight;
    canvas.width = w * dpr; canvas.height = h * dpr; ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    const n = Math.round(Math.min(320, (w * h) / 5200));
    stars = Array.from({ length: n }, () => ({ x: Math.random() * w, y: Math.random() * h, r: Math.random() * 1.3 + 0.25,
      a: Math.random() * 0.6 + 0.3, s: Math.random() * 2 + 0.6, p: Math.random() * Math.PI * 2, v: Math.random() * 0.06 + 0.01,
      c: Math.random() < 0.14 ? '255,226,160' : (Math.random() < 0.5 ? '190,215,255' : '255,255,255') }));
  };
  const draw = (t) => {
    ctx.clearRect(0, 0, w, h);
    for (const s of stars) {
      const tw = still ? s.a : s.a * (0.55 + 0.45 * Math.sin(t / 1000 * s.s + s.p));
      if (!still) { s.x -= s.v; s.y += s.v * 0.35; if (s.x < -2) s.x = w + 2; if (s.y > h + 2) s.y = -2; }
      ctx.beginPath(); ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2); ctx.fillStyle = `rgba(${s.c},${tw})`; ctx.fill();
      if (s.r > 1.25) { ctx.fillStyle = `rgba(${s.c},${tw * 0.18})`; ctx.beginPath(); ctx.arc(s.x, s.y, s.r * 3.2, 0, Math.PI * 2); ctx.fill(); }
    }
    if (still) return;
    if (!shot && Math.random() < 0.004) shot = { x: Math.random() * w * 0.8 + w * 0.2, y: Math.random() * h * 0.4, l: 0 };
    if (shot) {
      shot.l += 1;
      const x = shot.x - shot.l * 9, y = shot.y + shot.l * 4.5, a = Math.max(0, 1 - shot.l / 45);
      const g = ctx.createLinearGradient(x, y, x + 90, y - 45);
      g.addColorStop(0, `rgba(255,236,190,${a})`); g.addColorStop(1, 'rgba(255,236,190,0)');
      ctx.strokeStyle = g; ctx.lineWidth = 1.6; ctx.beginPath(); ctx.moveTo(x, y); ctx.lineTo(x + 90, y - 45); ctx.stroke();
      if (shot.l > 45) shot = null;
    }
  };
  const loop = (t) => {
    const visible = canvas.closest('.fms-auth').style.display !== 'none';
    if (visible && !w) resize();
    if (visible && t - last > 30) { last = t; draw(t); }
    requestAnimationFrame(loop);
  };
  document.addEventListener('DOMContentLoaded', () => {
    canvas = document.getElementById('fms-stars');
    if (!canvas) return;
    ctx = canvas.getContext('2d');
    resize(); window.addEventListener('resize', resize);
    if (still) draw(0); else requestAnimationFrame(loop);
  });
})();

// Hide the app's top bar while the sign-in screen is up (fallback for browsers without :has).
(function () {
  function sync() { var a = document.querySelector('.fms-auth'); document.body.classList.toggle('fms-signed-out', !!a && getComputedStyle(a).display !== 'none'); }
  $(document).on('shiny:value shiny:conditional shiny:connected', function () { setTimeout(sync, 0); });
  $(function () { sync(); var a = document.querySelector('.fms-auth'); if (a) new MutationObserver(sync).observe(a, { attributes: true, attributeFilter: ['style', 'class'] }); });
})();

// Local preview (RStudio "Run in Window"): open the window at the full size of the screen.
// Only on this computer (127.0.0.1 / localhost), only for a top-level window; browsers ignore it for normal tabs.
(function () {
  if (!/^(127\.0\.0\.1|localhost)$/.test(location.hostname) || window.top !== window) return;
  if (window.outerWidth >= screen.availWidth - 16 && window.outerHeight >= screen.availHeight - 16) return;
  try { window.moveTo(screen.availLeft || 0, screen.availTop || 0); window.resizeTo(screen.availWidth, screen.availHeight); } catch (e) { /* not allowed here */ }
})();

// Lost connection to the server (idle timeout, restart, network): reload the page as soon as the server answers again.
// The saved sign-in token then signs the user back in on the page they were on.
$(document).on('shiny:disconnected', () => {
  if (FMS.leaving) return;
  let tries = 0;
  const retry = () => {
    tries++;
    fetch(window.location.pathname, { cache: 'no-store' })
      .then((r) => { if (r.ok) window.location.reload(); else throw new Error(r.status); })
      .catch(() => { if (tries < 60) setTimeout(retry, Math.min(2000 * tries, 15000)); });
  };
  setTimeout(retry, 1500);
});

// Password fields: an eye button to show or hide what is typed.
(function () {
  const EYE = '<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M1 12s4-7 11-7 11 7 11 7-4 7-11 7S1 12 1 12z"/><circle cx="12" cy="12" r="3"/></svg>';
  const EYE_OFF = '<svg viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17.94 17.94A10.07 10.07 0 0 1 12 19c-7 0-11-7-11-7a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 7 11 7a18.5 18.5 0 0 1-2.16 3.19M14.12 14.12a3 3 0 1 1-4.24-4.24"/><line x1="1" y1="1" x2="23" y2="23"/></svg>';
  const LABELS = { en: ['Show password', 'Hide password'], vi: ['Hiện mật khẩu', 'Ẩn mật khẩu'], fr: ['Afficher le mot de passe', 'Masquer le mot de passe'], zh: ['显示密码', '隐藏密码'] };
  const label = (shown) => (LABELS[document.documentElement.lang] || LABELS.en)[shown ? 1 : 0];
  function addEyes() {
    document.querySelectorAll('input[type="password"]:not([data-eye])').forEach((inp) => {
      inp.dataset.eye = '1';
      const wrap = document.createElement('div'); wrap.className = 'fms-pw';
      inp.parentNode.insertBefore(wrap, inp); wrap.appendChild(inp);
      const btn = document.createElement('button'); btn.type = 'button'; btn.className = 'fms-eye'; btn.tabIndex = -1;
      btn.innerHTML = EYE; btn.title = label(false); btn.setAttribute('aria-label', label(false));
      btn.addEventListener('click', () => {
        const show = inp.type === 'password'; inp.type = show ? 'text' : 'password';
        btn.innerHTML = show ? EYE_OFF : EYE; btn.title = label(show); btn.setAttribute('aria-label', label(show)); inp.focus();
      });
      wrap.appendChild(btn);
    });
  }
  $(document).on('shiny:value shiny:connected', () => setTimeout(addEyes, 0));
  document.addEventListener('DOMContentLoaded', addEyes);
})();

// Markets: the instrument tables sit above the chart, so clicking one scrolls down to its chart.
$(document).on('click', '[data-action="market"]', () => {
  const c = document.getElementById('mk_chart_card');
  if (c) setTimeout(() => c.scrollIntoView({ behavior: 'smooth', block: 'center' }), 150);
});

// Loading popup: shown from start-up until the server is connected and has drawn the first screen (at most 25 s),
// then again whenever the server stays busy for more than 0.4 s, e.g. while a large table loads.
(function () {
  let connected = false, started = false, timer = null;
  const el = () => document.getElementById('fms-loader');
  const text = (t) => { const x = el() && el().querySelector('.fms-loader-text'); if (x) x.textContent = t; };
  const hide = () => { clearTimeout(timer); timer = null; if (el()) el().classList.add('done'); };
  const firstDone = () => { if (started) return; started = true; setTimeout(() => { hide(); el() && el().classList.add('fms-loader-busy'); }, 600); };
  $(document).on('shiny:connected', () => { connected = true; });
  $(document).on('shiny:busy', () => {
    if (!started || timer) return;
    timer = setTimeout(() => { text('Loading data...'); el() && el().classList.remove('done'); }, 400);
  });
  $(document).on('shiny:idle', () => { if (!started) { if (connected) firstDone(); } else hide(); });
  setTimeout(firstDone, 25000);
})();

// Markets tables: click a column heading to sort (again to reverse); the filter box hides rows that do not match.
// The sort and filter are re-applied whenever the server redraws the tables.
(function () {
  let sort = null;   // { col, dir }
  const num = (t) => { const v = parseFloat(t.replace(/[,%+\s]|bp/g, '').replace('\u2212', '-')); return isNaN(v) ? null : v; };
  const apply = () => {
    const box = document.getElementById('mk_filter_box'), q = (box ? box.value : '').trim().toLowerCase();
    document.querySelectorAll('#mk_tables table.fms-sortable').forEach((t) => {
      const ths = t.querySelectorAll('thead th');
      ths.forEach((th, i) => { th.classList.toggle('sort-asc', !!sort && sort.col === i && sort.dir > 0); th.classList.toggle('sort-desc', !!sort && sort.col === i && sort.dir < 0); });
      const body = t.tBodies[0], rows = Array.from(body.rows);
      if (sort) {
        rows.sort((a, b) => {
          const x = a.cells[sort.col].textContent.trim(), y = b.cells[sort.col].textContent.trim(), nx = num(x), ny = num(y);
          return sort.dir * (nx !== null && ny !== null ? nx - ny : x.localeCompare(y));
        }).forEach((r) => body.appendChild(r));
      }
      let shown = 0;
      rows.forEach((r) => { const ok = !q || r.textContent.toLowerCase().includes(q); r.style.display = ok ? '' : 'none'; if (ok) shown++; });
      const wrap = t.closest('.fms-table-wrap'), head = wrap && wrap.previousElementSibling, src = wrap && wrap.nextElementSibling;
      [wrap, head && head.classList.contains('fms-section-head') ? head : null, src && src.classList.contains('fms-src') ? src : null]
        .forEach((el) => { if (el) el.style.display = shown ? '' : 'none'; });
    });
  };
  window.FMS = window.FMS || {}; window.FMS.mkApply = apply;
  $(document).on('click', '#mk_tables table.fms-sortable thead th', function () {
    const i = Array.from(this.parentNode.children).indexOf(this);
    if (/trend/i.test(this.textContent)) return;   // the sparkline column has nothing to sort on
    sort = sort && sort.col === i ? { col: i, dir: -sort.dir } : { col: i, dir: this.classList.contains('num') ? -1 : 1 };
    apply();
  });
  $(document).on('shiny:value', (e) => { if (e.name === 'mk_tables') setTimeout(apply, 0); });
})();
