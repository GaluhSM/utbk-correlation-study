/* Antarmuka kalkulator. Semua teks dibuat lewat DOM (textContent), tanpa
 * innerHTML, tanpa permintaan jaringan, tanpa penyimpanan apa pun. */
(function () {
  'use strict';
  var D = window.UTBK_DATA, C = window.UTBKCalc;
  var N = D.meta.n_populasi, MU = D.meta.mu;
  var SVGNS = 'http://www.w3.org/2000/svg';

  var nf0 = new Intl.NumberFormat('id-ID', { maximumFractionDigits: 0 });
  function nf(d) { return new Intl.NumberFormat('id-ID', { minimumFractionDigits: d, maximumFractionDigits: d }); }
  var nf1 = nf(1), nf2 = nf(2), nf3 = nf(3);

  function h(tag, attrs) {
    var e = document.createElement(tag), i, kids = Array.prototype.slice.call(arguments, 2);
    for (var k in (attrs || {})) e.setAttribute(k, attrs[k]);
    for (i = 0; i < kids.length; i++) tambah(e, kids[i]);
    return e;
  }
  function tambah(e, kid) {
    if (kid == null || kid === false) return;
    if (Array.isArray(kid)) { kid.forEach(function (x) { tambah(e, x); }); return; }
    e.appendChild(typeof kid === 'object' ? kid : document.createTextNode(String(kid)));
  }
  function svg(tag, attrs) {
    var e = document.createElementNS(SVGNS, tag);
    for (var k in (attrs || {})) e.setAttribute(k, attrs[k]);
    return e;
  }

  // ---- format ---------------------------------------------------------------
  function fmtRank(p) {
    var b = C.bulatPeringkat(p, N);
    return (b.kurangDari ? 'kurang dari ' : '') + nf0.format(b.nilai);
  }
  function fmtAtas(x) {                 // persen teratas
    if (x < 0.01) return '< 0,01';
    if (x < 1) return new Intl.NumberFormat('id-ID', { maximumSignificantDigits: 2 }).format(x);
    return nf1.format(x);
  }
  function fmtPersentil(atas) {
    var p = 100 - atas;
    return (atas < 1 ? nf2 : nf1).format(p);
  }

  // ---- formulir -------------------------------------------------------------
  var bidang = document.getElementById('bidang');
  D.subtes.forEach(function (st) {
    bidang.appendChild(h('div', { 'class': 'baris' },
      h('label', { 'for': 'in-' + st.kode }, st.label + ' (' + st.singkat + ')'),
      h('input', { id: 'in-' + st.kode, type: 'text', inputmode: 'decimal', autocomplete: 'off', spellcheck: 'false' })
    ));
  });

  var form = document.getElementById('form');
  var galat = document.getElementById('galat');
  var hasilEl = document.getElementById('hasil');
  var terakhir = null;

  form.addEventListener('submit', function (ev) {
    ev.preventDefault();
    var skor = {}, salah = [], kosong = 0;
    D.subtes.forEach(function (st) {
      var el = document.getElementById('in-' + st.kode);
      var v = C.parseSkor(el.value);
      var ok = !isNaN(v) && v > 0 && v <= 1500;
      if (el.value.trim() === '') kosong += 1;
      el.setAttribute('aria-invalid', ok ? 'false' : 'true');
      if (ok) skor[st.kode] = v; else salah.push(st.singkat);
    });
    if (salah.length) {
      galat.textContent = kosong === salah.length
        ? 'Isi kedelapan skor terlebih dahulu (angka, boleh pakai koma desimal). Belum valid: ' + salah.join(', ') + '.'
        : 'Periksa skor ini (harus angka antara 0 dan 1.500): ' + salah.join(', ') + '.';
      galat.hidden = false;
      hasilEl.hidden = true;
      return;
    }
    galat.hidden = true;
    tampilkan(skor);
  });

  document.getElementById('hapus').addEventListener('click', function () {
    D.subtes.forEach(function (st) {
      var el = document.getElementById('in-' + st.kode);
      el.value = ''; el.setAttribute('aria-invalid', 'false');
    });
    galat.hidden = true; hasilEl.hidden = true; hasilEl.textContent = ''; terakhir = null;
    document.getElementById('in-' + D.subtes[0].kode).focus();
  });

  // ---- perhitungan + tampilan ------------------------------------------------
  function tampilkan(skor) {
    var vektor = D.subtes.map(function (st) { return skor[st.kode]; });
    var komposit = C.rerata(vektor);

    var perSubtes = D.subtes.map(function (st) {
      var r = C.dariZ(C.zScore(skor[st.kode], st.sd, MU), N);
      return { st: st, skor: skor[st.kode], r: r, pos: C.posisiKuartil(skor[st.kode], st) };
    });
    var luar = perSubtes.filter(function (x) { return x.pos.kode === 'luar'; });

    var skenario = D.skenario.map(function (s) {
      return { s: s, r: C.dariZ(C.zScore(komposit, s.sd, MU), N) };
    });
    var utama = skenario.filter(function (x) { return x.s.kode === 'thorndike'; })[0];
    var ranks = skenario.map(function (x) { return x.r.peringkat; });
    var pMin = Math.min.apply(null, ranks), pMax = Math.max.apply(null, ranks);
    var atasMin = Math.min.apply(null, skenario.map(function (x) { return x.r.atasPct; }));
    var atasMax = Math.max.apply(null, skenario.map(function (x) { return x.r.atasPct; }));

    var rentangTeks = fmtRank(pMin) === fmtRank(pMax) ? fmtRank(pMin) : fmtRank(pMin) + ' – ' + fmtRank(pMax);
    terakhir = {
      komposit: komposit, tengah: fmtRank(utama.r.peringkat), rentang: rentangTeks,
      atas: fmtAtas(utama.r.atasPct)
    };

    hasilEl.textContent = '';
    hasilEl.appendChild(h('h2', null, 'Hasil'));

    if (luar.length) {
      hasilEl.appendChild(h('p', { 'class': 'catatan' },
        'Perhatian: skor ' + luar.map(function (x) { return x.st.singkat; }).join(', ') +
        ' berada di luar rentang minimum–maksimum nasional tidak resmi. Periksa kembali inputmu.'));
    }

    hasilEl.appendChild(h('dl', null,
      h('dt', null, 'Skor komposit'),
      h('dd', null, nf2.format(komposit), ' ', h('small', null, '(rerata sederhana 8 subtes)')),
      h('dt', null, 'Perkiraan peringkat nasional'),
      h('dd', null, 'sekitar ', fmtRank(utama.r.peringkat), ' ', h('small', null, 'dari ' + nf0.format(N) + ' peserta hadir')),
      h('dt', null, 'Kisaran antarskenario korelasi'),
      h('dd', null, rentangTeks),
      h('dt', null, 'Persentil'),
      h('dd', null, fmtPersentil(utama.r.atasPct), ' ', h('small', null, '(teratas ' + fmtAtas(utama.r.atasPct) + '%; kisaran teratas ' + fmtAtas(atasMin) + '–' + fmtAtas(atasMax) + '%)'))
    ));

    hasilEl.appendChild(batangRentang(pMin, pMax, utama.r.peringkat));
    hasilEl.appendChild(h('p', { 'class': 'catatan' },
      'Perkiraan kasar · parameter nasional tidak resmi · bukan peringkat resmi · bukan prediksi lolos. ' +
      'Kisaran mencakup tujuh skenario korelasi antarsubtes (r = 0,300–0,550) dari paper; titik tengah memakai estimasi Thorndike Case III (r ≈ ' +
      nf3.format(utama.s.r) + '). Ketidakpastian terbesar justru berasal dari SD nasional yang tidak resmi (lihat rincian).'));

    if (utama.r.z > 3) {
      hasilEl.appendChild(h('p', { 'class': 'catatan' },
        'Skor komposit ini lebih dari 3 SD di atas rerata: bagian ekor atas paling tidak pasti, dan peringkat sebenarnya kemungkinan lebih besar daripada hitungan normal (lihat "Bentuk distribusi").'));
    }

    hasilEl.appendChild(rincianSubtes(perSubtes));
    hasilEl.appendChild(rincianSkenario(komposit, skenario));
    hasilEl.appendChild(rincianEkor(utama));
    hasilEl.appendChild(rincianGrafik(komposit, utama));
    hasilEl.appendChild(penjelasan());

    hasilEl.appendChild(h('p', { 'class': 'aksi' },
      h('button', { type: 'button', id: 'salin' }, 'Salin hasil'),
      h('button', { type: 'button', id: 'cetak' }, 'Cetak'),
      h('span', { id: 'status', 'class': 'kecil' })
    ));
    document.getElementById('salin').addEventListener('click', salin);
    document.getElementById('cetak').addEventListener('click', function () { window.print(); });

    hasilEl.hidden = false;
    hasilEl.scrollIntoView();
  }

  function salin() {
    var t = terakhir, s = document.getElementById('status');
    var teks = 'Perkiraan kasar UTBK-SNBT 2026: skor komposit ' + nf2.format(t.komposit) +
      '; peringkat nasional sekitar ' + t.tengah + ' (kisaran ' + t.rentang + ') dari ' + nf0.format(N) +
      ' peserta hadir; teratas ' + t.atas + '%. Parameter nasional tidak resmi; bukan peringkat resmi dan bukan prediksi lolos.';
    if (!navigator.clipboard) { s.textContent = 'Peramban ini tidak mengizinkan penyalinan otomatis.'; return; }
    navigator.clipboard.writeText(teks).then(
      function () { s.textContent = 'Tersalin.'; },
      function () { s.textContent = 'Penyalinan ditolak peramban.'; }
    );
  }

  window.addEventListener('beforeprint', function () {
    Array.prototype.forEach.call(document.querySelectorAll('details'), function (d) {
      d.setAttribute('data-buka', d.open ? '1' : '0'); d.open = true;
    });
  });
  window.addEventListener('afterprint', function () {
    Array.prototype.forEach.call(document.querySelectorAll('details'), function (d) {
      d.open = d.getAttribute('data-buka') === '1';
    });
  });

  // ---- komponen --------------------------------------------------------------
  // Batang kisaran pada skala log peringkat (1 ... N); titik = estimasi utama.
  function batangRentang(pMin, pMax, pTengah) {
    function pos(p) { return Math.max(0, Math.min(100, Math.log10(Math.max(p, 1)) / Math.log10(N) * 100)); }
    var isi = h('span', { 'class': 'isi' }), titik = h('span', { 'class': 'titik' });
    isi.style.left = pos(pMin) + '%';
    isi.style.width = Math.max(pos(pMax) - pos(pMin), 0.4) + '%';
    titik.style.left = pos(pTengah) + '%';
    var skala = h('div', { 'class': 'skala' });
    [[1, '1'], [10, '10'], [100, '100'], [1e3, '1 rb'], [1e4, '10 rb'], [1e5, '100 rb'], [1e6, '1 jt']].forEach(function (t) {
      if (t[0] > N) return;
      var s = h('span', null, t[1]); s.style.left = pos(t[0]) + '%'; skala.appendChild(s);
    });
    return h('div', { 'class': 'rentang', role: 'img',
      'aria-label': 'Kisaran peringkat ' + fmtRank(pMin) + ' sampai ' + fmtRank(pMax) + ', titik tengah ' + fmtRank(pTengah) },
      h('div', { 'class': 'jalur' }, isi, titik), skala);
  }

  function tabel(kepala, baris) {
    var thead = h('tr', null, kepala.map(function (k) { return h('th', null, k); }));
    return h('div', { 'class': 'gulir' }, h('table', null, h('thead', null, thead), h('tbody', null, baris)));
  }

  function teksKuartil(p) {
    var q = { q4: [0, 0.25], q3: [0.25, 0.5], q2: [0.5, 0.75], q1: [0.75, 1] }[p.kode];
    if (p.kode === 'luar') return 'di luar rentang nasional';
    var a = Math.round(N * q[0]), b = Math.round(N * q[1]);
    var hi = nf0.format(C.bulatPeringkat(b, N).nilai), lo = nf0.format(C.bulatPeringkat(a, N).nilai);
    var nama = { q4: 'di atas Q3', q3: 'antara median dan Q3', q2: 'antara Q1 dan median', q1: 'di bawah Q1' }[p.kode];
    var rentang = p.kode === 'q4' ? 'sampai ±' + hi : p.kode === 'q1' ? 'lebih dari ±' + lo : '±' + lo + '–' + hi;
    return nama + ' (peringkat ' + rentang + ')';
  }

  function rincianSubtes(rows) {
    var baris = rows.map(function (x) {
      return h('tr', null,
        h('td', null, h('abbr', { title: x.st.label }, x.st.singkat)),
        h('td', null, nf2.format(x.skor)),
        h('td', null, nf2.format(x.r.z)),
        h('td', null, fmtAtas(x.r.atasPct)),
        h('td', null, fmtRank(x.r.peringkat)),
        h('td', { 'class': 'kiri' }, teksKuartil(x.pos)));
    });
    return h('details', null,
      h('summary', null, 'Rincian per subtes'),
      h('p', { 'class': 'catatan' }, 'Model normal dengan SD nasional tidak resmi tiap subtes (PU = 71,52; lainnya 100). Distribusi aslinya miring (rerata ≠ median), terutama PK, LBE, dan PM, sehingga kolom kuartil nasional (empiris) menjadi pembanding yang tidak bergantung pada asumsi normal.'),
      tabel(['Subtes', 'Skor', 'z', 'Teratas (%)', 'Peringkat', 'Posisi (kuartil nasional)'], baris));
  }

  function barisSkenario(label, rTeks, sd, komposit, utama) {
    var r = C.dariZ(C.zScore(komposit, sd, MU), N);
    return h('tr', utama ? { 'class': 'utama' } : null,
      h('td', { 'class': 'kiri' }, label), h('td', null, rTeks), h('td', null, nf2.format(sd)),
      h('td', null, nf2.format(r.z)), h('td', null, fmtAtas(r.atasPct)), h('td', null, fmtRank(r.peringkat)));
  }

  function rincianSkenario(komposit, skenario) {
    var baris = [h('tr', { 'class': 'kelompok' }, h('th', { colspan: 6 }, 'Skenario korelasi antarsubtes (Tabel 9 paper)'))];
    skenario.forEach(function (x) {
      baris.push(barisSkenario(x.s.nama, nf3.format(x.s.r), x.s.sd, komposit, x.s.kode === 'thorndike'));
    });
    baris.push(h('tr', { 'class': 'kelompok' }, h('th', { colspan: 6 }, 'Ketidakpastian sampling (bootstrap 95% untuk SD komposit)')));
    baris.push(barisSkenario('Batas bawah S_Z', '—', D.bootstrap.s_z.lo, komposit, false));
    baris.push(barisSkenario('Batas atas S_Z', '—', D.bootstrap.s_z.hi, komposit, false));
    baris.push(h('tr', { 'class': 'kelompok' }, h('th', { colspan: 6 }, 'Sensitivitas terhadap SD nasional tidak resmi (ilustratif)')));
    D.sens_sd.slice(1).forEach(function (s) {
      baris.push(barisSkenario(s.label.replace(/ x /g, ' × '), nf3.format(s.r), s.s_z, komposit, false));
    });
    return h('details', null,
      h('summary', null, 'Rincian skenario komposit dan ketidakpastian'),
      h('p', { 'class': 'catatan' }, 'Korelasi mentah sampel (0,300) terpotong rentang karena sampel berskor tinggi; estimasi Thorndike Case III (≈ 0,425) mengoreksinya. Nilai 0,350, 0,400, 0,450, 0,500, dan 0,550 bersifat hipotetis (analisis sensitivitas), bukan klaim rentang literatur. Perubahan SD nasional sebesar ±10% menggeser korelasi terkoreksi dari sekitar 0,300 hingga 0,519, jauh melebihi ketidakpastian sampling.'),
      tabel(['Skenario', 'r rerata', 'SD komposit', 'z', 'Teratas (%)', 'Peringkat'], baris));
  }

  function namaVarian(b) {
    if (b === 0) return 'Distribusi miring kanan (simulasi paper)';
    return 'Miring kanan + korelasi lebih kecil di skor tinggi, b = ' + String(b).replace('.', ',') + ' (ilustratif)';
  }

  function rincianEkor(utama) {
    var z = utama.r.z, isi = [];
    if (z < 1) {
      isi.push(h('p', null, 'Koreksi bentuk distribusi hanya dihitung untuk skor komposit di atas +1 SD (di titik itu selisih simulasi paper hanya sekitar 1–3%). Untuk skormu, hitungan normal dipakai apa adanya.'));
    } else {
      var bs = [0, 0.02, 0.04], baris = [];
      bs.forEach(function (b) {
        var f = C.faktorEkor(z, D.ekor.filter(function (e) { return e.b === b; }));
        baris.push(h('tr', null,
          h('td', { 'class': 'kiri' }, namaVarian(b)),
          h('td', null, (f.diLuar ? '≥ ' : '') + nf2.format(f.rasio) + '×'),
          h('td', null, (f.diLuar ? '≥ ' : '') + fmtRank(utama.r.peringkat * f.rasio))));
      });
      isi.push(h('p', null, 'Pada skormu (z = ' + nf2.format(z) + ' terhadap SD komposit Thorndike), simulasi paper menunjukkan ekor atas distribusi komposit lebih tebal daripada kurva normal: lebih banyak peserta berada di atas skor yang sama, sehingga peringkat sebenarnya cenderung lebih besar (kurang unggul) daripada hitungan normal. Angka di bawah memakai interpolasi di antara titik simulasi +1, +2, dan +3 SD; sifatnya indikatif.'));
      isi.push(tabel(['Varian', 'Pengali peringkat', 'Perkiraan peringkat'], baris));
      if (z > 3) isi.push(h('p', { 'class': 'catatan' }, 'Skormu melampaui +3 SD, di luar jangkauan simulasi paper; pengali yang dipakai adalah nilai pada +3 SD, sehingga angkanya batas bawah.'));
    }
    return h('details', null, h('summary', null, 'Bentuk distribusi (ekor atas)'), isi);
  }

  function rincianGrafik(komposit, utama) {
    var S = utama.s.sd, W = 600, H = 160, x0 = 20, x1 = 580, yb = 140, yt = 20;
    var lo = MU - 4 * S, hi = MU + 4 * S;
    function X(s) { return x0 + (Math.max(lo, Math.min(hi, s)) - lo) / (hi - lo) * (x1 - x0); }
    function Y(z) { return yb - Math.exp(-0.5 * z * z) * (yb - yt); }
    var g = svg('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'img',
      'aria-label': 'Kurva normal skor komposit dengan penanda posisi skormu' });
    var kurva = '', area = 'M' + X(Math.max(komposit, lo)) + ',' + yb, i, z, s;
    for (i = 0; i <= 160; i++) {
      z = -4 + 8 * i / 160; s = MU + z * S;
      kurva += (i ? 'L' : 'M') + X(s).toFixed(1) + ',' + Y(z).toFixed(1);
      if (s >= komposit) area += 'L' + X(s).toFixed(1) + ',' + Y(z).toFixed(1);
    }
    area += 'L' + x1 + ',' + yb + 'Z';
    g.appendChild(svg('path', { 'class': 'area', d: area }));
    g.appendChild(svg('path', { 'class': 'kurva', d: kurva }));
    g.appendChild(svg('line', { 'class': 'sumbu', x1: x0, x2: x1, y1: yb, y2: yb }));
    for (z = -3; z <= 3; z++) {
      var t = svg('text', { x: X(MU + z * S), y: yb + 16, 'text-anchor': 'middle', 'class': 'pudar' });
      t.textContent = nf0.format(Math.round(MU + z * S)); g.appendChild(t);
    }
    var px = X(komposit);
    g.appendChild(svg('line', { 'class': 'penanda', x1: px, x2: px, y1: yt - 8, y2: yb }));
    var lbl = svg('text', { x: px, y: 12, 'text-anchor': px > W - 80 ? 'end' : (px < 80 ? 'start' : 'middle') });
    lbl.textContent = 'skormu ' + nf1.format(komposit); g.appendChild(lbl);
    return h('details', null, h('summary', null, 'Grafik posisi'), g,
      h('p', { 'class': 'catatan' }, 'Sumbu: skor komposit. Area terarsir = peserta berskor lebih tinggi daripadamu (model normal dengan SD komposit ' + nf2.format(S) + ').'));
  }

  function penjelasan() {
    var m = D.meta;
    var sumber = ['Versi ' + m.versi + ' (dibekukan; angka sama dengan paper). Jumlah peserta hadir (' + nf0.format(N) + ') bersumber dari ' + m.sumber_n + '. '];
    if (m.judul_paper) sumber.push('Paper: ' + m.judul_paper + '. ');
    if (m.doi) sumber.push(h('a', { href: 'https://doi.org/' + m.doi, rel: 'noopener noreferrer' }, 'DOI ' + m.doi), ' ');
    if (m.repo) sumber.push(h('a', { href: m.repo, rel: 'noopener noreferrer' }, 'Kode dan data'), '.');
    return h('details', null,
      h('summary', null, 'Cara membaca hasil dan keterbatasan'),
      h('ul', null,
        h('li', null, 'Kisaran peringkat memperlihatkan seberapa jauh jawaban berubah bila korelasi antarsubtes dianggap berbeda. Titik tengah adalah estimasi terbaik paper, bukan nilai pasti.'),
        h('li', null, 'Makin tinggi skor komposit, makin tidak pasti peringkatnya. Di ekor atas, selisih kecil pada asumsi korelasi atau bentuk distribusi mengubah peringkat berlipat-lipat; karena itu angka dibulatkan.'),
        h('li', null, 'Skor komposit di sini adalah rerata sederhana delapan subtes (kedua skor LBI sama-sama dihitung). PTN dan program studi memakai pembobotan sendiri, sehingga posisimu menurut komposit PTN bisa berbeda.'),
        h('li', null, 'Parameter nasional per subtes (rerata, SD, minimum, maksimum, kuartil) berasal dari tabel internal yang tidak resmi dan tidak dapat diverifikasi; paper menunjukkan hasil sangat sensitif terhadapnya.'),
        h('li', null, 'Model dasar mengasumsikan distribusi normal. Distribusi nyata miring ke kanan, sehingga perkiraan di ekor atas cenderung terlalu optimistis. Efek ini dibahas pada bagian "Bentuk distribusi".'),
        h('li', null, 'Alat ini tidak membandingkan skormu dengan daya tampung atau peluang diterima, dan tidak memprediksi kelulusan.'),
        h('li', null, 'Gratis, tanpa iklan, tanpa analitik, dan tidak berafiliasi dengan SNPMB. Skor yang kamu ketik tidak dikirim ke mana pun dan tidak disimpan.')),
      h('p', { 'class': 'catatan' }, sumber));
  }

  // ---- kaki halaman -------------------------------------------------------------
  document.getElementById('kaki').textContent =
    'Versi ' + D.meta.versi + ' · perkiraan kasar, bukan peringkat resmi · tidak berafiliasi dengan SNPMB';
})();