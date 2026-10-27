/* Modul perhitungan murni (tanpa DOM): dipakai oleh app.js di peramban
 * dan oleh tests/web/verify_web.mjs di Node. Rumus identik dengan
 * scripts/06_ranking_simulation.R:
 *   z = (skor - 500) / SD ;  teratas% = 100 * (1 - Phi(z)) ;  peringkat = N * teratas% / 100
 */
(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.UTBKCalc = factory();
})(typeof self !== 'undefined' ? self : this, function () {
  'use strict';

  var SQRT2 = Math.SQRT2;
  var INV_SQRT_2PI = 1 / Math.sqrt(2 * Math.PI);

  // P(Z > z) untuk Z ~ N(0,1), akurat secara relatif juga di ekor.
  function qnorm(z) {
    if (z < 0) return 1 - qnorm(-z);
    if (z < 3) {
      // deret erf: erf(x) = 2/sqrt(pi) * sum (-1)^n x^(2n+1) / (n! (2n+1))
      var x = z / SQRT2, term = x, sum = x, n = 0;
      while (Math.abs(term) > 1e-18 && n < 200) {
        n += 1;
        term *= -x * x / n;
        sum += term / (2 * n + 1);
      }
      return 0.5 * (1 - 2 / Math.sqrt(Math.PI) * sum);
    }
    // pecahan berlanjut: Q(z) = phi(z) / (z + 1/(z + 2/(z + 3/(z + ...))))
    var f = z;
    for (var k = 300; k >= 1; k--) f = z + k / f;
    return INV_SQRT_2PI * Math.exp(-0.5 * z * z) / f;
  }

  // Terima "670,20", "670.20", " 670 "; kembalikan NaN bila tidak valid.
  function parseSkor(teks) {
    var t = String(teks == null ? '' : teks).trim().replace(',', '.');
    if (!/^\d+(\.\d+)?$/.test(t)) return NaN;
    return parseFloat(t);
  }

  function zScore(skor, sd, mu) { return (skor - mu) / sd; }

  // Hasil normal untuk satu z: persentase teratas dan peringkat (belum dibulatkan).
  function dariZ(z, n) {
    var atas = 100 * qnorm(z);
    return { z: z, atasPct: atas, persentil: 100 - atas, peringkat: n * atas / 100 };
  }

  // Pembulatan peringkat agar tidak berpresisi semu: <10 -> "kurang dari 10",
  // <100 -> puluhan, selebihnya 2 angka signifikan; tidak melebihi N.
  function bulatPeringkat(p, n) {
    if (p < 10) return { nilai: 10, kurangDari: true };
    var v;
    if (p < 100) v = Math.round(p / 10) * 10;
    else {
      var m = Math.pow(10, Math.floor(Math.log10(p)) - 1);
      v = Math.round(p / m) * m;
    }
    return { nilai: Math.min(v, n), kurangDari: false };
  }

  function rerata(arr) {
    var s = 0; for (var i = 0; i < arr.length; i++) s += arr[i];
    return s / arr.length;
  }

  // Posisi skor terhadap kuartil nasional (empiris, tanpa asumsi normal).
  function posisiKuartil(skor, st) {
    if (skor < st.min || skor > st.maks) return { kode: 'luar', batasAtas: null };
    if (skor >= st.q3) return { kode: 'q4', bagian: 0.25 };
    if (skor >= st.median) return { kode: 'q3', bagian: 0.5 };
    if (skor >= st.q1) return { kode: 'q2', bagian: 0.75 };
    return { kode: 'q1', bagian: 1 };
  }

  // Faktor ekor atas (rasio peringkat simulasi / normal) pada z dasar, interpolasi
  // linear pada log-rasio di antara titik simulasi k = 1, 2, 3. Mengembalikan
  // null bila z < 1 (selisih kecil, tidak dikoreksi).
  function faktorEkor(zDasar, titik) {
    if (zDasar < 1 || !titik || titik.length === 0) return null;
    var t = titik.slice().sort(function (a, b) { return a.k - b.k; });
    var batasAtas = zDasar > t[t.length - 1].k;
    if (zDasar <= t[0].k) return { rasio: t[0].rasio, diLuar: false };
    for (var i = 1; i < t.length; i++) {
      if (zDasar <= t[i].k) {
        var w = (zDasar - t[i - 1].k) / (t[i].k - t[i - 1].k);
        var lr = Math.log(t[i - 1].rasio) * (1 - w) + Math.log(t[i].rasio) * w;
        return { rasio: Math.exp(lr), diLuar: false };
      }
    }
    return { rasio: t[t.length - 1].rasio, diLuar: batasAtas };
  }

  return {
    qnorm: qnorm, parseSkor: parseSkor, zScore: zScore, dariZ: dariZ,
    bulatPeringkat: bulatPeringkat, rerata: rerata, posisiKuartil: posisiKuartil,
    faktorEkor: faktorEkor
  };
});