// Uji kecocokan web vs paper: membandingkan perhitungan JavaScript (web/calc.js
// + web/data.js) dengan keluaran R (output/tables/*.csv).
// Jalankan dari akar proyek:  node tests/web/verify_web.mjs
import { readFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const calc = require('../../web/calc.js');

const sandbox = { window: {} };
vm.runInNewContext(readFileSync('web/data.js', 'utf8'), sandbox);
const D = sandbox.window.UTBK_DATA;

let gagal = 0, lulus = 0;
function cek(nama, ok, rinci = '') {
  if (ok) lulus += 1; else { gagal += 1; console.log('GAGAL:', nama, rinci); }
}
function csv(path) {
  const [h, ...baris] = readFileSync(path, 'utf8').replace(/\r/g, '').trim().split('\n');
  const kol = h.split(',');
  return baris.map(b => Object.fromEntries(b.split(',').map((v, i) => [kol[i], v])));
}

// 1. Fungsi ekor normal vs nilai rujukan (scipy)
const rujukan = [[0, 0.5], [0.5, 0.3085375387259869], [1.7, 0.04456546275854304],
  [2.5, 0.006209665325776132], [2.9999, 0.001350341282954925], [3, 0.0013498980316300933],
  [3.0001, 0.0013494549132607172], [4, 3.167124183311986e-05], [5, 2.866515718791933e-07],
  [6, 9.865876450376946e-10], [8, 6.22096057427174e-16]];
for (const [z, q] of rujukan) {
  const rel = Math.abs(calc.qnorm(z) - q) / q;
  cek(`qnorm(${z})`, rel < 1e-9, `rel=${rel}`);
}
cek('qnorm simetri', Math.abs(calc.qnorm(-1.3) + calc.qnorm(1.3) - 1) < 1e-15);

// 2. Tabel komposit (Tabel 9) vs tabel_ranking_komposit.csv
const N = D.meta.n_populasi;
const komp = csv('output/tables/tabel_ranking_komposit.csv');
for (const b of komp) {
  const sk = D.skenario.find(s => Math.abs(s.sd - parseFloat(b.sd_pop_z)) < 1e-9);
  cek(`skenario ${b.scenario} ada`, !!sk);
  if (!sk) continue;
  const z = calc.zScore(parseFloat(b.score), sk.sd, D.meta.mu);
  const h = calc.dariZ(z, N);
  cek(`top_pct ${b.scenario} ${b.benchmark_label}`,
    Math.abs(h.atasPct - parseFloat(b.top_pct)) <= 1e-9 * Math.max(1, parseFloat(b.top_pct)));
  cek(`rank ${b.scenario} ${b.benchmark_label}`, Math.round(h.peringkat) === parseInt(b.rank_nasional, 10),
    `${Math.round(h.peringkat)} vs ${b.rank_nasional}`);
}

// 3. Tabel per subtes vs tabel_ranking_subtes.csv
for (const b of csv('output/tables/tabel_ranking_subtes.csv')) {
  const st = D.subtes.find(s => s.kode === b.subtest);
  const h = calc.dariZ(calc.zScore(parseFloat(b.score), st.sd, D.meta.mu), N);
  cek(`rank ${b.subtest} ${b.benchmark_label}`, Math.round(h.peringkat) === parseInt(b.rank_nasional, 10),
    `${Math.round(h.peringkat)} vs ${b.rank_nasional}`);
}

// 4. Parameter inti vs berkas hasil
const hist = csv('output/tables/iteration_history.csv');
const sZ = parseFloat(hist[hist.length - 1].sd_composite_out);
const th = D.skenario.find(s => s.kode === 'thorndike');
cek('S_Z Thorndike', Math.abs(th.sd - sZ) < 1e-9);
cek('N populasi', N === 846518);
cek('8 subtes', D.subtes.length === 8);
cek('7 skenario', D.skenario.length === 7);

// 5. Parser, pembulatan, faktor ekor
cek('parse koma', calc.parseSkor('670,20') === 670.2);
cek('parse titik', calc.parseSkor(' 670.2 ') === 670.2);
cek('parse sampah', Number.isNaN(calc.parseSkor('6a7')) && Number.isNaN(calc.parseSkor('')) &&
  Number.isNaN(calc.parseSkor('-5')) && Number.isNaN(calc.parseSkor('1.2.3')));
cek('bulat 3449', calc.bulatPeringkat(3449, N).nilai === 3400);
cek('bulat 57', calc.bulatPeringkat(57, N).nilai === 60);
cek('bulat <10', calc.bulatPeringkat(3, N).kurangDari === true);
cek('bulat cap N', calc.bulatPeringkat(846000, N).nilai <= N);
const t0 = D.ekor.filter(e => e.b === 0);
cek('faktor ekor k=2', Math.abs(calc.faktorEkor(2, t0).rasio - t0.find(e => e.k === 2).rasio) < 1e-12);
cek('faktor ekor <1 null', calc.faktorEkor(0.5, t0) === null);
cek('faktor ekor >3 diLuar', calc.faktorEkor(3.5, t0).diLuar === true);
const r25 = calc.faktorEkor(2.5, t0).rasio;
cek('faktor ekor monoton', r25 > t0.find(e => e.k === 2).rasio && r25 < t0.find(e => e.k === 3).rasio);

console.log(`Lulus: ${lulus}, gagal: ${gagal}`);
process.exit(gagal ? 1 : 0);
