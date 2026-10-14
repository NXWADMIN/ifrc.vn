// Builds shiny/data/fms-seed.db from the shared seed (core/seed.js), so both apps use the same data.
// Run from the fms folder: node shiny/tools/export-seed.js   then   Rscript shiny/tools/rehash.R
const fs = require('node:fs');
const path = require('node:path');
const { DatabaseSync } = require('node:sqlite');
const { open } = require('../../server/db.js');
const FMS = require('../../server/load-core.js');

(async () => {
  const out = path.join(__dirname, '..', 'data', 'fms-seed.db');
  const file = path.join(require('node:os').tmpdir(), `fms-seed-${process.pid}.db`);
  for (const ext of ['', '-wal', '-shm']) fs.rmSync(file + ext, { force: true });
  const db = open(file);
  const logins = await FMS.seed(db);
  db.close();
  const raw = new DatabaseSync(file);
  raw.exec('PRAGMA journal_mode = DELETE; VACUUM;');
  raw.close();
  fs.copyFileSync(file, out);
  fs.rmSync(file, { force: true });
  fs.writeFileSync(path.join(__dirname, 'logins.json'), JSON.stringify(logins, null, 2));
  console.log(`Wrote ${out} (${(fs.statSync(out).size / 1e6).toFixed(1)} MB) and logins.json`);
})().catch((e) => { console.error(e); process.exit(1); });
