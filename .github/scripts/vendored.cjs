// What the Ranvier.BAREWire package owes BAREWire and its Fable consumers: each Ranvier.BAREWire*.nupkg that holds
// the library assembly ships BAREWire's licence files under licenses/BAREWire/, the notice, an `MIT AND Apache-2.0`
// licence expression, and every compiled source under fable/, the vendored ones included.
//
//   node vendored.cjs <packages directory>
const fs = require('node:fs');
const path = require('node:path');
const { zipEntries } = require('./packages.cjs');

const required = [
  'licenses/BAREWire/LICENSE',
  'licenses/BAREWire/PATENTS.md',
  'licenses/BAREWire/Commercial.md',
  'licenses/BAREWire/VENDORED.md',
  'licenses/Ranvier.BAREWire/LICENSE',
  'NOTICE.md',
];

function check(packagesDirectory, repositoryRoot) {
  const problems = [];
  const props = fs.readFileSync(path.join(repositoryRoot, 'src/Ranvier.BAREWire/Vendor/BAREWire/BAREWire.props'), 'utf8');
  const vendored = [...props.matchAll(/<Compile Include="([^"]+)"/g)].map(match => match[1]);
  if (vendored.length === 0) problems.push('BAREWire.props lists no vendored source');
  const packages = fs.readdirSync(packagesDirectory)
    .filter(file => /^Ranvier\.BAREWire(\.Traced)?\.\d.*\.nupkg$/.test(file));
  if (packages.length !== 2) problems.push(`expected the untraced and traced Ranvier.BAREWire packages, found [${packages}]`);
  for (const file of packages) {
    const entries = zipEntries(fs.readFileSync(path.join(packagesDirectory, file)));
    for (const entry of required) {
      if (!entries.has(entry)) problems.push(`${file}: missing ${entry}`);
    }
    const nuspec = [...entries.keys()].find(name => !name.includes('/') && name.endsWith('.nuspec'));
    const manifest = entries.get(nuspec)().toString('utf8');
    if (!/<license type="expression">MIT AND Apache-2\.0<\/license>/.test(manifest)) {
      problems.push(`${file}: the licence expression is not MIT AND Apache-2.0`);
    }
    for (const source of [...vendored, 'Types.fs', 'BareCodec.fs', 'Frames.fs', 'Session.fs']) {
      if (!entries.has(`fable/${source}`)) problems.push(`${file}: missing fable/${source}`);
    }
  }
  return problems;
}

module.exports = { check };

if (require.main === module) {
  const [packagesDirectory = 'bin'] = process.argv.slice(2);
  const problems = check(packagesDirectory, path.resolve(__dirname, '../..'));
  for (const problem of problems) console.error(problem);
  if (problems.length > 0) process.exit(1);
  console.log(`The Ranvier.BAREWire packages in ${packagesDirectory} ship BAREWire's licence files and every Fable source.`);
}
