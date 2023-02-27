import { execFileSync } from 'node:child_process';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const directoryName = dirname(fileURLToPath(import.meta.url));

const binaryPath = resolve(directoryName, '..', 'parser');

export default function parse(code, options) {
	const cp = execFileSync(binaryPath, { input: code });

	console.log(cp.stdout);

	return cp.stdout;
}
