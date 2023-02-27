import { readFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import parse from './parser.js';

const directoryName = dirname(fileURLToPath(import.meta.url));

parse(await readFile(resolve(directoryName, '..', 'TestModule.purs')));
