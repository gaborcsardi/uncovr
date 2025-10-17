import * as vscode from 'vscode';
import { inPositron } from '@posit-dev/positron';
import * as path from 'path';
import * as fsx from 'fs';
import { promises as fs } from "fs";
const getFolderSize = require("get-folder-size");
import { rimraf } from 'rimraf';

interface Build {
  r_version: string;
	platform: string;
	type: string;
	hash: string;
	pkg_name: string;
	pkg_version: string;
	disk_size: number;
	last_built: Date;
}

interface AppState {
	folder?: string;
	builds: Array<Build>;
}

let appState: AppState = { builds: [] };

async function updateState() {
  // Get the first workspace folder (if there is one)
  const folders = vscode.workspace.workspaceFolders;
  if (!folders) {
    vscode.window.showErrorMessage("No workspace folder open");
    return;
  }

  const folderUri = vscode.Uri.joinPath(folders[0].uri, ".dev");

	appState.folder = path.basename(path.dirname(folderUri.fsPath));

	let dirs = await listBuildDirs(folderUri);
	if (!dirs) { return; }
	let builds = [];

	for (const d of dirs) {
		let conf = path.join(d, "_setup.json");
		if (!fsx.existsSync(conf)) { continue; }
		let cnt = await fs.readFile(conf, 'utf8');
		let obj = JSON.parse(cnt);
		let stat = await fs.stat(conf);
		let size = await getFolderSize.loose(d);
		builds.push({
			r_version: obj.rver,
			platform: obj.platform,
			type: obj.type,
			hash: obj.hash,
			pkg_name: obj.pkgname,
			pkg_version: obj.pkgversion,
			disk_size: size,
			last_built: stat.mtime
		});
	}

	appState.builds = builds;
}

async function updateStateAndNotify(panel: vscode.WebviewPanel) {
	await updateState();
  panel.webview.postMessage({ command: "setState", data: appState });
}

async function deleteBuild(hash: string) {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders) {
    vscode.window.showErrorMessage("No workspace folder open");
    return;
  }

  const folderUri = vscode.Uri.joinPath(folders[0].uri, ".dev", hash);
	if (!fsx.existsSync(folderUri.fsPath)) {
		vscode.window.showErrorMessage(`No such build: ${hash}`);
		return;
	}

	const libPath = vscode.Uri.joinPath(folderUri, "__dev_lib__");
	try {
		if (fsx.existsSync(libPath.fsPath)) {
			await fs.chmod(libPath.fsPath, 0o755);
		}
		await rimraf(folderUri.fsPath);
	} catch (err) {
		vscode.window.showErrorMessage(`Failed to delete build ${hash}: ${err}`);
	}
}

async function deleteAll() {
  // Get the first workspace folder (if there is one)
  const folders = vscode.workspace.workspaceFolders;
  if (!folders) {
    vscode.window.showErrorMessage("No workspace folder open");
    return;
  }

  const folderUri = vscode.Uri.joinPath(folders[0].uri, ".dev");

	appState.folder = path.basename(path.dirname(folderUri.fsPath));

	let dirs = await listBuildDirs(folderUri);
	if (!dirs) { return; }

	for (const d of dirs) {
		await deleteBuild(path.basename(d));
	}
}

export async function listBuildDirs(folderUri: vscode.Uri) {
  try {
    // Read directory entries
    const entries = await vscode.workspace.fs.readDirectory(folderUri);
		return entries.map(ent => path.join(folderUri.fsPath, ent[0]));
  } catch (err) {
    vscode.window.showErrorMessage(`Failed to read directory: ${err}`);
  }
}

export function activate(context: vscode.ExtensionContext) {

	const reload = vscode.commands.registerCommand('uncovr.reload', () => {
		if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::reload()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::reload()');
		}
	});
	context.subscriptions.push(reload);

	const test = vscode.commands.registerCommand('uncovr.test', () => {
		if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::test()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::test()');
		}
	});
	context.subscriptions.push(test);

  const document = vscode.commands.registerCommand('uncovr.document', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::document()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::document()');
		}
	});
	context.subscriptions.push(document);

	const retest = vscode.commands.registerCommand('uncovr.retest', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::retest()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::retest()');
		}
	});
	context.subscriptions.push(retest);

	const report = vscode.commands.registerCommand('uncovr.report', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::report()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::report()');
		}
	});
	context.subscriptions.push(report);

	const builds = vscode.commands.registerCommand('uncovr.builds', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::builds()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::builds()');
		}
	});
	context.subscriptions.push(builds);

	const last = vscode.commands.registerCommand('uncovr.last', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::last()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::last()');
		}
	});
	context.subscriptions.push(last);

	const testActive = vscode.commands.registerCommand('uncovr.testActive', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::test_active()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::test_active()');
		}
	});
	context.subscriptions.push(testActive);

	const lcov = vscode.commands.registerCommand('uncovr.lcov', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::lcov()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::lcov()');
		}
	});
	context.subscriptions.push(lcov);

	const install = vscode.commands.registerCommand('uncovr.install', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::install()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::install()');
		}
	});
	context.subscriptions.push(install);

	const diff = vscode.commands.registerCommand('uncovr.diff', () => {
	if (inPositron()) {
			vscode.commands.executeCommand(
				'workbench.action.executeCode.console',
				'uncovr::diff()'
			);
		} else {
			vscode.commands.executeCommand('r.runCommand', 'uncovr::diff()');
		}
	});
	context.subscriptions.push(install);

  context.subscriptions.push(
    vscode.commands.registerCommand('uncovr.buildView', async () => {
			const panel = vscode.window.createWebviewPanel(
				"uncovrBuilds",
				"R builds",
				vscode.ViewColumn.Two,
				{
					enableScripts: true,
					localResourceRoots: [vscode.Uri.file(path.join(context.extensionPath, "media"))]
				}
			);

			await updateState();
			panel.webview.html = getWebviewContent(
				panel.webview,
				context.extensionPath,
				appState
			);

			// Handle messages from React
			panel.webview.onDidReceiveMessage(async message => {
				switch (message.command) {
					case "reload":
						await updateStateAndNotify(panel);
						break;
					case "delAll":
						await deleteAll();
						await updateStateAndNotify(panel);
						break;
					case "delBuild":
						await deleteBuild(message.args[0]);
						await updateStateAndNotify(panel);
						break;
				}
			});

			// Watch for changes
		  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
			if (!!workspaceFolder) {
				const pattern = new vscode.RelativePattern(workspaceFolder, '.dev/*/_setup.json');
				const watcher = vscode.workspace.createFileSystemWatcher(pattern);
				watcher.onDidCreate(async uri => {
					await updateStateAndNotify(panel);
				});

				watcher.onDidChange(async uri => {
					await updateStateAndNotify(panel);
				});

				watcher.onDidDelete(async uri => {
					await updateStateAndNotify(panel);
				});

				context.subscriptions.push(watcher);
			}
		})
  );
}

function getWebviewContent(
  webview: vscode.Webview,
  extensionPath: string,
  initialData: any
): string {
  const mediaPath = path.join(extensionPath, 'media');
  const indexPath = path.join(mediaPath, 'index.html');
  let html = fsx.readFileSync(indexPath, 'utf8');

  html = html.replace(
    /"\/assets\//g,
    `"${webview.asWebviewUri(
      vscode.Uri.file(path.join(mediaPath, 'assets'))
    ).toString()}/`
  );

  // Inject initial data before </head>
  const initScript = `<script>window.initialData = ${JSON.stringify(
    initialData
  )};</script>`;
  html = html.replace('</head>', `${initScript}</head>`);

  return html;
}
