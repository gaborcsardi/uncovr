import * as vscode from 'vscode';
import { inPositron } from '@posit-dev/positron';

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
}

export function deactivate() {}
