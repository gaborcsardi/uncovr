import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	const reload = vscode.commands.registerCommand('uncovr.reload', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::reload()');
	});
	context.subscriptions.push(reload);

	const test = vscode.commands.registerCommand('uncovr.test', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::test()');
	});
	context.subscriptions.push(test);

  const document = vscode.commands.registerCommand('uncovr.document', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::document()');
	});
	context.subscriptions.push(document);

	const retest = vscode.commands.registerCommand('uncovr.retest', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::retest()');
	});
	context.subscriptions.push(retest);

	const report = vscode.commands.registerCommand('uncovr.report', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::report()');
	});
	context.subscriptions.push(report);

	const builds = vscode.commands.registerCommand('uncovr.builds', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::builds()');
	});
	context.subscriptions.push(builds);

	const last = vscode.commands.registerCommand('uncovr.last', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::last()');
	});
	context.subscriptions.push(last);

	const testActive = vscode.commands.registerCommand('uncovr.testActive', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::test_active()');
	});
	context.subscriptions.push(testActive);

	const lcov = vscode.commands.registerCommand('uncovr.lcov', () => {
		vscode.commands.executeCommand('r.runCommand', 'uncovr::lcov()');
	});
	context.subscriptions.push(lcov);
}

export function deactivate() {}
