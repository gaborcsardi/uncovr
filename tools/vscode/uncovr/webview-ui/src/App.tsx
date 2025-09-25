import { useState, useEffect } from "react";
import '@vscode-elements/elements';
import '@vscode-elements/elements/dist/vscode-button';
import '@vscode-elements/elements/dist/vscode-table';

interface Build {
  r_version: string;
	platform: string;
	type: string;
	hash: string;
	pkg_name: string;
	pkg_version: string;
	disk_size?: bigint;
	last_built?: Date;
}

interface AppState {
	folder?: string;
	builds: Array<Build>;
}

declare global {
  interface Window {
    initialData: AppState;
  }
}

// @ts-expect-error acquireVsCodeApi comes from VS Code
const vscode = acquireVsCodeApi();

function App() {
  const [data, setData] = useState(() => window.initialData);

  useEffect(() => {
    const handler = (event: MessageEvent) => {
      const message = event.data;
      switch (message.command) {
        case "setState":
          setData(message.data);
          break;
      }
    };
    window.addEventListener("message", handler);
    return () => window.removeEventListener("message", handler);
  }, []);

  const reload = () => {
    vscode.postMessage({ command: "reload" });
  };

  const delBuild = (hash: string) => {
    vscode.postMessage({ command: "delBuild", args: [ hash ] });
  };

  const delAll = () => {
    vscode.postMessage({ command: "delAll" });
  };

  return (
    <div>
    <div style={{ padding: 20 }}>
      <h2>R builds — <code>{data.folder}</code></h2>
      <vscode-table zebra bordered-rows>
        <vscode-table-header slot="header">
          <vscode-table-header-cell></vscode-table-header-cell>
          <vscode-table-header-cell>Type</vscode-table-header-cell>
          <vscode-table-header-cell>R</vscode-table-header-cell>
          <vscode-table-header-cell>Platform</vscode-table-header-cell>
          <vscode-table-header-cell>Last built</vscode-table-header-cell>
          <vscode-table-header-cell>Disk size</vscode-table-header-cell>
          <vscode-table-header-cell>Id</vscode-table-header-cell>
        </vscode-table-header>
        <vscode-table-body slot="body">
        {
          data.builds.map((build: Build) => {
            return(<vscode-table-row>
              <vscode-table-cell>
                <vscode-button icon="trash" iconOnly onClick={() => delBuild(build.hash)}>
                </vscode-button>
              </vscode-table-cell>
              <vscode-table-cell>{build.type}</vscode-table-cell>
              <vscode-table-cell>{build.r_version}</vscode-table-cell>
              <vscode-table-cell><code>{build.platform}</code></vscode-table-cell>
              <vscode-table-cell></vscode-table-cell>
              <vscode-table-cell></vscode-table-cell>
              <vscode-table-cell><code className="code">{build.hash}</code></vscode-table-cell>
            </vscode-table-row>)
          })
        }
        </vscode-table-body>
      </vscode-table>
    </div>
    <div>
      <vscode-button icon="refresh" onClick={reload}>
        Refresh
      </vscode-button>
      <vscode-button icon="trash" onClick={delAll}>
        Delete all
      </vscode-button>
    </div>
    </div>
  );
}

export default App;
