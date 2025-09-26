import { useState, useEffect } from "react";
import '@vscode-elements/elements';
import '@vscode-elements/elements/dist/vscode-button';
import '@vscode-elements/elements/dist/vscode-button-group';
import '@vscode-elements/elements/dist/vscode-table';
import { siR } from "simple-icons/icons";
import TimeAgo from 'timeago-react';
import prettyBytes from 'pretty-bytes';

function Icon({ icon, size = 14 }: { icon: { svg: string; hex: string }; size?: number }) {
  return (
    <svg
      role="img"
      viewBox="0 0 14 14"
      width={size}
      height={size}
      fill="currentColor"
      style={{ verticalAlign: "-0.225em" }}
      dangerouslySetInnerHTML={{ __html: icon.svg }}
    />
  );
}

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
      <vscode-table zebra bordered-rows
        columns={["6", "6", "15", "10", "10", "10", "6"]}>
        <vscode-table-header slot="header">
          <vscode-table-header-cell>Type</vscode-table-header-cell>
          <vscode-table-header-cell><Icon icon={siR}/></vscode-table-header-cell>
          <vscode-table-header-cell>Platform</vscode-table-header-cell>
          <vscode-table-header-cell>Last</vscode-table-header-cell>
          <vscode-table-header-cell>Size</vscode-table-header-cell>
          <vscode-table-header-cell>Id</vscode-table-header-cell>
          <vscode-table-header-cell></vscode-table-header-cell>
        </vscode-table-header>
        <vscode-table-body slot="body">
        {
          data.builds.map((build: Build) => {
            return(<vscode-table-row>
              <vscode-table-cell>{build_type(build.type)}</vscode-table-cell>
              <vscode-table-cell>{build.r_version}</vscode-table-cell>
              <vscode-table-cell><code>{build_platform(build.platform)}</code></vscode-table-cell>
              <vscode-table-cell><TimeAgo datetime={build.last_built}/></vscode-table-cell>
              <vscode-table-cell>{prettyBytes(build.disk_size)}</vscode-table-cell>
              <vscode-table-cell><code className="code">{build.hash}</code></vscode-table-cell>
              <vscode-table-cell>
                <vscode-button secondary icon="trash" iconOnly
                  onClick={() => delBuild(build.hash)}>
                </vscode-button>
              </vscode-table-cell>
            </vscode-table-row>)
          })
        }
        </vscode-table-body>
      </vscode-table>
    </div>
    <div style={{ paddingLeft: 20}}>
      <small>
        🏭: release &nbsp;&nbsp; 🐛: debug &nbsp;&nbsp; 🧪: coverage
      </small>
    </div>
    <div style={{ padding: 10 }}>
      <vscode-button-group style={{ padding: 10 }}>
        <vscode-button icon="refresh" onClick={reload}>
          Refresh
        </vscode-button>
      </vscode-button-group>
      <vscode-button-group>
        <vscode-button secondary icon="trash" onClick={delAll}>
          Delete all
        </vscode-button>
      </vscode-button-group>
    </div>
    </div>
  );
}

function build_type(type: string) {
  switch (type) {
    case "release": return "🏭";
    case "debug": return "🐛";
    case "coverage": return "🧪";
    default: return type;
  }
}

function build_platform(platform: string) {
  const parts = platform.split("-");
  if (parts[1] == "apple") {
    const arch = parts[0] == "aarch64" ? " arm64" : "";
    return `macOS${arch}`;
  } else if (parts[2] == "linux") {
    return "Linux";
  } else if (parts[2] == "mingw32") {
    const arch = parts[0] == "aarch64" ? " arm64" : "";
    return `Windows${arch}`;
  } else {
    return platform;
  }
}

export default App;
