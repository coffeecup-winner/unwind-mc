import { app, BrowserWindow } from "electron";
import * as ffi from "ffi";
import * as path from "path";
import * as ref from "ref";

const libunwindmc = ffi.Library("libunwindmc", {
    version: ["string", []],
});

let window: Electron.BrowserWindow;

function createWindow() {
    window = new BrowserWindow({ width: 800, height: 600 });
    window.loadFile(path.join(__dirname, "../index.html"));
    window.setTitle("Unwind MC v" + libunwindmc.version());
    window.on("closed", () => {
        window = null;
    });
}

app.on("ready", createWindow);
