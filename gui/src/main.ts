import { app, BrowserWindow } from "electron";
import * as path from "path";

let window: Electron.BrowserWindow;

function createWindow() {
    window = new BrowserWindow({ width: 800, height: 600 });
    window.loadFile(path.join(__dirname, "../index.html"));
    window.on("closed", () => {
        window = null;
    });
}

app.on("ready", createWindow);
