import { app, BrowserWindow } from 'electron'
import installExtension, { VUEJS_DEVTOOLS } from 'electron-devtools-installer'
import { enableLiveReload } from 'electron-compile'
import { unwindmc } from './unwindmc'

const isDevMode = process.execPath.match(/[\\/]electron/)
if (isDevMode) {
    enableLiveReload()
}

let window: Electron.BrowserWindow | null

const createWindow = async () => {
    window = new BrowserWindow({
        title: `Unwind MC v${unwindmc.version()}`,
        width: 1024,
        height: 768,
        webPreferences: {
            nodeIntegration: true,
        },
    })
    window.loadURL(`file://${__dirname}/index.jade`)
    if (isDevMode) {
        await installExtension(VUEJS_DEVTOOLS)
        window.webContents.openDevTools({mode: 'bottom'})
    }
    window.on('closed', () => {
        window = null
    })
}

app.on('ready', createWindow)
