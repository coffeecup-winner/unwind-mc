const config = {
    "make_targets": {
        "win32": [
            "squirrel"
        ],
        "darwin": [
            "zip"
        ],
        "linux": [
            "zip",
            "deb",
            "rpm"
        ]
    },
    "electronPackagerConfig": {
        "packageManager": "npm"
    },
    "electronWinstallerConfig": {
        "name": "unwindmc"
    },
    "electronInstallerDebian": {},
    "electronInstallerRedhat": {},
    "github_repository": {
        "owner": "",
        "name": ""
    },
    "windowsStoreConfig": {
        "packageName": "",
        "name": "unwindmc"
    }
}

module.exports = config
