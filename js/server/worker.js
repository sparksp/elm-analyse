const open = require('open');
const fileLoadingPorts = require('../util/file-loading-ports');
const loggingPorts = require('../util/logging-ports');
const dependencies = require('../util/dependencies');
module.exports = function worker(config, elmPackage, onload) {
    dependencies.getDependencies(function(registry) {
        const directory = process.cwd();
        var Elm = require('../backend-elm.js');
        var app = Elm.Elm.Analyser.worker({
            server: true,
            elmPackage: elmPackage,
            registry: registry || []
        });

        app.ports.sendReportValue.subscribe(function(report) {
            console.log('Found ' + report.messages.length + ' message(s)');
        });

        loggingPorts(app, config, directory);
        fileLoadingPorts(app, config, process.cwd());
        onload(app);
        if (config.open) {
            open('http://localhost:' + config.port);
        }
    });
};
