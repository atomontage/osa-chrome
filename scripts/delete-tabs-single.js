// This is free and unencumbered software released into the public domain.

function delete_tabs_single(chrome_app_name, tab_ids)
{
    const name  = chrome_app_name;
    const se    = Application('System Events');
    const procs = se.applicationProcesses.whose({name: name}).unixId.get();

    if (procs.length !== 0) {
        const chrome = Application(name);
        let count    = 0;

        // tab_ids should be a table with window ids as keys
        // and lists of tab ids as values

        try {
            for (var window_id of Object.keys(tab_ids)) {
                try {
                    var window = chrome.windows.byId(window_id);

                    for (var tab_id of tab_ids[window_id]) {
                        try {
                            window.tabs.byId(tab_id).close();
                            count += 1;
                        } catch (_) {}
                    }
                } catch (_) {}
            }
        } catch (_) {}

        return {'count' : count};
    }

    return {
        'error' : 'Process not found',
    };
}
