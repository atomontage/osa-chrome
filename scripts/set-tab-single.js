// This is free and unencumbered software released into the public domain.

function set_tab_single(chrome_app_name, window_id, tab_id, raise)
{
    const name  = chrome_app_name;
    const se    = Application('System Events');
    const procs = se.applicationProcesses.whose({name: name}).unixId.get();

    if (procs.length !== 0) {
        const chrome = Application(name);

        try {
            var window = chrome.windows.byId(window_id);
        } catch (_) {
            return {
                'error'      : 'Window not found',
                'error-data' : window_id,
            };
        }

        window.index   = 1;
        window.visible = true;

        var tabs = window.tabs.id.get();

        try {
            window.activeTabIndex = 1 + tabs.indexOf(tab_id);
        } catch (_) {
            return {
                'error'      : 'Tab not found',
                'error-data' : tab_id,
            };
        }

        if (raise === true) {
            if (chrome.windows.length === 1) {
                chrome.activate();
            } else {
                let proc = se.processes[name];

                if (proc.windows.length == 0) {
                    return {
                        'warn' : "System Events does not allow focusing Chrome window in non-currently-visible space",
                    };
                }

                proc.windows[0].actions['AXRaise'].perform();
                proc.frontmost = true;
            }
        }

        return {};
    }

    return {
        'error' : 'Process not found',
    };
}
