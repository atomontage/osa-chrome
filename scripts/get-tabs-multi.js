// This is free and unencumbered software released into the public domain.

function get_tabs_multi(chrome_app_names, machine_url)
{
    // Retrieves all tabs from all windows belonging to all currently running
    // Chrome instances (processes).
    const se  = Application('System Events');
    let query = [];
    let out   = {};

    // Build query to retrieve all PIDs for process names in chrome_app_names
    for (var name of chrome_app_names) {
        query.push({name: name});
    }

    const procs = se.applicationProcesses.whose({_or: query}).unixId.get();
    for (var pid of procs) {
        const chrome  = Application(machine_url + '/?pid=' + pid);
        const tabs    = [chrome.windows.tabs.id.get(),
                         chrome.windows.tabs.url.get(),
                         chrome.windows.tabs.title.get()];

        const windows = chrome.windows.id.get();
        const active  = chrome.windows.activeTab.id.get();
        out[pid]      = [windows, active, tabs];
    }

    return out;
}
