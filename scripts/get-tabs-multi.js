// This is free and unencumbered software released into the public domain.

function get_tabs_multi(chrome_app_name, machine_url)
{
    // Retrieves all tabs from all windows belonging to all currently running
    // Chrome instances (processes).
    const name  = chrome_app_name;
    const se    = Application('System Events');
    const procs = se.applicationProcesses.whose({name: name}).unixId.get();
    let out     = {};

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
