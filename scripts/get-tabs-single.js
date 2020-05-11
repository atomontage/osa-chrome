// This is free and unencumbered software released into the public domain.

function get_tabs_single(chrome_app_name)
{
    // Retrieves all tabs from all windows belonging to a single Chrome
    // instance (process). This function should be what most users need.
    const name  = chrome_app_name;
    const se    = Application('System Events');
    const procs = se.applicationProcesses.whose({name: name}).unixId.get();
    let out     = {};


    if (procs.length !== 0) {
        // Note that the following works if there's only one Chrome process
        // currently running. Otherwise it's possible that the PID returned
        // from System Events will not match the Chrome instance retrieved
        // through Application(). AFAIK it is not possible to get a correct
        // reference to an application by PID through Application(), one
        // has to use System Events.
        const pid    = procs[0];
        const chrome = Application(name);

        // These are lazy-evaluated selectors that return results in one batch,
        // minimizing the number of Apple event calls. A standard loop to
        // retrieve each tab url/title would be noticeably slower.
        const tabs    = [chrome.windows.tabs.id.get(),
                         chrome.windows.tabs.url.get(),
                         chrome.windows.tabs.title.get()];

        const windows = chrome.windows.id.get();
        const active  = chrome.windows.activeTab.id.get();
        out[pid]      = [windows, active, tabs];
    }

    return out;
}
