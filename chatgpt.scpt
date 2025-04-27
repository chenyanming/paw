on run argv
    if (count of argv) < 2 then
        display dialog "Please provide both browser name and message text"
        return
    end if
    set browserName to item 1 of argv
    set messageText to item 2 of argv

    -- Check if the browserName is a valid application
    set supportedBrowsers to {"Safari", "Google Chrome", "Firefox", "Brave Browser"}
    if browserName is not in supportedBrowsers then
        display dialog "Unsupported browser: " & browserName & return & "Supported browsers: Safari, Google Chrome, Firefox, Brave Browser"
        return
    end if

    -- Execute appropriate handler based on browser name
    if browserName is "Brave Browser" then
        handleBraveBrowser(messageText)
    else if browserName is "Google Chrome" then
        handleChromeBrowser(messageText)
    else if browserName is "Safari" then
        handleSafari(messageText)
    else if browserName is "Firefox" then
        handleFirefox(messageText)
    end if
end run


on handleBraveBrowser(messageText)
    tell application "Brave Browser"
        set foundTab to false
        set chatGPTURL to "https://chatgpt.com/"
        set currentTab to missing value

        repeat with w in windows
            repeat with t in tabs of w
                if URL of t starts with chatGPTURL then
                    set currentTab to t
                    set foundTab to true
                    exit repeat
                end if
            end repeat
            if foundTab then exit repeat
        end repeat

        if not foundTab then
            try
                set currentTab to make new tab at end of tabs of window 1 with properties {URL:chatGPTURL}
            on error
                set currentTab to make new tab at end of tabs of (make new window) with properties {URL:chatGPTURL}
            end try
            delay 2
        end if

        try
            tell currentTab to execute javascript "
            (function (message) {
                const promptTextarea = document.querySelector('#prompt-textarea');
                if (!promptTextarea || !promptTextarea.isContentEditable) {
                    throw new Error('Could not find editable prompt field');
                }
                promptTextarea.focus();
                promptTextarea.innerText = message;
                const events = [
                    new Event('focus', { bubbles: true }),
                    new InputEvent('input', { bubbles: true }),
                    new KeyboardEvent('keydown', { bubbles: true }),
                    new KeyboardEvent('keyup', { bubbles: true })
                ];
                events.forEach(event => promptTextarea.dispatchEvent(event));
                setTimeout(() => {
                    const sendButtons = [
                        document.querySelector('[data-testid=\\'send-button\\']'),
                        document.querySelector('[data-testid=\\'composer-submit-button\\']'),
                        document.querySelector('button[aria-label=\\'Send message\\']')
                    ].filter(Boolean);
                    const sendButton = sendButtons.find(btn => !btn.disabled);
                    if (sendButton) sendButton.click();
                }, 100);
            })('" & messageText & "');"
        on error jsError
            display notification "JavaScript failed: " & jsError
        end try
    end tell
end handleBraveBrowser

on handleChromeBrowser(messageText)
    tell application "Google Chrome"
        set foundTab to false
        set chatGPTURL to "https://chatgpt.com/"
        set currentTab to missing value

        repeat with w in windows
            repeat with t in tabs of w
                if URL of t starts with chatGPTURL then
                    set currentTab to t
                    set foundTab to true
                    exit repeat
                end if
            end repeat
            if foundTab then exit repeat
        end repeat

        if not foundTab then
            try
                set currentTab to make new tab at end of tabs of window 1 with properties {URL:chatGPTURL}
            on error
                set currentTab to make new tab at end of tabs of (make new window) with properties {URL:chatGPTURL}
            end try
            delay 2
        end if

        try
            tell currentTab to execute javascript "
            (function (message) {
                const promptTextarea = document.querySelector('#prompt-textarea');
                if (!promptTextarea || !promptTextarea.isContentEditable) {
                    throw new Error('Could not find editable prompt field');
                }
                promptTextarea.focus();
                promptTextarea.innerText = message;
                const events = [
                    new Event('focus', { bubbles: true }),
                    new InputEvent('input', { bubbles: true }),
                    new KeyboardEvent('keydown', { bubbles: true }),
                    new KeyboardEvent('keyup', { bubbles: true })
                ];
                events.forEach(event => promptTextarea.dispatchEvent(event));
                setTimeout(() => {
                    const sendButtons = [
                        document.querySelector('[data-testid=\\'send-button\\']'),
                        document.querySelector('[data-testid=\\'composer-submit-button\\']'),
                        document.querySelector('button[aria-label=\\'Send message\\']')
                    ].filter(Boolean);
                    const sendButton = sendButtons.find(btn => !btn.disabled);
                    if (sendButton) sendButton.click();
                }, 100);
            })('" & messageText & "');"
        on error jsError
            display notification "JavaScript failed: " & jsError
        end try
    end tell
end handleChromeBrowser


on handleSafari(messageText)
    tell application "Safari"
        activate
        set foundTab to false
        set chatGPTURL to "https://chat.openai.com/"
        set currentTab to missing value
        repeat with w in windows
            repeat with t in tabs of w
                if URL of t starts with chatGPTURL then
                    set current tab of w to t
                    set foundTab to true
                    exit repeat
                end if
            end repeat
            if foundTab then exit repeat
        end repeat
        if not foundTab then
            try
                tell window 1
                    set currentTab to make new tab with properties {URL:chatGPTURL}
                    set current tab to currentTab
                end tell
            on error
                make new document with properties {URL:chatGPTURL}
            end try
            delay 2
        end if

        try
            tell current tab of window 1
                do JavaScript "
                (function (message) {
                    const promptTextarea = document.querySelector('#prompt-textarea');
                    if (!promptTextarea || !promptTextarea.isContentEditable) {
                        throw new Error('Could not find editable prompt field');
                    }
                    promptTextarea.focus();
                    promptTextarea.innerText = message;
                    const events = [
                        new Event('focus', { bubbles: true }),
                        new InputEvent('input', { bubbles: true }),
                        new KeyboardEvent('keydown', { bubbles: true }),
                        new KeyboardEvent('keyup', { bubbles: true })
                    ];
                    events.forEach(event => promptTextarea.dispatchEvent(event));
                    setTimeout(() => {
                        const sendButtons = [
                            document.querySelector('[data-testid=\\'send-button\\']'),
                            document.querySelector('[data-testid=\\'composer-submit-button\\']'),
                            document.querySelector('button[aria-label=\\'Send message\\']')
                        ].filter(Boolean);
                        const sendButton = sendButtons.find(btn => !btn.disabled);
                        if (sendButton) sendButton.click();
                    }, 100);
                })('" & messageText & "');"
            end tell
        on error jsError
            display notification "JavaScript failed: " & jsError
        end try
    end tell
end handleSafari

on handleFirefox(messageText)
    tell application "Firefox"
        activate
        open location "https://chat.openai.com/"
        delay 3

        -- Firefox has limited AppleScript support, so we'll use System Events
        tell application "System Events"
            -- Try to find and focus the text area
            delay 1
            keystroke tab
            delay 0.5

            -- Type the message
            keystroke messageText
            delay 0.5

            -- Press Enter to send
            keystroke return
        end tell
    end tell
end handleFirefox
