export const flags = ({ env }) => {
    return {
        token: JSON.parse(localStorage.token || null)
    }
}

export const onReady = ({ app, env }) => {
    // PORTS
    if (app.ports && app.ports.saveToLocalStorage) {
        app.ports.saveToLocalStorage.subscribe(({ key, value }) => {
            localStorage[key] = JSON.stringify(value)
        })
    }

    if (app.ports && app.ports.scrollTo) {
        app.ports.scrollTo.subscribe(function (id) {
            var element = document.getElementById(id);
            if (element) {
                element.scrollIntoView({ behavior: 'smooth' });
            }
        });
    }
}
