const registerSw = async () => {
  try {
    const registration = await navigator.serviceWorker.register(`/sw.js`, {scope: "./"})
    registration.update();
    console.log("Service worker registration succeeded:", registration)
  } catch (error) {
    console.error(`Service worker registration failed: ${error}`)
  }
}

registerSw()

// reload if page is not controlled as a workaround to the fact that when the page is force reloaded 
// the SW doesn't work
// https://github.com/mswjs/msw/issues/98
if (!navigator.serviceWorker.controller) {
  location.reload()
}

const COUNTER = 'counter'

const counterInitialValue = window.localStorage.getItem(COUNTER) || '0'

const app = Elm.Main.init({flags: Number(counterInitialValue)});

app.ports.setCounter.subscribe(counter => {
	window.localStorage.setItem(COUNTER, counter)
})

app.ports.getUserName.send('baz')
