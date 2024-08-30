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

const getCounter = () => {

	const counterValue = window.localStorage.getItem(COUNTER) || '0'

	return Number(counterValue)
}

const app = Elm.Main.init({flags: getCounter()});

app.ports.setCounter.subscribe(counter => {
	window.localStorage.setItem(COUNTER, counter)
})

app.ports.getUserName.send('baz')

app.ports.sendUrlChangeRequest.subscribe(url => {

	if (url.includes('survey')) {
		document.documentElement.className = 'page-two'
	} else {
		document.documentElement.className = 'page-one'
	}

	const changePage = () => {
    app.ports.performUrlChange.send(url)
  };

  // Fallback for browsers that don't support View Transitions:
  if (!document.startViewTransition) {
    changePage();
    return;
  }

  // With View Transitions:
  const transition = document.startViewTransition(() => changePage());
})

app.ports.requestCounter.subscribe(() => {
	return getCounter()
})
