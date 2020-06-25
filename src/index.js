// js and cs files
import './assets/main.css';
import './assets/howler.core';
import './assets/simptip.min.css';

// images
import smh from './assets/img/smh.png';
import tadamp3File from './assets/sounds/tada.mp3';
import tadawavFile from './assets/sounds/tada.wav';
import tickFile from './assets/sounds/tick.mp3';
import whistleFile from './assets/sounds/whistle.mp3';

// other imports
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


// init sounds

const whistle = new Howl({
    volume: 0.8,
    src: [whistleFile]
});

const tada = new Howl({
    volume: 0.8,
    src: [tadamp3File, tadawavFile]
});

const tick = new Howl({
    volume: 1,
    src: [tickFile]
});

var storedConfig = localStorage.getItem('config');
console.log("Retrieved state: ", storedConfig);

var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {
        windowSize: {
            height: window.innerHeight,
            width: window.innerWidth
        },
        storedConfig: JSON.parse(storedConfig),
        smhSrc: smh
    }
});

app.ports.storeConfig.subscribe(config => {
    let configJson = JSON.stringify(config);
    localStorage.setItem('config', configJson);
    console.log("Saved state: ", configJson);
    // the actual process is too fast so I set a timeout to make it seem more realiztis LOL
    setTimeout(() => app.ports.storeConfigSuccess.send(null), 250);
});

app.ports.playWhistle.subscribe(() => {
    whistle.play()
});

app.ports.playTada.subscribe(() => {
    tada.play()
});

app.ports.playTick.subscribe(() => {
    tick.play()
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
