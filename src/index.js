// js and cs files
import './assets/main.css';
import './assets/howler.core';
import './assets/simptip.min.css';

// images and files
import smh from './assets/img/smh.png';
import iosShareIcon from './assets/img/ios_share_icon.png';
import tadamp3File from './assets/sounds/tada.mp3';
import tadawavFile from './assets/sounds/tada.wav';
import tickFile from './assets/sounds/tick.mp3';
import whistleFile from './assets/sounds/whistle.mp3';

// other imports
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// for the window to s p e a k
var synth = window.speechSynthesis;


// iOS "Add to home screen" popup
// Detects if device is on iOS 
const isIos = () => {
    const userAgent = window.navigator.userAgent.toLowerCase();
    console.log(userAgent)
    return /iphone|ipad|ipod/.test(userAgent);
}

// Detects if device is in standalone mode
const isInStandaloneMode = () => ('standalone' in window.navigator) && (window.navigator.standalone);
let showIosInstall = false;
// Checks if should display install popup notification:
if (isIos() && !isInStandaloneMode()) {
    showIosInstall = true
}


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
        showIosInstall: showIosInstall,
        images: {
            smhSrc: smh,
            iosShareIconSrc: iosShareIcon
        }
    }
});

app.ports.storeConfig.subscribe(config => {
    let configJson = JSON.stringify(config);
    localStorage.setItem('config', configJson);
    console.log("Saved state: ", configJson);
    // the actual process is too fast so I set a timeout to make it seem more realistic LOL
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

app.ports.speak.subscribe(value => {
    let speaker = new SpeechSynthesisUtterance(value);
    let voices = synth.getVoices()
    console.log(voices)
    // Google US English is the best, but we default to Samantha. If none, it will default to Alex
    let voice = voices.filter(function (voice) { return voice.name == 'Google US English' || voice.name == 'Samantha' })[0];
    console.log(voice)
    speaker.voice = voice
    synth.speak(speaker);
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
