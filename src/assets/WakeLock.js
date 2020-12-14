// Keeps the screen on while workout


export default class WakeLock {
    constructor() {
        if ('wakeLock' in navigator) {
            // Screen Wake Lock API supported 🎉
            console.log('wakelock supported');
            this.supported = true;
            this.wakeLock = null;
        } else {
            console.log('wakelock not supported');
            this.supported = false;
            this.wakeLock = null;
        }
    }

    async start() {
        try {
            this.wakeLock = await navigator.wakeLock.request('screen')
            this.wakeLock.addEventListener('release', () => {
                console.log('Screen Wake Lock released:', this.released);
            });
            console.log('Screen Wake Lock released:', this.wakeLock.released);
        } catch (err) {
            console.error(`${err.name}, ${err.message}`);
        }
    }

    end() {
        if (this.wakeLock) {
            this.wakeLock.release();
            this.wakeLock = null;
        }
    }
}

// Function that attempts to request a screen wake lock.
export async function start() {

};

export function end() {

}