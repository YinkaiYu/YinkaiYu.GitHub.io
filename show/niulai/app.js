document.documentElement.classList.add("js");

const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
const revealItems = document.querySelectorAll(".reveal");

if ("IntersectionObserver" in window && !reducedMotion.matches) {
	const observer = new IntersectionObserver((entries) => {
		for (const entry of entries) {
			if (!entry.isIntersecting) continue;
			entry.target.classList.add("is-visible");
			observer.unobserve(entry.target);
		}
	}, { rootMargin: "0px 0px -10%", threshold: 0.08 });

	revealItems.forEach((item) => observer.observe(item));
} else {
	revealItems.forEach((item) => item.classList.add("is-visible"));
}

const videos = [...document.querySelectorAll("video")];
for (const video of videos) {
	const wrap = video.closest(".video-wrap");
	video.addEventListener("play", () => {
		for (const other of videos) {
			if (other !== video && !other.paused) other.pause();
		}
		wrap?.classList.add("is-playing");
	});
	video.addEventListener("pause", () => wrap?.classList.remove("is-playing"));
	video.addEventListener("ended", () => wrap?.classList.remove("is-playing"));
}

let scheduled = false;
function updateScrollEffects() {
	const scrollable = Math.max(1, document.documentElement.scrollHeight - window.innerHeight);
	const progress = Math.min(1, Math.max(0, window.scrollY / scrollable));
	document.documentElement.style.setProperty("--scroll-progress", `${progress * 100}%`);

	if (!reducedMotion.matches) {
		const shift = Math.min(window.innerHeight * .1, window.scrollY * .08);
		document.documentElement.style.setProperty("--hero-shift", `${shift}px`);
	}
	scheduled = false;
}

window.addEventListener("scroll", () => {
	if (scheduled) return;
	scheduled = true;
	requestAnimationFrame(updateScrollEffects);
}, { passive: true });

window.addEventListener("resize", updateScrollEffects, { passive: true });
updateScrollEffects();
