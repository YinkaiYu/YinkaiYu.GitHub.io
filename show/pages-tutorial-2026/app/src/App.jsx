import { useEffect, useState } from 'react'
import heroClouds from './assets/clouds.jpg'

const NAV_ITEMS = [
	{ id: 'prepare', label: '准备工具' },
	{ id: 'choose', label: '选风格' },
	{ id: 'write', label: '写内容' },
	{ id: 'publish', label: '发布' },
]

const THEME_SOURCES = [
	{ name: 'Astro 主题', url: 'https://astro.build/themes/' },
	{ name: 'Hugo 主题', url: 'https://themes.gohugo.io/' },
	{ name: 'Hexo 主题', url: 'https://hexo.io/themes/' },
	{ name: 'Jekyll 主题', url: 'https://jekyllthemes.io/' },
]

const REFERENCES = [
	{ name: 'yyk space', domain: 'yykspace.com', url: 'https://www.yykspace.com/' },
	{ name: 'refraction-ray', domain: 're-ra.xyz', url: 'https://re-ra.xyz/' },
	{ name: 'JIEJOE · 视觉设计者', domain: 'jiejoe.com', url: 'https://www.jiejoe.com/home' },
	{ name: "It's Nicky Case!", domain: 'ncase.me', url: 'https://ncase.me/' },
	{ name: "Xuejia Yu's homepage", domain: 'mysxl.cn', url: 'https://site-1028119-4516-4712.mysxl.cn/' },
	{ name: '有希＝ペルレイノ', domain: 'metanoise.in', url: 'https://metanoise.in/' },
	{ name: '秘密花园', domain: 'yini.org · HTTP', url: 'http://www.yini.org/club/garden.html' },
	{ name: '约伊兹的萌狼乡手札', domain: 'blog.yoitsu.moe', url: 'https://blog.yoitsu.moe/' },
	{ name: 'Tian-Hua Yang’s Academic Homepage', domain: 'thomasyangth.github.io', url: 'https://thomasyangth.github.io/' },
	{ name: '暗之城！科学之城！', domain: 'jack-works.github.io', url: 'https://jack-works.github.io/' },
	{ name: 'I/O OVER · この星の半分を真っ赤に染めた', domain: 'ioover.net', url: 'https://ioover.net/' },
	{ name: "Zakee's Planet", domain: 'zak.ee', url: 'https://zak.ee/' },
	{ name: 'Yi-Ming Ding', domain: 'ymd-physics.github.io', url: 'https://ymd-physics.github.io/' },
]

const PROMPTS = {
	environment: `我准备制作个人网站。请先检查这台 Windows 电脑上的开发环境，并在我选定的项目文件夹里开始工作。

需要安装工具时，请告诉我它的用途，并使用官方来源。环境准备好以后，请创建一个最小页面、运行本地预览，再告诉我怎样打开它。`,
	design: `我想做一个个人网站。请参考这些页面或截图：
[粘贴链接，或者把截图发给 ChatGPT]

我喜欢其中的：[排版、颜色、字体、动效或某个页面细节]

请先和我确认页面结构与实现方式，再在当前项目文件夹里制作。可以选择合适的网站框架，也可以从零开始。视觉与结构可以借鉴，文字、图片和个人信息使用我自己的。完成后请运行本地预览。`,
	content: `请帮我整理个人网站首页的内容。我会提供姓名、介绍、兴趣、经历、作品或联系方式。

请把这些信息写进当前页面，语言保持自然。内容不足的部分可以暂时留空。保留当前风格，完成后打开本地预览，让我检查电脑和手机上的效果。`,
	publish: `请把当前个人网站发布到 GitHub Pages。我的 GitHub 用户名是：[填写用户名]。

请根据项目实际使用的技术选择发布方式，创建或连接“[用户名].github.io”仓库。提交前检查 API Key 和访问配置没有进入网站文件。发布后打开公开网址，检查首页、图片和链接。

需要我登录或确认时，请告诉我当前要完成的一步。`,
}

function ExternalIcon() {
	return (
		<svg viewBox="0 0 20 20" aria-hidden="true">
			<path d="M11 4h5v5M9 11l7-7M15 11v4a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V6a1 1 0 0 1 1-1h4" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" strokeLinejoin="round" />
		</svg>
	)
}

function CopyBlock({ title, prompt }) {
	const [copied, setCopied] = useState(false)

	async function copyPrompt() {
		try {
			await navigator.clipboard.writeText(prompt)
		} catch {
			const textArea = document.createElement('textarea')
			textArea.value = prompt
			textArea.style.position = 'fixed'
			textArea.style.opacity = '0'
			document.body.appendChild(textArea)
			textArea.select()
			document.execCommand('copy')
			textArea.remove()
		}
		setCopied(true)
		window.setTimeout(() => setCopied(false), 1600)
	}

	return (
		<div className="prompt-block reveal">
			<div className="prompt-heading">
				<div>
					<p className="prompt-label">参考说法</p>
					<h3>{title}</h3>
				</div>
				<button className={copied ? 'is-copied' : ''} type="button" onClick={copyPrompt} aria-live="polite">
					{copied ? '已复制' : '复制'}
				</button>
			</div>
			<pre><code>{prompt}</code></pre>
		</div>
	)
}

function SectionHeading({ number, title, children }) {
	return (
		<header className="section-heading reveal">
			<p className="step-number">{number}</p>
			<h2>{title}</h2>
			<p>{children}</p>
		</header>
	)
}

function ExternalLink({ href, children, className = '' }) {
	return (
		<a className={className} href={href} target="_blank" rel="noreferrer">
			<span>{children}</span>
			<ExternalIcon />
		</a>
	)
}

function App() {
	const [activeSection, setActiveSection] = useState('prepare')
	const [menuOpen, setMenuOpen] = useState(false)

	useEffect(() => {
		const sections = NAV_ITEMS.map((item) => document.getElementById(item.id)).filter(Boolean)
		const observer = new IntersectionObserver(
			(entries) => {
				const visible = entries
					.filter((entry) => entry.isIntersecting)
					.sort((a, b) => b.intersectionRatio - a.intersectionRatio)[0]
				if (visible) setActiveSection(visible.target.id)
			},
			{ rootMargin: '-18% 0px -66% 0px', threshold: [0, 0.2, 0.5] },
		)
		sections.forEach((section) => observer.observe(section))
		return () => observer.disconnect()
	}, [])

	useEffect(() => {
		const items = document.querySelectorAll('.reveal')
		const observer = new IntersectionObserver(
			(entries) => {
				entries.forEach((entry) => {
					if (entry.isIntersecting) {
						entry.target.classList.add('is-visible')
						observer.unobserve(entry.target)
					}
				})
			},
			{ threshold: 0.08 },
		)
		items.forEach((item) => observer.observe(item))
		return () => observer.disconnect()
	}, [])

	function closeMenu() {
		setMenuOpen(false)
	}

	return (
		<>
			<a className="skip-link" href="#prepare">跳到第一步</a>
			<header className="site-header">
				<a className="brand" href="#top" onClick={closeMenu}>个人网站制作教学</a>
				<button
					className="menu-button"
					type="button"
					aria-expanded={menuOpen}
					aria-controls="site-nav"
					onClick={() => setMenuOpen((value) => !value)}
				>
					<span>{menuOpen ? '关闭' : '目录'}</span>
					<span className="menu-lines" aria-hidden="true" />
				</button>
				<nav id="site-nav" className={menuOpen ? 'is-open' : ''} aria-label="教程目录">
					{NAV_ITEMS.map((item) => (
						<a key={item.id} className={activeSection === item.id ? 'is-active' : ''} href={`#${item.id}`} onClick={closeMenu}>
							{item.label}
						</a>
					))}
				</nav>
			</header>

			<main>
				<section id="top" className="hero" style={{ '--hero-image': `url(${heroClouds})` }}>
					<div className="hero-shade" />
					<div className="hero-inner">
						<h1>个人网站制作教学</h1>
						<p>路线和参考说法仅供参考，可以按自己的情况调整。</p>
					</div>
				</section>

				<section id="prepare" className="lesson">
					<div className="section-shell lesson-layout">
						<SectionHeading number="01" title="准备好工具">
							我会提供 API Key（每人 100 美元额度）、CC Switch 配置方法和 Clash Verge 节点。
						</SectionHeading>
						<div className="lesson-content">
							<ol className="action-list reveal">
								<li>
									<span className="action-number">1</span>
									<div><h3>下载 ChatGPT</h3><p>安装 Windows 版，登录以后就可以和 AI agent 一起工作。</p></div>
									<ExternalLink href="https://apps.microsoft.com/detail/9PLM9XGG6VKS" className="action-link">下载</ExternalLink>
								</li>
								<li>
									<span className="action-number">2</span>
									<div><h3>配置 CC Switch</h3><p>下载后，按我提供的方法填入 API Key。API Key 只放在配置工具里。</p></div>
									<ExternalLink href="https://github.com/farion1231/cc-switch/releases" className="action-link">下载</ExternalLink>
								</li>
								<li>
									<span className="action-number">3</span>
									<div><h3>配置网络访问</h3><p>安装 Clash Verge Rev，导入我提供的节点。</p></div>
									<ExternalLink href="https://github.com/Clash-Verge-rev/clash-verge-rev/releases" className="action-link">下载</ExternalLink>
								</li>
								<li>
									<span className="action-number">4</span>
									<div>
										<h3>注册 GitHub</h3>
										<p>慎重考虑用户名，它会决定个人网站的默认域名：<code>用户名.github.io</code>。</p>
									</div>
									<ExternalLink href="https://github.com/signup" className="action-link">注册</ExternalLink>
								</li>
							</ol>

							<div className="github-note reveal">
								<p>GitHub 是全世界最接近共产主义的地方。</p>
								<span>无数人把代码、工具和知识公开出来，彼此使用、修改和继续创造，很值得认真逛一逛。</span>
							</div>

							<CopyBlock title="让 AI agent 配置环境" prompt={PROMPTS.environment} />
						</div>
					</div>
				</section>

				<section id="choose" className="lesson lesson-light">
					<div className="section-shell lesson-layout">
						<SectionHeading number="02" title="挑选喜欢的样子">
							先逛一圈。看到喜欢的页面，就保存链接或截图，再交给 ChatGPT 实现。
						</SectionHeading>
						<div className="lesson-content">
							<p className="lead-copy reveal">你可以直接写 HTML 和 CSS，可以挑选 Astro、Hugo、Hexo、Jekyll 的主题，也可以让 ChatGPT 从零制作。</p>

							<div className="theme-links reveal" aria-label="主题网站">
								{THEME_SOURCES.map((source) => (
									<ExternalLink key={source.name} href={source.url}>{source.name}</ExternalLink>
								))}
							</div>

							<div className="reference-heading reveal">
								<h3>这些网站都值得看一看</h3>
								<ExternalLink href="https://github.com/YinkaiYu/YinkaiYu.GitHub.io" className="source-link">查看 yyk space 源码</ExternalLink>
							</div>
							<div className="reference-list reveal">
								{REFERENCES.map((site, index) => (
									<a key={site.url} href={site.url} target="_blank" rel="noreferrer">
										<span className="reference-index">{String(index + 1).padStart(2, '0')}</span>
										<span className="reference-name">{site.name}</span>
										<span className="reference-domain">{site.domain}</span>
										<ExternalIcon />
									</a>
								))}
							</div>

							<CopyBlock title="把喜欢的页面交给 ChatGPT" prompt={PROMPTS.design} />
						</div>
					</div>
				</section>

				<section id="write" className="lesson">
					<div className="section-shell lesson-layout">
						<SectionHeading number="03" title="写成你的网站">
							把名字换成自己的，再写下你真正想放在网上的内容。写什么都可以。
						</SectionHeading>
						<div className="lesson-content">
							<ul className="topic-list reveal">
								<li>名字与介绍</li>
								<li>兴趣与生活</li>
								<li>学习与经历</li>
								<li>项目与作品</li>
								<li>照片与收藏</li>
								<li>联系方式</li>
							</ul>
							<CopyBlock title="让 AI agent 帮你整理内容" prompt={PROMPTS.content} />
						</div>
					</div>
				</section>

				<section id="publish" className="lesson publish-section">
					<div className="section-shell lesson-layout">
						<SectionHeading number="04" title="发布到 GitHub Pages">
							发布以后，你会得到一个可以直接分享的网址。使用的框架不同，AI agent 会选择对应的发布方式。
						</SectionHeading>
						<div className="lesson-content">
							<div className="public-url reveal">
								<span>你的默认网址</span>
								<strong>https://你的用户名.github.io/</strong>
							</div>
							<CopyBlock title="让 AI agent 完成发布" prompt={PROMPTS.publish} />
							<div className="domain-note reveal">
								<h3>可选：使用自己的域名</h3>
								<p>网站发布以后，可以再购买喜欢的域名，让 AI agent 帮你连接到 GitHub Pages。</p>
							</div>
						</div>
					</div>
				</section>
			</main>

			<footer>
				<span>© 2026 Yinkai Yu</span>
				<ExternalLink href="https://www.yykspace.com/">yyk space</ExternalLink>
			</footer>
		</>
	)
}

export default App
