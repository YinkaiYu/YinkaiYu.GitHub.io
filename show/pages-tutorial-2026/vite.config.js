import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
	root: 'app',
	base: './',
	publicDir: false,
	plugins: [react()],
	build: {
		outDir: '..',
		emptyOutDir: false,
		assetsDir: 'assets',
		rollupOptions: {
			output: {
				entryFileNames: 'assets/app.js',
				chunkFileNames: 'assets/[name].js',
				assetFileNames: (assetInfo) => {
					if (assetInfo.names?.some((name) => name.endsWith('.css'))) {
						return 'assets/app.css'
					}
					return 'assets/[name][extname]'
				},
			},
		},
	},
})
