import { defineConfig } from 'astro/config';

function markdownDefaultLayoutPlugin () {
    return function (tree, file) {
        if (file.data.astro.frontmatter.layout === undefined) {
            file.data.astro.frontmatter.layout = '@layouts/MarkdownLayout.astro';
        }
    }
}

// https://astro.build/config
export default defineConfig({
    markdown: {
        remarkPlugins: [
            markdownDefaultLayoutPlugin,
        ],
        extendDefaultPlugins: true,
    },
});
