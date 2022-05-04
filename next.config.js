/** @type {import('next').NextConfig} */
const withPlugins = require("next-compose-plugins");
const withReactSvg = require("next-react-svg");
const path = require("path");

const nextConfig = {
  reactStrictMode: true,
  // optimizeFonts: false,
};

module.exports = withPlugins(
  [
    [
      withReactSvg,
      {
        include: path.resolve(__dirname, "public/images"),
        webpack(config, options) {
          return config;
        },
      },
    ],
  ],
  nextConfig
);
