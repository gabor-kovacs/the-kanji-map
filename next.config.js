/** @type {import('next').NextConfig} */
const withPlugins = require("next-compose-plugins");
const withReactSvg = require("next-react-svg");
const path = require("path");

// my secret api keys are in keys.js not found in the  public repo
// module.exports = {
//   kanjialiveApiKey: "YOUR_API_KEY",
//   kanjialiveApiKey: "5a6bb15de1mshc2ba606776929acp105495jsn09f3ae80e70d",
// };
const keys = require("./keys");

const nextConfig = {
  reactStrictMode: true,
  env: {
    KANJIALIVE_API_KEY: keys.kanjialiveApiKey,
  },
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
