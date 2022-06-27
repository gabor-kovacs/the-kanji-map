/** @type {import('next').NextConfig} */
const withPlugins = require("next-compose-plugins");
const withReactSvg = require("next-react-svg");
const withPWA = require("next-pwa");
const path = require("path");

const nextConfig = {
  reactStrictMode: true,

  typescript: {
    // !! WARN !!
    // Dangerously allow production builds to successfully complete even if
    // your project has type errors.
    // !! WARN !!
    ignoreBuildErrors: true,
  },
};

module.exports = withPlugins(
  [
    [
      withReactSvg,
      {
        include: path.resolve(__dirname, "public"),
        webpack(config, options) {
          return config;
        },
      },
    ],
    [
      withPWA,
      {
        pwa: {
          dest: "public",
          register: true,
          skipWaiting: true,
          disable: process.env.NODE_ENV === "development",
        },
      },
    ],
  ],

  nextConfig
);
