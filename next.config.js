/** @type {import('next').NextConfig} */
const withPlugins = require("next-compose-plugins");
const withPWA = require("next-pwa");
const path = require("path");

const nextConfig = {
  reactStrictMode: true,
  typescript: {
    ignoreBuildErrors: false,
  },
};

module.exports = withPlugins(
  [
    // [
    //   {
    //     include: path.resolve(__dirname, "public"),
    //     webpack(config, options) {
    //       return config;
    //     },
    //   },
    // ],
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
