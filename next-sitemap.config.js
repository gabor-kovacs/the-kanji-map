/** @type {import('next-sitemap').IConfig} */
const config = {
  siteUrl: process.env.SITE_URL || "https://thekanjimap.com",
  generateRobotsTxt: true, // (optional)
  // ...other options
};

module.exports = config;
