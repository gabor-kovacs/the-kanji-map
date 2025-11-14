import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Only use static export for production builds, not dev mode
  ...(process.env.NODE_ENV === "production" && { output: "export" }),
  eslint: {
    // Warning: This allows production builds to successfully complete even if
    // your project has ESLint errors.
    ignoreDuringBuilds: true,
  },
};

export default nextConfig;
