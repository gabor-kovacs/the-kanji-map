export const dynamic = "force-static";

import { getAllKanji } from "@/lib";
import { MetadataRoute } from "next";

export default function sitemap(): MetadataRoute.Sitemap {
  const kanjis = getAllKanji();
  const baseUrl =
    process.env.NEXT_PUBLIC_BASE_URL || "https://the-kanji-map.com";

  const kanjiPages: MetadataRoute.Sitemap = kanjis.map(
    ({ params: { id } }) => ({
      url: `${baseUrl}/${id}`,
      lastModified: new Date(),
    })
  );

  return [
    {
      url: baseUrl,
      lastModified: new Date(),
      changeFrequency: "yearly",
      priority: 1,
    },
    {
      url: `${baseUrl}/about`,
      lastModified: new Date(),
      changeFrequency: "yearly",
      priority: 0.8,
    },
    ...kanjiPages,
  ];
}
