export const dynamic = "force-static";

import { getAllKanji } from "@/lib";
import { SITE_URL as baseUrl } from "@/lib/site";
import { MetadataRoute } from "next";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const lastModified = new Date();
  const kanjis = getAllKanji();

  const kanjiPages: MetadataRoute.Sitemap = kanjis.map(
    ({ params: { id } }) => ({
      url: `${baseUrl}/${encodeURIComponent(id)}`,
      lastModified,
    })
  );

  return [
    {
      url: baseUrl,
      lastModified,
      changeFrequency: "yearly",
      priority: 1,
    },
    {
      url: `${baseUrl}/about`,
      lastModified,
      changeFrequency: "yearly",
      priority: 0.8,
    },
    ...kanjiPages,
  ];
}
