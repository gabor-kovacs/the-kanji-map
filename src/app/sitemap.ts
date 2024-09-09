import { getAllKanji } from "@/lib";
import { MetadataRoute } from "next";

export default function sitemap(): MetadataRoute.Sitemap {
  const kanjis = getAllKanji();

  const kanjiPages: MetadataRoute.Sitemap = kanjis.map(
    ({ params: { id } }) => ({
      url: `${process.env.NEXT_PUBLIC_BASE_URL}/${id}`,
      //   lastModified: new Date(),
      // changeFrequency:,
      // priority:
    })
  );

  return [
    {
      url: `${process.env.NEXT_PUBLIC_BASE_URL}/about`,
      lastModified: new Date(),
    },
    ...kanjiPages,
  ];
}
