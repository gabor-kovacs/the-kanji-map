import Cors from "cors";
import initMiddleware from "../../lib/init-middleware";

import type { NextApiRequest, NextApiResponse } from "next";

// Initialize the cors middleware
const corsa = initMiddleware(
  // You can read more about the available options here: https://github.com/expressjs/cors#configuration-options
  Cors({
    // Only allow requests with GET, POST and OPTIONS
    methods: ["GET", "POST", "OPTIONS"],
  })
);

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  // Run cors
  await corsa(req, res);
  // Rest of the API logic
  res.json({ message: "Hello Everyone!" });
}
