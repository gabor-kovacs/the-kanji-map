// unify_kangxi_cjk replaces Kangxi/CJK compatibility radicals with their
// unified CJK ideograph equivalents across ALL files in the `data` and
// `preprocess` folders. It runs without parameters and updates files in place.
//
// This makes visually identical forms (like 忄 vs 忄, 扌 vs 扌, 辶 vs 辶, 十 vs 十)
// consistent so downstream tools can reliably match and join data.
//
// What it does:
// - Recursively walks `../../data` and `..` (the whole preprocess folder)
// - Reads each regular file as UTF-8 text
// - Replaces compatibility radicals with unified forms
// - Writes the normalized text back to the same file
//
// How to run (from this directory):
//
//	go run ./unify_kangxi_cjk.go
//
// Notes:
//   - It attempts to process any text file; if a file isn't UTF-8 text, it will
//     likely remain unchanged.
//   - Safe to run multiple times; normalization is idempotent.
package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	eqi "github.com/mochi-co/equivalent-unified-ideograph"
)

func main() {
	// Roots to process (relative to this file's directory):
	//   - ../../data      (project data folder)
	//   - ..              (entire preprocess folder)
	roots := []string{"../../data", ".."}

	var filesProcessed, filesChanged int

	for _, root := range roots {
		// Resolve to absolute path based on CWD
		r := root
		err := filepath.WalkDir(r, func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				return nil
			}
			// Read file
			inFile, err := os.Open(path)
			if err != nil {
				// Log and continue
				log.Printf("skip %s: %v", path, err)
				return nil
			}
			defer inFile.Close()

			// changed is a map[string]int (counts of replacements)
			buf, changed, err := eqi.BufferedReplace(inFile)
			if err != nil {
				log.Printf("normalize %s: %v", path, err)
				return nil
			}
			filesProcessed++
			if len(changed) > 0 {
				if err := os.WriteFile(path, buf.Bytes(), 0o644); err != nil {
					log.Printf("write %s: %v", path, err)
					return nil
				}
				filesChanged++
				fmt.Printf("normalized: %s\n", path)
			}
			return nil
		})
		if err != nil {
			log.Printf("walk %s: %v", r, err)
		}
	}

	fmt.Printf("Done. Processed %d files, updated %d files.\n", filesProcessed, filesChanged)
}
