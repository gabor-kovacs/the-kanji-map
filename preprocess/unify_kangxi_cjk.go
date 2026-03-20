// unify_kangxi_cjk replaces Kangxi/CJK compatibility radicals with their
// unified CJK ideograph equivalents across specified files or directories.
// It updates files in place.
//
// This makes visually identical forms (like 忄 vs 忄, 扌 vs 扌, 辶 vs 辶, 十 vs 十)
// consistent so downstream tools can reliably match and join data.
//
// What it does:
// - With explicit arguments: processes each file or recursively walks each directory
// - Without arguments: recursively walks `../data`
// - Reads each regular file as UTF-8 text
// - Replaces compatibility radicals with unified forms
// - Writes the normalized text back to the same file
//
// How to run (from this directory):
//
//	go run ./unify_kangxi_cjk.go
//	go run ./unify_kangxi_cjk.go ./kanjivg.xml
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
	"runtime"

	eqi "github.com/mochi-co/equivalent-unified-ideograph"
)

func normalizeFile(path string) (bool, error) {
	inFile, err := os.Open(path)
	if err != nil {
		return false, err
	}
	defer inFile.Close()

	buf, changed, err := eqi.BufferedReplace(inFile)
	if err != nil {
		return false, err
	}
	if len(changed) == 0 {
		return false, nil
	}

	if err := os.WriteFile(path, buf.Bytes(), 0o644); err != nil {
		return false, err
	}

	return true, nil
}

func main() {
	// Resolve paths from this source file location so behavior is stable
	// regardless of current working directory.
	_, srcFile, _, ok := runtime.Caller(0)
	if !ok {
		log.Fatal("unable to determine script location")
	}
	preprocessDir := filepath.Dir(srcFile)
	projectRoot := filepath.Clean(filepath.Join(preprocessDir, ".."))
	dataDir := filepath.Join(projectRoot, "data")

	targets := os.Args[1:]
	if len(targets) == 0 {
		targets = []string{dataDir}
	}

	skipDirs := map[string]bool{
		".git":         true,
		"node_modules": true,
		".next":        true,
		"out":          true,
	}

	var filesProcessed, filesChanged int

	for _, target := range targets {
		info, err := os.Stat(target)
		if err != nil {
			log.Printf("skip %s: %v", target, err)
			continue
		}

		if !info.IsDir() {
			filesProcessed++
			changed, err := normalizeFile(target)
			if err != nil {
				log.Printf("normalize %s: %v", target, err)
				continue
			}
			if changed {
				filesChanged++
				fmt.Printf("normalized: %s\n", target)
			}
			continue
		}

		err = filepath.WalkDir(target, func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				if skipDirs[d.Name()] {
					return filepath.SkipDir
				}
				return nil
			}

			filesProcessed++
			changed, err := normalizeFile(path)
			if err != nil {
				log.Printf("normalize %s: %v", path, err)
				return nil
			}
			if changed {
				filesChanged++
				fmt.Printf("normalized: %s\n", path)
			}
			return nil
		})
		if err != nil {
			log.Printf("walk %s: %v", target, err)
		}
	}

	fmt.Printf("Done. Processed %d files, updated %d files.\n", filesProcessed, filesChanged)
}
