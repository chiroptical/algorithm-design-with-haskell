format:
	find . -name "*.hs" -exec ormolu -m inplace {} \;

.PHONY: format
