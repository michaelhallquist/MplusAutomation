# Normalize output text to UTF-8 where possible

Mplus output occasionally contains non-UTF-8 bytes in echoed input
blocks, especially when the source syntax was saved with a legacy
single-byte encoding. Recover common encodings line-by-line so
downstream regex and tokenization logic do not fail on invalid strings.

## Usage

``` r
sanitize_mplus_text(text, filename = NA_character_)
```

## Arguments

- text:

  Character vector of file contents

- filename:

  Optional filename used in warnings

## Value

A UTF-8-safe character vector
