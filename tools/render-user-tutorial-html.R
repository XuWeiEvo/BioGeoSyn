args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[[1L]] else "docs/user-tutorial.zh-CN.md"
output <- if (length(args) >= 2L) args[[2L]] else sub("\\.md$", ".html", input)

if (!requireNamespace("commonmark", quietly = TRUE)) {
  stop("The commonmark package is required to render the tutorial HTML.", call. = FALSE)
}

lines <- readLines(input, encoding = "UTF-8", warn = FALSE)
markdown <- paste(lines, collapse = "\n")
body <- commonmark::markdown_html(markdown)

title <- if (length(lines) > 0L) {
  sub("^#\\s*", "", lines[[1L]])
} else {
  "BioGeoSyn user tutorial"
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

css <- paste(
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Microsoft YaHei',Helvetica,Arial,sans-serif;line-height:1.65;color:#1f2937;background:#f8fafc;margin:0;}",
  ".page{max-width:980px;margin:0 auto;background:#fff;padding:42px 52px;box-shadow:0 1px 18px rgba(15,23,42,.08);}",
  "h1{font-size:34px;line-height:1.25;color:#111827;margin:0 0 18px;}",
  "h2{font-size:24px;border-top:1px solid #e5e7eb;padding-top:24px;margin-top:34px;color:#111827;}",
  "h3{font-size:18px;margin-top:24px;color:#111827;}",
  "p,li{font-size:16px;}",
  "code{font-family:Consolas,Menlo,Monaco,monospace;background:#eef2f7;border-radius:4px;padding:2px 5px;}",
  "pre{background:#0f172a;color:#e5e7eb;padding:16px 18px;border-radius:6px;overflow-x:auto;}",
  "pre code{background:transparent;color:inherit;padding:0;}",
  "blockquote{border-left:4px solid #2563eb;margin-left:0;padding-left:14px;color:#374151;background:#eff6ff;}",
  "a{color:#1d4ed8;text-decoration:none;}",
  "a:hover{text-decoration:underline;}",
  "table{border-collapse:collapse;width:100%;margin:18px 0;}",
  "th,td{border:1px solid #d1d5db;padding:8px 10px;text-align:left;}",
  "th{background:#f3f4f6;}",
  ".print-note{background:#ecfdf5;border:1px solid #bbf7d0;border-left:5px solid #16a34a;padding:12px 14px;border-radius:6px;margin:0 0 24px 0;}",
  "@media print{body{background:#fff}.page{box-shadow:none;padding:20mm;max-width:none}a{color:#111827;text-decoration:none}pre{white-space:pre-wrap;word-break:break-word}h2{page-break-after:avoid}}",
  sep = "\n"
)

html <- paste0(
  "<!doctype html>\n",
  "<html lang=\"zh-CN\">\n",
  "<head>\n",
  "<meta charset=\"utf-8\">\n",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
  "<title>", html_escape(title), "</title>\n",
  "<style>\n", css, "\n</style>\n",
  "</head>\n",
  "<body>\n",
  "<main class=\"page\">\n",
  "<div class=\"print-note\"><strong>Tip:</strong> Send this HTML file directly, or use Ctrl+P in a browser to save it as PDF.</div>\n",
  body,
  "\n</main>\n",
  "</body>\n",
  "</html>\n"
)

writeLines(enc2utf8(html), output, useBytes = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
