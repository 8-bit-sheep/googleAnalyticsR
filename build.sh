echo "==> Building documentation"
R -e "devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"
echo "Documentation complete"
echo "==> Building googleAnalyticsR"
cd .. && R CMD INSTALL --no-multiarch --with-keep.source googleAnalyticsR
echo "==> Building website"
cd googleAnalyticsR && R -e "pkgdown::build_site()"
echo "Website complete"
